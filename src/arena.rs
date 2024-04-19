use std::{mem, ptr, alloc::{alloc, Layout, handle_alloc_error, dealloc}, slice, marker::PhantomData, io, path::Path, fmt, os::unix::ffi::OsStrExt, io::Write as ioWrite};

// Arena - a simple untyped arena.
// StringTable - arena equipped with a list of pointers to allocated strings, allowing search.
//               E.g. put all type names in it, each associated with an id, then iterate over them sequentially for fast search.
//
// Rust borrow checker really doesn't combine well with arenas, so we try to use arrays or pools where possible.
// But for types I couldn't come up with any reasonable way to avoid arenas; I tried implementing it with pools,
// but it quickly got out of hand because types from different pools may interact and even reference each other (when temporary types are introduced in watch expressions).
// So we use arenas for that, with some manual transmuting of lifetimes.

pub struct Arena {
    chunks: Vec<Chunk>,
    current_chunk: Option<usize>,
}

pub struct StringTable {
    pub arena: Arena,
    pub strings: Vec<StringRef>,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct StringRef {
    pub s: &'static [u8],
    pub id: usize,
}


struct Chunk {
    data: *mut u8, // avoiding Box<[u8]> because I'm not sure whether it's valid with aliasing rules
    capacity: usize,
    used: usize,
}
unsafe impl Send for Chunk {}
unsafe impl Sync for Chunk {}

const CHUNK_SIZE: usize = 1 << 18;

impl Arena {
    pub fn new() -> Self { Self {chunks: Vec::new(), current_chunk: None} }

    pub fn alloc(&mut self, size: usize, align: usize) -> &mut [u8] {
        assert!(align != 0 && (align - 1) & align == 0);
        let chunk_idx = self.pick_chunk(size + align - 1, size);
        let c = &mut self.chunks[chunk_idx];
        let start = (c.used + align - 1) & !(align - 1);
        c.used = start + size;
        unsafe {slice::from_raw_parts_mut(c.data.add(c.used-size), size)}
    }

    // Copies an value into this arena, bytewise. Immutable version casts away the lifetime.
    // Mutable version doesn't, to prevent UB if the caller tries to iterate over the arena while still holding a mutable reference to its element, contradicting the reference.
    // Source: https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html ("The precise Rust aliasing rules are somewhat in flux, but the main points are not contentious: [...]")
    pub fn add_mut<T>(&mut self, val: T) -> &mut T {
        assert!(!mem::needs_drop::<T>());
        let r: &mut T = unsafe {mem::transmute(self.alloc(mem::size_of::<T>(), mem::align_of::<T>()).as_mut_ptr())};
        *r = val;
        r
    }
    pub fn add<T>(&mut self, val: T) -> &'static T { unsafe {mem::transmute(self.add_mut(val))} }

    pub fn add_slice_mut<'a, T>(&'a mut self, a: &[T]) -> &'a mut [T] {
        assert!(!mem::needs_drop::<T>());
        unsafe {
            let r: *mut T = self.alloc(mem::size_of::<T>() * a.len(), mem::align_of::<T>()).as_mut_ptr() as _;
            ptr::copy_nonoverlapping(a.as_ptr(), r, a.len());
            slice::from_raw_parts_mut(r, a.len())
        }
    }
    pub fn add_slice<T>(&mut self, a: &[T]) -> &'static [T] { unsafe {mem::transmute(self.add_slice_mut(a))} }

    pub fn add_str(&mut self, s: &str) -> &'static str { unsafe {mem::transmute(self.add_slice(s.as_bytes()))} }
    pub fn add_path(&mut self, p: &Path) -> &'static Path { unsafe {mem::transmute(self.add_slice(p.as_os_str().as_bytes()))} }

    // If the given array was the last thing allocated by this arena, and there's enough room for one more element, grows the array in-place.
    // Otherwise allocates a new array of size len+1 and copies the old array to it. Needless to say, this should only be used if the latter case is rare.
    // TODO: Delet dis and have a scratch Vec in LoaderStackEntry instead.
    pub fn push_to_array_likely_at_end<T>(&mut self, start: &mut *const T, len: &mut usize, add: T) {
        assert!(!mem::needs_drop::<T>());
        unsafe {
            let size = mem::size_of::<T>();
            let align = mem::align_of::<T>();
            assert!(size % align == 0);
            let end = (*start).add(*len) as *const u8;
            if let &Some(i) = &self.current_chunk {
                let c = &mut self.chunks[i];
                if end == c.data.add(c.used) && c.used + size <= c.capacity {
                    // Fast path.
                    unsafe {*(c.data.add(c.used) as *mut T) = add};
                    c.used += size;
                    *len += 1;
                    return;
                }
            }
            if let Some(c) = self.chunks.last_mut() {
                if end == c.data.add(c.used) && c.used + size <= c.capacity {
                    unsafe {*(c.data.add(c.used) as *mut T) = add};
                    c.used += size;
                    *len += 1;
                    return;
                }                    
            }

            // Grow chunk size exponentially to avoid rapidly eating all memory with O(n^2) waste if one array is bigger than default chunk size (ask how I know).
            let bytes = size * (*len + 1);
            let i = self.pick_chunk(bytes * 2 + align - 1, bytes);
            let c = &mut self.chunks[i];
            c.used = ((c.used + align - 1) & !(align - 1)) + bytes;
            let r: *mut T = c.data.add(c.used-bytes) as _;
            ptr::copy_nonoverlapping(*start, r, *len);
            *r.add(*len) = add;
            *start = r as _;
            *len += 1;
        }
    }

    pub fn capacity(&self) -> usize {
        let mut r = 0;
        for c in &self.chunks {
            r += c.capacity;
        }
        r
    }
    pub fn used(&self) -> usize {
        let mut r = 0;
        for c in &self.chunks {
            r += c.used;
        }
        r
    }

    // Intended for single-type arenas.
    pub unsafe fn iter<'a, T: 'a>(&'a self) -> impl Iterator<Item = &'a T> { Iter {arena: self, chunk_idx: 0, offset: 0, _boo: PhantomData::default()} }

    pub fn write<'a>(&'a mut self) -> ArenaWrite<'a> {
        ArenaWrite {arena: self, len: 0, chunk_idx: usize::MAX, start: ptr::null_mut(), capacity: 0}
    }

    fn pick_chunk(&mut self, size: usize, likely_usage: usize) -> usize {
        if let &Some(i) = &self.current_chunk {
            if self.chunks[i].capacity - self.chunks[i].used >= size {
                return i;
            }
            // This may be useful in case the arena was cleared.
            if i + 1 < self.chunks.len() && self.chunks[i+1].capacity - self.chunks[i+1].used >= size {
                self.current_chunk = Some(i+1);
                return i+1;
            }
        }
        let res = self.chunks.len();

        let capacity = CHUNK_SIZE.max(size);
        let layout = Layout::from_size_align(capacity, 1).unwrap();
        let p = unsafe { alloc(layout) };
        if p.is_null() {
            handle_alloc_error(layout);
        }
        let c = Chunk {data: p, capacity, used: 0};

        // Between the new chunk and current_chunk, nominate one to be current and the other to be immutable.
        // Useful if there are both small and huge (> CHUNK_SIZE) allocations.
        if let &Some(i) = &self.current_chunk {
            if self.chunks[i].capacity - self.chunks[i].used < capacity - likely_usage {
                self.current_chunk = Some(res);
            }
        } else {
            self.current_chunk = Some(res);
        }

        self.chunks.push(c);
        res
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        unsafe { dealloc(self.data, Layout::from_size_align(self.capacity as usize, 1).unwrap()); }
    }
}

struct Iter<'a, T: 'a> {
    arena: &'a Arena,
    chunk_idx: usize,
    offset: usize,
    _boo: PhantomData<&'a [T]>,
}
impl<'a, T: 'a> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let entry_size = mem::size_of::<T>();
        loop {
            if self.chunk_idx >= self.arena.chunks.len() {
                return None;
            }
            let chunk = &self.arena.chunks[self.chunk_idx];
            if self.offset + entry_size > chunk.used {
                self.chunk_idx += 1;
                self.offset = 0;
                continue;
            }
            let r: &'a T = unsafe {mem::transmute(&*chunk.data.add(self.offset))};
            self.offset += entry_size;
            return Some(r)
        }
    }
}

pub struct ArenaWrite<'a> {
    arena: &'a mut Arena,
    len: usize,

    chunk_idx: usize,
    start: *mut u8,
    capacity: usize,
}

impl<'a> ArenaWrite<'a> {
    // Completes the write. If the ArenaWrite is dropped without calling this, the write is cancelled and nothing breaks.
    pub fn finish(self) -> &'static [u8] {
        if self.len == 0 {
            return &mut [];
        }
        let c = &mut self.arena.chunks[self.chunk_idx];
        c.used += self.len;
        unsafe {slice::from_raw_parts(c.data.add(c.used-self.len), self.len)}
    }
    pub fn finish_str(self) -> &'static str { unsafe {mem::transmute(self.finish())} }
}

impl io::Write for ArenaWrite<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let new_size = self.len + buf.len();
        if new_size > self.capacity {
            // Migrate to new chunk.

            // Exponential growth if this one string is bigger than a whole default chunk size.
            let new_idx = self.arena.pick_chunk(new_size * 2, new_size);
            let new_chunk = &mut self.arena.chunks[new_idx];
            let new_start = unsafe {new_chunk.data.add(new_chunk.used)};
            if self.len != 0 {
                unsafe { ptr::copy_nonoverlapping(self.start, new_start, self.len); }
            }

            self.chunk_idx = new_idx;
            self.start = new_start;
            self.capacity = new_chunk.capacity - new_chunk.used;
        }

        unsafe { ptr::copy_nonoverlapping(buf.as_ptr(), self.start.add(self.len), buf.len()); }
        self.len += buf.len();

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}
impl cpp_demangle::DemangleWrite for ArenaWrite<'_> {
    fn write_string(&mut self, s: &str) -> fmt::Result {
        self.write(s.as_bytes()).unwrap();
        Ok(())
    }
}

impl StringTable {
    pub fn new() -> Self { Self {arena: Arena::new(), strings: Vec::new()} }

    pub fn add_slice(&mut self, s: &[u8], id: usize) -> &'static [u8] {
        let s = self.arena.add_slice(s);
        self.strings.push(StringRef {s, id});
        s
    }
    pub fn add_str(&mut self, s: &str, id: usize) -> &'static str { unsafe {mem::transmute(self.add_slice(s.as_bytes(), id))} }
    pub fn add_path(&mut self, p: &Path, id: usize) -> &'static Path { unsafe {mem::transmute(self.add_slice(p.as_os_str().as_bytes(), id))} }

    pub fn write<'a>(&'a mut self) -> StringTableWrite<'a> { StringTableWrite {table: self, w: self.arena.write()} }
}

pub struct StringTableWrite<'a> {
    table: *mut StringTable,
    w: ArenaWrite<'a>,
}

impl StringTableWrite<'_> {
    pub fn finish(self, id: usize) -> &'static [u8] {
        let s = self.w.finish();
        let table = unsafe {&mut *self.table};
        table.strings.push(StringRef {s, id});
        s
    }
    pub fn finish_str(self, id: usize) -> &'static str { unsafe {mem::transmute(self.finish(id))} }
}
impl io::Write for StringTableWrite<'_> { fn write(&mut self, buf: &[u8]) -> io::Result<usize> { self.w.write(buf) }  fn flush(&mut self) -> io::Result<()> { Ok(()) } }
impl cpp_demangle::DemangleWrite for StringTableWrite<'_> { fn write_string(&mut self, s: &str) -> fmt::Result { self.w.write_string(s) } }

#[cfg(test)]
mod tests {
    use crate::arena::*;
     #[test]
    fn arena_push() {
        let mut arena = Arena::new();
        let mut p: *const u64 = ptr::null();
        let mut l: usize = 0;
        arena.push_to_array_likely_at_end(&mut p, &mut l, 10);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 20);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 30);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 40);
        assert_eq!(arena.chunks.len(), 1);
        assert_eq!(arena.chunks[0].used, 8*4);
        arena.alloc(8u8);
        assert_eq!(arena.chunks[0].used, 8*4+1);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 50);
        assert_eq!(arena.chunks[0].used, 8*10);
        let s = unsafe {slice::from_raw_parts(p, l)};
        assert_eq!(s, [10, 20, 30, 40, 50]);
    }

    #[test]
    fn string_table() {
        let mut t = StringTable::new();
        t.add_str("hello", 10);
        assert_eq!(t.arena.chunks.len(), 1);
        t.add(&vec![b'-'; CHUNK_SIZE - 2], 20);
        assert_eq!(t.arena.chunks.len(), 2);
        t.add_str("world", 30);
        {
            let mut w = t.write();
            w.write(&vec![b'+'; CHUNK_SIZE - 13]).unwrap();
            w.finish(40);
        }
        assert_eq!(t.arena.chunks.len(), 2);
        let mut w = t.write();
        assert_eq!(w.arena.chunks.len(), 3);
        w.write(&vec![b'%'; CHUNK_SIZE - 1]).unwrap();
        w.write(&[]).unwrap();
        assert_eq!(w.arena.chunks.len(), 3);
        w.write(&[b'%']).unwrap();
        assert_eq!(w.arena.chunks.len(), 4);
        w.write(&[b'%'; CHUNK_SIZE/2]).unwrap();
        w.finish(50);
        assert_eq!(t.arena.chunks.len(), 4);

        assert_eq!(t.strings.len(), 5);
        assert_eq!(t.strings[0], StringRef {s: b"hello".as_slice(), id: 10});
        assert_eq!(t.strings[1], StringRef {s: b"world".as_slice(), id: 30});
        assert_eq!(t.strings[2], StringRef {s: vec![b'+'; CHUNK_SIZE - 13].as_slice(), id: 40});
        assert_eq!(t.strings[3], StringRef {s: vec![b'-'; CHUNK_SIZE - 2].as_slice(), id: 20});
        assert_eq!(t.strings[4], StringRef {s: vec![b'%'; CHUNK_SIZE + CHUNK_SIZE/2].as_slice(), id: 50});

        assert_eq!(t.arena.used(), CHUNK_SIZE*3 + CHUNK_SIZE/2 - 13 - 2 + "hello".len() + "world".len() + 5);
    }
}
