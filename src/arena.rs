use std::{mem, mem::MaybeUninit, ptr, alloc::{alloc, Layout, handle_alloc_error, dealloc}, slice, marker::PhantomData, io, path::Path, fmt, os::unix::ffi::OsStrExt, io::Write as ioWrite, ptr::NonNull, ops::{Deref, DerefMut, Index, IndexMut}, ops};

// Arena - a simple untyped arena.
// StringTable - arena equipped with a list of pointers to allocated strings, allowing search.
//               E.g. put all type names in it, each associated with an id, then iterate over them sequentially for fast search.
//
// Rust borrow checker really doesn't combine well with arenas, so we try to use arrays or pools where possible.
// But for the type system I couldn't come up with any reasonable way to avoid arenas; I tried implementing it with pools,
// but it quickly got out of hand because types from different pools may interact and even reference each other (when temporary types are introduced in watch expressions).
// So we use arenas for that, with some manual transmuting of lifetimes and lots of unsafe dereferencing of *const TypeInfo.

#[derive(Default)]
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
    allocated: usize,
    capacity: usize, // currently always equal to `allocated`, but we can make it smaller for SIMD padding if needed
    used: usize,
}
unsafe impl Send for Chunk {}
unsafe impl Sync for Chunk {}

const CHUNK_SIZE: usize = 1 << 18;

impl Arena {
    pub fn new() -> Self { Self::default() }

    pub fn alloc(&mut self, size: usize, align: usize) -> &mut [u8] {
        assert!(align != 0 && (align - 1) & align == 0);
        let chunk_idx = self.pick_chunk(size + align - 1, size);
        let c = &mut self.chunks[chunk_idx];
        let start = (c.used + align - 1) & !(align - 1);
        c.used = start + size;
        unsafe {slice::from_raw_parts_mut(c.data.add(c.used-size), size)}
    }

    pub fn alloc_slice<T>(&mut self, len: usize) -> &mut [MaybeUninit<T>] {
        assert!(!mem::needs_drop::<T>());
        unsafe {
            let r: *mut MaybeUninit<T> = self.alloc(mem::size_of::<T>() * len, mem::align_of::<T>()).as_mut_ptr() as _;
            slice::from_raw_parts_mut(r, len)
        }
    }

    // Copies an value into this arena, bytewise. Immutable version casts away the lifetime.
    // Mutable version doesn't, to prevent UB if the caller tries to iterate over the arena while still holding a mutable reference to its element, contradicting the reference.
    // Source: https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html ("The precise Rust aliasing rules are somewhat in flux, but the main points are not contentious: [...]")
    pub fn add_mut<T>(&mut self, val: T) -> &mut T {
        assert!(!mem::needs_drop::<T>());
        let r: &mut T = unsafe {&mut *(self.alloc(mem::size_of::<T>(), mem::align_of::<T>()).as_mut_ptr() as *mut T)};
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
                    *(c.data.add(c.used) as *mut T) = add;
                    c.used += size;
                    *len += 1;
                    return;
                }
            }
            if let Some(c) = self.chunks.last_mut() {
                if end == c.data.add(c.used) && c.used + size <= c.capacity {
                    *(c.data.add(c.used) as *mut T) = add;
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
    pub unsafe fn iter_mut<'a, T: 'a>(&'a mut self) -> impl Iterator<Item = &'a mut T> { IterMut {arena: self, chunk_idx: 0, offset: 0, _boo: PhantomData::default()} }

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
        let allocated = capacity;
        let layout = Layout::from_size_align(allocated, 1).unwrap();
        let p = unsafe { alloc(layout) };
        if p.is_null() {
            handle_alloc_error(layout);
        }
        let c = Chunk {data: p, allocated, capacity, used: 0};

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
        unsafe { dealloc(self.data, Layout::from_size_align(self.allocated as usize, 1).unwrap()); }
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
            let r: &'a T = unsafe {&*(chunk.data.add(self.offset) as *const T)};
            self.offset += entry_size;
            return Some(r)
        }
    }
}

struct IterMut<'a, T: 'a> {
    arena: &'a mut Arena,
    chunk_idx: usize,
    offset: usize,
    _boo: PhantomData<&'a mut [T]>,
}
impl<'a, T: 'a> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let entry_size = mem::size_of::<T>();
        loop {
            if self.chunk_idx >= self.arena.chunks.len() {
                return None;
            }
            let chunk = &mut self.arena.chunks[self.chunk_idx];
            if self.offset + entry_size > chunk.used {
                self.chunk_idx += 1;
                self.offset = 0;
                continue;
            }
            let r: &'a mut T = unsafe {&mut *(chunk.data.add(self.offset) as *mut T)};
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

    pub fn binary_search(&self, s: &[u8]) -> Option</*id*/ usize> {
        let i = self.strings.partition_point(|x| x.s < s);
        if self.strings.get(i).is_some_and(|x| x.s == s) {
            Some(self.strings[i].id)
        } else {
            None
        }
    }
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

// Small helper for appending things like "#2", "#3", etc to names of type/variable/etc to make them unique.
// Notice that:
//  * '#' is less than any character we expect to see in type/variable names (there are 3 printable characters less than '#': '"', '!', and ' ').
//  * Suffixes in the same group of equal names have the same length, e.g. "x#01", "x#02", ..., "x#42".
// Together these two properties ensure that a sorted list of names remains sorted after appending these suffixes.
#[derive(Clone, Copy)]
pub struct DisambiguatingSuffixes {
    pub num_digits: usize,
}
impl DisambiguatingSuffixes {
    pub fn new(count: usize) -> Self {
        let num_digits = if count > 1 {
            ((count + 1) as f64).log10().ceil() as usize
        } else {
            0
        };
        Self {num_digits}
    }

    pub fn disambiguate(self, s: &str, idx: usize, id: usize, table: &mut StringTable) -> (&'static str, /*changed*/ bool) {
        if idx == 0 {
            (table.add_str(s, id), false)
        } else {
            let mut out = table.write();
            write!(out, "{}#{:0>2$}", s, idx + 1, self.num_digits).unwrap();
            (out.finish_str(id), true)
        }
    }
}


// Like Vec but allocates memory directly with mmap/mremap. This:
//  * avoids the cost of memcpy on growth and shrink_to_fit,
//  * reliably returns memory to the OS when BigVec is destroyed; useful for temp memory in symbols loading,
// but:
//  * high overhead of creating/destroying BigVec,
//  * some cost if there are lots of BigVec-s (e.g. 100k) as each one is a linux vma, e.g. listed in /proc/<pid>/maps,
//  * slow first access to contents because of page faults; for memory from regular allocator, this cost is avoided when reusing previously freed memory;
//    this is usually fine for symbols loading as it allocates lots of memory quickly, there's wouldn't be much reuse of previously freed memory.
//
// But for some reason this makes no difference for performance in practice.
pub struct BigVec<T> {
    ptr: NonNull<T>,
    len: usize,
    cap: usize,
    _marker: PhantomData<T>,
}
impl<T> BigVec<T> {
    const INITIAL_SIZE: usize = 64 * 1024 * 1024;
    const GROWTH_FACTOR: usize = 8;
    const ELEMENT_BYTES: usize = if mem::size_of::<T>() == 0 {1} else {mem::size_of::<T>()};

    pub fn new() -> Self {
        let cap = (Self::INITIAL_SIZE / Self::ELEMENT_BYTES).max(1);

        let ptr = unsafe {libc::mmap(std::ptr::null_mut(), cap * Self::ELEMENT_BYTES, libc::PROT_READ | libc::PROT_WRITE, libc::MAP_PRIVATE | libc::MAP_ANONYMOUS, -1, 0)};
        if ptr == libc::MAP_FAILED { panic!("mmap failed"); }

        Self {ptr: NonNull::new(ptr as *mut T).unwrap(), len: 0, cap, _marker: PhantomData}
    }

    pub fn len(&self) -> usize { self.len }
    pub fn is_empty(&self) -> bool {self.len == 0}
    pub fn capacity(&self) -> usize {self.cap}

    pub fn last(&self) -> Option<&T> { if self.len == 0 {None} else {unsafe { Some(&*self.ptr.as_ptr().add(self.len - 1)) }} }
    pub fn last_mut(&mut self) -> Option<&mut T> { if self.len == 0 {None} else {unsafe { Some(&mut *self.ptr.as_ptr().add(self.len - 1)) }} }

    pub fn push(&mut self, value: T) {
        if self.len >= self.cap {
            assert!(self.len == self.cap);
            self.change_capacity(self.cap * Self::GROWTH_FACTOR);
            assert!(self.len < self.cap);
        }
        unsafe {
            self.ptr.as_ptr().add(self.len).write(value);
        }
        self.len += 1;
    }

    pub fn append(&mut self, from: &mut Vec<T>) {
        let new_len = self.len + from.len();
        if new_len > self.cap {
            let mut new_cap = self.cap;
            while new_len > new_cap {
                new_cap *= Self::GROWTH_FACTOR;
            }
            self.change_capacity(new_cap);
        }
        for x in mem::take(from).into_iter() {
            unsafe {
                self.ptr.as_ptr().add(self.len).write(x);
            }
            self.len += 1;
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(self.ptr.as_ptr().add(self.len).read()) }
        }
    }

    pub fn clear(&mut self) {
        unsafe {
            std::ptr::drop_in_place(std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len));
        }
        self.len = 0;
    }

    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        let mut write_idx = 0;
        for read_idx in 0..self.len {
            unsafe {
                let elem = self.ptr.as_ptr().add(read_idx);
                if f(&*elem) {
                    if read_idx != write_idx {
                        let dest = self.ptr.as_ptr().add(write_idx);
                        std::ptr::copy_nonoverlapping(elem, dest, 1);
                    }
                    write_idx += 1;
                } else {
                    std::ptr::drop_in_place(elem);
                }
            }
        }
        self.len = write_idx;
    }

    pub fn shrink_to_fit(&mut self) {
        if self.cap > self.len {
            self.change_capacity(self.len);
        }
    }

    fn change_capacity(&mut self, new_cap: usize) {
        assert!(new_cap >= self.len);
        let new_cap = new_cap.max(1);
        let old_cap_bytes = self.cap * Self::ELEMENT_BYTES;

        let new_ptr = unsafe {libc::mremap(self.ptr.as_ptr() as *mut libc::c_void, old_cap_bytes, new_cap * Self::ELEMENT_BYTES, libc::MREMAP_MAYMOVE)};
        if new_ptr == libc::MAP_FAILED {panic!("mremap failed");}

        self.ptr = NonNull::new(new_ptr as *mut T).unwrap();
        self.cap = new_cap;
    }
}
impl<T> Drop for BigVec<T> {
    fn drop(&mut self) {
        self.clear();
        let r = unsafe {libc::munmap(self.ptr.as_ptr() as *mut libc::c_void, self.cap * mem::size_of::<T>().max(1))};
        if r != 0 {panic!("munmap failed");}
    }
}
impl<T> Deref for BigVec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}
impl<T> DerefMut for BigVec<T> { fn deref_mut(&mut self) -> &mut [T] {unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }} }
impl<T> Index<usize> for BigVec<T> {type Output = T; fn index(&self, idx: usize) -> &T {&(**self)[idx]} }
impl<T> IndexMut<usize> for BigVec<T> {fn index_mut(&mut self, idx: usize) -> &mut T {&mut (**self)[idx]} }
impl<T> Default for BigVec<T> { fn default() -> Self {Self::new()} }
unsafe impl<T: Send> Send for BigVec<T> {}
unsafe impl<T: Sync> Sync for BigVec<T> {}

impl<T> Index<ops::Range<usize>> for BigVec<T> { type Output = [T]; fn index(&self, range: ops::Range<usize>) -> &[T] {&(**self)[range]} }
impl<T> IndexMut<ops::Range<usize>> for BigVec<T> { fn index_mut(&mut self, range: ops::Range<usize>) -> &mut [T] {&mut (**self)[range]} }
impl<T> Index<ops::RangeFrom<usize>> for BigVec<T> { type Output = [T]; fn index(&self, range: ops::RangeFrom<usize>) -> &[T] {&(**self)[range]} }
impl<T> IndexMut<ops::RangeFrom<usize>> for BigVec<T> { fn index_mut(&mut self, range: ops::RangeFrom<usize>) -> &mut [T] {&mut (**self)[range]} }
impl<T> Index<ops::RangeTo<usize>> for BigVec<T> { type Output = [T]; fn index(&self, range: ops::RangeTo<usize>) -> &[T] {&(**self)[range]} }
impl<T> IndexMut<ops::RangeTo<usize>> for BigVec<T> { fn index_mut(&mut self, range: ops::RangeTo<usize>) -> &mut [T] {&mut (**self)[range]} }
impl<T> Index<ops::RangeFull> for BigVec<T> { type Output = [T]; fn index(&self, _: ops::RangeFull) -> &[T] {self} }
impl<T> IndexMut<ops::RangeFull> for BigVec<T> { fn index_mut(&mut self, _: ops::RangeFull) -> &mut [T] {self} }
impl<T> Index<ops::RangeInclusive<usize>> for BigVec<T> { type Output = [T]; fn index(&self, range: ops::RangeInclusive<usize>) -> &[T] {&(**self)[range]} }
impl<T> IndexMut<ops::RangeInclusive<usize>> for BigVec<T> { fn index_mut(&mut self, range: ops::RangeInclusive<usize>) -> &mut [T] {&mut (**self)[range]} }
impl<T> Index<ops::RangeToInclusive<usize>> for BigVec<T> { type Output = [T]; fn index(&self, range: ops::RangeToInclusive<usize>) -> &[T] {&(**self)[range]} }
impl<T> IndexMut<ops::RangeToInclusive<usize>> for BigVec<T> { fn index_mut(&mut self, range: ops::RangeToInclusive<usize>) -> &mut [T] {&mut (**self)[range]} }

impl<'a, T> IntoIterator for &'a BigVec<T> { type Item = &'a T; type IntoIter = std::slice::Iter<'a, T>; fn into_iter(self) -> Self::IntoIter {self.iter()} }
impl<'a, T> IntoIterator for &'a mut BigVec<T> { type Item = &'a mut T; type IntoIter = std::slice::IterMut<'a, T>; fn into_iter(self) -> Self::IntoIter {self.iter_mut()} }

//pub type BigVec<T> = Vec<T>;


#[cfg(test)]
mod tests {
    use crate::arena::*;
     #[test]
    fn arena_push() {
        let mut arena = Arena::new();
        let mut p: *const u64 = [].as_ptr();
        let mut l: usize = 0;
        arena.push_to_array_likely_at_end(&mut p, &mut l, 10);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 20);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 30);
        arena.push_to_array_likely_at_end(&mut p, &mut l, 40);
        assert_eq!(arena.chunks.len(), 1);
        assert_eq!(arena.chunks[0].used, 8*4);
        arena.add(8u8);
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
        t.add_slice(&vec![b'-'; CHUNK_SIZE - 2], 20);
        assert_eq!(t.arena.chunks.len(), 2);
        t.add_str("world", 30);
        {
            let mut w = t.write();
            w.write(&vec![b'+'; CHUNK_SIZE - 13]).unwrap();
            w.finish(40);
        }
        assert_eq!(t.arena.chunks.len(), 3);
        let mut w = t.write();
        assert_eq!(w.w.arena.chunks.len(), 3);
        w.write(&vec![b'%'; CHUNK_SIZE - 1]).unwrap();
        w.write(&[]).unwrap();
        assert_eq!(w.w.arena.chunks.len(), 4);
        w.write(&[b'%']).unwrap();
        assert_eq!(w.w.arena.chunks.len(), 4);
        w.write(&[b'%'; CHUNK_SIZE/2]).unwrap();
        w.finish(50);
        assert_eq!(t.arena.chunks.len(), 4);

        assert_eq!(t.strings.len(), 5);
        assert_eq!(t.strings[0], StringRef {s: b"hello".as_slice(), id: 10});
        assert_eq!(t.strings[2], StringRef {s: b"world".as_slice(), id: 30});
        assert_eq!(t.strings[3].s, vec![b'+'; CHUNK_SIZE - 13].as_slice());
        assert_eq!(t.strings[3].id, 40);
        assert_eq!(t.strings[1].s, vec![b'-'; CHUNK_SIZE - 2].as_slice());
        assert_eq!(t.strings[1].id, 20);
        assert_eq!(t.strings[4].s, vec![b'%'; CHUNK_SIZE + CHUNK_SIZE/2].as_slice());
        assert_eq!(t.strings[4].id, 50);

        assert_eq!(t.arena.used(), CHUNK_SIZE*3 + CHUNK_SIZE/2 - 13 - 2 + "hello".len() + "world".len());
    }
}
