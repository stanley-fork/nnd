use crate::{*, error::*, elf::*, util::*, log::*};
use std::io::{BufReader, BufRead};
use std::{fs, fs::File, os::fd::{OwnedFd, AsRawFd}, str::FromStr, ops::Range, cmp::Ordering, collections::HashSet, mem::MaybeUninit};
use bitflags::*;
use libc::{pid_t, c_void};

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Default, Clone, Debug)]
pub struct BinaryId {
    pub path: String,
    // Inode number, to notice if binary version doesn't match, e.g. if it was recompiled.
    // Doesn't notice if the file was modified in place, but that hardly ever happens in practice.
    // For vdso, this is pid instead of inode.
    pub inode: u64,

    // If it's an executable segment not backed by an ELF file.
    pub special: SpecialSegmentId,
}
impl BinaryId {
    // When a BinaryId is saved and loaded across debugger restarts, we don't want to save inode number or vdso address, because they may change (e.g. if the binary was recompiled).
    // So a loaded BinaryId is "incomplete" and needs to be searched in the list of loaded binaries at runtime.
    pub fn save_state_incomplete(&self, out: &mut Vec<u8>) -> Result<()> {
        out.write_str(&self.path)?;
        out.write_u8(self.special.is_vdso() as u8)?;
        Ok(())
    }
    pub fn load_state_incomplete(inp: &mut &[u8]) -> Result<Self> {
        Ok(Self {path: inp.read_str()?, inode: 0, special: if inp.read_u8()? == 1 {SpecialSegmentId::Vdso(0..0)} else {SpecialSegmentId::None}})
    }
    pub fn matches_incomplete(&self, incomplete: &BinaryId) -> bool {
        (&self.path, self.special.is_vdso()) == (&incomplete.path, incomplete.special.is_vdso())
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum SpecialSegmentId {
    None,
    Vdso(Range<usize>),
}
impl Default for SpecialSegmentId { fn default() -> Self { Self::None } }
impl Ord for SpecialSegmentId {
    fn cmp(&self, other: &Self) -> Ordering {
        let key = |s: &Self| -> (usize, usize, usize) { match s { Self::None => (0, 0, 0), Self::Vdso(r) => (1, r.start, r.end) } };
        key(self).cmp(&key(other))
    }
}
impl PartialOrd for SpecialSegmentId { fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) } }
impl SpecialSegmentId {
    fn is_vdso(&self) -> bool { match self { Self::Vdso(_) => true, _ => false } }
}

pub struct MemMapInfo {
    pub start: usize,
    pub len: usize,
    pub perms: MemMapPermissions,
    pub offset: usize,
    pub inode: u64,
    pub path: Option<String>,

    // Just `path` and `inode` bundled together, but present only if this appears to be an executable or shared library.
    pub binary_id: Option<BinaryId>,
}

bitflags! {
    pub struct MemMapPermissions: u8 {
        const READ = 0b00001;
        const WRITE = 0b00010;
        const EXECUTE = 0b00100;
        const SHARED = 0b01000;
        const PRIVATE = 0b10000;
    }
}

// Information from /proc/[pid]/maps
#[derive(Default)]
pub struct MemMapsInfo {
    pub maps: Vec<MemMapInfo>, // sorted by address
}

// ELF file contains virtual memory addresses for segments. But actual addresses at which segments are mapped at runtime (by the ELF loader) may be different, especially for dynamic libraries (PIC).
// We'll call these "static" vs "dynamic" addresses. This struct is responsible for mapping between the two.
// We expect that the difference between dynamic and static addresses is consistent across all sections that we care about (.text, .data, etc).
// So, the "mapping" is just one number that needs to be added/subtracted. (It gets a whole struct because it turned out more convenient this way, and in case this mapping turns out to be more complex on other platforms.)
#[derive(Clone)]
pub struct AddrMap {
    // `dynamic - static`. May be "negative", use wrapping_add.
    diff: usize,
    known: bool,
    // If different sections are offset by a different amount, this is set to false, so that we can render some warning in the UI or something.
    // Not to be confused with the difference between address and file offset - this difference may be different for different sections, we make no assumptions about that.
    consistent: bool,
}

// Thing for reading debuggee's memory.
#[derive(Clone)]
pub struct MemReader {
    pid: pid_t,
}

impl AddrMap {
    pub fn new() -> Self {
        Self {diff: 0, known: false, consistent: true}
    }

    pub fn update(&mut self, map: &MemMapInfo, elf: &ElfFile, /* just for logging */ path: &str) {
        // We assume that all mmaps backed by ELF files are created by the ELF loader, and so correspond to ELF sections and have consistent difference between dynamic and static addresses.
        // (So if the program manually mmap()s its own executable or some dynamic library it's using, we may get incorrect `diff` value and fail to use debug symbols correctly and even fail to unwind stacks correctly.)
        // For now we just look at .text section (and other executable sections, which are not important). Would be nice to check all sections/segments (in particular, .data, .rodata, .got) and print warnings if their `diff` values are different;
        // this would be done by guessing the mmap <-> segment correspondence by looking at both addresses and flags (writable, executable).
        if !map.perms.contains(MemMapPermissions::EXECUTE) {
            return;
        }
        match &map.binary_id.as_ref().unwrap().special {
            SpecialSegmentId::None => (),
            SpecialSegmentId::Vdso(range) => {
                self.diff = range.start;
                self.known = true;
                return;
            }
        }
        let mut idx = elf.section_by_offset.partition_point(|t| t.0 < map.offset);
        while idx < elf.section_by_offset.len() && elf.section_by_offset[idx].1 <= map.offset + map.len {
            let s = &elf.sections[elf.section_by_offset[idx].2];
            if s.section_type != SHT_PROGBITS || s.flags & SHF_EXECINSTR == 0 {
                idx += 1;
                continue;
            }
            let diff = map.start.wrapping_sub(map.offset).wrapping_sub(s.address.wrapping_sub(s.offset));
            if !self.known {
                self.diff = diff;
                self.known = true;
            } else if diff != self.diff && self.consistent {
                eprintln!("warning: inconsistent address differences between sections: section {} has diff {}, some other section has diff {} in {}", s.name, diff, self.diff, path);
                self.consistent = false;
            }
            idx += 1;
        }
    }

    pub fn static_to_dynamic(&self, s: usize) -> usize {
        s.wrapping_add(self.diff)
    }

    pub fn dynamic_to_static(&self, d: usize) -> usize {
        d.wrapping_sub(self.diff)
    }
}

impl MemMapsInfo {
    pub fn read_proc_maps(pid: pid_t) -> Result<MemMapsInfo> {
        let reader = BufReader::new(File::open(format!("/proc/{}/maps", pid))?);
        let mut res: Vec<MemMapInfo> = Vec::new();
        let mut executables: HashSet<BinaryId> = HashSet::new();
        for line in reader.lines() {
            let line = line?;
            
            // The last field of the line is path. It can contain spaces (including trailing).
            // So we can't just use line.split_whitespace() (SplitWhitespace.remainder() is currently nightly-only).
            // We also can't just use splitn(5) because it doesn't skip repeated spaces.
            // We also can't just re-concatenate split_whitespace() trailing tokens because that loses repeated and trailing spaces in path.

            let mut s = line.trim_start().splitn(2, ' ');
            let (range, rest) = (s.next(), s.next());
            if rest.is_none() { return err!(Format, "too few fields"); }

            let mut s = range.unwrap().splitn(2, '-');
            let (start, end) = (s.next(), s.next());
            if end.is_none() { return err!(Format, "bad range"); }
            let start = usize::from_str_radix(start.unwrap(), 16)?;
            let end = usize::from_str_radix(end.unwrap(), 16)?;

            let mut s = rest.unwrap().trim_start().splitn(2, ' ');
            let (perms, rest) = (s.next(), s.next());
            if rest.is_none() { return err!(Format, "too few fields"); }
            let mut permissions = MemMapPermissions::empty();
            for ch in perms.unwrap().chars() {
                match ch {
                    'r' => permissions.insert(MemMapPermissions::READ),
                    'w' => permissions.insert(MemMapPermissions::WRITE),
                    'x' => permissions.insert(MemMapPermissions::EXECUTE),
                    's' => permissions.insert(MemMapPermissions::SHARED),
                    'p' => permissions.insert(MemMapPermissions::PRIVATE),
                    _ => (),
                }
            }

            let mut s = rest.unwrap().trim_start().splitn(2, ' ');
            let (offset, rest) = (s.next(), s.next());
            if rest.is_none() { return err!(Format, "too few fields"); }
            let offset = usize::from_str_radix(offset.unwrap(), 16)?;

            let mut s = rest.unwrap().trim_start().splitn(2, ' ');
            let (_dev, rest) = (s.next(), s.next());
            if rest.is_none() { return err!(Format, "too few fields"); }

            let mut s = rest.unwrap().trim_start().splitn(2, ' ');
            let (inode, rest) = (s.next(), s.next());
            if inode.is_none() { return err!(Format, "too few fields"); }
            let inode = u64::from_str_radix(inode.unwrap(), 10)?;

            let path = match rest { None => None, Some(p) => Some(p.trim_start().to_string()) };

            let mut binary_id: Option<BinaryId> = None;
            if let Some(p) = &path {
                if p == "[vdso]" && permissions.contains(MemMapPermissions::EXECUTE) {
                    binary_id = Some(BinaryId {path: p.clone(), inode: pid as u64, special: SpecialSegmentId::Vdso(start..end)});
                } else if !p.is_empty() && !p.starts_with('[') {
                    binary_id = Some(BinaryId {path: p.clone(), inode, special: SpecialSegmentId::None});
                }
                // We currently ignore executable anon (p.is_empty()) mappings, e.g. JITted code.
            }
            if permissions.contains(MemMapPermissions::EXECUTE) {
                if let Some(id) = &binary_id {
                    executables.insert(id.clone());
                }
            }

            res.push(MemMapInfo {start, len: end - start, perms: permissions, offset, inode, path, binary_id});
        }

        // Distinguish dynamic libraries vs mmapped data files. If the file is mapped as executable at least once,
        // consider it an executable file and assign binary_id for all its mapped segments (including non-executable, e.g. .rodata).
        for m in &mut res {
            if let Some(id) = &m.binary_id {
                if !executables.contains(id) {
                    m.binary_id = None;
                }
            }
        }

        res.sort_by_key(|m| m.start);

        Ok(MemMapsInfo {maps: res})
    }

    pub fn list_binaries(&self) -> Vec<BinaryId> {
        let mut v: Vec<(usize, &BinaryId)> = Vec::new();
        for i in 0..self.maps.len() {
            let id = match &self.maps[i].binary_id {
                None => continue,
                Some(i) => i };
            v.push((i, id));
        }
        v.sort_by_key(|t| t.1);
        v.dedup_by_key(|t| t.1);
        v.sort_by_key(|(i, _)| *i);
        v.iter().map(|t| t.1).cloned().collect()
    }

    pub fn addr_to_map(&self, addr: usize) -> Option<&MemMapInfo> {
        let idx = self.maps.partition_point(|m| m.start + m.len <= addr);
        if idx < self.maps.len() && self.maps[idx].start <= addr {
            Some(&self.maps[idx])
        } else {
            None
        }
    }

    pub fn offset_to_map(&self, binary_id: &BinaryId, offset: usize) -> Option<&MemMapInfo> {
        self.maps.iter().find_map(|m| if m.binary_id.as_ref() == Some(binary_id) && m.offset <= offset && m.offset + m.len > offset { Some(m) } else { None })
    }

    pub fn clear(&mut self) { self.maps.clear(); }
}

impl MemReader {
    pub fn new(pid: pid_t) -> Self {
        MemReader {pid: pid}
    }

    pub fn invalid() -> Self {
        MemReader {pid: 0}
    }

    pub fn read_uninit<'a>(&self, offset: usize, buf: &'a mut [MaybeUninit<u8>]) -> Result<&'a mut [u8]> {
        unsafe {
            let local_iov = libc::iovec {iov_base: buf.as_mut_ptr() as *mut c_void, iov_len: buf.len()};
            let mut remote_iov = libc::iovec {iov_base: offset as *mut c_void, iov_len: buf.len()};
            let r = libc::process_vm_readv(self.pid, &local_iov as *const libc::iovec, 1, &mut remote_iov as *mut libc::iovec, 1, 0);
            if r < 0 {
                if unsafe {*libc::__errno_location()} == libc::EFAULT {
                    return err!(ProcessState, "bad address"); // shorter message for common error (e.g. null pointer dereference in watch expression)
                } else {
                    return errno_err!("process_vm_readv failed");
                }
            }
            if r != buf.len() as isize {
                return err!(ProcessState, "unexpected EOF in mem @{:x}:0x{:x}", offset, buf.len());
            }
            Ok(std::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len()))
        }
    }

    pub fn read(&self, offset: usize, buf: &mut [u8]) -> Result<()> {
        unsafe {self.read_uninit(offset, std::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut MaybeUninit<u8>, buf.len()))}?;
        Ok(())
    }

    pub fn read_u8(&self, offset: usize) -> Result<u8> {
        let mut buf = [0u8; 1];
        self.read(offset, &mut buf)?;
        Ok(buf[0])
    }

    pub fn read_u64(&self, offset: usize) -> Result<u64> {
        let mut buf = [0u8; 8];
        self.read(offset, &mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    // The address range must be flagged as writable, so we can't use this to write to the code.
    pub fn write(&self, offset: usize, buf: &[u8]) -> Result<()> {
        unsafe {
            let local_iov = libc::iovec {iov_base: buf.as_ptr() as *mut c_void, iov_len: buf.len()};
            let mut remote_iov = libc::iovec {iov_base: offset as *mut c_void, iov_len: buf.len()};
            let r = libc::process_vm_writev(self.pid, &local_iov as *const _, 1, &mut remote_iov as *mut _, 1, 0);
            if r < 0 { return errno_err!("process_vm_writev failed"); }
            if r != buf.len() as isize { return err!(ProcessState, "unexpected EOF when writing mem @{:x}:0x{:x}", offset, buf.len()); }
            Ok(())
        }
    }

    pub fn write_u8(&self, offset: usize, v: u8) -> Result<()> {
        self.write(offset, &[v])
    }
    
    pub fn write_u64(&self, offset: usize, v: u64) -> Result<()> {
        self.write(offset, &v.to_le_bytes())
    }
}

pub fn peak_memory_usage_of_current_process() -> Result<usize> {
    let reader = BufReader::new(File::open("/proc/self/status")?);
    for line in reader.lines() {
        let line = line?;
        let mut s = line.split_whitespace();
        let name = s.next();
        if name != Some("VmHWM:") {
            continue;
        }
        let (number, unit, extra) = (s.next(), s.next(), s.next());
        if let Some(x) = extra {
            return err!(Format, "unexpected tokens in VmHWM line: {}", x);
        }
        if unit != Some("kB") {
            return err!(Format, "unexpected unit in VmHWM line: {}", unit.unwrap_or("<none>"));
        }
        let number = usize::from_str(number.unwrap())?;
        return Ok(number << 10);
    }
    err!(Format, "No VmHWM line")
}

// Contents of /proc/[pid]/stat
pub struct ProcStat {
    comm: [u8; 33], // length, then bytes
    pub state: char,
    pub utime: usize, // in _SC_CLK_TCK units
    pub stime: usize,
    rss: usize, // in pages
}
impl Default for ProcStat { fn default() -> Self { Self {comm: [0; 33], state: '?', utime: 0, stime: 0, rss: 0} } }
impl ProcStat {
    pub fn parse(path: &str, prof: &mut ProfileBucket) -> Result<ProcStat> {
        let timer = TscScope::new();
        let line = std::fs::read_to_string(path)?;
        prof.syscall_count += 1;
        prof.syscall_tsc += timer.finish();

        // We panic if parsing fails. The data comes from the kernel, not from a real file, so it should always be valid.

        let open_paren = line.char_indices().find(|(_, c)| *c == '(').unwrap().0;
        let close_paren = line.char_indices().rev().find(|(_, c)| *c == ')').unwrap().0;
        let mut comm = [0u8; 33];
        let comm_len = (close_paren - open_paren - 1).min(comm.len() - 1);
        comm[1..1+comm_len].copy_from_slice(&line.as_bytes()[open_paren+1..open_paren+1+comm_len]);
        comm[0] = comm_len as u8;

        let mut tokens = line[close_paren+1..].split_whitespace();

        let state = tokens.next().unwrap();
        assert!(state.len() == 1);
        let state = state.chars().next().unwrap();

        let utime = usize::from_str(tokens.nth(10).unwrap())?;
        let stime = usize::from_str(tokens.nth(0).unwrap())?;
        let rss = usize::from_str(tokens.nth(8).unwrap())?;

        Ok(ProcStat {comm, state, utime, stime, rss})
    }

    // Thread name.
    pub fn comm(&self) -> &str {
        std::str::from_utf8(&self.comm[1..1+self.comm[0] as usize]).unwrap() // valid by construction
    }

    pub fn rss_bytes(&self) -> usize {
        self.rss * sysconf_PAGE_SIZE()
    }
}

pub fn list_threads(pid: pid_t) -> Result<Vec<pid_t>> {
    let mut r: Vec<pid_t> = Vec::new();
    for entry in fs::read_dir(format!("/proc/{}/task/", pid))? {
        let entry = entry?;
        r.push(pid_t::from_str(&entry.file_name().into_string().unwrap()).unwrap());
    }
    Ok(r)
}
