use crate::{*, error::*, log::*, util::*, registers::*, procfs::*, process_info::*, os::*};
use std::{fs::{File}, mem, mem::MaybeUninit, io::{self, BufReader, SeekFrom, Seek, Read, BufRead}, sync::{Arc, OnceLock}, collections::{HashMap, hash_map::Entry}, str, ptr, fmt::Debug, fmt, result, slice, ops::Range};
use libc::pid_t;

pub struct ElfSection {
    pub idx: usize,
    pub name: String,

    pub section_type: u32,
    pub flags: u64,

    pub address: usize,
    pub offset: usize,
    pub size: usize,

    pub link: u32,
    pub info: u32,
    pub alignment: usize, // keep in mind that if SHF_COMPRESSED is set this is alignment in compressed, not decompressed data
    pub entry_size: usize,

    pub name_offset_in_strtab: u32,

    // If flags has SHF_COMPRESSED.
    pub compression_header: Option<libc::Elf64_Chdr>,
    pub decompressed_data: OnceLock<Result<Vec<u8>>>,
}

pub struct ElfSegment {
    pub idx: usize,
    pub segment_type: u32,
    pub flags: u32,
    pub offset: usize,
    pub address: usize,
    pub size_in_file: usize,
    pub size_in_memory: usize,
    pub alignment: usize,
}

pub struct ElfNote<'a> {
    pub type_: u32,
    pub name: &'a[u8],
    pub desc: &'a[u8],
}

pub struct ElfFile {
    pub name: String, // just for logging

    pub segments: Vec<ElfSegment>,
    pub sections: Vec<ElfSection>,
    pub entry_point: usize,

    pub section_by_offset: Vec<(usize, usize, usize)>, // (offset, offset + size, index in `sections`), sorted, only mapped sections with nonzero size in file are included
    pub section_by_name: HashMap<String, usize>,
    pub text_section: Option<usize>,

    pub is_core_dump: bool,
    pub is_reconstructed: bool, // fake ELF assembled from pieces available in a core dump

    pub build_id: Option<Vec<u8>>,

    // Path to dynamic linker, from .interp section.
    pub interp: Option<String>,

    // Static address at which the dynamic linker will patch the pointer to r_debug struct, if this is the main executable (as opposed to a dynamic library).
    pub r_debug_ptr_addr: Option<usize>,

    // Slightly longer than the file because of padding.
    mmapped: Option<Mmap>,
    owned: Vec<u8>,
    // Length equal to file size.
    data: &'static [u8],
}

pub const ELF_PAD_RIGHT: usize = 4096;

// Where the contents of a range of memory can be found.
#[derive(Clone)]
pub enum CoreDumpMemorySource {
    // In the crashed process this range was inside an anon mmap but not resident in RAM. It would return zeroes if read.
    Zero,
    // One of:
    //  * this range was resident in RAM and so was written to the core dump (then `file` is the core dump), or
    //  * this range was not resident in RAM, and it was a file-backed mmap corresponding to some binary, and we have that binary (`file`).
    File {file: Arc<ElfFile>, offset: usize},
    // This range was not resident in RAM, and it was in a file-backed mmap, and we don't have that file (or don't know how to find it).
    MissingFile,
}

#[derive(Clone)]
pub struct CoreDumpMemoryRange {
    pub start_address: usize,
    pub size: usize,
    pub source: CoreDumpMemorySource,
    pub permissions: MemMapPermissions,
}

pub struct CoreDumpMemReader {
    pub ranges: Vec<CoreDumpMemoryRange>, // sorted by start_address
}

impl ElfFile {
    pub fn data(&self) -> &[u8] {
        self.data as _
    }

    pub unsafe fn bytes_from_strtab(&self, section_offset: usize, offset: usize) -> &[u8] {
        std::ffi::CStr::from_ptr(self.data()[(section_offset + offset) as usize..].as_ptr() as *const i8).to_bytes()
    }

    pub unsafe fn str_from_strtab(&self, section_offset: usize, offset: usize) -> Result<&str> {
        Ok(std::str::from_utf8(self.bytes_from_strtab(section_offset, offset))?)
    }

    pub fn addr_to_offset(&self, addr: usize) -> Option<usize> {
        for s in &self.sections {
            if s.address <= addr && s.address + s.size > addr {
                return Some(addr - s.address + s.offset);
            }
        }
        None
    }

    pub fn addr_range_to_offset_range(&self, start: usize, end: usize) -> Option<(usize, usize)> {
        for s in &self.sections {
            if s.address <= start && s.address + s.size >= end {
                return Some((start - s.address + s.offset, end - s.address + s.offset));
            }
        }
        None
    }

    // The returned reference points either into mmap or into section's decompressed_data.
    pub fn section_data(&self, idx: usize) -> Result<&[u8]> {
        let section = &self.sections[idx];
        Ok(if let Some(header) = &section.compression_header {
            // Decompress lazily to cover the case where a binary with big compressed debug info is passed using --module.
            // We parse ELF headers for such binaries in a blocking way on startup (to get build id), before even showing UI.
            &section.decompressed_data.get_or_init(|| {
                let compressed = &self.data()[section.offset..section.offset + section.size_in_file()];
                let compressed = &compressed[mem::size_of::<libc::Elf64_Chdr>()..];
                let mut decompressed = vec![0u8; header.ch_size as usize + ELF_PAD_RIGHT];
                decompressed.truncate(header.ch_size as usize);

                match header.ch_type {
                    ELFCOMPRESS_ZLIB => {
                        let mut decoder = flate2::read::ZlibDecoder::new(compressed);
                        decoder.read_exact(&mut decompressed)?;
                    }
                    _ => return err!(UnsupportedExecutable, "ELF compression {} not supported", header.ch_type),
                };

                Ok(decompressed)
            }).as_ref_clone_error()?
        } else {
            &self.data()[section.offset..section.offset + section.size_in_file()]
        })
    }

    pub fn decompressed_size(&self) -> usize {
        self.sections.iter().map(|s| s.compression_header.map_or(s.size_in_file(), |h| h.ch_size as usize)).sum()
    }

    pub fn has_section_data(&self, name: &str) -> bool {
        match self.section_by_name.get(name) {
            Some(&idx) => self.sections[idx].section_type != SHT_NOBITS,
            None => false,
        }
    }

    pub fn has_text_section_data(&self) -> bool {
        match &self.text_section {
            &Some(idx) => self.sections[idx].section_type != SHT_NOBITS,
            None => false,
        }
    }

    pub fn from_file(name: String, file: &File, file_len: u64) -> Result<Self> {
        open_elf(name, Some((file, file_len as usize)), Vec::new())
    }

    pub fn from_contents(name: String, contents: Vec<u8>) -> Result<Self> {
        open_elf(name, None, contents)
    }
}

pub fn parse_elf_note<'a>(data: &'a [u8]) -> Result<(ElfNote<'a>, /*remainder*/ &'a [u8])> {
    let mut reader = io::Cursor::new(data);
    let name_len = reader.read_u32()? as usize;
    let desc_len = reader.read_u32()? as usize;
    let type_ = reader.read_u32()?;
    let name_len_padded = (name_len + 3) & !3;
    let desc_len_padded = (desc_len + 3) & !3;
    let pos = reader.position() as usize;
    if pos + name_len_padded + desc_len_padded > data.len() {
        return err!(MalformedExecutable, "ELF note is too short");
    }
    Ok((ElfNote {type_, name: &data[pos..pos+name_len], desc: &data[pos+name_len_padded..pos+name_len_padded+desc_len]}, &data[pos+name_len_padded+desc_len_padded..]))
}

impl ElfSection {
    pub fn size_in_file(&self) -> usize {
        if self.section_type == SHT_NOBITS {
            0
        } else {
            self.size
        }
    }
}

pub fn parse_core_dump(elf: Arc<ElfFile>) -> Result<(CoreDumpMemReader, Vec<(pid_t, ThreadInfo, Option</*signal*/ i32>)>, MemMapsInfo)> {
    if !elf.is_core_dump {
        return err!(Usage, "not a core dump file");
    }

    // Be careful to not confuse two mappings:
    //  * Address <-> core dump offset. For virtual memory ranges that were resident in RAM at the time of crash, and therefore were dumped into the file. This is in elf.segments.
    //  * Address <-> binary+offset. For file-based mmaps that the process had at the time of crash. In particular, loaded dynamic libraries and main executable. This is in NT_FILE note.

    let mut ranges: Vec<CoreDumpMemoryRange> = Vec::new();
    let mut notes: Vec<ElfNote> = Vec::new();
    for segment in &elf.segments {
        if segment.segment_type == PT_LOAD {
            let mut permissions = MemMapPermissions::empty();
            if segment.flags & PF_R != 0 {permissions.insert(MemMapPermissions::READ);}
            if segment.flags & PF_W != 0 {permissions.insert(MemMapPermissions::WRITE);}
            if segment.flags & PF_X != 0 {permissions.insert(MemMapPermissions::EXECUTE);}
            if segment.size_in_file > 0 {
                ranges.push(CoreDumpMemoryRange {start_address: segment.address, size: segment.size_in_file.min(segment.size_in_memory), source: CoreDumpMemorySource::File {file: elf.clone(), offset: segment.offset}, permissions});
            }
            if segment.size_in_file < segment.size_in_memory {
                ranges.push(CoreDumpMemoryRange {start_address: segment.address + segment.size_in_file, size: segment.size_in_memory - segment.size_in_file, source: CoreDumpMemorySource::Zero, permissions});
            }
        } else if segment.segment_type == PT_NOTE {
            let mut data = &elf.data[segment.offset..segment.offset+segment.size_in_file];
            while !data.is_empty() {
                let (note, remainder) = parse_elf_note(data)?;
                data = remainder;
                notes.push(note);
            }
        }
    }

    ranges.sort_unstable_by_key(|r| r.start_address);
    for i in 1..ranges.len() {
        let diff = ranges[i].start_address - ranges[i-1].start_address;
        if ranges[i-1].size > diff {
            eprintln!("warning: overlapping segments in core dump");
            ranges[i-1].size = diff;
        }
    }

    let mut threads: Vec<(pid_t, ThreadInfo, Option<i32>)> = Vec::new();
    let mut maps: Vec<MemMapInfo> = Vec::new();
    for note in notes {
        let name = String::from_utf8_lossy(note.name);
        match note.type_ {
            NT_PRSTATUS => {
                let (prstatus, _) = unsafe {memcpy_struct::<elf_prstatus>(note.desc, "NT_PRSTATUS")}?;
                // Core dump has the same signo on all threads for some reason, but we want to show it only on the thread that received it (which is always listed first).
                let signal = if threads.is_empty() && prstatus.si_signo != 0 {Some(prstatus.si_signo)} else {None};
                let mut extra_regs = LazyExtraRegisters::default();
                extra_regs.set_error(error!(ProcessState, "no simd registers in core dump"));
                threads.push((prstatus.pr_pid, ThreadInfo {regs: Registers::from_ptrace(&prstatus.pr_reg), extra_regs, ..Default::default()}, signal));
            }
            NT_PRPSINFO => {
                let (_prpsinfo, _) = unsafe {memcpy_struct::<elf_prpsinfo>(note.desc, "NT_PRPSINFO")}?;
                // (elf_prpsinfo doesn't seem to have any information useful to us.)
            }
            NT_SIGINFO => {
                let (_siginfo, _) = unsafe {memcpy_struct::<libc::siginfo_t>(note.desc, "NT_SIGINFO")}?;
                // (We take signal number from NT_PRSTATUS instead. We could take `errno` and `code` from the siginfo_t here, if we wanted to show it in the UI somewhere.)
            }
            NT_AUXV => (),
            NT_PRFPREG => (), // prefix of NT_X86_XSTATE
            NT_X86_XSTATE => {
                if threads.is_empty() {
                    return err!(MalformedExecutable, "unexpected NT_X86_XSTATE before any NT_PRSTATUS");
                }
                let extra_regs = ExtraRegisters::from_xsave(note.desc);
                threads.last_mut().unwrap().1.extra_regs.set(extra_regs);
            }
            NT_FILE => {
                let mut reader = io::Cursor::new(note.desc);
                let count = reader.read_u64()? as usize;
                let page_size = reader.read_u64()? as usize;
                if !maps.is_empty() { return err!(MalformedExecutable, "multiple NT_FILE notes in core dump"); }
                for i in 0..count {
                    let start_address = reader.read_u64()? as usize;
                    let end_address = reader.read_u64()? as usize;
                    if end_address < start_address { return err!(MalformedExecutable, "inverted address range in NT_FILE"); }
                    let offset_pages = reader.read_u64()? as usize;

                    // Find mmaps overlapping this range.
                    let mut perms = MemMapPermissions::empty();
                    let mut idx = ranges.partition_point(|r| r.start_address + r.size <= start_address);
                    if idx < ranges.len() && ranges[idx].start_address < end_address {
                        perms = ranges[idx].permissions;
                        while idx < ranges.len() && ranges[idx].start_address < end_address {
                            if ranges[idx].start_address < start_address || ranges[idx].start_address + ranges[idx].size > end_address {
                                eprintln!("warning: core dump has file address range [0x{:x}, 0x{:x}) partially overlapping a mmap address range [0x{:x}, 0x{:x}); this is not supported well", start_address, end_address, ranges[idx].start_address, ranges[idx].start_address + ranges[idx].size);
                            }
                            if let CoreDumpMemorySource::Zero = ranges[idx].source {
                                ranges[idx].source = CoreDumpMemorySource::MissingFile; // we'll assign it later if we find the file
                            }
                            idx += 1;
                        }
                    }

                    maps.push(MemMapInfo {start: start_address, len: end_address - start_address, perms, offset: offset_pages * page_size, inode: 0, path: None, binary_locator: None, binary_id: None, elf_seen: false});
                }
                let mut files_with_executable_maps: HashSet<String> = HashSet::new();
                for i in 0..count {
                    let mut filename: Vec<u8> = Vec::new();
                    reader.read_until(b'\0', &mut filename)?;
                    if !filename.ends_with(b"\0") { return err!(MalformedExecutable, "non-null-terminated filename in core dump"); }
                    filename.pop();
                    let path = str::from_utf8(&filename)?.to_string();
                    if maps[i].perms.contains(MemMapPermissions::EXECUTE) {
                        files_with_executable_maps.insert(path.clone());
                    }
                    maps[i].path = Some(path);
                }
                for i in 0..count {
                    if let Some(path) = &maps[i].path {
                        if !files_with_executable_maps.contains(path) {
                            // Cores produced by gcore are missing the 'executable' flag on most maps for some reason.
                            // So if all maps of some file are non-executable, we pretend that they're all executable instead.
                            // This is currently required to make our loaded binary detection code work at all on gcore outputs.
                            maps[i].perms.insert(MemMapPermissions::EXECUTE);
                        }
                    }
                }
            }
            _ => (),
        }
    }

    // Detect vdso.
    for range in ranges.iter_mut().rev() {
        if range.permissions.contains(MemMapPermissions::EXECUTE) && range.size >= 4096 && range.size <= 65536 {
            if let &CoreDumpMemorySource::File {ref file, offset} = &range.source {
                // Beginning of expected ELF header: magic bytes, 64-bit, little-endian, version 1, System V ABI, ABI version 0, padding.
                if &file.data[offset..offset+16] == &[0x7f, 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] {
                    let path = "[vdso]".to_string();
                    maps.push(MemMapInfo {start: range.start_address, len: range.size, perms: range.permissions, offset: 0, inode: 0, path: Some(path.clone()), binary_locator: Some(BinaryLocator {path: path.clone(), inode: 0, special: SpecialSegmentId::Vdso(range.start_address..range.start_address+range.size)}), binary_id: None, elf_seen: false});
                }
            }
        }
    }

    maps.sort_by_key(|m| m.start);
    let mut maps = MemMapsInfo {maps};
    maps.detect_executables(/*pid*/ 0);

    Ok((CoreDumpMemReader {ranges}, threads, maps))
}

impl CoreDumpMemReader {
    pub fn read_uninit<'a>(&self, addr: usize, buf: &'a mut [MaybeUninit<u8>]) -> Result<&'a mut [u8]> {
        let mut idx = self.ranges.partition_point(|r| r.start_address + r.size <= addr);
        let mut pos = 0;
        while pos < buf.len() {
            if idx == self.ranges.len() || self.ranges[idx].start_address > addr + pos {
                return err!(ProcessState, "address 0x{:x} not mapped", addr + pos);
            }
            let range = &self.ranges[idx];
            assert!(range.start_address + range.size > addr + pos);
            let n = (range.start_address + range.size - addr - pos).min(buf.len() - pos);
            unsafe {
                let dest = buf.as_ptr().add(pos) as *mut u8;
                match &range.source {
                    CoreDumpMemorySource::Zero => ptr::write_bytes(dest, 0u8, n),
                    CoreDumpMemorySource::File {file, offset} => {
                        let skip = addr + pos - range.start_address;
                        let start_offset = offset + skip;
                        if start_offset.saturating_add(n) > file.data.len() {
                            return err!(MalformedExecutable, "out of bounds core dump or binary access: file {}, offset {} + {} + {}", file.name, offset, skip, n);
                        }
                        ptr::copy_nonoverlapping(file.data.as_ptr().add(start_offset), dest, n);
                    }
                    CoreDumpMemorySource::MissingFile => return err!(ProcessState, "missing mapped file"),
                }
            }
            pos += n;
            idx += 1;
        }
        unsafe {Ok(std::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len()))}
    }

    pub fn read_best_effort(&self, addr: usize, buf: &mut [u8]) {
        let start_idx = self.ranges.partition_point(|r| r.start_address + r.size <= addr);
        for range in &self.ranges[start_idx..] {
            if range.start_address >= addr + buf.len() {
                break;
            }
            let &CoreDumpMemorySource::File {ref file, offset} = &range.source else {
                continue;
            };
            let start = if addr > range.start_address {addr - range.start_address} else {0};
            let end = range.size.min(file.data.len().saturating_sub(offset)).min(addr + buf.len() - range.start_address);
            if end <= start {
                continue;
            }
            let pos = range.start_address + start - addr;
            buf[pos..pos+(end-start)].copy_from_slice(&file.data[offset+start..offset+end]);
        }
    }
}

// Quickly parses the first few bytes of ELF header to tell whether it's a core dump.
pub fn is_core_dump_file(path: &str) -> Result<bool> {
    let file = File::open(path)?;
    let mut reader = io::BufReader::new(file);
    let magic = reader.read_u32()?;
    if magic != 0x464c457f {
        return err!(MalformedExecutable, "invalid ELF magic bytes: 0x{:x}", magic);
    }
    let mut skip = [0u8; 12];
    reader.read_exact(&mut skip)?;
    let e_type = reader.read_u16()?;
    Ok(e_type == 4)
}

// Reads and parses just enough ELF headers to find build id. That's usually just first page of the file,
// and it's usually resident in memory (and so present in coredumps) even under memory pressure (I couldn't figure out what's the mechanism for keeping it resident).
// This looks like a big hack, but it appears to be the standard way of extracting build ids of loaded binaries from core dumps;
// or maybe not, I'm not sure, I didn't have the patience to read enough of eu-unstrip code to find how it does it.
// `len` is the length of the prefix of the file that we have available, not necessarily the whole file.
pub fn extract_build_id_from_mapped_elf(memory: &MemReader, addr: usize, len: usize) -> Result<Vec<u8>> {
    let mut reader = CachedMemReader::new(memory.clone());
    let mut header: libc::Elf64_Ehdr;
    unsafe {
        header = mem::zeroed();
        reader.read(addr, slice::from_raw_parts_mut(&raw mut header as *mut u8, mem::size_of::<libc::Elf64_Ehdr>()))?;
    }
    if &header.e_ident[..4] != &[0x7f, 0x45, 0x4c, 0x46] { return err!(MalformedExecutable, "invalid ELF magic bytes: {}", hexdump(&header.e_ident[..4], 100)); }
    let phdr_size = mem::size_of::<libc::Elf64_Phdr>();
    if (header.e_phentsize as usize) < phdr_size { return err!(MalformedExecutable, "section header size too small: {}", header.e_phentsize); }

    let mut have_notes_out_of_bounds = false;
    for idx in 0..header.e_phnum as usize {
        let offset = idx.saturating_mul(header.e_phentsize as usize).saturating_add(header.e_phoff as usize);
        if offset.saturating_add(phdr_size) > len {
            return err!(MalformedExecutable, "segment header is outside of mapped range");
        }
        let mut segment: libc::Elf64_Phdr;
        unsafe {
            segment = mem::zeroed();
            reader.read(offset.saturating_add(addr), slice::from_raw_parts_mut(&raw mut segment as *mut u8, phdr_size))?;
        }
        if segment.p_type != PT_NOTE {
            continue;
        }

        let mut offset = segment.p_offset as usize;
        let end_offset = offset.saturating_add(segment.p_filesz as usize);
        while offset < end_offset {
            if offset.saturating_add(12) > len {
                have_notes_out_of_bounds = true;
                break;
            }
            let name_len = reader.read_u32(offset + addr)? as usize;
            let desc_len = reader.read_u32(offset + addr + 4)? as usize;
            let type_ = reader.read_u32((offset + 8).saturating_add(addr))?;
            offset += 12;
            let name_len_padded = (name_len + 3) / 4 * 4;
            let desc_len_padded = (desc_len + 3) / 4 * 4;
            if offset.saturating_add(name_len_padded).saturating_add(desc_len_padded) > len {
                have_notes_out_of_bounds = true;
                break;
            }
            if type_ == NT_GNU_BUILD_ID {
                if desc_len > 10000 { return err!(Sanity, "NT_GNU_BUILD_ID note is very long: {} bytes", desc_len); }
                let mut build_id = vec![0u8; desc_len];
                reader.read((offset + name_len_padded).saturating_add(addr), &mut build_id)?;
                return Ok(build_id);
            }
            offset += name_len_padded + desc_len_padded;
        }
    }
    err!(MalformedExecutable, "{}", if have_notes_out_of_bounds {"NOTE segment not mapped"} else {"no NOTE segment"})
}

// Situation: the user opened a core dump produced on another machine, and the crashed process used some dynamic libraries that is not available on this machine (e.g. a different version of libc),
// and we failed to find it in debuginfod etc. If we do nothing, the debugger won't even be able to unwind stack through this library, which usually makes the debugger useless.
// We really want to at least get .eh_frame, and maybe also .dynsym and .dynstr (for function names). But they're usually mapped and resident in memory, so are usually present in the core dump.
// This function extracts these sections (whichever are available) and puts them together into a fake "ElfFile".
// ... After implementing this, I noticed that usually core dumps don't have .eh_frame anyway. Either it's usually not resident or the core dump code excludes it. Oops. This code can probably be removed.
pub fn reconstruct_elf_from_mapped_parts(name: String, memory: &Arc<CoreDumpMemReader>, maps: MemMapsInfo, elf_prefix_addr: Range<usize>) -> Result<ElfFile> {
    let mut reader = CachedMemReader::new(MemReader::CoreDump(memory.clone()));
    let mut header: libc::Elf64_Ehdr;
    unsafe {
        header = mem::zeroed();
        reader.read(elf_prefix_addr.start, slice::from_raw_parts_mut(&raw mut header as *mut u8, mem::size_of::<libc::Elf64_Ehdr>()))?;
    }
    if &header.e_ident[..4] != &[0x7f, 0x45, 0x4c, 0x46] { return err!(MalformedExecutable, "invalid ELF magic bytes: {}", hexdump(&header.e_ident[..4], 100)); }
    let phdr_size = mem::size_of::<libc::Elf64_Phdr>();
    if (header.e_phentsize as usize) < phdr_size { return err!(MalformedExecutable, "section header size too small: {}", header.e_phentsize); }

    let mut elf = ElfFile {name, segments: Vec::new(), sections: Vec::new(), entry_point: 0, section_by_offset: Vec::new(), section_by_name: HashMap::new(), text_section: None, is_core_dump: false, is_reconstructed: true, build_id: None, mmapped: None, owned: Vec::new(), data: &[], r_debug_ptr_addr: None, interp: None};

    // Find .eh_frame, .text (very roughly), .dynstr, .dynsym.

    // Assume that all segments that we care about are loaded at addresses with the same offset relative to their addresses in segment headers.
    // And that one of the segments starts at file offset 0, which is how we're able to get the elf file header at all.
    // And that the file-offset-0 segment is listed earlier than any other segments we care about.
    let mut addr_static_to_dynamic = 0usize;
    let mut found_map_at_offset_zero = false;

    let mut longest_executable_segment: (Vec<u8>, /*addr*/ usize, /*offset*/ usize) = (Vec::new(), 0, 0);
    for idx in 0..header.e_phnum as usize {
        let offset = idx.saturating_mul(header.e_phentsize as usize).saturating_add(header.e_phoff as usize);
        if offset.saturating_add(phdr_size) > elf_prefix_addr.len() {
            return err!(MalformedExecutable, "segment header is outside of mapped range");
        }
        let mut segment: libc::Elf64_Phdr;
        unsafe {
            segment = mem::zeroed();
            reader.read(offset.saturating_add(elf_prefix_addr.start), slice::from_raw_parts_mut(&raw mut segment as *mut u8, phdr_size))?;
        }

        if segment.p_memsz == 0 || segment.p_filesz == 0 {
            continue;
        }

        if segment.p_offset == 0 {
            addr_static_to_dynamic = elf_prefix_addr.start.wrapping_sub(segment.p_vaddr as usize);
            found_map_at_offset_zero = true;
        }
        let segment_addr = (segment.p_vaddr as usize).wrapping_add(addr_static_to_dynamic);

        match segment.p_type {
            PT_GNU_EH_FRAME => {
                if !found_map_at_offset_zero { return err!(Dwarf, "got .eh_frame before segment with offset zero"); }

                // EH_FRAME segment is .eh_frame_hdr section. But we want .eh_frame, which is usually somewhere in the middle of another segment.
                // Parse the beginning of .eh_frame_hdr to get the address of .eh_frame start.
                let mut buf = [0u8; 4];
                match reader.read(segment_addr, &mut buf) {
                    Ok(()) => (),
                    Err(_) => return err!(ProcessState, "EH_FRAME segment is not in the core dump (addr 0x{:x} + 0x{:x})", segment.p_vaddr, addr_static_to_dynamic),
                }
                let encoding = buf[1];
                let mut addr = segment_addr + 4;
                let eh_frame_addr = match encoding & 0xf {
                    0xf => return err!(Dwarf, "EH_FRAME doesn't have a pointer to .eh_frame"),
                    0x1 => reader.eat_uleb128(&mut addr)?,
                    0x9 => reader.eat_sleb128(&mut addr)? as usize,
                    0x2 => reader.read_u16(addr)? as usize,
                    0x3 => reader.read_u32(addr)? as usize,
                    0x4 | 0xc => reader.read_usize(addr)?,
                    0xa => reader.read_u16(addr)? as i16 as isize as usize,
                    0xb => reader.read_u32(addr)? as i32 as isize as usize,
                    _ => return err!(Dwarf, "EH_FRAME has unexpected pointer encoding: {}", encoding),
                };
                let eh_frame_addr = match encoding & 0xf0 {
                    0x00 => eh_frame_addr,
                    0x10 => eh_frame_addr.wrapping_add(segment_addr + 4),
                    0x30 => eh_frame_addr.wrapping_add(segment_addr),
                    _ => return err!(Dwarf, "EH_FRAME has unexpected pointer encoding: {}", encoding),
                };

                // Neither .eh_frame_hdr nor .eh_frame header has the size of .eh_frame, ugh. So we scan .eh_frame to find the null terminator (idk whether it's always present, but in the one binary I looked at it was).
                let mut data: Vec<u8> = Vec::new();
                loop {
                    let len = reader.read_u32(eh_frame_addr + data.len())?;
                    data.extend_from_slice(&len.to_le_bytes());
                    if len == 0 {
                        break;
                    }
                    let mut len = len as usize;
                    if len == u32::MAX as usize {
                        len = reader.read_usize(eh_frame_addr + data.len())? as usize;
                        data.extend_from_slice(&len.to_le_bytes());
                    }
                    if data.len().saturating_add(len) > 1<<32 {
                        return err!(Sanity, "suspiciously long .eh_frame in core dump: >= {} + {} bytes", data.len(), len);
                    }
                    let prev_len = data.len();
                    data.resize(data.len() + len, 0u8);
                    reader.read(eh_frame_addr + prev_len, &mut data[prev_len..])?;
                }

                let section = ElfSection {idx: elf.sections.len(), name: ".eh_frame".to_string(), section_type: SHT_PROGBITS, flags: 0, address: eh_frame_addr.wrapping_sub(addr_static_to_dynamic), offset: elf.owned.len(), size: data.len(), link: 0, info: 0, alignment: 0, entry_size: 0, name_offset_in_strtab: 0, compression_header: None, decompressed_data: OnceLock::new()};
                elf.owned.append(&mut data);
                elf.sections.push(section);
            }
            PT_DYNAMIC => {
                if !found_map_at_offset_zero { return err!(Dwarf, "got .dynamic before segment with offset zero"); }
                // (Here we should parse the contents of this segment to find address+size of .dynstr and address of .dynsym)
            }
            _ if segment.p_flags & PF_X != 0 => {
                if !found_map_at_offset_zero { return err!(Dwarf, "got executable segment before segment with offset zero"); }

                if segment.p_memsz > 16<<30 {
                    return err!(Sanity, "suspiciously long executable segment: {} bytes", segment.p_memsz);
                }
                if segment.p_memsz as usize > longest_executable_segment.0.len() {
                    longest_executable_segment.0.resize(segment.p_memsz as usize, 0u8);
                    memory.read_best_effort(segment_addr, &mut longest_executable_segment.0[..]);
                    longest_executable_segment.1 = segment_addr;
                    longest_executable_segment.2 = segment.p_offset as usize;
                }
            }
            _ => (),
        }
    }

    let section = ElfSection {idx: elf.sections.len(), name: ".text".to_string(), section_type: SHT_PROGBITS, flags: 0, address: longest_executable_segment.1.wrapping_sub(addr_static_to_dynamic), offset: longest_executable_segment.2, size: longest_executable_segment.0.len(), link: 0, info: 0, alignment: 0, entry_size: 0, name_offset_in_strtab: 0, compression_header: None, decompressed_data: OnceLock::new()};
    elf.owned.append(&mut longest_executable_segment.0);
    elf.text_section = Some(elf.sections.len());
    elf.sections.push(section);

    for idx in 0..elf.sections.len() {
        let section = &elf.sections[idx];
        if section.size > 0 {
            elf.section_by_offset.push((section.offset, section.offset + section.size, idx));
        }
        elf.section_by_name.insert(section.name.clone(), idx);
    }
    elf.section_by_offset.sort_unstable_by_key(|t| t.0);
    elf.owned.reserve_exact(ELF_PAD_RIGHT);
    elf.data = unsafe {mem::transmute(&elf.owned[..])};

    Ok(elf)
}

// Read the ELF headers.
fn open_elf(name: String, file: Option<(&File, /*file_len*/ usize)>, mut owned: Vec<u8>) -> Result<ElfFile> {
    let len;
    let mmapped = match file {
        Some((f, file_len)) => {
            len = file_len;
            Some(Mmap::new(f, file_len, file_len + ELF_PAD_RIGHT)?)
        }
        None => {
            len = owned.len();
            owned.resize(len + ELF_PAD_RIGHT, 0u8);
            None
        }
    };
    let data: &[u8] = match &mmapped {
        Some(m) => m.data(),
        None => &owned[..len],
    };

    let (header, _) = unsafe {memcpy_struct::<libc::Elf64_Ehdr>(data, "Elf64_Ehdr")}?;

    if &header.e_ident[..4] != &[0x7f, 0x45, 0x4c, 0x46] { return err!(MalformedExecutable, "invalid ELF magic bytes: {}", hexdump(&header.e_ident[..4], 100)); }

    if header.e_ident[4] == 1 { return err!(UnsupportedExecutable, "32-bit executables are not supported"); }
    if header.e_ident[4] != 2 { return err!(MalformedExecutable, "invalid EI_CLASS: {}", header.e_ident[4]); }

    if header.e_ident[5] == 2 { return err!(UnsupportedExecutable, "big-endian executables are not supported"); }
    if header.e_ident[5] != 1 { return err!(MalformedExecutable, "invalid EI_DATA: {}", header.e_ident[5]); }

    if header.e_ident[6] != 1 { return err!(MalformedExecutable, "invalid EI_VERSION: {}", header.e_ident[6]); }

    let abi = header.e_ident[7];
    if abi != 0 && abi != 3 { return err!(UnsupportedExecutable, "only Linux and System V ABIs are supported (got: EI_OSABI = {})", abi); }

    let is_core_dump = header.e_type == 4;
    // 3 is "Shared object", and some executables use it.
    if !is_core_dump && header.e_type != 2 && header.e_type != 3 { return err!(UnsupportedExecutable, "unexpected or unsupported species of elf: e_type = {}", header.e_type); }

    if header.e_machine != 0x3e { return err!(UnsupportedExecutable, "only AMD x86-64 executables are supported, for now (got: e_machine = {})", header.e_machine); }

    if header.e_version != 1 { return err!(MalformedExecutable, "invalid e_version: {}", header.e_version); }

    let entry_point = header.e_entry as usize;

    if header.e_phnum > 0 && (header.e_phentsize as usize) < mem::size_of::<libc::Elf64_Phdr>() { return err!(MalformedExecutable, "ELF e_phentsize too small in {}", name); }
    if header.e_shnum > 0 && (header.e_shentsize as usize) < mem::size_of::<libc::Elf64_Shdr>() { return err!(MalformedExecutable, "ELF e_shentsize too small in {}", name); }
    if (header.e_phnum as usize).saturating_mul(header.e_phentsize as usize).saturating_add(header.e_phoff as usize) > data.len() { return err!(MalformedExecutable, "ELF program header out of bounds in {}", name); }
    if (header.e_shnum as usize).saturating_mul(header.e_shentsize as usize).saturating_add(header.e_shoff as usize) > data.len() { return err!(MalformedExecutable, "ELF section header out of bounds in {}", name); }

    let mut segments: Vec<ElfSegment> = Vec::new();
    for idx in 0..header.e_phnum as usize {
        let (segment, _) = unsafe {memcpy_struct::<libc::Elf64_Phdr>(&data[header.e_phoff as usize + idx * header.e_phentsize as usize..], "Elf64_Phdr").unwrap()};

        let mut offset = segment.p_offset as usize;
        let mut size_in_file = segment.p_filesz as usize;
        if offset.saturating_add(size_in_file) > data.len() {
            eprintln!("warning: ELF segment {} out of bounds (offset: {}, size in file: {}, file size: {})", idx, offset, size_in_file, data.len());
            offset = offset.min(data.len());
            size_in_file = size_in_file.min(data.len() - offset);
        }

        segments.push(ElfSegment {idx, segment_type: segment.p_type, flags: segment.p_flags, offset, address: segment.p_vaddr as usize, size_in_file, size_in_memory: segment.p_memsz as usize, alignment: segment.p_align as usize});
    }

    let mut sections: Vec<ElfSection> = Vec::new();
    for idx in 0..header.e_shnum as usize {
        let (section, _) = unsafe {memcpy_struct::<libc::Elf64_Shdr>(&data[header.e_shoff as usize + idx * header.e_shentsize as usize..], "Elf64_Shdr").unwrap()};
        sections.push(ElfSection {
            idx, name: String::new(), name_offset_in_strtab: section.sh_name, section_type: section.sh_type, flags: section.sh_flags,
            address: section.sh_addr as usize, offset: section.sh_offset as usize, size: section.sh_size as usize, link: section.sh_link,
            info: section.sh_info, alignment: section.sh_addralign as usize, entry_size: section.sh_entsize as usize, compression_header: None, decompressed_data: OnceLock::new()});
    }

    let data: &'static [u8] = unsafe {mem::transmute(data)};

    let mut elf = ElfFile {name, mmapped, owned, data, segments, sections, entry_point, section_by_offset: Vec::new(), section_by_name: HashMap::new(), text_section: None, is_core_dump, is_reconstructed: false, build_id: None, r_debug_ptr_addr: None, interp: None};

    for idx in 0..elf.sections.len() {
        let name = unsafe{elf.str_from_strtab(elf.sections[header.e_shstrndx as usize].offset, elf.sections[idx].name_offset_in_strtab as usize)?}.to_string();
        elf.sections[idx].name = name.clone();

        let s = &mut elf.sections[idx];
        if s.offset.saturating_add(s.size_in_file()) > elf.data.len() {
            eprintln!("warning: ELF section {} (type 0x{:x}, flags 0x{:x}) out of bounds: {} + {} > {}", name, s.section_type, s.flags, s.offset, s.size_in_file(), elf.data.len());
            // Clamp the range to make sure we won't segfault trying to read mmap out of bounds.
            s.offset = s.offset.min(elf.data.len());
            s.size = s.size.min(elf.data.len() - s.offset);
        }

        if s.flags & SHF_COMPRESSED != 0 {
            let compressed = &elf.data[s.offset..s.offset+s.size_in_file()];
            s.compression_header = Some(unsafe {memcpy_struct(compressed, "Elf64_Chdr")}?.0);
        }

        if s.size_in_file() != 0 {
            elf.section_by_offset.push((s.offset, s.offset + s.size, idx));
        }

        let prev = elf.section_by_name.insert(s.name.clone(), idx);
        // (Core dumps produced by gcore have lots of sections named "load".)
        if prev.is_some() && !is_core_dump {
            eprintln!("warning: ELF has duplicate section name: {}", s.name);
        }

        //let s = &elf.sections[idx]; println!("section {}: type: 0x{:x}, flags: 0x{:x}, address: {}, offset: {}, size: {}, link: {}, info: {}, alignment: {}, entry size: {}", s.name, s.section_type, s.flags, s.address, s.offset, s.size, s.link, s.info, s.alignment, s.entry_size);
    }

    elf.section_by_offset.sort_unstable();

    for idx in 1..elf.section_by_offset.len() {
        let prev = &elf.section_by_offset[idx-1];
        let cur = &elf.section_by_offset[idx];
        if prev.1 > cur.0 {
            // Our binary search won't work.
            eprintln!("warning: ELF has overlapping sections: {} is [{}, {}), {} is [{}, {})", elf.sections[prev.2].name, prev.0, prev.1, elf.sections[cur.2].name, cur.0, cur.1);
        }
    }

    elf.text_section = elf.section_by_name.get(".text").copied();

    // Parse all notes to find build id. We could instead just look at section ".note.gnu.build-id", but it's not always present, e.g. it's missing in vdso on my machine.
    let mut build_id = None;
    for section_idx in 0..elf.sections.len() {
        if elf.sections[section_idx].section_type == SHT_NOTE {
            let mut data = elf.section_data(section_idx)?;
            while !data.is_empty() {
                let note;
                (note, data) = parse_elf_note(data)?;
                match note.type_ {
                    NT_GNU_BUILD_ID if note.name == b"GNU\0" => if !note.desc.is_empty() {
                        build_id = Some(note.desc.to_owned());
                    }
                    _ => (),
                }
            }
        }
    }
    elf.build_id = build_id;
    
    // Parse .dynamic section.
    let mut r_debug_ptr_addr = None;
    if let Some(&section_idx) = elf.section_by_name.get(".dynamic") {
        let data = elf.section_data(section_idx)?;
        let mut offset = 0;
        while offset + 16 <= data.len() {
            let tag = usize::from_le_bytes(data[offset..offset+8].try_into().unwrap());
            if tag == 21 { // DT_DEBUG
                r_debug_ptr_addr = Some(elf.sections[section_idx].address + offset + 8);
                break;
            }
            offset += 16;
        }
    }
    elf.r_debug_ptr_addr = r_debug_ptr_addr;

    let mut interp = None;
    if let Some(&section_idx) = elf.section_by_name.get(".interp") {
        let data = elf.section_data(section_idx)?;
        let len = data.iter().copied().position(|x| x == b'\0').unwrap_or(data.len());
        interp = Some(str::from_utf8(&data[..len])?.to_string());
    }
    elf.interp = interp;

    if let Some(s) = &elf.build_id {
        eprintln!("info: build id {} in {}", hexdump(s, 1000), elf.name);
    } else if !elf.is_core_dump && elf.name != "[vdso]" {
        eprintln!("warning: no build id in {}", elf.name);
    }

    Ok(elf)
}
