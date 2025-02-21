use crate::{*, error::*, log::*, util::*, registers::*, procfs::*, process_info::*};
use std::{fs::{File}, mem, mem::MaybeUninit, io::{self, BufReader, SeekFrom, Seek, Read, BufRead}, sync::Arc, collections::{HashMap, hash_map::Entry}, str, ptr, fmt::Debug, fmt, result};
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

    pub decompressed_data: Vec<u8>, // if flags has SHF_COMPRESSED
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

    // Slightly longer than the file because of padding.
    mmapped: Option<Mmap>,
    owned: Vec<u8>,
    // Length equal to file size.
    data: &'static [u8],
}

pub const SHT_PROGBITS: u32 = 0x1;
pub const SHT_SYMTAB: u32 = 0x2;
pub const SHT_NOBITS: u32 = 0x8; // pronounced as "shit! no bits!"

pub const SHF_TLS: u64 = 1 << 10;
pub const SHF_COMPRESSED: u64 = 1 << 11;
pub const SHF_STRINGS: u64 = 1 << 5;
pub const SHF_EXECINSTR: u64 = 1 << 2;

pub const STT_FUNC: u8 = 2;
pub const STT_OBJECT: u8 = 1;

pub const SHN_UNDEF: u16 = 0;

pub const PT_LOAD: u32 = 1;
pub const PT_NOTE: u32 = 4;

// Segment permissions.
pub const PF_R: u32 = 0x4;
pub const PF_W: u32 = 0x2;
pub const PF_X: u32 = 0x1;

// These are used in core dumps.
pub const NT_PRSTATUS: u32 = 1;
pub const NT_PRFPREG: u32 = 2;
pub const NT_PRPSINFO: u32 = 3;
pub const NT_TASKSTRUCT: u32 = 4;
pub const NT_AUXV: u32 = 6;
pub const NT_SIGINFO: u32 = 0x53494749;
pub const NT_FILE: u32 = 0x46494c45;
pub const NT_PRXFPREG: u32 = 0x46e62b7f;
pub const NT_X86_XSTATE: u32 = 0x202;

pub const ELFCOMPRESS_ZLIB: u32 = 1;

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
    pub fn section_data(&self, idx: usize) -> &[u8] {
        let section = &self.sections[idx];
        if section.flags & SHF_COMPRESSED == 0 {
            &self.data()[section.offset..section.offset + section.size_in_file()]
        } else {
            &section.decompressed_data
        }
    }

    pub fn section_data_by_name<'a>(&'a self, name: &str) -> Option<&'a [u8]> {
        match self.section_by_name.get(name) {
            None => None,
            Some(i) => Some(self.section_data(*i)),
        }
    }

    pub fn decompressed_size(&self) -> usize {
        (0..self.sections.len()).map(|i| self.section_data(i).len()).sum()
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
    let name_len_padded = (name_len + 3) / 4 * 4;
    let desc_len_padded = (desc_len + 3) / 4 * 4;
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

// Structs found in core dump notes.

#[repr(C)]
#[derive(Copy, Clone)]
struct elf_prstatus {
	si_signo: i32, // signal number
    // These two seem to be reversed compared to siginfo_t. They also don't seem to be populated by binfmt_elf.c. The real siginfo is in NT_SIGINFO.
	si_code_but_actually_it_is_zero: i32,  // extra code
	si_errno_but_actually_it_is_zero: i32, // errno

    pr_cursig: i16, // Current signal
    pr_sigpend: usize, // Set of pending signals
    pr_sighold: usize, // Set of held signals
    pr_pid: pid_t,
    pr_ppid: pid_t,
    pr_pgrp: pid_t,
    pr_sid: pid_t,
    pr_utime: libc::timeval, // User time
    pr_stime: libc::timeval, // System time
    pr_cutime: libc::timeval, // Cumulative user time
    pr_cstime: libc::timeval, // Cumulative system time
    pr_reg: libc::user_regs_struct, // GP registers
    pr_fpvalid: i32, // True if math co-processor being used.
}

#[repr(C)]
#[derive(Copy, Clone)]
struct elf_prpsinfo {
    pr_state: i8, // numeric process state
    pr_sname: i8, // char for pr_state
    pr_zomb: i8, // zombie
    pr_nice: i8, // nice val
    pr_flag: u64, // flags
    pr_uid: u32,
    pr_gid: u32,
    pr_pid: pid_t,
    pr_ppid: pid_t,
    pr_pgrp: pid_t,
    pr_sid: pid_t,
    pr_fname: [u8; 16], // filename of executable
    pr_psargs: [u8; 80], // initial part of arg list
}

pub fn parse_core_dump(elf: Arc<ElfFile>) -> Result<(CoreDumpMemReader, Vec<(pid_t, ThreadInfo, Option</*signal*/ i32>)>, MemMapsInfo)> {
    if !elf.is_core_dump {
        return err!(Usage, "not a core dump file");
    }

    // Be careful to not confuse:
    //  * Mapping between the crashed process's virtual memory ranges and core dump file offset ranges.
    //    This just tells CoreDumpMemReader how to read from "memory", and also which memory ranges were executable.
    //    This is elf.segments.
    //  * Binaries that the crashed process had mapped. Analogous to /proc/<pid>/maps of a running debuggee.
    //    Has file paths and offsets into those files (not into core dump file).
    //    This is in NT_FILE note.

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
                threads.push((prstatus.pr_pid, ThreadInfo {regs: Registers::from_ptrace(&prstatus.pr_reg), ..Default::default()}, signal));
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
                //asdqwe xsave
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

                    let mut perms = MemMapPermissions::empty();
                    let mut idx = ranges.partition_point(|r| r.start_address + r.size <= start_address);
                    if idx < ranges.len() && ranges[idx].start_address < end_address {
                        perms = ranges[idx].permissions;
                        while idx < ranges.len() && ranges[idx].start_address < end_address {
                            if let CoreDumpMemorySource::Zero = ranges[idx].source {
                                ranges[idx].source = CoreDumpMemorySource::MissingFile; // we'll assign it later if we find the file
                            }
                            idx += 1;
                        }
                    }

                    maps.push(MemMapInfo {start: start_address, len: end_address - start_address, perms, offset: offset_pages * page_size, inode: 0, path: None, binary_locator: None, binary_id: None, elf_seen: false});
                }
                for i in 0..count {
                    let mut filename: Vec<u8> = Vec::new();
                    reader.read_until(b'\0', &mut filename)?;
                    if !filename.ends_with(b"\0") { return err!(MalformedExecutable, "non-null-terminated filename in core dump"); }
                    filename.pop();
                    maps[i].path = Some(str::from_utf8(&filename)?.to_string());
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

// Read the ELF headers.
//
// If there are compressed sections, decompresses them right here.
// This is maybe not ideal: maybe we won't need all of them, and maybe decompression can be parallelized.
// Hopefully this won't be a problem because the huge binaries usually have uncompressed debug symbols.
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

    let mut reader = io::Cursor::new(&data[..]);

    let magic = reader.read_u32()?;
    if magic != 0x464c457f { return err!(MalformedExecutable, "invalid ELF magic bytes: 0x{:x}", magic); }

    let x = reader.read_u8()?;
    if x == 1 { return err!(UnsupportedExecutable, "32-bit executables are not supported"); }
    if x != 2 { return err!(MalformedExecutable, "invalid EI_CLASS: {}", x); }

    let x = reader.read_u8()?;
    if x == 2 { return err!(UnsupportedExecutable, "big-endian executables are not supported"); }
    if x != 1 { return err!(MalformedExecutable, "invalid EI_DATA: {}", x); }

    let x = reader.read_u8()?;
    if x != 1 { return err!(MalformedExecutable, "invalid EI_VERSION: {}", x); }

    let abi = reader.read_u8()?;
    if abi != 0 && abi != 3 { return err!(UnsupportedExecutable, "only Linux and System V ABIs are supported (got: EI_OSABI = {})", abi); }

    reader.read_u64()?;

    let x = reader.read_u16()?;
    let is_core_dump = x == 4;
    // 3 is "Shared object", and some executables use it.
    if !is_core_dump && x != 2 && x != 3 { return err!(UnsupportedExecutable, "unexpected or unsupported species of elf: e_type = {}", x); }

    let machine = reader.read_u16()?;
    if machine != 0x3e { return err!(UnsupportedExecutable, "only AMD x86-64 executables are supported, for now (got: e_machine = {})", machine); }

    let x = reader.read_u32()?;
    if x != 1 { return err!(MalformedExecutable, "invalid e_version: {}", x); }

    let entry_point = reader.read_u64()? as usize;
    let program_header_table_offset = reader.read_u64()? as usize;
    let section_header_table_offset = reader.read_u64()? as usize;
    let flags = reader.read_u32()?;

    let x = reader.read_u16()?;

    let program_header_entry_size = reader.read_u16()? as usize;
    let program_header_entry_count = reader.read_u16()? as usize;
    let section_header_entry_size = reader.read_u16()? as usize;
    let section_header_entry_count = reader.read_u16()? as usize;
    let section_names_section_idx = reader.read_u16()? as usize;

    reader.seek(SeekFrom::Start(program_header_table_offset as u64))?;
    let mut segments: Vec<ElfSegment> = Vec::new();
    for idx in 0..program_header_entry_count {
        if program_header_entry_size < 0x38 { return err!(MalformedExecutable, "invalid program header entry size: {}", program_header_entry_size); }
        if idx > 0 { io::copy(&mut reader.by_ref().take(program_header_entry_size as u64 - 0x38), &mut io::sink())?; }

        let segment_type = reader.read_u32()?;
        let flags = reader.read_u32()?;
        let mut offset = reader.read_u64()? as usize;
        let address = reader.read_u64()? as usize;
        let physical_address = reader.read_u64()? as usize;
        let mut size_in_file = reader.read_u64()? as usize;
        let size_in_memory = reader.read_u64()? as usize;
        let alignment = reader.read_u64()? as usize;

        if offset.saturating_add(size_in_file) > data.len() {
            eprintln!("warning: ELF segment {} out of bounds (type: 0x{:x}, offset: {}, size in file: {}, address: {}, size in memory: {}, file size: {})", idx, segment_type, offset, size_in_file, address, size_in_memory, data.len());
            offset = offset.min(data.len());
            size_in_file = size_in_file.min(data.len() - offset);
        }

        segments.push(ElfSegment {idx, segment_type, flags, offset, address, size_in_file, size_in_memory, alignment});
        //println!("segment: type: 0x{:x}, flags: 0x{:x}, offset: {}, address: {}, paddr: {}, size in file: {}, size in memory: {}, alignment: {}", segment_type, flags, offset, address, physical_address, size_in_file, size_in_memory, alignment);
    }

    reader.seek(SeekFrom::Start(section_header_table_offset as u64))?;
    let mut sections: Vec<ElfSection> = Vec::new();
    for idx in 0..section_header_entry_count {
        if section_header_entry_size < 0x40 { return err!(MalformedExecutable, "invalid section header entry size: {}", section_header_entry_size); }
        if idx > 0 { io::copy(&mut reader.by_ref().take(section_header_entry_size as u64 - 0x40), &mut io::sink())?; }

        sections.push(ElfSection {
            idx: idx,
            name: String::new(),
            name_offset_in_strtab: reader.read_u32()?,
            section_type: reader.read_u32()?,
            flags: reader.read_u64()?,
            address: reader.read_u64()? as usize,
            offset: reader.read_u64()? as usize,
            size: reader.read_u64()? as usize,
            link: reader.read_u32()?,
            info: reader.read_u32()?,
            alignment: reader.read_u64()? as usize,
            entry_size: reader.read_u64()? as usize,
            decompressed_data: Vec::new()});
    }

    let data: &'static [u8] = unsafe {mem::transmute(data)};

    let mut elf = ElfFile {name, mmapped, owned, data, segments, sections, entry_point, section_by_offset: Vec::new(), section_by_name: HashMap::new(), text_section: None, is_core_dump};

    for idx in 0..elf.sections.len() {
        let name = unsafe{elf.str_from_strtab(elf.sections[section_names_section_idx].offset, elf.sections[idx].name_offset_in_strtab as usize)?}.to_string();
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
            let mut reader = io::Cursor::new(compressed);
            let header = libc::Elf64_Chdr {
                ch_type: reader.read_u32()?,
                ch_reserved: reader.read_u32()?,
                ch_size: reader.read_u64()?,
                ch_addralign: reader.read_u64()?,
            };
            let header_bytes = reader.position();
            let compressed = &reader.into_inner()[header_bytes as usize..];
            let mut decompressed = vec![0u8; header.ch_size as usize];

            match header.ch_type {
                ELFCOMPRESS_ZLIB => {
                    let mut decoder = flate2::read::ZlibDecoder::new(compressed);
                    decoder.read_exact(&mut decompressed)?;
                }
                _ => return err!(UnsupportedExecutable, "ELF compression {} not supported", header.ch_type),
            };

            s.decompressed_data = decompressed;
        }

        if s.size_in_file() != 0 {
            elf.section_by_offset.push((s.offset, s.offset + s.size, idx));
        }

        let prev = elf.section_by_name.insert(s.name.clone(), idx);
        if prev.is_some() {
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

    Ok(elf)
}
