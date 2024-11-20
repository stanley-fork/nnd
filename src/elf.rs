use std::{fs::{File}, mem};
use std::io::{self, BufReader, SeekFrom, Seek, Read, BufRead};
use std::collections::{HashMap, hash_map::Entry};
use crate::{*, error::*, log::*, util::*};

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

pub struct ElfNote {
    pub type_: u32,
    pub name: String,
    pub desc: Vec<u8>,
}

pub struct ElfFile {
    pub name: String, // just for logging

    pub segments: Vec<ElfSegment>,
    pub sections: Vec<ElfSection>,
    pub entry_point: usize,

    pub section_by_offset: Vec<(usize, usize, usize)>, // (offset, offset + size, index in `sections`), sorted, only mapped sections with nonzero size in file are included
    pub section_by_name: HashMap<String, usize>,
    pub text_section: Option<usize>,

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

pub const ELFCOMPRESS_ZLIB: u32 = 1;

pub const ELF_PAD_RIGHT: usize = 4096;

impl ElfFile {
    pub fn data(&self) -> &[u8] {
        self.data as _
    }

    pub unsafe fn bytes_from_strtab(&self, section_offset: usize, offset: usize) -> &[u8] {
        std::ffi::CStr::from_ptr(self.data()[(section_offset + offset) as usize..].as_ptr() as *const i8).to_bytes()
    }

    pub unsafe fn str_from_strtab(&self, section_offset: usize, offset: usize) -> &str {
        // We don't check for unterminated strings or invalid utf8, for speed.
        // This is fine, we're not trying to be secure to adversarial attacks here, and files in practice are not going to be malformed in this way.
        std::str::from_utf8_unchecked(self.bytes_from_strtab(section_offset, offset))
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

    pub fn parse_note(&self, section_idx: usize) -> Result<ElfNote> {
        let mut reader = io::Cursor::new(self.section_data(section_idx));
        let name_len = reader.read_u32()? as usize;
        let desc_len = reader.read_u32()? as usize;
        let type_ = reader.read_u32()?;
        let mut buf = vec![0u8; (name_len + 3) / 4 * 4];
        reader.read_exact(&mut buf)?;
        buf.truncate(name_len);
        let name = String::from_utf8(buf)?;
        let mut buf = vec![0u8; (desc_len + 3) / 4 * 4];
        reader.read_exact(&mut buf)?;
        buf.truncate(desc_len);
        if !reader.fill_buf()?.is_empty() { return err!(MalformedExecutable, "{} has unexpected bytes at the end", self.sections[section_idx].name); }
        Ok(ElfNote {type_: type_, name: name, desc: buf})
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

impl ElfSection {
    pub fn size_in_file(&self) -> usize {
        if self.section_type == SHT_NOBITS {
            0
        } else {
            self.size
        }
    }
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
    // 3 is "Shared object", and some executables use it.
    if x != 2 && x != 3 { return err!(UnsupportedExecutable, "only executable ELF files are supported, for now (got: e_type = {})", x); }

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
        let offset = reader.read_u64()? as usize;
        let address = reader.read_u64()? as usize;
        let physical_address = reader.read_u64()? as usize;
        let size_in_file = reader.read_u64()? as usize;
        let size_in_memory = reader.read_u64()? as usize;
        let alignment = reader.read_u64()? as usize;

        segments.push(ElfSegment {idx: idx, segment_type: segment_type, flags: flags, offset: offset, address: address, size_in_file: size_in_file, size_in_memory: size_in_memory, alignment: alignment});
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

    let mut elf = ElfFile {name, mmapped, owned, data, segments, sections, entry_point, section_by_offset: Vec::new(), section_by_name: HashMap::new(), text_section: None};

    for idx in 0..elf.sections.len() {
        let name = unsafe{elf.str_from_strtab(elf.sections[section_names_section_idx].offset, elf.sections[idx].name_offset_in_strtab as usize)}.to_string();
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
