use crate::{*, error::{*, Result, Error}, util::*, elf::*, procfs::*, range_index::*, registers::*, log::*, arena::*, types::*, expr::{*, Value}};
use std::{cmp, str, mem, rc::Rc, fs::File, path::{Path, PathBuf}, sync::atomic::{AtomicUsize, Ordering, AtomicBool}, sync::{Arc, Mutex}, collections::{HashMap, hash_map::{Entry, DefaultHasher}}, hash::{Hash, Hasher}, ffi::OsStr, os::unix::ffi::OsStrExt, io, io::{Read, Write as ioWrite}, fmt::Write, time::{Instant, Duration}, ptr, slice, fmt};
use memmap2::Mmap;
use gimli::*;
use bitflags::*;
use rand::random;
use std::ops::Range;

// Data structures for the program information: functions, variables, struct fields, etc.
//
// The main goal of this debugger is to work well with the bloated modern binaries with 3 GB of debug symbols.
// So it's important to make these data structures reasonably efficient and compact.
//
// (Note: this long comment at the top of this file is one of the first things written in this project. It's probably quite outdated and wrong and needs to be updated.)
//
// Some rough stats about a typical binary (clickhouse, DWARF4, written in C++, statically linked with lots of libraries, some in rust):
//  * Binary size: 2.4 GB
//  * Size of the .text section (i.e. the actual program): 303 MB
//  * Debug info section sizes:
//     .debug_line       132 MB
//     .debug_abbrev     14 MB
//     .debug_info       853 MB
//     .debug_aranges    6 MB
//     .debug_ranges     128 MB
//     .debug_str        332 MB
//     .debug_pubnames   6 MB
//     .debug_pubtypes   5 MB
//     .debug_loc        449 MB
//     .gdb_index        7 MB
//  * Other noteworthy sections:
//     .eh_frame         27 MB
//     .eh_frame_hdr     4 MB
//     .rodata section   59 MB
//     .symtab           21 MB
//     .strtab           99 MB
//  * Compilation units:                              33 K
//  * DWARF DIEs:                                     83 M
//  * DWARF attributes:                               275 M
//  * Functions:                                      600 K
//  * Functions with address range:                   12 M
//  * Structs/classes/unions/enums:                   2.3 M
//  * Fields of structs/classes/unions:               3 M
//  * Variables and function parameters:              38 M
//  * Variable loc ranges:                            21 M
//  * Inlined function calls:                         9 M
//  * Inline function ranges:                         12 M
//  * Total length of mangled function names:         926 MB
//  * Total length of fully-qualified function names: 1800 MB
//  * Total length of unqualified function names:     165 MB
//  * Total length of variable/arg/field names:       40 MB
//  * Total length of fully-qualified type names:     273 MB
//  * 73% of functions and 57% of types are template instantiations. This sounds suspiciously low, maybe I didn't count right.
//  * In .eh_frame:
//     - CIEs: 4
//     - FDEs: 533 K
//     - rows: 4.6 M
//     - register rules: 17 M
//     - max rows per FDE: 2.5 K
//
// (There are many weird things going on that I don't understand, e.g.:
//   * Most of the 33 K compilation units are duplicates, and in other builds the number is more like 6 K.
//     But 6 K seems too small - surely (?) ClickHouse and its 100+ dependencies have much more files than that.
//   * Template instantiations are probably duplicated a lot - once for each compilation unit that instantiated it. AFAIU, the actual machine code gets deduplicated by
//     the linker, but debug symbols don't. So we should probably try to deduplicate that somehow.
//   * Vast majority of functions have no address range. Are they all inlined? The fraction seems suspiciously high. Maybe they're some useless duplicates that need to be
//     coalesced with the primary instance that does have an address range? Idk.
//   * Lots of variables have no locations. Are they some weird duplicates like in previous item? Or are they optimized out so completely that their value never actually
//     exists anywhere? Or does the compiler fail to emit debug information about their location? Is this why gdb and lldb constantly say "optimized out" about variables
//     that are obviously not optimized out if you look at the disassembly?)
//
// Some more stats, from bin/load_symbols.rs, debug build of clickhouse:
// 8.720 K units, 27.170 K files (34.24x dedup, 1.884 MiB of paths, 7.097 MiB of remap), 2.654 M functions (2.00x dedup, 101.224 MiB, 450.984 MiB of names, 1.111 MiB extra names), 43.018 M lines (1.282 GiB), 13.450 M local variables (718.309 MiB), 11.366 M global variables, 1.556 M types (8.27x dedup, 1.54x offsets, 1.744 M names (435.198 MiB), 118.750 MiB infos, 49.791 MiB fields (1.89% growth waste), 0 B misc, temp 1.312 GiB offset maps, temp 191.406 MiB dedup maps) (128.317 K base types)


// All 'static lifetimes actually point into the mmapped region inside ElfFile. (I believe it's not undefined behavior if we don't actually dereference dangling references, specifically for immutable 'static references.)

// We mostly don't worry about portability and abstraction layers, but here we try to keep DWARF stuff contained in this file, in case we want to support Mac OS in future or something, and also because it's ugly.
// But one could imagine getting rid of all on-the-fly parsing of DWARF, and instead pre-convert everything into our own data structures, and then having multiple converters for different source formats. (Ok, no, dwarf expressions make this too complex.)
// (Or using something like a vtable to parse different formats on the fly, but hopefully our data structures can be made compact enough and conversion can be made fast enough that this won't be needed.)

type SliceType = EndianSlice<'static, LittleEndian>;
// We currently don't support .debug_types section. If we were to support it, we would pack section id and offset into one 8-byte value and use that as DieOffset (just like UnitSectionOffset, but 8 bytes instead of 16).
type DieOffset = DebugInfoOffset<usize>;

pub struct SymbolsShard {
    // Address to line number. Each entry applies to addresses [this_entry.addr(), next_entry.addr()), where next_entry is the next higher entry across all shards, as if they were merged into one sorted array.
    // If file_idx() == None, this range of addresses has no line information.
    // Doesn't include inlined function call sites.
    // Expect tens of millions of these.
    // This takes ~60% more memory than the corresponding .debug_line section, but the conversion is pretty fast (compared to .debug_info).
    pub addr_to_line: Vec<LineInfo>,

    // Line number to addresses, for adding breakpoints.
    // Includes both regular line numbers and inlined function call sites. For inlined call sites, includes subfunction_level, for regular line numbers it's u16::MAX.
    // For .debug_line, includes only entries marked as "statement", i.e. recommended breakpoint addresses.
    // Sorted by (file_idx, line, column).
    pub line_to_addr: Vec<(LineInfo, /*subfunction_level*/ u16)>,

    pub types: Types,

    // Functions and inlined function calls.
    pub subfunctions: Vec<Subfunction>,
    // Address ranges for them. Normally accessed through subfunction_levels.
    pub subfunction_pc_ranges: Vec<SubfunctionPcRange>,
    // Indices in subfunction_pc_ranges of boundaries between sorted runs of ranges corresponding to one function+level.
    // Normally accessed through FunctionInfo.subfunction_levels, which points to a slice of this array.
    // With that slice in hand, subfunction_pc_ranges[slice[i]..slice[i+1]] are the sorted ranges at level i within the function.
    // Length of the slice is number of levels + 1.
    pub subfunction_levels: Vec<usize>,

    // (Used only for restoring tabs in disassembly window after restart. They're matched by name in case the executabe was recompiled, and addresses and DIE offsets changed.
    //  Unfortunate that we have to spend time and memory on building this whole map just for that minor feature, but I don't have better ideas. This adds ~3s/num_cores to load time.)
    pub mangled_name_to_function: Vec<(&'static [u8], FunctionAddr, /*idx in Symbols.functions*/ usize)>,
    
    pub misc_arena: Arena,

    // "Local" variables, by which we mean variables that should show up in the local variables window when the instruction pointer is in the correct range.
    // Includes static variables inside functions, but with address range clamped to the function's address range - we want
    // the variable to automatically show up in the UI only if we're in that function, not all the time.
    pub local_variables: Vec<LocalVariable>,
}

pub struct Symbols {
    pub elf: Arc<ElfFile>,
    pub debuglink_elf: Option<ElfFile>,
    pub identity: usize, // randomly generated identifier of this specific Symbols instance; changes if we reload the symbols even for the exact same binary (to reflect the fact that Symbols loading is not deterministic, e.g. functions may end up in different shards)

    // .debug_info, .debug_line, etc - sections describing things in the source code (functions, line numbers, structs and their fields, etc) and how they map to address ranges.
    // If some or all sections are missing, we treat them as empty, and no special handling is needed because empty sections parse as valid DWARF debug info with 0 units.
    pub dwarf: Dwarf<SliceType>,

    // Units in .debug_info, sorted by DieOffset (not to be confused with address).
    // Expect tens of thousands of these.
    pub units: Vec<CompilationUnit>,

    // Indexes to map addresses (in debuggee's virtual address space) to stuff.
    // Keep in mind that these assume segment addresses as defined in ELF file, which are different from addresses at runtime.
    // We call these "static" and "dynamic" addresses.

    // Deduplicated source code files. Expect tens of thousands of these.
    pub files: Vec<FileInfo>,
    pub file_paths: StringTable,
    pub path_to_used_file: HashMap<&'static Path, usize>, // files with used_lines != 0

    // All functions that either have machine code or are inlined at least once. Taken from .debug_info and .symtab. Sorted by address. Expect millions of these.
    // We assume that functions' address ranges cover the whole .text section and don't overlap, so we only store start address for each range
    // and assume that each range ends where the next one begins. This assumption is also useful for .symtab, which seems to lack reliable range length information
    // (the information is there, but it's often incorrect).
    pub functions: Vec<FunctionInfo>,

    pub shards: Vec<SymbolsShard>,

    pub builtin_types: BuiltinTypes,

    // Annoyingly, DWARF expressions refer to primitive data types by their DIE offset within the unit, and each unit has its own copy of all the primitive type descriptions.
    // So we have this map from offset to the primitive type description. (Maybe we should remove it and parse the DIE on demand. And maybe parse more DIEs on demand in general.)
    pub base_types: Vec<u64>, // packed: bits 8-63 are DIE offset, bits 0-7 are gimli::read::ValueType enum.

    // Static addresses corresponding to .text section.
    pub code_addr_range: Range<usize>,

    // C++ vtables addresses, sorted. Comes from .symtab. Used for automatically downcasting to concrete types.
    pub vtables: Vec<VTableInfo>,

    // Other things to add:
    //  * "Global" variables, i.e. variables that are available at ~any ip, including static variables in functions. Expect tens of millions of them. Can be inspected using watches. Name may be mangled. May come from symtab.
}

pub struct CompilationUnit {
    pub offset: DieOffset,
    pub unit: Unit<SliceType>,

    pub name: String,
    pub comp_dir: &'static str,

    pub shard_idx: usize,

    pub file_idx_remap: Vec<usize>, // DWARF file index -> index in Symbols.files
}

// Data structures for functions and inlined function calls.
//
// FunctionInfo is a function. It may or may not have address ranges.
// Because of peculiarities of debug information encountered in practice,
// the exact ranges we attribute to functions are not known until we merge and deduplicate functions from .debug_info and .symtab.
// That's why FunctionInfo doesn't contain the end of the range, and subfunctions (see below) may have
// address ranges not fully consistent with their function's ranges (i.e. stick out or have gaps).
//
// Subfunction is a set of address ranges inside a function, corresponding either to the
// whole function (level-0 subfunction) or to an inlined function call. Subfunctions of each function form a tree.
// Subfunctions and their ranges are organized into "levels" by nesting depth. Ranges of each level live in their own sorted array slice.
// Ranges of level i are in slice s.subfunction_pc_ranges[s.subfunction_levels[f.subfunction_levels.start + i]],
// where s is SymbolsShard, and f is FunctionInfo.
// Subfunction ranges are always properly nested, ranges on each level don't overlap,
// and the nesting of ranges matches the child-parent relationships in the subfunction tree:
//
// |------------------------------------------------|   <--  level 0 - whole function
//      |----------------------|     |---------|        <--  level 1 - inlined function calls directly from the function
//         |----|    |-------|            |--|          <--  level 2 - inlined function calls from level 1, etc
//
// The range tree is well-formed within each function (we do a pass to validate and fix up the ranges after parsing each function),
// but nothing is guaranteed across functions.

#[derive(Clone)]
pub struct Subfunction {
    // Index in Symbols.functions. MAX if level == 0 or if DWARF had invalid reference.
    // (During symbols loading, this is a FunctionAddr instead.)
    pub callee_idx: usize,
    pub parent: usize, // index in `subfunctions`, if level > 0
    local_variables: usize, // packed range of indices in `local_variables`
    // Location of the function definition (if level-0) or call site (if level > 0). Invalid if unknown.
    pub call_line: LineInfo,
    pub last_range_idx: usize, // index in the function+level's slice of subfunction_pc_ranges; MAX means something was wrong in DWARF, and this subfunction should be ignored
    pub level: u16,
}
impl Subfunction {
    pub fn local_variables_range(&self) -> Range<usize> { let start = self.local_variables >> 24; Range {start, end: start + (self.local_variables & 0xffffff)} }

    pub fn pack_range(r: Range<usize>) -> usize { r.start << 24 | r.len() }
}

pub struct SubfunctionPcRange {
    pub range: Range<usize>,
    pub subfunction_idx: usize, // in `subfunctions`
    pub prev_range_idx: usize, // idx in the function's slice of `subfunction_pc_ranges` of previous range with same subfunction_idx; circular linked list sorted by addr
}

bitflags! { pub struct FunctionFlags: u8 {
    // This is not a function but a marker telling where the previous function ends.
    const SENTINEL = 0x1;
    // Not a real function but address range of a misc code section, e.g. .plt
    const SECTION = 0x2;
    // The information is taken from .symtab
    const SYMTAB = 0x4;
    // The function is inlined at least once.
    const INLINED = 0x8;
}}

// For functions that have machine code: address of the first instruction.
// For inline-only functions: FUNCTION_ADDR_MAX + DIE offset.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct FunctionAddr(pub usize);
pub const FUNCTION_ADDR_MAX: usize = 1 << 60; // reserve at least two high bits for function_sorting_key
impl FunctionAddr {
    pub fn new(addr: usize) -> Self { assert!(addr < FUNCTION_ADDR_MAX); Self(addr) }
    pub fn inline(die: DieOffset) -> Self { assert!(die.0 < FUNCTION_ADDR_MAX); Self(FUNCTION_ADDR_MAX + die.0) }

    pub fn addr(&self) -> Option<usize> { if self.0 < FUNCTION_ADDR_MAX { Some(self.0) } else { None } }
    pub fn is_addr(&self) -> bool { self.0 < FUNCTION_ADDR_MAX }
}

#[derive(Clone)]
pub struct FunctionInfo {
    // If the function has machine code then addr.is_addr(), and one of the address ranges of this function is:
    // [functions[i].addr, functions[i+1].addr).
    pub addr: FunctionAddr,
    die: DieOffset,

    // If a function has multiple address ranges, we create a FunctionInfo for each range and link them together into a circular linked list, sorted by addr.
    // Only the FunctionInfo with lowest addr has all the fields populated, so lookup walks to it.
    // Usually there's just one range, i.e. prev_addr == addr.
    // This linked list may be malformed in theory, if DWARF has bad ranges, but I've never seen that in practice. So expect it to be good but make sure to not crash if it's bad,
    pub prev_addr: FunctionAddr,

    // Points either into the binary's mmap or into misc_arena, or to a string literal.
    // Usually mangled. It would be better to demangle all the names when loading symbols, so that we can do fast search for demangled name substring.
    // But cpp_demangle library is too slow for that: it takes ~1 minute to demangle 1-2 GB of names. Maybe eventually we'll try to make a faster demangler, but for now it seems like too much work.
    mangled_name: *const u8,
    mangled_name_len: u32,
    pub shard_idx: u16,
    pub flags: FunctionFlags,
    pub language: LanguageFamily,
    // Range in SymbolsShard.subfunction_levels, which points to a series of slices in subfunction_pc_ranges,
    // where i-th slice is the sorted sequence of ranges of subfunctions at level i.
    // Empty if the function has no machine code or comes from .symtab
    pub subfunction_levels: Range<usize>,
}
unsafe impl Send for FunctionInfo {}
unsafe impl Sync for FunctionInfo {}
impl FunctionInfo {
    pub fn mangled_name(&self) -> &'static [u8] { unsafe {slice::from_raw_parts(self.mangled_name, self.mangled_name_len as usize)} }
    pub fn debug_info_offset(&self) -> Option<DieOffset> { if self.die.0 == usize::MAX { None } else { Some(self.die) } }
    // 0 means this function has no machine code or comes from .symtab, 1 means it has no inline functions.
    pub fn num_levels(&self) -> usize { self.subfunction_levels.len().saturating_sub(1) }
    pub fn shard_idx(&self) -> usize { self.shard_idx as usize }

    pub fn demangle_name(&self) -> String {
        let name = self.mangled_name();
        match self.language {
            LanguageFamily::Rust => if let Ok(s) = str::from_utf8(name) {
                return rustc_demangle::demangle(s).to_string();
            }
            _ => if name.starts_with(b"_Z") {
                if let Ok(symbol) = cpp_demangle::BorrowedSymbol::new_with_options(name, &cpp_demangle::ParseOptions::default().recursion_limit(1000)) {
                    let options = cpp_demangle::DemangleOptions::new().recursion_limit(1000).no_return_type().no_params().hide_expression_literal_types();
                    if let Ok(r) = symbol.demangle(&options) {
                        return r;
                    }
                }
            }
        }
        String::from_utf8_lossy(name).into_owned()
    }
}

// Identifying information about a function, suitable for writing to the save file.
// We want both more specific and less specific information here.
// Specific (e.g. address) to be able to find the exactly correct function if the binary hasn't changed.
// Less specific (e.g. name) to be likely to find the correct function if the binary changed a little.
#[derive(Debug, Clone)]
pub struct FunctionLocator {
    pub binary_id: BinaryId,
    pub mangled_name: Vec<u8>,
    pub demangled_name: String,
    pub addr: FunctionAddr,
}
impl FunctionLocator {
    pub fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        self.binary_id.save_state_incomplete(out)?;
        out.write_slice(&self.mangled_name)?;
        out.write_str(&self.demangled_name)?; // can't just demangle on load because we don't know the language (alernatively we could save the language here)
        out.write_usize(self.addr.0)?;
        Ok(())
    }
    pub fn load_state(inp: &mut &[u8]) -> Result<Self> {
        Ok(Self {binary_id: BinaryId::load_state_incomplete(inp)?, mangled_name: inp.read_slice()?, demangled_name: inp.read_str()?, addr: FunctionAddr(inp.read_usize()?)})
    }
}


bitflags! { pub struct LocalVariableFlags: u8 {
    // It's an argument of the containing function.
    const PARAMETER = 0x1;
    // It's pseudovariable holding the expression from DW_AT_frame_base.
    const FRAME_BASE = 0x2;
}}

// There are tens of millions of these, so this struct needs to be compact.
#[derive(Clone, Debug)]
pub struct LocalVariable {
    pub name_ptr: *const u8,
    pub name_len: u32,
    pub type_: *const TypeInfo,
    pub expr: Expression<SliceType>,
    pub start: usize,
    pub len: u32,
    offset_and_flags: u64, // DieOffset and LocalVariableFlags
}
unsafe impl Send for LocalVariable {}
unsafe impl Sync for LocalVariable {}
impl LocalVariable {
    pub unsafe fn name(&self) -> &'static str { if self.name_len == 0 { "" } else { str::from_utf8_unchecked(std::slice::from_raw_parts(self.name_ptr, self.name_len as usize)) } }
    pub fn range(&self) -> core::ops::Range<usize> { self.start..self.start + self.len as usize }
    pub fn pack_offset_and_flags(off: DieOffset, flags: LocalVariableFlags) -> u64 { (off.0 as u64) << 8 | flags.bits() as u64 }
    pub fn offset(&self) -> DieOffset { DebugInfoOffset((self.offset_and_flags as usize) >> 8) }
    pub fn flags(&self) -> LocalVariableFlags { unsafe{mem::transmute((self.offset_and_flags & 0xff) as u8)} }
    
    fn with_range(&self, expr: Expression<SliceType>, range: gimli::Range) -> Self {
        let mut r = self.clone();
        r.expr = expr;
        r.start = range.begin as usize;
        r.len = (range.end - range.begin).min(u32::MAX as u64) as u32;
        r
    }
}

// Source code file. Can be referenced from line number information, functions (to say where the function was defined), etc.
#[derive(Clone)]
pub struct FileInfo {
    pub unit_comp_dir: &'static Path, // not actually static, points into the binary's mmap (either main binary or debuglink binary)
    pub directory: &'static Path, // same
    pub filename: &'static Path, // same
    // Joined path unit_comp_dir / directory / filename. If directory is absolute (starts with '/'), unit_comp_dir is omitted.
    // Lives in file_paths arena.
    pub path: &'static Path,
    pub version: FileVersionInfo,
    // How many LineInfo-s with nonzero line() in line_to_addr point to this file.
    // (What's the deal with zero line()? I've seen a binary that has most source files listed twice in the file table: once with path "./src/<...>", once with "./build/./src/<...>".
    //  The former is only referenced by a few inlined function call sites, and only with line() == 0. Idk what's up with that. Let's ignore these files so that they don't show up
    //  in file search dialog. They'll still show up in source code window if we actually hit that inlined function and select it in the stack trace; idk what to do about that, there's
    //  no good way for the debugger to know that these different paths refer to the same file.)
    pub used_lines: usize,
}
impl FileInfo {
    fn concat_path(&self, out: &mut PathBuf) {
        out.clear();
        out.push(self.unit_comp_dir);
        out.push(self.directory);
        out.push(self.filename);
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FileVersionInfo {
    pub timestamp: u64, // 0 if unknown
    pub size: u64, // 0 if unknown
    pub md5: Option<[u8; 16]>,
}
impl Default for FileVersionInfo { fn default() -> Self { Self {timestamp: 0, size: 0, md5: None} } }
impl fmt::Debug for FileVersionInfo { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> { write!(f, "({}, {}, {:?})", self.timestamp, self.size, self.md5.as_ref().map(|a| a[0])) } }
impl FileVersionInfo {
    pub fn save_state(&self, out: &mut Vec<u8>) -> io::Result<()> {
        out.write_u64(self.timestamp)?;
        out.write_u64(self.size)?;
        match &self.md5 {
            None => out.write_u8(0)?,
            Some(x) => {
                out.write_u8(1)?;
                out.write(x)?;
            }
        }
        Ok(())
    }

    pub fn load_state(inp: &mut &[u8]) -> Result<Self> {
        Ok(FileVersionInfo {timestamp: inp.read_u64()?, size: inp.read_u64()?, md5: match inp.read_u8()? {
                             0 => None,
                             1 => {let mut x = [0u8; 16]; inp.read(&mut x)?; Some(x)}
                             x => return err!(Environment, "unexpected bool in save file (md5): {}", x) }})
    }
}

bitflags! { pub struct LineFlags: usize {
    // In addr_to_line: end of inlined function.
    // In line_to_addr: start of inlined function.
    const INLINED_FUNCTION = 0x1;
    // Whether this address is suitable for setting a breakpoint.
    const STATEMENT = 0x2;
}}
const LINE_FLAGS_BITS: u32 = 2;

// A machine code location + source code location: address, file, line number, column number.
#[derive(Clone, Eq, PartialEq)]
pub struct LineInfo {
    // There are tens of millions of instances of this struct, so it's important to make it compact.
    // Currently we bit-pack things into 128 bits like this:
    //   48 bits - address
    //   16 bits - column number
    //   31 bits - file idx; MAX means this is an end-of-sequence marker, not a real line info
    //   1 bit   - LineFlags
    //   32 bits - line number
    // (If some of these get exceeded in practice, or if it takes too much memory, feel free to redistribute the bits or rethink the whole thing. E.g.:
    //  * code could be split into 64 KB pages, with each page having its own addr -> everything indexes, making all addresses 16-bit,
    //  * we could do a pre-pass to get max line number for each file, then use a common numbering for all lines of all files, as if the files were all concatenated into one;
    //    or just redistribute the bits here dynamically based on how many files there are etc,
    //  * instead of copying all line number information out of DWARF, we could store sparse checkpoints into DWARF's line number programs; note that the state of
    //    line number "program" execution is just the gimli::read::LineRow struct, and the whole thing is pretty simple if you read the gimli code around it,
    //    so this seems very doable if needed; (I'm guessing what people usually do is have an index of "sequences" (DWARF term), but those can be >100k rows, so we should split them up, which is easy;)
    //    I went with the copy-everything-out approach to make it easier to support other platforms or split-DWARF in future if needed.)
    data: [usize; 2],
}

static PRINTED_LINE_INFO_OVERFLOW_WARNING: AtomicUsize = AtomicUsize::new(0);

const LINE_FILE_IDX_MAX: usize = u32::MAX as usize >> LINE_FLAGS_BITS;

impl LineInfo {
    pub fn new(addr: usize, file_idx: Option<usize>, mut line: usize, mut column: usize, flags: LineFlags) -> Result<LineInfo> {
        let file_idx = match file_idx {
            None => LINE_FILE_IDX_MAX,
            Some(i) if i >= LINE_FILE_IDX_MAX => return err!(NotImplemented, "unsupportedly many files: {}", i),
            Some(i) => i };
        if addr >= 1 << 48 { return err!(NotImplemented, "unsupportedly high address: {}", addr); }
        if line >= 1usize << 32 || column >= 1usize << 16 {
            if PRINTED_LINE_INFO_OVERFLOW_WARNING.fetch_add(1, Ordering::SeqCst) < 5 {
                eprintln!("warning: line number ({}) or column number ({}) is too high and will be clamped (to 32 and 16 bits respectively)", line, column);
                line = line.min((1usize << 32) - 1);
                column = column.min((1usize << 16) - 1);
            }
        }
        Ok(LineInfo {data: [(addr << 16) | column, file_idx << (32 + LINE_FLAGS_BITS) | flags.bits() << 32 | line]})
    }
    pub fn invalid() -> Self { Self {data: [usize::MAX, usize::MAX]} }

    pub fn addr(&self) -> usize { self.data[0] >> 16 }
    pub fn line(&self) -> usize { self.data[1] & 0xffffffff }
    pub fn column(&self) -> usize { self.data[0] & 0xffff }
    pub fn flags(&self) -> LineFlags { LineFlags::from_bits_truncate(self.data[1] >> 32) }

    pub fn file_idx(&self) -> Option<usize> {
        let r = self.data[1] >> (32 + LINE_FLAGS_BITS);
        if r == LINE_FILE_IDX_MAX as usize {
            None
        } else {
            Some(r)
        }
    }

    pub fn with_addr(mut self, addr: usize) -> Self {
        self.data[0] = (self.data[0] & 0xffff) | (addr << 16);
        self
    }
    pub fn with_addr_and_flags(mut self, addr: usize, flags: LineFlags) -> Self {
        self.data[0] = (self.data[0] & 0xffff) | (addr << 16);
        self.data[1] = self.data[1] & !(LineFlags::all().bits() << 32) | (flags.bits() << 32);
        self
    }

    // Sorting key equivalent to (file, line, column, addr)
    pub fn file_line_column_addr(&self) -> [usize; 2] {
        [self.data[1] & !(LineFlags::all().bits() << 32), self.data[0] << 48 | self.data[0] >> 16]
    }
    // Sorting key equivalent to (addr(), file_idx().is_none(), line() == 0, flags().contains(INLINED_FUNCTION)).
    // Used when sorting addr_to_line - addr() is for sorting, everything else is deduplication priority.
    // In particular:
    //  * An address range may start where another one ends (file_idx().is_none()) - keep the start, discard the end marker.
    //  * An inlined function end (from .debug_info) may clash with an explicit line number information row (from .debug_line) - keep the explicit one.
    //  * ... except the case when the row in .debug_line is missing the line number, which does happen, ugh.
    pub fn addr_filenone_linebad_inlined(&self) -> (usize, bool, bool, bool) {
        (self.data[0] & 0xffffffffffff0000, self.data[1] == LINE_FILE_IDX_MAX as usize, self.line() == 0, (self.data[1] >> 32) & LineFlags::INLINED_FUNCTION.bits() != 0)
    }
}
impl fmt::Debug for LineInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        write!(f, "(f{:?},l{},c{},a{},f{:?})", self.file_idx(), self.line(), self.column(), self.addr(), self.flags())
    }
}

#[derive(Clone)]
pub struct VTableInfo {
    // We don't expect the [start, end) ranges to overlap between vtables.
    pub start: usize,
    pub end: usize,
    pub name: &'static str, // in misc_arena
    pub type_: Option<*const TypeInfo>,
}
unsafe impl Send for VTableInfo {}
unsafe impl Sync for VTableInfo {}

impl Symbols {
    pub fn find_unit(&self, offset: DieOffset) -> Result<&CompilationUnit> {
        let idx = self.units.partition_point(|u| u.offset <= offset);
        if idx == 0 { return err!(Internal, "entry offset out of bounds"); }
        Ok(&self.units[idx - 1])
    }

    pub fn addr_to_function(&self, addr: usize) -> Result<(&FunctionInfo, /*function_idx*/ usize)> {
        let mut idx = self.functions.partition_point(|f| f.addr.0 <= addr);
        if idx == 0 { return err!(ProcessState, "address below min function address"); }
        if idx == self.functions.len() || !self.functions[idx].addr.is_addr() { return err!(ProcessState, "address above max function address"); }
        idx -= 1;
        if self.functions[idx].flags.contains(FunctionFlags::SENTINEL) { return err!(ProcessState, "no function at address"); }

        // Follow the linked list to the first address range of this function.
        loop {
            let prev_addr = self.functions[idx].prev_addr;
            if prev_addr >= self.functions[idx].addr {
                break;
            }
            let new_idx = self.functions.partition_point(|f| f.addr < prev_addr);
            if new_idx + 1 >= self.functions.len() || self.functions[new_idx].addr != prev_addr || self.functions[new_idx].die != self.functions[idx].die {
                break;
            }
            idx = new_idx;
        }
        
        Ok((&self.functions[idx], idx))
    }

    pub fn function_addr_ranges(&self, mut function_idx: usize) -> Vec<Range<usize>> {
        let mut ranges: Vec<Range<usize>> = Vec::new();
        if self.functions[function_idx].addr.addr().is_none() {
            return ranges;
        }
        loop {
            let f = &self.functions[function_idx];
            assert!(!f.flags.contains(FunctionFlags::SENTINEL));
            ranges.push(f.addr.addr().unwrap()..self.functions[function_idx + 1].addr.addr().unwrap());

            let new_idx = self.functions.partition_point(|ff| ff.addr < f.prev_addr);
            if new_idx + 1 >= self.functions.len() {
                break;
            }
            let g = &self.functions[new_idx];
            if g.addr != f.prev_addr || g.die != f.die || g.prev_addr >= g.addr {
                break;
            }

            function_idx = new_idx;
        }
        ranges.reverse();
        ranges
    }

    pub fn find_nearest_function(&self, mangled_name: &[u8], addr: FunctionAddr) -> Option<usize> {
        // If there are multiple functions with the same name (e.g. anonymous), pick the one with the nearest address or DIE offset.
        let mut res: (usize, Option<usize>) = (usize::MAX, None);
        for shard in &self.shards {
            let a = &shard.mangled_name_to_function;
            let idx = a.partition_point(|t| t < &(mangled_name, addr, 0));
            if idx < a.len() && a[idx].0 == mangled_name {
                res = res.min((a[idx].1.0 - addr.0, Some(a[idx].2)));
            }
            if idx > 0 && a[idx-1].0 == mangled_name {
                res = res.min((addr.0 - a[idx-1].1.0, Some(a[idx-1].2)));
            }
        }
        res.1
    }

    pub fn subfunction_ranges_at_level<'a>(&'a self, level: usize, function: &FunctionInfo) -> &'a [SubfunctionPcRange] {
        let shard = &self.shards[function.shard_idx()];
        let levels = &shard.subfunction_levels[function.subfunction_levels.clone()];
        assert!(levels.len() > level + 1);
        &shard.subfunction_pc_ranges[levels[level]..levels[level+1]]
    }

    pub fn root_subfunction<'a>(&'a self, function: &FunctionInfo) -> Option<(&'a Subfunction, usize)> {
        if function.subfunction_levels.is_empty() {
            None
        } else {
            let idx = self.subfunction_ranges_at_level(0, function)[0].subfunction_idx;
            Some((&self.shards[function.shard_idx()].subfunctions[idx], idx))
        }
    }

    pub fn local_variables_in_subfunction<'a>(&'a self, subfunction: &Subfunction, shard_idx: usize) -> &'a [LocalVariable] {
        &self.shards[shard_idx].local_variables[subfunction.local_variables_range()]
    }

    pub fn containing_subfunction_at_level(&self, addr: usize, level: u16, function: &FunctionInfo) -> Option<usize> {
        let range = if level == u16::MAX {
            0..function.num_levels()
        } else {
            level as usize..level as usize + 1
        };
        let mut res: Option<usize> = None;
        for level in range {
            let ranges = self.subfunction_ranges_at_level(level, function);
            let i = ranges.partition_point(|r| r.range.end <= addr);
            if i == ranges.len() || ranges[i].range.start > addr  {
                break;
            }
            res = Some(ranges[i].subfunction_idx);
        }
        res
    }

    // Iterator that merges and deduplicates sorted addr_to_line arrays from all shards on the fly.
    // The first element (if any) either covers `addr` or is above `addr`; its file_idx() may be None.
    pub fn addr_to_line_iter<'a>(&'a self, addr: usize) -> impl Iterator<Item = LineInfo> + 'a {
        let mut sources: Vec<&'a [LineInfo]> = Vec::new();
        let mut first_below = 0;
        for s in &self.shards {
            let mut i = s.addr_to_line.partition_point(|line| line.addr() <= addr);
            if i > 0 {
                i -= 1;
                first_below = first_below.max(s.addr_to_line[i].addr());
            }
            if i < s.addr_to_line.len() {
                sources.push(&s.addr_to_line[i..]);
            }
        }
        for s in &mut sources {
            if s[0].addr() < first_below {
                *s = &s[1..];
            }
        }
        AddrToLineIter {prev_addr: usize::MAX, iter: MergeIterator::new(sources, |line| cmp::Reverse(line.addr_filenone_linebad_inlined()))}
    }

    // The file_idx() in the returned LineInfo is never None (instead, the whole LineInfo is None).
    pub fn find_line(&self, addr: usize) -> Option<LineInfo> {
        match self.addr_to_line_iter(addr).next() {
            None => None,
            Some(line) if line.addr() > addr || line.file_idx().is_none() => None,
            Some(line) => Some(line.clone()),
        }
    }

    // If the given line has no addresses, returns the next line that has.
    pub fn line_to_addrs(&self, file_idx: usize, line_number: usize, only_statements: bool) -> std::result::Result<Vec<(LineInfo, /*subfunction_level*/ u16)>, Option<usize>> {
        let mut res: Vec<(LineInfo, u16)> = Vec::new();
        let mut min_line = usize::MAX;
        for s in &self.shards {
            let mut idx = s.line_to_addr.partition_point(|l| (l.0.file_idx().unwrap(), l.0.line()) < (file_idx, line_number));
            while idx < s.line_to_addr.len() && only_statements && !s.line_to_addr[idx].0.flags().contains(LineFlags::STATEMENT) {
                idx += 1;
            }
            if idx < s.line_to_addr.len() && s.line_to_addr[idx].0.file_idx() == Some(file_idx) {
                min_line = min_line.min(s.line_to_addr[idx].0.line());
            }
            while idx < s.line_to_addr.len() && s.line_to_addr[idx].0.file_idx() == Some(file_idx) && s.line_to_addr[idx].0.line() == line_number {
                if !only_statements || s.line_to_addr[idx].0.flags().contains(LineFlags::STATEMENT) {
                    res.push(s.line_to_addr[idx].clone());
                }
                
                idx += 1;
            }
        }
        if !res.is_empty() {
            Ok(res)
        } else if min_line == usize::MAX {
            Err(None)
        } else {
            Err(Some(min_line))
        }
    }

    pub fn list_lines_for_file(&self, path: &Path) -> Vec<LineInfo> {
        let mut res: Vec<LineInfo> = Vec::new();
        let file_idx = match self.path_to_used_file.get(path) {
            Some(i) => *i,
            None => return res };
        for s in &self.shards {
            let mut i = s.line_to_addr.partition_point(|l| l.0.file_idx().unwrap() < file_idx);
            while i < s.line_to_addr.len() && s.line_to_addr[i].0.file_idx().unwrap() == file_idx {
                res.push(s.line_to_addr[i].0.clone());
                i += 1;
            }
        }
        res
    }

    pub fn find_base_type(&self, offset: DieOffset) -> Result<ValueType> {
        let idx = self.base_types.partition_point(|x| ((x >> 8) as usize) < offset.0);
        if idx < self.base_types.len() && (self.base_types[idx] >> 8) as usize == offset.0 {
            Ok(unsafe{mem::transmute((self.base_types[idx] & 0xff) as u8)})
        } else {
            err!(Dwarf, "no base type @0x{:x}", offset.0)
        }
    }

    pub fn find_vtable(&self, static_addr: usize) -> Result<VTableInfo> {
        let idx = self.vtables.partition_point(|v| v.end <= static_addr);
        if idx > 0 && self.vtables[idx].start <= static_addr {
            Ok(self.vtables[idx].clone())
        } else {
            err!(Dwarf, "no known vtable at address")
        }
    }
}

struct AddrToLineIter<'a, K: Ord, F: FnMut(&LineInfo) -> K> {
    prev_addr: usize,
    iter: MergeIterator<'a, LineInfo, K, F>,
}
impl<K: Ord, F: FnMut(&LineInfo) -> K> Iterator for AddrToLineIter<'_, K, F> {
    type Item = LineInfo;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(line) = self.iter.next() {
            if line.addr() != self.prev_addr {
                self.prev_addr = line.addr();
                return Some(line.clone());
            }
        }
        None
    }
}

// ============================================== symbols loading code below ===============================================

pub struct SymbolsLoader {
    pub num_shards: usize,
    
    sym: Symbols,
    shards: Vec<SyncUnsafeCell<CachePadded<SymbolsLoaderShard>>>,
    die_to_function_shards: Vec<SyncUnsafeCell<CachePadded<Vec<(DieOffset, usize)>>>>,
    types: TypesLoader,

    // [.strtab, .symtab], [.dynsym, .dynstr]
    strtab_symtab: Vec<[&'static [u8]; 2]>,

    // Things for management and bookkeeping.
    status: Arc<SymbolsLoadingStatus>,
    progress_per_stage: Vec<[(f64, &'static str); /* prepare, run */ 2]>, // what the progress indicator should show at the start of [prepare_stage(), run()] for each stage; stage 0 is new()
    prepare_time_per_stage_ns: Vec<usize>, // stage -> time
    run_time_per_stage_ns: Vec<Vec<AtomicUsize>>, // stage -> shard -> time
    shard_progress_ppm: Vec<CachePadded<AtomicUsize>>, // shard -> progress
    stage: usize,

    // Stats, just for information.
    types_before_dedup: usize,
    type_offsets: usize,
    type_offset_maps_bytes: usize,
    type_dedup_maps_bytes: usize,
}

pub struct SymbolsLoadingStatus {
    pub cancel: AtomicBool,
    pub progress_ppm: AtomicUsize,
    pub stage: Mutex<String>,
}
impl SymbolsLoadingStatus { pub fn new() -> Self { Self {cancel: false.into(), progress_ppm: 0.into(), stage: String::new().into()} } }

struct SymbolsLoaderShard {
    sym: SymbolsShard,

    units: Vec<usize>,
    symtab_ranges: Vec<Vec<Range<usize>>>,

    // Each DWARF unit has its own table of file paths. We merge them into one table and deduplicate. This decreases their number from millions to tens of thousands.
    file_dedup: Vec<FileDedup>,

    file_used_lines: Vec<(/*file_idx*/ usize, /*used_lines*/ usize)>,

    // Side note: types in DWARF are usually organized in one of 3 ways:
    //  (a) Each unit has its own types. All DW_AT_type are offsets within current unit (UnitRef). There are lots of duplicate types because each unit gets
    //      a copy of all types from the headers in includes. Some types are declared in one unit and defined in another, and we have to match them up using names.
    //  (b) DW_AT_type point to any units in .debug_info (DebugInfoRef). I've seen this with ThinLTO. Presumably it deduplicates types across units.
    //  (c) DW_AT_type point to type units in .debug_types using type signatures. For us, this is probably no different from (b) - types are already deduped.
    // Currently we're not taking advantage of this. We could skip deduplication in cases (b) and (c) (although hold on, my latest test shows lots of dedup on
    // ThinLTO-d binary, idk which info is right, would need to check more carefully), and do processing/deduplication incrementally one unit at a time in case (a).
    types: TypesLoadingShard,

    base_types: Vec<u64>,

    functions: Vec<FunctionInfo>,
    max_function_end: usize,

    vtables: Vec<VTableInfo>,

    // Stats, just for information.
    functions_before_dedup: usize,
    num_global_variables: usize,

    warn: Limiter,
}

struct FileDedup {
    file: FileInfo,
    path_hash: usize,
    unit_idx: usize,
    idx_in_unit: usize,
}

// Usage: call new(), then alternate between single-threaded calls to prepare_stage(i) and multi-threaded calls to run(i, shard_idx) (for all shard_idx in parallel); when prepare_stage() returns Ok(false), call into_result().
impl SymbolsLoader {
    pub fn new(elf: Arc<ElfFile>, max_shards: usize, status: Arc<SymbolsLoadingStatus>) -> Result<Self> {
        let start_time = Instant::now();

        *status.stage.lock().unwrap() = "opening debuglink".to_string();
        let debuglink_elf = open_debuglink(&elf)?;

        let mut elves: Vec<&ElfFile> = vec![&elf];
        if let Some(d) = &debuglink_elf {
            elves.push(d);
        }

        let load_section = |id: SectionId| -> std::result::Result<EndianSlice<'static, LittleEndian>, gimli::Error> {
            for &elf in &elves {
                if let Some(&idx) = elf.section_by_name.get(id.name()) {
                    let data = elf.section_data(idx);
                    if !data.is_empty() {
                        return Ok(EndianSlice::new(unsafe {mem::transmute(data)}, LittleEndian::default()));
                    }
                }
            }
            Ok(EndianSlice::new(&[0u8;0][..], LittleEndian::default()))
        };

        let code_addr_range;
        {
            let text = match elf.section_by_name.get(".text") {
                None => return err!(UnsupportedExecutable, "no .text section"),
                Some(i) => &elf.sections[*i],
            };
            code_addr_range = text.address..text.address+text.size;
        }

        let mut dwarf = Dwarf::load(load_section)?;
        *status.stage.lock().unwrap() = "loading abbreviations".to_string();
        {
            let _prof = ProfileScope::with_threshold(0.01, format!("loading abbreviations {}", elf.name));
            dwarf.populate_abbreviations_cache(AbbreviationsCacheStrategy::All);
        }
        *status.stage.lock().unwrap() = "listing units".to_string();

        if dwarf.debug_info.reader().is_empty() {
            return err!(MissingSymbols, "no debug symbols");
        }

        if !dwarf.debug_types.reader().is_empty() {
            // This was removed in DWARF 5, so probably not worth supporting.
            return err!(Dwarf, ".debug_types not supported");
        }

        let mut strtab_symtab: Vec<[&'static [u8]; 2]> = Vec::new();
        for &elf in &elves {
            for [strtab_name, symtab_name] in [[".strtab", ".symtab"], [".dynstr", ".dynsym"]] {
                match (elf.section_data_by_name(strtab_name), elf.section_data_by_name(symtab_name)) {
                    (Some(strtab), Some(symtab)) => {
                        if symtab.len() != 0 {
                            strtab_symtab.push(unsafe {[mem::transmute(strtab), mem::transmute(symtab)]});
                        }
                    }
                    _ => (),
                }
            }
        }

        let binary_size: usize = elves.iter().map(|e| e.decompressed_size()).sum();
        let num_shards = max_shards.min(binary_size >> 20).max(1);
        // How should we assign units to shards?
        // For maximum balance, we'd want to shuffle the units and assign them round-robin. But that's bad for locality of file reads.
        // For maximum locality, we'd want to just split the file into num_shards subranges. But that's bad for balance
        // if there's some bias in how expensive units are per byte (e.g. maybe rust units come before C++ units,
        // and maybe they have smaller DIEs, making them slower to process for the same size.)
        // So we do something hybrid: take ranges of 4 MB consecutive units and assign them to shards round-robin.
        // I haven't tested if this actually helps or not.
        let unit_distribution_granularity = (1usize << 22).min(dwarf.debug_info.reader().len() / num_shards).max(1);

        // Estimates of how long different stages take relative to each other, for progress indication.
        let progress_fraction = [
            [(1.746, "listing units"), (0.000, ""), ],
            [(0.000, ""), (0.039, "parsing file tables"), ],
            [(0.131, "deduping file tables"), (6.766, "parsing DWARF"), ],
            [(0.153, "deduping functions"), (0.198, "resolving type decls and sorting funcs"), ],
            [(0.000, ""), (2.084, "traversing type graph"), ],
            [(0.000, ""), (0.404, "processing name -> type"), ],
            [(0.000, ""), (0.031, "processing type -> name"), ],
            [(0.000, ""), (0.244, "fixing up type and func refs"), ],
            [(0.000, ""), (0.000, ""), ],
            [(0.000, ""), (0.000, ""), ],
            [(0.000, ""), (0.000, ""), ],];
        let mut progress_per_stage: Vec<[(f64, &'static str); 2]> = Vec::new();
        {
            let s: f64 = progress_fraction.iter().map(|a| a[0].0 + a[1].0).sum();
            let mut p = 0.0f64;
            for a in progress_fraction {
                progress_per_stage.push([(p, a[0].1), (p + a[0].0/s, a[1].1)]);
                p += (a[0].0 + a[1].0)/s;
            }
            assert!((p - 1.0).abs() < 1e-6);
            progress_per_stage.push([(p, ""), (p, "")]);
        }
        let mut prepare_time_per_stage_ns: Vec<usize> = vec![0; progress_per_stage.len()];
        let run_time_per_stage_ns: Vec<Vec<AtomicUsize>> = (0..progress_per_stage.len()).map(|_| (0..num_shards).map(|_| AtomicUsize::new(0)).collect()).collect();

        let (types_loader, types_shards) = TypesLoader::create(num_shards);
        let mut shards: Vec<SymbolsLoaderShard> = types_shards.into_iter().map(|types| SymbolsLoaderShard {
            sym: SymbolsShard {addr_to_line: Vec::new(), line_to_addr: Vec::new(), types: Types::new(), misc_arena: Arena::new(), local_variables: Vec::new(), subfunctions: Vec::new(), subfunction_pc_ranges: Vec::new(), subfunction_levels: Vec::new(), mangled_name_to_function: Vec::new()},
            units: Vec::new(), symtab_ranges: Vec::new(), file_dedup: Vec::new(), file_used_lines: Vec::new(), types, base_types: Vec::new(), functions: Vec::new(), vtables: Vec::new(),
            max_function_end: 0, functions_before_dedup: 0, num_global_variables: 0, warn: Limiter::new()}).collect();

        let mut round_robin_shard = 0usize;
        let mut round_robin_offset = 0usize;

        let debug_info_section = dwarf.debug_info.reader();
        let mut reported_progress_offset = 0usize;

        let mut units: Vec<CompilationUnit> = Vec::new();
        {
            let _prof = ProfileScope::with_threshold(0.01, format!("listing units {}", elf.name));
            let mut units_iter = dwarf.units();
            while let Some(unit_header) = units_iter.next()? {
                let unit = dwarf.unit(unit_header)?;

                let offset = match unit.header.offset() {
                    UnitSectionOffset::DebugInfoOffset(o) => o,
                    _ => return err!(Internal, "unit offset has unexpected type"),
                };
                let name = match &unit.name {
                    Some(n) => str::from_utf8(n.slice())?.to_string(),
                    None => format!("[unit @0x{:x}]", offset.0) };
                let comp_dir = match &unit.comp_dir {
                    Some(d) => str::from_utf8(d.slice())?,
                    None => "" };

                if offset.0 > round_robin_offset + unit_distribution_granularity {
                    round_robin_offset = offset.0;
                    round_robin_shard = (round_robin_shard + 1) % shards.len();
                }
                let shard_idx = round_robin_shard;
                shards[shard_idx].units.push(units.len());

                if offset.0 > reported_progress_offset + 10000000 {
                    reported_progress_offset = offset.0;
                    status.progress_ppm.store((offset.0 as f64 / debug_info_section.len() as f64 * progress_per_stage[0][1].0 * 1e6) as usize, Ordering::Relaxed);
                }

                // This is not necessarily a compilation unit (can be type unit), but we don't care, it's just a tree of DIEs either way.
                units.push(CompilationUnit {offset, name, comp_dir, unit, shard_idx, file_idx_remap: Vec::new()});
            }
        }

        for [_, symtab] in &strtab_symtab {
            let entry_size = mem::size_of::<libc::Elf64_Sym>();
            if symtab.len() % entry_size != 0 {
                return err!(Dwarf, ".symtab or .dynsym length is not divisible by {}: {}", entry_size, symtab.len());
            }
            let count = symtab.len() / entry_size;
            for i in 0..num_shards {
                shards[i].symtab_ranges.push(vec![count*i/num_shards*entry_size .. count*(i+1)/num_shards*entry_size]);
            }
        }

        prepare_time_per_stage_ns[0] = start_time.elapsed().as_nanos() as usize;
        Ok(SymbolsLoader {
            num_shards: shards.len(), sym: Symbols {elf, debuglink_elf, identity: random(), dwarf, units, files: Vec::new(), file_paths: StringTable::new(), path_to_used_file: HashMap::new(), functions: Vec::new(), shards: Vec::new(), builtin_types: BuiltinTypes::invalid(), base_types: Vec::new(), vtables: Vec::new(), code_addr_range},
            shards: shards.into_iter().map(|s| SyncUnsafeCell::new(CachePadded::new(s))).collect(), die_to_function_shards: (0..num_shards).map(|_| SyncUnsafeCell::new(CachePadded::new(Vec::new()))).collect(), types: types_loader, strtab_symtab, status, progress_per_stage,
            prepare_time_per_stage_ns, run_time_per_stage_ns, shard_progress_ppm: (0..num_shards).map(|_| CachePadded::new(AtomicUsize::new(0))).collect(), stage: 0, types_before_dedup: 0, type_offsets: 0, type_offset_maps_bytes: 0, type_dedup_maps_bytes: 0})
    }

    // The loading procedure alternates between single-threaded and multithreaded (sharded) parts. This is the single-threaded part, run() is the multithreaded part.
    // Returns Ok(false) if there are no more stages, and into_result() should be called instead.
    pub fn prepare_stage(&mut self, stage: usize) -> Result<bool> {
        self.check_cancellation()?;
        let start_time = Instant::now();
        assert_eq!(TypesLoader::num_stages(), 4); // update the matches here and in run() when this changes
        self.stage = stage;
        *self.status.stage.lock().unwrap() = self.progress_per_stage[stage][0].1.to_string();
        self.report_prepare_progress(0.0);
        for p in &self.shard_progress_ppm { p.store(0, Ordering::Relaxed); }
        match stage {
            1 => (), // then parse file tables
            2 => {
                self.dedup_files();
                self.parse_misc_sections(0, unsafe {&mut *self.shards[0].get()});
            } // then parse dwarf and symtab
            3 => {
                self.types_before_dedup = self.shards.iter().map(|s| unsafe {(*s.get()).types.types_before_dedup}).sum();
                self.type_offsets = self.shards.iter().map(|s| unsafe {(*s.get()).types.num_offsets()}).sum();
                self.type_offset_maps_bytes = self.shards.iter().map(|s| unsafe {(*s.get()).types.offset_map_bytes()}).sum();

                self.types.shards_ready(self.shards.iter().map(|s| unsafe {mem::replace(&mut (*s.get()).types, TypesLoadingShard::invalid())}).collect());
                self.collect_used_lines_per_file();
                self.dedup_functions();
            }// then work on types (match declarations to definitions) and on functions (sort die -> idx arrays)
            4 => (), // types (traverse the graph)
            5 => (), // types (process name -> type mapping, detect name collisions)
            6 => (), // types (process type -> name mapping, pick main name for each type)
            7 => (), // then fixup type and function refs
            8 => {
                self.type_dedup_maps_bytes = self.types.dedup_maps_bytes();

                self.finish_types();
                self.merge_base_types();
                self.collect_vtables();

                self.prepare_time_per_stage_ns[self.stage] = start_time.elapsed().as_nanos() as usize;
                self.log_timing_and_stats();
                return Ok(false);
            }
            _ => panic!("unexpected stage to prepare"),
        }
        self.report_prepare_progress(1.0);
        *self.status.stage.lock().unwrap() = self.progress_per_stage[stage][1].1.to_string();
        self.prepare_time_per_stage_ns[self.stage] = start_time.elapsed().as_nanos() as usize;
        Ok(true)
    }
    pub fn run(&self, stage: usize, shard_idx: usize) -> Result<()> {
        self.check_cancellation()?;
        let start_time = Instant::now();
        let progress_callback = |prog: f64| self.report_run_progress(shard_idx, prog);
        match stage {
            1 => self.parse_file_tables(shard_idx, unsafe {&mut *self.shards[shard_idx].get()})?,
            2 => {
                let shard = unsafe {&mut *self.shards[shard_idx].get()};
                self.parse_dwarf(shard_idx, shard)?;
                self.parse_symtab(shard_idx, shard)?;
                self.sort_functions(shard);
                self.parse_debug_line(shard)?;
                self.sort_addr_to_line(shard);
                self.sort_line_to_addr(shard);
            }
            3 => {
                self.build_name_to_function_map(shard_idx, unsafe {&mut *self.shards[shard_idx].get()});
                self.sort_die_to_function(unsafe {&mut *self.die_to_function_shards[shard_idx].get()});
                self.types.run(0, shard_idx, &progress_callback);
            }
            4 => self.types.run(1, shard_idx, &progress_callback),
            5 => self.types.run(2, shard_idx, &progress_callback),
            6 => self.types.run(3, shard_idx, &progress_callback),
            7 => {
                let shard = unsafe {&mut *self.shards[shard_idx].get()};
                self.fixup_type_refs(shard_idx, shard);
                self.fixup_function_refs(shard);
            }
            _ => panic!("unexpected stage to run"),
        }
        self.run_time_per_stage_ns[stage][shard_idx].store(start_time.elapsed().as_nanos() as usize, Ordering::Relaxed);
        Ok(())
    }

    pub fn into_result(mut self) -> Symbols {
        self.sym.shards = self.shards.into_iter().map(|s| s.into_inner().into_inner().sym).collect();
        self.sym
    }

    fn check_cancellation(&self) -> Result<()> {
        if self.status.cancel.load(Ordering::Relaxed) { err!(Cancelled, "") } else { Ok(()) }
    }

    fn report_prepare_progress(&mut self, prog: f64) {
        let prog = self.progress_per_stage[self.stage][0].0 * (1.0-prog) + self.progress_per_stage[self.stage][1].0 * prog;
        self.status.progress_ppm.store((prog * 1e6) as usize, Ordering::Relaxed);
    }

    fn report_run_progress(&self, shard_idx: usize, new_prog: f64) {
        self.shard_progress_ppm[shard_idx].store((new_prog * 1e6) as usize, Ordering::Relaxed);
        let prog: usize = self.shard_progress_ppm.iter().map(|s| s.load(Ordering::Relaxed)).sum();
        let prog = prog as f64 / 1e6 / self.shard_progress_ppm.len() as f64;
        let prog = self.progress_per_stage[self.stage][1].0 * (1.0-prog) + self.progress_per_stage[self.stage+1][0].0 * prog;
        self.status.progress_ppm.store((prog * 1e6) as usize, Ordering::Relaxed);
    }

    fn parse_file_tables(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) -> Result<()> {
        let mut temp_path = PathBuf::new();
        for &unit_idx in &shard.units {
            if let Some(program) = &self.sym.units[unit_idx].unit.line_program {
                let header = program.header();
                // DWARF filename table indexing is 1-based in DWARF <= 4, 0-based in DWARF 5+.
                let idx_base = (header.encoding().version <= 4) as usize;
                let entries = header.file_names();
                if idx_base == 1 {
                    shard.file_dedup.push(FileDedup {file: FileInfo {unit_comp_dir: Path::new(""), directory: Path::new(""), filename: Path::new(""), path: Path::new(""), version: FileVersionInfo {timestamp: 0, size: 0, md5: None}, used_lines: 0}, path_hash: 0, unit_idx, idx_in_unit: 0});
                }
                let comp_unit = &self.sym.units[unit_idx];
                let unit = &comp_unit.unit;
                for (idx, file) in entries.iter().enumerate() {
                    let directory = Path::new(OsStr::from_bytes(match file.directory(header) {
                        None => &[],
                        Some(d) => self.sym.dwarf.attr_string(unit, d)?.slice(),
                    }));
                    let filename = Path::new(OsStr::from_bytes(self.sym.dwarf.attr_string(unit, file.path_name())?.slice()));
                    let f = FileInfo {unit_comp_dir: Path::new(comp_unit.comp_dir), directory, filename, path: /*assigned after dedup*/ Path::new(""), used_lines: 0,
                                          version: FileVersionInfo {timestamp: file.timestamp(), size: file.size(), md5: if program.header().file_has_md5() { Some(file.md5().clone()) } else { None } }};
                    f.concat_path(&mut temp_path);
                    let mut hasher = DefaultHasher::new();
                    temp_path.hash(&mut hasher);
                    shard.file_dedup.push(FileDedup {file: f, path_hash: hasher.finish() as usize, unit_idx, idx_in_unit: idx_base + idx});
                }
            }
        }
        Ok(())
    }

    fn dedup_files(&mut self) {
        let mut files: Vec<FileDedup> = Vec::new();
        for shard in &mut self.shards {
            files.append(&mut mem::take(&mut shard.get_mut().file_dedup));
        }
        files.sort_unstable_by_key(|f| f.path_hash);
        let mut file_idx = 0;
        let mut temp_path = PathBuf::new();
        for (i, file) in files.iter().enumerate() {
            if i == 0 || file.path_hash != files[i-1].path_hash {
                let mut f = file.file.clone();
                file_idx = self.sym.files.len();
                f.concat_path(&mut temp_path);
                f.path = self.sym.file_paths.add_path(&temp_path, file_idx);
                self.sym.files.push(f);
            }
            let remap = &mut self.sym.units[file.unit_idx].file_idx_remap;
            if remap.len() <= file.idx_in_unit {
                remap.resize(file.idx_in_unit + 1, 0);
            }
            remap[file.idx_in_unit] = file_idx;
        }
    }

    fn parse_dwarf(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) -> Result<()> {
        let bytes_total: usize = shard.units.iter().map(|i| self.sym.units[*i].unit.header.unit_length()).sum();
        let mut bytes_reported = 0usize;
        let mut bytes_processed = 0usize;

        let units_copy = shard.units.clone();
        for unit_idx in units_copy {
            {
                let mut loader = DwarfLoader::new(self, shard, unit_idx)?;
                loader.load()?;
            }

            bytes_processed += self.sym.units[unit_idx].unit.header.unit_length();
            if bytes_processed > bytes_reported + 10000000 {
                self.check_cancellation()?;
                bytes_reported = bytes_processed;
                self.report_run_progress(shard_idx, bytes_processed as f64 / bytes_total as f64);
            }
        }

        shard.types.finalize();

        shard.sym.local_variables.shrink_to_fit();
        shard.sym.subfunctions.shrink_to_fit();
        shard.sym.subfunction_pc_ranges.shrink_to_fit();
        shard.sym.subfunction_levels.shrink_to_fit();

        Ok(())
    }

    fn parse_debug_line(&self, shard: &mut SymbolsLoaderShard) -> Result<()> {
        for &unit_idx in &shard.units {
            let unit = &self.sym.units[unit_idx];
            let program = match &unit.unit.line_program {
                None => continue,
                Some(x) => x };

            let mut rows_iter = program.clone().rows();
            let mut skip_current_sequence = false;
            let mut prev_is_stmt: Option<bool> = None;
            while let Some((header, row)) = rows_iter.next_row()? {
                if row.address() == 0 {
                    // Binaries often have lots (like 70%) of sequences that start at zero address, presumably because a relocation was lost at some point during compilation.
                    // Maybe that's what happens when code gets optimized out at linking time (by ThinLTO?), or maybe it's a compiler bug, idk.
                    // Skip such sequences because they take up memory and overlap each other a lot.
                    // Apart from that, line number programs seem to be well-formed in practice, so for non-skipped sequences we expect that address ranges don't overlap,
                    // addresses don't decrease within a sequence, each sequence ends with a row with end_sequence() = true, etc.
                    skip_current_sequence = true;
                }

                if !skip_current_sequence {
                    let mut is_stmt = false;
                    let info = if row.end_sequence() {
                        LineInfo::new(row.address() as usize, None, 0, 0, LineFlags::empty())?
                    } else {
                        let file = unit.file_idx_remap.get(row.file_index() as usize).copied(); // if out of bounds (unexpected in practice), just treat it as an end-of-sequence
                        let line = row.line().map_or(0, |x| u64::from(x) as usize);
                        let column = match row.column() {
                            ColumnType::LeftEdge => 0,
                            ColumnType::Column(c) => u64::from(c) as usize,
                        };
                        is_stmt = row.is_stmt() && file.is_some();
                        LineInfo::new(row.address() as usize, file, line, column, if is_stmt {LineFlags::STATEMENT} else {LineFlags::empty()})?
                    };

                    let mut skip = false;
                    if let &Some(prev_is_stmt) = &prev_is_stmt {
                        let prev = shard.sym.addr_to_line.last().unwrap();
                        if info.line() == 0 && info.file_idx().is_some() && prev.file_idx() == info.file_idx() && prev.line() != 0 {
                            // Often there's a zero line number out of nowhere, surrounded by good line numbers with same file.
                            // Idk what this means or whether it's intended. Let's pretend it's not there.
                            skip = true;
                        } else if prev.addr() == row.address() as usize {
                            // Two consecutive rows have the same address. Disacard the first one.
                            // This happens when an inlined function call was optimized down to zero instructions (e.g. the called function does nothing),
                            // but the compiler still wants to tell the debugger about the call site, to allow setting breakpoints on it
                            // or pretending that instruction pointer stands on those optimized-out lines. Not sure we want to support that.
                            // (Even if we did, it'd look different from just adding to addr_to_line here because it's deduped.)

                            if prev_is_stmt {
                                shard.sym.line_to_addr.pop();

                                // Sometimes there are two identical line numbers in a row, but the first one has is_stmt and the other one doesn't.
                                // Uuuuugh why can't DWARF just suck less.
                                if !is_stmt && info.file_idx().is_some() && prev.file_idx() == info.file_idx() && prev.line() == info.line() {
                                    is_stmt = true;
                                }
                            }

                            shard.sym.addr_to_line.pop();
                        }
                    }
                    if !skip {
                        if info.file_idx().is_some() {
                            shard.sym.line_to_addr.push((info.clone(), u16::MAX));
                        }
                        shard.sym.addr_to_line.push(info);
                        prev_is_stmt = Some(is_stmt);
                    }
                }

                if row.end_sequence() {
                    skip_current_sequence = false;
                }
            }
        }

        Ok(())
    }

    fn function_sorting_key(f: &FunctionInfo) -> usize {
        assert!(FUNCTION_ADDR_MAX << 2 != 0 && (FUNCTION_ADDR_MAX - 1) & FUNCTION_ADDR_MAX == 0);
        // If a function starts at the same address as another function ends, the real function should come first (to discard the end marker).
        // If a function is present both in .symtab and .debug_info, the .debug_info function should come first (to discard the other one).
        (f.addr.0 << 2) | ((f.flags.contains(FunctionFlags::SENTINEL) as usize) << 1) | f.flags.contains(FunctionFlags::SYMTAB) as usize
    }
    
    fn sort_functions(&self, shard: &mut SymbolsLoaderShard) {
        shard.functions_before_dedup = shard.functions.len();
        shard.functions.sort_unstable_by_key(Self::function_sorting_key);
        shard.functions.dedup_by_key(|f| f.addr);
    }
    fn sort_addr_to_line(&self, shard: &mut SymbolsLoaderShard) {
        shard.sym.addr_to_line.sort_unstable_by_key(|x| x.addr_filenone_linebad_inlined()); // if a sequence starts at the same address another sequence ends, keep the start and discard the end
        shard.sym.addr_to_line.dedup_by_key(|x| x.addr());

        // I've seen line number info like this:
        //                       src/Functions/sleep.h:122:29
        //   000022265edb <+4fb> call DB::IDataType::createColumnConst
        //   000022265ee0 <+500> jmp _+505h
        //                       src/Functions/sleep.h     <----------------------------------------- ???
        // ⮕ 000022265ee5 <+505> lea rdi,[rbp-130h]
        //                       src/Functions/sleep.h:122:16
        //   000022265eec <+50c> call boost::intrusive_ptr<DB::IColumn const>::operator->
        //
        // What's that line table entry in the middle without a line number? Idk. Let's just remove it.
        let mut prev = LineInfo::new(0, None, 0, 0, LineFlags::empty()).unwrap();
        shard.sym.addr_to_line.retain(|l| {
            let remove = l.line() == 0 && l.file_idx().is_some() && l.file_idx() == prev.file_idx() && prev.line() != 0;
            prev = l.clone();
            !remove
        });
        shard.sym.addr_to_line.shrink_to_fit();
    }

    fn sort_line_to_addr(&self, shard: &mut SymbolsLoaderShard) {
        shard.sym.line_to_addr.sort_unstable_by_key(|x| x.0.file_line_column_addr());

        for (l, _) in &shard.sym.line_to_addr {
            if l.line() == 0 || !l.flags().contains(LineFlags::STATEMENT) { // see comment on LineInfo::used_lines
                continue;
            }
            let idx = l.file_idx().unwrap();
            match shard.file_used_lines.last_mut() {
                Some(x) if x.0 == idx => x.1 += 1,
                _ => shard.file_used_lines.push((idx, 1)),
            }
        }
    }

    fn parse_symtab(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) -> Result<()> {
        for (section_idx, ranges) in shard.symtab_ranges.iter().enumerate() {
            let [strtab, symtab] = &self.strtab_symtab[section_idx];
            let entry_size = mem::size_of::<libc::Elf64_Sym>();
            for range in ranges {
                for offset in range.clone().step_by(entry_size) {
                    let mut sym: libc::Elf64_Sym;
                    unsafe {
                        sym = mem::zeroed();
                        ptr::copy_nonoverlapping::<u8>(symtab[offset..offset+entry_size].as_ptr(), mem::transmute(&mut sym), entry_size);
                    }
                    if sym.st_value == 0 || sym.st_shndx == SHN_UNDEF {
                        continue;
                    }
                    let addr = sym.st_value as usize;

                    // Function.
                    if sym.st_info & 0xf == STT_FUNC {
                        let name_ref = if sym.st_name == 0 {
                            let mut out = shard.sym.misc_arena.write();
                            write!(out, "[function at {:x} (.symtab)]", addr)?;
                            out.finish()
                        } else {
                            let suf = &strtab[sym.st_name as usize..];
                            let end = suf.iter().position(|c| *c == b'\0').unwrap_or(suf.len());
                            &suf[..end]
                        };
                        let name_ref: &'static str = unsafe {mem::transmute(name_ref)};

                        let f_addr = FunctionAddr::new(addr);
                        shard.functions.push(FunctionInfo {addr: f_addr, die: DebugInfoOffset(usize::MAX), prev_addr: f_addr, mangled_name: name_ref.as_ptr(), mangled_name_len: name_ref.len() as u32, shard_idx: shard_idx as u16, flags: FunctionFlags::SYMTAB, language: LanguageFamily::Unknown, subfunction_levels: 0..0});

                        // For some reason functions' [st_value, st_value + st_size) ranges overlap a lot. Maybe st_size is just not reliable.
                        // So we ignore st_size and assume that each function ends where the next function starts.
                        continue;
                    }

                    // Vtable.
                    if sym.st_info & 0xf == STT_OBJECT && sym.st_size != 0 && sym.st_name != 0 && strtab[sym.st_name as usize..].starts_with(b"_ZTV") {
                        let mangled = &strtab[sym.st_name as usize..];
                        let end = mangled.iter().position(|c| *c == b'\0').unwrap_or(mangled.len());
                        let mangled = &mangled[..end];
                        // Unfortunately, there's no pointer from vtable info to the type DIE. There's only the mangled name. And the type DIE doesn't even contain the mangled name, or even the fully-qualified name!
                        // We have to reconstruct the fully-qualified name while parsing DWARF, from the DIEs and attributes describing namespaces, template arguments, etc.
                        // Then match that reconstructed name against the name from symtab. We currently don't do it well at all. Doing it fully correctly is probably crazy complicated,
                        // but there's probably plenty of low-hanging improvement in the name reconstruction logic.
                        // Probably it would be better to do structural comparison on the demangling AST instead of demangled strings, to not be sensitive to the arbitrary details of how different manglers/demanglers
                        // print things like lambdas and anonymous namespaces; but that sounds too complicated.
                        match cpp_demangle::BorrowedSymbol::new_with_options(mangled, &cpp_demangle::ParseOptions::default().recursion_limit(1000)) {
                            Err(e) => if shard.warn.check(line!()) { eprintln!("warning: failed to demangle (1) vtable symtab entry '{}': {}", String::from_utf8_lossy(mangled), e); }
                            Ok(symbol) => match symbol.demangle(&cpp_demangle::DemangleOptions::default().recursion_limit(1000)) {
                                Err(e) => if shard.warn.check(line!()) { eprintln!("warning: failed to demangle (2) vtable symtab entry '{}': {}", String::from_utf8_lossy(mangled), e); }
                                Ok(name) if name.starts_with("{vtable(") => {
                                    // It's either "{vtable(Foo)}" or "{vtable(Foo)} [.clone .3e570ba5df68adedf72ec05448f83d40]". AFAIU, the latter means it's a synthetic type that's not quite Foo; I guess we should still treat it as Foo.
                                    let name = &name["{vtable(".len()..];
                                    if let Some(end) = name.rfind(")}") {
                                        let name = &name[..end];
                                        let name = shard.sym.misc_arena.add_str(name);
                                        shard.vtables.push(VTableInfo {start: addr, end: addr + sym.st_size as usize, name, type_: None});
                                    } else {
                                        if shard.warn.check(line!()) { eprintln!("error: unexpected demangled vtable name format, this is a bug: '{}' -> {}", String::from_utf8_lossy(mangled), name); }
                                    }
                                }
                                Ok(name) => if shard.warn.check(line!()) { eprintln!("error: unexpected demangled vtable name format, this is a bug: '{}' -> {}", String::from_utf8_lossy(mangled), name); }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_misc_sections(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) {
        // Sections that contain executable code but are not usually covered by debug symbols. We pretend that each of them is a function, to make them show up in disassembly window at all.
        for name in [".init", ".fini", ".plt", ".plt.got", ".plt.sec"] {
            if let Some(&idx) = self.sym.elf.section_by_name.get(name) {
                let s = &self.sym.elf.sections[idx];
                if s.size != 0 && s.address != 0 {
                    let name_ref = shard.sym.misc_arena.add_slice(name.as_bytes());
                    shard.functions.push(FunctionInfo {
                        addr: FunctionAddr::new(s.address), prev_addr: FunctionAddr::new(s.address),
                        die: DebugInfoOffset(usize::MAX), mangled_name: name_ref.as_ptr(), mangled_name_len: name_ref.len() as u32,
                        shard_idx: shard_idx as u16, flags: FunctionFlags::SECTION, language: LanguageFamily::Unknown, subfunction_levels: 0..0});
                    shard.functions.push(FunctionInfo {
                        addr: FunctionAddr::new(s.address + s.size), prev_addr: FunctionAddr::new(s.address + s.size),
                        die: DebugInfoOffset(usize::MAX), mangled_name: "".as_ptr(), mangled_name_len: 0,
                        shard_idx: shard_idx as u16, flags: FunctionFlags::SECTION | FunctionFlags::SENTINEL, language: LanguageFamily::Unknown, subfunction_levels: 0..0});
                }
            }
        }
    }

    fn fixup_type_refs(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) {
        for v in &mut shard.sym.local_variables {
            v.type_ = self.types.find_final_type(DebugInfoOffset(v.type_ as usize));
        }
    }

    fn finish_types(&mut self) {
        let (types, builtin_types) = mem::replace(&mut self.types, TypesLoader::invalid()).into_results();
        self.sym.builtin_types = builtin_types;
        for (i, t) in types.into_iter().enumerate() {
            self.shards[i].get_mut().sym.types = t;
        }
    }

    fn merge_base_types(&mut self) {
        for shard in &mut self.shards {
            self.sym.base_types.append(&mut mem::take(&mut shard.get_mut().base_types));
        }
        self.sym.base_types.shrink_to_fit();
        self.sym.base_types.sort_unstable();
    }

    fn collect_vtables(&mut self) {
        // There are usually only tens of thousands of vtables, so it's ok to process them in one thread.
        for shard in &mut self.shards {
            self.sym.vtables.append(&mut mem::take(&mut shard.get_mut().vtables));
        }
        self.sym.vtables.sort_unstable_by_key(|v| v.start);
        for v in &mut self.sym.vtables {
            for shard in &mut self.shards {
                if let Some(t) = shard.get_mut().sym.types.find_by_name(v.name) {
                    v.type_ = Some(t);
                    break;
                }
            }
        }
    }

    fn collect_used_lines_per_file(&mut self) {
        for shard in &mut self.shards {
            for &(idx, count) in &shard.get_mut().file_used_lines {
                self.sym.files[idx].used_lines += count;
            }
        }
        for i in 0..self.sym.files.len() {
            if self.sym.files[i].used_lines == 0 {
                continue;
            }
            self.sym.path_to_used_file.insert(self.sym.files[i].path, i);
        }
    }

    fn dedup_functions(&mut self) {
        let max_function_end = self.shards.iter().map(|s| unsafe {&(*s.get()).max_function_end}).max().copied().unwrap_or(0);
        let additional_functions = [FunctionInfo {addr: FunctionAddr::new(max_function_end), prev_addr: FunctionAddr::new(max_function_end), mangled_name: "".as_ptr(), mangled_name_len: 0, shard_idx: 0, flags: FunctionFlags::SENTINEL, language: LanguageFamily::Unknown, subfunction_levels: 0..0, die: DebugInfoOffset(usize::MAX)}];
        let mut sources: Vec<&[FunctionInfo]> = self.shards.iter().map(|s| unsafe {&(*s.get()).functions[..]}).collect();
        sources.push(&additional_functions);
        let mut iter = MergeIterator::new(sources, |f| !Self::function_sorting_key(f));
        let mut functions: Vec<FunctionInfo> = Vec::new();
        let mut prev_addr = FunctionAddr(usize::MAX);
        while let Some(f) = iter.next() {
            if f.addr != prev_addr {
                prev_addr = f.addr;
                functions.push(f.clone());
            }
        }
        for (i, f) in functions.iter().enumerate() {
            if f.flags.contains(FunctionFlags::INLINED) {
                let shard_idx = f.die.0 % self.die_to_function_shards.len();
                self.die_to_function_shards[shard_idx].get_mut().push((f.die, i));
            }
        }
        functions.shrink_to_fit();
        self.sym.functions = functions;
    }

    fn build_name_to_function_map(&self, shard_idx: usize, shard: &mut SymbolsLoaderShard) {
        let start_idx = self.sym.functions.len() * shard_idx / self.shards.len();
        let end_idx = self.sym.functions.len() * (shard_idx + 1) / self.shards.len();
        for idx in start_idx..end_idx {
            let f = &self.sym.functions[idx];
            shard.sym.mangled_name_to_function.push((f.mangled_name(), f.addr, idx));
        }
        shard.sym.mangled_name_to_function.sort_unstable();
    }
    
    fn sort_die_to_function(&self, die_to_function: &mut Vec<(DieOffset, usize)>) {
        die_to_function.sort_unstable_by_key(|t| t.0);
    }

    fn fixup_function_refs(&self, shard: &mut SymbolsLoaderShard) {
        for sf in &mut shard.sym.subfunctions {
            if sf.level == 0 {
                continue;
            }
            let die = sf.callee_idx;
            let shard_idx = die % self.die_to_function_shards.len();
            let die_to_function = unsafe {&*self.die_to_function_shards[shard_idx].get()};
            let i = die_to_function.partition_point(|t| t.0.0 < die);
            if i == die_to_function.len() || die_to_function[i].0.0 != die {
                sf.callee_idx = usize::MAX;
            } else {
                sf.callee_idx = die_to_function[i].1;
            }
        }
    }

    fn log_timing_and_stats(&self) {
        let prepare_ns: usize = self.prepare_time_per_stage_ns.iter().copied().sum();
        let total_cpu_ns  = prepare_ns + self.run_time_per_stage_ns.iter().map(|v| v.iter().map(|t| t.load(Ordering::Relaxed)).sum::<usize>()).sum::<usize>();
        let total_wall_ns = prepare_ns + self.run_time_per_stage_ns.iter().map(|v| v.iter().map(|t| t.load(Ordering::Relaxed)).max().unwrap()).sum::<usize>();
        if total_wall_ns > 500_000_000 {
            let funcs_before_dedup: usize = self.shards.iter().map(|s| unsafe {(*s.get()).functions_before_dedup}).sum();
            let misc_arena_size: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.misc_arena.capacity()}).sum();
            let lines: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.addr_to_line.len()}).sum();
            let local_variables: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.local_variables.len()}).sum();
            let global_variables: usize = self.shards.iter().map(|s| unsafe {(*s.get()).num_global_variables}).sum();
            let files_before_dedup: usize = self.sym.units.iter().map(|u| u.file_idx_remap.len()).sum();

            let mut function_names_len = 0usize;
            let mut max_subfunction_level = 0usize;
            for f in &self.sym.functions {
                if f.prev_addr >= f.addr {
                    function_names_len += f.mangled_name().len();
                }
                max_subfunction_level = max_subfunction_level.max(f.subfunction_levels.len().saturating_sub(1));
            }

            let final_types: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.num_types()}).sum();
            let type_names = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.sorted_type_names.strings.len()}).sum();
            let type_names_len = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.sorted_type_names.arena.used()}).sum();
            let type_infos_bytes = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.types_arena.used()}).sum();
            let fields_bytes = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.fields_arena.used()}).sum();
            let types_misc_bytes = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.misc_arena.used()}).sum();
            let field_used_bytes: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.types.iter().map(|t| match &t.t {
                Type::Struct(s) => s.fields().len() * mem::size_of::<StructField>(), Type::Enum(e) => e.enumerands.len() * mem::size_of::<Enumerand>(), _ => 0 }).sum::<usize>()}).sum();
            let subfunctions: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.subfunctions.len()}).sum();
            let subfunction_pc_ranges: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.subfunction_pc_ranges.len()}).sum();
            let subfunction_levels: usize = self.shards.iter().map(|s| unsafe {(*s.get()).sym.subfunction_levels.len()}).sum();
            let unresolved_vtables: usize = self.sym.vtables.iter().map(|v| v.type_.is_none() as usize).sum();

            let peak_mem_str = match peak_memory_usage_of_current_process() {
                Ok(x) => format!("{}", PrettySize(x)),
                Err(e) => format!("<{}>", e) };

            let mut time_breakdown = String::new();
            if true {
                // Print how long different stages took relative to each other, in a form copy-pastable into the progress_per_stage initialization code.
                write!(time_breakdown, ", time taken per stage: [").unwrap();
                for stage in 0..self.progress_per_stage.len() {
                    let wall_ns = [self.prepare_time_per_stage_ns[stage], self.run_time_per_stage_ns[stage].iter().map(|t| t.load(Ordering::Relaxed)).max().unwrap()];
                    write!(time_breakdown, "\n[").unwrap();
                    for j in 0..2 {
                        write!(time_breakdown, "({:.3}, \"{}\"), ", wall_ns[j] as f64 / 1e9, self.progress_per_stage[stage][j].1).unwrap();
                    }
                    write!(time_breakdown, "],").unwrap();
                }
                write!(time_breakdown, "];").unwrap();
            }

            eprintln!("info: loaded symbols for {} in {:.3}s (cpu: {:.3}s) in {} threads, process peak memory usage: {}, stats: {} units, {} files \
                       ({:.2}x dedup, {:.2}x unused, {} of paths, {} of remap), \
                       {} functions ({:.2}x dedup, {}, {} of names), {} in misc arena \
                       {} subfunctions ({:.2}x ranges, {} sf, {} ranges, {} levels), \
                       {} lines ({}), {} local variables ({}), {} global variables, \
                       {} types ({:.2}x dedup, {:.2}x offsets, {} names ({}), \
                       {} infos, {} fields ({:.2}% growth waste), {} misc, temp {} offset maps, temp {} dedup maps) \
                       ({} base types), {} vtables ({:.2}% unresolved){}",
                      self.sym.elf.name, total_wall_ns as f64 / 1e9, total_cpu_ns as f64 / 1e9, self.num_shards, peak_mem_str, PrettyCount(self.sym.units.len()), PrettyCount(self.sym.files.len()),
                      files_before_dedup as f64 / self.sym.files.len() as f64, self.sym.files.len() as f64 / self.sym.path_to_used_file.len() as f64, PrettySize(self.sym.file_paths.arena.used()), PrettySize(files_before_dedup * mem::size_of::<usize>()),
                      PrettyCount(self.sym.functions.len()), funcs_before_dedup as f64 / self.sym.functions.len() as f64, PrettySize(self.sym.functions.len() * mem::size_of::<FunctionInfo>()), PrettySize(function_names_len), PrettySize(misc_arena_size),
                      PrettyCount(subfunctions), subfunction_pc_ranges as f64 / subfunctions as f64, PrettySize(subfunctions * mem::size_of::<Subfunction>()), PrettySize(subfunction_pc_ranges * mem::size_of::<SubfunctionPcRange>()), PrettySize(subfunction_levels * mem::size_of::<usize>()),
                      PrettyCount(lines), PrettySize(lines * mem::size_of::<LineInfo>() * 2), PrettyCount(local_variables), PrettySize(local_variables * mem::size_of::<LocalVariable>()), PrettyCount(global_variables),
                      PrettyCount(final_types), self.types_before_dedup as f64 / final_types as f64, self.type_offsets as f64 / self.types_before_dedup as f64, PrettyCount(type_names), PrettySize(type_names_len),
                      PrettySize(type_infos_bytes), PrettySize(fields_bytes), fields_bytes as f64 / field_used_bytes as f64 * 100.0 - 100.0, PrettySize(types_misc_bytes), PrettySize(self.type_offset_maps_bytes), PrettySize(self.type_dedup_maps_bytes),
                      PrettyCount(self.sym.base_types.len()), self.sym.vtables.len(), unresolved_vtables as f64 / self.sym.vtables.len() as f64 * 100.0, time_breakdown);
        }
    }
}

fn open_debuglink(elf: &ElfFile) -> Result<Option<ElfFile>> {
    match (elf.section_by_name.get(".gnu_debuglink"), elf.section_by_name.get(".note.gnu.build-id")) {
        (Some(&debuglink), Some(&build_id)) => {
            let _prof = ProfileScope::with_threshold(0.01, format!("opening debuglink elf {}", elf.name));

            let debuglink = elf.section_data(debuglink);
            if debuglink.len() <= 4 { return err!(Dwarf, ".gnu_debuglink section is too short: {}", debuglink.len()); }
            let (filename, crc32) = debuglink.split_at(debuglink.len()-4);
            let filename = &filename[..filename.iter().position(|&x| x == b'\0').unwrap_or(filename.len())]; // null-terminated string
            let filename = str::from_utf8(filename)?;
            let crc32 = u32::from_le_bytes(crc32.try_into().unwrap());

            let build_id = elf.parse_note(build_id)?;
            if build_id.desc.len() < 1 { return err!(Dwarf, ".note.gnu.build-id descr is too short: {}", build_id.desc.len()); }

            let path: String = format!("/usr/lib/debug/.build-id/{:02x}/{}", build_id.desc[0], filename);
            eprintln!("info: opening debuglink file for {} at {}", elf.name, path);
            let file = match File::open(&path) {
                Ok(f) => f,
                Err(e) if e.kind() == io::ErrorKind::NotFound => return err!(MissingSymbols, "debuglink file not found: {}", path),
                Err(e) => return Err(e.into()),
            };
            let mmap = unsafe { Mmap::map(&file)? };

            let actual_crc32 = crc32fast::hash(&mmap[..]);
            if actual_crc32 != crc32 { return err!(Dwarf, "debuglink checksum mismatch: expected {}, found {} in {}", crc32, actual_crc32, path); }

            let res = ElfFile::from_mmap(path, mmap)?;
            return Ok(Some(res));
        }
        _ => return Ok(None),
    };
}

// A thing that traverses the tree of DIEs, one unit at a time.
// This is the slowest part of loading the debug symbols, so we should worry about speed the most here.
struct DwarfLoader<'a> {
    // Example tree, in order of iteration:
    //   root
    //     entry
    //     entry
    //       entry
    //       null
    //     entry
    //       entry
    //         entry
    //         entry
    //         null
    //       null
    //     null
    //
    // Notice that every nonempty list of children is terminated by a null entry, here represented as abbrev == None.
    // abbrev.has_children() tells whether current entry has children.
    // These two pieces of information are enough to keep track of depth.
    //
    // We use EntriesRaw instead of EntriesTree because the latter looks slow: if I'm reading it right, it'll parse each DIE as many times as its depth.
    // EntriesCursor would probably be ok, but not much different from EntriesRaw.
    // When visiting each node, we *must* call cursor.read_abbreviation(), then read or skip all attributes using read_attribute() or skip_attributes().
    loader: &'a SymbolsLoader,
    shard_idx: usize,
    shard: &'a mut SymbolsLoaderShard,
    unit_idx: usize,
    unit: &'a Unit<SliceType>,
    unit_language: LanguageFamily,

    // Scope name, like "std::vector<int>". We append to it when going into a namespace/class/function/etc, then un-append when leaving the subtree.
    scope_name: String,

    // stack[depth] corresponds to current entry.
    // We don't pop from the Vec. Instead, entries are reused to avoid frequent allocations (of the Vec-s inside each entry).
    stack: Vec<LoaderStackEntry>,
    depth: usize,
}

// Start or end of a tentative SubfunctionPcRange.
struct SubfunctionEvent {
    addr: usize,
    level: u16,
    subfunction_idx: usize,
    sign: i8, // +1 - range start, -1 - range end
}

#[derive(Clone, Copy)]
struct VariantInfo {
    discriminant_die: DieOffset,
    field_flags: FieldFlags, // VARIANT, DEFAULT_VARIANT, or empty
    discr_value: usize,
}

struct LoaderStackEntry {
    tag: DwTag,
    scope_name_len: usize, // excluding current DIE
    // Indicates that we're not inside an anonymous namespace or function, so scope_name should be somewhat legit and can be used for deduplication.
    scope_name_is_linkable: bool,

    exact_type: *mut TypeInfo, // a real pointer (not DieOffset) to the type; non-null iff the *current* DIE is a type (struct/union/array/etc); not propagated to descendants, e.g. if there's a function inside the type, the type_ is unset inside the function
    variant: Option<VariantInfo>, // if we're inside a discriminated union (e.g. Rust enum)

    // Address Range(s) of this unit/function/lexical-scope/inlined-function/etc. Used as the range for local variables (unless they have loclist ranges).
    pc_ranges: Vec<gimli::Range>,
    pc_ranges_depth: usize, // stack[pc_ranges_depth].pc_ranges is the actual value to use

    // Index in `functions` if this DIE is a function, and we created a FunctionInfo for it (i.e. it either has address ranges or is inlined at least once). Otherwise MAX.
    function: usize,
    // If nonzero, stack[function_depth] is the innermost containing DIE that is a function, regardless of whether we created a FunctionInfo for it (to distinguish local from global variables correctly).
    function_depth: usize,
    subfunction_events: Vec<SubfunctionEvent>, // for current function

    // Index in `subfunctions` if this DIE is a function or inlined function, and we created a Subfunction for it. Otherwise MAX.
    subfunction: usize,
    // If nonzero, stack[subfunction_depth] is the innermost containing DIE that is a function or inlined function call, regardless of whether we created FunctionInfo/Subfunction for it.
    subfunction_depth: usize,
    local_variables: Vec<LocalVariable>, // for current subfunction
}
impl Default for LoaderStackEntry { fn default() -> Self { Self {tag: DW_TAG_null, scope_name_len: 0, scope_name_is_linkable: true, exact_type: ptr::null_mut(), variant: None, pc_ranges: Vec::new(), pc_ranges_depth: 0, function: usize::MAX, function_depth: 0, subfunction_events: Vec::new(), subfunction: usize::MAX, subfunction_depth: 0, local_variables: Vec::new()} } }

// The way tree traversal is currently implemented is kind of complicated and has a lot of boilerplate, in part because it's trying to be fast.
// I wonder if there's a better way to organize this.
impl<'a> DwarfLoader<'a> {
    fn new(loader: &'a SymbolsLoader, shard: &'a mut SymbolsLoaderShard, unit_idx: usize) -> Result<Self> {
        let unit = &loader.sym.units[unit_idx];
        Ok(Self {loader, shard_idx: unit.shard_idx, shard, unit_idx, unit: &unit.unit, unit_language: LanguageFamily::Unknown, scope_name: String::new(), stack: Vec::new(), depth: 0})
    }

    fn push(&mut self, tag: DwTag) {
        if self.depth + 1 == self.stack.len() {
            self.stack.push(LoaderStackEntry::default());
        }
        self.depth += 1;
        let (old, new) = self.stack.split_at_mut(self.depth);
        let (old, new) = (&mut old[self.depth - 1], &mut new[0]);

        new.tag = tag;
        new.scope_name_len = self.scope_name.len();
        
        new.scope_name_is_linkable = old.scope_name_is_linkable;
        new.pc_ranges_depth = old.pc_ranges_depth;
        new.function_depth = old.function_depth;
        new.subfunction_depth = old.subfunction_depth;

        new.exact_type = ptr::null_mut();
        new.variant = None;
        new.pc_ranges.clear();
        new.function = usize::MAX;
        new.subfunction_events.clear();
        new.subfunction = usize::MAX;
        new.local_variables.clear();
    }

    fn append_namespace_to_scope_name(&mut self, s: &str, linkable: bool) {
        if !self.scope_name.is_empty() {
            self.scope_name.push_str("::");
        }
        self.scope_name.push_str(s);
        if !linkable {
            self.stack[self.depth].scope_name_is_linkable = false;
        }
    }

    // Extracts DieOffset and transmutes it to *const TypeInfo.
    fn parse_type_ref_custom(attr: &Attribute<SliceType>, tag: DwTag, unit: &Unit<SliceType>, warn: &mut Limiter, builtin_types: &BuiltinTypes) -> *const TypeInfo {
        let offset = match &attr.value() {
            AttributeValue::UnitRef(unit_offset) => unit_offset.to_debug_info_offset(&unit.header).unwrap(),
            AttributeValue::DebugInfoRef(offset) => *offset,
            _ => {
                if warn.check(line!()) { eprintln!("warning: form {:?} on {} is not supported", attr, tag); }
                return builtin_types.unknown;
            }
        };
        offset.0 as *const TypeInfo
    }

    fn parse_type_ref(&mut self, attr: &AttributeSpecification, tag: DwTag, cursor: &mut EntriesRaw<'a, 'a, SliceType>) -> Result<*const TypeInfo> {
        Ok(Self::parse_type_ref_custom(&cursor.read_attribute(*attr)?, tag, self.unit, &mut self.shard.warn, &self.loader.types.builtin_types))
    }

    fn parse_line_info(&mut self, file: Option<Attribute<SliceType>>, line: Option<Attribute<SliceType>>, column: Option<Attribute<SliceType>>) -> LineInfo {
        let file = match file {
            Some(f) => f,
            None => return LineInfo::invalid() };
        let file = match file.value() {
            AttributeValue::FileIndex(idx) => idx,
            _ => {
                if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", file.name(), file.value()); }
                return LineInfo::invalid();
            }
        };
        let file = match self.loader.sym.units[self.unit_idx].file_idx_remap.get(file as usize) {
            Some(f) => *f,
            None => return LineInfo::invalid() };

        let line = match line {
            None => 0,
            Some(line) => match line.value().udata_value() {
                None => {
                    if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", line.name(), line.value()); }
                    0
                }
                Some(line) => line as usize } };
        let column = match column {
            None => 0,
            Some(col) => match col.value().udata_value() {
                None => {
                    if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", col.name(), col.value()); }
                    0
                }
                Some(col) => col as usize } };

        match LineInfo::new(0, Some(file), line, column, LineFlags::empty()) {
            Ok(x) => x,
            Err(e) => {
                if self.shard.warn.check(line!()) { eprintln!("warning: {}", e); }
                LineInfo::invalid()
            }
        }
    }

    fn add_variable_locations(&mut self, location_attr: &Attribute<SliceType>, var: &LocalVariable, offset: DieOffset) -> Result<()> {
        let top = &self.stack[self.depth];
        let subfunction_depth = top.subfunction_depth;
        // It's possible that the containing function has no ranges and was discarded.
        // In this case we still have to interpret the variable as local rather than global.
        // We still add them to the stack entry, then no one picks them up from there.
        if let Some(expr) = location_attr.exprloc_value() {
            if subfunction_depth != 0 {
                let ranges_depth = top.pc_ranges_depth;
                for i in 0..self.stack[ranges_depth].pc_ranges.len() {
                    let range = self.stack[ranges_depth].pc_ranges[i];
                    self.stack[subfunction_depth].local_variables.push(var.with_range(expr, range));
                }
            }else {
                // (If ranges.is_empty(), use infinite range.)
                self.shard.num_global_variables += 1;
            }
        } else if let Some(mut locs_iter) = self.loader.sym.dwarf.attr_locations(self.unit, location_attr.value())? {
            let function_depth = top.function_depth;
            while let Some(entry) = locs_iter.next()? {
                if subfunction_depth != 0 {
                    // Guess whether it's a local or a static variable.
                    // (The `r.end + 1` is because I've seen ranges stick out like that in practice, idk why. Not sure if it was specifically variable vs function ranges, or some other kind of ranges of nested DIEs, I didn't check.)
                    let is_local = var.flags().intersects(LocalVariableFlags::FRAME_BASE | LocalVariableFlags::PARAMETER)
                        || self.stack[function_depth].pc_ranges.iter().any(|r| r.begin <= entry.range.begin && r.end + 1 >= entry.range.end);
                    if is_local {
                        self.stack[subfunction_depth].local_variables.push(var.with_range(entry.data, entry.range));
                    } else {
                        // Probably a static variable inside a function. Add both local and global variable.
                        self.stack[subfunction_depth].local_variables.push(var.with_range(entry.data, entry.range));

                        // (Add global variable here.)
                        self.shard.num_global_variables += 1;
                    }
                } else {
                    // Global variable.
                    self.shard.num_global_variables += 1;
                }
            }
        } else if self.shard.warn.check(line!()) { eprintln!("warning: unexpected form @0x{:x}: {:?}", offset.0, location_attr); }
        Ok(())
    }

    fn chase_origin_pointers(&mut self, mut attr_specification_or_origin: Option<Attribute<SliceType>>, name: &mut Option<&'a str>, linkage_name: &mut Option<&'a str>, type_: &mut Option<*const TypeInfo>, mut decl: Option<&mut LineInfo>) -> Result<()> {
        let mut cur_unit = self.unit;
        while let Some(specification_or_origin) = attr_specification_or_origin {
            let (next_unit, next_offset) = match specification_or_origin.value() {
                AttributeValue::UnitRef(off) => (cur_unit, off),
                AttributeValue::DebugInfoRef(off) => {
                    let idx = self.loader.sym.units.partition_point(|u| u.offset <= off);
                    if idx == 0 { return err!(Dwarf, "specification/abstract_origin offset out of bounds"); }
                    let u = &self.loader.sym.units[idx - 1];
                    let off = match off.to_unit_offset(&u.unit.header) {
                        None => return err!(Dwarf, "specification/abstract_origin entry offset out of unit bounds"),
                        Some(o) => o };
                    (&u.unit, off)
                }
                _ => return err!(Dwarf, "{:?} has unexpected form", specification_or_origin),
            };
            cur_unit = next_unit;

            let mut entries_iter = cur_unit.entries_raw(Some(next_offset))?;
            if entries_iter.is_empty() { return err!(Dwarf, "specification/abstract_origin offset is invalid"); }
            let abbrev = match entries_iter.read_abbreviation()? {
                None => return err!(Dwarf, "specification/abstract_origin points to null entry"),
                Some(a) => a };
            attr_specification_or_origin = None;
            let (mut attr_file, mut attr_line, mut attr_column) = (None, None, None);
            for &attr in abbrev.attributes() {
                match attr.name() {
                    DW_AT_name => {name.get_or_insert(parse_attr_str(&self.loader.sym.dwarf, cur_unit, &Some(entries_iter.read_attribute(attr)?))?);}
                    DW_AT_linkage_name => {linkage_name.get_or_insert(parse_attr_str(&self.loader.sym.dwarf, cur_unit, &Some(entries_iter.read_attribute(attr)?))?);}
                    DW_AT_type => {type_.get_or_insert(Self::parse_type_ref_custom(&entries_iter.read_attribute(attr)?, abbrev.tag(), cur_unit, &mut self.shard.warn, &self.loader.types.builtin_types));}
                    DW_AT_specification => attr_specification_or_origin = Some(entries_iter.read_attribute(attr)?),
                    DW_AT_abstract_origin => attr_specification_or_origin = Some(entries_iter.read_attribute(attr)?),
                    DW_AT_decl_file => attr_file = Some(entries_iter.read_attribute(attr)?),
                    DW_AT_decl_line => attr_line = Some(entries_iter.read_attribute(attr)?),
                    DW_AT_decl_column => attr_column = Some(entries_iter.read_attribute(attr)?),
                    _ => entries_iter.skip_attributes(&[attr])?
                }
            }
            if let Some(d) = &mut decl {
                if **d == LineInfo::invalid() && attr_file.is_some() {
                    **d = self.parse_line_info(attr_file, attr_line, attr_column);
                }
            }
        }
        Ok(())
    }
    
    fn load(&mut self) -> Result<()> {
        {
            let start_offset = self.unit.header.offset().as_debug_info_offset().unwrap();
            self.shard.types.set_unit(start_offset, DebugInfoOffset(start_offset.0 + self.unit.header.length_including_self()));
        }

        self.stack.push(LoaderStackEntry::default());

        // Iterate over DIEs in depth-first order.
        let mut cursor = self.unit.entries_raw(None)?;
        let mut prev_has_children = true;
        let mut skip_subtree = usize::MAX;
        loop {
            // Check if we're done.
            let offset = cursor.next_offset().to_debug_info_offset(&self.unit.header).unwrap();
            let next_depth = cursor.next_depth();
            if cursor.is_empty() {
                if cursor.next_depth() != 0 || self.depth != 1 {
                    return err!(Dwarf, "tree ended early @0x{:x}", offset.0);
                }
                return Ok(());
            }

            // Pop from the stack if needed.
            let finish_subtree = if !prev_has_children {
                if self.depth < 1 {
                    return err!(Dwarf, "tree underflow");
                }
                self.depth -= 1;
                let top = &self.stack[self.depth + 1];
                self.scope_name.truncate(top.scope_name_len);

                if skip_subtree == usize::MAX {
                    true
                } else {
                    if self.depth < skip_subtree {
                        skip_subtree = usize::MAX;
                    }
                    false
                }
            } else {
                false
            };

            if finish_subtree {
                let top = &mut self.stack[self.depth + 1];
                if top.subfunction != usize::MAX {
                    let start = self.shard.sym.local_variables.len();
                    self.shard.sym.local_variables.append(&mut top.local_variables);
                    let end = self.shard.sym.local_variables.len();
                    self.shard.sym.subfunctions[top.subfunction].local_variables = Subfunction::pack_range(start..end);
                }
                match top.tag {
                    DW_TAG_subprogram => self.finish_function(),
                    _ => (),
                }
            }

            // Read next DIE.
            let abbrev = cursor.read_abbreviation()?;
            prev_has_children = abbrev.is_some_and(|e| e.has_children());

            let abbrev = match abbrev {
                Some(a) => a,
                None => continue,
            };

            self.push(abbrev.tag());

            if skip_subtree != usize::MAX {
                cursor.skip_attributes(abbrev.attributes())?;
                continue;
            }

            // Each case here has to read/skip all attributes from `cursor`.
            match abbrev.tag() {
                DW_TAG_compile_unit | DW_TAG_partial_unit | DW_TAG_type_unit => {
                    let mut low_pc = None;
                    let mut high_pc = None;
                    let mut attr_ranges = None;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_low_pc => low_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_high_pc => high_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_ranges => attr_ranges = Some(cursor.read_attribute(attr)?),
                            DW_AT_language => {
                                let val = cursor.read_attribute(attr)?.value();
                                match val {
                                    AttributeValue::Language(lang) => self.unit_language = match lang {
                                        // Unfortunately the C_plus_plus_* constants are not consecutive and show no sign of becoming consecutive in future, so we'll have to update this list every few years.
                                        DW_LANG_C | DW_LANG_C11 | DW_LANG_C17 | DW_LANG_C89 | DW_LANG_C99 | DW_LANG_C_plus_plus | DW_LANG_C_plus_plus_03 | DW_LANG_C_plus_plus_11 | DW_LANG_C_plus_plus_14 | DW_LANG_C_plus_plus_17 | DW_LANG_C_plus_plus_20 => LanguageFamily::Cpp,
                                        DW_LANG_Rust => LanguageFamily::Rust,
                                        _ => LanguageFamily::Other,
                                    },
                                    _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", attr.name(), val); }
                                }
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    parse_attr_ranges(&self.loader.sym.dwarf, self.unit, &low_pc, &high_pc, &attr_ranges, offset, self.loader.sym.code_addr_range.start, &mut self.stack[self.depth].pc_ranges, &mut self.shard.warn)?;
                    self.stack[self.depth].pc_ranges_depth = self.depth;
                }

                // This tag means that the debug info is split into a separate file using DWARF's complicated mechanism for that.
                // Not sure if this is worth supporting, since simpler ways of splitting are available: debuglink, or just providing an unstripped executable on the side.
                DW_TAG_skeleton_unit => return err!(NotImplemented, "skeleton units not supported (too spooky)"),

                // Namespaces.
                DW_TAG_namespace => {
                    let mut has_name = false;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_name => {
                                let name = parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?;
                                has_name = true;
                                self.append_namespace_to_scope_name(name, true);
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    if !has_name {
                        self.append_namespace_to_scope_name("_", false);
                    }
                }

                // Variables.
                DW_TAG_variable | DW_TAG_formal_parameter => {
                    // Applicable attributes:
                    // Useful: DECL, name, linkage_name, location, type, declaration, specification
                    // Other: artificial, accessibility, alignment, const_expr, const_value, endianity, external, segment, start_scope, visibility
                    // Other (for parameters): default_value, is_optional, variable_parameter
                    let mut name = None;
                    let mut linkage_name = None;
                    let mut type_ = None;
                    let mut attr_specification_or_origin = None;
                    let mut attr_location = None;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_name => name = Some(parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?),
                            DW_AT_linkage_name => linkage_name = Some(parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?),
                            DW_AT_type => type_ = Some(self.parse_type_ref(&attr, abbrev.tag(), &mut cursor)?),
                            DW_AT_specification | DW_AT_abstract_origin => attr_specification_or_origin = Some(cursor.read_attribute(attr)?),
                            DW_AT_location => attr_location = Some(cursor.read_attribute(attr)?), 
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }

                    if let Some(loc) = attr_location {
                        self.chase_origin_pointers(attr_specification_or_origin, &mut name, &mut linkage_name, &mut type_, None)?;
                        let var_flags = if abbrev.tag() == DW_TAG_variable { LocalVariableFlags::empty() } else { LocalVariableFlags::PARAMETER };
                        let var = LocalVariable {
                            name_ptr: name.map_or(ptr::null(), |n| n.as_ptr()), name_len: name.map_or(0, |n| n.len().min(u32::MAX as usize) as u32), type_: type_.unwrap_or(self.loader.types.builtin_types.unknown), offset_and_flags: LocalVariable::pack_offset_and_flags(offset, var_flags),
                            expr: Expression(SliceType::default()), start: 0, len: 0};
                        self.add_variable_locations(&loc, &var, offset)?;
                    }
                }

                // Varargs.
                DW_TAG_unspecified_parameters | DW_TAG_GNU_formal_parameter_pack => {
                    skip_subtree = self.depth;
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                // Call sites (seem to be very incomplete, presumed useless).
                DW_TAG_GNU_call_site | DW_TAG_call_site | DW_TAG_GNU_call_site_parameter | DW_TAG_call_site_parameter => {
                    skip_subtree = self.depth;
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                // Functions.
                DW_TAG_subprogram => {
                    // Applicable attributes:
                    // Useful: declaration, specification, frame_base, low_pc, high_pc, ranges, name, linkage_name, main_subprogram (sometimes missing for some reason), object_pointer, return_addr, type, inline
                    // Other: artificial, calling_convention, entry_pc, start_scope (haven't seen it in practice), trampoline, virtuality, vtable_elem_location
                    // Other: accessibility, address_class, alignment, defaulted, deleted, elemental, pure, explicit, external, noreturn, prototyped, recursive, reference, rvalue_reference, segment, static_link, visibility
                    let mut low_pc = None;
                    let mut high_pc = None;
                    let mut attr_ranges = None;
                    let mut name = None;
                    let mut linkage_name = None;
                    let mut type_ = None;
                    let mut attr_specification_or_origin = None;
                    let mut frame_base = None;
                    let mut is_inlined = false;
                    let (mut decl_file, mut decl_line, mut decl_column) = (None, None, None);
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_low_pc => low_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_high_pc => high_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_ranges => attr_ranges = Some(cursor.read_attribute(attr)?),
                            DW_AT_name => name = Some(parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?),
                            DW_AT_linkage_name => linkage_name = Some(parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?),
                            DW_AT_type => type_ = Some(self.parse_type_ref(&attr, abbrev.tag(), &mut cursor)?),
                            DW_AT_specification | DW_AT_abstract_origin => attr_specification_or_origin = Some(cursor.read_attribute(attr)?),
                            DW_AT_frame_base => frame_base = Some(cursor.read_attribute(attr)?),
                            DW_AT_decl_file => decl_file = Some(cursor.read_attribute(attr)?),
                            DW_AT_decl_line => decl_line = Some(cursor.read_attribute(attr)?),
                            DW_AT_decl_column => decl_column = Some(cursor.read_attribute(attr)?),
                            DW_AT_inline => {
                                let val = cursor.read_attribute(attr)?.value();
                                match &val {
                                    AttributeValue::Inline(inl) => match *inl {
                                        // The value of this attribute seems unreliable, let's ignore it.
                                        // I've seen it say DW_INL_declared_not_inlined when the function is actually inlined,
                                        // and there are DW_TAG_inlined_subroutine-s with DW_AT_abstract_origin pointing to this exact DIE.
                                        // (That was g++. Clang seems to only ever use DW_INL_inlined for this value.)
                                        //DW_INL_declared_not_inlined | DW_INL_not_inlined => (),
                                        _ => is_inlined = true,
                                    }
                                    _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", attr.name(), val); }
                                }
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    let top = &mut self.stack[self.depth];
                    parse_attr_ranges(&self.loader.sym.dwarf, self.unit, &low_pc, &high_pc, &attr_ranges, offset, self.loader.sym.code_addr_range.start, &mut top.pc_ranges, &mut self.shard.warn)?;
                    top.function_depth = self.depth;
                    top.pc_ranges_depth = self.depth;
                    top.subfunction_depth = self.depth;

                    if !top.pc_ranges.is_empty() || is_inlined {
                        // Chase the specification/origin pointers to reach the function name (for function declarations and inlining stuff).
                        // Functions can be missing one of the two names (linkage name or regular name), but not both (in the binaries I looked at).
                        let mut decl = self.parse_line_info(decl_file, decl_line, decl_column);
                        self.chase_origin_pointers(attr_specification_or_origin, &mut name, &mut linkage_name, &mut type_, Some(&mut decl))?;
                        let top = &mut self.stack[self.depth];

                        let name_ref = if let Some(n) = linkage_name {
                            n
                        } else {
                            // Usually we have DW_AT_linkage_name (fully qualified and mangled), but if it's missing we use namespace + DW_AT_name.
                            let mut out = self.shard.sym.misc_arena.write();
                            write!(out, "{}{}", self.scope_name, if self.scope_name.is_empty() {""} else {"::"})?;
                            if let Some(n) = name {
                                write!(out, "{}", n)?;
                            } else {
                                let mut found = false;
                                if let Some(file_idx) = decl.file_idx() {
                                    if let Some(file) = self.loader.sym.files.get(file_idx) {
                                        write!(out, "[function at {}:{}]", Path::new(file.filename.file_name().unwrap_or_else(|| OsStr::new("?"))).display(), decl.line()).unwrap();
                                        found = true;
                                    }
                                }
                                if !found {
                                    write!(out, "[function at DIE 0x{:x}]", offset.0)?;
                                }
                            }
                            out.finish_str()
                        };

                        // Function may have multiple ranges. In practice this happens because of compiler optimizations, e.g. splitting a function into hot and cold part.
                        // These ranges may also be present in .symtab with names like add_path.constprop.0.isra.0.cold , where "constprop", "isra", and "cold" are
                        // the transformations that the compiler did to arrive at these ranges.
                        // Do multi-ranges also appear on C++ coroutines or Rust async functions? No, they produce multiple functions instead.

                        top.function = self.shard.functions.len();
                        let mut f = FunctionInfo {addr: FunctionAddr(0), prev_addr: FunctionAddr(0), die: offset, mangled_name: name_ref.as_ptr(), mangled_name_len: name_ref.len() as u32, shard_idx: self.shard_idx as u16, flags: FunctionFlags::empty(), language: self.unit_language, subfunction_levels: 0..0};
                        if is_inlined {
                            f.flags.insert(FunctionFlags::INLINED);
                        }
                        if top.pc_ranges.is_empty() {
                            // Inline-only function.
                            f.addr = FunctionAddr::inline(f.die);
                            f.prev_addr = f.addr;
                            self.shard.functions.push(f);
                        } else {
                            top.pc_ranges.sort_unstable_by_key(|r| r.begin);
                            let sf_idx = self.shard.sym.subfunctions.len();
                            self.shard.sym.subfunctions.push(Subfunction {callee_idx: offset.0, parent: usize::MAX, local_variables: Subfunction::pack_range(0..0), call_line: decl, level: 0, last_range_idx: usize::MAX});
                            top.subfunction = sf_idx;
                            for (i, range) in top.pc_ranges.iter().enumerate() {
                                let mut f = f.clone();
                                f.addr = FunctionAddr::new(range.begin as usize);
                                f.prev_addr = FunctionAddr::new(top.pc_ranges[(i + top.pc_ranges.len() - 1) % top.pc_ranges.len()].begin as usize);
                                self.shard.functions.push(f);

                                top.subfunction_events.push(SubfunctionEvent {addr: range.begin as usize, level: 0, subfunction_idx: sf_idx, sign: 1});
                                top.subfunction_events.push(SubfunctionEvent {addr: range.end as usize, level: 0, subfunction_idx: sf_idx, sign: -1});

                                // Functions often overlap, especially with .symtab functions, idk why. So don't add function end markers.
                                self.shard.max_function_end = self.shard.max_function_end.max(range.end as usize);
                            }
                        }

                        if let Some(frame_base) = frame_base {
                            let var = LocalVariable {
                                name_ptr: "#frame_base".as_ptr(), name_len: "#frame_base".len() as u32, type_: self.loader.types.builtin_types.void_pointer, offset_and_flags: LocalVariable::pack_offset_and_flags(offset, LocalVariableFlags::FRAME_BASE),
                                expr: Expression(SliceType::default()), start: 0, len: 0};
                            self.add_variable_locations(&frame_base, &var, offset)?;
                        }
                    }

                    // Would be nice to use the actual function name instead of "_", but there are difficulties:
                    //  * DW_AT_name may be missing and only DW_AT_linkage_name present,
                    //  * DW_AT_linkage_name is fully qualified, so should override previous scope name rather than be appended to it,
                    //  * demangling DW_AT_linkage_name is slow, so it should only be done if the function actually contains nested types,
                    //  * function may be overloaded, so we'd need to include some description of its parameters,
                    //  * but if the function is not overloaded, it's better to omit the parameters.
                    self.append_namespace_to_scope_name("_", false);
                }

                DW_TAG_enumeration_type if self.stack[self.depth - 1].tag == DW_TAG_array_type => {
                    // Haven't seen these in C++ or Rust.
                    if self.shard.warn.check(line!()) { eprintln!("warning: enum-valued array indices are not supported (have one @0x{:x})", offset.0); }
                    cursor.skip_attributes(abbrev.attributes())?;
                }
                    
                // Types.
                DW_TAG_base_type | DW_TAG_unspecified_type | DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type | DW_TAG_pointer_type | DW_TAG_reference_type | DW_TAG_rvalue_reference_type | DW_TAG_array_type | DW_TAG_const_type | DW_TAG_restrict_type | DW_TAG_volatile_type | DW_TAG_atomic_type | DW_TAG_typedef => {
                    // Applicable attributes (DW_AT_*) (DECL means decl_file, decl_line, decl_column):
                    // Useful: DECL, name, bit_size, byte_size, declaration, type, signature, alignment
                    // For base types: encoding
                    // For arrays and enums used as array index: bit_stride, byte_stride, ordering, rank
                    // Possible in theory, but I haven't seen these on types: specification, abstract_origin
                    // Other: data_bit_offset, data_location
                    // Other: binary_scale, decimal_scale, decimal_sign, digit_count, picture_string, small
                    // Other: allocated, associated, enum_class, accessibility, visibility, start_scope, export_symbols, endianity, calling_convention, address_class
                    let mut is_alias = false;
                    let t = match abbrev.tag() {
                        DW_TAG_base_type => Type::Primitive(PrimitiveFlags::empty()),
                        DW_TAG_unspecified_type => Type::Primitive(PrimitiveFlags::UNSPECIFIED),
                        DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type => Type::Struct(StructType::default()),
                        DW_TAG_enumeration_type => Type::Enum(EnumType {enumerands: &[], type_: self.loader.types.builtin_types.unknown}),
                        DW_TAG_pointer_type | DW_TAG_reference_type | DW_TAG_rvalue_reference_type => Type::Pointer(
                            PointerType {flags: if abbrev.tag() == DW_TAG_pointer_type {PointerFlags::empty()} else {PointerFlags::REFERENCE}, type_: self.loader.types.builtin_types.unknown}),
                        DW_TAG_array_type => Type::Array(ArrayType {flags: ArrayFlags::empty(), type_: self.loader.types.builtin_types.unknown, stride: 0, len: 0}),
                        DW_TAG_const_type | DW_TAG_restrict_type | DW_TAG_volatile_type | DW_TAG_atomic_type | DW_TAG_typedef => {
                            is_alias = true;
                            Type::Pointer(PointerType {flags: PointerFlags::empty(), type_: self.loader.types.builtin_types.unknown})
                        }
                        _ => panic!("huh?") };
                    let mut info = TypeInfo {die: offset, t, language: self.unit_language, ..Default::default()};
                    let mut saw_type = false;
                    let mut is_complex_float = false;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_name => {
                                let n = parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?.to_string();
                                self.append_namespace_to_scope_name(&n, true);
                                info.name = self.shard.types.temp_types.unsorted_type_names.add_str(&self.scope_name, 0);
                                if self.stack[self.depth].scope_name_is_linkable {
                                    info.flags.insert(TypeFlags::LINKABLE_NAME);
                                }
                            }
                            DW_AT_byte_size | DW_AT_bit_size => {
                                let mut s = cursor.read_attribute(attr)?.value().udata_value().ok_or_else(|| error!(Dwarf, "{} has unexpected form", attr.name()))? as usize;
                                if attr.name() == DW_AT_bit_size {
                                    if s & 7 != 0 {
                                        if self.shard.warn.check(line!()) { eprintln!("warning: {} = {} on {} is not supported", attr.name(), s, abbrev.tag()); }
                                        info.flags.insert(TypeFlags::UNSUPPORTED);
                                    }
                                    s = (s+7)/8;
                                }
                                info.size = s;
                                info.flags.insert(TypeFlags::SIZE_KNOWN);
                            }
                            DW_AT_declaration => info.flags.insert(TypeFlags::DECLARATION),
                            DW_AT_specification | DW_AT_abstract_origin => if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} is not supported", attr.name(), abbrev.tag()); }
                            DW_AT_type => {
                                saw_type = true;
                                let type_ = self.parse_type_ref(&attr, abbrev.tag(), &mut cursor)?;
                                match &mut info.t {
                                    Type::Enum(e) => e.type_ = type_,
                                    Type::Pointer(p) => p.type_ = type_,
                                    Type::Array(a) => a.type_ = type_,
                                    _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} is not supported", attr.name(), abbrev.tag()); }
                                }
                            }
                            DW_AT_encoding => {
                                let val = cursor.read_attribute(attr)?.value();
                                match &mut info.t {
                                    Type::Primitive(p) => {
                                        match val {
                                            AttributeValue::Encoding(e) => match e {
                                                // Why did DW eat all these things? Stop it, DW! Bad dog!
                                                DW_ATE_address => (),
                                                DW_ATE_boolean => p.insert(PrimitiveFlags::BOOL),
                                                DW_ATE_imaginary_float | DW_ATE_float => p.insert(PrimitiveFlags::FLOAT),
                                                DW_ATE_signed => p.insert(PrimitiveFlags::SIGNED),
                                                DW_ATE_signed_char => p.insert(PrimitiveFlags::SIGNED | PrimitiveFlags::CHAR | if self.unit_language.presumably_cpp() {PrimitiveFlags::AMBIGUOUS_CHAR} else {PrimitiveFlags::empty()}),
                                                DW_ATE_unsigned => (),
                                                DW_ATE_unsigned_char => p.insert(PrimitiveFlags::CHAR),
                                                DW_ATE_UTF | DW_ATE_UCS | DW_ATE_ASCII => p.insert(PrimitiveFlags::CHAR | if self.unit_language.presumably_cpp() {PrimitiveFlags::AMBIGUOUS_CHAR} else {PrimitiveFlags::empty()}),
                                                DW_ATE_complex_float => is_complex_float = true,
                                                //DW_ATE_packed_decimal | DW_ATE_numeric_string | DW_ATE_edited | DW_ATE_signed_fixed | DW_ATE_unsigned_fixed | DW_ATE_decimal_float
                                                _ => {
                                                    if self.shard.warn.check(line!()) { eprintln!("warning: {} = {} on {} is not supported", attr.name(), e, abbrev.tag()); }
                                                    info.flags.insert(TypeFlags::UNSUPPORTED);
                                                }
                                            }
                                            _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} = {:?} on {} is not supported", attr.name(), val, abbrev.tag()); }
                                        }
                                    }
                                    // The spec doesn't seem to allow DW_AT_encoding on DW_TAG_enumeration_type, but it appears in most Linux standard libraries, e.g. libc.so.6
                                    // In those cases, both DW_AT_encoding and DW_AT_type are present, and the former matches the encoding inside the type pointed to by the latter.
                                    // So we just ignore it.
                                    Type::Enum(_) => (),
                                    _ => {
                                        if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} is not supported", attr.name(), abbrev.tag()); }
                                        info.flags.insert(TypeFlags::UNSUPPORTED)
                                    }
                                }
                            }
                            DW_AT_bit_stride | DW_AT_byte_stride => if let Some(s) = parse_attr_stride(cursor.read_attribute(attr)?, offset, &mut self.shard.warn) {
                                match &mut info.t {
                                    Type::Array(a) => a.stride = s,
                                    _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} is not supported", attr.name(), abbrev.tag()); }
                                }
                            }
                            DW_AT_ordering | DW_AT_rank => {
                                if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} is not supported", attr.name(), abbrev.tag()); }
                                cursor.skip_attributes(&[attr])?;
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    if info.name.is_empty() {
                        self.append_namespace_to_scope_name("_", false);
                    }

                    if is_complex_float {
                        // Treat complex float as a struct rather than primitive type.
                        let inner_size = if !info.flags.contains(TypeFlags::SIZE_KNOWN) {
                            if self.shard.warn.check(line!()) { eprintln!("warning: DW_ATE_complex_float without size @0x{:x}", offset.0); }
                            8
                        } else if info.size != 8 && info.size != 16 && info.size != 32 {
                            if self.shard.warn.check(line!()) { eprintln!("warning: DW_ATE_complex_float with unexpected size: {} @0x{:x}", info.size, offset.0); }
                            8
                        } else {
                            info.size / 2
                        };
                        let inner_type = match inner_size {
                            4 => self.loader.types.builtin_types.f32_,
                            8 => self.loader.types.builtin_types.f64_,
                            16 => self.loader.types.builtin_types.f128_,
                            _ => panic!("huh"),
                        };
                        let fields = [
                            StructField {name: "real", flags: FieldFlags::empty(), bit_offset: 0, bit_size: 0, type_: inner_type, discr_value: 0},
                            StructField {name: "imag", flags: FieldFlags::empty(), bit_offset: inner_size * 8, bit_size: 0, type_: inner_type, discr_value: 0}];
                        let fields = self.shard.types.temp_types.fields_arena.add_slice(&fields);
                        let mut struct_type = StructType::default();
                        struct_type.set_fields(fields);
                        info.t = Type::Struct(struct_type);
                    }

                    if !saw_type {
                        // Pointer without DW_AT_type is a void*
                        match &mut info.t {
                            Type::Pointer(p) => p.type_ = self.loader.types.builtin_types.void, // (this will apply to typedefs too, seems ok)
                            _ => (),
                        }
                    }
                    if !info.flags.contains(TypeFlags::SIZE_KNOWN) {
                        match &info.t {
                            Type::Primitive(_) | Type::Pointer(_) => {
                                info.size = 8;
                                info.flags.insert(TypeFlags::SIZE_KNOWN);
                            }
                            _ => (),
                        }
                    }

                    if abbrev.tag() == DW_TAG_base_type {
                        let type_enum = if let &Type::Primitive(flags) = &info.t {
                            match info.size {
                                8 if flags.contains(PrimitiveFlags::FLOAT) => ValueType::F64,
                                8 if flags.contains(PrimitiveFlags::SIGNED) => ValueType::I64,
                                8 => ValueType::U64,
                                4 if flags.contains(PrimitiveFlags::FLOAT) => ValueType::F32,
                                4 if flags.contains(PrimitiveFlags::SIGNED) => ValueType::I32,
                                4 => ValueType::U32,
                                2 if flags.contains(PrimitiveFlags::SIGNED) => ValueType::I16,
                                2 => ValueType::U16,
                                1 if flags.contains(PrimitiveFlags::SIGNED) => ValueType::I8,
                                1 => ValueType::U8,
                                _ => ValueType::Generic, // unexpected
                            }
                        } else {
                            ValueType::Generic // probably a 16-byte complex float, not really supported by gimli; use 8-byte Generic instead
                        };
                        debug_assert!(mem::size_of::<ValueType>() == 1);
                        self.shard.base_types.push((offset.0 as u64) << 8 | type_enum as u64);
                    }

                    if is_alias {
                        self.shard.types.add_alias(offset, DebugInfoOffset(info.t.as_pointer().unwrap().type_ as usize), info.name);
                    } else {
                        let t = self.shard.types.add_type(info).0;
                        assert!(t != ptr::null());
                        self.stack[self.depth].exact_type = t as *mut TypeInfo;
                    }
                }

                // Fields and enumerators.
                DW_TAG_inheritance | DW_TAG_member | DW_TAG_enumerator => {
                    // Applicable attributes:
                    // Useful: DECL, name, artificial, bit_size, byte_size, data_bit_offset, data_member_location, external (seems undocumented), declaration, type
                    // For enumerators: const_value
                    // Other: accessibility, mutable, visibility, virtuality
                    let type_ = self.stack[self.depth - 1].exact_type;
                    if type_ == ptr::null_mut() {
                        if self.shard.warn.check(line!()) { eprintln!("warning: {} in {} is not supported", abbrev.tag(), self.stack[self.depth - 1].tag); }
                        cursor.skip_attributes(abbrev.attributes())?;
                    } else {
                        let mut field = StructField {name: "", flags: FieldFlags::empty(), bit_offset: 0, bit_size: 0, type_: self.loader.types.builtin_types.unknown, discr_value: 0};
                        if let Some(variant) = &self.stack[self.depth - 1].variant {
                            if offset == variant.discriminant_die {
                                field.flags.insert(FieldFlags::DISCRIMINANT);
                            }
                            field.flags.insert(variant.field_flags);
                            field.discr_value = variant.discr_value;
                        }
                        if abbrev.tag() == DW_TAG_inheritance {
                            field.flags.insert(FieldFlags::INHERITANCE);
                            field.name = "#base";
                        }
                        let mut enum_value = 0usize;
                        let mut is_static_field = false;
                        for &attr in abbrev.attributes() {
                            match attr.name() {
                                DW_AT_name => field.name = unsafe {mem::transmute(parse_attr_str(&self.loader.sym.dwarf, self.unit, &Some(cursor.read_attribute(attr)?))?)},
                                DW_AT_type => field.type_ = self.parse_type_ref(&attr, abbrev.tag(), &mut cursor)?,
                                DW_AT_bit_size | DW_AT_byte_size => {
                                    let mut s = cursor.read_attribute(attr)?.value().udata_value().ok_or_else(|| error!(Dwarf, "{} has unexpected form", attr.name()))? as usize;
                                    if attr.name() == DW_AT_byte_size {
                                        s *= 8;
                                    }
                                    field.bit_size = s;
                                    field.flags.insert(FieldFlags::SIZE_KNOWN);
                                }
                                DW_AT_data_bit_offset | DW_AT_data_member_location => {
                                    let val = cursor.read_attribute(attr)?.value();
                                    if let Some(mut x) = val.udata_value() {
                                        if attr.name() == DW_AT_data_member_location {
                                            x *= 8;
                                        }
                                        field.bit_offset = x as usize;
                                    } else {
                                        unsafe {(*type_).flags.insert(TypeFlags::UNSUPPORTED)};
                                        if let AttributeValue::Exprloc(_) = val {
                                            // Virtual inheritance, not supported right now.
                                        } else {
                                            if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} has unexpected form: {:?}", attr.name(), abbrev.tag(), val); }
                                        }
                                    }
                                }
                                DW_AT_const_value if abbrev.tag() == DW_TAG_enumerator => {
                                    let val = cursor.read_attribute(attr)?.value();
                                    match val.udata_value() {
                                        Some(x) => enum_value = x as usize,
                                        None => match val {
                                            AttributeValue::Sdata(x) => enum_value = x as usize, // udata_value() is too picky about signedness
                                            _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} on {} has unexpected form: {:?}", attr.name(), abbrev.tag(), val); }
                                        }
                                    }
                                }
                                DW_AT_external => { // static field, i.e. not a field at all but a global variable (why are you like this, C++)
                                    cursor.skip_attributes(&[attr])?;
                                    if abbrev.tag() == DW_TAG_member {
                                        is_static_field = true;
                                    } else if self.shard.warn.check(line!()) { eprintln!("warning: unexpected {} on {}", attr.name(), abbrev.tag()); }
                                }
                                DW_AT_artificial => {
                                    cursor.skip_attributes(&[attr])?;
                                    field.flags.insert(FieldFlags::ARTIFICIAL);
                                }
                                _ => cursor.skip_attributes(&[attr])?,
                            }
                        }
                        if abbrev.tag() == DW_TAG_enumerator {
                            unsafe {
                                match &mut (*type_).t {
                                    Type::Enum(e) => self.shard.types.temp_types.add_enumerand(e, Enumerand {value: enum_value, name: field.name, flags: EnumerandFlags::empty()}),
                                    _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} in {} is not supported", abbrev.tag(), self.stack[self.depth - 1].tag); }
                                }
                            }
                        } else {
                            if is_static_field {
                                // (Once we have global variables, actually add the variable here.)
                                self.shard.num_global_variables += 1;
                            } else {
                                if field.type_ == self.loader.types.builtin_types.unknown {
                                    if self.shard.warn.check(line!()) { eprintln!("warning: field with no type @0x{:x}", offset.0); }
                                }
                                unsafe {
                                    match &mut (*type_).t {
                                        Type::Struct(s) => self.shard.types.temp_types.add_field(s, field),
                                        _ => if self.shard.warn.check(line!()) { eprintln!("warning: {} in {} is not supported", abbrev.tag(), self.stack[self.depth - 1].tag); }
                                    }
                                }
                            }
                        }
                    }
                }

                DW_TAG_subrange_type if self.stack[self.depth - 1].tag != DW_TAG_array_type => {
                    if self.shard.warn.check(line!()) { eprintln!("warning: {} in {} is not supported", abbrev.tag(), self.stack[self.depth - 1].tag); }
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                // Array dimensions.
                DW_TAG_subrange_type => {
                    // Applicable attributes:
                    // Useful: count, byte_stride, bit_stride, byte_size, bit_size, lower_bound, upper_bound, type
                    // Other: alignment, data_location, declaration, threads_scaled
                    // Other: DECL, name, accessibility, allocated, associated, visibility
                    let mut array = unsafe {(*self.stack[self.depth - 1].exact_type).t.as_array_mut().unwrap()};
                    if !array.flags.contains(ArrayFlags::PARSED_SUBRANGE) {
                        array.flags.insert(ArrayFlags::PARSED_SUBRANGE);
                    } else {
                        // Turn multidimensional array into array of arrays.
                        let info = TypeInfo {die: offset, language: self.unit_language, t: Type::Array(ArrayType {flags: ArrayFlags::PARSED_SUBRANGE, type_: array.type_, stride: 0, len: 0}), ..TypeInfo::default()};
                        let (ptr, off) = self.shard.types.add_type(info);
                        assert!(ptr != ptr::null());
                        array.type_ = off;
                        self.stack[self.depth - 1].exact_type = ptr as *mut TypeInfo;
                        array = unsafe {(*self.stack[self.depth - 1].exact_type).t.as_array_mut().unwrap()};
                    }

                    let mut lower_bound: u64 = 0;
                    let mut upper_bound: Option<u64> = None;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_byte_stride | DW_AT_bit_stride => if let Some(s) = parse_attr_stride(cursor.read_attribute(attr)?, offset, &mut self.shard.warn) {
                                array.stride = s;
                            },
                            DW_AT_count => match cursor.read_attribute(attr)?.value().udata_value() {
                                None => (), // likely VLA
                                Some(n) => {
                                    array.len = n as usize;
                                    array.flags.insert(ArrayFlags::LEN_KNOWN);
                                }
                            }
                            DW_AT_lower_bound => match cursor.read_attribute(attr)?.value().udata_value() {
                                None => (), // likely VLA
                                Some(s) => {
                                    if s != 0 && self.shard.warn.check(line!()) { eprintln!("warning: arrays with nonzero lower bound are not supported (have one @0x{:x})", offset.0); }
                                    // We calculate length as upper_bound-lower_bound, but don't subtract lower bound when indexing in watch expressions.
                                    lower_bound = s;
                                }
                            }
                            DW_AT_upper_bound => match cursor.read_attribute(attr)?.value().udata_value() {
                                None => (), // likely VLA
                                Some(s) => upper_bound = Some(s),
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    if let Some(upper_bound) = upper_bound {
                        if lower_bound > upper_bound {
                            if self.shard.warn.check(line!()) { eprintln!("warning: array has lower bound {} > upper bound {} @0x{:x}", lower_bound, upper_bound, offset.0); }
                        } else if !array.flags.contains(ArrayFlags::LEN_KNOWN) {
                            array.len = (upper_bound - lower_bound + 1) as usize;
                            array.flags.insert(ArrayFlags::LEN_KNOWN);
                        }
                    }
                }

                DW_TAG_generic_subrange => {
                    // Haven't seen these in C++ or Rust.
                    if self.shard.warn.check(line!()) { eprintln!("warning: arrays with dynamic number of dimensions are not supported (got one @0x{:x}", offset.0); }
                        cursor.skip_attributes(abbrev.attributes())?;
                }

                // Rust enum.
                // The tree usually looks like this:
                //   structure_type
                //     variant_part - with DW_AT_discr pointing to the discriminant member
                //       member - discriminant
                //       variant - with DW_AT_discr_value
                //         member
                //       variant
                //         member
                //       ...
                DW_TAG_variant_part => {
                    // Applicable attributes:
                    // Useful: discr
                    // Other: accessibility, declaration, type
                    let mut variant = VariantInfo {discriminant_die: DebugInfoOffset(0), field_flags: FieldFlags::empty(), discr_value: 0};
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_discr => match cursor.read_attribute(attr)?.value() {
                                AttributeValue::UnitRef(unit_offset) => variant.discriminant_die = unit_offset.to_debug_info_offset(&self.unit.header).unwrap(),
                                v => if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", attr.name(), v); }
                            }
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    let t = self.stack[self.depth - 1].exact_type;
                    self.stack[self.depth].exact_type = t;
                    self.stack[self.depth].variant = Some(variant);
                }
                DW_TAG_variant => {
                    // Applicable attributes:
                    // Useful: discr_value, discr_list
                    // Other: accessibility, declaration
                    let mut variant = self.stack[self.depth - 1].variant.clone();
                    let mut found_discr = false;
                    if let Some(variant) = &mut variant {
                        for &attr in abbrev.attributes() {
                            match attr.name() {
                                DW_AT_discr_value => {
                                    found_discr = true;
                                    let v = cursor.read_attribute(attr)?.value();
                                    if let Some(x) = v.udata_value().or_else(|| v.sdata_value().map(|x| x as u64)) {
                                        variant.discr_value = x as usize;
                                        variant.field_flags.insert(FieldFlags::VARIANT);
                                    } else {
                                        if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", attr.name(), v); }
                                    }
                                }
                                // (I looked at one Rust binary, and there were no disct_list-s, so not supporting it yet.)
                                DW_AT_discr_list => {
                                    found_discr = true;
                                    if self.shard.warn.check(line!()) { eprintln!("warning: DW_AT_discr_list is not supported"); }
                                }
                                _ => cursor.skip_attributes(&[attr])?,
                            }
                        }
                        if !found_discr {
                            variant.field_flags.insert(FieldFlags::DEFAULT_VARIANT);
                        }
                    } else {
                        if self.shard.warn.check(line!()) { eprintln!("warning: variant is not inside variant_part @0x{:x}", offset.0); }
                        cursor.skip_attributes(abbrev.attributes())?;
                    }
                    let t = self.stack[self.depth - 1].exact_type;
                    self.stack[self.depth].exact_type = t;
                    self.stack[self.depth].variant = variant;
                }

                // TODO: Function pointers and pointers to members.
                DW_TAG_ptr_to_member_type | DW_TAG_subroutine_type => {
                    self.shard.types.add_type(TypeInfo {name: "<unsupported>", die: offset, language: self.unit_language, flags: TypeFlags::UNSUPPORTED, ..TypeInfo::default()});
                    skip_subtree = self.depth;
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                // Lexical block, just provides address ranges for the variables it contains.
                DW_TAG_lexical_block => {
                    let mut low_pc = None;
                    let mut high_pc = None;
                    let mut attr_ranges = None;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_low_pc => low_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_high_pc => high_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_ranges => attr_ranges = Some(cursor.read_attribute(attr)?),
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    if low_pc.is_some() || high_pc.is_some() || attr_ranges.is_some() {
                        parse_attr_ranges(&self.loader.sym.dwarf, self.unit, &low_pc, &high_pc, &attr_ranges, offset, self.loader.sym.code_addr_range.start, &mut self.stack[self.depth].pc_ranges, &mut self.shard.warn)?;
                        self.stack[self.depth].pc_ranges_depth = self.depth;
                    }
                }

                DW_TAG_inlined_subroutine if self.stack[self.depth].function_depth == 0 => {
                    if self.shard.warn.check(line!()) { eprintln!("warning: unexpected inlined_subroutine not inside function @0x{:x}", offset.0); }
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                // Inlined functions.
                DW_TAG_inlined_subroutine => {
                    // Applicable attributes:
                    // Useful: call_file, call_line, call_column, low_pc, high_pc, ranges, abstract_origin
                    // Other: const_expr, entry_pc, return_addr, segment, start_scope, trampoline
                    let (mut low_pc, mut high_pc, mut attr_ranges) = (None, None, None);
                    let (mut call_file, mut call_line, mut call_column) = (None, None, None);
                    let mut abstract_origin = None;
                    for &attr in abbrev.attributes() {
                        match attr.name() {
                            DW_AT_low_pc => low_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_high_pc => high_pc = Some(cursor.read_attribute(attr)?),
                            DW_AT_ranges => attr_ranges = Some(cursor.read_attribute(attr)?),
                            DW_AT_call_file => call_file = Some(cursor.read_attribute(attr)?),
                            DW_AT_call_line => call_line = Some(cursor.read_attribute(attr)?),
                            DW_AT_call_column => call_column = Some(cursor.read_attribute(attr)?),
                            DW_AT_abstract_origin => abstract_origin = Some(cursor.read_attribute(attr)?),
                            _ => cursor.skip_attributes(&[attr])?,
                        }
                    }
                    let top = &mut self.stack[self.depth];
                    parse_attr_ranges(&self.loader.sym.dwarf, self.unit, &low_pc, &high_pc, &attr_ranges, offset, self.loader.sym.code_addr_range.start, &mut top.pc_ranges, &mut self.shard.warn)?;
                    top.pc_ranges_depth = self.depth;
                    let parent_depth = top.subfunction_depth;
                    top.subfunction_depth = self.depth;

                    let function_depth = top.function_depth;
                    if self.stack[function_depth].subfunction != usize::MAX {
                        assert!(parent_depth != 0);
                        let call_line = self.parse_line_info(call_file, call_line, call_column);
                        let callee_die = match abstract_origin {
                            None => {
                                if self.shard.warn.check(line!()) { eprintln!("warning: inlined_subroutine doesn't say which function was inlined (DW_AT_abstract_origin) @0x{:x}", offset.0); }
                                DebugInfoOffset(usize::MAX)
                            }
                            Some(attr) => match attr.value() {
                                AttributeValue::UnitRef(off) => off.to_debug_info_offset(&self.unit.header).unwrap(),
                                AttributeValue::DebugInfoRef(off) => off,
                                _ => {
                                    if self.shard.warn.check(line!()) { eprintln!("warning: {} has unexpected form: {:?}", attr.name(), attr.value()); }
                                    DebugInfoOffset(usize::MAX)
                                }
                            }
                        };

                        let parent_idx = self.stack[parent_depth].subfunction;
                        assert!(parent_idx != usize::MAX);
                        let level = self.shard.sym.subfunctions[parent_idx].level + 1;
                        let sf_idx = self.shard.sym.subfunctions.len();
                        self.shard.sym.subfunctions.push(Subfunction {callee_idx: callee_die.0, parent: parent_idx, local_variables: Subfunction::pack_range(0..0), call_line: call_line.clone(), level, last_range_idx: usize::MAX});
                        self.stack[self.depth].subfunction = sf_idx;
                        for i in 0..self.stack[self.depth].pc_ranges.len() {
                            let range = self.stack[self.depth].pc_ranges[i].clone();
                            self.stack[function_depth].subfunction_events.push(SubfunctionEvent {addr: range.begin as usize, level, subfunction_idx: sf_idx, sign: 1});
                            self.stack[function_depth].subfunction_events.push(SubfunctionEvent {addr: range.end as usize, level, subfunction_idx: sf_idx, sign: -1});
                        }
                    }
                }

                // Things we (hopefully) don't care about.
                DW_TAG_label | DW_TAG_imported_declaration | DW_TAG_imported_module | DW_TAG_template_type_parameter | DW_TAG_template_value_parameter | DW_TAG_GNU_template_parameter_pack | DW_TAG_GNU_template_template_param => {
                    skip_subtree = self.depth;
                    cursor.skip_attributes(abbrev.attributes())?;
                }

                _ => cursor.skip_attributes(abbrev.attributes())?,
            }
        }
    }

    fn finish_function(&mut self) {
        let top = &mut self.stack[self.depth + 1];
        if top.subfunction == usize::MAX {
            return;
        }
        assert!(top.function != usize::MAX && !top.pc_ranges.is_empty());
        
        // Do a sweeping line (or whatever it's called) to turn arbitrary set of ranges into a well-formed tree of nested ranges,
        // and link ranges of each subfunction into a linked list.
        top.subfunction_events.sort_unstable_by_key(|e| (e.addr, e.sign, e.level as i32 * e.sign as i32));
        let mut list_heads: Vec<(/*subfunction_idx*/ usize, /*first_range_idx*/ usize)> = Vec::new();
        let mut stack: Vec<(/*subfunction_idx*/ usize, /*range_idx*/ usize, /*active*/ usize)> = Vec::new();
        let mut levels: Vec<Vec<SubfunctionPcRange>> = Vec::new();
        let mut last_closed: (usize, LineInfo) = (usize::MAX, LineInfo::invalid());
        for &SubfunctionEvent {addr, subfunction_idx, level, sign} in &top.subfunction_events {
            if addr > last_closed.0 {
                if last_closed.1.file_idx().is_some() {
                    // .debug_line often doesn't have a row at the end of inlined function. I.e. the first few instructions
                    // after the inlined function still have the same line number as the insides of the inlined function, which makes things really confusing.
                    // I'm not sure what's the intention there. I guess the debugger is supposed to realize that the instructions after an inlined function
                    // have the same line number as the inlined function's call site? Let's interpret it that way and add LineInfo, with lower priority than the
                    // LineInfo-s from .debug_line.
                    self.shard.sym.addr_to_line.push(last_closed.1.clone().with_addr_and_flags(last_closed.0, LineFlags::INLINED_FUNCTION));
                }
                last_closed.0 = usize::MAX;
            }

            let level = level as usize;
            if sign > 0 {
                if level < stack.len() {
                    if stack[level].0 == subfunction_idx {
                        stack[level].2 += 1;
                    } else {
                        if self.shard.warn.check(line!()) { eprintln!("warning: overlapping inline function ranges at address 0x{:x} @0x{:x}", addr, self.shard.sym.subfunctions[subfunction_idx].callee_idx); }
                    }
                    continue;
                }
                let start_level = stack.len();
                stack.resize(level + 1, (0, 0, 0));
                if level >= levels.len() {
                    levels.resize_with(level + 1, || Vec::new());
                }

                // Open a range at current level and for all ancestors that don't have a range open.
                stack.last_mut().unwrap().2 += 1;
                let mut cur_sf_idx = subfunction_idx;
                for cur_level in (start_level..level+1).rev() {
                    let cur_sf = &mut self.shard.sym.subfunctions[cur_sf_idx];
                    assert!(cur_level == cur_sf.level as usize);
                    let new_range_idx = levels[cur_level].len();
                    stack[cur_level].0 = cur_sf_idx;
                    stack[cur_level].1 = new_range_idx;
                    levels[cur_level].push(SubfunctionPcRange {range: addr..addr, subfunction_idx: cur_sf_idx, prev_range_idx: cur_sf.last_range_idx});
                    if cur_sf.last_range_idx == usize::MAX {
                        list_heads.push((cur_sf_idx, new_range_idx));
                    }
                    cur_sf.last_range_idx = new_range_idx;

                    if cur_level > 0 && cur_sf.call_line.file_idx().is_some() {
                        self.shard.sym.line_to_addr.push((cur_sf.call_line.clone().with_addr_and_flags(addr, LineFlags::INLINED_FUNCTION | LineFlags::STATEMENT), cur_level as u16 - 1));
                    }

                    cur_sf_idx = cur_sf.parent;
                }
            } else {
                if stack[level].0 == subfunction_idx {
                    stack[level].2 -= 1;
                }
                while let Some(&(_, range_idx, active)) = stack.last() {
                    if active != 0 {
                        break;
                    }
                    stack.pop();
                    let range = &mut levels[stack.len()][range_idx];
                    range.range.end = addr;
                    let sf = &self.shard.sym.subfunctions[range.subfunction_idx];
                    last_closed = (addr, sf.call_line.clone());
                }
            }
        }
        assert!(stack.is_empty());

        // Circularize the linked lists.
        for (subfunction_idx, first_range_idx) in list_heads {
            let sf = &mut self.shard.sym.subfunctions[subfunction_idx];
            levels[sf.level as usize][first_range_idx].prev_range_idx = sf.last_range_idx;
        }

        // Concatenate the per-level range arrays into one array.
        if self.shard.sym.subfunction_levels.last() != Some(&self.shard.sym.subfunction_pc_ranges.len()) {
            self.shard.sym.subfunction_levels.push(self.shard.sym.subfunction_pc_ranges.len());
        }
        let levels_start = self.shard.sym.subfunction_levels.len() - 1;
        for level in 0..levels.len() {
            self.shard.sym.subfunction_pc_ranges.append(&mut levels[level]);
            self.shard.sym.subfunction_levels.push(self.shard.sym.subfunction_pc_ranges.len());
        }
        let level_idxs = levels_start..self.shard.sym.subfunction_levels.len();
        assert!(level_idxs.len() == levels.len() + 1);

        self.shard.functions[top.function].subfunction_levels = level_idxs;
    }
}

fn parse_attr_str<'a>(dwarf: &'a Dwarf<SliceType>, unit: &'a Unit<SliceType>, attr: &Option<Attribute<SliceType>>) -> Result<&'a str> {
    let val = match attr {
        None => return err!(Dwarf, "no name"),
        Some(a) => a.value(),
    };
    // TODO: perf says this takes 5% of the symbols loading time, because it iterates over the null-terminated string one byte at a time. Replace with simd implementation or something. Maybe contribute it to gimli.
    let slice = dwarf.attr_string(unit, val)?;
    let s = str::from_utf8(slice.slice())?;
    Ok(s)
}

// Use this when the string will be presented to the user, so the error won't go unnoticed.
// Otherwise use parse_attr_str() and handle the error.
fn parse_attr_str_or_error_message(dwarf: &Dwarf<SliceType>, unit: &Unit<SliceType>, attr: &Option<Attribute<SliceType>>) -> &'static str {
    let val = match attr {
        None => return "[no name]",
        Some(a) => a.value(),
    };
    let slice = match dwarf.attr_string(unit, val) {
        Err(e) => return "[error in str]",
        Ok(s) => s,
    };
    match str::from_utf8(slice.slice()) {
        Err(e) => return "[not utf8]",
        Ok(s) => s,
    }
}

fn parse_attr_stride(attr: Attribute<SliceType>, offset: DieOffset, warn: &mut Limiter) -> Option<usize> {
    match attr.value().sdata_value() {
        None => if warn.check(line!()) { eprintln!("warning: arrays with dynamic stride are not supported (have dynamic {} @0x{:x})", attr.name(), offset.0); }
        Some(s) if s < 0 => if warn.check(line!()) { eprintln!("warning: arrays with negative stride are not supported (have {} = {} @0x{:x})", attr.name(), s, offset.0); }
        Some(s) if attr.name() == DW_AT_bit_stride && s % 8 != 0 => if warn.check(line!()) { eprintln!("warning: bit-packed arrays are not supported (have DW_AT_bit_stride @0x{:x})", offset.0); }
        Some(s) if attr.name() == DW_AT_bit_stride => return Some((s / 8) as usize),
        Some(s) => return Some(s as usize),
    }
    None
}

fn parse_attr_ranges(dwarf: &Dwarf<SliceType>, unit: &Unit<SliceType>, low_pc: &Option<Attribute<SliceType>>, high_pc: &Option<Attribute<SliceType>>, ranges: &Option<Attribute<SliceType>>, offset: DieOffset, code_start: usize, out: &mut Vec<gimli::Range>, warn: &mut Limiter) -> Result<()> {
    out.clear();
    // Sometimes both low_pc and ranges are present on compilation units. In this case ranges take precedence, and low_pc acts as "base address" for all range lists in the unit (which gimli presumably takes care of automatically).
    if let Some(rngs) = ranges {
        if let Some(ranges_offset) = dwarf.attr_ranges_offset(unit, rngs.value())? {
            let mut it = dwarf.ranges(unit, ranges_offset)?;
            while let Some(range) = it.next()? {
                // In practice ranges often contain garbage near-zero addresses, even in debug builds without LTO. Presumably linker relocations get lost at some point, maybe because of compiler optimizations.
                // Empty ranges are intended to be skipped, I guess, but often they're not empty. Sometimes they don't start at 0, and sometimes none of the bad ranges in the list start at 0.
                // Sometimes bad ranges coexist with good ranges in the same list of ranges.
                // Idk if that's intended or compiler bug. Let's silently ignore invalid ranges.
                // Maybe a cleaner way to detect bad ranges would be to check if the base address is zero (suggesting missing relocation) when parsing the range list data, but that's currently hidden inside gimli.
                if range.end as usize <= code_start || range.end <= range.begin {
                    continue;
                }
                out.push(range);
            }
            return Ok(());
        }
        if warn.check(line!()) { eprintln!("warning: unexpected form @0x{:x}: {:?}", offset.0, rngs); }
    } else if let &Some(low) = low_pc {
        let low_addr = match dwarf.attr_address(unit, low.value())? {
            None => return err!(Dwarf, "DW_AT_low_pc has unexpected form: {:?}", low),
            Some(a) => a,
        };
        if low_addr == 0 { // same as above, see other comment
            return Ok(());
        }
        let high_addr = if let &Some(high) = high_pc {
            let high = high.value();
            if let Some(high_addr) = dwarf.attr_address(unit, high)? {
                high_addr
            } else if let Some(diff) = high.udata_value() {
                low_addr + diff
            } else {
                if warn.check(line!()) { eprintln!("warning: unexpected form @0x{:x}: {:?}", offset.0, high); }
                return Ok(());
            }
        } else {
            low_addr + 1
        };
        if high_addr > low_addr {
            out.push(gimli::Range {begin: low_addr, end: high_addr});
        }
    }
    Ok(())
}
