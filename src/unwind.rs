use crate::{*, error::{*, Error, Result}, range_index::*, util::*, elf::*, procfs::*, registers::*, symbols::*, expr::*};
use gimli::*;
use std::{sync::Arc, rc::Rc, mem, path::PathBuf, ops::Range};

type SliceType = EndianSlice<'static, LittleEndian>;
pub type UnwindScratchBuffer = UnwindContext<usize>;

pub struct UnwindInfo {
    elf: Arc<ElfFile>,

    // .eh_frame - section with information needed for stack unwinding, e.g. where's the return address and saved registers (for different address ranges).
    eh_frame: Option<UnwindSectionIndex<EhFrame<SliceType>>>,
    // .debug_frame - approximately the same as .eh_frame. But not exactly the same! And sometimes both are present. Why do people do that.
    debug_frame: Option<UnwindSectionIndex<DebugFrame<SliceType>>>,
}

#[derive(Clone)]
pub struct StackFrame {
    pub addr: usize,
    // If this is not the top frame, `addr` is positioned at the next instructin after the call:
    //     call foo
    //  => mov eax, whatever
    // But for most purposes we're interested in the address of the call instruction itself: for looking up line numbers, unwind info, local variables, etc:
    //  => call foo
    //     mov eax, whatever
    // But calculating that address would be a bit of a hassle (we'd need to parse instructions, and it may be ambiguous).
    // Instead, we just use addr-1:
    //     call foo
    //  =>
    //     mov eax, whatever
    // That's what pseudo_addr is. It doesn't necessarily point to a valid instruction, but it should be used for most address range lookups.
    pub pseudo_addr: usize,

    pub regs: Registers,
    pub frame_base: Result<(usize, /*dubious*/ bool)>, // result of the DW_AT_frame_base expression

    // If we found a mapped binary corresponding to this address, these two are set.
    pub binary_id: Option<BinaryId>,
    pub addr_static_to_dynamic: usize,
    // If we had a loaded Symbols for that Binary, this is set. Always present if any of this frame's subframes have function_idx, subfunction_idx, or line.
    pub symbols_identity: usize,

    // Indices in StackTrace.subframes, from innermost to outermost inlined functions. Last one represents the frame. Not empty.
    pub subframes: Range<usize>,
}
impl Default for StackFrame { fn default() -> Self { Self {addr: 0, pseudo_addr: 0, regs: Registers::default(), frame_base: err!(Dwarf, "no frame base"), binary_id: None, addr_static_to_dynamic: 0, symbols_identity: 0, subframes: 0..0} } }

#[derive(Clone)]
pub struct StackSubframe {
    pub frame_idx: usize,
    pub function_idx: Result<usize>,
    pub function_name: String, // demangled, present iff function_idx is present
    pub subfunction_idx: Option<usize>,
    pub line: Option<FileLineInfo>,
}
impl Default for StackSubframe { fn default() -> Self { Self {frame_idx: 0, subfunction_idx: None, function_idx: err!(Internal, "not symbolized"), function_name: String::new(), line: None} } }

#[derive(Clone)]
pub struct StackTrace {
    pub frames: Vec<StackFrame>,
    pub subframes: Vec<StackSubframe>,
    pub truncated: Option<Error>, // if unwinding stopped early
}
impl Default for StackTrace { fn default() -> Self { StackTrace {frames: Vec::new(), subframes: Vec::new(), truncated: None} } }
impl StackTrace {
    pub fn error(e: Error) -> Self { Self {frames: Vec::new(), subframes: Vec::new(), truncated: Some(e)} }

    // Hash of (cfa, subfunction id). Used for diffing the stack trace before and after a step to decide what subframe to select, see StepState.
    pub fn subframe_identity(&self, subframe_idx: usize) -> usize {
        let subframe = &self.subframes[subframe_idx];
        let frame = &self.frames[subframe.frame_idx];
        let cfa = match frame.regs.get_int(RegisterIdx::Cfa) {
            Ok((c, _)) => c,
            Err(_) => return 0 };
        let frame_identity = cfa as usize ^ frame.symbols_identity;
        if let Some(i) = subframe.subfunction_idx.as_ref() {
            hash(&(frame_identity, *i, 0usize))
        } else if let Ok(i) = subframe.function_idx.as_ref() {
            hash(&(frame_identity, *i, 1usize))
        } else {
            hash(&(frame_identity, 0usize, 2usize))
        }
    }
}

#[derive(Clone)]
pub struct FileLineInfo {
    pub line: LineInfo,
    pub filename: PathBuf,
    pub path: PathBuf,
    pub version: FileVersionInfo,
}


struct UnwindSectionIndex<S: UnwindSection<SliceType>> {
    section: S,
    // CIEs in the section (you don't have to know what "CIE" means, it's just part of deserializing the stack unwinding information).
    cies: Vec<CommonInformationEntry<SliceType>>,
    // Static address -> offset of FDE in the section (FDE is a unit of information in .eh_frame or .debug_frame, corresponding to a range of addresses).
    static_addr_to_fde: RangeIndex<usize>,
    static_base_addresses: BaseAddresses,
}

impl<S: UnwindSection<SliceType>> UnwindSectionIndex<S> {
    fn load(data: &'static [u8], static_base_addresses: BaseAddresses, section: S) -> Result<Self> {
        let mut cies: Vec<CommonInformationEntry<SliceType>> = Vec::new();
        let mut fde_ranges: Vec<RangeIndexEntry<usize>> = Vec::new();

        let mut entries_iter = section.entries(&static_base_addresses);
        while let Some(entry) = entries_iter.next()? {
            match entry {
                CieOrFde::Cie(cie) => cies.push(cie),
                CieOrFde::Fde(fde) => {
                    let fde = fde.parse(|_, _, offset| find_cie_in(UnwindOffset::into(offset) as u64, &cies))?;
                    fde_ranges.push(RangeIndexEntry {start: fde.initial_address() as usize, end: (fde.initial_address() + fde.len()) as usize, max_end: 0, value: fde.offset()});
                }
            }
        }

        Ok(Self {section, static_base_addresses, cies, static_addr_to_fde: RangeIndex::new(fde_ranges, "unwind")})
    }

    fn find_fde_and_row<'a>(&self, pseudo_addr: usize, addr_map: &AddrMap, scratch: &'a mut UnwindScratchBuffer) -> Result<Option<(FrameDescriptionEntry<SliceType>, &'a UnwindTableRow<usize>, /*section*/ SliceType)>> {
        let static_pseudo_addr = addr_map.dynamic_to_static(pseudo_addr);
        let fde_offset = match self.static_addr_to_fde.find(static_pseudo_addr) {
            None => return Ok(None),
            Some(o) => o.value,
        };
        let fde = self.section.fde_from_offset(&self.static_base_addresses, fde_offset.into(), |_, _, offset| find_cie_in(UnwindOffset::into(offset) as u64, &self.cies))?;
        let row = match fde.unwind_info_for_address(&self.section, &self.static_base_addresses, scratch, static_pseudo_addr as u64) {
            Ok(r) => r,
            Err(e) if e == gimli::read::Error::NoUnwindInfoForAddress => return Ok(None),
            Err(e) => return Err(e.into()),
        };
        Ok(Some((fde, row, self.section.section().clone())))
    }
}

fn find_cie_in(offset: u64, cies: &Vec<CommonInformationEntry<SliceType>>) -> gimli::read::Result<CommonInformationEntry<SliceType>> {
    let idx = cies.partition_point(|cie| cie.offset() < offset as usize);
    if idx < cies.len() && cies[idx].offset() == offset as usize {
        Ok(cies[idx].clone())
    } else {
        Err(gimli::read::Error::NoEntryAtGivenOffset)
    }
}

impl UnwindInfo {
    pub fn load(elf: Arc<ElfFile>) -> Result<Self> {
        // Section data and its static address.
        let load_section = |name| -> Result<(&'static [u8], u64)> {
            let section = match elf.section_by_name.get(name) {
                None => return err!(NoSection, "no section {}", name),
                Some(idx) => &elf.sections[*idx],
            };
            let data = &elf.data()[section.offset..section.offset+section.size];
            let addr = section.address;
            Ok((unsafe { mem::transmute(data) }, addr as u64))
        };

        // Addresses in eh_frame may be encoded relative to these sections.
        // At this point we only know their static addresses (from elf), not dynamic ones (from runtime memory maps); we assume that:
        //  1. The relative addresses of these sections are preserved when the binary is loaded. I.e. dynamic address = static address + const.
        //  2. CIEs don't use BaseAddresses for anything we care about. So we can parse them now once rather than on every stack walk.
        let (_text_data, text_addr) = load_section(".text")?;
        let mut static_base_addresses = BaseAddresses::default().set_text(text_addr);
        match load_section(".got") {
            Ok((_, addr)) => static_base_addresses = static_base_addresses.set_got(addr),
            Err(e) if e.is_no_section() => (),
            Err(e) => return Err(e), // if .got is present in ELF but not mapped at runtime, that's unexpected
        };

        let mut res = Self {elf: elf.clone(), eh_frame: None, debug_frame: None};

        match load_section(".eh_frame") {
            Ok((data, addr)) => res.eh_frame = Some(UnwindSectionIndex::load(data, static_base_addresses.clone().set_eh_frame(addr), EhFrame::new(data, LittleEndian))?),
            Err(e) if e.is_no_section() => (),
            Err(e) => return Err(e),
        }
        match load_section(".debug_frame") {
            Ok((data, addr)) => res.debug_frame = Some(UnwindSectionIndex::load(data, static_base_addresses, DebugFrame::new(data, LittleEndian))?),
            Err(e) if e.is_no_section() => (),
            Err(e) => return Err(e),
        }
        if res.eh_frame.is_none() && res.debug_frame.is_none() {
            return err!(MissingSymbols, "no .eh_frame or .debug_frame section");
        }

        Ok(res)
    }

    pub fn find_row_and_eval_cfa<'a>(&self, memory: &MemReader, addr_map: &AddrMap, scratch: &'a mut UnwindScratchBuffer, pseudo_addr: usize, regs: &Registers) -> Result<(FrameDescriptionEntry<SliceType>, &'a UnwindTableRow<usize>, /*cfa*/ usize, /*cfa_dubious*/ bool, /*section*/ SliceType)> {
        let found = match &self.debug_frame {
            Some(section) => section.find_fde_and_row(pseudo_addr, addr_map, scratch)?,
            None => None,
        };
        let found = match found {
            Some(x) => Some(x),
            None => match &self.eh_frame {
                Some(section) => section.find_fde_and_row(pseudo_addr, addr_map, scratch)?,
                None => None,
            }
        };
        let (fde, row, section) = match found {
            Some(x) => x,
            None => return err!(Dwarf, "no unwind info for address"),
        };
        let (cfa, cfa_dubious) = eval_cfa_rule(row.cfa(), fde.cie().encoding(), section, &regs, memory, addr_map)?;

        let row: &'static UnwindTableRow<usize> = unsafe {mem::transmute(row)}; // work around borrow checker weirdness by transmuting to 'static and back
        Ok((fde, row, cfa as usize, cfa_dubious, section))
    }

    // Assigns cfa and return address in current frame and returns registers for next frame.
    pub fn step(&self, memory: &MemReader, addr_map: &AddrMap, scratch: &mut UnwindScratchBuffer, pseudo_addr: usize, regs: &mut Registers) -> Result<(Registers, /*is_signal_trampoline*/ bool)> {
        let (fde, row, cfa, cfa_dubious, section) = self.find_row_and_eval_cfa(memory, addr_map, scratch, pseudo_addr, regs)?;
        regs.set_int(RegisterIdx::Cfa, cfa as u64, cfa_dubious);

        let mut new_regs = Registers::default();
        let mut seen_regs = 0usize; // to distinguish registers that were explicitly set to RegisterRule::Undefined from ones that weren't specified
        let mut found_return_address = false;
        for (reg_num, rule) in row.registers() {
            let reg = RegisterIdx::from_dwarf(*reg_num);
            if let &Some(r) = &reg {
                seen_regs |= 1usize << r as u32;
            }

            let val = eval_register_rule(reg.clone(), rule, fde.cie().encoding(), section, &regs, memory, addr_map)?;
            if let Some((v, dubious)) = val {
                if let Some(r) = reg {
                    new_regs.set_int(r, v, dubious);
                }
                if *reg_num == fde.cie().return_address_register() {
                    regs.set_int(RegisterIdx::Ret, v, dubious);
                    found_return_address = true;
                }
            }
        }

        // For callee-saved registers that don't have recovery rules in CIE or FDE, keep the same value.
        // I couldn't figure out whether this is supposed to be correct or not. Full story:
        //
        // Typically, the CIE and FDE would specify recovery rules for only a few of the registers. Often just one: RIP.
        // But CFA/register recovery rules often *read* such registers that didn't have recovery rules in higher stack frames.
        // E.g. it's typical that the CFA rule is [RBP + n], but the previous frame doesn't have recovery rule for RBP.
        // What's the intended way to evaluate such rules? I couldn't fully figure it out.
        //  * Perhaps registers without recovery rule should default to RegisterRule::SameValue? That can't be fully right because
        //    e.g. RAX usually doesn't have recovery rule even if it's clearly clobbered by earlier code.
        //    But that's what libunwind appears to do, see setup_fde().
        //  * Perhaps these registers should default to RegisterRule::Undefined, like gimli does? That can't be fully right because of
        //    the situation described above - subsequent CFA rules often need such register values.
        //    But that's what gimli does. That's why we're here writing this stupid comment instead of doing something productive.
        //  * Perhaps there are some unwritten (or at least unfound by me) ABI-specific default rules for each register?
        //    (There's at least one such rule: RSP = CFA, but that seems special and maybe not even ABI-specific.)
        //    Maybe callee-saved registers are meant to default to SameValue, and caller-saved should default to Undefined?
        //    I.e. maybe the compiler starts emitting register rules for callee-saved registers only after the function actually overwrites them? Idk.
        //  * Maybe debuggers do some heroic guesswork to infer the saved register locations.
        //    In GDB there's code that detects standard function prelude (push rbp; mov rbp, rsp). I didn't figure out what it's used for.
        //    Maybe it's to know when/where the callee-preserved registers are saved?
        //    Maybe debuggers do some code analysis: look for `push` instructions for callee-saved registers near the start of the function, then assume that
        //    these pushed values are never modified and can be used to retrieve the saved registers? Idk. I sure hope this is not how it works.
        //
        // For now, for callee-saved registers, we just treat Undefined as SameValue, but mark the value as dubious (which is shown in UI).
        //
        // (Except for RBP - don't mark it as dubious. I saw omitted RBP recovery rule for the first instruction of a function,
        //  before the 'push rbp' happens, where clealy RegisterRule::SameValue behavior is expected. So I guess this is
        //  the general convention at least for this register.)
        for reg in [RegisterIdx::Rbp, RegisterIdx::Rbx, RegisterIdx::R12, RegisterIdx::R13, RegisterIdx::R14, RegisterIdx::R15] {
            if seen_regs & 1usize << reg as u32 == 0 && regs.has(reg) {
                let dubious = reg != RegisterIdx::Rbp;
                new_regs.set_int(reg, regs.get_int(reg).unwrap().0, dubious);
            }
        }
        if seen_regs & 1usize << RegisterIdx::Rsp as u32 == 0 {
            new_regs.set_int(RegisterIdx::Rsp, cfa as u64, false);
        }

        if !found_return_address {
            // According to libunwind: "Leaf function keeps the return address in register and there is no explicit intructions how to restore it"
            let reg = match RegisterIdx::from_dwarf(fde.cie().return_address_register()) {
                None => return err!(Dwarf, "unrecognized return address register: {:?}", fde.cie().return_address_register()),
                Some(r) => r };
            // Typically the return address register is just RIP, and for the root stack frame there's no RIP in new_regs.
            if new_regs.has(reg) {
                let (v, dubious) = new_regs.get_int(reg).unwrap();
                new_regs.set_int(RegisterIdx::Ret, v, dubious);
            }
        }

        Ok((new_regs, fde.is_signal_trampoline()))
    }
}

fn eval_dwarf_expression_as_u64(expression: Expression<SliceType>, encoding: Encoding, regs: &Registers, memory: &MemReader, addr_map: &AddrMap, skip_final_dereference: bool) -> Result<(u64, /* dubious */ bool)> {
    let (val, dubious) = eval_dwarf_expression(expression, &DwarfEvalContext {encoding, memory, symbols: None, unit: None, addr_map, regs: Some(regs), frame_base: &err!(Dwarf, "no frame base"), local_variables: &[]})?;
    let val = if skip_final_dereference {
        // I'm probably misunderstanding something, but the meaning of gimli::read::Location::Address seems to be different between CFA/frame-base vs registers/variables.
        // In CFA and frame base, we treat Location::Address the same as Location::Value, presumably because the "value" of CFA/frame-base is an "address", so no dereference is needed.
        // In registers and variables, we treat Location::Address as the address of the register/variable value, i.e. we dereference it.
        // Otherwise things don't work.
        match val {
            AddrOrValueBlob::Addr(a) => a as u64,
            AddrOrValueBlob::Blob(v) => v.get_usize()? as u64,
        }
    } else {
        val.into_value(8, memory)?.get_usize()? as u64
    };
    Ok((val, dubious))
}

fn eval_cfa_rule(rule: &CfaRule<usize>, encoding: Encoding, section: SliceType, registers: &Registers, memory: &MemReader, addr_map: &AddrMap) -> Result<(u64, bool)> {
    Ok(match rule {
        CfaRule::Expression(e) => {
            let e = Expression(EndianSlice::new(&section.slice()[e.offset..e.offset+e.length], LittleEndian));
            eval_dwarf_expression_as_u64(e, encoding, registers, memory, addr_map, /*skip_final_dereference*/ true)?
        }
        CfaRule::RegisterAndOffset {register: reg, offset} => {
            let reg = match RegisterIdx::from_dwarf(*reg) {
                None => return err!(Dwarf, "unsupported register in cfa: {:?}", reg),
                Some(r) => r,
            };
            if !registers.has(reg) {
                return err!(Dwarf, "register {} optimized away (in cfa)", reg);
            }
            let (r, dub) = registers.get_int(reg).unwrap();
            (r.wrapping_add(*offset as u64), dub)
        }
    })
}

fn eval_register_rule(reg: Option<RegisterIdx>, rule: &RegisterRule<usize>, encoding: Encoding, section: SliceType, registers: &Registers, memory: &MemReader, addr_map: &AddrMap) -> Result<Option<(u64, bool)>> {
    let (cfa, cfa_dubious) = registers.get_int(RegisterIdx::Cfa).unwrap();
    let val = match rule {
        RegisterRule::Undefined => return Ok(None),
        RegisterRule::SameValue => match &reg {
            None => return err!(Dwarf, "got same-value rule for unrecognized register"),
            Some(reg) => match registers.get_int(*reg) {
                Err(_) => return err!(Dwarf, "register {} optimized away (in reg)", reg),
                Ok(x) => x,
            }
        }
        RegisterRule::Register(reg) => {
            let reg = match RegisterIdx::from_dwarf(*reg) {
                None => return err!(NotImplemented, "unsupported register (in reg): {:?}", reg),
                Some(r) => r,
            };
            match registers.get_int(reg) {
                Err(_) => return err!(Dwarf, "register {} optimized away (in other reg)", reg),
                Ok(x) => x,
            }
        }
        RegisterRule::Offset(offset) => (memory.read_u64(cfa.wrapping_add(*offset as u64) as usize)?, cfa_dubious),
        RegisterRule::ValOffset(offset) => (cfa.wrapping_add(*offset as u64), cfa_dubious),
        RegisterRule::Expression(e) => {
            let e = Expression(EndianSlice::new(&section.slice()[e.offset..e.offset+e.length], LittleEndian));
            let (addr, dubious) = eval_dwarf_expression_as_u64(e, encoding, registers, memory, addr_map, /*skip_final_dereference*/ true)?;
            (memory.read_u64(addr as usize)?, dubious)
        }
        RegisterRule::ValExpression(e) => {
            let e = Expression(EndianSlice::new(&section.slice()[e.offset..e.offset+e.length], LittleEndian));
            eval_dwarf_expression_as_u64(e, encoding, registers, memory, addr_map, /*skip_final_dereference*/ false)?
        }
        RegisterRule::Architectural => return err!(Dwarf, "architectural register rule"),

        RegisterRule::Constant(c) => (*c, false),

        _ => return err!(Dwarf, "unsupported type of register rule: {:?}", rule),
    };
    Ok(Some(val))
}
