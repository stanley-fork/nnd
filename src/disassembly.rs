use crate::{*, symbols::*, error::*, symbols_registry::*, util::*, procfs::*, settings::*, common_ui::*};
use std::{fmt::Write, ops::Range};
use iced_x86::*;

pub const MAX_X86_INSTRUCTION_BYTES: usize = 15;

// Disassembled function - a sequence of lines.
// For each line, a prefix is formatted by the UI on the fly (with some dynamic elements), a suffix is pre-formatted (in `text`).
//
// Example:
//  text                                                        DisassemblyLineKind       leaf_line   subfunction_idx
//
//  /usr/lib/x86_64-linux-gnu/libc.so.6                         Intro
//  dwarf offset: 0x1bee14                                      Intro
//                                                              Intro
//                         futex-internal.c:138:1               LeafLineNumber            138
//  7ffff7c91030 <  +0>    endbr64                              Instruction               138
//  7ffff7c91034 <  +4>    push r13                             Instruction               138
//  7ffff7c91036 <  +6>    mov r13d,esi                         Instruction               138
//                         futex-internal.c:138:1               LeafLineNumber            138
//  7ffff7c91039 <  +9>    push r12                             Instruction               138
//  7ffff7c9103b <  +b>    push rbx                             Instruction               138
//  7ffff7c9103c <  +c>    sub rsp,20h                          Instruction               138
//                         futex-internal.c:139:10              InlinedCallLineNumber     139         
//                         __futex_abstimed_wait_common         InlinedFunctionName       139
//                         ┆futex-internal.c:77:6               LeafLineNumber            77          42 (or whatever the idx of this inlined __futex_abstimed_wait_common is)
//  7ffff7c91040 < +10>    ┆test rcx,rcx                        Instruction               77          42
//  7ffff7c91043 < +13>  ↓ ┆jne near _+100h                     Instruction               77          42
//                         ┆futex-internal.c:80:6               LeafLineNumber            80          42
//  7ffff7c91049 < +19>    ┆cmp edx,1                           Instruction               80          42

pub struct Disassembly {
    pub text: StyledText,
    pub lines: Vec<DisassemblyLineInfo>, // parallel to `text` lines
    pub error: Option<Error>, // also baked into `lines`

    pub max_abs_relative_addr: usize,
    pub indent_width: usize,
    pub widest_line: usize,

    // Don't want to hold an Arc<Symbols> here, we look it up in SymbolsRegistry every frame. This shard_idx is just to assert that we found the correct one (so `subfunction` indices will match).
    pub symbols_shard: Option<usize>,
}

// Information about one line of text in the disassembly listing.
pub struct DisassemblyLineInfo {
    pub kind: DisassemblyLineKind,

    //  7ffff7c91043 < +13>  ↓ ┆jne near _+100h
    //  ^^^^^^^^^^^^   ^^^   ^  ^^^^^^^^^^^^^^^
    // static_addr     |     | |     `text`
    //     relative_addr     | |
    //          jump_indicator |
    //         subfunction_level

    // Address of the current or next instruction. For Intro: 0. For Error: usize::MAX. Binary-searchable. Should be rendered only for DisassemblyLineKind::Instruction.
    pub static_addr: usize,

    pub relative_addr: isize,
    pub jump_indicator: char,
    pub jump_target: Option<usize>,

    // Line number for the current or previous LeafLineNumber or InlinedCallLineNumber.
    pub leaf_line: Option<LineInfo>,
    // Innermost inlined function containing this line. For InlinedCallLineNumber and InlinedCallFunctionName: the *parent* subfunction (if any). Level always >= 1.
    pub subfunction: Option<usize>,
    pub subfunction_level: u16, // `subfunction` level, 0 if None; indentation level
}
impl Default for DisassemblyLineInfo { fn default() -> Self { Self {kind: DisassemblyLineKind::Error, static_addr: 0, relative_addr: 0, jump_indicator: ' ', jump_target: None, subfunction_level: 0, leaf_line: None, subfunction: None} } }

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum DisassemblyLineKind {
    Intro,
    InlinedCallLineNumber,
    InlinedFunctionName,
    LeafLineNumber,
    Instruction,
    Separator,
    Error,
}

impl Disassembly {
    pub fn new() -> Self { Self {text: StyledText::default(), lines: Vec::new(), error: None, max_abs_relative_addr: 0, indent_width: 1, widest_line: 0, symbols_shard: None} }

    pub fn static_addr_to_line(&self, static_addr: usize) -> Option<usize> {
        let idx = self.lines.partition_point(|l| l.static_addr <= static_addr);
        if idx > 0 && self.lines[idx-1].static_addr == static_addr && self.lines[idx-1].kind == DisassemblyLineKind::Instruction {
            Some(idx-1)
        } else {
            None
        }
    }

    // If pseudo_addr is in between instructions, round down to the previous instruction.
    pub fn static_pseudo_addr_to_line(&self, static_pseudo_addr: usize) -> Option<usize> {
        let idx = self.lines.partition_point(|l| l.static_addr <= static_pseudo_addr);
        if idx == 0 { return None; }
        let l = &self.lines[idx-1];
        // (Would be better to compare to function addr ranges instead of the MAX_X86_INSTRUCTION_BYTES guesswork.)
        if l.kind == DisassemblyLineKind::Instruction && l.static_addr + MAX_X86_INSTRUCTION_BYTES > static_pseudo_addr {
            Some(idx-1)
        } else {
            None
        }
    }

    pub fn with_error(mut self, e: Error, palette: &Palette) -> Self {
        assert!(self.error.is_none());
        styled_writeln!(self.text, palette.error, "{}", e);
        self.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Error, static_addr: usize::MAX, ..Default::default()});
        self.error = Some(e);
        self.finish()
    }

    pub fn finish(mut self) -> Self {
        assert_eq!(self.text.num_lines(), self.lines.len());
        for i in 0..self.lines.len() {
            self.widest_line = self.widest_line.max(self.lines[i].subfunction_level as usize * self.indent_width + str_width(self.text.get_line_str(i)));
            self.max_abs_relative_addr = self.max_abs_relative_addr.max(self.lines[i].relative_addr.abs() as usize);
        }
        self
    }
}

struct StyledFormatter<'a> {
    palette: &'a Palette,
    text: &'a mut StyledText,
}

impl<'a> FormatterOutput for StyledFormatter<'a> {
    fn write(&mut self, text: &str, kind: FormatterTextKind) {
        use FormatterTextKind::*;
        let s = match kind {
            Directive | Keyword => self.palette.disas_keyword,
            Prefix | Mnemonic => self.palette.disas_mnemonic,
            Register => self.palette.disas_register,
            Number => self.palette.disas_number,
            Function => self.palette.disas_function,
            _ => self.palette.disas_default,
        };

        self.text.chars.push_str(text);
        self.text.close_span(s);
    }
}

struct Resolver<'a> {
    symbols: Option<&'a Symbols>,
    current_function: core::ops::Range<usize>,
}

impl<'a> SymbolResolver for Resolver<'a> {
    fn symbol(&mut self, _: &Instruction, _operand: u32, _instruction_operand: Option<u32>, static_addr: u64, _address_size: u32) -> Option<SymbolResult<'a>> {
        let static_addr = static_addr as usize;
        if self.current_function.contains(&static_addr) {
            // Make jumps inside current function easier to read: "_+42h" instead of "__futex_abstimed_wait_cancelable64+42h".
            return Some(SymbolResult {address: self.current_function.start as u64, text: SymResTextInfo::new("_", FormatterTextKind::Function), flags: SymbolFlags::NONE, symbol_size: Some(MemorySize::UInt64)});
        }

        if let Some(symbols) = &self.symbols {
            if let Ok((f, _)) = symbols.addr_to_function(static_addr) {
                let name = f.demangle_name();
                let text = SymResString::String(name);
                return Some(SymbolResult {address: f.addr.addr().unwrap() as u64, text: SymResTextInfo::Text(SymResTextPart {text, color: FormatterTextKind::Function}), flags: SymbolFlags::NONE, symbol_size: Some(MemorySize::UInt64)});
            }
        }
        None
    }
}

pub fn disassemble_function(function_idx: usize, mut static_addr_ranges: Vec<Range<usize>>, symbols: Option<&Symbols>, code: Option<&[u8]>, intro: StyledText, palette: &Palette) -> Disassembly {
    clean_up_ranges(&mut static_addr_ranges);

    let mut res = Disassembly {text: intro, lines: Vec::new(), error: None, max_abs_relative_addr: 0, indent_width: str_width(&palette.tree_indent.0), widest_line: 0, symbols_shard: None};
    let mut subfunc_ranges: Vec<&[SubfunctionPcRange]> = Vec::new();

    while res.lines.len() < res.text.num_lines() {
        res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Intro, static_addr: 0, ..Default::default()});
    }

    if let Some(symbols) = &symbols {
        let function = &symbols.functions[function_idx];
        res.symbols_shard = Some(function.shard_idx());
        subfunc_ranges = (1..function.num_levels()).map(|i| symbols.subfunction_ranges_at_level(i, function)).collect();
    }

    for (addr_range_idx, static_addr_range) in static_addr_ranges.iter().enumerate() {
        if static_addr_range.len() > 100_000_000 {
            return res.with_error(error!(Sanity, "{} MB to disassemble, suspiciously much", static_addr_range.len() / 1_000_000), palette);
        }

        let code: &[u8] = if let Some(code) = code.clone() {
            assert_eq!(static_addr_ranges.len(), 1);
            code
        } else if let Some(symbols) = &symbols {
            // Read the machine code from file rather than memory so that it doesn't show our breakpoint instructions.
            match symbols.elf.addr_range_to_offset_range(static_addr_range.start, static_addr_range.end) {
                None => return res.with_error(error!(Dwarf, "function address range out of bounds of executable: {:x}-{:x}", static_addr_range.start, static_addr_range.end), palette),
                Some((start, end)) => &symbols.elf.data()[start..end],
            }
        } else {
            panic!("huh");
        };

        if addr_range_idx != 0 {
            styled_write!(res.text, palette.disas_default,  "──────────");
            res.text.close_line();
            res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Separator, static_addr: static_addr_range.start, ..Default::default()});
        }

        let resolver = Resolver {symbols: symbols.clone(), current_function: static_addr_range.clone()};
        // NasmFormatter wants to own the symbol resolver for some reason. (Probably it would be too inconvenient or inefficient to have lifetime argument all throughout the formatter implementation.)
        // We trust that the SymbolResolver reference isn't retained after the formatter is destroyed, so it should be ok to fudge the lifetime here.
        let resolver: Resolver<'static> = unsafe { std::mem::transmute(resolver) };

        let mut decoder = Decoder::with_ip(64, code, static_addr_range.start as u64, DecoderOptions::NONE);
        let mut formatter = NasmFormatter::with_options(Some(Box::new(resolver)), None);

        let mut line_iter = if let Some(symbols) = &symbols {
            let mut line_iter = symbols.addr_to_line_iter(static_addr_range.start).peekable();
            line_iter.next_if(|line| line.addr() < static_addr_range.start);
            Some(line_iter)
        } else {
            None
        };

        let mut instruction = Instruction::default();
        let mut prev_static_addr = 0usize;
        let mut cur_leaf_line: Option<LineInfo> = None;

        while decoder.can_decode() {
            decoder.decode_out(&mut instruction);

            if let Some(l) = res.lines.last() {
                assert!(instruction.ip() as usize >= l.static_addr); // can be == because the "-----[...]" separator above is assigned to address of the first instruction after it
            }
            let static_addr = instruction.ip() as usize;
            let mut subfunction_level = 0u16;
            let mut cur_subfunction: Option<usize> = None;

            if let &Some(symbols) = &symbols {
                let write_line_number = |line: LineInfo, kind: DisassemblyLineKind, res: &mut Disassembly, subfunction_level: u16, leaf_line: &mut Option<LineInfo>, subfunction: Option<usize>| {
                    let file = match line.file_idx() {
                        None => return,
                        Some(f) => f };
                    *leaf_line = Some(line.clone());
                    let file = &symbols.files[file];
                    let name = file.filename.as_os_str().to_string_lossy();
                    styled_write!(res.text, palette.disas_filename, "{}", name);
                    if line.line() != 0 {
                        styled_write!(res.text, palette.line_number, ":{}", line.line());
                        if line.column() != 0 {
                            styled_write!(res.text, palette.column_number, ":{}", line.column());
                        }
                    }
                    res.text.close_line();
                    res.lines.push(DisassemblyLineInfo {kind, static_addr, subfunction_level, leaf_line: leaf_line.clone(), subfunction, ..Default::default()});
                };

                // Add inlined function calls information: function names, call line numbers, and indentation.
                let function = &symbols.functions[function_idx];
                for r in subfunc_ranges.iter_mut() {
                    while !r.is_empty() && r[0].range.end <= static_addr {
                        *r = &r[1..];
                    }
                    if r.is_empty() || r[0].range.start > static_addr {
                        break;
                    }
                    cur_subfunction = Some(r[0].subfunction_idx);
                    let subfunction = &symbols.shards[function.shard_idx()].subfunctions[r[0].subfunction_idx];
                    subfunction_level += 1;
                    assert!(subfunction.level == subfunction_level);
                    if r[0].range.start > prev_static_addr {
                        let callee_name = if subfunction.callee_idx == usize::MAX {
                            "?".to_string()
                        } else {
                            symbols.functions[subfunction.callee_idx].demangle_name()
                        };

                        write_line_number(subfunction.call_line.clone(), DisassemblyLineKind::InlinedCallLineNumber, &mut res, subfunction_level, &mut cur_leaf_line, cur_subfunction.clone());

                        styled_write!(res.text, palette.default_dim, "{}", callee_name);
                        res.text.close_line();
                        res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::InlinedFunctionName, static_addr, subfunction_level, leaf_line: cur_leaf_line.clone(), subfunction: cur_subfunction.clone(), ..Default::default()});
                    }
                }

                // Add line number information.
                while let Some(line) = line_iter.as_mut().unwrap().next_if(|line| line.addr() <= static_addr) {
                    write_line_number(line, DisassemblyLineKind::LeafLineNumber, &mut res, subfunction_level, &mut cur_leaf_line, cur_subfunction.clone());
                }
            }

            prev_static_addr = static_addr;

            let mut jump_target: Option<usize> = None;
            let jump_indicator = match instruction.flow_control() {
                FlowControl::Next => ' ',
                FlowControl::Return => '←',
                FlowControl::Call | FlowControl::IndirectCall => '→',
                FlowControl::Interrupt | FlowControl::XbeginXabortXend | FlowControl::Exception => '!',
                FlowControl::UnconditionalBranch | FlowControl::IndirectBranch | FlowControl::ConditionalBranch => {
                    let target_known = instruction.flow_control() != FlowControl::IndirectBranch && match instruction.op0_kind() {
                        iced_x86::OpKind::NearBranch16 | iced_x86::OpKind::NearBranch32 | iced_x86::OpKind::NearBranch64 => true,
                        _ => false };
                    if !target_known {
                        '↕'
                    } else {
                        jump_target = Some(instruction.near_branch_target() as usize);
                        if instruction.near_branch_target() > instruction.ip() {
                            '↓'
                        } else {
                            '↑'
                        }
                    }
                }
            };

            // Finally write the actual asm instruction.
            formatter.format(&instruction, &mut StyledFormatter {palette, text: &mut res.text});

            res.text.close_line();
            res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Instruction, static_addr, relative_addr: static_addr as isize - static_addr_range.start as isize, subfunction_level, jump_indicator, jump_target, leaf_line: cur_leaf_line.clone(), subfunction: cur_subfunction.clone()});
        }
    }
    res.finish()
}

fn clean_up_ranges(ranges: &mut Vec<Range<usize>>) {
    if ranges.is_empty() {
        return;
    }
    ranges.sort_unstable_by_key(|r| r.start);
    let mut j = 0;
    for i in 1..ranges.len() {
        if ranges[i].start > ranges[j].end {
            // Normal case.
            j += 1;
            ranges[j] = ranges[i].clone();
        } else if ranges[i].end <= ranges[j].end {
            // This range is contained in another. Discard.
        } else {
            // This range overlaps another. Extend.
            ranges[j].end = ranges[i].end;
        }
    }
    ranges.truncate(j+1);
}
