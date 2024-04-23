use crate::{*, symbols::*, error::*, symbols_registry::*, util::*, procfs::*, settings::*};
use std::{fmt::Write, ops::Range};
use tui::{text::{Span, Spans, Text}, style::{Style, Color, Modifier}};
use iced_x86::*;

pub const MAX_X86_INSTRUCTION_BYTES: usize = 15;

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
//                         ⸽futex-internal.c:77:6               LeafLineNumber            77          42 (or whatever the idx of this inlined __futex_abstimed_wait_common is)
//  7ffff7c91040 < +10>    ⸽test rcx,rcx                        Instruction               77          42
//  7ffff7c91043 < +13>  ↓ ⸽jne near _+100h                     Instruction               77          42
//                         ⸽futex-internal.c:80:6               LeafLineNumber            80          42
//  7ffff7c91049 < +19>    ⸽cmp edx,1                           Instruction               80          42

pub struct Disassembly {
    pub text: StyledText,
    pub lines: Vec<DisassemblyLineInfo>,
    pub error: Option<Error>, // also baked into `text`
    pub longest_line: usize,

    pub symbols_shard: Option<(*const Symbols, usize)>, // for an assert
}

// Information about one line of text in the disassembly listing.
pub struct DisassemblyLineInfo {
    pub kind: DisassemblyLineKind,
    // Address of the current or next instruction. For Intro: 0. For Error: usize::MAX.
    pub addr: usize,
    // Line number for the current or previous LeafLineNumber or InlinedCallLineNumber.
    pub leaf_line: Option<LineInfo>,
    // Innermost subfunction containing this line, consistent with indentation. For InlinedCallLineNumber and InlinedCallFunctionName: the *parent* subfunction (if any).
    pub subfunction: Option<usize>,
    // Which of the spans of this line contains indentation characters ("⸽⸽⸽...") and nothing else. Used for highlighting selected subfunction level.
    pub indent_span_idx: usize,
}

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
    pub fn new() -> Self { Self {text: StyledText::new(), lines: Vec::new(), error: None, longest_line: 0, symbols_shard: None} }
    
    pub fn addr_to_line(&self, addr: usize) -> Option<usize> {
        let idx = self.lines.partition_point(|l| l.addr <= addr);
        if idx > 0 && self.lines[idx-1].addr == addr && self.lines[idx-1].kind == DisassemblyLineKind::Instruction {
            Some(idx-1)
        } else {
            None
        }
    }

    // If pseudo_addr is in between instructions, round down to the previous instruction.
    pub fn pseudo_addr_to_line(&self, pseudo_addr: usize) -> Option<usize> {
        let idx = self.lines.partition_point(|l| l.addr <= pseudo_addr);
        if idx == 0 { return None; }
        let l = &self.lines[idx-1];
        // (Would be better to compare to function addr ranges instead of the MAX_X86_INSTRUCTION_BYTES guesswork.)
        if l.kind == DisassemblyLineKind::Instruction && l.addr + MAX_X86_INSTRUCTION_BYTES > pseudo_addr {
            Some(idx-1)
        } else {
            None
        }
    }

    pub fn with_error(mut self, e: Error, palette: &Palette) -> Self {
        assert!(self.error.is_none());
        styled_write!(self.text, palette.error, "{}", e);
        self.text.close_line();
        self.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Error, addr: usize::MAX, leaf_line: None, subfunction: None, indent_span_idx: 0});
        self.error = Some(e);
        self.finish()
    }
    
    pub fn finish(mut self) -> Self {
        assert_eq!(self.text.num_lines(), self.lines.len());
        self.longest_line = self.text.longest_line();
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
    addr_to_static: usize,
    current_function: core::ops::Range<usize>,
}

impl<'a> SymbolResolver for Resolver<'a> {
    fn symbol(&mut self, _: &Instruction, _operand: u32, _instruction_operand: Option<u32>, addr: u64, _address_size: u32) -> Option<SymbolResult<'a>> {
        let addr = addr as usize;
        if self.current_function.contains(&addr) {
            // Make jumps inside current function easier to read: "_+42h" instead of "__futex_abstimed_wait_cancelable64+42h".
            return Some(SymbolResult {address: self.current_function.start as u64, text: SymResTextInfo::new("_", FormatterTextKind::Function), flags: SymbolFlags::NONE, symbol_size: Some(MemorySize::UInt64)});
        }

        if let Some(symbols) = &self.symbols {
            let static_addr = addr.wrapping_add(self.addr_to_static);
            if let Ok((f, _)) = symbols.addr_to_function(static_addr) {
                let name = f.demangle_name();
                let text = SymResString::String(name);
                return Some(SymbolResult {address: f.addr.addr().unwrap().wrapping_sub(self.addr_to_static) as u64, text: SymResTextInfo::Text(SymResTextPart {text, color: FormatterTextKind::Function}), flags: SymbolFlags::NONE, symbol_size: Some(MemorySize::UInt64)});
            }
        }
        None
    }
}

pub fn disassemble_function(function_idx: usize, mut static_addr_ranges: Vec<Range<usize>>, symbols: Option<&Symbols>, addr_map: &AddrMap, memory: &MemReader, intro: StyledText, palette: &Palette) -> Disassembly {
    clean_up_ranges(&mut static_addr_ranges);

    let mut res = Disassembly {text: intro, lines: Vec::new(), error: None, longest_line: 0, symbols_shard: None};
    let addr_to_static = addr_map.dynamic_to_static(0);
    let mut subfunc_ranges: Vec<&[SubfunctionPcRange]> = Vec::new();

    while res.lines.len() < res.text.num_lines() {
        res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Intro, addr: 0, leaf_line: None, subfunction: None, indent_span_idx: 0});
    }

    if let Some(symbols) = &symbols {
        let function = &symbols.functions[function_idx];
        res.symbols_shard = Some((symbols as &Symbols as *const Symbols, function.shard_idx()));
        subfunc_ranges = (1..function.num_levels()).map(|i| symbols.subfunction_ranges_at_level(i, function)).collect();
    }

    for (addr_range_idx, static_addr_range) in static_addr_ranges.iter().enumerate() {
        if static_addr_range.len() > 100_000_000 {
            return res.with_error(error!(Sanity, "{} MB to disassemble, suspiciously much", static_addr_range.len() / 1_000_000), palette);
        }

        let mut buf: Vec<u8> = Vec::new();
        let code: &[u8] = if let Some(symbols) = &symbols {
            // Read the machine code from file rather than memory so that it doesn't show our breakpoint instructions.
            match symbols.elf.addr_range_to_offset_range(static_addr_range.start, static_addr_range.end) {
                None => return res.with_error(error!(Dwarf, "function address range out of bounds of executable: {:x}-{:x}", static_addr_range.start, static_addr_range.end), palette),
                Some((start, end)) => &symbols.elf.data()[start..end],
            }
        } else {
            buf.resize(static_addr_range.len(), 0);
            match memory.read(addr_map.static_to_dynamic(static_addr_range.start), &mut buf) {
                Ok(()) => (),
                Err(e) => return res.with_error(e, palette),
            }
            &buf
        };

        let addr = addr_map.static_to_dynamic(static_addr_range.start);

        if addr_range_idx != 0 {
            res.text.close_line();
            styled_write!(res.text, palette.disas_default,  "----------");
            res.text.close_line();
            res.text.close_line();
            for i in 0..3 {
                res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Separator, addr, leaf_line: None, subfunction: None, indent_span_idx: 0});
            }
        }

        let rel_addr_len = ((code.len() as f64).log2() / 4.0).ceil() as usize + 1; // how many hex digits to use in the "<+1abc>" things
        let prelude_width = 12 + 4 + rel_addr_len + 3;

        let resolver = Resolver {symbols: symbols.clone(), addr_to_static, current_function: addr..addr+code.len()};
        // NasmFormatter wants to own the symbol resolver for some reason. (Probably it would be too inconvenient or inefficient to have lifetime argument all throughout the formatter implementation.)
        // We trust that the SymbolResolver reference isn't retained after the formatter is destroyed, so it should be ok to fudge the lifetime here.
        let resolver: Resolver<'static> = unsafe { std::mem::transmute(resolver) };

        let mut decoder = Decoder::with_ip(64, code, addr as u64, DecoderOptions::NONE);
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
                assert!(instruction.ip() as usize >= l.addr); // can be == because the "-----[...]" separator above is assigned to address of the first instruction after it
            }
            let static_addr = addr_map.dynamic_to_static(instruction.ip() as usize);
            let mut indent = String::new();
            let mut cur_subfunction: Option<usize> = None;

            if let &Some(symbols) = &symbols {
                let write_line_number = |line: LineInfo, kind: DisassemblyLineKind, res: &mut Disassembly, indent: &str, leaf_line: &mut Option<LineInfo>, subfunction: Option<usize>| {
                    let file = match line.file_idx() {
                        None => return,
                        Some(f) => f };
                    *leaf_line = Some(line.clone());
                    let file = &symbols.files[file];
                    let name = file.filename.as_os_str().to_string_lossy();
                    styled_write!(res.text, palette.default_dim, "{: <1$}", "", prelude_width);
                    let indent_span_idx = res.text.spans.len() - *res.text.lines.last().unwrap();
                    styled_write!(res.text, palette.default_dim, "{}", indent);
                    styled_write!(res.text, palette.location_filename.add_modifier(Modifier::DIM), "{}", name);
                    if line.line() != 0 {
                        styled_write!(res.text, palette.location_line_number.add_modifier(Modifier::DIM), ":{}", line.line());
                        if line.column() != 0 {
                            styled_write!(res.text, palette.location_column_number.add_modifier(Modifier::DIM), ":{}", line.column());
                        }
                    }
                    res.text.close_line();
                    res.lines.push(DisassemblyLineInfo {kind, addr: instruction.ip() as usize, leaf_line: leaf_line.clone(), subfunction, indent_span_idx});
                };

                // Add inlined function calls information: function names, call line numbers, and indentation.
                let function = &symbols.functions[function_idx];
                for (level_minus_one, r) in subfunc_ranges.iter_mut().enumerate() {
                    while !r.is_empty() && r[0].range.end <= static_addr {
                        *r = &r[1..];
                    }
                    if r.is_empty() || r[0].range.start > static_addr {
                        break;
                    }
                    let subfunction = &symbols.shards[function.shard_idx()].subfunctions[r[0].subfunction_idx];
                    assert!(subfunction.level as usize == level_minus_one + 1);
                    if r[0].range.start > prev_static_addr {
                        let callee_name = if subfunction.callee_idx == usize::MAX {
                            "?".to_string()
                        } else {
                            symbols.functions[subfunction.callee_idx].demangle_name()
                        };

                        write_line_number(subfunction.call_line.clone(), DisassemblyLineKind::InlinedCallLineNumber, &mut res, &indent, &mut cur_leaf_line, cur_subfunction.clone());

                        styled_write!(res.text, palette.default_dim, "{: <1$}", "", prelude_width);
                        let indent_span_idx = res.text.spans.len() - *res.text.lines.last().unwrap();
                        styled_write!(res.text, palette.default_dim, "{}", indent);
                        styled_write!(res.text, palette.default_dim, "{}", callee_name);
                        res.text.close_line();
                        res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::InlinedFunctionName, addr: instruction.ip() as usize, leaf_line: cur_leaf_line.clone(), subfunction: cur_subfunction.clone(), indent_span_idx});
                    }
                    cur_subfunction = Some(r[0].subfunction_idx);
                    indent.push('⸽');
                }

                // Add line number information.
                while let Some(line) = line_iter.as_mut().unwrap().next_if(|line| line.addr() <= static_addr) {
                    write_line_number(line, DisassemblyLineKind::LeafLineNumber, &mut res, &indent, &mut cur_leaf_line, cur_subfunction.clone());
                }
            }

            prev_static_addr = static_addr;

            // Write the actual asm instruction.

            styled_write!(res.text, palette.default_dim, "{:012x} ", instruction.ip());
            styled_write!(res.text, palette.disas_relative_address, "<{: >+1$x}> ", instruction.ip() as usize - addr, rel_addr_len);

            let jump_arrow = match instruction.flow_control() {
                FlowControl::Next => " ",
                FlowControl::Return => "←",
                FlowControl::Call | FlowControl::IndirectCall => "→",
                FlowControl::Interrupt | FlowControl::XbeginXabortXend | FlowControl::Exception => "!",
                FlowControl::UnconditionalBranch | FlowControl::IndirectBranch | FlowControl::ConditionalBranch => {
                    let target_known = instruction.flow_control() != FlowControl::IndirectBranch && match instruction.op0_kind() {
                        iced_x86::OpKind::NearBranch16 | iced_x86::OpKind::NearBranch32 | iced_x86::OpKind::NearBranch64 => true,
                        _ => false };
                    if !target_known {
                        "↕"
                    } else if instruction.near_branch_target() > instruction.ip() {
                        "↓"
                    } else {
                        "↑"
                    }
                }
            };
            styled_write!(res.text, palette.disas_jump_arrow, " {} ", jump_arrow);
            let indent_span_idx = res.text.spans.len() - *res.text.lines.last().unwrap();
            styled_write!(res.text, palette.default_dim, "{}", indent);

            formatter.format(&instruction, &mut StyledFormatter {palette, text: &mut res.text});

            res.text.close_line();
            res.lines.push(DisassemblyLineInfo {kind: DisassemblyLineKind::Instruction, addr: instruction.ip() as usize, leaf_line: cur_leaf_line.clone(), subfunction: cur_subfunction.clone(), indent_span_idx});
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
