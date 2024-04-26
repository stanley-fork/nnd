#![allow(non_upper_case_globals)]
extern crate nnd;
use nnd::{*, error::{*, Result, Error}, elf::*, log::*};
use std::{fs::{File}, str, mem, collections::hash_map::DefaultHasher, hash::Hasher, fmt::Write as FmtWrite, io::{self, Write}, mem::drop, collections::HashMap};
use memmap2::Mmap;
use gimli::*;

type SliceType = EndianSlice<'static, LittleEndian>;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} binary", args[0]);
        std::process::exit(1);
    }

    let mut out = io::BufWriter::new(io::stdout());

    let file = File::open(&args[1])?;
    let mmap = unsafe { Mmap::map(&file)? };
    let elf = ElfFile::from_mmap(args[1].clone(), mmap)?;

    let load_section = |id: SectionId| -> std::result::Result<SliceType, Error> {
        match elf.section_by_name.get(id.name()) {
            None => Ok(EndianSlice::new(&[0u8;0][..], LittleEndian::default())),
            Some(&idx) => {
                let data = elf.section_data(idx);
                Ok(EndianSlice::new(unsafe {mem::transmute(data)}, LittleEndian::default()))
            }
        }
    };

    // Addresses that can be assumed to be at the start of a machine instructions.
    // Suitable places to start disassembling from.
    let mut all_anchors: Vec<usize> = Vec::new();

    // .eh_frame
    if false { // change to skip/unskip .eh_frame
        let prof = ProfileScope::new(".eh_frame".to_string());

        let printing = true; // change to dump .eh_frame contents

        let mut base_addresses = BaseAddresses::default()
            .set_text(elf.sections[*elf.section_by_name.get(".text").unwrap()].address as u64)
            .set_eh_frame(elf.sections[*elf.section_by_name.get(".eh_frame").unwrap()].address as u64)
            .set_eh_frame_hdr(elf.sections[*elf.section_by_name.get(".eh_frame_hdr").unwrap()].address as u64);
        if let Some(g) = elf.section_by_name.get(".got") {
            base_addresses = base_addresses.set_got(elf.sections[*g].address as u64);
        }
        let eh = EhFrame::new(load_section(SectionId::EhFrame)?.slice(), LittleEndian);

        let mut fde_count = 0usize;
        let mut row_count = 0usize;
        let mut bytes_covered_by_rows = 0usize;
        let mut bytes_covered_by_fdes = 0usize;
        let mut register_rules = 0usize;
        let mut max_rows_per_fde = 0usize;

        let mut cies: Vec<CommonInformationEntry<EndianSlice<LittleEndian>>> = Vec::new();
        let mut entries_iter = eh.entries(&base_addresses);
        while let Some(entry) = entries_iter.next()? {
            if let CieOrFde::Cie(cie) = entry {
                if printing { writeln!(out, "cie @{}: encoding {:?}, address_size {}, entry_len {}, version {}, has_lsda {}, lsda_encoding {:?}, personality_with_encoding {:?}, fde_address_encoding {:?}, is_signal_trampoline {:?}, code_alignment_factor {}, data_alignment_factor {}, return_address_register {:?}", cie.offset(), cie.encoding(), cie.address_size(), cie.entry_len(), cie.version(), cie.has_lsda(), cie.lsda_encoding(), cie.personality_with_encoding(), cie.fde_address_encoding(), cie. is_signal_trampoline(), cie.code_alignment_factor(), cie.data_alignment_factor(), cie.return_address_register())?; }
                cies.push(cie);
            }
        }
        cies.sort_by_key(|c| c.offset());

        let mut entries_iter = eh.entries(&base_addresses);
        let mut unwind_context = UnwindContext::new();
        dbg!(mem::size_of::<UnwindContext<EndianSlice<LittleEndian>>>()); // documentation says to put it in a Box because it's big, but idk what they mean, it's only 64 bytes; maybe they changed UnwindContext to store stuff on the heap internally but didn't update the documentation?
        while let Some(entry) = entries_iter.next()? {
            match entry {
                CieOrFde::Cie(_) => (),
                CieOrFde::Fde(fde) => {
                    let fde = fde.parse(|_, _, offset| {
                        let idx = cies.partition_point(|c| c.offset() < offset.0);
                        assert!(idx < cies.len() && cies[idx].offset() == offset.0);
                        Ok(cies[idx].clone())
                    })?;
                    fde_count += 1;
                    bytes_covered_by_fdes += fde.len() as usize;
                    if printing { writeln!(out, "fde @{}: initial_address {}, len {}, lsda {:?}, personality {:?}", fde.offset(), fde.initial_address(), fde.len(), fde.lsda(), fde.personality())?; }
                    let mut prev_address = fde.initial_address();
                    let mut rows_iter = fde.rows(&eh, &base_addresses, &mut unwind_context)?;
                    let mut count = 0usize;
                    while let Some(row) = rows_iter.next_row()? {
                        if printing { writeln!(out, "  row: start_address {}, end_address {}, saved_args_size {}, cfa {:?}", row.start_address(), row.end_address(), row.saved_args_size(), row.cfa())?; }
                        count += 1;
                        bytes_covered_by_rows += (row.end_address() - row.start_address()) as usize;
                        all_anchors.push(row.start_address() as usize);
                        assert!(row.start_address() == prev_address);
                        assert!(row.end_address() >= row.start_address());
                        if row.end_address() == row.start_address() {
                            if printing { writeln!(out, "  (row has equal start and end addresses!)")?; }
                        }
                        prev_address = row.end_address();
                        for (reg, rule) in row.registers() {
                            register_rules += 1;
                            if printing { writeln!(out, "    register {:?} rule {:?}", reg, rule)?; }
                        }
                    }
                    row_count += count;
                    max_rows_per_fde = max_rows_per_fde.max(count);
                }
            }
        }

        drop(prof);
        dbg!(cies.len());
        dbg!(fde_count);
        dbg!(row_count);
        dbg!(bytes_covered_by_fdes);
        dbg!(bytes_covered_by_rows);
        dbg!(register_rules);
        dbg!(max_rows_per_fde);
        eprintln!("-----");
    }
    
    let dwarf = Dwarf::load(load_section)?;

    let mut line_programs: Vec<(IncompleteLineProgram<SliceType>, Unit<SliceType>)> = Vec::new();
    let mut units_vec: Vec<Unit<SliceType>> = Vec::new();

    {
        let prof = ProfileScope::new(".debug_info pre-pass".to_string());

        // We have to iterate everything twice because dwarf sux.
        let mut units_iter = dwarf.units();
        while let Some(header) = units_iter.next()? {
            let unit = dwarf.unit(header)?;
            let unit_offset = unit.header.offset().as_debug_info_offset().unwrap();

            let mut entries_iter = unit.entries();
            let root = match entries_iter.next_dfs()? {
                None => return err!(Dwarf, "unit @{} has no entries", unit_offset.0),
                Some((_, e)) => e,
            };
            if let Some(val) = root.attr_value(DW_AT_stmt_list)? {
                if let AttributeValue::DebugLineRef(off) = val {
                    let program = dwarf.debug_line.program(off, unit.header.address_size(), unit.comp_dir, unit.name)?;
                    line_programs.push((program, dwarf.unit(unit.header.clone())?));
                } else {
                    eprintln!("DW_AT_stmt_list has unexpected form: {:?}", val);
                }
            }

            units_vec.push(unit);
        }

        drop(prof);
        dbg!(units_vec.len());
        dbg!(line_programs.len());
        eprintln!("-----");
    }

    if true { // change to skip/unskip .debug_info
        let find_unit = |offset: DebugInfoOffset| -> Result<(&Unit<SliceType>, UnitOffset)> {
            let idx = units_vec.partition_point(|u| u.header.offset().as_debug_info_offset().unwrap() <= offset);
            if idx == 0 { return err!(Dwarf, "unit offset out of bounds: {}", offset.0); }
            let unit = &units_vec[idx - 1];
            Ok((unit, offset.to_unit_offset(&unit.header).unwrap()))
        };

        let mut dies = 0usize;
        let mut attrs = 0usize;
        let mut funcs_including_decls_and_inline = 0usize; // all other "funcs" stats are only among functions with code (low_pc or ranges)
        let mut funcs = 0usize;
        let mut func_ranges = 0usize;
        let mut template_funcs = 0usize;
        let mut funcs_without_name = 0usize;
        let mut funcs_without_link_name = 0usize;
        let mut funcs_with_multiple_ranges = 0usize;
        let mut vars = 0usize;
        let mut locs = 0usize;
        let mut template_types = 0usize;
        let mut fields = 0usize;
        let mut inlined_funcs = 0usize;
        let mut inlined_func_ranges = 0usize;
        let mut inlined_funcs_without_ranges = 0usize;
        let mut total_func_link_name_len = 0usize;
        let mut total_func_full_name_len = 0usize;
        let mut total_func_local_name_len = 0usize;
        let mut total_var_name_len = 0usize;
        let mut total_type_full_name_len = 0usize;
        let mut longest_origin_chain = 0usize;

        let mut vars_without_loc = 0usize;
        let mut func_instr_len = 0usize;

        let mut types = 0usize;
        let mut composite_types = 0usize;
        let mut base_types = 0usize;
        let mut derivative_types = 0usize;
        let mut cross_unit_type_refs = 0usize;
        let mut forward_type_refs = 0usize;
        let mut forward_base_type_refs = 0usize;
        let mut forward_derivative_type_refs = 0usize;
        let mut forward_composite_type_refs = 0usize;
        let mut base_type_refs = 0usize;
        let mut derivative_type_refs = 0usize;
        let mut composite_type_refs = 0usize;
        let mut ptr_to_member_type_refs = 0usize;
        let mut subrange_type_refs = 0usize;
        let mut unrecognized_type_refs = 0usize;
        let mut type_refs = 0usize;
        let mut type_refs_with_specification = 0usize;
        let mut type_refs_with_abstract_origin = 0usize;
        let mut data_member_loc_simple = 0usize;
        let mut data_member_loc_expr = 0usize;
        let mut nontrivial_inheritance_loc = 0usize;
        let mut data_member_loc_list = 0usize;
        let mut type_definitions_in_different_unit = 0usize;

        let mut func_ranges_hashes: Vec<u64> = Vec::new();

        let mut type_declarations: Vec<usize> = Vec::new();
        let mut type_definitions: Vec<usize> = Vec::new();

        let prof = ProfileScope::new(".debug_info".to_string());

        for unit in &units_vec {
            let unit_offset = unit.header.offset().as_debug_info_offset().unwrap();
            //writeln!(out, "unit @{} {:?} type {:?} at {:?} version {} length {} type {:?}", unit_offset.0, match &unit.name { None => "[unknown]", Some(n) => str::from_utf8(n.slice())? }, unit.header.type_(), match &unit.comp_dir { None => "[unknown]", Some(n) => str::from_utf8(n.slice())? }, unit.header.version(), unit.header.unit_length(), unit.header.type_())?;

            // (tag, scope_name length, template, function)
            let mut stack: Vec<(gimli::DwTag, usize, bool, bool)> = Vec::new();
            let mut clean_depth = 0isize;

            let mut scope_name: String = String::new();

            let mut entries_iter = unit.entries();
            let mut depth = 0isize;
            let mut pretty_value = String::new();
            assert!(entries_iter.current().is_none());
            loop {
                let (mut delta_depth, entry) = match entries_iter.next_dfs()? {
                    None => break,
                    Some(v) => v,
                };

                dies += 1;

                if depth == 0 {
                    assert!(delta_depth >= 0);
                    delta_depth = 1;
                }

                if delta_depth <= 0 {
                    depth += delta_depth - 1;
                    stack.truncate(depth as usize);
                    clean_depth = clean_depth.min(depth);
                    scope_name.truncate(match stack.last() { None => 0, Some(x) => x.1 });
                }

                if depth == clean_depth {
                    // This is a matrix of which types of DIEs we expect to be children of which other types. If we get an unexpected nesting or unexpected type, we print the subtree starting from it (this is what clean_depth does).
                    // This is just a way for me to learn what to expect in the debug info, and to check that I didn't miss anything important. Not complete by any means, not based on the spec.
                    // If this outputs something then I have more things to learn and consider making use of. This could be cleaned up into a compatibility check tool, with the same idea but more "what the debugger actually uses" rather than "what I personally currently know about".
                    let expected = match stack.last() {
                        None => [DW_TAG_compile_unit, DW_TAG_partial_unit].contains(&entry.tag()),
                        Some(&(tag, _, _, _)) => {
                            if [DW_TAG_compile_unit, DW_TAG_partial_unit, DW_TAG_namespace, DW_TAG_subprogram, DW_TAG_structure_type, DW_TAG_class_type, DW_TAG_union_type, DW_TAG_enumeration_type, DW_TAG_lexical_block, DW_TAG_inlined_subroutine].contains(&tag) &&
                                [DW_TAG_namespace,
                                 DW_TAG_subprogram,
                                 DW_TAG_variable,
                                 DW_TAG_pointer_type, DW_TAG_base_type, DW_TAG_const_type, DW_TAG_reference_type, DW_TAG_array_type, DW_TAG_rvalue_reference_type, DW_TAG_unspecified_type, DW_TAG_restrict_type, DW_TAG_volatile_type, DW_TAG_ptr_to_member_type, DW_TAG_subroutine_type, DW_TAG_atomic_type,
                                 DW_TAG_typedef,
                                 DW_TAG_label,
                                 DW_TAG_structure_type, DW_TAG_class_type, DW_TAG_union_type, DW_TAG_enumeration_type,
                                 DW_TAG_imported_declaration, DW_TAG_imported_module, DW_TAG_imported_unit].contains(&entry.tag()) {
                                    true
                                } else if
                                [DW_TAG_subprogram, DW_TAG_inlined_subroutine, DW_TAG_lexical_block].contains(&tag) &&
                                [DW_TAG_formal_parameter, DW_TAG_unspecified_parameters, DW_TAG_GNU_formal_parameter_pack, DW_TAG_inlined_subroutine, DW_TAG_GNU_call_site, DW_TAG_call_site, DW_TAG_lexical_block].contains(&entry.tag()) {
                                    true
                                } else if
                                [DW_TAG_subprogram, DW_TAG_structure_type, DW_TAG_class_type, DW_TAG_union_type, DW_TAG_enumeration_type, DW_TAG_variable].contains(&tag) &&
                                [DW_TAG_template_type_parameter, DW_TAG_template_value_parameter, DW_TAG_GNU_template_parameter_pack, DW_TAG_GNU_template_template_param].contains(&entry.tag()) {
                                    true
                                } else if
                                [DW_TAG_structure_type, DW_TAG_class_type].contains(&tag) &&
                                [DW_TAG_inheritance, DW_TAG_variant_part].contains(&entry.tag()) {
                                    true
                                } else if
                                [DW_TAG_structure_type, DW_TAG_class_type, DW_TAG_union_type, DW_TAG_variant_part, DW_TAG_variant].contains(&tag) &&
                                [DW_TAG_member].contains(&entry.tag()) {
                                    true
                                } else {
                                    match tag {
                                        DW_TAG_enumeration_type => entry.tag() == DW_TAG_enumerator,
                                        DW_TAG_array_type => entry.tag() == DW_TAG_subrange_type,
                                        DW_TAG_subroutine_type => [DW_TAG_formal_parameter, DW_TAG_unspecified_parameters, DW_TAG_GNU_formal_parameter_pack].contains(&entry.tag()),
                                        DW_TAG_call_site | DW_TAG_GNU_call_site => [DW_TAG_GNU_call_site_parameter, DW_TAG_call_site_parameter].contains(&entry.tag()),
                                        DW_TAG_variant_part => entry.tag() == DW_TAG_variant,
                                        DW_TAG_GNU_template_parameter_pack => [DW_TAG_template_type_parameter, DW_TAG_template_value_parameter].contains(&entry.tag()),
                                        DW_TAG_GNU_formal_parameter_pack => entry.tag() == DW_TAG_formal_parameter,
                                        _ => false,
                                    }
                                }
                        }
                    };

                    if expected {
                        clean_depth += 1;
                    } else {
                        writeln!(out, "  unexpected transition, will print subtree, stack so far: {}", stack.iter().map(|x| format!("{}", x.0)).collect::<Vec<String>>().join(", "))?;
                    }
                }

                // Print either everything or only subtrees of unexpected entries.
                let printing = depth + 1 > clean_depth;
                //let printing = true;

                if printing { writeln!(out, "{: <4$}{} @{}:{}", "", entry.tag(), unit_offset.0, entry.offset().0, depth as usize * 2)?; }

                // Whether any parent is template. Note that the current func/type may turn out to be a template later, when inspecting its children.
                let mut in_template = stack.last().map_or(false, |x| x.2);
                // Whether the innermost function/struct is a function that has code.
                let mut in_function = stack.last().map_or(false, |x| x.3);

                let mut is_type = true;
                match entry.tag() {
                    DW_TAG_base_type | DW_TAG_unspecified_type => base_types += 1,
                    DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type | DW_TAG_variant | DW_TAG_subroutine_type => composite_types += 1,
                    DW_TAG_pointer_type | DW_TAG_const_type | DW_TAG_reference_type | DW_TAG_array_type | DW_TAG_rvalue_reference_type | DW_TAG_restrict_type | DW_TAG_volatile_type | DW_TAG_atomic_type | DW_TAG_typedef | DW_TAG_ptr_to_member_type | DW_TAG_subrange_type => derivative_types += 1,
                    _ => is_type = false,
                };
                if is_type {
                    types += 1;
                    if in_template {
                        template_types += 1;
                    }
                }

                let mut regular_name: Option<&'static str> = None;
                let mut linkage_name: Option<&'static str> = None;
                let mut num_locations = 0usize;
                let mut num_ranges = 0usize;
                let mut ranges_len = 0usize;
                let mut low_pc: Option<usize> = None;
                let mut high_pc: Option<(usize, /* relative to low_pc */ bool)> = None;
                let mut ranges_hash: Option<u64> = None;
                let mut is_declaration = false;
                let mut has_specification = false;
                let mut has_abstract_origin = false;

                let mut attrs_iter = entry.attrs();
                while let Some(attr) = attrs_iter.next()? {
                    attrs += 1;
                    pretty_value.clear();
                    
                    match attr.name() {
                        DW_AT_location => {
                            match dwarf.attr_locations(&unit, attr.value())? {
                                Some(mut loc_iter) => {
                                    while let Some(loc) = loc_iter.next()? {
                                        num_locations += 1;
                                        write!(pretty_value, "{}-{} ", loc.range.begin, loc.range.end)?;
                                    }
                                }
                                None => {
                                    match attr.value().exprloc_value() {
                                        None => {
                                            eprintln!("DW_AT_location has unexpected form: {:?}", attr);
                                            write!(pretty_value, "[error]")?;
                                        }
                                        Some(e) => {
                                            num_locations += 1;
                                            write!(pretty_value, "[expr {} bytes]", e.0.len())?;
                                        }
                                    }
                                }
                            }
                        }
                        DW_AT_ranges => {
                            match dwarf.attr_ranges(&unit, attr.value())? {
                                None => {
                                    eprintln!("DW_AT_ranges has unexpected form: {:?}", attr);
                                    write!(pretty_value, "{:?}", attr.value())?;
                                }
                                Some(mut range_iter) => {
                                    let mut hasher = DefaultHasher::new();
                                    while let Some(range) = range_iter.next()? {
                                        num_ranges += 1;
                                        ranges_len += (range.end - range.begin) as usize;
                                        all_anchors.push(range.begin as usize);
                                        hasher.write_u64(range.begin);
                                        hasher.write_u64(range.end);
                                        write!(pretty_value, "{}-{} ", range.begin, range.end)?;
                                    }
                                    ranges_hash = Some(hasher.finish());
                                }
                            }
                            
                        }
                        DW_AT_declaration => {
                            match attr.value() {
                                AttributeValue::Flag(f) if f => {
                                    is_declaration = true;
                                    write!(pretty_value, "true")?;
                                }
                                _ => {
                                    eprintln!("DW_AT_declaration has unexpected form: {:?}", attr);
                                    write!(pretty_value, "{:?}", attr.value())?;
                                }
                            };
                        }
                        DW_AT_specification | DW_AT_abstract_origin => {
                            match entry.tag() {
                                DW_TAG_subprogram | DW_TAG_inlined_subroutine | DW_TAG_formal_parameter | DW_TAG_variable | DW_TAG_GNU_call_site | DW_TAG_label => (),
                                _ => eprintln!("{} on {} @{}:{}", attr.name(), entry.tag(), unit.header.offset().as_debug_info_offset().unwrap().0, entry.offset().0),
                            }
                            
                            // Chase the specification/origin pointers.
                            let mut cur_attr = attr.clone();
                            let mut cur_unit = unit;
                            let mut chain_len = 1;
                            if cur_attr.name() == DW_AT_specification {
                                has_specification = true;
                            } else {
                                has_abstract_origin = true;
                            }
                            loop {
                                chain_len += 1;
                                let (next_unit, next_offset) = match cur_attr.value() {
                                    AttributeValue::UnitRef(unit_offset) => {
                                        write!(pretty_value, "-> [unit + {}] ", unit_offset.0)?;
                                        (cur_unit, unit_offset)
                                    }
                                    AttributeValue::DebugInfoRef(offset) => {
                                        write!(pretty_value, "-> [.debug_info + {}] ", offset.0)?;
                                        if is_type {
                                            type_definitions_in_different_unit += 1;
                                        }
                                        find_unit(offset)?
                                    }
                                    _ => return err!(Dwarf, "{} has unexpected form: {:?}", cur_attr.name(), cur_attr.value()),
                                };
                                let next_entry = next_unit.entry(next_offset)?;
                                if cur_attr.name() == DW_AT_specification {
                                    if next_entry.attr(DW_AT_declaration)?.is_none() {
                                        eprintln!("DW_AT_specification @{}:{} points to an entry without DW_AT_declaration", cur_unit.header.offset().as_debug_info_offset().unwrap().0, entry.offset().0);
                                    } else if is_type {
                                        type_definitions.push(next_unit.header.offset().as_debug_info_offset().unwrap().0 + next_entry.offset().0);
                                    }
                                }

                                let next_name = match next_entry.attr_value(DW_AT_name)? {
                                    None => None,
                                    Some(v) => Some(str::from_utf8(dwarf.attr_string(next_unit, v)?.slice())?),
                                };
                                let next_linkage_name = match next_entry.attr_value(DW_AT_linkage_name)? {
                                    None => None,
                                    Some(v) => Some(str::from_utf8(dwarf.attr_string(next_unit, v)?.slice())?),
                                };
                                if regular_name.is_none() {
                                    regular_name = next_name;
                                }
                                if linkage_name.is_none() {
                                    linkage_name = next_linkage_name;
                                }
                                if let Some(n) = next_name {
                                    write!(pretty_value, "{} ", n)?;
                                }
                                if let Some(n) = next_linkage_name {
                                    write!(pretty_value, "({}) ", n)?;
                                }

                                cur_unit = next_unit;
                                cur_attr = if let Some(a) = next_entry.attr(DW_AT_specification)? {
                                    a
                                } else if let Some(a) = next_entry.attr(DW_AT_abstract_origin)? {
                                    a
                                } else {
                                    break;
                                }
                            }
                            longest_origin_chain = longest_origin_chain.max(chain_len);
                        }
                        DW_AT_name | DW_AT_comp_dir | DW_AT_linkage_name => {
                            let slice = dwarf.attr_string(&unit, attr.value())?;
                            let s = str::from_utf8(slice.slice())?;
                            write!(pretty_value, "{}", s)?;
                            match attr.name() {
                                DW_AT_name => regular_name = Some(s),
                                DW_AT_linkage_name => linkage_name = Some(s),
                                _ => (),
                            }
                        }
                        DW_AT_low_pc => {
                            if let Some(a) = dwarf.attr_address(&unit, attr.value())? {
                                low_pc = Some(a as usize);
                                all_anchors.push(a as usize);
                                write!(pretty_value, "{}", a)?;
                            } else {
                                eprintln!("DW_AT_low_pc has unexpected form: {:?}", attr.value());
                                write!(pretty_value, "{:?}", attr.value())?;
                            }
                        }
                        DW_AT_high_pc => {
                            if let Some(a) = dwarf.attr_address(&unit, attr.value())? {
                                high_pc = Some((a as usize, false));
                                write!(pretty_value, "{}", a)?;
                            } else if let Some(n) = attr.value().sdata_value() {
                                high_pc = Some((n as usize, true));
                                write!(pretty_value, "{:+}", n)?;
                            } else {
                                eprintln!("DW_AT_high_pc has unexpected form: {:?}", attr.value());
                                write!(pretty_value, "{:?}", attr.value())?;
                            }
                        }
                        DW_AT_type => {
                            type_refs += 1;
                            match attr.value() {
                                AttributeValue::UnitRef(unit_offset) => {
                                    write!(pretty_value, "[unit + {}] ", unit_offset.0)?;
                                    let forward = if unit_offset.0 > entry.offset().0 {
                                        forward_type_refs += 1;
                                        1
                                    } else {
                                        0
                                    };
                                    let t = unit.entry(unit_offset)?;
                                    match t.tag() {
                                        DW_TAG_base_type | DW_TAG_unspecified_type => { base_type_refs += 1; forward_base_type_refs += forward; }
                                        DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type | DW_TAG_subroutine_type => { composite_type_refs += 1; forward_composite_type_refs += forward; }
                                        DW_TAG_pointer_type | DW_TAG_const_type | DW_TAG_reference_type | DW_TAG_array_type | DW_TAG_rvalue_reference_type | DW_TAG_restrict_type | DW_TAG_volatile_type | DW_TAG_atomic_type | DW_TAG_typedef => {
                                            derivative_type_refs += 1;
                                            forward_derivative_type_refs += forward;
                                        }
                                        DW_TAG_ptr_to_member_type => ptr_to_member_type_refs += 1,
                                        DW_TAG_subrange_type => subrange_type_refs += 1,
                                        _ => unrecognized_type_refs += 1,
                                    }
                                    if t.attr(DW_AT_specification)?.is_some() {
                                        type_refs_with_specification += 1;
                                    }
                                    if t.attr(DW_AT_abstract_origin)?.is_some() {
                                        type_refs_with_abstract_origin += 1;
                                    }
                                }
                                AttributeValue::DebugInfoRef(offset) => {
                                    write!(pretty_value, "[.debug_info + {}] ", offset.0)?;
                                    find_unit(offset)?;
                                    cross_unit_type_refs += 1;
                                }
                                AttributeValue::DebugTypesRef(_) => eprintln!("type signature reference found"),
                                _ => eprintln!("DW_AT_type has unexpected form: {:?}", attr.value()),
                            };
                        }
                        DW_AT_data_member_location => {
                            let val = attr.value();
                            if let Some(x) = val.udata_value() {
                                data_member_loc_simple += 1;
                                write!(pretty_value, "{}", x)?;
                            } else if let AttributeValue::Exprloc(expr) = &val { // virtual inheritance - offset of the base class is stored in the vtable
                                //eprintln!("nontrivial member loc @{}:{}: {:?}", unit_offset.0, entry.offset().0, val); let mut it = expr.operations(unit.header.encoding()); while let Some(op) = it.next()? { eprintln!("       {:?}", op); }
                                write!(pretty_value, "{:?}", expr)?;
                                data_member_loc_expr += 1;
                                if entry.tag() == DW_TAG_inheritance { nontrivial_inheritance_loc += 1; }
                            } else if let Some(locs) = dwarf.attr_locations(&unit, val)? {
                                write!(pretty_value, "{:?}", locs)?;
                                data_member_loc_list += 1;
                                if entry.tag() == DW_TAG_inheritance { nontrivial_inheritance_loc += 1; }
                            }
                        }
                        _ => {
                            if let Some(n) = attr.value().sdata_value() {
                                write!(pretty_value, "{}", n)?;
                            } else if let Ok(s) = dwarf.attr_string(&unit, attr.value()) {
                                write!(pretty_value, "{}", str::from_utf8(&s)?)?;
                            } else {
                                write!(pretty_value, "{:?}", attr.value())?;
                            }
                        }
                    }
                    if printing { writeln!(out, "{: <3$}| {} = {}", "", attr.name(), pretty_value, depth as usize * 2)?; }

                    // Matrix telling which attributes we expect to see on which entries. Like with tags, this is mostly just for my understanding, based on trial and error, very incomplete.
                    let ex: &'static [DwTag] = match attr.name() {
                        // These are uninteresting.
                        DW_AT_producer | DW_AT_language | DW_AT_GNU_pubnames | DW_AT_accessibility | DW_AT_explicit => &[],
                        DW_AT_inline => &[DW_TAG_subprogram],
                        DW_AT_external => &[DW_TAG_subprogram, DW_TAG_member, DW_TAG_variable],
                        DW_AT_artificial => &[DW_TAG_formal_parameter, DW_TAG_member, DW_TAG_subprogram, DW_TAG_variable],
                        DW_AT_alignment => &[DW_TAG_member, DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type, DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_typedef, DW_TAG_array_type],
                        DW_AT_GNU_vector => &[DW_TAG_array_type],
                        DW_AT_import => &[DW_TAG_imported_unit],
                        DW_AT_name | DW_AT_description => &[],
                        DW_AT_sibling => &[],
                        DW_AT_GNU_locviews => &[DW_TAG_formal_parameter, DW_TAG_variable],

                        DW_AT_stmt_list => &[DW_TAG_compile_unit, DW_TAG_partial_unit], // use as entry point to the line number program
                        DW_AT_comp_dir => &[DW_TAG_compile_unit, DW_TAG_partial_unit],

                        // Use for address -> stuff mapping.
                        DW_AT_low_pc => &[DW_TAG_compile_unit,DW_TAG_partial_unit, DW_TAG_inlined_subroutine, DW_TAG_subprogram, DW_TAG_lexical_block, DW_TAG_label, DW_TAG_GNU_call_site],
                        DW_AT_high_pc => &[DW_TAG_compile_unit,DW_TAG_partial_unit, DW_TAG_inlined_subroutine, DW_TAG_subprogram, DW_TAG_lexical_block],
                        DW_AT_ranges => &[DW_TAG_compile_unit,DW_TAG_partial_unit, DW_TAG_inlined_subroutine, DW_TAG_subprogram, DW_TAG_lexical_block],
                        
                        DW_AT_type => &[DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_inheritance, DW_TAG_typedef, DW_TAG_member, DW_TAG_subprogram, DW_TAG_enumeration_type,
                                        DW_TAG_pointer_type,DW_TAG_const_type,DW_TAG_reference_type,DW_TAG_array_type,DW_TAG_rvalue_reference_type,DW_TAG_restrict_type,DW_TAG_volatile_type,DW_TAG_ptr_to_member_type,DW_TAG_subrange_type,DW_TAG_subroutine_type,DW_TAG_atomic_type],
                        DW_AT_decl_file | DW_AT_decl_line | DW_AT_decl_column => &[DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_subprogram, DW_TAG_typedef, DW_TAG_member, DW_TAG_label,  DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type],
                        DW_AT_linkage_name => &[DW_TAG_variable, DW_TAG_subprogram],

                        DW_AT_frame_base => &[DW_TAG_subprogram],
                        DW_AT_calling_convention => &[DW_TAG_subprogram,  DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type],
                        DW_AT_declaration => &[DW_TAG_subprogram, DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type, DW_TAG_member, DW_TAG_variable],
                        DW_AT_prototyped => &[DW_TAG_subprogram, DW_TAG_subroutine_type],
                        
                        DW_AT_abstract_origin => &[DW_TAG_inlined_subroutine, DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_GNU_call_site, DW_TAG_subprogram, DW_TAG_label, DW_TAG_lexical_block],
                        DW_AT_call_file => &[DW_TAG_inlined_subroutine],
                        DW_AT_call_line => &[DW_TAG_inlined_subroutine],
                        DW_AT_call_column => &[DW_TAG_inlined_subroutine],
                        DW_AT_entry_pc => &[DW_TAG_inlined_subroutine],
                        DW_AT_GNU_entry_view => &[DW_TAG_inlined_subroutine],

                        DW_AT_byte_size => &[DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type, DW_TAG_base_type, DW_TAG_member, DW_TAG_array_type, DW_TAG_pointer_type],

                        DW_AT_data_member_location => &[DW_TAG_inheritance, DW_TAG_member],

                        DW_AT_containing_type => &[DW_TAG_ptr_to_member_type, DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type, DW_TAG_subprogram],
                        DW_AT_count => &[DW_TAG_subrange_type],

                        DW_AT_export_symbols => &[DW_TAG_structure_type,DW_TAG_class_type,DW_TAG_enumeration_type,DW_TAG_union_type, DW_TAG_namespace],

                        DW_AT_const_value => &[DW_TAG_enumerator, DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_member],

                        DW_AT_location => &[DW_TAG_variable, DW_TAG_formal_parameter, DW_TAG_GNU_call_site_parameter, DW_TAG_call_site_parameter, DW_TAG_dwarf_procedure],

                        DW_AT_discr => &[DW_TAG_variant_part],
                        DW_AT_discr_value => &[DW_TAG_variant],

                        DW_AT_encoding => &[DW_TAG_base_type, DW_TAG_enumeration_type],
                        DW_AT_address_class => &[DW_TAG_pointer_type],
                        DW_AT_lower_bound | DW_AT_upper_bound => &[DW_TAG_subrange_type],
                        DW_AT_enum_class => &[DW_TAG_enumeration_type],
                        DW_AT_specification => &[DW_TAG_subprogram, DW_TAG_variable, DW_TAG_formal_parameter], // points the the declaration; attrs should be merged from both places
                        DW_AT_reference => &[DW_TAG_subprogram],
                        DW_AT_rvalue_reference => &[DW_TAG_subprogram],
                        DW_AT_noreturn => &[DW_TAG_subprogram],
                        DW_AT_object_pointer => &[DW_TAG_subprogram],

                        DW_AT_GNU_tail_call => &[DW_TAG_GNU_call_site],
                        DW_AT_call_tail_call => &[DW_TAG_call_site],
                        DW_AT_GNU_call_site_target => &[DW_TAG_GNU_call_site],
                        DW_AT_GNU_call_site_value => &[DW_TAG_GNU_call_site_parameter],
                        DW_AT_call_target => &[DW_TAG_call_site],
                        DW_AT_call_all_calls => &[DW_TAG_subprogram],
                        DW_AT_GNU_all_call_sites => &[DW_TAG_subprogram],
                        DW_AT_call_all_tail_calls => &[DW_TAG_subprogram],
                        DW_AT_call_pc => &[DW_TAG_call_site],
                        DW_AT_call_return_pc => &[DW_TAG_call_site],
                        DW_AT_call_value => &[DW_TAG_call_site_parameter],
                        DW_AT_call_parameter => &[DW_TAG_call_site_parameter],
                        DW_AT_call_origin => &[DW_TAG_call_site],

                        DW_AT_virtuality => &[DW_TAG_subprogram, DW_TAG_inheritance],
                        DW_AT_vtable_elem_location => &[DW_TAG_subprogram],
                        DW_AT_deleted => &[DW_TAG_subprogram],

                        DW_AT_bit_offset => &[DW_TAG_member],
                        DW_AT_bit_size => &[DW_TAG_member],
                        DW_AT_data_bit_offset => &[DW_TAG_member],

                        _ => &[DW_TAG_compile_unit,DW_TAG_partial_unit],
                    };
                    let uninteresting_tags = &[DW_TAG_template_type_parameter, DW_TAG_template_value_parameter, DW_TAG_GNU_template_parameter_pack, DW_TAG_GNU_template_template_param, DW_TAG_imported_declaration, DW_TAG_imported_module];

                    if !ex.is_empty() && !ex.contains(&entry.tag()) && !uninteresting_tags.contains(&entry.tag()) {
                        eprintln!("Unexpected attribute @{}:{} {} on {} at {}", unit_offset.0, entry.offset().0, attr.name(), entry.tag(), stack.iter().map(|x| format!("{}", x.0)).collect::<Vec<String>>().join(", "));
                    }
                }

                if num_ranges == 0 {
                    if let &Some((high, rel)) = &high_pc {
                        if let &Some(low) = &low_pc {
                            num_ranges += 1;
                            let high = if rel { low.wrapping_add(high) } else { high };
                            if high < low {
                                eprintln!("bad [low_pc, high_pc) range: {} < {}", high, low);
                            }
                            ranges_len += high - low;
                            let mut hasher = DefaultHasher::new();
                            hasher.write_u64(low as u64);
                            hasher.write_u64(high as u64);
                            ranges_hash = Some(hasher.finish());
                        } else {
                            eprintln!("high_pc without low_pc");
                        }
                    } else if let &Some(low) = &low_pc {
                        num_ranges += 1;
                        ranges_len += 1;
                        let mut hasher = DefaultHasher::new();
                        hasher.write_u64(low as u64);
                        hasher.write_u64((low + 1) as u64);
                        ranges_hash = Some(hasher.finish());
                    }
                }

                // Append to current scope name. Note that the resulting "fully-qualified" names are not standard and not necessarily unique.
                // E.g. there may be structs with the same name in two different scopes of the same function, or in different anonymous namespaces.
                // Instead of inventing a mangling scheme to produce arcane unique names, we should make it easy to search and pick from the list of results,
                // or to specify file where the struct is declared.
                match entry.tag() {
                    DW_TAG_namespace | DW_TAG_subprogram | DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type => {
                        if let Some(n) = &regular_name {
                            if !scope_name.is_empty() {
                                scope_name += "::";
                            }
                            scope_name += n;
                        }
                    }
                    _ => (),
                }

                // Check that things that have code have names.
                match entry.tag() {
                    DW_TAG_subprogram | DW_TAG_variable | DW_TAG_compile_unit if num_ranges > 0 => {
                        if regular_name.is_none() && linkage_name.is_none() {
                            eprintln!("{} without name @{}:{}", entry.tag(), unit_offset.0, entry.offset().0);
                        }
                    }
                    _ => (),
                }

                if is_declaration && is_type {
                    type_declarations.push(unit_offset.0 + entry.offset().0);
                }

                if is_declaration && num_ranges != 0 {
                    eprintln!("declaration has ranges on {} @{}:{}", entry.tag(), unit_offset.0, entry.offset().0);
                }

                if has_specification as usize + has_abstract_origin as usize + is_declaration as usize > 1 {
                    // DW_TAG_variable can have both DW_AT_declaration and DW_AT_abstract_origin, and idk what that means.
                    if entry.tag() != DW_TAG_variable || has_specification {
                        eprintln!("{} @{}:{} is more than one of: specification ({}), abstract_origin ({}), declaration ({})", entry.tag(), unit_offset.0, entry.offset().0, has_specification, has_abstract_origin, is_declaration);
                    }
                }

                if entry.tag() == DW_TAG_subprogram {
                    funcs_including_decls_and_inline += 1;
                }

                match entry.tag() {
                    // Exclude functions with no code. Most of them are declarations, including duplicates (from including the same header in many translation units).
                    DW_TAG_subprogram if num_ranges > 0 => {
                        funcs += 1;
                        func_ranges += num_ranges;
                        func_instr_len += ranges_len;
                        if num_ranges > 1 {
                            funcs_with_multiple_ranges += 1;
                        }
                        in_function = true;
                        if in_template {
                            template_funcs += 1;
                        }

                        total_func_full_name_len += scope_name.len();

                        if let Some(n) = &regular_name {
                            total_func_local_name_len += n.len();
                        } else {
                            funcs_without_name += 1;
                        }
                        if let Some(n) = &linkage_name {
                            total_func_link_name_len += n.len();
                        } else {
                            funcs_without_link_name += 1;
                        }

                        func_ranges_hashes.push(ranges_hash.unwrap());

                        //println!("{}", scope_name);
                    }
                    DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type => {
                        in_function = false;
                        total_type_full_name_len += scope_name.len();
                    }
                    // Exclude parameters on function declarations. This doesn't exclude parameters and variables in duplicate template function instantiations; we should probably add another deduplication pre-pass for that.
                    DW_TAG_variable | DW_TAG_formal_parameter if in_function || entry.tag() == DW_TAG_variable => {
                        if stack.last().map_or(true, |x| x.0 != DW_TAG_subroutine_type) {
                            // Note that params may repeat between declaration and definition (e.g. if they have different name, or maybe always), so we're double-counting them.

                            vars += 1;
                            if let Some(n) = &regular_name {
                                total_var_name_len += n.len();
                            }

                            if num_locations == 0 {
                                vars_without_loc += 1;
                            } else {
                                locs += num_locations;
                            }
                        }
                    }
                    DW_TAG_member => {
                        fields += 1;
                        if let Some(n) = &regular_name {
                            total_var_name_len += n.len();
                        }
                    }
                    DW_TAG_inlined_subroutine => {
                        inlined_funcs += 1;
                        inlined_func_ranges += num_ranges;
                        if num_ranges == 0 {
                            inlined_funcs_without_ranges += 1;
                        }
                    }
                    DW_TAG_template_type_parameter | DW_TAG_template_value_parameter | DW_TAG_GNU_template_parameter_pack | DW_TAG_GNU_template_template_param => {
                        if let Some(&(tag, _, false, _)) = stack.last() {
                            in_template = true;
                            stack.last_mut().unwrap().2 = true;
                            match tag {
                                DW_TAG_subprogram if in_function => template_funcs += 1,
                                DW_TAG_structure_type | DW_TAG_class_type | DW_TAG_union_type | DW_TAG_enumeration_type => template_types += 1,
                                _ => (),
                            }
                        }
                    }
                    _ => ()
                }

                stack.push((entry.tag(), scope_name.len(), in_template, in_function));
                depth += 1;
            }
        }

        func_ranges_hashes.sort_unstable();
        assert!(func_ranges_hashes.len() == funcs);
        func_ranges_hashes.dedup();
        let deduped_funcs = func_ranges_hashes.len();

        type_declarations.sort_unstable();
        type_definitions.sort_unstable();
        let duplicate_type_declarations = type_declarations.len();
        let duplicate_type_definitions = type_definitions.len();
        type_declarations.dedup();
        type_definitions.dedup();
        let duplicate_type_declarations = duplicate_type_declarations - type_declarations.len();
        let duplicate_type_definitions = duplicate_type_definitions - type_definitions.len();
        let mut undefined_types = 0usize;
        let mut undeclared_types = 0usize;
        {
            let mut i = 0usize;
            let mut j = 0usize;
            while i < type_declarations.len() || j < type_definitions.len() {
                if i < type_declarations.len() && j < type_definitions.len() && type_declarations[i] == type_definitions[j] {
                    i += 1;
                    j += 1;
                } else if i < type_declarations.len() && (j == type_definitions.len() || type_declarations[i] < type_definitions[j]) {
                    i += 1;
                    undefined_types += 1;
                } else {
                    j += 1;
                    undeclared_types += 1;
                }
            }
        }

        drop(prof);
        dbg!(dies);
        dbg!(attrs);
        dbg!(funcs_including_decls_and_inline);
        dbg!(funcs);
        dbg!(deduped_funcs);
        dbg!(template_funcs as f64 / funcs as f64);
        dbg!(funcs_without_name as f64 / funcs as f64);
        dbg!(funcs_without_link_name as f64 / funcs as f64);
        dbg!(funcs_with_multiple_ranges);
        dbg!(func_ranges);
        dbg!(vars);
        dbg!(locs);
        dbg!(fields);
        dbg!(inlined_funcs);
        dbg!(inlined_func_ranges);
        dbg!(inlined_funcs_without_ranges);
        dbg!(total_func_link_name_len);
        dbg!(total_func_full_name_len);
        dbg!(total_func_local_name_len);
        dbg!(total_var_name_len);
        dbg!(total_type_full_name_len);
        dbg!(longest_origin_chain);
        dbg!(vars_without_loc);
        dbg!(func_instr_len);
        eprintln!("");
        dbg!(types);
        dbg!(base_types);
        dbg!(derivative_types);
        dbg!(composite_types);
        dbg!(template_types as f64 / types as f64);
        dbg!(cross_unit_type_refs);
        dbg!(forward_type_refs);
        dbg!(forward_base_type_refs);
        dbg!(forward_derivative_type_refs);
        dbg!(forward_composite_type_refs);
        dbg!(base_type_refs);
        dbg!(derivative_type_refs);
        dbg!(composite_type_refs);
        dbg!(ptr_to_member_type_refs);
        dbg!(subrange_type_refs);
        dbg!(unrecognized_type_refs);
        dbg!(type_refs);
        dbg!(type_refs_with_specification);
        dbg!(type_refs_with_abstract_origin);
        dbg!(data_member_loc_simple);
        dbg!(data_member_loc_expr);
        dbg!(nontrivial_inheritance_loc);
        dbg!(data_member_loc_list);
        dbg!(type_definitions_in_different_unit);
        dbg!(type_declarations.len());
        dbg!(type_definitions.len());
        dbg!(duplicate_type_declarations);
        dbg!(duplicate_type_definitions);
        dbg!(undefined_types);
        dbg!(undeclared_types);
        eprintln!("-----");
    }

    if false { // change to skip/unskip .debug_line
        let prof = ProfileScope::new(".debug_line".to_string());

        let mut num_empty_sequences = 0usize; // either no rows or empty address range
        let mut num_sequences_starting_at_zero = 0usize;
        let mut max_address_in_sequences_starting_at_zero = 0usize;
        let mut num_sequences = 0usize; // not empty and not starting at zero
        let mut num_all_rows = 0usize; // including bad sequences
        let mut num_rows = 0usize;
        let mut num_include_directories = 0usize;
        let mut num_files = 0usize;
        let mut kinda_num_statements = 0usize; // the "kinda" ones include rows in bad sequences, so should be compared to num_all_rows rather than num_rows
        let mut kinda_num_basic_blocks = 0usize;
        let mut kinda_num_prologues = 0usize;
        let mut kinda_num_epilogues = 0usize;
        let mut num_programs_where_files_have_md5 = 0usize;
        let mut addr_bytes_covered = 0usize;
        let mut total_program_header_bytes = 0usize;
        let mut total_program_bytes = 0usize;
        let mut max_program_header_bytes = 0usize;
        let mut max_program_bytes = 0usize;
        let mut max_include_directories = 0usize;
        let mut max_files = 0usize;
        let mut max_rows_per_program = 0usize;
        let mut max_rows_per_sequence = 0usize;
        let mut max_line = 0usize;
        let mut max_column = 0usize;

        struct Row {
            addr: usize,
            line: usize,
        }

        #[derive(Clone)]
        struct Sequence {
            start: usize,
            end: usize,
            program_offset: u64,
            // Index in `rows` vec.
            rows_start: u32,
            rows_len: u32,
        }

        let mut seqs: Vec<Sequence> = Vec::new();
        let mut rows: Vec<Row> = Vec::new();
        let mut file_to_idx: HashMap<(&'static [u8], &'static [u8]), usize> = HashMap::new();
        let mut printed_examples = 0usize;

        for (program, unit) in &line_programs {
            let header = program.header();
            let program_offset = header.offset();

            num_include_directories += header.include_directories().len();
            num_programs_where_files_have_md5 += header.file_has_md5() as usize;
            num_files += header.file_names().len();
            total_program_header_bytes += header.header_length();
            total_program_bytes += header.raw_program_buf().len();
            max_program_header_bytes = max_program_header_bytes.max(header.header_length());
            max_include_directories = max_include_directories.max(header.include_directories().len());
            max_files = max_files.max(header.file_names().len());
            max_program_bytes = max_program_bytes.max(header.raw_program_buf().len());

            for file in header.file_names() {
                let d = match file.directory(header) {
                    None => &[],
                    Some(d) => dwarf.attr_string(&unit, d)?.slice(),
                };
                let f = dwarf.attr_string(&unit, file.path_name())?;
                file_to_idx.entry((d, f.slice())).or_insert(0);
            }

            let mut rows_iter = program.clone().rows();
            let header_ptr = rows_iter.header() as *const _;
            let mut rows_in_sequence = 0usize;
            let mut rows_in_program = 0usize;
            let mut sequences_in_program = 0usize;
            let mut sequence_start_addr: Option<u64> = None;
            while let Some((row_header, row)) = rows_iter.next_row()? {
                // Idk why iterator returns the program header with each row, it's just always the same header as rows_iter.header().
                if header_ptr != row_header as *const _ {
                    eprintln!("unexpected header returned from LineRows::next_row()");
                }

                if sequence_start_addr.unwrap_or(row.address()) != 0 {
                    all_anchors.push(row.address() as usize);
                }
                
                // Not a real row.
                if row.end_sequence() {
                    sequences_in_program += 1;
                    if rows_in_sequence == 0 {
                        eprintln!("sequence with no rows in line program @{}", program_offset.0);
                        num_empty_sequences += 1;
                    } else {
                        let start = sequence_start_addr.unwrap() as usize;
                        let end = row.address() as usize;
                        if start == 0 {
                            // These occur in the wild. They're usually normal-looking sequences, somewhere in the middle of a program, surrounded by good sequences, with instructions that look similar to other sequences.
                            // The only difference is that DW_LNE_set_address instruction sets address to 0 instead of a reasonable value.
                            // I'm guessing that 0 was supposed to have a relocation to be fixed up by the linker, but at some point the relocation was lost. Idk if it's a compiler bug or intentional.
                            // In debug builds, ~70% of all sequences are like this.

                            num_sequences_starting_at_zero += 1;
                            max_address_in_sequences_starting_at_zero = max_address_in_sequences_starting_at_zero.max(end);

                            #[allow(unused_comparisons)]
                            if printed_examples < 0 { // change to print examples of bad sequences
                                printed_examples += 1;
                                eprintln!("example program @{} producing sequence starting at 0 (sequence number {}, {} rows)", program_offset.0, sequences_in_program, rows_in_sequence);
                                let mut instr_iter = header.instructions();
                                while let Some(instr) = instr_iter.next_instruction(header)? {
                                    eprintln!("  {}", instr);
                                }
                            }
                        } else if end < start {
                            eprintln!("inverted sequence: {} < {}", end, start);
                        } else if end == start {
                            num_empty_sequences += 1;
                        } else {
                            num_sequences += 1;
                            rows_in_program += rows_in_sequence;
                            max_rows_per_sequence = max_rows_per_sequence.max(rows_in_sequence);
                            addr_bytes_covered += (row.address() - sequence_start_addr.unwrap()) as usize;
                            seqs.push(Sequence {start: sequence_start_addr.unwrap() as usize, end: row.address() as usize, program_offset: program_offset.0 as u64, rows_start: (rows.len() - rows_in_sequence).try_into().unwrap(), rows_len: rows_in_sequence.try_into().unwrap()});
                        }

                        rows_in_sequence = 0;
                        sequence_start_addr = None;
                    }
                    continue;
                }

                let file_index = row.file_index();
                let file_index = if header.encoding().version <= 4 {
                    if file_index == 0 {
                        eprintln!("file_index = 0 (DWARF <= 4) in line number program (address {})", row.address());
                        file_index
                    } else {
                        file_index - 1
                    }
                } else {
                    file_index
                };
                if file_index as usize >= header.file_names().len() {
                    eprintln!("file_index out of bounds: {} >= {}; DWARF {}, address {}", file_index, header.file_names().len(), header.encoding().version, row.address());
                }

                if sequence_start_addr.is_none() {
                    sequence_start_addr = Some(row.address());
                }
                let line: usize = row.line().map_or(0usize, |x| u64::from(x) as usize);
                let column: usize = match row.column() {
                    ColumnType::LeftEdge => 0,
                    ColumnType::Column(c) => u64::from(c) as usize,
                };
                rows.push(Row {addr: row.address() as usize, line: line});
                num_all_rows += 1;
                rows_in_sequence += 1;
                kinda_num_statements += row.is_stmt() as usize;
                kinda_num_basic_blocks += row.basic_block() as usize;
                kinda_num_prologues += row.prologue_end() as usize;
                kinda_num_epilogues += row.epilogue_begin() as usize;
                max_line = max_line.max(line);
                max_column = max_column.max(column);
            }
            if rows_in_sequence != 0 {
                eprintln!("unterminated sequence in line program @{}", program_offset.0);
            }
            num_rows += rows_in_program;
            max_rows_per_program = max_rows_per_program.max(rows_in_program);
        }

        seqs.sort_by_key(|s| (s.start, !s.end));
        let mut duplicate_sequences = 0usize;
        let mut overlapping_sequences = 0usize; // if this is nonzero, the other stats don't make sense
        let mut deduped_addr_bytes_covered = 0usize;
        let mut printed_examples = 0usize;
        let mut prev = Sequence {start: 0, end: 0, program_offset: 0, rows_start: 0, rows_len: 0};
        for seq in &seqs {
            if (seq.start, seq.end) == (prev.start, prev.end) {
                duplicate_sequences += 1;
                continue;
            }
            if seq.start < prev.end {
                overlapping_sequences += 1;

                #[allow(unused_comparisons)]
                if printed_examples < 10 { // change to print examples
                    printed_examples += 1;
                    eprintln!("example of overlapping sequences:");
                    eprintln!("  {}-{}, {} rows in program @{}", seq.start, seq.end, seq.rows_len, seq.program_offset);
                    for idx in 0..seq.rows_len as usize {
                        let row = &rows[seq.rows_start as usize + idx];
                        eprintln!("    {} line {}", row.addr, row.line);
                    }
                    eprintln!("  vs");
                    eprintln!("  {}-{}, {} rows in program @{}", prev.start, prev.end, prev.rows_len, prev.program_offset);
                    for idx in 0..prev.rows_len as usize {
                        let row = &rows[prev.rows_start as usize + idx];
                        eprintln!("    {} line {}", row.addr, row.line);
                    }
                }
                continue;
            }
            if seq.start < prev.end {
                overlapping_sequences += 1;
                continue;
            }
            deduped_addr_bytes_covered += seq.end - seq.start;
            prev = seq.clone();
        }

        drop(prof);
        dbg!(num_empty_sequences);
        dbg!(num_sequences_starting_at_zero);
        dbg!(max_address_in_sequences_starting_at_zero);
        dbg!(num_sequences);
        dbg!(num_all_rows);
        dbg!(num_rows);
        dbg!(num_include_directories);
        dbg!(num_files);
        dbg!(file_to_idx.len());
        dbg!(kinda_num_statements);
        dbg!(kinda_num_basic_blocks);
        dbg!(kinda_num_prologues);
        dbg!(kinda_num_epilogues);
        dbg!(num_programs_where_files_have_md5);
        dbg!(addr_bytes_covered);
        dbg!(total_program_header_bytes);
        dbg!(total_program_bytes);
        dbg!(max_program_header_bytes);
        dbg!(max_program_bytes);
        dbg!(max_include_directories);
        dbg!(max_files);
        dbg!(max_rows_per_program);
        dbg!(max_rows_per_sequence);
        dbg!(duplicate_sequences);
        dbg!(overlapping_sequences);
        dbg!(deduped_addr_bytes_covered);
        dbg!(mem::size_of::<LineRow>());
        dbg!(max_line);
        dbg!(max_column);
    }

    {
        let _prof = ProfileScope::new("sorting anchors".to_string());
        dbg!(all_anchors.len());
        all_anchors.sort_unstable();
        all_anchors.dedup();
        dbg!(all_anchors.len());

        let text_section = &elf.sections[*elf.section_by_name.get(".text").unwrap()];
        let end = text_section.address + text_section.size;
        let mut prev = text_section.address;
        let mut longest_run_without_anchors = 0usize;
        for a in all_anchors {
            if a >= end {
                break;
            }
            if a < prev {
                continue;
            }
            longest_run_without_anchors = longest_run_without_anchors.max(a - prev);
            prev = a;
        }
        longest_run_without_anchors = longest_run_without_anchors.max(end - prev);
        dbg!(longest_run_without_anchors);
    }

    Ok(())
}
