use crate::{*, error::{*, Result, Error}, log::*};
use gimli::{*, constants::DwForm};
use std::{str, borrow::Cow, fmt, sync::Mutex, collections::HashSet, mem, ptr};

// DWARF parsing.
// Currently we use some things from gimli, while most hot paths are reimplemented in a different way for speed (and incidentally some convenience). We should probably get rid of gimli altogether, for simplicity, compilation speed, and executable size.
//
// Approach to parsing attributes: we declare a struct for each tag we're interested in, pre-register information about layouts of these structs,
// then pre-process abbreviations to produce a compact plan for parsing attributes and storing results in the struct. This should be faster than gimli's approach because:
//  * attribute forms are checked only once for each abbreviation instead of once for each DIE,
//  * there's no two-step data transformation where we first do a big `match` to parse the attribute into an enum, then do another `match` on that enum.
//
// Use macro dwarf_struct!() to declare the struct and describe which attributes and forms to expect.

// We currently don't support .debug_types section. If we were to support it, we would pack section id and offset into one 8-byte value and use that as DieOffset (just like UnitSectionOffset, but 8 bytes instead of 16).
pub type DieOffset = DebugInfoOffset<usize>;

#[derive(Clone, Copy, Default)]
pub struct DwarfSlice {
    pub slice: &'static [u8], // opt out of the borrow checker for this, for now
}

pub struct SliceReader {
    pub slice: DwarfSlice, // TODO: after all gimli parsing code is replaced, unwrap this to &'static [u8]
    // TODO: pub unexpected_eof: bool, add it to check(), audit everything
}
impl SliceReader {
    pub fn new(slice: DwarfSlice) -> Self { Self {slice} }
    pub fn check(&self) -> Result<()> { Ok(()) } //if self.unexpected_eof {err!(Dwarf, "unexpected end of section")} else {Ok(())} }

    pub fn read_attribute(&mut self, spec: AttributeSpecification, encoding: Encoding) -> gimli::Result<AttributeValue<DwarfSlice>> {
        self.slice.read_attribute(spec, encoding)
    }
    pub fn skip_attribute(&mut self, spec: AttributeSpecification, encoding: Encoding) -> gimli::Result<()> {
        self.slice.skip_attribute(spec, encoding)
    }
    pub fn skip_attributes(&mut self, specs: &[AttributeSpecification], encoding: Encoding) -> gimli::Result<()> {
        self.slice.skip_attributes(specs, encoding)
    }

    pub unsafe fn read_attributes(&mut self, abbreviation: &Abbreviation, secondary: bool, context: &AttributeContext, out: *mut u8) -> Result<()> {
        let actions = &abbreviation.actions[secondary as usize];
        let flags_ptr = out.add(actions.flags_offset as usize) as *mut u32;
        *flags_ptr = actions.flags_value;
        for mut action in actions.actions.iter().copied() {
            if action.form == DW_FORM_indirect {
                let mut form = action.form;
                while form == constants::DW_FORM_indirect {
                    form = constants::DwForm(self.slice.read_uleb128_u16()?);
                }
                let implicit_const_value = if form == DW_FORM_implicit_const {
                    self.slice.read_sleb128()? as usize
                } else {
                    0
                };
                // Do the checks+dispatch that we normally do when preprocessing abbreviations.
                let layout_idx = action.param >> 16;
                let name = DwAt((action.param & 0xffff) as u16);
                let mut flags = *flags_ptr;
                let layout = &context.shared.layouts.layouts[layout_idx].1;
                action = prepare_attribute_action(AttributeSpecification {name, form, implicit_const_value}, layout, context.unit.header.encoding(), &context.shared.warnings, &mut flags)?;
                *flags_ptr = flags;
            }

            let out_field = out.add(action.field_offset as usize);
            // I guess this is not UB? I just want to emit a `mov` instruction, is it too much to ask.
            //asdqwe check that this compiles to a simple mov and is inlined
            let set_usize = |x: usize| ptr::copy_nonoverlapping(&x as *const usize as *const u8, out_field, 8);
            let set_slice = |x: &'static [u8], check_utf8: bool| {
                if check_utf8 && std::str::from_utf8(x).is_err() {
                    if context.shared.warnings.warn(DW_AT_null, DW_FORM_string) {
                        eprintln!("warning: invalid UTF8 in string attribute: {:?}", &x[..x.len().min(500)]);
                    }
                    return;
                }
                ptr::copy_nonoverlapping(&x as *const &'static [u8] as *const u8, out_field, 16);
            };

            let set_strx = |index: usize, context: &AttributeContext| -> Result<()> {
                let offset = context.dwarf.string_offset(context.unit, DebugStrOffsetsIndex(index))?;
                let s = context.dwarf.string(offset)?;
                set_slice(s.slice, action.param != 0);
                Ok(())
            };
            let set_addrx = |index: usize, context: &AttributeContext| -> Result<()> {
                let addr = context.dwarf.address(context.unit, DebugAddrIndex(index))? as usize;
                set_usize(addr);
                Ok(())
            };
            let set_unit_offset = |mut offset: usize, context: &AttributeContext| {
                if action.param != 0 {
                    // Convert to global offset.
                    offset = UnitOffset(offset).to_debug_info_offset(&context.unit.header).unwrap().0; // (panics if `unit` is a type unit instead of compile unit)
                }
                set_usize(offset);
            };

            match action.form {
                DW_FORM_addr => set_usize(if context.unit.header.encoding().address_size == 8 {self.slice.read_u64()? as usize} else {self.slice.read_u32()? as usize}),

                DW_FORM_block1 => {
                    let len = self.slice.read_u8()? as usize;
                    set_slice(self.slice.read_bytes(len)?, false);
                }
                DW_FORM_block2 => {
                    let len = self.slice.read_u16()? as usize;
                    set_slice(self.slice.read_bytes(len)?, false);
                }
                DW_FORM_block4 => {
                    let len = self.slice.read_u32()? as usize;
                    set_slice(self.slice.read_bytes(len)?, false);
                }
                DW_FORM_block | DW_FORM_exprloc => {
                    let len = self.slice.read_uleb128()? as usize;
                    set_slice(self.slice.read_bytes(len)?, false);
                }
                DW_FORM_data16 => set_slice(self.slice.read_bytes(16)?, false),

                DW_FORM_data1 => set_usize(self.slice.read_u8()? as usize),
                DW_FORM_data2 => set_usize(self.slice.read_u16()? as usize),
                DW_FORM_data4 => set_usize(self.slice.read_u32()? as usize),
                DW_FORM_data8 => set_usize(self.slice.read_u64()? as usize),
                DW_FORM_sdata => set_usize(self.slice.read_sleb128()? as usize),
                DW_FORM_udata => set_usize(self.slice.read_uleb128()? as usize),

                DW_FORM_string => set_slice(self.slice.read_null_terminated_slice()?.slice, action.param != 0),
                DW_FORM_strp => {
                    let offset = self.slice.read_offset(context.unit.header.encoding().format)?;
                    let s = context.dwarf.string(DebugStrOffset(offset))?;
                    set_slice(s.slice, action.param != 0);
                }
                DW_FORM_strp_sup => {
                    let offset = self.slice.read_offset(context.unit.header.encoding().format)?;
                    let s = context.dwarf.sup_string(DebugStrOffset(offset))?;
                    set_slice(s.slice, action.param != 0);
                }
                DW_FORM_line_strp => {
                    let offset = self.slice.read_offset(context.unit.header.encoding().format)?;
                    let s = context.dwarf.line_string(DebugLineStrOffset(offset))?;
                    set_slice(s.slice, action.param != 0);
                }
                DW_FORM_strx => set_strx(self.slice.read_uleb128()? as usize, context)?,
                DW_FORM_strx1 => set_strx(self.slice.read_u8()? as usize, context)?,
                DW_FORM_strx2 => set_strx(self.slice.read_u16()? as usize, context)?,
                DW_FORM_strx3 => set_strx(self.slice.read_u24()? as usize, context)?,
                DW_FORM_strx4 => set_strx(self.slice.read_u32()? as usize, context)?,

                DW_FORM_flag => {
                    let v = self.slice.read_u8()?;
                    if v > 1 {
                        return err!(Dwarf, "invalid flag value: {}", v);
                    }
                    *(out_field as *mut bool) = v != 0;
                }
                DW_FORM_flag_present => (),

                DW_FORM_ref1 => set_unit_offset(self.slice.read_u8()? as usize, context),
                DW_FORM_ref2 => set_unit_offset(self.slice.read_u16()? as usize, context),
                DW_FORM_ref4 => set_unit_offset(self.slice.read_u32()? as usize, context),
                DW_FORM_ref8 => set_unit_offset(self.slice.read_u64()? as usize, context),
                DW_FORM_ref_udata => set_unit_offset(self.slice.read_uleb128()? as usize, context),

                DW_FORM_ref_addr | DW_FORM_sec_offset => set_usize(self.slice.read_offset(context.unit.header.encoding().format)?),

                DW_FORM_addrx => set_addrx(self.slice.read_uleb128()? as usize, context)?,
                DW_FORM_addrx1 => set_addrx(self.slice.read_u8()? as usize, context)?,
                DW_FORM_addrx2 => set_addrx(self.slice.read_u16()? as usize, context)?,
                DW_FORM_addrx3 => set_addrx(self.slice.read_u24()? as usize, context)?,
                DW_FORM_addrx4 => set_addrx(self.slice.read_u32()? as usize, context)?,

                DW_FORM_implicit_const => set_usize(action.param),

                DW_FORM_loclistx => {
                    let index = self.slice.read_uleb128()? as usize;
                    let offset = context.dwarf.locations_offset(context.unit, DebugLocListsIndex(index))?.0;
                    set_usize(offset);
                }
                DW_FORM_rnglistx => {
                    let index = self.slice.read_uleb128()? as usize;
                    let offset = context.dwarf.ranges_offset(context.unit, DebugRngListsIndex(index))?.0;
                    set_usize(offset);
                }

                DW_EXTRA_FORM_skip_bytes => self.slice.skip(action.param)?,
                DW_EXTRA_FORM_skip_leb128 => self.slice.skip_leb128()?,
                DW_EXTRA_FORM_skip_block => {
                    let len = self.slice.read_uleb128()? as usize;
                    self.slice.skip(len)?;
                }
                DW_EXTRA_FORM_skip_block1 => {
                    let len = self.slice.read_u8()? as usize;
                    self.slice.skip(len)?;
                }
                DW_EXTRA_FORM_skip_block2 => {
                    let len = self.slice.read_u16()? as usize;
                    self.slice.skip(len)?;
                }
                DW_EXTRA_FORM_skip_block4 => {
                    let len = self.slice.read_u32()? as usize;
                    self.slice.skip(len)?;
                }
                DW_EXTRA_FORM_skip_string => {self.slice.read_null_terminated_slice()?;}

                // These are not supported:
                // DW_FORM_ref_sig8 => ,
                // DW_FORM_ref_sup4 => ,
                // DW_FORM_ref_sup8 => ,
                // DW_FORM_GNU_ref_alt => ,

                _ => panic!("unexpected attribute form in prepared action"), // (we already validated action.form in prepare_attribute_action())
            }
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct AttributeSpecification {
    pub name: DwAt,
    pub form: DwForm,
    pub implicit_const_value: usize,
}
impl AttributeSpecification {
    // TODO: Remove these.
    #[inline]
    pub fn name(&self) -> DwAt { self.name }
    #[inline]
    pub fn form(&self) -> DwForm { self.form }
}

#[derive(Clone)]
pub struct Abbreviation {
    pub code: usize,
    pub tag: DwTag,
    pub has_children: bool,
    pub attributes: Vec<AttributeSpecification>, // TODO: remove

    // 0 - normal tag-specific struct layout.
    // 1 - common struct layout for chasing abstract_origin/specification pointers without looking at the tag.
    actions: [AbbreviationActions; 2],
}
impl Abbreviation {
    // TODO: Remove these.
    #[inline]
    pub fn tag(&self) -> DwTag { self.tag }
    #[inline]
    pub fn attributes(&self) -> &[AttributeSpecification] { &self.attributes }
}

#[derive(Clone)]
pub struct AbbreviationSet {
    pub vec: Vec<Abbreviation>,
    pub consecutive: bool,
}
impl AbbreviationSet {
    fn new() -> Self { Self {vec: Vec::new(), consecutive: true} }

    fn get(&self, code: usize) -> Result<&Abbreviation> {
        if self.consecutive {
            if code < self.vec.len() {
                Ok(&self.vec[code])
            } else {
                err!(Dwarf, "abbvreviation code out of bounds: {} >= {}", code, self.vec.len())
            }
        } else {
            let i = self.vec.partition_point(|a| a.code < code);
            if i == self.vec.len() || self.vec[i].code > code {
                err!(Dwarf, "abbreviation code not found: {}", code)
            } else {
                Ok(&self.vec[i])
            }
        }
    }
}

#[derive(Clone, Copy)]
struct AttributeAction {
    form: DwForm,
    field_offset: u32,
    // For string forms: 1 to check utf8 validity, 0 to leave a &[u8].
    // For DW_FORM_ref*: 1 to convert to DebugInfoOffset, 0 to keep UnitOffset.
    // For DW_FORM_implicit_const: the constant value.
    // For DW_EXTRA_FORM_skip_bytes: how many bytes to skip.
    // For DW_FORM_indirect: packed layout_idx and DwAt.
    param: usize,
}

#[derive(Default, Clone)]
struct AbbreviationActions {
    flags_offset: u32,
    flags_value: u32,
    actions: Vec<AttributeAction>, // TODO: try replacing this with Range<u32> of indices into a shared array
}

#[derive(Default)]
pub struct AttributeStructLayout {
    pub flags_offset: u32,
    pub fields: Vec<(/*offset*/ u32, DwAt, AttributeType)>,

    pub num_fields_included_in_flags: u32,
}
impl AttributeStructLayout {
    // Expand special attribute groups into individual attributes.
    fn preprocess(&mut self) {
        assert!(self.fields.len() <= 26);
        self.num_fields_included_in_flags = self.fields.len() as u32;
        let (mut saw_ranges, mut saw_location, mut saw_specification) = (false, false, false);
        for i in 0..self.num_fields_included_in_flags as usize {
            let &(offset, attr_name, attr_type) = &self.fields[i];
            match attr_type {
                AttributeType::Ranges => {
                    assert!(!mem::replace(&mut saw_ranges, true));
                    self.fields[i] = (offset + offsetof!(DwarfRanges, ranges) as u32, DW_AT_ranges, AttributeType::Ranges);
                    self.fields.push((offset + offsetof!(DwarfRanges, low_pc) as u32, DW_AT_low_pc, AttributeType::Ranges));
                    self.fields.push((offset + offsetof!(DwarfRanges, high_pc) as u32, DW_AT_high_pc, AttributeType::Ranges));
                }
                AttributeType::CodeLocation => {
                    assert!(!mem::replace(&mut saw_location, true));
                    let attrs = match attr_name {
                        DW_AT_decl_file => [DW_AT_decl_file, DW_AT_decl_line, DW_AT_decl_column],
                        DW_AT_call_file => [DW_AT_call_file, DW_AT_call_line, DW_AT_call_column],
                        _ => panic!("unexpected attribute name for CodeLocation field: {}", attr_name),
                    };
                    self.fields[i] = (offset + offsetof!(DwarfCodeLocation, file) as u32, attrs[0], AttributeType::Unsigned);
                    self.fields.push((offset + offsetof!(DwarfCodeLocation, line) as u32, attrs[1], AttributeType::Unsigned));
                    self.fields.push((offset + offsetof!(DwarfCodeLocation, column) as u32, attrs[2], AttributeType::Unsigned));
                }
                AttributeType::SpecificationOrAbstractOrigin => {
                    assert!(!mem::replace(&mut saw_specification, true));
                    let other_attr = match attr_name {
                        DW_AT_specification => DW_AT_abstract_origin,
                        DW_AT_abstract_origin => DW_AT_specification,
                        _ => panic!("unexpected attribute for SpecificationOrAbstractOrigin field: {}", attr_name),
                    };
                    self.fields.push((offset, other_attr, AttributeType::SpecificationOrAbstractOrigin));
                }
                _ => (),
            }
        }
    }
}

#[derive(Default)]
pub struct AllAttributeStructLayouts {
    layouts: Vec<(DwTag, AttributeStructLayout)>,
    num_primary_layouts: usize, // layouts[0..num_primary_layouts] correspond to tags and are sorted by tag
    secondary_layout_idx: usize, // layouts[secondary_layout_idx] is the alternative layout to use for all tags in secondary_tags
    unit_layout_idx: usize, // layouts[unit_layout_idx] is the alternative layout to use for DW_TAG_compile_unit
    secondary_tags: Vec<DwTag>,
}
impl AllAttributeStructLayouts {
    pub fn new(mut layouts: Vec<(DwTag, AttributeStructLayout)>, secondary_layout: AttributeStructLayout, mut secondary_tags: Vec<DwTag>) -> Self {
        layouts.sort_unstable_by_key(|(t, _)| *t);
        let num_primary_layouts = layouts.len();
        let secondary_layout_idx = layouts.len();
        layouts.push((DW_TAG_null, secondary_layout));
        let unit_layout_idx = layouts.len();
        layouts.push((DW_TAG_null, UnitPrepassAttributes::layout()));
        for (_, l) in &mut layouts {
            l.preprocess();
        }
        secondary_tags.sort_unstable();
        Self {layouts, num_primary_layouts, secondary_layout_idx, unit_layout_idx, secondary_tags}
    }
    
    fn find(&self, tag: DwTag) -> Option<usize> {
        let i = self.layouts[..self.num_primary_layouts].partition_point(|(t, _)| *t < tag);
        if i < self.num_primary_layouts && self.layouts[i].0 == tag {
            Some(i)
        } else {
            None
        }
    }

    fn find_secondary(&self, tag: DwTag) -> Option<usize> {
        if tag == DW_TAG_compile_unit {
            Some(self.unit_layout_idx)
        } else if self.secondary_tags.contains(&tag) {
            Some(self.secondary_layout_idx)
        } else {
            None
        }
    }
}

#[derive(Default)]
struct FormWarningLimiter {
    warnings_printed: Mutex<HashSet<(DwAt, DwForm)>>,    
}
impl FormWarningLimiter {
    fn warn(&self, name: DwAt, form: DwForm) -> bool {
        // If this turns out to be slow, can use RW mutex with double-checked locking, since the set should be very small.
        let mut set = self.warnings_printed.lock().unwrap();
        set.insert((name, form))
    }
}

pub struct AbbreviationsSharedData {
    layouts: AllAttributeStructLayouts,
    warnings: FormWarningLimiter,
}

pub struct AttributeContext<'a> {
    // TODO: Try inlining frequently accessed fields of these structs here for speed.
    pub unit: &'a Unit<DwarfSlice>,
    pub dwarf: &'a Dwarf<DwarfSlice>,
    pub shared: &'a AbbreviationsSharedData,
}

// What to do with an attribute: which forms to expect, what output type to produce, what conversions to apply.
// Approximately maps to DWARF's "attribute class" (as in DWARF 5 spec).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AttributeType {
    // Type: &'static str.
    // Class: string.
    // Forms: DW_FORM_string, DW_FORM_strp, DW_FORM_line_strp, DW_FORM_strp_sup, DW_FORM_strx{,1,2,3,4}.
    String,
    // Type: &'static [u8].
    // Class: string, block; also, DW_FORM_data16.
    // Forms: DW_FORM_block{,1,2,4}, DW_FORM_data16, and all String forms.
    Slice,
    // Type: usize.
    // Class: constant, excluding data16, sdata, and implicit_const.
    // Forms: DW_FORM_data{1,2,4,8}, DW_FORM_udata.
    Unsigned,
    // Type: isize.
    // Class: constant, only sdata or implicit_const.
    // Forms: DW_FORM_sdata, DW_FORM_implicit_const.
    Signed, // isize
    // Type: usize. Signed value is bit-cast to unsigned.
    // Class: constant: excluding data16.
    // Forms: Unsigned + Signed.
    MaybeSigned,
    // Type: usize (UnitOffset).
    // Class: reference, only within-unit (i.e. not DW_FORM_ref_addr).
    // Forms: DW_FORM_ref{1,2,4,8,_udata}.
    UnitOffset,
    // Type: usize (DebugInfoOffset).
    // Class: reference.
    // Forms: DW_FORM_ref{1,2,4,8,_udata}, DW_FORM_ref_addr.
    // Unit offset is converted to .debug_info offset.
    DebugInfoOffset,
    // Type: usize.
    // Class: address.
    // Forms: DW_FORM_addr, DW_FORM_addrx{,1,2,3,4}, DW_FORM_GNU_addr_index.
    Address,
    // Type: Expression, aka &'static [u8].
    // Class: exprloc.
    // Forms: DW_FORM_exprloc.
    Expression,
    // Type: LocationListsOffset, aka usize.
    // Class: loclist.
    // Forms: DW_FORM_loclistx, DW_FORM_sec_offset.
    LocationListsOffset,
    // Type: bool.
    // Class: flag.
    // Forms: DW_FORM_flag, DW_FORM_flag_present.
    Flag,
    // Type: usize.
    // Class: lineptr, loclistsptr, etc.
    // Forms: DW_FORM_sec_offset.
    SectionOffset,

    // Groups of attributes that are special-cased and grouped into sub-structs for convenience.

    // Type: DwarfRanges.
    // Attributes: DW_AT_ranges (rnglist), DW_AT_low_pc (address), DW_AT_high_pc (address or constant).
    // DW_AT_ranges forms: DW_FORM_rnglistx, DW_FORM_sec_offset.
    // Upper bits of the `fields` bitmask will indicate which fields of DwarfRanges were set. See constants in DwarfRanges.
    Ranges,
    // Type: DwarfCodeLocation.
    // If declared with attribute name DW_AT_decl_file, matches attributes DW_AT_decl_file, DW_AT_decl_line, DW_AT_decl_column.
    // If declared with attribute name DW_AT_call_file, matches attributes DW_AT_call_file, DW_AT_call_line, DW_AT_call_column.
    // The `fields` bit is set iff DW_AT_decl_file/DW_AT_call_file is present.
    CodeLocation,
    // Type: DwarfReference, aka usize.
    // Attributes: DW_AT_specification, DW_AT_abstract_origin.
    // Class: reference.
    // Forms: DW_FORM_ref{1,2,4,8,_udata}, DW_FORM_ref_addr.
    // Normally at most one of these two attributes is present, so we merge them into one field.
    // If either of the attributes is present, bit DwarfReference::HAS_SPECIFICATION_OR_ABSTRACT_ORIGIN is set in `fields`.
    // The attribute may have offset relative to the current unit or relative to the whole .debug_info section. In the latter case, bit DwarfReference::GLOBAL is set in `fields`.
    SpecificationOrAbstractOrigin,
}

#[macro_export]
macro_rules! dwarf_field_mask_constants {
    (($counter:expr, {})) => {}; // base case

    (($counter:expr, { $field:ident, $($rest:ident,)* })) => { // recursive case
        pub const $field: u32 = 1 << ($counter);
        dwarf_field_mask_constants!(($counter + 1, { $($rest,)* }));
    };
}

// Example usage:
//asdqwe change to different example since ranges stuff got special-cased
//
// dwarf_struct!{ MyStruct {
//     range_lists: RangeListsOffset, DW_AT_ranges, RangeLists;
//     low_pc: usize, DW_AT_low_pc, Address;
//     high_pc_addr: usize, DW_AT_high_pc, Address;
//     high_pc_num: usize, DW_AT_high_pc, Unsigned;
// }}
//
//
// Expands into approximately:
//
// struct MyStruct {
//     range_lists: RangeListsOffset,
//     low_pc: usize,
//     high_pc_addr: usize,
//     high_pc_num: usize,
//
//     fields: u32, // bitmask saying which fields were assigned
// }
// impl MyStruct {
//     // Bits in the `fields` mask.
//     const range_lists: u32 = 1 << 0;
//     const low_pc: u32 = 1 << 1;
//     const high_pc_addr: u32 = 1 << 2;
//     const high_pc_num: u32 = 1 << 3;
//
//     fn layout() -> AttributeStructLayout {
//         AttributeStructLayout {fields: vec![
//             (offsetof!(Self, range_lists) as u32, DW_AT_ranges, AttributeType::RangeLists),
//             (offsetof!(Self, low_pc) as u32, DW_AT_low_pc, AttributeType::Address),
//             (offsetof!(Self, high_pc_addr) as u32, DW_AT_high_pc, AttributeType::Address),
//             (offsetof!(Self, high_pc_num) as u32, DW_AT_high_pc, AttributeType::Unsigned),
//         ]}
//     }
// }
//
//
// If multiple fields have the same attribute name, the first one that matches the form will be assigned.
// E.g. in the example above high_pc_addr will be assigned if the attribute has form DW_FORM_addr*, high_pc_num will be assigned if DW_FORM_data*/DW_FORM_udata.

#[macro_export]
macro_rules! dwarf_struct {
    ($name:ident {
        $($field:ident: $type:ty, $dw_at:ident, $attribute_type:ident;)*
    }) => {
        #[derive(Default)]
        pub struct $name {
            pub $($field: $type,)*

            pub fields: u32,
        }

        impl $name {
            dwarf_field_mask_constants!((0, {$($field,)*}));

            pub fn layout() -> AttributeStructLayout {
                AttributeStructLayout {
                    flags_offset: offsetof!(Self, fields) as u32,
                    fields: vec![$(
                        (
                            offsetof!(Self, $field) as u32,
                            gimli::constants::$dw_at,
                            dwarf::AttributeType::$attribute_type,
                        ),
                    )*],

                    num_fields_included_in_flags: 0,
                }
            }
        }
    };
}

// Special type that can be used in dwarf_struct!():
//   ranges: DwarfRanges, DW_AT_ranges, AttributeType::Ranges;
// Covers 3 attributes (even though only one is explicitly given to dwarf_struct!()): DW_AT_ranges, DW_AT_low_pc, DW_AT_high_pc.
#[derive(Default)]
pub struct DwarfRanges {
    ranges: usize, // offset in .debug_ranges or .debug_rnglists
    low_pc: usize,
    high_pc: usize,
}
impl DwarfRanges {
    // These bits would be set in the parent struct's `fields` bitmask.
    // (Make sure to not clash with the bit in DwarfReference. Also keep it in sync with the assert in AttributeStructLayout.preprocess())
    const RANGES: u32 = 1 << 26;
    const LOW_PC: u32 = 1 << 27;
    const HIGH_PC: u32 = 1 << 28;
    // Set if the DW_AT_high_pc had data form (i.e. it needs to be added to low_pc), unset if address form.
    const HIGH_PC_IS_RELATIVE: u32 = 1 << 29;
}

#[derive(Default, Clone, Copy)]
pub struct DwarfReference {
    offset: usize,
}
impl DwarfReference {
    // (Keep these from clashing with bits in DwarfRanges and in sync with assert in preprocess().)
    const HAS_SPECIFICATION_OR_ABSTRACT_ORIGIN: u32 = 1 << 30;
    const GLOBAL: u32 = 1 << 31;
}

#[derive(Default)]
pub struct DwarfCodeLocation {
    file: usize,
    line: usize,
    column: usize,
}

dwarf_struct!{ UnitPrepassAttributes {
    rnglists_base: usize, DW_AT_rnglists_base, SectionOffset;
    // TODO: All the other section offsets, etc. (Probably can't have name here because it may need section offset.)
}}

pub fn list_units(dwarf: &mut Dwarf<DwarfSlice>, binary_name: &str, layouts: AllAttributeStructLayouts) -> Result<(Vec<(Unit<DwarfSlice>, AbbreviationSet)>, AbbreviationsSharedData)> {
    let mut shared = AbbreviationsSharedData {layouts, warnings: FormWarningLimiter::default()};

    let mut unit_headers: Vec<(UnitHeader<DwarfSlice>, AbbreviationSet)> = Vec::new();
    let mut abbrev_offsets: Vec<((/*offset*/ usize, Encoding), /*unit_idx*/ usize)> = Vec::new();
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("listing units ({})", binary_name));
        let mut iter = dwarf.debug_info.units();
        while let Some(header) = iter.next()? {
            abbrev_offsets.push(((header.debug_abbrev_offset().0, header.encoding()), unit_headers.len()));
            unit_headers.push((header, AbbreviationSet::new()));
        }
    }
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("loading abbreviations ({})", binary_name));
        abbrev_offsets.sort_unstable_by_key(|((offset, encoding), _)| (*offset, encoding.address_size, encoding.format == Format::Dwarf64, encoding.version));
        let mut off_start = 0usize;
        let mut all_consecutive = true;
        while off_start < abbrev_offsets.len() {
            let &(offset, encoding) = &abbrev_offsets[off_start].0;
            let mut off_end = off_start + 1;
            while off_end < abbrev_offsets.len() && abbrev_offsets[off_end].0 == (offset, encoding) {
                off_end += 1;
            }

            let mut vec = vec![Abbreviation {code: 0, tag: DW_TAG_null, has_children: false, attributes: Vec::new(), actions: [Default::default(), Default::default()]}];
            let (mut consecutive, mut sorted) = (true, true);
            let mut prev_code = 0;

            let mut reader = SliceReader::new(dwarf.debug_abbrev.reader().clone());
            reader.slice.skip(offset)?;
            loop {
                let code = reader.slice.read_uleb128()? as usize;
                if code == 0 {
                    break;
                }
                let tag = reader.slice.read_uleb128_u16()?;
                if tag == 0 {
                    reader.check()?;
                    return err!(Dwarf, "abbreviation has tag 0");
                }
                let tag = DwTag(tag);
                let has_children = reader.slice.read_u8()?;
                if has_children > 1 {
                    return err!(Dwarf, "invalid has_children value in abbreviation");
                }
                
                let mut attributes: Vec<AttributeSpecification> = Vec::new();
                loop {
                    let name = reader.slice.read_uleb128_u16()?;
                    let form = reader.slice.read_uleb128_u16()?;
                    if (name == 0) != (form == 0) {
                        return err!(Dwarf, "invalid name/form pair in attribute specification");
                    }
                    if name == 0 {
                        break;
                    }
                    let implicit_const_value = if form == DW_FORM_implicit_const.0 {
                        reader.slice.read_sleb128()? as usize
                    } else {
                        0
                    };
                    attributes.push(AttributeSpecification {name: DwAt(name), form: DwForm(form), implicit_const_value});
                }

                let mut actions = [AbbreviationActions::default(), AbbreviationActions::default()];
                if let Some(layout_idx) = shared.layouts.find(tag) {
                    actions[0] = prepare_abbreviation_actions(&attributes, layout_idx, encoding, &mut shared)?;
                }
                if let Some(layout_idx) = shared.layouts.find_secondary(tag) {
                    actions[1] = prepare_abbreviation_actions(&attributes, layout_idx, encoding, &mut shared)?;
                }

                vec.push(Abbreviation {code, tag, has_children: has_children != 0, attributes, actions});

                if code != prev_code + 1 {
                    consecutive = false;
                    if code <= prev_code {
                        sorted = false;
                    }
                }
                prev_code = code;
            }

            if !sorted {
                vec.sort_unstable_by_key(|a| a.code);
                for i in 0..vec.len()-1 {
                    if vec[i].code == vec[i+1].code {
                        return err!(Dwarf, "duplicate abbreviation code: {}", vec[i].code);
                    }
                }
            }
            all_consecutive &= consecutive;
            let set = AbbreviationSet {vec, consecutive};
            for i in off_start+1..off_end {
                // Instead of copying the array here, we could use something like Arc (like gimli does). But in one executable I looked at,
                // almost all abbreviation sets were unique, so the deduplication is probably not worth the added indirection on lookup.
                // TODO: A more promising deduplication would be at individual abbreviation granularity: individual abbreviations are duplicated ~10x,
                //       so it may make sense to put unique abbreviations in an array and make AbbreviationSet have u32 indices instead of Abbreviation-s.
                //       It's an extra indirection on lookup, but the whole data structure becomes much smaller and more likely to fit in cache. May or may not be net faster, need to test.
                unit_headers[abbrev_offsets[i].1].1 = set.clone();
            }
            unit_headers[abbrev_offsets[off_start].1].1 = set;

            off_start = off_end;
        }

        if !all_consecutive {
            eprintln!("info: not all abbreviation codes are consecutive ({})", binary_name)
        }
    }
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("loading gimli abbreviations ({})", binary_name));
        dwarf.populate_abbreviations_cache(AbbreviationsCacheStrategy::All);
    }
    let mut units: Vec<(Unit<DwarfSlice>, AbbreviationSet)> = Vec::with_capacity(unit_headers.len());
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("preparing units ({})", binary_name));
        for (header, set) in unit_headers {
            let unit = dwarf.unit(header)?;
            units.push((unit, set));
        }
    }

    Ok((units, shared))
}

// Boilerplate adapter so that we can use gimli parsing code for some things.
impl gimli::Reader for DwarfSlice {
    type Endian = LittleEndian;
    type Offset = usize;
    fn endian(&self) -> Self::Endian {LittleEndian::default()}

    // Boring adapters.
    fn len(&self) -> usize {self.slice.len()}
    fn empty(&mut self) {self.slice = &self.slice[..0]}
    fn truncate(&mut self, len: usize) -> gimli::Result<()> {if len > self.slice.len() {Err(gimli::Error::UnexpectedEof(self.offset_id()))} else {self.slice = &self.slice[..len]; Ok(())}}
    fn offset_id(&self) -> ReaderOffsetId {ReaderOffsetId(self.slice.as_ptr() as u64)}
    fn lookup_offset_id(&self, id: ReaderOffsetId) -> Option<usize> {let ptr = self.slice.as_ptr() as u64; if id.0 >= ptr && id.0 <= ptr + self.slice.len() as u64 {Some((id.0 - ptr) as usize)} else {None}}
    fn skip(&mut self, len: usize) -> gimli::Result<()> {self.read_bytes(len)?; Ok(())}
    fn split(&mut self, len: usize) -> gimli::Result<Self> {Ok(Self::new(self.read_bytes(len)?))}
    fn to_slice(&self) -> gimli::Result<Cow<'_, [u8]>> {Ok(self.slice.into())}
    fn to_string_lossy(&self) -> gimli::Result<Cow<'_, str>> {Ok(String::from_utf8_lossy(self.slice))}
    fn to_string(&self) -> gimli::Result<Cow<'_, str>> {match str::from_utf8(self.slice) {Ok(s) => Ok(s.into()), _ => Err(gimli::Error::BadUtf8)}} // unused, no need to optimize
    fn read_slice(&mut self, buf: &mut [u8]) -> gimli::Result<()> {buf.copy_from_slice(self.read_bytes(buf.len())?); Ok(())}
    fn offset_from(&self, base: &Self) -> usize {
        let base_ptr = base.slice.as_ptr() as usize;
        let ptr = self.slice.as_ptr() as usize;
        debug_assert!(ptr >= base_ptr && ptr <= base_ptr + base.slice.len());
        ptr - base_ptr
    }

    // Functions that have slow default implementations in gimli and need to be optimized.

    fn find(&self, byte: u8) -> gimli::Result<usize> {
        // TODO: Make this fast with simd.
        for i in 0..self.slice.len() {
            if self.slice[i] == byte {
                return Ok(i);
            }
        }
        Err(gimli::Error::UnexpectedEof(self.offset_id()))
    }

    fn read_u8(&mut self) -> gimli::Result<u8> {Ok(self.read_bytes(1)?[0])}
    fn read_u16(&mut self) -> gimli::Result<u16> {Ok(u16::from_le_bytes(self.read_bytes(2)?.try_into().unwrap()))}
    fn read_u32(&mut self) -> gimli::Result<u32> {Ok(u32::from_le_bytes(self.read_bytes(4)?.try_into().unwrap()))}
    fn read_u64(&mut self) -> gimli::Result<u64> {Ok(u64::from_le_bytes(self.read_bytes(8)?.try_into().unwrap()))}

    fn read_i8(&mut self) -> gimli::Result<i8> {Ok(self.read_u8()? as i8)}
    fn read_i16(&mut self) -> gimli::Result<i16> {Ok(self.read_u16()? as i16)}
    fn read_i32(&mut self) -> gimli::Result<i32> {Ok(self.read_u32()? as i32)}
    fn read_i64(&mut self) -> gimli::Result<i64> {Ok(self.read_u64()? as i64)}
    fn read_f32(&mut self) -> gimli::Result<f32> {Ok(f32::from_bits(self.read_u32()?))}
    fn read_f64(&mut self) -> gimli::Result<f64> {Ok(f64::from_bits(self.read_u64()?))}

    fn skip_leb128(&mut self) -> gimli::Result<()> {
        // TODO: Optimize.
        while self.read_u8()? >= 128 {}
        Ok(())
    }

    fn read_uleb128(&mut self) -> gimli::Result<u64> {
        // TODO: Make it fast using simd and/or bit tricks. The input is padded, so it's safe to e.g. start by reading 8 bytes into a usize instead of checking for eof after every byte.
        let mut r = 0u64;
        let mut shift = 0u32;
        loop {
            if self.slice.is_empty() {
                return Err(gimli::Error::UnexpectedEof(self.offset_id()));
            }
            let b = self.slice[0];
            self.slice = &self.slice[1..];
            if shift == 63 && b > 1 {
                return Err(gimli::Error::BadUnsignedLeb128);
            }
            r |= ((b & 0x7f) as u64) << shift;
            if b < 128 {
                return Ok(r);
            }
            shift += 7;
        }
    }

    fn read_uleb128_u32(&mut self) -> gimli::Result<u32> {
        let r = self.read_uleb128()?;
        if r > u32::MAX as u64 {
            Err(gimli::Error::BadUnsignedLeb128)
        } else {
            Ok(r as u32)
        }
    }

    // TODO: fn read_uleb128_u16(&mut self) -> gimli::Result<u16>
    // TODO: fn read_sleb128(&mut self) -> gimli::Result<i64>
}

impl fmt::Debug for DwarfSlice {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(fmt, "DwarfSlice of length {}", self.slice.len())
    }
}

impl DwarfSlice {
    pub fn new(slice: &'static [u8]) -> DwarfSlice {
        DwarfSlice {slice}
    }

    pub fn slice(&self) -> &'static [u8] {self.slice}

    pub fn as_ptr(&self) -> *const u8 {self.slice.as_ptr()}

    #[inline]
    pub fn read_bytes(&mut self, len: usize) -> gimli::Result<&'static [u8]> {
        if len > self.slice.len() {
            Err(gimli::Error::UnexpectedEof(self.offset_id()))
        } else {
            let r = &self.slice[..len];
            self.slice = &self.slice[len..];
            Ok(r)
        }
    }

    pub fn read_u24(&mut self) -> gimli::Result<u32> {
        let mut a = [0u8; 4];
        a[..3].copy_from_slice(self.read_bytes(3)?);
        Ok(u32::from_le_bytes(a))
    }

    // Functions replacing gimli EntriesRaw.

    pub fn read_abbreviation<'ab>(&mut self, abbreviations: &'ab AbbreviationSet) -> Result<Option<&'ab Abbreviation>> {
        let code = self.read_uleb128()? as usize;
        if code == 0 {
            return Ok(None);
        }
        Ok(Some(abbreviations.get(code)?))
    }

    #[inline]
    fn chase_attribute_form(&mut self, mut form: DwForm) -> gimli::Result<DwForm> {
        while form == constants::DW_FORM_indirect {
            form = constants::DwForm(self.read_uleb128_u16()?);
        }
        Ok(form)
    }        

    // Doesn't do the conversions that gimli Attribute::value() does, so the caller needs to recognize some more forms sometimes.
    // Specifically, it should use offset_value() instead of expecting things like AttributeValue::RangeListsRef (idk why they exist,
    // attribute name already uniquely determines which section the offset points to; I guess it's just a paranoid redundant check to make bugs less likely?
    // doesn't seem worth the extra code and the overhead).
    pub fn read_attribute(&mut self, spec: AttributeSpecification, encoding: Encoding) -> gimli::Result<AttributeValue<DwarfSlice>> {
        let form = self.chase_attribute_form(spec.form())?;
        Ok(match form {
            constants::DW_FORM_addr => AttributeValue::Addr(self.read_address(encoding.address_size)?),
            constants::DW_FORM_block1 => AttributeValue::Block({let n = self.read_u8()? as usize; self.split(n)?}),
            constants::DW_FORM_block2 => AttributeValue::Block({let n = self.read_u16()? as usize; self.split(n)?}),
            constants::DW_FORM_block4 => AttributeValue::Block({let n = self.read_u32()? as usize; self.split(n)?}),
            constants::DW_FORM_block => AttributeValue::Block({let n = self.read_uleb128()? as usize; self.split(n)?}),
            constants::DW_FORM_data1 => AttributeValue::Data1(self.read_u8()?),
            constants::DW_FORM_data2 => AttributeValue::Data2(self.read_u16()?),
            // Note: this is missing logic for DWARF 2/3, see allow_section_offset() in gimli.
            constants::DW_FORM_data4 => AttributeValue::Data4(self.read_u32()?),
            constants::DW_FORM_data8 => AttributeValue::Data8(self.read_u64()?),
            constants::DW_FORM_data16 => AttributeValue::Block(self.split(16)?),
            constants::DW_FORM_udata => AttributeValue::Udata(self.read_uleb128()?),
            constants::DW_FORM_sdata => AttributeValue::Sdata(self.read_sleb128()?),
            constants::DW_FORM_exprloc => AttributeValue::Exprloc(Expression({let n = self.read_uleb128()? as usize; self.split(n)?})),
            constants::DW_FORM_flag => AttributeValue::Flag(self.read_u8()? != 0),
            constants::DW_FORM_flag_present => AttributeValue::Flag(true),
            constants::DW_FORM_sec_offset => AttributeValue::SecOffset(self.read_offset(encoding.format)?),
            constants::DW_FORM_ref1 => AttributeValue::UnitRef(UnitOffset(self.read_u8()? as usize)),
            constants::DW_FORM_ref2 => AttributeValue::UnitRef(UnitOffset(self.read_u16()? as usize)),
            constants::DW_FORM_ref4 => AttributeValue::UnitRef(UnitOffset(self.read_u32()? as usize)),
            constants::DW_FORM_ref8 => AttributeValue::UnitRef(UnitOffset(self.read_u64()? as usize)),
            constants::DW_FORM_ref_udata => AttributeValue::UnitRef(UnitOffset(self.read_uleb128()? as usize)),
            // Note: this is missing logic for DWARF 2, see this case in parse_attribute() in gimli.
            constants::DW_FORM_ref_addr => AttributeValue::DebugInfoRef(DebugInfoOffset(self.read_offset(encoding.format)?)),
            constants::DW_FORM_ref_sig8 => AttributeValue::DebugTypesRef(DebugTypeSignature(self.read_u64()?)),
            constants::DW_FORM_ref_sup4 => AttributeValue::DebugInfoRefSup(DebugInfoOffset(self.read_u32()? as usize)),
            constants::DW_FORM_ref_sup8 => AttributeValue::DebugInfoRefSup(DebugInfoOffset(self.read_u64()? as usize)),
            constants::DW_FORM_GNU_ref_alt => AttributeValue::DebugInfoRefSup(DebugInfoOffset(self.read_offset(encoding.format)?)),
            constants::DW_FORM_string => AttributeValue::String(self.read_null_terminated_slice()?),
            constants::DW_FORM_strp => AttributeValue::DebugStrRef(DebugStrOffset(self.read_offset(encoding.format)?)),
            constants::DW_FORM_strp_sup | constants::DW_FORM_GNU_strp_alt => AttributeValue::DebugStrRefSup(DebugStrOffset(self.read_offset(encoding.format)?)),
            constants::DW_FORM_line_strp => AttributeValue::DebugLineStrRef(DebugLineStrOffset(self.read_offset(encoding.format)?)),
            constants::DW_FORM_implicit_const => AttributeValue::Sdata(spec.implicit_const_value as i64),
            constants::DW_FORM_strx | constants::DW_FORM_GNU_str_index => AttributeValue::DebugStrOffsetsIndex(DebugStrOffsetsIndex(self.read_uleb128()? as usize)),
            constants::DW_FORM_strx1 => AttributeValue::DebugStrOffsetsIndex(DebugStrOffsetsIndex(self.read_u8()? as usize)),
            constants::DW_FORM_strx2 => AttributeValue::DebugStrOffsetsIndex(DebugStrOffsetsIndex(self.read_u16()? as usize)),
            constants::DW_FORM_strx3 => AttributeValue::DebugStrOffsetsIndex(DebugStrOffsetsIndex(self.read_u32()? as usize)),
            constants::DW_FORM_strx4 => AttributeValue::DebugStrOffsetsIndex(DebugStrOffsetsIndex(self.read_u64()? as usize)),
            constants::DW_FORM_addrx | constants::DW_FORM_GNU_addr_index => AttributeValue::DebugAddrIndex(DebugAddrIndex(self.read_uleb128()? as usize)),
            constants::DW_FORM_addrx1 => AttributeValue::DebugAddrIndex(DebugAddrIndex(self.read_u8()? as usize)),
            constants::DW_FORM_addrx2 => AttributeValue::DebugAddrIndex(DebugAddrIndex(self.read_u16()? as usize)),
            constants::DW_FORM_addrx3 => AttributeValue::DebugAddrIndex(DebugAddrIndex(self.read_u24()? as usize)),
            constants::DW_FORM_addrx4 => AttributeValue::DebugAddrIndex(DebugAddrIndex(self.read_u32()? as usize)),
            constants::DW_FORM_loclistx => AttributeValue::DebugLocListsIndex(DebugLocListsIndex(self.read_uleb128()? as usize)),
            constants::DW_FORM_rnglistx => AttributeValue::DebugRngListsIndex(DebugRngListsIndex(self.read_uleb128()? as usize)),
            _ => return Err(gimli::Error::UnknownForm(form)),
        })
    }

    pub fn skip_attribute(&mut self, spec: AttributeSpecification, encoding: Encoding) -> gimli::Result<()> {
        let form = self.chase_attribute_form(spec.form())?;
        let bytes: usize = match form {
            constants::DW_FORM_addr => encoding.address_size as usize,
            constants::DW_FORM_implicit_const | constants::DW_FORM_flag_present => 0,
            constants::DW_FORM_data1 | constants::DW_FORM_flag | constants::DW_FORM_strx1 | constants::DW_FORM_ref1 | constants::DW_FORM_addrx1 => 1,
            constants::DW_FORM_data2 | constants::DW_FORM_ref2 | constants::DW_FORM_addrx2 | constants::DW_FORM_strx2 => 2,
            constants::DW_FORM_addrx3 | constants::DW_FORM_strx3 => 3,
            constants::DW_FORM_data4 | constants::DW_FORM_ref_sup4 | constants::DW_FORM_ref4 | constants::DW_FORM_strx4 | constants::DW_FORM_addrx4 => 4,
            constants::DW_FORM_data8 | constants::DW_FORM_ref8 | constants::DW_FORM_ref_sig8 | constants::DW_FORM_ref_sup8 => 8,
            constants::DW_FORM_data16 => 16,
            constants::DW_FORM_sec_offset | constants::DW_FORM_GNU_ref_alt | constants::DW_FORM_strp | constants::DW_FORM_strp_sup | constants::DW_FORM_GNU_strp_alt | constants::DW_FORM_line_strp => encoding.format.word_size() as usize,

            constants::DW_FORM_ref_addr => if encoding.version == 2 {encoding.address_size as usize} else {encoding.format.word_size() as usize},

            constants::DW_FORM_block1 => self.read_u8()? as usize,
            constants::DW_FORM_block2 => self.read_u16()? as usize,
            constants::DW_FORM_block4 => self.read_u32()? as usize,
            constants::DW_FORM_block | constants::DW_FORM_exprloc => self.read_uleb128()? as usize,
            constants::DW_FORM_string => self.find(0)? + 1,
            constants::DW_FORM_udata | constants::DW_FORM_sdata | constants::DW_FORM_ref_udata | constants::DW_FORM_strx | constants::DW_FORM_GNU_str_index | constants::DW_FORM_addrx | constants::DW_FORM_GNU_addr_index | constants::DW_FORM_loclistx | constants::DW_FORM_rnglistx => {
                self.skip_leb128()?;
                0
            }
            _ => return Err(gimli::Error::UnknownForm(form)),
        };
        self.read_bytes(bytes)?;
        Ok(())
    }

    pub fn skip_attributes(&mut self, specs: &[AttributeSpecification], encoding: Encoding) -> gimli::Result<()> {
        for &spec in specs {
            self.skip_attribute(spec, encoding)?;
        }
        Ok(())
    }
}


const DW_FORM_addr: DwForm = DwForm(0x01);
const DW_FORM_block2: DwForm = DwForm(0x03);
const DW_FORM_block4: DwForm = DwForm(0x04);
const DW_FORM_data2: DwForm = DwForm(0x05);
const DW_FORM_data4: DwForm = DwForm(0x06);
const DW_FORM_data8: DwForm = DwForm(0x07);
const DW_FORM_string: DwForm = DwForm(0x08);
const DW_FORM_block: DwForm = DwForm(0x09);
const DW_FORM_block1: DwForm = DwForm(0x0a);
const DW_FORM_data1: DwForm = DwForm(0x0b);
const DW_FORM_flag: DwForm = DwForm(0x0c);
const DW_FORM_sdata: DwForm = DwForm(0x0d);
const DW_FORM_strp: DwForm = DwForm(0x0e);
const DW_FORM_udata: DwForm = DwForm(0x0f);
const DW_FORM_ref_addr: DwForm = DwForm(0x10);
const DW_FORM_ref1: DwForm = DwForm(0x11);
const DW_FORM_ref2: DwForm = DwForm(0x12);
const DW_FORM_ref4: DwForm = DwForm(0x13);
const DW_FORM_ref8: DwForm = DwForm(0x14);
const DW_FORM_ref_udata: DwForm = DwForm(0x15);
const DW_FORM_indirect: DwForm = DwForm(0x16);
const DW_FORM_sec_offset: DwForm = DwForm(0x17);
const DW_FORM_exprloc: DwForm = DwForm(0x18);
const DW_FORM_flag_present: DwForm = DwForm(0x19);
const DW_FORM_ref_sig8: DwForm = DwForm(0x20);
const DW_FORM_strx: DwForm = DwForm(0x1a);
const DW_FORM_addrx: DwForm = DwForm(0x1b);
const DW_FORM_ref_sup4: DwForm = DwForm(0x1c);
const DW_FORM_strp_sup: DwForm = DwForm(0x1d);
const DW_FORM_data16: DwForm = DwForm(0x1e);
const DW_FORM_line_strp: DwForm = DwForm(0x1f);
const DW_FORM_implicit_const: DwForm = DwForm(0x21);
const DW_FORM_loclistx: DwForm = DwForm(0x22);
const DW_FORM_rnglistx: DwForm = DwForm(0x23);
const DW_FORM_ref_sup8: DwForm = DwForm(0x24);
const DW_FORM_strx1: DwForm = DwForm(0x25);
const DW_FORM_strx2: DwForm = DwForm(0x26);
const DW_FORM_strx3: DwForm = DwForm(0x27);
const DW_FORM_strx4: DwForm = DwForm(0x28);
const DW_FORM_addrx1: DwForm = DwForm(0x29);
const DW_FORM_addrx2: DwForm = DwForm(0x2a);
const DW_FORM_addrx3: DwForm = DwForm(0x2b);
const DW_FORM_addrx4: DwForm = DwForm(0x2c);
const DW_FORM_GNU_addr_index: DwForm = DwForm(0x1f01);
const DW_FORM_GNU_str_index: DwForm = DwForm(0x1f02);
const DW_FORM_GNU_ref_alt: DwForm = DwForm(0x1f20);
const DW_FORM_GNU_strp_alt: DwForm = DwForm(0x1f2);

// These are not actual DWARF forms, I made these numbers up. If future versions of the standard clash with these values, change them. (Or, if you're philosophically opposed to this, change the type to u32 and use values >= 2^16.)
const DW_EXTRA_FORM_skip_bytes: DwForm = DwForm(0xfe00);
const DW_EXTRA_FORM_skip_leb128: DwForm = DwForm(0xfe01);
const DW_EXTRA_FORM_skip_block: DwForm = DwForm(0xfe02);
const DW_EXTRA_FORM_skip_block1: DwForm = DwForm(0xfe03);
const DW_EXTRA_FORM_skip_block2: DwForm = DwForm(0xfe04);
const DW_EXTRA_FORM_skip_block4: DwForm = DwForm(0xfe05);
const DW_EXTRA_FORM_skip_string: DwForm = DwForm(0xfe06);


/*asdqwe
DW_AT_type, DW_AT_specification, DW_AT_abstract_origin;
UnitRef, DebugInfoRef;

DW_AT_decl_file, DW_AT_decl_line, DW_AT_decl_column, DW_AT_call_file, DW_AT_call_line, DW_AT_call_column;
udata_value();

DW_AT_ranges;
RangeListsRef(offset) => dwarf.ranges_offset_from_raw(unit, offset);
DebugRngListsIndex(index) => dwarf.ranges_offset(unit, index)?;

DW_AT_low_pc;
attr_address()
    Addr(addr) => addr
    DebugAddrIndex(index) => dwarf.address(unit, index);

DW_AT_high_pc;
attr_address() or udata_value();

DW_AT_language;
u16_value();

DW_AT_const_value;
udata_value()/sdata_value() => usize;
Block => &'static [u8];
string_value() => &'static [u8];

DW_AT_location, DW_AT_frame_base;
exprloc_value(),
LocationListsRef(offset) => offset,
DebugLocListsIndex(index) => dwarf.locations_offset(unit, index)?,
Some(offset) = attr.offset_value() => LocationListsOffset(offset);

DW_AT_main_subprogramm, DW_AT_declaration, DW_AT_external, DW_AT_artificial;
flag;

DW_AT_inline;
u8_value();

DW_AT_byte_size, DW_AT_bit_size;
udata_value();

DW_AT_encoding;
u8_value();

DW_AT_bit_stride, DW_AT_byte_stride;
sdata_value(), otherwise "warning: arrays with dynamic stride are not supported ({:?} @0x{:x})";

DW_AT_ordering, DW_AT_rank, DW_AT_discr_list;
"warning: {} on {} is not supported";

DW_AT_data_bit_offset, DW_AT_data_member_location;
udata_value();
exprloc_value() not supported;

DW_AT_discr;
UnitRef;

DW_AT_discr_value;
udata_value()/sdata_value();

DW_AT_count, DW_AT_lower_bound, DW_AT_upper_bound;
udata_value()/sdata_value(), ignore otherwise (exprloc?);
*/

fn prepare_attribute_action(attr: AttributeSpecification, layout: &AttributeStructLayout, encoding: Encoding, warnings: &FormWarningLimiter, flags: &mut u32) -> Result<AttributeAction> {
    assert!(attr.form != DW_FORM_indirect);
    let mut found_name = false;
    for i in 0..layout.fields.len() {
        let &(field_offset, attr_name, attr_type) = &layout.fields[i];
        if attr.name != attr_name {
            continue;
        }
        found_name = true;
        let mut found_match = true;
        let mut form = attr.form;
        let mut param = 0usize;
        match attr_type {
            AttributeType::String => match attr.form {
                DW_FORM_string | DW_FORM_strp | DW_FORM_line_strp | DW_FORM_strp_sup | DW_FORM_strx | DW_FORM_strx1 | DW_FORM_strx2 | DW_FORM_strx3 | DW_FORM_strx4 => param = 1,
                DW_FORM_GNU_str_index => {form = DW_FORM_strx; param = 1;}
                DW_FORM_GNU_strp_alt => {form = DW_FORM_strp_sup; param = 1;}
                _ => found_match = false,
            }
            AttributeType::Slice => match attr.form {
                DW_FORM_block | DW_FORM_block1 | DW_FORM_block2 | DW_FORM_block4 | DW_FORM_string | DW_FORM_strp | DW_FORM_line_strp | DW_FORM_strp_sup | DW_FORM_strx | DW_FORM_strx1 | DW_FORM_strx2 | DW_FORM_strx3 | DW_FORM_strx4 | DW_FORM_data16 => (),
                DW_FORM_GNU_str_index => form = DW_FORM_strx,
                DW_FORM_GNU_strp_alt => form = DW_FORM_strp_sup,
                _ => found_match = false,
            }
            AttributeType::Unsigned => match attr.form {
                DW_FORM_data1 | DW_FORM_data2 | DW_FORM_data4 | DW_FORM_data8 | DW_FORM_udata => (),
                _ => found_match = false,
            }
            AttributeType::Signed => match attr.form {
                DW_FORM_sdata => (),
                DW_FORM_implicit_const => param = attr.implicit_const_value,
                _ => found_match = false,
            }
            AttributeType::MaybeSigned => match attr.form {
                DW_FORM_data1 | DW_FORM_data2 | DW_FORM_data4 | DW_FORM_data8 | DW_FORM_udata | DW_FORM_sdata => (),
                DW_FORM_implicit_const => param = attr.implicit_const_value,
                _ => found_match = false,
            }
            AttributeType::UnitOffset => match attr.form {
                DW_FORM_ref1 | DW_FORM_ref2 | DW_FORM_ref4 | DW_FORM_ref8 | DW_FORM_ref_udata => (),
                _ => found_match = false,
            }
            AttributeType::DebugInfoOffset => match attr.form {
                DW_FORM_ref1 | DW_FORM_ref2 | DW_FORM_ref4 | DW_FORM_ref8 | DW_FORM_ref_udata | DW_FORM_ref_addr => param = 1,
                _ => found_match = false,
            }
            AttributeType::Address => match attr.form {
                DW_FORM_addr | DW_FORM_addrx | DW_FORM_addrx1 | DW_FORM_addrx2 | DW_FORM_addrx3 | DW_FORM_addrx4 => (),
                DW_FORM_GNU_addr_index => form = DW_FORM_addrx,
                _ => found_match = false,
            }
            AttributeType::Expression => match attr.form {
                DW_FORM_exprloc => (),
                _ => found_match = false,
            }
            AttributeType::LocationListsOffset => match attr.form {
                DW_FORM_loclistx | DW_FORM_sec_offset => (),
                _ => found_match = false,
            }
            AttributeType::Flag => match attr.form {
                DW_FORM_flag | DW_FORM_flag_present => (),
                _ => found_match = false,
            }
            AttributeType::SectionOffset => match attr.form {
                DW_FORM_sec_offset => (),
                _ => found_match = false,
            }

            AttributeType::Ranges => match attr.name {
                DW_AT_ranges => match attr.form {
                    // Note the sec_offset case would need additional logic to support split dwarf (dwo).
                    DW_FORM_rnglistx | DW_FORM_sec_offset => *flags |= DwarfRanges::RANGES,
                    _ => found_match = false,
                }
                DW_AT_low_pc => match attr.form {
                    DW_FORM_addr | DW_FORM_addrx | DW_FORM_addrx1 | DW_FORM_addrx2 | DW_FORM_addrx3 | DW_FORM_addrx4 => *flags |= DwarfRanges::LOW_PC,
                    _ => found_match = false,
                }
                DW_AT_high_pc => match attr.form {
                    DW_FORM_addr | DW_FORM_addrx | DW_FORM_addrx1 | DW_FORM_addrx2 | DW_FORM_addrx3 | DW_FORM_addrx4 => *flags |= DwarfRanges::HIGH_PC,
                    DW_FORM_data1 | DW_FORM_data2 | DW_FORM_data4 | DW_FORM_data8 | DW_FORM_udata => *flags |= DwarfRanges::HIGH_PC | DwarfRanges::HIGH_PC_IS_RELATIVE,
                    _ => found_match = false,
                }
                _ => panic!("unexpected Ranges attribute name"),
            }
            AttributeType::SpecificationOrAbstractOrigin => {
                if *flags & DwarfReference::HAS_SPECIFICATION_OR_ABSTRACT_ORIGIN != 0 {
                    if warnings.warn(attr.name, attr.form) {
                        eprintln!("warning: an entry has both DW_AT_specification and DW_AT_abstract_origin");
                    }
                    // If both attributes are present, make sure we don't end up with the value from one of them but flag DwarfReference::GLOBAL from the other.
                    found_match = false;
                    found_name = false; // don't warn about unexpected form
                } else {
                    *flags |= DwarfReference::HAS_SPECIFICATION_OR_ABSTRACT_ORIGIN;
                    match attr.form {
                        DW_FORM_ref1 | DW_FORM_ref2 | DW_FORM_ref4 | DW_FORM_ref8 | DW_FORM_ref_udata => (),
                        DW_FORM_ref_addr => *flags |= DwarfReference::GLOBAL,
                        _ => found_match = false,
                    }
                }
            }
            AttributeType::CodeLocation => panic!("this was supposed to be preprocessed away"),
        }

        if found_match {
            if i < layout.num_fields_included_in_flags as usize {
                *flags |= 1 << i;
            }
            return Ok(AttributeAction {form, field_offset, param});
        }
    }

    if found_name && warnings.warn(attr.name, attr.form) {
        eprintln!("warning: attribute {} form {} is not supported", attr.name, attr.form);
    }
    let (form, param): (DwForm, usize) = match attr.form {
        DW_FORM_addr => (DW_EXTRA_FORM_skip_bytes, encoding.address_size as usize),
        DW_FORM_strp | DW_FORM_ref_addr | DW_FORM_sec_offset | DW_FORM_strp_sup | DW_FORM_line_strp | DW_FORM_GNU_ref_alt | DW_FORM_GNU_strp_alt => (DW_EXTRA_FORM_skip_bytes, encoding.format.word_size() as usize),

        DW_FORM_data1 | DW_FORM_flag | DW_FORM_ref1 | DW_FORM_strx1 | DW_FORM_addrx1 => (DW_EXTRA_FORM_skip_bytes, 1),
        DW_FORM_data2 | DW_FORM_ref2 | DW_FORM_strx2 | DW_FORM_addrx2 => (DW_EXTRA_FORM_skip_bytes, 2),
        DW_FORM_strx3 | DW_FORM_addrx3 => (DW_EXTRA_FORM_skip_bytes, 3),
        DW_FORM_data4 | DW_FORM_ref4 | DW_FORM_ref_sup4 | DW_FORM_strx4 | DW_FORM_addrx4 => (DW_EXTRA_FORM_skip_bytes, 4),
        DW_FORM_data8 | DW_FORM_ref8 | DW_FORM_ref_sig8 | DW_FORM_ref_sup8 => (DW_EXTRA_FORM_skip_bytes, 8),
        DW_FORM_data16 => (DW_EXTRA_FORM_skip_bytes, 16),

        DW_FORM_string => (DW_EXTRA_FORM_skip_string, 0),

        DW_FORM_block | DW_FORM_exprloc => (DW_EXTRA_FORM_skip_block, 0),
        DW_FORM_block1 => (DW_EXTRA_FORM_skip_block1, 0),
        DW_FORM_block2 => (DW_EXTRA_FORM_skip_block2, 0),
        DW_FORM_block4 => (DW_EXTRA_FORM_skip_block4, 0),

        DW_FORM_sdata | DW_FORM_udata | DW_FORM_ref_udata | DW_FORM_strx | DW_FORM_addrx | DW_FORM_loclistx | DW_FORM_rnglistx | DW_FORM_GNU_addr_index | DW_FORM_GNU_str_index => (DW_EXTRA_FORM_skip_leb128, 0),

        DW_FORM_flag_present | DW_FORM_implicit_const => (DW_EXTRA_FORM_skip_bytes, 0),

        _ => return err!(Dwarf, "unknown form: {}", attr.form),
    };

    Ok(AttributeAction {form, param, field_offset: u32::MAX})
}

fn prepare_abbreviation_actions(attributes: &[AttributeSpecification], layout_idx: usize, encoding: Encoding, shared: &mut AbbreviationsSharedData) -> Result<AbbreviationActions> {
    let mut flags_value = 0u32;
    let mut actions: Vec<AttributeAction> = Vec::new();
    let layout = &shared.layouts.layouts[layout_idx].1;

    for attr in attributes {
        if attr.form == DW_FORM_indirect {
            // DW_FORM_indirect is annoying and almost never used, so we don't have to support it. But we support it.
            // Defer the prepare_attribute_action() call till parsing time.
            actions.push(AttributeAction {form: DW_FORM_indirect, field_offset: u32::MAX, param: layout_idx << 16 | attr.name.0 as usize});
            continue;
        }

        let action = prepare_attribute_action(*attr, layout, encoding, &shared.warnings, &mut flags_value)?;
        if action.form == DW_FORM_flag_present {
            continue;
        }
        if action.form == DW_EXTRA_FORM_skip_bytes {
            if action.param == 0 {
                continue;
            }
            if actions.last().is_some_and(|a| a.form == DW_EXTRA_FORM_skip_bytes) {
                actions.last_mut().unwrap().param += action.param;
                continue;
            }
        }
        actions.push(action);
    }

    Ok(AbbreviationActions {flags_offset: layout.flags_offset, flags_value, actions})
}
