use crate::{*, error::{*, Result, Error}};
use gimli::{*, constants::DwForm};
use std::{str, borrow::Cow, fmt};

// Experimental partial incremental rewrite of gimli, to see how much performance can be improved. Currently just replacing small pieces of gimli while leaving the overall structure the same.
// At some point we should rethink this and either fully redesign+rewrite DWARF parsing or contribute optimizations to gimli and get rid of this (if it turns out that achieving good performance in gimli codebase is not much harder than rewriting it).
// In particular, it would be nice to figure out a way to avoid the cost of checking for eof when reading every value, not sure how exactly.

// We currently don't support .debug_types section. If we were to support it, we would pack section id and offset into one 8-byte value and use that as DieOffset (just like UnitSectionOffset, but 8 bytes instead of 16).
pub type DieOffset = DebugInfoOffset<usize>;

#[derive(Clone, Copy, Default)]
pub struct DwarfSlice {
    pub slice: &'static [u8], // opt out of the borrow checker for this, for now
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

    pub fn read_abbreviation<'ab>(&mut self, abbreviations: &'ab Abbreviations) -> Result<Option<&'ab Abbreviation>> {
        let code = self.read_uleb128()?;
        if code == 0 {
            return Ok(None);
        }
        let Some(abbrev) = abbreviations.get(code) else { return err!(Dwarf, "unexpected abbreviation code: {}", code) };
        Ok(Some(abbrev))
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
            constants::DW_FORM_implicit_const => AttributeValue::Sdata(spec.implicit_const_value().ok_or(gimli::Error::InvalidImplicitConst)?),
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
