use crate::{util::*, arena::*, log::*, *, settings::*, common_ui::*, symbols::*};
use std::{hint, collections::{HashMap, hash_map::{DefaultHasher, Entry}}, hash::{Hash, Hasher}, sync::{Mutex, atomic::{AtomicUsize, Ordering}}, ptr, mem, mem::MaybeUninit, result, io::Write, slice, ops::Range, fmt, fmt::Write as fmtWrite};
use bitflags::*;
use gimli::DebugInfoOffset;

// There are usually lots of types (millions or tens of millions) in debug symbols, so we have to use somewhat efficient data structures for them
// (or use the DWARF DIEs on the fly instead of loading into our data structures, but that's not how we currently do this).

// All types of one binary form one immutable graph of TypeInfo structs.
// TypeInfo can have pointers to other TypeInfo-s (e.g. array element type), and to things like StructField or str. These pointers are never null (at worst they point to a builtin Unknown type).
// For this graph, we bypass the borrow checker (because I really really couldn't come up with a reasonable way to make it work otherwise).
// Be careful when using TypeInfo-s or any references that come from them!
// All shards of a binary must be kept alive as long as there are references to any of its types or their parts.
//
// The TypeInfo-s and objects they point to live in arenas in Types. Each Symbols instance has a set of Types instances.
// Although there's one Types per shard, absolutely no shard locality is guaranteed for the pointers;
// e.g. TypeInfo's `name` pointer typically points to a different shard from where this TypeInfo is.
//
// Each shard's Types has a type_names StringTable suitable for searching.
// The sizes of these tables are reasonably balanced across shards, so name search should parallize well.
// Sizes of other things (types_arena, fields_arena, misc_arena) may be arbitrarily unbalanced.

// (During symbols loading, types are mutable, and pointers are not pointers. See TypesLoader etc.)

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LanguageFamily {
    Cpp,
    Rust,
    Other,
    Unknown,
    Internal, // produced by this debugger
}
impl LanguageFamily {
    // For unrecognized or missing language id, assume C++. In particular, covers future versions of C++ until we add them to the list of recognized language ids.
    pub fn presumably_cpp(self) -> bool {
        match self {
            LanguageFamily::Cpp | LanguageFamily::Other | LanguageFamily::Unknown => true,
            _ => false,
        }
    }
}

bitflags! { pub struct PrimitiveFlags: u8 {
    const SIGNED = 0x1;
    const FLOAT = 0x2;
    const CHAR = 0x4;
    // C/C++ char/int8_t/uint8_t. I.e. it may be a character or a number, we have to guess. When this flag is set, CHAR flag is also set.
    const AMBIGUOUS_CHAR = 0x8;
    const BOOL = 0x10;
    const UNSPECIFIED = 0x20; // DW_TAG_unspecified_type
}}

bitflags! { pub struct PointerFlags: u8 {
    const REFERENCE = 0x1;
}}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct PointerType {
    pub flags: PointerFlags,
    pub type_: *const TypeInfo,
}

bitflags! { pub struct ArrayFlags: u8 {
    // Length is constant. For VLA, DWARF contains an expression to calculate actual length, but we don't support it.
    const LEN_KNOWN = 0x1;
    // We print array as string if its element type is 1-byte char, or if this flag is set. In both cases we assume UTF8.
    const UTF_STRING = 0x2;
    // This array represents a prefix of a longer array (e.g. pretty-printer timed out walking a long linked list).
    const TRUNCATED = 0x4;

    const PARSED_SUBRANGE = 0x8; // used during loading
}}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct ArrayType {
    pub flags: ArrayFlags,
    pub type_: *const TypeInfo,
    pub stride: usize, // 0 means type_'s size
    pub len: usize,
}

bitflags! { pub struct SliceFlags: u8 {
    const UTF_STRING = 0x2;
}}

// Always 16 bytes: start address (in debuggee's address space) and number of elements.
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct SliceType {
    pub flags: SliceFlags,
    pub type_: *const TypeInfo,
}

bitflags! { pub struct FieldFlags: u8 {
    // If name lives in Types.misc_arena. Otherwise it lives in binary's mmap or static.
    // Only relevant during symbols loading, can be omitted for types created by watch expressions.
    const NAME_IN_ARENA = 0x1;
    // Base class, for C++-style inheritance.
    const INHERITANCE = 0x2;
    // DW_AT_artificial. May mean e.g. vtable pointer.
    const ARTIFICIAL = 0x4;
    // If bit_size was assigned. Otherwise use type_'s calculated size.
    const SIZE_KNOWN = 0x8;

    // This is the discriminant field of a discriminated union (e.g. Rust enum).
    const DISCRIMINANT = 0x10;
    // Field of discriminated union. Active if the value of DISCRIMINANT field is equal to this field's discr_value.
    const VARIANT = 0x20;
    // Field of discriminated union. Active if no DISCRIMINATED fields are active.
    const DEFAULT_VARIANT = 0x40;
}}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct StructField {
    pub name: &'static str, // not actually static
    pub flags: FieldFlags,
    pub bit_offset: usize,
    pub bit_size: usize,
    pub type_: *const TypeInfo,

    pub discr_value: usize,
}
impl StructField {
    pub fn calculate_bit_size(&self) -> usize {
        if self.flags.contains(FieldFlags::SIZE_KNOWN) {
            self.bit_size
        } else {
            unsafe {(*self.type_).calculate_size() * 8}
        }
    }

    // Prints non-internal flags in form readable to the user.
    pub fn readable_flags(&self) -> String {
        let mut s = String::new();
        if self.flags.contains(FieldFlags::INHERITANCE) {s.push_str("inheritance | ");}
        if self.flags.contains(FieldFlags::ARTIFICIAL) {s.push_str("artificial | ");}
        if self.flags.contains(FieldFlags::DISCRIMINANT) {s.push_str("discriminant | ");}
        if self.flags.contains(FieldFlags::VARIANT) {s.push_str("variant | ");}
        if self.flags.contains(FieldFlags::DEFAULT_VARIANT) {s.push_str("default_variant | ");}
        if !s.is_empty() {
            s.replace_range(s.len()-3.., "");
        }
        s
    }
}

bitflags! { pub struct StructFlags: u8 {
    const UNION = 0x1;
}}

#[derive(Eq, Clone, Debug)]
pub struct StructType {
    pub flags: StructFlags,
    pub fields_ptr: *const StructField,
    pub fields_len: usize,
}
impl Default for StructType { fn default() -> Self { let p: &'static [StructField] = &[]; Self {flags: StructFlags::empty(), fields_ptr: unsafe {p.as_ptr()}, fields_len: 0} } }
impl Hash for StructType { fn hash<H: Hasher>(&self, state: &mut H) { self.flags.hash(state); self.fields().hash(state); } }
impl PartialEq for StructType { fn eq(&self, other: &Self) -> bool { (self.flags, self.fields()).eq(&(other.flags, other.fields())) } }
impl StructType {
    pub fn fields(&self) -> &[StructField] {unsafe {slice::from_raw_parts(self.fields_ptr, self.fields_len)}}
    pub fn set_fields(&mut self, fields: &'static [StructField]) { self.fields_ptr = fields.as_ptr(); self.fields_len = fields.len(); }
}

bitflags! { pub struct EnumerandFlags: u8 {
    const NAME_IN_ARENA = 0x1;
}}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Enumerand {
    pub name: &'static str, // not actually static
    pub value: usize, // if type_ is signed, sign-extended to 8 bytes
    pub flags: EnumerandFlags,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct EnumType {
    pub enumerands: &'static [Enumerand], // not actually static
    pub type_: *const TypeInfo,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Unknown, // unresolved type reference or other errors
    Primitive(PrimitiveFlags),
    Pointer(PointerType),
    Array(ArrayType),
    // We don't distinguish structs from unions, we just allow fields to overlap.
    // For discriminated unions, discriminant is just another field.
    Struct(StructType),
    Enum(EnumType),
    // *TypeInfo pointer, for inspecting types in the watch window. E.g. a MetaType value is returned from typeof() expression when evaluating watches.
    // Only produced by expression interpreter, never appears in Symbols. Unlike Pointer, this pointer is in debugger's address space.
    // We cast pointers to bytes and back, which may stop being possible in rust in future. Maybe we'll have to add a designated pointer to ValueBlob or something.
    MetaType,
    // Similar to MetaType, but for *StructField and *Variable.
    MetaField,
    MetaVariable, // 16 bytes: binary_id, *Variable
    MetaCodeLocation, // 24 bytes: binary_id, LineInfo
    // Pointer + count. Currently only produced by pretty-printers; things like Rust slices are represented as structs in debug info,
    // and we don't spend time recognizing them and converting to slices at symbols loading time.
    // This type is mostly superfluous in the current very-dynamically-typed interpreter, where we can just create a new Array type for each array,
    // but hopefully we'll do something more statically typed later.
    Slice(SliceType),
}
impl Type {
    pub fn as_primitive(&self) -> Option<PrimitiveFlags> { match self { &Type::Primitive(f) => Some(f), _ => None } }
    pub fn as_pointer(&self) -> Option<&PointerType> { match self { Type::Pointer(p) => Some(p), _ => None } }
    pub fn as_array(&self) -> Option<&ArrayType> { match self { Type::Array(a) => Some(a), _ => None } }
    pub fn as_array_mut(&mut self) -> Option<&mut ArrayType> { match self { Type::Array(a) => Some(a), _ => None } }
    pub fn as_struct(&self) -> Option<&StructType> { match self { Type::Struct(s) => Some(s), _ => None } }
    pub fn as_struct_mut(&mut self) -> Option<&mut StructType> { match self { Type::Struct(s) => Some(s), _ => None } }
    pub fn is_meta(&self) -> bool { match self { Type::MetaType | Type::MetaField | Type::MetaVariable | Type::MetaCodeLocation => true, _ => false} }
    pub fn is_primitive_char(&self) -> bool { match self { Type::Primitive(p) if p.contains(PrimitiveFlags::CHAR) => true, _ => false } }
    pub fn kind_name(&self) -> &'static str {
        match self {
            Type::Unknown => "unknown",
            Type::Primitive(_) => "primitive type",
            Type::Pointer(_) => "pointer",
            Type::Array(_) => "array",
            Type::Slice(s) if s.flags.contains(SliceFlags::UTF_STRING) => "string",
            Type::Slice(_) => "slice",
            Type::Struct(s) if s.flags.contains(StructFlags::UNION) => "union",
            Type::Struct(_) => "struct",
            Type::Enum(_) => "enum",
            Type::MetaType => "type",
            Type::MetaField => "field",
            Type::MetaVariable => "variable",
            Type::MetaCodeLocation => "code location",
        }
    }
}    

bitflags! { pub struct TypeFlags: u8 {
    // The symbols had type declaration but no definition.
    const DECLARATION = 0x1;
    // The type uses something we don't support, e.g. fixed-point primitive types. We'll treat it as a byte string, or something.
    const UNSUPPORTED = 0x2;
    // The name is unique enough to use for deduplication, i.e. it's not in anonymous namespace/function and not missing.
    const LINKABLE_NAME = 0x4;
    // If not set, `size` field is 0, and we have to chase pointers to find the actual size.
    const SIZE_KNOWN = 0x8;

    const BUILTIN = 0x10;

    // Used only during symbols loading. Means that all *const TypeInfo pointers inside this type are actually DieOffset-s, not real pointers.
    const POINTERS_NOT_RESOLVED = 0x20;
}}

pub const FAKE_DWARF_OFFSET_START: usize = (1 << 48) - 1000;

#[derive(Clone, Copy, Debug)]
pub enum NestedName {
    Type(*const TypeInfo),
    // Global variable, e.g. static field. Points into SymbolsShard.global_variables.
    Variable(*const Variable),
}

#[derive(Clone, Debug)]
pub struct TypeInfo {
    // This is the name to display in watches window and such. Type may have other names that are recognized in user expressions, they're in name_to_type.
    pub name: &'static str, // empty means unknown or anonymous; not actually static
    pub size: usize,
    pub die: DieOffset, // 0 means generated by interpreter, >= FAKE_DWARF_OFFSET_START means debugger builtin
    pub binary_id: usize,
    pub line: LineInfo,
    pub flags: TypeFlags,
    pub language: LanguageFamily,
    // Typedefs and static fields/constants. In particular useful for things like std::list<T>::value_type.
    // Doesn't include enum enumerands (to save memory).
    // &'static is either static or in misc_arena.
    pub nested_names: &'static [(&'static str, NestedName)],
    pub t: Type,
}
impl Default for TypeInfo { fn default() -> Self { Self {name: "", size: 0, die: DebugInfoOffset(0), binary_id: usize::MAX, line: LineInfo::invalid(), flags: TypeFlags::empty(), language: LanguageFamily::Internal, nested_names: &[], t: Type::Unknown} } }

impl TypeInfo {
    fn can_have_own_name(&self) -> bool { match &self.t { Type::Pointer(_) | Type::Array(_) | Type::Slice(_) => false, _ => true } }
    fn can_be_forward_declared(&self) -> bool { match &self.t { Type::Struct(_) | Type::Enum(_) => true, _ => false } }

    pub fn calculate_size(&self) -> usize {
        let mut t = self;
        let mut multiplier = 1usize;
        for i in 0..100 {
            if t.flags.contains(TypeFlags::SIZE_KNOWN) {
                return t.size * multiplier;
            }
            match &t.t {
                Type::Unknown | Type::Primitive(_) | Type::Pointer(_) | Type::MetaType | Type::MetaField => return 8 * multiplier,
                Type::MetaVariable => return 16 * multiplier,
                Type::MetaCodeLocation => return 24 * multiplier,
                Type::Slice(_) => return 16 * multiplier,
                Type::Struct(_) => return 0,
                Type::Enum(e) => t = unsafe {&*e.type_},
                Type::Array(a) => {
                    if a.flags.contains(ArrayFlags::LEN_KNOWN) {
                        multiplier *= a.len;
                    } else {
                        // Let's pretend length is 1, so that we at least show the first element in watches by default.
                    }
                    t = unsafe {&*a.type_};
                }
            }
        }
        return 0;
    }
}

pub struct Types {
    // Name -> type mapping. Id is transmuted *const TypeInfo.
    // If multiple types have the same name, we append suffixes "#2", "#3", etc to make them unique.
    // Sorted lexicographically, so we can binary search to look up fully qualified names.
    pub sorted_type_names: StringTable,
    // Like sorted_type_names, but not sorted, requires linear search. Each name is present in exactly one of the two.
    // For SymbolsShard, unsorted_type_names is empty, so searching by full type name is reasonably fast.
    pub unsorted_type_names: StringTable,
    pub types_arena: Arena, // TypeInfo only (iterable)
    pub fields_arena: Arena, // StructField, Enumerand; do no add anything else here, it'll break the push_to_array_likely_at_end thing!
    pub misc_arena: Arena, // field/enumerand names; separate from `fields_arena` to make the push_to_array_likely_at_end array growth work for the slices in StructType/EnumType
}

impl Types {
    pub fn new() -> Self { Self {sorted_type_names: StringTable::new(), unsorted_type_names: StringTable::new(), types_arena: Arena::new(), misc_arena: Arena::new(), fields_arena: Arena::new()} }

    pub fn num_types(&self) -> usize { self.types_arena.used() / mem::size_of::<TypeInfo>() }

    // Adds the type, copies the name into unsorted_type_names and puts it in TypeInfo (if not empty).
    pub fn add_and_import_name(&mut self, info: TypeInfo, name: &str) -> *mut TypeInfo {
        let res: *mut TypeInfo = self.types_arena.add_mut(info) as _;
        if !name.is_empty() {
            unsafe {(*res).name = self.unsorted_type_names.add_str(name, res as *const _ as usize)};
        }
        res
    }

    pub fn add_field(&mut self, s: &mut StructType, field: StructField) {
        self.fields_arena.push_to_array_likely_at_end(&mut s.fields_ptr, &mut s.fields_len, field);
    }
    pub fn add_enumerand(&mut self, e: &mut EnumType, enumerand: Enumerand) {
        unsafe {
            let mut p = e.enumerands.as_ptr();
            let mut l = e.enumerands.len();
            self.fields_arena.push_to_array_likely_at_end(&mut p, &mut l, enumerand);
            e.enumerands = slice::from_raw_parts(p, l);
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a TypeInfo> { unsafe {self.types_arena.iter()} }

    pub fn add_builtins(&mut self) -> BuiltinTypes {
        BuiltinTypes::create(|t| self.types_arena.add(t))
    }

    pub fn add_primitive(&mut self, name: &'static str, size: usize, flags: PrimitiveFlags) -> *const TypeInfo {
        self.types_arena.add(TypeInfo {name, size, flags: TypeFlags::SIZE_KNOWN, t: Type::Primitive(flags), ..TypeInfo::default()})
    }
    pub fn add_pointer(&mut self, type_: *const TypeInfo, flags: PointerFlags) -> *const TypeInfo {
        self.types_arena.add(TypeInfo {size: 8, flags: TypeFlags::SIZE_KNOWN, t: Type::Pointer(PointerType {type_, flags}), ..TypeInfo::default()})
    }
    pub fn add_array(&mut self, type_: *const TypeInfo, len: Option<usize>, extra_flags: ArrayFlags) -> *const TypeInfo {
        let stride = unsafe {(*type_).calculate_size()};
        let len_or_fallback = len.clone().unwrap_or(0);
        let mut flags = extra_flags;
        if len.is_some() {
            flags.insert(ArrayFlags::LEN_KNOWN);
        }
        self.types_arena.add(TypeInfo {size: len_or_fallback * stride, flags: TypeFlags::SIZE_KNOWN, t: Type::Array(ArrayType {len: len_or_fallback, stride, type_, flags}), ..TypeInfo::default()})
    }
    pub fn add_slice(&mut self, type_: *const TypeInfo, flags: SliceFlags) -> *const TypeInfo {
        self.types_arena.add(TypeInfo {size: 16, flags: TypeFlags::SIZE_KNOWN, t: Type::Slice(SliceType {type_, flags}), ..TypeInfo::default()})
    }

    pub fn find_by_name(&self, name: &str) -> Option<*const TypeInfo> {
        if let Some(id) = self.sorted_type_names.binary_search(name.as_bytes()) {
            return Some(id as *const TypeInfo);
        }
        for s in &self.unsorted_type_names.strings {
            if s.s == name.as_bytes() {
                return Some(s.id as *const TypeInfo);
            }
        }
        None
    }

    // Copies parts of TypeInfo into self's arenas and updates the corresponding pointers inside TypeInfo.
    // Doesn't copy the TypeInfo itself and its type name. Copies things like fields/enumerands and their names.
    // Adjusts field names to make them unique, if they're not.
    fn partially_import_and_adjust_field_names(&mut self, info: *mut TypeInfo, scratch_arena: &mut Arena) {
        unsafe {
            match &mut (*info).t {
                Type::Struct(s) => {
                    let fs = self.fields_arena.add_slice_mut(&s.fields());

                    // Make field names unique by appending "#2" etc when needed.
                    // And import names into the correct arena when needed.
                    let field_names: &mut [MaybeUninit<(&str, usize)>] = scratch_arena.alloc_slice(fs.len());
                    for i in 0..fs.len() {
                        field_names[i].write((fs[i].name, i));
                    }
                    let field_names: *mut MaybeUninit<(&str, usize)> = field_names.as_mut_ptr();
                    let field_names: &mut [(&str, usize)] = unsafe {std::slice::from_raw_parts_mut(field_names as _, fs.len())};
                    field_names.sort_unstable();
                    let mut repeats = 0usize;
                    for i in 0..fs.len() {
                        if i > 0 && field_names[i-1].0 == field_names[i].0 {
                            repeats += 1;
                        } else {
                            repeats = 0;
                        }
                        let f = &mut fs[field_names[i].1];
                        if repeats > 0 && !f.name.is_empty() {
                            let mut out = self.misc_arena.write();
                            write!(out, "{}#{}", f.name, repeats + 1).unwrap();
                            f.name = out.finish_str();
                            f.flags.insert(FieldFlags::NAME_IN_ARENA);
                        } else if f.flags.contains(FieldFlags::NAME_IN_ARENA) {
                            f.name = self.misc_arena.add_str(f.name);
                        }
                    }

                    s.fields_ptr = fs.as_ptr();
                    s.fields_len = fs.len();
                }
                Type::Enum(e) => {
                    let es = self.fields_arena.add_slice_mut(&e.enumerands);
                    for ee in es.iter_mut() {
                        if ee.flags.contains(EnumerandFlags::NAME_IN_ARENA) {
                            ee.name = self.misc_arena.add_str(ee.name);
                        }
                    }
                    e.enumerands = unsafe {mem::transmute(es)};
                }
                _ => (),
            }
        }
    }
}

pub struct BuiltinTypes {
    pub void: *const TypeInfo,
    pub void_pointer: *const TypeInfo,
    pub unknown: *const TypeInfo,
    pub u8_: *const TypeInfo,
    pub u16_: *const TypeInfo,
    pub u32_: *const TypeInfo,
    pub u64_: *const TypeInfo,
    pub i8_: *const TypeInfo,
    pub i16_: *const TypeInfo,
    pub i32_: *const TypeInfo,
    pub i64_: *const TypeInfo,
    pub f32_: *const TypeInfo,
    pub f64_: *const TypeInfo,
    pub f128_: *const TypeInfo, // not actually supported currently
    pub char8: *const TypeInfo,
    pub char32: *const TypeInfo,
    pub bool_: *const TypeInfo,
    pub meta_type: *const TypeInfo,
    pub meta_field: *const TypeInfo,
    pub meta_variable: *const TypeInfo,
    pub meta_code_location: *const TypeInfo,
}
impl BuiltinTypes {
    pub fn invalid() -> Self { Self {void: ptr::null(), void_pointer: ptr::null(), unknown: ptr::null(), u8_: ptr::null(), u16_: ptr::null(), u32_: ptr::null(), u64_: ptr::null(), i8_: ptr::null(), i16_: ptr::null(), i32_: ptr::null(), i64_: ptr::null(), f32_: ptr::null(), f64_: ptr::null(), f128_: ptr::null(), char8: ptr::null(), char32: ptr::null(), bool_: ptr::null(), meta_type: ptr::null(), meta_field: ptr::null(), meta_variable: ptr::null(), meta_code_location: ptr::null()} }

    fn map<F: FnMut(*const TypeInfo) -> *const TypeInfo>(&self, mut f: F) -> Self {
        Self {void: f(self.void), void_pointer: f(self.void_pointer), unknown: f(self.unknown), u8_: f(self.u8_), u16_: f(self.u16_), u32_: f(self.u32_), u64_: f(self.u64_), i8_: f(self.i8_), i16_: f(self.i16_), i32_: f(self.i32_), i64_: f(self.i64_), f32_: f(self.f32_), f64_: f(self.f64_), f128_: f(self.f128_), char8: f(self.char8), char32: f(self.char32), bool_: f(self.bool_), meta_type: f(self.meta_type), meta_field: f(self.meta_field), meta_variable: f(self.meta_variable), meta_code_location: f(self.meta_code_location)}
    }

    fn create<F: FnMut(TypeInfo) -> *const TypeInfo>(mut f: F) -> Self {
        let primitive = |name, size, flags| TypeInfo {name, size, flags: TypeFlags::SIZE_KNOWN | TypeFlags::BUILTIN, t: Type::Primitive(flags), ..Default::default()};
        let void = f(primitive("void", 0, PrimitiveFlags::UNSPECIFIED));
        Self {
            unknown: f(TypeInfo {name: "<unknown>", size: 8, flags: TypeFlags::SIZE_KNOWN | TypeFlags::BUILTIN, ..Default::default()}),
            void,
            void_pointer: f(TypeInfo {size: 8, flags: TypeFlags::SIZE_KNOWN | TypeFlags::BUILTIN, t: Type::Pointer(PointerType {type_: void, flags: PointerFlags::empty()}), ..Default::default()}),
            u8_: f(primitive("u8", 1, PrimitiveFlags::empty())),
            u16_: f(primitive("u16", 2, PrimitiveFlags::empty())),
            u32_: f(primitive("u32", 4, PrimitiveFlags::empty())),
            u64_: f(primitive("u64", 8, PrimitiveFlags::empty())),
            i8_: f(primitive("i8", 1, PrimitiveFlags::SIGNED)),
            i16_: f(primitive("i16", 2, PrimitiveFlags::SIGNED)),
            i32_: f(primitive("i32", 4, PrimitiveFlags::SIGNED)),
            i64_: f(primitive("i64", 8, PrimitiveFlags::SIGNED)),
            f32_: f(primitive("f32", 4, PrimitiveFlags::FLOAT)),
            f64_: f(primitive("f64", 8, PrimitiveFlags::FLOAT)),
            f128_: f(TypeInfo {name: "<f128 not supported>", size: 16, flags: TypeFlags::SIZE_KNOWN | TypeFlags::BUILTIN, ..Default::default()}),
            char8: f(primitive("char8", 1, PrimitiveFlags::CHAR)),
            char32: f(primitive("char32", 4, PrimitiveFlags::CHAR)),
            bool_: f(primitive("bool", 1, PrimitiveFlags::BOOL)),
            meta_type: f(TypeInfo {name: "type", size: 8, flags: TypeFlags::SIZE_KNOWN, t: Type::MetaType, ..Default::default()}),
            meta_field: f(TypeInfo {name: "field", size: 8, flags: TypeFlags::SIZE_KNOWN, t: Type::MetaField, ..Default::default()}),
            meta_variable: f(TypeInfo {name: "variable", size: 16, flags: TypeFlags::SIZE_KNOWN, t: Type::MetaVariable, ..Default::default()}),
            meta_code_location: f(TypeInfo {name: "location", size: 24, flags: TypeFlags::SIZE_KNOWN, t: Type::MetaCodeLocation, ..Default::default()}),
        }
    }
}

pub struct DumpType(*const TypeInfo);
impl fmt::Debug for DumpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        unsafe {
            let t: &TypeInfo = &*self.0;
            write!(f, "@{:x} '{}' size {} flags {:?}  ", t.die.0, t.name, t.size, t.flags)?;
            match &t.t {
                Type::Unknown => write!(f, "unknown")?,
                Type::Primitive(p) => write!(f, "primitive {:?}", p)?,
                Type::Pointer(p) => write!(f, "* {:?}  @{:x} '{}'", p.flags, (*p.type_).die.0, (*p.type_).name)?,
                Type::Array(a) => write!(f, "[] {:?} /{}  @{:x} '{}'", a.flags, a.stride, (*a.type_).die.0, (*a.type_).name)?,
                Type::Slice(s) => write!(f, "[..] {:?} @{:x} '{}'", s.flags, (*s.type_).die.0, (*s.type_).name)?,
                Type::Struct(s) => {
                    write!(f, "{{")?;
                    for field in s.fields() {
                        write!(f, "{}: @{:x} {} [{}:{}] {:?}, ", field.name, (*field.type_).die.0, (*field.type_).name, field.bit_offset, field.bit_size, field.flags)?;
                    }
                    write!(f, "}} {:?}", s.flags)?;
                }
                Type::Enum(e) => {
                    write!(f, "enum {} {{", (*e.type_).name)?;
                    for en in e.enumerands {
                        write!(f, "{} = {}, ", en.name, en.value)?;
                    }
                    write!(f, "}}")?;
                }
                Type::MetaType => write!(f, "type")?,
                Type::MetaField => write!(f, "field")?,
                Type::MetaVariable => write!(f, "field")?,
                Type::MetaCodeLocation => write!(f, "code location")?,
            }
        }
        Ok(())
    }
}

pub fn print_type_name(t: *const TypeInfo, out: &mut StyledText, palette: &Palette, recursion_depth: usize) {
    if recursion_depth > 100 {
        styled_write!(out, palette.truncation_indicator.2, "{}", palette.truncation_indicator.1);
        return;
    }
    unsafe {
        let t: &TypeInfo = &*t;
        match &t.t {
            Type::Pointer(p) => {
                styled_write!(out, palette.value_misc, "{} ", if p.flags.contains(PointerFlags::REFERENCE) {'&'} else {'*'});
                print_type_name((*p).type_, out, palette, recursion_depth + 1);
                return;
            }
            Type::Array(a) => {
                // Print length before name (instead of e.g. Rust syntax [type; len]) because name may be long and cut off.
                if a.flags.contains(ArrayFlags::UTF_STRING) {
                    styled_write!(out, palette.value, "str");
                }
                styled_write!(out, palette.value_misc, "[");
                if a.flags.contains(ArrayFlags::LEN_KNOWN) {
                    styled_write!(out, palette.value, "{}", a.len);
                } else {
                    styled_write!(out, palette.value_misc, "<length unknown>");
                }
                styled_write!(out, palette.value_misc, "] ");
                print_type_name((*a).type_, out, palette, recursion_depth + 1);
                return;
            }
            Type::Slice(s) => {
                if s.flags.contains(SliceFlags::UTF_STRING) {
                    styled_write!(out, palette.value, "&str ");
                } else {
                    styled_write!(out, palette.value_misc, "&[] ");
                }
                print_type_name((*s).type_, out, palette, recursion_depth + 1);
                return;
            }
            _ => (),
        }
        if t.name.is_empty() {
            styled_write!(out, palette.type_name, "<{} @{:x}>", t.t.kind_name(), t.die.0);
        } else {
            styled_write!(out, palette.type_name, "{}", t.name);
        }
    }
}

unsafe impl Send for PointerType {}
unsafe impl Sync for PointerType {}
unsafe impl Send for ArrayType {}
unsafe impl Sync for ArrayType {}
unsafe impl Send for SliceType {}
unsafe impl Sync for SliceType {}
unsafe impl Send for StructField {}
unsafe impl Sync for StructField {}
unsafe impl Send for StructType {}
unsafe impl Sync for StructType {}
unsafe impl Send for EnumType {}
unsafe impl Sync for EnumType {}
unsafe impl Send for BuiltinTypes {}
unsafe impl Sync for BuiltinTypes {}

// ======================= Types loading and deduplication code below =====================================

// Type loading proceeds in 6 stages (sorry it's so many):
// -1. DWARF info traversal: load types in each shard independently, into temp_types, with POINTERS_NOT_RESOLVED flag.
//  0. First map-reduce: group by name, match up declarations with definitions. Prefer definitions in the same unit as the declaration.
//  1. Graph traversal: deduplicate types, resolve aliases, and copy to final arenas (except names).
//  2. Second map-reduce: group by name, detect name collisions and generate artificial names (like "foo" -> "foo#2"), copy names to final arenas.
//  3. Third map-reduce: group by *const TypeInfo, pick the best name to represent each type.
//  4. Resolve type references (i.e. map DieOffset -> *const TypeInfo) from other debug info (e.g. variables).
// Step 1 is the only one that involves any mutable state shared among threads, and it's by far the trickiest.
// It does a parallel DFS, with threads contending for access to TypeLoadState-s (protected by spinlocks)
// and dedup_maps (protected by mutexes).
// (By "map-reduce" I mean first having each shard add messages into its row of n^2 lists: [source shard][destination shard = hash(key)%num_shards],
//  then having each shard collect messages from its column of the same matrix, bucket the messages by key, and do something with them.
//  I'm sure there's a proper name for this technique, instead of "map-reduce", but I don't know it.)

type DieOffset = DebugInfoOffset<usize>;

pub struct TypesLoadingShard {
    pub temp_types: Types,

    pub types_before_dedup: usize,

    units: Vec<(Range<DieOffset>, Range<usize>)>,
    finalized: bool,
    // Sorted by offset.
    offset_map: Vec<TypeLoadState>,
    // Outgoing messages for map-reduce stages. Index is destination shard idx.
    // (This is O(num_threads^2) arrays, which seems fine up to maybe hundreds of threads, e.g. 2023 threadripper (not tested).
    //  For future CPUs with 1000 cores, maybe we'll have to add two-step routing, or maybe just use 100 cores.)
    send_decl_def_names: Vec<CachePadded<SyncUnsafeCell<Vec<NameDeclDefMessage>>>>,
    send_dedup_names: Vec<CachePadded<SyncUnsafeCell<Vec<NameDedupMessage>>>>,
    send_assign_names: Vec<CachePadded<SyncUnsafeCell<Vec<NameDedupMessage>>>>,

    temp_fields_arena: CachePadded<SyncUnsafeCell<Arena>>,
    final_types: CachePadded<SyncUnsafeCell<Types>>, // accessed only by the corresponding shard, no synchronization needed

    warn: SyncUnsafeCell<Limiter>,
}

pub struct TypesLoader {
    pub builtin_types: BuiltinTypes,
    shards: Vec<TypesLoadingShard>,
    // We map whole units to shards for locality, instead of e.g. hash(DieOffset)%num_shards.
    units: Vec<UnitInfo>,
    // Point to a fake unit in one of the shards, with high DieOffset-s.
    // Sharded hash map for deduplicating types by content. Usually has more shards than `shards`.
    dedup_maps: Vec<CachePadded<Mutex<HashMap<DedupKey, (*const TypeInfo, TypeSpecialInfo)>>>>, // no other mutex/spinlock may be held under this one
}
unsafe impl Send for TypesLoader {}
unsafe impl Sync for TypesLoader {}

#[derive(Copy, Clone)]
struct UnitInfo {
    // Packed:
    //  48 bits - DieOffset
    //  16 bits - shard idx
    //  32 bits - first index in offset_map
    //  32 bits - number of elements in offset_map
    data: [usize; 2],
}
impl UnitInfo {
    fn new(offset: DieOffset, shard_idx: usize, offset_map_range: Range<usize>) -> Self { assert!(offset.0 >> 48 == 0 && shard_idx >> 16 == 0 && offset_map_range.start >> 32 == 0 && offset_map_range.len() >> 32 == 0); Self {data: [offset.0 << 16 | shard_idx, offset_map_range.start << 32 | offset_map_range.len()]} }
    fn offset(&self) -> DieOffset { DebugInfoOffset(self.data[0] >> 16) }
    fn shard(&self) -> usize { self.data[0] & 0xffff }
    fn offset_map_range(&self) -> Range<usize> { self.data[1]>>32 .. self.data[1] & 0xffffffff }
    fn offset_comparison_key(offset: DieOffset) -> usize { assert!(offset.0 >> 48 == 0); offset.0 << 16 | 0xffff }
}

// Set iff the TypeLoadState.state is Final. After this, the state is immutable (but the type it points to may be mutable).
// If FLAG_WAITING_FOR_DFS is also set, the TypeInfo is a dummy value (but at the final address), to be fixed up by the corresponding DFS on the way back.
const FLAG_FINALIZED: usize = 0x1;
// Spinlock protecting TypeLoadState.state when FLAG_FINALIZED is not set.
// Don't wait for this lock (or for FLAG_WAITING_FOR_DFS) while holding any other mutex/spinlock in this file.
const FLAG_LOCKED: usize = 0x2;
// This vertex is not finalized, and some thread (with index flags >> NUM_FLAGS) is processing its DFS subtree.
// Note that this flag may be unset even before the DFS unwinds back to this vertex:
//  * If a cycle is detected, the first non-Alias vertex of the cycle gets finalized by the inner dfs call for this vertex, before returning to the outer call.
//  * If a thread with higher index reaches this vertex, it steals ownership of this vertex (assigns flags >> NUM_FLAGS).
//    Then the original thread leaves the vertex alone, and the new thread is in charge of finalizing it (unless an even-higher-indexed thread takes over, etc).
//    The new thread may finalize the vertex before the original thread returned to it. This prevents the original thread from detecting a cycle, which is fine.
const FLAG_WAITING_FOR_DFS: usize = 0x4;
// What do we do if we detect a cycle in the graph, i.e. if DFS visits a vertex that's already on the stack?
// We just finalize that vertex, leaving its TypeInfo with POINTERS_NOT_RESOLVED flag, and skipping deduplication for that one vertex
// (just because its pointers are not resolved) (deduping its predecessors is still fine, it just won't be very effective).
// There's an inconvenience: the vertex may be an Alias, having no TypeInfo to finalize and point to. In that case we continue the DFS, following the aliases,
// until we encounter a non-Alias vertex, which we then finalize and return. When we follow aliases this way, we set FLAG_CYCLE; if we see a vertex with FLAG_CYCLE,
// that means there's a cycle of aliases (i.e. malformed DWARF), in which case we point all these aliases to an Unknown type instead.
const FLAG_CYCLE: usize = 0x8;
const NUM_FLAGS: u32 = 4;

struct TypeLoadState {
    offset: DieOffset,
    // First NUM_FLAGS bits are flags (FLAG_*).
    // Bits NUM_FLAGS+ are the index of the thread if FLAG_WAITING_FOR_DFS is set.
    flags: AtomicUsize,
    state: SyncUnsafeCell<TypeLoadStateEnum>,
}
impl TypeLoadState {
    // Returns new flags. If FLAG_FINALIZED is found, doesn't lock.
    fn lock(&self, mut flags: usize, shard_idx: usize) -> usize {
        loop {
            assert_eq!(flags & (FLAG_LOCKED | FLAG_FINALIZED), 0);
            assert!(flags & FLAG_WAITING_FOR_DFS == 0 || (flags >> NUM_FLAGS) <= shard_idx);
            let new_flags = flags | FLAG_LOCKED;
            match self.flags.compare_exchange_weak(flags, new_flags, Ordering::Acquire, /*because we read TypeLoadStateEnum if FLAG_FINALIZED is set*/ Ordering::Acquire) {
                Ok(prev) => {
                    // Acquired the lock.
                    assert_eq!(prev, flags);
                    return new_flags;
                }
                Err(prev) => flags = prev,
            }
            if flags & FLAG_FINALIZED != 0 {
                assert!(flags & FLAG_LOCKED == 0);
                return flags;
            }
            if flags & FLAG_WAITING_FOR_DFS != 0 && (flags >> NUM_FLAGS) > shard_idx {
                // Wait for the higher-priority shard to finish processing this whole DFS subtree.
                flags &= ((1 << NUM_FLAGS) - 1) & !FLAG_WAITING_FOR_DFS & !FLAG_LOCKED;
                hint::spin_loop();
                continue;
            }
            if flags & FLAG_LOCKED != 0 {
                flags &= !FLAG_LOCKED;
                hint::spin_loop();
                continue;
            }
        }
    }

    fn finalize_and_unlock(&self, val: *const TypeInfo, special: TypeSpecialInfo, extra_flags: usize) -> (*const TypeInfo, TypeSpecialInfo) {
        unsafe {(*self.state.get()) = TypeLoadStateEnum::Final((val, special))};
        let prev = self.flags.swap(FLAG_FINALIZED | extra_flags, Ordering::Release);
        assert_eq!(prev & (FLAG_LOCKED | FLAG_FINALIZED), FLAG_LOCKED);
        (val, special)
    }

    fn is_alias(&self) -> bool { unsafe { match &*self.state.get() { TypeLoadStateEnum::Alias(_) => true, _ => false } } }
    fn is_temp(&self) -> bool { unsafe { match &*self.state.get() { TypeLoadStateEnum::Temp(_) => true, _ => false } } }
    fn as_final(&self) -> (*const TypeInfo, TypeSpecialInfo) { unsafe { match &*self.state.get() { TypeLoadStateEnum::Final(x) => x.clone(), _ => panic!("finalized type expected") } } }
}

enum TypeLoadStateEnum {
    Alias((DieOffset, &'static str)),
    // Lives in temp_types, type pointers are DieOffset-s. At most one TypeLoadState may point to each TypeInfo.
    Temp(*mut TypeInfo),
    // Lives in final_types. There can be multiple pointers to each TypeInfo. The pointer is final, but the TypeInfo contents are not.
    // During DFS, only the "owner" of this TypeInfo is allowed to dereference the pointer. The owner is whoever added this TypeInfo into final_types.
    // If the dfs caller needs to know some information from such TypeInfo, this information can be duplicated in TypeSpecialInfo
    // (the TypeSpecialInfo may be incorrect if this type is part of a cycle, but the current usage doesn't care about that).
    Final((*const TypeInfo, TypeSpecialInfo)),
}
unsafe impl Send for TypeLoadStateEnum {}
unsafe impl Sync for TypeLoadStateEnum {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeSpecialInfo {
    None,
    AmbiguousChar {signed: bool},
}
impl TypeSpecialInfo {
    fn is_none(self) -> bool { match self { TypeSpecialInfo::None => true, _ => false } }
    fn extract(t: &TypeInfo) -> Self {
        match &t.t {
            Type::Primitive(p) if p.contains(PrimitiveFlags::AMBIGUOUS_CHAR) => TypeSpecialInfo::AmbiguousChar {signed: p.contains(PrimitiveFlags::SIGNED)},
            _ => TypeSpecialInfo::None,
        }
    }
}

// Information about type names exchanged among threads to connect type declarations to definitions.
struct NameDeclDefMessage {
    name_hash: usize,
    name: &'static str,
    offset: DieOffset, // must be unique across all ExchangedTypeName-s for a binary
    declaration: bool,
}

#[derive(Clone)]
struct NameDedupMessage {
    name_hash: usize,
    name: &'static str,
    type_: *const TypeInfo,
    unpriority: u8,

    from_alias: Option<DieOffset>,
    adjusted_to_avoid_name_collision: bool,
}
unsafe impl Send for NameDedupMessage {}
unsafe impl Sync for NameDedupMessage {}
impl Default for NameDedupMessage { fn default() -> Self { Self {name_hash: 0, name: "", type_: ptr::null(), unpriority: 0, from_alias: None, adjusted_to_avoid_name_collision: false} } }

#[derive(Hash, PartialEq, Eq, Clone)]
struct DedupKey {
    name: &'static str,
    size: usize,
    t: Type,
}


impl TypesLoadingShard {
    pub fn invalid() -> Self { Self::new(0) }
    
    pub fn new(num_shards: usize) -> Self {
        Self {
            temp_types: Types::new(),
            offset_map: Vec::new(),
            send_decl_def_names: (0..num_shards).map(|_| CachePadded::new(SyncUnsafeCell::new(Vec::new()))).collect(),
            send_dedup_names: (0..num_shards).map(|_| CachePadded::new(SyncUnsafeCell::new(Vec::new()))).collect(),
            send_assign_names: (0..num_shards).map(|_| CachePadded::new(SyncUnsafeCell::new(Vec::new()))).collect(),
            temp_fields_arena: CachePadded::new(SyncUnsafeCell::new(Arena::new())),
            final_types: CachePadded::new(SyncUnsafeCell::new(Types::new())),
            units: Vec::new(),
            finalized: false,
            types_before_dedup: 0,
            warn: SyncUnsafeCell::new(Limiter::new()),
        }
    }

    pub fn set_unit(&mut self, start: DieOffset, end: DieOffset) {
        self.units.push((start..end, 0..0));
    }

    // Adds to temp_types, updates other data structures as needed.
    // Pointers/references inside TypeInfo (e.g. name or fields) should be allocated on temp_types arenas or outlive the TypesLoader (e.g. point to the binary mmap).
    // For type names, the id in temp_types.unsorted_type_names is ignored, just use 0.
    // If the type has a name, it must be assigned before calling this. Same for TypeFlags::LINKABLE_NAME.
    // Ok to assign other fields later, or add fields/enumerands etc.
    // Returns (real pointer, DieOffset transmuted into pointer). The latter is what should be used inside other TypeInfo-s (during symbols loading)!
    pub fn add_type(&mut self, mut t: TypeInfo) -> (*const TypeInfo, *const TypeInfo) {
        assert!(self.units.last().unwrap().0.contains(&t.die));
        self.types_before_dedup += 1;
        t.flags.insert(TypeFlags::POINTERS_NOT_RESOLVED);
        if !t.name.is_empty() && t.flags.contains(TypeFlags::LINKABLE_NAME) && t.can_be_forward_declared() {
            let mut hasher = DefaultHasher::new();
            t.name.hash(&mut hasher);
            let hash = hasher.finish() as usize;
            let target_shard = hash % self.send_decl_def_names.len();
            let msg = NameDeclDefMessage {name_hash: hash, name: t.name, offset: t.die, declaration: t.flags.contains(TypeFlags::DECLARATION)};
            unsafe {(*self.send_decl_def_names[target_shard].get()).push(msg)};
        }
        let t = self.temp_types.types_arena.add_mut(t);
        self.offset_map.push(TypeLoadState {offset: t.die, flags: 0.into(), state: TypeLoadStateEnum::Temp(t as _).into()});
        (t, t.die.0 as _)
    }

    // The name must live in temp_types.unsorted_type_names or be static.
    pub fn add_alias(&mut self, offset: DieOffset, target: DieOffset, name: &'static str) {
        assert!(self.units.last().unwrap().0.contains(&offset));
        self.offset_map.push(TypeLoadState {offset, flags: 0.into(), state: TypeLoadStateEnum::Alias((target, name)).into()});
    }

    pub fn finalize(&mut self) {
        assert!(!self.finalized);
        //let _prof = ProfileScope::with_threshold(0.01, format!("sorting type offsets")); // ~30ms
        self.units.sort_unstable_by_key(|u| u.0.start);
        self.offset_map.sort_unstable_by_key(|s| s.offset);
        let mut map_idx = 0usize;
        for unit_idx in 0..self.units.len() {
            let u = &mut self.units[unit_idx];
            assert!(self.offset_map.get(map_idx).map_or(true, |s| s.offset >= u.0.start));
            u.1.start = map_idx;
            while map_idx < self.offset_map.len() && self.offset_map[map_idx].offset.0 < u.0.end.0 {
                map_idx += 1;
            }
            u.1.end = map_idx;
        }
        assert_eq!(map_idx, self.offset_map.len());
        self.finalized = true;
    }

    pub fn num_offsets(&self) -> usize { self.offset_map.len() }
    pub fn offset_map_bytes(&self) -> usize { self.offset_map.capacity() * (mem::size_of::<TypeLoadState>()) }

    fn find_offset(&self, offset: DieOffset, unit_range: Range<usize>) -> Option<&TypeLoadState> {
        let i = self.offset_map.partition_point(|s| s.offset <= offset);
        if i > 0 && self.offset_map[i-1].offset == offset {
            Some(&self.offset_map[i-1])
        } else {
            None
        }
    }
}

impl TypesLoader {
    pub fn invalid() -> Self { TypesLoader {shards: Vec::new(), units: Vec::new(), dedup_maps: Vec::new(), builtin_types: BuiltinTypes::invalid()} }

    pub fn create(num_shards: usize) -> (Self, Vec<TypesLoadingShard>) {
        let mut shards: Vec<TypesLoadingShard> = (0..num_shards).map(|_| TypesLoadingShard::new(num_shards)).collect();

        let mut off = FAKE_DWARF_OFFSET_START;
        shards[0].set_unit(DebugInfoOffset(off), DebugInfoOffset((1 << 48) - 1));
        let builtin_types = BuiltinTypes::create(|mut t| { t.die.0 = off; off += 1; shards[0].add_type(t).1 });

        let num_dedup_maps = (shards.len() * 100).min(10000);
        let res = Self {shards: Vec::new(), units: Vec::new(), dedup_maps: (0..num_dedup_maps).map(|_| CachePadded::new(Mutex::new(HashMap::new()))).collect(), builtin_types};
        (res, shards)
    }

    pub fn shards_ready(&mut self, shards: Vec<TypesLoadingShard>) {
        let mut units: Vec<(UnitInfo, /*end*/ DieOffset)> = Vec::new();
        for i in 0..shards.len() {
            assert!(shards[i].finalized);
            assert_eq!(shards[i].send_dedup_names.len(), shards.len());
            for (die_range, map_range) in &shards[i].units {
                units.push((UnitInfo::new(die_range.start, i, map_range.clone()), die_range.end));
            }
        }
        units.sort_unstable_by_key(|t| t.0.data[0]);
        if units.len() > 2 {
            for i in 1..units.len()-1 {
                assert_eq!(units[i].0.offset(), units[i-1].1);
            }
        }
        self.shards = shards;
        self.units = units.iter().map(|t| t.0).collect();
    }

    pub fn num_stages() -> usize { 4 }

    // Run stage 0 for all shards (possibly in parallel), wait for all to complete, then run stage 1 for all shards, etc.
    // TODO: Progress indication.
    pub fn run(&self, stage: usize, shard_idx: usize, progress_callback: &dyn Fn(f64)) {
        match stage {
            0 => self.run_match_declarations(shard_idx),
            1 => self.run_dfs(shard_idx, progress_callback),
            2 => self.run_dedup_names(shard_idx),
            3 => self.run_assign_names(shard_idx),
            _ => panic!("unexpected stage number"),
        }
    }

    pub fn find_final_type(&self, offset: DieOffset) -> *const TypeInfo {
        if let (_, _, Some(s)) = self.find_offset(offset) {
            return s.as_final().0;
        }
        self.find_offset(DebugInfoOffset(self.builtin_types.unknown as usize)).2.unwrap().as_final().0
    }

    pub fn dedup_maps_bytes(&self) -> usize {
        self.dedup_maps.iter().map(|m| m.lock().unwrap().capacity() * (mem::size_of::<(DedupKey, (*const TypeInfo, TypeSpecialInfo))>() + /*hash*/ 8)).sum()
    }

    pub fn into_results(mut self) -> (Vec<Types>, BuiltinTypes) {
        let builtin = self.builtin_types.map(|t| self.find_final_type(DebugInfoOffset(t as usize)));
        let mut ts: Vec<Types> = Vec::new();
        for shard in &mut self.shards {
            ts.push(mem::replace(shard.final_types.get_mut(), Types::new()));
        }
        (ts, builtin)
    }

    // Point each declaration to the nearest definition with the same name. This doesn't apply to names in anonymous namespaces or inside functions.
    // (We could additionally deduplicate the defitions with the same name, but that's not fully correct as different
    // compilation units may contain unrelated types with the same name, although this is usually avoided in practice.
    // Instead, we do more-correct deduplication by struct contents, during DFS.)
    fn run_match_declarations(&self, shard_idx: usize) {
        let mut names: Vec<NameDeclDefMessage> = Vec::new();
        for other_shard in &self.shards {
            // We access only the array element destined for our shard, no lock needed.
            // The mem::take is to deallocate memory as we go.
            names.append(&mut mem::take(unsafe {&mut *other_shard.send_decl_def_names[shard_idx].get()}));
        }
        names.sort_unstable_by_key(|k| (k.name_hash, k.declaration, k.offset)); // definitions first

        let mut def_start = 0;
        while def_start < names.len() { // loop over different names
            let h = names[def_start].name_hash;
            let mut decl_start = def_start;
            while decl_start < names.len() && names[decl_start].name_hash == h && !names[decl_start].declaration {
                decl_start += 1;
            }
            let mut end = decl_start;
            while end < names.len() && names[end].name_hash == h {
                end += 1;
            }

            if def_start < decl_start && decl_start < end {
                let mut def = def_start; // two-pointers
                for decl in decl_start..end {
                    // Find nearest definition to this declaration.
                    let decl_off = names[decl].offset;
                    let (decl_shard, decl_unit, decl_state) = self.find_offset(decl_off);
                    let decl_state = decl_state.unwrap();
                    while def + 1 < decl_start && names[def + 1].offset < decl_off {
                        def += 1;
                    }
                    // Prefer definition in the same compilation unit as the declaration.
                    if def + 1 < decl_start && names[def].offset < decl_off && self.offset_to_unit(names[def + 1].offset) == decl_unit {
                        def += 1;
                    }

                    // Turn the declaration into an alias to the definition.
                    // Offsets are unique, no lock needed.
                    assert!(decl_state.is_temp());
                    unsafe {*decl_state.state.get() = TypeLoadStateEnum::Alias((names[def].offset, ""))};
                }
            }

            def_start = end;
        }
    }

    fn offset_to_unit(&self, offset: DieOffset) -> usize {
        let key = UnitInfo::offset_comparison_key(offset);
        self.units.partition_point(|u| u.data[0] <= key).saturating_sub(1)
    }
    fn find_offset(&self, offset: DieOffset) -> (/*shard*/ usize, /*unit*/ usize, Option<&TypeLoadState>) {
        let i = self.offset_to_unit(offset);
        let u = &self.units[i];
        let s = u.shard();
        (s, i, self.shards[s].find_offset(offset, u.offset_map_range()))
    }

    fn send_dedup_name(&self, original_shard_idx: usize, mut msg: NameDedupMessage) {
        let mut hasher = DefaultHasher::new();
        msg.name.hash(&mut hasher);
        msg.name_hash = hasher.finish() as usize;
        let target_shard = msg.name_hash % self.shards.len();
        unsafe {(*self.shards[original_shard_idx].send_dedup_names[target_shard].get()).push(msg)};
    }

    fn send_assign_name(&self, original_shard_idx: usize, msg: NameDedupMessage) {
        let mut hasher = DefaultHasher::new();
        msg.type_.hash(&mut hasher);
        let target_shard = hasher.finish() as usize % self.shards.len();
        unsafe {(*self.shards[original_shard_idx].send_assign_names[target_shard].get()).push(msg)};
    }
    
    fn run_dfs(&self, shard_idx: usize, progress_callback: &dyn Fn(f64)) {
        let shard = &self.shards[shard_idx];
        let progress_every = shard.offset_map.len() / 100 + 1;
        let mut next_progress_idx = progress_every;
        for (i, state) in shard.offset_map.iter().enumerate() {
            self.dfs(shard_idx, shard_idx, state, /*on_cycle*/ false);
            if i >= next_progress_idx {
                progress_callback(i as f64 / shard.offset_map.len() as f64);
                next_progress_idx = i + progress_every;
            }
        }
    }

    fn find_and_dfs(&self, original_shard_idx: usize, offset: DieOffset, on_cycle: bool) -> (*const TypeInfo, TypeSpecialInfo) {
        let (vertex_shard_idx, _, state) = self.find_offset(offset);
        match state {
            Some(state) => self.dfs(original_shard_idx, vertex_shard_idx, state, on_cycle),
            None => {
                if unsafe {(*self.shards[original_shard_idx].warn.get()).check(line!())} { eprintln!("warning: invalid DIE reference: 0x{:x}", offset.0); }
                let r = unsafe {(*self.shards[original_shard_idx].final_types.get()).types_arena.add(TypeInfo {name: "<bad DIE offset>", ..TypeInfo::default()}) as _};
                (r, TypeSpecialInfo::extract(unsafe {&*r}))
            }
        }
    }
    
    // All threads do a depth-first search on the graph simultaneously, potentially contending on access to the same vertices.
    // We use flags to synchronize that access.
    fn dfs(&self, original_shard_idx: usize, vertex_shard_idx: usize, state: &TypeLoadState, mut on_cycle: bool) -> (*const TypeInfo, TypeSpecialInfo) {
        // Lock the state, or return if already finalized.
        let mut flags = state.lock(0, original_shard_idx);
        if flags & FLAG_FINALIZED != 0 {
            assert!(flags & FLAG_LOCKED == 0);
            // Finalized state is immutable, lock not needed.
            return state.as_final();
        }
        assert!(flags & FLAG_LOCKED != 0);
        // We must unlock the state on all code paths before returning or recursing.

        let alias_cycle = flags & FLAG_CYCLE != 0 && (flags >> NUM_FLAGS) == original_shard_idx;

        if alias_cycle {
            assert!(on_cycle);
            assert!(flags & FLAG_WAITING_FOR_DFS != 0);
            assert!(state.is_alias());
        }

        // Copy the TypeInfo or DieOffset from the vertex.
        let info: result::Result<TypeInfo, (DieOffset, &'static str)> = match unsafe {&*state.state.get()} {
            TypeLoadStateEnum::Alias((off, name)) => {
                if alias_cycle {
                    if unsafe {(*self.shards[original_shard_idx].warn.get()).check(line!())} { eprintln!("warning: alias cycle @0x{:x}", state.offset.0); }
                    // Add a fake Unknown type, don't bother with deduplication.
                    let res: *const TypeInfo = unsafe {(*self.shards[original_shard_idx].final_types.get()).types_arena.add(TypeInfo {name: "<alias cycle>", ..TypeInfo::default()}) as _};
                    return state.finalize_and_unlock(res, TypeSpecialInfo::extract(unsafe {&*res}), 0);
                } else {
                    Err((*off, name))
                }
            }
            TypeLoadStateEnum::Temp(t) => Ok(unsafe {(**t).clone()}),
            TypeLoadStateEnum::Final(_) => panic!("FLAG_FINALIZED was supposed to be set"),
        };

        // If a cycle is detected, and this vertex is not an Alias, skip recursion and dedup, and finalize right away.
        on_cycle |= flags & FLAG_WAITING_FOR_DFS != 0 && (flags >> NUM_FLAGS) == original_shard_idx;
        if on_cycle && info.is_ok() {
            // The current DFS stack (i.e. current call stack) contains this vertex twice: here and higher up the stack.
            // Add a dummy type to be replaced later when we return to the call higher up the stack.
            // The *address* of that type must be final though, so that other types can be pointed to it.
            let res: *const TypeInfo = unsafe {(*self.shards[original_shard_idx].final_types.get()).types_arena.add(TypeInfo {flags: TypeFlags::POINTERS_NOT_RESOLVED, ..TypeInfo::default()})};
            return state.finalize_and_unlock(res, TypeSpecialInfo::extract(unsafe {&*res}), FLAG_WAITING_FOR_DFS | (original_shard_idx << NUM_FLAGS));
        }

        // Function to unlock the vertex. We call it it lazily, only if recursion is needed.
        let mut unlock = || {
            assert!(flags & FLAG_LOCKED != 0);
            let mut new_flags = FLAG_WAITING_FOR_DFS | (original_shard_idx << NUM_FLAGS);
            if on_cycle {
                new_flags |= FLAG_CYCLE;
            }
            let prev_flags = state.flags.swap(new_flags, Ordering::Release);
            assert_eq!(prev_flags & (FLAG_LOCKED | FLAG_FINALIZED), FLAG_LOCKED);
            flags = new_flags;
        };

        // Recurse.
        // Note that while we do this, another thread may steal and finalize this vertex, obsoleting what we're doing.
        // That's ok because we're operating on a copy of the TypeInfo here, and will lock+re-check the vertex state before finalizing.
        let mut dedup: Option<DedupKey> = None;
        let info: TypeInfo = match info {
            Err((off, name)) => {
                // An alias. Recurse, resolve the alias, and return. (Because almost none of the code below applies to aliases.)
                unlock();

                let (res, special) = self.find_and_dfs(original_shard_idx, off, on_cycle);

                flags = state.lock(flags, original_shard_idx);
                if flags & FLAG_FINALIZED != 0 {
                    assert_eq!(flags & (FLAG_LOCKED | FLAG_WAITING_FOR_DFS | FLAG_CYCLE), 0);
                    return state.as_final();
                }
                if let Some(mut info) = detect_cpp_non_char_8bit_type_from_typedef(name, special) {
                    // Something like `typedef char int8_t`. Introduce a new non-char i8 type instead of aliasing to char.
                    info.flags.insert(TypeFlags::POINTERS_NOT_RESOLVED);
                    info
                } else {
                    state.finalize_and_unlock(res, special, 0);
                    if !name.is_empty() {
                        self.send_dedup_name(original_shard_idx, NameDedupMessage {name, type_: res, from_alias: Some(/*not to be confused with `off`*/ state.offset), unpriority: 255, ..Default::default()});
                    }
                    return (res, special);
                }
            }
            Ok(mut info) => {
                let do_dedup = true;
                let mut dedup_with_name = false;
                match &mut info.t {
                    Type::Unknown | Type::Primitive(_) => (),
                    Type::MetaType | Type::MetaField | Type::MetaVariable | Type::MetaCodeLocation | Type::Slice(_) => (), // these don't appear in debug symbols, but we add them as builtins along the way, so end up participating in deduplication unnecessarily
                    Type::Pointer(p) => {
                        unlock();
                        p.type_ = self.find_and_dfs(original_shard_idx, DebugInfoOffset(p.type_ as usize), on_cycle).0;
                    }
                    Type::Array(a) => {
                        unlock();
                        a.type_ = self.find_and_dfs(original_shard_idx, DebugInfoOffset(a.type_ as usize), on_cycle).0;
                    }
                    Type::Struct(s) => {
                        // Can't modify fields in-place because it would race with other threads.
                        let new_fields = unsafe {(*self.shards[original_shard_idx].temp_fields_arena.get()).add_slice_mut(s.fields())};
                        unlock();
                        for f in &mut *new_fields {
                            f.type_ = self.find_and_dfs(original_shard_idx, DebugInfoOffset(f.type_ as usize), on_cycle).0;
                        }
                        s.fields_ptr = new_fields.as_ptr();
                        dedup_with_name = true;
                    }
                    Type::Enum(e) => {
                        unlock();
                        e.type_ = self.find_and_dfs(original_shard_idx, DebugInfoOffset(e.type_ as usize), on_cycle).0;
                        dedup_with_name = true;
                    }
                }
                if do_dedup {
                    // The dedup key hashing+comparison is shallow, it compares pointers to other types rather than whole subgraphs.
                    // This is correct because those subgraphs have already been deduplicated at this point.
                    dedup = Some(DedupKey {name: if dedup_with_name { info.name } else { "" }, size: info.size, t: info.t.clone()});
                }
                info
            }
        };

        // Lock and re-check the vertex state.
        let mut ptr: Option<(*mut TypeInfo, TypeSpecialInfo)> = None;
        let final_types: &mut Types = unsafe {&mut *self.shards[original_shard_idx].final_types.get()};
        if flags & FLAG_LOCKED == 0 {
            flags = state.lock(flags, original_shard_idx);
            if flags & FLAG_FINALIZED != 0 {
                if flags & FLAG_WAITING_FOR_DFS == 0 || (flags >> NUM_FLAGS) > original_shard_idx {
                    return state.as_final();
                }
                assert_eq!(flags >> NUM_FLAGS, original_shard_idx);
                // Reassign dummy type that we created to break a cycle. Must use existing address. FLAG_LOCKED is not needed.
                // Fields/enumerands will be moved to final arena separately, below.
                // (We should in theory update the TypeSpecialInfo in this case, but we can't do it, and it doesn't matter.)
                let (p, special) = state.as_final();
                let p = p as *mut TypeInfo;
                unsafe {(*p) = info.clone()};
                ptr = Some((p, special));
            } else {
                assert!(flags & FLAG_LOCKED != 0);
            }
        }
        let mut get_or_add_ptr = || -> (*mut TypeInfo, TypeSpecialInfo) {
            match &ptr {
                Some(p) => *p,
                None => {
                    let p = final_types.types_arena.add_mut(info.clone());
                    let special = TypeSpecialInfo::extract(unsafe {&*p});
                    ptr = Some((p, special));
                    (p, special)
                }
            }
        };

        // Check if duplicate.
        if let Some(dedup) = dedup {
            let mut hasher = DefaultHasher::new();
            dedup.hash(&mut hasher);
            let hash = hasher.finish();
            // Hash it again to avoid resonating with HashMap's hash->slot mapping. Should switch this to some very fast simple integer hash function.
            // (Or we can use a different hash_map::RandomState, but then we can't reuse this hash for the HashMap.)
            let mut hasher = DefaultHasher::new();
            hash.hash(&mut hasher);
            let hash = hasher.finish() as usize;

            let dedup_shard = hash % self.dedup_maps.len();
            let mut dedup_lock = self.dedup_maps[dedup_shard].lock().unwrap();
            // Unfortunate that we calculate the hash twice. Can try using hashbrown directly (which supports precalculated hashes), or at least wrap the key in some kind of Prehashed struct (and maybe a custom hasher).
            match dedup_lock.entry(dedup.clone()) {
                Entry::Vacant(v) => {
                    let (p, special) = get_or_add_ptr();
                    v.insert((p as *const TypeInfo, special));
                }
                Entry::Occupied(o) => {
                    if flags & FLAG_FINALIZED != 0 {
                        // Can't dedup, we committed (in order to break a cycle) to having a valid TypeInfo at address `ptr` (because there may already be other pointers to it).
                        // (Also, maybe this is unreachable, I'm not sure.)
                    } else {
                        let (res, special) = *o.get();
                        if !info.name.is_empty() && dedup.name.is_empty() {
                            self.send_dedup_name(original_shard_idx, NameDedupMessage {name: info.name, type_: res, unpriority: name_dedup_unpriority(&info), ..Default::default()});
                        }
                        return state.finalize_and_unlock(res, special, 0);
                    }
                }
            }
        }

        let (p, special) = get_or_add_ptr();
        let t: &mut TypeInfo = unsafe {&mut *(p as *mut _)};
        assert!(t.flags.contains(TypeFlags::POINTERS_NOT_RESOLVED));
        t.flags.remove(TypeFlags::POINTERS_NOT_RESOLVED);
        final_types.partially_import_and_adjust_field_names(t, unsafe {&mut *self.shards[original_shard_idx].temp_fields_arena.get()});

        if flags & FLAG_FINALIZED != 0 {
            let prev = state.flags.swap(FLAG_FINALIZED, Ordering::Relaxed);
            assert!(prev == FLAG_FINALIZED | FLAG_WAITING_FOR_DFS | (original_shard_idx << NUM_FLAGS));
        } else {
            state.finalize_and_unlock(p, special, 0);
        }

        if !t.nested_names.is_empty() {
            let new_nested_names = unsafe {&mut (*(final_types.misc_arena.add_slice_mut(t.nested_names) as *mut [(&'static str, NestedName)]))};
            for (name, n) in new_nested_names.iter_mut() {
                *name = final_types.misc_arena.add_str(*name);
                match n {
                    // (Alternatively, we could do this as a separate loading stage that does find_final_type() without dfs.)
                    NestedName::Type(type_) => *type_ = self.find_and_dfs(original_shard_idx, DebugInfoOffset(*type_ as usize), /*on_cycle*/ false).0,
                    NestedName::Variable(_) => (),
                }
            }
            t.nested_names = new_nested_names;
        }

        if !t.name.is_empty() {
            self.send_dedup_name(original_shard_idx, NameDedupMessage {name: t.name, type_: p, unpriority: name_dedup_unpriority(&info), ..Default::default()});

            if !t.can_have_own_name() {
                // For pointer/array typedefs, make them searchable by the typedef name, but don't use the name when printing type (print it as "* foo" instead of "foo_ptr_t").
                t.name = "";
            }
        }

        (p, special)
    }

    // For each name, look at the set of types with this name.
    // Adjust names to make them unique (e.g. "foo", "foo#2", "foo#3", etc).
    // Add names to final_types.sorted_type_names string table.
    fn run_dedup_names(&self, shard_idx: usize) {
        let final_types: &mut Types = unsafe {&mut *self.shards[shard_idx].final_types.get()};
        let mut names: Vec<NameDedupMessage> = Vec::new();
        for other_shard in &self.shards {
            names.append(unsafe {&mut *other_shard.send_dedup_names[shard_idx].get()});
        }
        // Sorting by hash would save ~0.5 seconds, but we want the table to be sorted by name to make binary searching easier in presence of the '#' suffixes.
        names.sort_unstable_by_key(|k| (k.name, k.type_, k.unpriority));

        let mut start = 0;
        while start < names.len() { // iterate over distinct names
            let mut end = start + 1;
            let mut num_types = 1;
            while end < names.len() && names[end].name_hash == names[start].name_hash {
                if names[end].type_ != names[end-1].type_ {
                    num_types += 1;
                }
                end += 1;
            }
            let suffix_helper = DisambiguatingSuffixes::new(end - start);
            let mut suffix_idx = 0;
            for i in start..end {
                if i > start && names[i].type_ == names[i-1].type_ {
                    continue;
                }
                let n = &mut names[i];
                let final_name;
                (final_name, n.adjusted_to_avoid_name_collision) = suffix_helper.disambiguate(n.name, suffix_idx, n.type_ as usize, &mut final_types.sorted_type_names);
                suffix_idx += 1;

                if unsafe {(*n.type_).can_have_own_name()} {
                    n.name_hash = 0;
                    n.name = final_name;
                    self.send_assign_name(shard_idx, n.clone());
                } else {
                    // Don't assign main name for things like pointers and arrays.
                    assert!(unsafe {(*n.type_).name.is_empty()});
                }
            }

            start = end;
        }
    }

    // For each type, look at its set of names.
    // Pick the "best" one as the main name for the type.
    //
    // In particluar, if a struct has no name but has a named typedef, promote the typedef name to struct name.
    // Useful in C:
    //   typedef struct {
    //     int quot;
    //     int rem;
    //   } div_t;
    fn run_assign_names(&self, shard_idx: usize) {
        let mut names: Vec<NameDedupMessage> = Vec::new();
        for other_shard in &self.shards {
            names.append(unsafe {&mut *other_shard.send_assign_names[shard_idx].get()});
        }
        names.sort_unstable_by_key(|k| (k.type_, k.from_alias.is_some()));

        let mut prev_type = ptr::null();
        for n in names {
            if n.type_ == prev_type {
                continue;
            }
            prev_type = n.type_;
            let t: &mut TypeInfo = unsafe {&mut *(n.type_ as *mut _)};
            t.name = n.name;
        }
    }
}

fn name_dedup_unpriority(t: &TypeInfo) -> u8 {
    // On name collisions, prefer debugger's built-in types over debuggee's types.
    // In particular, C++ programs often do `typedef char u8`, but we want `u8` to always refer to the non-char integer type.
    !t.flags.contains(TypeFlags::BUILTIN) as u8
}

// Heuristically detect things like `typedef char int8_t` and use a non-char integer type, i.e. it'll be printed as a number.
// TODO: This turned out to be pretty useless, we should probably delet dis, along with TypeSpecialInfo.
fn detect_cpp_non_char_8bit_type_from_typedef(name: &str, target: TypeSpecialInfo) -> Option<TypeInfo> {
    let signed = match target {
        TypeSpecialInfo::AmbiguousChar {signed} => signed,
        _ => return None,
    };
    // Faster check.
    if !name.ends_with("8") && !name.ends_with("8_t") && !name.ends_with("yte") && !name.ends_with("yte_t") {
        return None;
    }
    // Slower check.
    if name.find("byte").is_none() && name.find("i8").is_none() && name.find("u8").is_none() && name.find("int8").is_none() {
        return None;
    }
    let (name, flags) = if signed {("i8", PrimitiveFlags::SIGNED)} else {("u8", PrimitiveFlags::empty())};
    Some(TypeInfo {name, size: 1, flags: TypeFlags::SIZE_KNOWN, t: Type::Primitive(flags), ..Default::default()})
}
