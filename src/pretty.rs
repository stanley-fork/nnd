use crate::{*, types::*, error::*, expr::*, util::*, settings::*, common_ui::*, procfs::*};
use std::{mem, ptr, fmt::Write, borrow::Cow, io::Write as ioWrite};
use bitflags::*;

// Apply pretty-printers and other transformations to a value. Used both for printing and expression evaluaion.
// E.g. this function may turn an std::vector<int> into a *[int; 123], making it appear as an array both when printed whole
// and when used in expression like v[10] (will index the array) or v._M_begin (will fail even if std::vector has field _M_begin).
pub fn prettify_value(val: &mut Cow<Value>, warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    if val.flags.contains(ValueFlags::NO_UNWRAPPING_INTERNAL) {
        return Ok(());
    }

    // Plan:
    //  1. Downcast to concrete type.
    //  2. Unravel.
    //  3. Resolve discriminated union.
    //  4. Recognize containers.
    //  5. Unwrap single-field struct.

    let mut type_ = unsafe {&*val.type_};
    let mut struct_ = match &type_.t {
        Type::Struct(s) => s,
        _ => return Ok(()),
    };

    let mut allow_full_unwrap = true;

    if let Some(off) = find_vtable_ptr_field_offset(type_, struct_) {
        match downcast_to_concrete_type(val, off, context) {
            Ok(()) => {
                allow_full_unwrap = false;
                type_ = unsafe {&*val.type_};
                struct_ = match &type_.t {
                    Type::Struct(s) => s,
                    _ => return Ok(()),
                };
            }
            Err(e) => *warning = Some(e),
        }
    }

    let mut fields = Cow::Borrowed(struct_.fields());
    let mut additional_names: Vec<&'static str> = Vec::new();
    unravel_struct(&mut fields, Some(&mut additional_names));

    if resolve_discriminated_union(&val.val, &mut fields, warning, context)? {
        allow_full_unwrap = false;
    }

    match recognize_containers(val, &mut fields, &additional_names, warning, state, context) {
        Ok(true) => return Ok(()),
        Err(e) if !e.is_not_container() => *warning = Some(e),
        _ => (),
    }

    // Final level of unwrapping: replace the whole struct with its field. Unlike unravel_struct(), this works even if the field is not a struct.
    // E.g. turns unique_ptr into plain pointer. Useful for things like strong typedefs, Box/unique_ptr, Atomic/atomic, NonzeroUsize, Pin, etc.
    //
    // If the field is a pointer to the same struct, don't unwrap it; it's probably an std::forward_list;
    // if we unwrap, it gets formatted in a single line, as a long chain of addresses with no children, which is valid but confusing.
    let is_pointer_to = |ptr: *const TypeInfo, inner: *const TypeInfo| -> bool {
        let t = unsafe {&*ptr};
        t.t.as_pointer().is_some_and(|p| p.type_ == inner)
    };
    if allow_full_unwrap && fields.len() == 1 && !is_pointer_to(fields[0].type_, type_) {
        let field_val = get_struct_field(&val.val, &fields[0], context.memory)?;
        let new_val = Value {val: field_val, type_: fields[0].type_, flags: val.flags};
        *val = Cow::Owned(new_val);
        return Ok(());
    }

    // If we altered the set of fields, create a new struct type.
    if let Cow::Owned(fields) = fields {
        let val = val.to_mut();
        assert!(type_ as *const TypeInfo == val.type_);
        let mut new_type = type_.clone();
        let new_struct = new_type.t.as_struct_mut().unwrap();

        let new_fields = state.types.fields_arena.add_slice(&fields);
        new_struct.set_fields(new_fields);

        let mut w = state.types.unsorted_type_names.write();
        write!(w, "{}#pretty", type_.name).unwrap();
        new_type.name = w.finish_str(0);

        let new_type = state.types.types_arena.add(new_type);
        val.type_ = new_type;
    }

    Ok(())
}

// Append a span to Option<&mut StyledText> if it's Some. The Option must be mut.
macro_rules! styled_write_maybe {
    ($out:expr, $style:ident, $($arg:tt)*) => (
        if let Some((out, palette)) = &mut ($out) {
            styled_write!(out, palette.$style, $($arg)*);
        }
    );
}

// For a *TypeInfo or a *StructField, tells how to show this type or field in watches window.
// (Not a value of this type. The type itself. E.g. when using typeof().)
// The returned value is a struct that the user can explore using field access in watch expressions or by expanding the line in the watches window.
// E.g. if the *TypeInfo describes a struct, we'll return something like {name: "foo", size: 24, fields: {f1: <MetaField>, f2: <MetaField>}}.
pub fn reflect_meta_value(val: &Value, state: &mut EvalState, context: &EvalContext, mut out: Option<(&mut StyledText, &Palette)>) -> Value {
    let meta_t = unsafe {&*val.type_};
    let mut builder = StructBuilder::default();
    match meta_t.t {
        Type::MetaType => {
            let t = val.val.blob_ref().unwrap().get_usize().unwrap() as *const TypeInfo;
            let t = unsafe {&*t};
            let mut show_size = false;
            match &t.t {
                Type::Unknown => styled_write_maybe!(out, default, "unknown type"),
                Type::Primitive(p) => {
                    styled_write_maybe!(out, type_name, "{}",
                                        if p.contains(PrimitiveFlags::FLOAT) {
                                            "float"
                                        } else if p.contains(PrimitiveFlags::BOOL) {
                                            "bool"
                                        } else if p.contains(PrimitiveFlags::CHAR) {
                                            "char"
                                        } else if p.contains(PrimitiveFlags::SIGNED) {
                                            "signed"
                                        } else {
                                            "unsigned"
                                        });
                    show_size = true;
                }
                Type::Pointer(p) => {
                    if let Some((text, palette)) = &mut out {print_type_name(t, *text, *palette, 0);}
                    builder.add_usize_field("type", p.type_ as usize, state.builtin_types.meta_type);
                }
                Type::Array(a) => {
                    if let Some((text, palette)) = &mut out {print_type_name(t, *text, *palette, 0);}
                    builder.add_usize_field("type", a.type_ as usize, state.builtin_types.meta_type);
                }
                Type::Slice(s) => {
                    if let Some((text, palette)) = &mut out {print_type_name(t, *text, *palette, 0);}
                    builder.add_usize_field("type", s.type_ as usize, state.builtin_types.meta_type);
                }
                Type::Struct(s) => {
                    styled_write_maybe!(out, keyword, "{}", if s.flags.contains(StructFlags::UNION) {"union"} else {"struct"});
                    show_size = true;
                    let mut fields = StructBuilder::default();
                    for f in s.fields() {
                        fields.add_usize_field(f.name, f as *const StructField as usize, state.builtin_types.meta_field);
                    }
                    let fields = fields.finish("", val.flags, &mut state.types);
                    builder.add_field("fields", fields);
                }
                Type::Enum(e) => {
                    styled_write_maybe!(out, keyword, "enum");
                    show_size = true;
                    let mut items = StructBuilder::default();
                    let mut size = unsafe {(*e.type_).calculate_size()};
                    if size == 0 || size > 8 {
                        size = 8;
                    }
                    for en in e.enumerands {
                        items.add_blob_field(en.name, &en.value.to_le_bytes()[..size], e.type_);
                    }
                    let items = items.finish("", val.flags, &mut state.types);
                    builder.add_field("items", items);
                    builder.add_usize_field("type", e.type_ as usize, state.builtin_types.meta_type);
                }
                Type::MetaType | Type::MetaField => {
                    styled_write_maybe!(out, default, "{}", match &t.t { Type::MetaType => "type", Type::MetaField => "field", _ => panic!("huh") });
                    return builder.finish("", val.flags, &mut state.types);
                }
            }
            let size = t.calculate_size();
            if show_size {
                styled_write_maybe!(out, default, " ({} bytes):", size);
            }
            builder.add_usize_field("size", size, state.builtin_types.u64_);
            if !t.name.is_empty() {
                styled_write_maybe!(out, default, " {}", t.name);
                builder.add_str_field("name", t.name, &mut state.types, &state.builtin_types);
            }
            // TODO: Decl file+line.
            if t.die.0 != 0 && t.die.0 < FAKE_DWARF_OFFSET_START {
                builder.add_usize_field("dwarf_offset", t.die.0 as usize, state.builtin_types.u64_);
            }

            builder.finish("", val.flags | ValueFlags::NO_UNWRAPPING_INTERNAL, &mut state.types)
        }
        Type::MetaField => {
            let f = val.val.blob_ref().unwrap().get_usize().unwrap() as *const StructField;
            let f = unsafe {&*f};
            builder.add_str_field("name", f.name, &mut state.types, &state.builtin_types);
            builder.add_usize_field("type", f.type_ as usize, state.builtin_types.meta_type);
            if f.bit_offset % 8 == 0 {
                styled_write_maybe!(out, default, "offset {}: ", f.bit_offset / 8);
                builder.add_usize_field("offset", f.bit_offset / 8, state.builtin_types.u64_);
            } else {
                styled_write_maybe!(out, default, "bit_offset {}: ", f.bit_offset);
                builder.add_usize_field("bit_offset", f.bit_offset, state.builtin_types.u64_);
            }
            if let Some((text, palette)) = &mut out {print_type_name(f.type_, *text, *palette, 0);}
            if f.flags.contains(FieldFlags::SIZE_KNOWN) && f.bit_size != unsafe {(*f.type_).calculate_size() * 8} {
                builder.add_usize_field("bit_size", f.bit_size, state.builtin_types.u64_);
            }
            builder.add_str_field("flags", &f.readable_flags(), &mut state.types, &state.builtin_types);
            if f.flags.contains(FieldFlags::VARIANT) {
                builder.add_usize_field("discriminant_value", f.discr_value, state.builtin_types.u64_);
            }
            // TODO: Decl file+line.
            builder.finish("", val.flags, &mut state.types)
        }
        _ => panic!("expected meta type/field, got {}", meta_t.t.kind_name()),
    }
}

fn is_field_uninformative(mut f: &StructField) -> bool {
    for step in 0..100 {
        let size = f.calculate_bit_size();
        if size == 0 {
            return true;
        }
        if size != 8 {
            return false;
        }

        // If 1 byte, this may be the C++ thing where empty struct has size 1 instead of 0.
        // This is common for comparators and allocators.
        // Here's a partial check for that. It doesn't cover the case where the struct
        // has multiple uninformative fields (and may have size > 1 byte), but that's rare.
        let t = unsafe {&*f.type_};
        f = match &t.t {
            Type::Struct(s) if s.fields().len() == 0 => return true,
            Type::Struct(s) if s.fields().len() == 1 => &s.fields()[0],
            _ => return false,
        };
    }
    false
}

// Static transformation on a struct type that makes it more digestible:
//  * Remove fields that carry no information, specifically empty structs possibly wrapped in some number of layers of single-field structs.
//    Common in C++ for allocators (often as base class, which we consider a field).
//  * If there's only one field, and it's a struct, inline that struct's fields as if they're this struct's fields.
//    Very common for various wrappers in C++ and Rust.
//  * Inline fields from base structs, as if they're this struct's fields.
//  * Repeat to convergence.
// This undoes a lot of cruft in standard libraries and makes watch values much more readable.
// This is a shallow operation: it changes the set of fields, but doesn't change the types inside those fields.
// When formatting a value, we apply prettification for each tree node, so end up with a deeply prettified tree.
// When unwrapping single-field structs, the names of removed structs are added to additional_names; they're useful for container detection,
// e.g. if the user has struct Foo {s: String}, and we inline the String, the container detection needs to know that there was a struct named "String" in there,
// to distinguish between string and array of u8.
// Returns the new set of fields if any changes were made, None otherwise.
fn unravel_struct(fields: &mut Cow<[StructField]>, mut additional_names: Option<&mut Vec<&'static str>>) {
    let (mut has_inheritance, mut empty_fields) = (false, 0usize);
    for f in fields.iter() {
        has_inheritance |= f.flags.contains(FieldFlags::INHERITANCE);
        if is_field_uninformative(f) {
            empty_fields += 1;
        }
    }
    if !has_inheritance && empty_fields == 0 && fields.len() != 1 {
        // Fast path.
        return;
    }

    let mut temp_fields: Vec<StructField> = Vec::new();
    for pass in 0..100 {
        temp_fields.clear();

        // Remove uninformative fields.
        let mut changed = false;
        for f in fields.iter() {
            if is_field_uninformative(f) {
                changed = true;
            } else {
                temp_fields.push(f.clone());
            }
        }
        let mut res = match fields {
            Cow::Borrowed(_) => Vec::new(),
            Cow::Owned(v) => { // reuse memory
                v.clear();
                mem::take(v)
            }
        };

        for f in &temp_fields {
            // Inline field's fields if it's base class or the only field.
            // This works correctly with Rust enums: the discriminant and all variants will be inlined and will work correctly
            // (as long as we inline only one field this way; there should be no base classes in Rust, so we're fine).
            let unwrap = temp_fields.len() == 1;
            let mut inlined = false;
            if f.flags.contains(FieldFlags::INHERITANCE) || unwrap {
                let field_type = unsafe {&*f.type_};
                if let Type::Struct(field_struct) = &field_type.t {
                    for mut ff in field_struct.fields().iter().cloned() {
                        ff.bit_offset += f.bit_offset;
                        res.push(ff);
                    }
                    inlined = true;
                    changed = true;
                    if unwrap && !field_type.name.is_empty() {
                        if let Some(v) = &mut additional_names {
                            v.push(field_type.name);
                        }
                    }
                }
            }
            if !inlined {
                res.push(f.clone());
            }
        }

        *fields = Cow::Owned(res);

        if !changed {
            break;
        }
    }
}

fn is_field_vtable_ptr(f: &StructField) -> bool {
    f.flags.contains(FieldFlags::ARTIFICIAL) && f.name.starts_with("_vptr$")
}

fn find_vtable_ptr_field_offset(t: &TypeInfo, s: &StructType) -> Option<usize> {
    // Quickly check if any inheritance is involved.
    if !s.fields().iter().any(|f| f.flags.contains(FieldFlags::INHERITANCE) || is_field_vtable_ptr(f)) {
        return None;
    }

    // DFS through all base classes of this class. (E.g. imagine multiple inheritance where some of the base classes are nonvirtual.)
    let mut bases: Vec<(usize, *const TypeInfo)> = vec![(0, t)];
    for attempt in 0..100 {
        let Some((off, t)) = bases.pop() else { return None };
        let t = unsafe {&*t};
        let Type::Struct(s) = &t.t else { continue };
        for f in s.fields() {
            if f.bit_offset % 8 != 0 {
                continue;
            }
            if is_field_vtable_ptr(f) {
                return Some(off + f.bit_offset/8);
            }
            if f.flags.contains(FieldFlags::INHERITANCE) {
                bases.push((off + f.bit_offset/8, f.type_));
            }
        }
    }
    None
}

fn downcast_to_concrete_type(val: &mut Cow<Value>, vtable_ptr_field_offset: usize, context: &EvalContext) -> Result<()> {
    let Some(addr) = val.val.addr() else { return err!(Runtime, "struct address not known") };
    let vptr = context.memory.read_u64(addr + vtable_ptr_field_offset)? as usize;
    // Itanium ABI vtable layout is:
    //  * (various optional stuff)
    //  * 8 bytes: offset from vptr to start of subclass instance - that's how we downcast
    //  * 8 bytes: whatever
    //  * <vptr field points here>
    //  * function pointers
    let binary = context.process_info.addr_to_binary(vptr)?;
    let symbols = binary.symbols.clone()?;
    let static_vptr = binary.addr_map.dynamic_to_static(vptr);
    let vtable = symbols.find_vtable(static_vptr)?;
    let Some(type_) = vtable.type_.clone() else { return err!(Dwarf, "unknown type: {}", vtable.name) };
    let offset = context.memory.read_u64(vptr.saturating_sub(16))? as usize;
    let addr = (addr + vtable_ptr_field_offset).wrapping_add(offset);
    let new_val = Value {val: AddrOrValueBlob::Addr(addr), type_, flags: val.flags | ValueFlags::SHOW_TYPE_NAME};
    *val = Cow::Owned(new_val);
    Ok(())
}

// Removes fields representing inactive variants. If nothing looks broken, removes discriminant too.
fn resolve_discriminated_union(val: &AddrOrValueBlob, fields: &mut Cow<[StructField]>, warning: &mut Option<Error>, context: &EvalContext) -> Result<bool> {
    let mut discriminant: Option<usize> = None;
    for field in fields.iter() {
        if field.flags.contains(FieldFlags::DISCRIMINANT) {
            if discriminant.is_some() {
                *warning = Some(error!(Dwarf, "multiple discriminants"));
                return Ok(false);
            }

            let t = unsafe {&*field.type_};

            let (size, signed) = match &t.t {
                Type::Primitive(p) => (t.size, p.contains(PrimitiveFlags::SIGNED)),
                _ if t.flags.contains(TypeFlags::SIZE_KNOWN) => (t.size, false),
                _ => {
                    *warning = Some(error!(Dwarf, "unexpected discriminant type"));
                    return Ok(false);
                }
            };
            if size > 8 {
                *warning = Some(error!(Dwarf, "discriminant too big: {} bytes", size));
                return Ok(false);
            }

            let val = get_struct_field(&val, field, context.memory)?;
            let mut x = val.into_value(size, context.memory)?.get_usize()?;

            if signed && size < 8 && size > 0 && x & 1 << (size*8-1) as u32 != 0 {
                // Sign-extend (because that's what we do to discr_values when parsing DWARF).
                x |= !((1usize << size*8)-1);
            }

            discriminant = Some(x);
        }
    }

    let discriminant = match discriminant {
        None => return Ok(false),
        Some(x) => x };

    let (mut found, mut has_default, mut has_nonvariant) = (false, false, false);
    fields.to_mut().retain(|f| {
        if f.flags.contains(FieldFlags::DEFAULT_VARIANT) {
            has_default = true;
        } else if f.flags.contains(FieldFlags::VARIANT) {
            if f.discr_value == discriminant {
                found = true;
            } else {
                return false;
            }
        } else if !f.flags.contains(FieldFlags::DISCRIMINANT) {
            has_nonvariant = true;
        }
        true
    });
    if found || has_default {
        let mut delete_if = FieldFlags::empty();
        if !has_nonvariant {
            delete_if.insert(FieldFlags::DISCRIMINANT);
        }
        if found {
            delete_if.insert(FieldFlags::DEFAULT_VARIANT);
        }
        fields.to_mut().retain(|f| !f.flags.intersects(delete_if));
    }
    Ok(true)
}

fn container_unravel_pointer_or_size_field(field: &StructField, offset: &mut usize) -> Result<&'static TypeInfo> {
    // Non-64-bit or bit-packed not supported, I haven't seen them in std containers in C++ or Rust
    // (except as part of more elaborate packing that needs separate logic anyway).
    if field.calculate_bit_size() != 64 || field.bit_offset % 8 != 0 {
        return err!(NotContainer, "");
    }
    *offset = field.bit_offset / 8;
    let t = unsafe {&*field.type_};
    if let Type::Struct(s) = &t.t {
        // Container begin/end/size/capacity fields are often wrapped in more structs. Ugh. Peel them too.
        let mut fields = Cow::Borrowed(s.fields());
        unravel_struct(&mut fields, None);
        if fields.len() == 1 && fields[0].bit_size == field.bit_size && fields[0].bit_offset % 8 == 0 {
            *offset += fields[0].bit_offset / 8;
            return Ok(unsafe {&*fields[0].type_});
        }
        err!(NotContainer, "")
    } else {
        Ok(t)
    }
}
fn container_pointer_field(field: &StructField, inner_type: &mut *const TypeInfo, offset: &mut usize) -> Result<()> {
    let t = container_unravel_pointer_or_size_field(field, offset)?;
    if let Some(p) = t.t.as_pointer() {
        if *inner_type != ptr::null() && *inner_type != p.type_ {
            return err!(NotContainer, "");
        }
        *inner_type = p.type_;
        return Ok(());
    }
    err!(NotContainer, "")
}
fn container_size_field(field: &StructField, offset: &mut usize) -> Result<()> {
    let t = container_unravel_pointer_or_size_field(field, offset)?;
    if let Some(p) = t.t.as_primitive() {
        // Allow u64 and i64.
        if p.intersects(PrimitiveFlags::FLOAT | PrimitiveFlags::CHAR | PrimitiveFlags::UNSPECIFIED) {
            return err!(NotContainer, "");
        }
        return Ok(());
    }
    err!(NotContainer, "")
}

fn read_container_struct<'a>(value: &'a Value, scratch: &'a mut [u8], memory: &MemReader) -> Result<&'a [u8]> {
    let t = unsafe {&*value.type_};
    if t.size > scratch.len() {
        return err!(NotContainer, "");
    }
    match &value.val {
        AddrOrValueBlob::Blob(b) => Ok(b.as_slice()),
        &AddrOrValueBlob::Addr(addr) => {
            memory.read(addr, &mut scratch[..t.size])?;
            Ok(&scratch[..t.size])
        }
    }
}

fn get_container_usize_field(offset: usize, slice: &[u8]) -> Result<usize> {
    if offset + 8 > slice.len() {
        return err!(NotContainer, "");
    }
    let mut a = [0u8; 8];
    a.copy_from_slice(&slice[offset..offset+8]);
    Ok(usize::from_le_bytes(a))
}

fn one_container_name_looks_like_string(s: &str) -> bool {
    let end = s.find('<').unwrap_or(s.len());
    let s = &s[..end];
    // Hope no one will have a struct called "NotString".
    s.find("str").is_some() || s.find("Str").is_some()
}

fn container_name_looks_like_string(t: &TypeInfo, additional_names: &[&str]) -> bool {
    if one_container_name_looks_like_string(t.name) {
        return true;
    }
    additional_names.iter().any(|s| one_container_name_looks_like_string(*s))
}

fn trim_field_name(mut name: &str) -> &str {
    if name.starts_with("_M_") {
        name = &name[3..];
    } else if name.starts_with("c_") {
        name = &name[3..];
    }
    while name.starts_with("_") {
        name = &name[1..];
    }
    while name.ends_with("_") {
        name = &name[..name.len()-1];
    }
    name
}

// Rust RawVec: {ptr: *T, cap: usize}
fn recognize_raw_vec(type_: *const TypeInfo, inner_type: &mut *const TypeInfo, out_ptr_offset: &mut usize, out_cap_offset: &mut usize) -> Result<()> {
    let type_ = unsafe {&*type_};
    if type_.size != 16 {
        return err!(NotContainer, "");
    }
    if let Type::Struct(s) = &type_.t {
        let mut fields = Cow::Borrowed(s.fields());
        unravel_struct(&mut fields, None);
        if fields.len() != 2 {
            return err!(NotContainer, "");
        }
        let mut found = 0usize;
        for f in fields.iter() {
            if f.bit_offset % 8 != 0 {
                return err!(NotContainer, "");
            }
            match trim_field_name(f.name) {
                "ptr" => {
                    found |= 1;
                    container_pointer_field(f, inner_type, out_ptr_offset)?;
                }
                "cap" => {
                    found |= 2;
                    container_size_field(f, out_cap_offset)?;
                }
                _ => return err!(NotContainer, ""),
            }
        }
        if found == 3 {
            return Ok(());
        }
    }
    err!(NotContainer, "")
}

// Fields we're looking for to recognize standard library containers.
bitflags! { pub struct ContainerFields: u64 {
    const BEGIN = 0x1;
    const END = 0x2;
    const END_OF_STORAGE = 0x4;
    const LEN = 0x8;
    const CAPACITY = 0x10;
    const DATA = 0x20;
    const PTR = 0x40;
    const STR = 0x80;
    const REFCOUNT = 0x100;
    const BUF = 0x200;
    const HEAD = 0x400;
}}
const CONTAINER_FIELD_NAMES: [(&'static str, ContainerFields); 20] = [
    ("begin", ContainerFields::BEGIN),
    ("start", ContainerFields::BEGIN),
    ("data_ptr", ContainerFields::BEGIN),
    ("end", ContainerFields::END),
    ("finish", ContainerFields::END),
    ("end_of_storage", ContainerFields::END_OF_STORAGE),
    ("end_cap", ContainerFields::END_OF_STORAGE),
    ("len", ContainerFields::LEN),
    ("length", ContainerFields::LEN),
    ("size", ContainerFields::LEN),
    ("extent", ContainerFields::LEN),
    ("cap", ContainerFields::CAPACITY),
    ("capacity", ContainerFields::CAPACITY),
    ("data", ContainerFields::DATA),
    ("ptr", ContainerFields::PTR),
    ("str", ContainerFields::STR),
    ("refcount", ContainerFields::REFCOUNT),
    ("ctrl", ContainerFields::REFCOUNT),
    ("buf", ContainerFields::BUF),
    ("head", ContainerFields::HEAD),
];

// Guess the common C++ and Rust containers. Turn them into slices/arrays/pointers as needed.
// The input Value already went through do_general_transformations(), so wrappers and inheritance are mostly gone.
// Returns true if the whole Value was replaced and doesn't need any further transformations.
// May modify `fields` and return false, e.g. remove refcount field from shared_ptr expecting the caller to unwrap the remaining single-field struct into a plain pointer.
// Returning NotContainer error is equivalent to Ok(false) - just for convenience.
pub fn recognize_containers(val: &mut Cow<Value>, fields: &mut Cow<[StructField]>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<bool> {
    let t = unsafe {&*val.type_};
    let language = match t.language {
        LanguageFamily::Internal => return Ok(false),
        LanguageFamily::Rust => LanguageFamily::Rust,
        LanguageFamily::Cpp | LanguageFamily::Other | LanguageFamily::Unknown => LanguageFamily::Cpp,
    };

    // We recognize containers in a duck-typed way, by approximate field names, after do_general_transformations().
    // This way we're not too sensitive to small changes in implementation, and sometimes recognize custom containers along the way.
    // This may have some false positives, but that's not catastrophic, the user can always use .#r to see raw struct.

    const max_fields: usize = 5;
    if fields.len() > max_fields {
        return Ok(false);
    }

    // Map field names to a bitset. If there are any unrecognized names, assume it's not a container.
    // We should make this part reasonably fast.
    let mut seen_fields = ContainerFields::empty();
    let mut field_kinds = [ContainerFields::empty(); max_fields];
    for (i, f) in fields.iter().enumerate() {
        let name = trim_field_name(f.name);
        let mut found = false;
        for &(n, f) in &CONTAINER_FIELD_NAMES {
            if name == n {
                found = true;
                field_kinds[i] = f;
                seen_fields.insert(f);
                break;
            }
        }
        if !found {
            return Ok(false);
        }
    }

    // Slice or vector.
    // Something like: {start: *T, len: usize} or {begin: *T, end: *T, end_of_storage: *T}, or {buf: {ptr: *T, cap: usize}, len: usize}.
    let begin_like: ContainerFields = ContainerFields::BEGIN | ContainerFields::DATA | ContainerFields::PTR | ContainerFields::STR | ContainerFields::BUF;
    let len_like: ContainerFields = ContainerFields::END | ContainerFields::LEN;
    let capacity_like: ContainerFields = ContainerFields::END_OF_STORAGE | ContainerFields::CAPACITY;
    if seen_fields.intersects(begin_like) && seen_fields.intersects(len_like) && (begin_like | len_like | capacity_like).contains(seen_fields) {
        // Find field offsets.
        let (mut begin_field, mut end_field, mut len_field, mut end_of_storage_field, mut capacity_field) = (usize::MAX, usize::MAX, usize::MAX, usize::MAX, usize::MAX);
        let mut inner_type: *const TypeInfo = ptr::null();
        for (i, f) in fields.iter().enumerate() {
            if field_kinds[i] == ContainerFields::BUF {
                if f.bit_offset % 8 != 0 {
                    return Ok(false);
                }
                recognize_raw_vec(f.type_, &mut inner_type, &mut begin_field, &mut capacity_field)?;
                begin_field += f.bit_offset / 8;
                capacity_field += f.bit_offset / 8;
                continue;
            }
            if begin_like.contains(field_kinds[i]) {
                container_pointer_field(f, &mut inner_type, &mut begin_field)?;
                continue;
            }
            match field_kinds[i] {
                ContainerFields::END => container_pointer_field(f, &mut inner_type, &mut end_field)?,
                ContainerFields::END_OF_STORAGE => container_pointer_field(f, &mut inner_type, &mut end_of_storage_field)?,
                ContainerFields::LEN => container_size_field(f, &mut len_field)?,
                ContainerFields::CAPACITY => container_size_field(f, &mut capacity_field)?,
                _ => panic!("huh"),
            }
        }
        let inner_size = (unsafe {&*inner_type}).calculate_size();

        // Read the struct into temp buffer (instead of reading fields one by one using multiple syscalls).
        let mut scratch = [0u8; 40];
        let slice = read_container_struct(val, &mut scratch, &context.memory)?;

        // Parse the field values.
        let start = get_container_usize_field(begin_field, slice)?;
        let len = if len_field != usize::MAX {
            get_container_usize_field(len_field, slice)?
        } else {
            if inner_size == 0 {
                *warning = Some(error!(NotContainer, "element size is 0"));
                return Ok(false);
            }
            let end = get_container_usize_field(end_field, slice)?;
            if end < start {
                *warning = Some(error!(ProcessState, "end < start"));
                return Ok(false);
            }
            let bytes = end - start;
            if bytes % inner_size != 0 {
                *warning = Some(error!(ProcessState, "slice size not divisible by element size"));
                return Ok(false);
            }
            bytes / inner_size
        };

        // Guess whether this is a string.
        let mut flags = SliceFlags::empty();
        if inner_size == 1 && container_name_looks_like_string(t, additional_names) {
            flags.insert(SliceFlags::UTF_STRING);
        }

        // Return a slice.
        let type_ = state.types.add_slice(inner_type, flags);
        let v = AddrOrValueBlob::Blob(ValueBlob::from_two_usizes([start, len]));
        let new_val = Value {val: v, type_, flags: val.flags};
        *val = Cow::Owned(new_val);
        return Ok(true);
    }

    Ok(false)
}
