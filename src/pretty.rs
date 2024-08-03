use crate::{*, types::*, error::*, expr::*, util::*, settings::*, common_ui::*, procfs::*};
use std::{mem, mem::MaybeUninit, ptr, fmt::Write, borrow::Cow, io::Write as ioWrite, ops::Range};
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
        Ok(()) => return Ok(()),
        Err(e) if !e.is_not_container() && !e.is_no_field() => *warning = Some(e),
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
        write!(w, "{}", type_.name).unwrap();
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


// Container stuff.
// Throughout these functions, error NoField means "this container wasn't recognized, but we should try others", while NotContainer means "stop all container recognition".

struct ContainerSubstruct<'a> {
    fields: Cow<'a, [StructField]>,
    bit_offset: usize,
    used_fields_mask: usize,
}
impl ContainerSubstruct<'_> {
    fn check_all_fields_used(&self) -> Result<()> {
        if self.used_fields_mask != (!0usize >> (64-self.fields.len().min(64) as u32)) {
            err!(NotContainer, "")
        } else {
            Ok(())
        }
    }
    fn clear_used(&mut self) {
        self.used_fields_mask = 0;
    }
}

fn find_field(names: &[&str], substruct: &mut ContainerSubstruct) -> Result<StructField> {
    for (i, f) in substruct.fields.iter().enumerate() {
        let name = trim_field_name(f.name);
        if names.iter().position(|n| *n == name).is_some() {
            if i < 64 {
                substruct.used_fields_mask |= 1usize << i as u32;
            }
            return Ok(f.clone());
        }
    }
    err!(NoField, "")
}

// Container begin/end/size/capacity fields are often wrapped in more structs. Peel them.
fn unravel_container_field(field: &StructField, substruct: &mut ContainerSubstruct, primitive: bool) -> Result<(&'static TypeInfo, Range<usize>)> {
    let mut t = unsafe {&*field.type_};
    let mut bit_offset = substruct.bit_offset + field.bit_offset;
    let mut bit_size = if field.flags.contains(FieldFlags::SIZE_KNOWN) {field.bit_size} else {t.calculate_size()*8};
    if let Type::Struct(s) = &t.t {
        let mut fields = Cow::Borrowed(s.fields());
        unravel_struct(&mut fields, None);
        if fields.len() != 1 {
            return err!(NoField, "");
        }
        let field = &fields[0];
        t = unsafe {&*field.type_};
        bit_offset += field.bit_offset;
        bit_size = if field.flags.contains(FieldFlags::SIZE_KNOWN) {field.bit_size} else {t.calculate_size()*8};
    }
    if primitive {
        if bit_size == 0 {
            return err!(NotContainer, "");
        }
        if bit_size > 64 {
            return err!(NoField, "");
        }
    }
    Ok((t, bit_offset..bit_offset+bit_size))
}

fn find_struct_field<'a>(names: &[&str], substruct: &mut ContainerSubstruct<'a>) -> Result<ContainerSubstruct<'a>> {
    let f = find_field(names, substruct)?;
    let t = unsafe {&*f.type_};
    match &t.t {
        Type::Struct(s) => {
            if f.flags.contains(FieldFlags::SIZE_KNOWN) && f.bit_size != t.size * 8 {
                return err!(NotContainer, "");
            }
            let mut fields = Cow::Borrowed(s.fields());
            unravel_struct(&mut fields, None);
            Ok(ContainerSubstruct {fields, bit_offset: substruct.bit_offset + f.bit_offset, used_fields_mask: 0})
        }
        _ => err!(NoField, ""),
    }
}
fn find_int_field(names: &[&str], substruct: &mut ContainerSubstruct) -> Result<Range<usize>> {
    let f = find_field(names, substruct)?;
    let (t, r) = unravel_container_field(&f, substruct, true)?;
    match &t.t {
        Type::Primitive(p) => {
            if p.intersects(PrimitiveFlags::FLOAT | PrimitiveFlags::UNSPECIFIED) {
                return err!(NotContainer, "");
            }
            if p.contains(PrimitiveFlags::SIGNED) && r.len() != 64 { // we'd need to sign-extend in this case
                return err!(NotContainer, "");
            }
            Ok(r)
        }
        _ => err!(NoField, ""),
    }
}
fn find_pointer_field(names: &[&str], substruct: &mut ContainerSubstruct, inner_type: &mut *const TypeInfo) -> Result<Range<usize>> {
    let f = find_field(names, substruct)?;
    let (t, r) = unravel_container_field(&f, substruct, true)?;
    match &t.t {
        Type::Pointer(p) => {
            if *inner_type != ptr::null() && *inner_type != p.type_ {
                return err!(NotContainer, "");
            }
            *inner_type = p.type_;
            Ok(r)
        }
        _ => err!(NoField, ""),
    }
}
fn find_array_field(names: &[&str], substruct: &mut ContainerSubstruct, inner_type: &mut *const TypeInfo) -> Result<(Range<usize>, usize)> {
    let f = find_field(names, substruct)?;
    let (t, r) = unravel_container_field(&f, substruct, false)?;
    match &t.t {
        Type::Array(a) => {
            if r.start % 8 != 0 {
                return err!(NotContainer, "");
            }
            if !a.flags.contains(ArrayFlags::LEN_KNOWN) {
                return err!(NotContainer, "");
            }
            if *inner_type != ptr::null() && *inner_type != a.type_ {
                return err!(NotContainer, "");
            }
            *inner_type = a.type_;
            let t = unsafe {&*a.type_};
            let inner_size = t.calculate_size();
            if a.stride != 0 && a.stride != inner_size {
                return err!(NotContainer, "");
            }
            if r.len() != inner_size*a.len*8 {
                return err!(NotContainer, "");
            }
            Ok((r, a.len))
        }
        _ => err!(NoField, ""),
    }
}

fn get_container_usize_field(bit_offset: Range<usize>, slice: &[u8]) -> Result<usize> {
    assert!(bit_offset.len() <= 64);
    if bit_offset.end > slice.len() * 64 {
        return err!(NotContainer, "");
    }
    let mut a = [0u8; 8];
    let byte_offset = bit_offset.start/8..bit_offset.end/8;
    a[..byte_offset.len()].copy_from_slice(&slice[byte_offset.clone()]);
    let mut r = usize::from_le_bytes(a);

    if bit_offset.start%8 != 0 {
        r >>= (bit_offset.start%8) as u32;
    }
    if bit_offset.end%8 != 0 {
        let e = slice[bit_offset.end/8];
        let e = e & ((1u8 << (bit_offset.end%8) as u32) - 1);
        r |= (e as usize) << (bit_offset.len() - bit_offset.end%8) as u32;
    }

    Ok(r)
}

fn read_container_struct<'a>(value: &'a Value, scratch: &'a mut [MaybeUninit<u8>], memory: &MemReader) -> Result<&'a [u8]> {
    let t = unsafe {&*value.type_};
    if t.size > scratch.len() {
        return err!(NotContainer, "");
    }
    match &value.val {
        AddrOrValueBlob::Blob(b) => Ok(b.as_slice()),
        &AddrOrValueBlob::Addr(addr) => {
            Ok(memory.read_uninit(addr, &mut scratch[..t.size])?)
        }
    }
}

fn optional_field<T>(r: Result<T>) -> Result<Option<T>> {
    match r {
        Ok(x) => Ok(Some(x)),
        Err(e) if e.is_no_field() => Ok(None),
        Err(e) => Err(e),
    }
}

fn one_container_name_looks_like_string(s: &str) -> bool {
    let end = s.find('<').unwrap_or(s.len());
    let s = &s[..end];
    // Hope no one will have a struct called "NotString".
    s.find("str").is_some() || s.find("Str").is_some()
}

fn container_name_looks_like_string(t: *const TypeInfo, additional_names: &[&str]) -> bool {
    if one_container_name_looks_like_string(unsafe {(*t).name}) {
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

// Guess the common C++ and Rust containers. Turn them into slices/arrays/pointers as needed.
// The input Value already went through do_general_transformations(), so wrappers and inheritance are mostly gone.
// Returns Ok if the whole Value was replaced and doesn't need any further transformations.
// Returns NotContainer error if the value wasn't replaced (but `fields` may have been mutated, e.g. removing refcount field from shared_ptr).
// Returns other errors if we should fail the whole prettification (e.g. failed to read the struct from memory).
fn recognize_containers(val: &mut Cow<Value>, fields: &mut Cow<[StructField]>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    let t = unsafe {&*val.type_};
    let language = match t.language {
        LanguageFamily::Internal => return err!(NotContainer, ""),
        LanguageFamily::Rust => LanguageFamily::Rust,
        LanguageFamily::Cpp | LanguageFamily::Other | LanguageFamily::Unknown => LanguageFamily::Cpp,
    };

    // We recognize containers in a duck-typed way, by approximate field names, after unraveling the struct.
    // This way we're not too sensitive to small changes in implementation, and sometimes recognize custom containers along the way.
    // This may have some false positives, but that's not catastrophic, the user can always use .#r to see raw struct.

    match recognize_shared_ptr(fields) {
        Ok(()) => return Ok(()),
        Err(e) if e.is_no_field() => (),
        Err(e) => return Err(e),
    }

    let mut substruct = ContainerSubstruct {fields: fields.clone(), bit_offset: 0, used_fields_mask: 0};

    match recognize_slice_or_vec(&mut substruct, val, additional_names, warning, state, context) {
        Ok(()) => return Ok(()),
        Err(e) if e.is_no_field() => substruct.clear_used(),
        Err(e) => return Err(e),
    }
    match recognize_rust_vec(&mut substruct, val, additional_names, warning, state, context) {
        Ok(()) => return Ok(()),
        Err(e) if e.is_no_field() => substruct.clear_used(),
        Err(e) => return Err(e),
    }
    match recognize_libcpp_string(&mut substruct, val, additional_names, warning, state, context) {
        Ok(()) => return Ok(()),
        Err(e) if e.is_no_field() => substruct.clear_used(),
        Err(e) => return Err(e),
    }
    match recognize_absl_inlined_vector(&mut substruct, val, additional_names, warning, state, context) {
        Ok(()) => return Ok(()),
        Err(e) if e.is_no_field() => substruct.clear_used(),
        Err(e) => return Err(e),
    }

    err!(NotContainer, "")
}

fn make_slice(val: &mut Cow<Value>, ptr: usize, len: usize, inner_type: *const TypeInfo, is_string: bool, state: &mut EvalState) {
    let flags = if is_string {SliceFlags::UTF_STRING} else {SliceFlags::empty()};
    let slice_type = state.types.add_slice(inner_type, flags);
    let v = AddrOrValueBlob::Blob(ValueBlob::from_two_usizes([ptr, len]));
    let new_val = Value {val: v, type_: slice_type, flags: val.flags};
    *val = Cow::Owned(new_val);
}

// Slice or vector.
// Something like: {start: *T, len: usize} or {begin: *T, end: *T, end_of_storage: *T}, or {buf: {ptr: *T, cap: usize}, len: usize}.
fn recognize_slice_or_vec(substruct: &mut ContainerSubstruct, val: &mut Cow<Value>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    // Find field offsets.
    // (We first analyze the fields, then look at data. Not necessarily the simplest way to do this right now, but a sort of rehearsal of compiling to bytecode.)
    let mut inner_type: *const TypeInfo = ptr::null();
    let begin_field = find_pointer_field(&["begin", "start", "data_ptr", "ptr", "str", "data", "dataplus"], substruct, &mut inner_type)?;
    let len_field = optional_field(find_int_field(&["len", "length", "size", "extent", "string_length"], substruct))?;
    let end_field = optional_field(find_pointer_field(&["end", "finish"], substruct, &mut inner_type))?;
    if len_field.is_some() == end_field.is_some() {
        return err!(NoField, "");
    }
    optional_field(find_field(&["end_of_storage", "end_cap", "capacity", "cap"], substruct))?;
    if let Some(mut anon) = optional_field(find_struct_field(&[""], substruct))? {
        // libstdc++ string: {string_length, dataplus, union {local_buf, allocated_capacity}}
        optional_field(find_int_field(&["local_buf"], &mut anon))?;
    }
    substruct.check_all_fields_used()?;

    let inner_size = (unsafe {&*inner_type}).calculate_size();

    // Read the struct into temp buffer (instead of reading fields one by one using multiple syscalls).
    let mut scratch: [MaybeUninit<u8>; 40] = unsafe {MaybeUninit::uninit().assume_init()};
    let slice = read_container_struct(val, &mut scratch, &context.memory)?;

    // Parse the field values.
    let start = get_container_usize_field(begin_field, slice)?;
    let len = if let Some(r) = len_field {
        get_container_usize_field(r, slice)?
    } else {
        if inner_size == 0 {
            *warning = Some(error!(NotContainer, "element size is 0"));
            return err!(NotContainer, "");
        }
        let end = get_container_usize_field(end_field.unwrap(), slice)?;
        if end < start {
            *warning = Some(error!(ProcessState, "end < start"));
            return err!(NotContainer, "");
        }
        let bytes = end - start;
        if bytes % inner_size != 0 {
            *warning = Some(error!(ProcessState, "slice size not divisible by element size"));
            return err!(NotContainer, "");
        }
        bytes / inner_size
    };

    // Guess whether this is a string.
    let is_string = inner_size == 1 && container_name_looks_like_string(val.type_, additional_names);

    // Return a slice.
    make_slice(val, start, len, inner_type, is_string, state);
    Ok(())
}

// {len, buf: {ptr, cap}}
// Or VecDeque: {head, len, buf: {ptr, cap}}
fn recognize_rust_vec(substruct: &mut ContainerSubstruct, val: &mut Cow<Value>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    let mut buf = find_struct_field(&["buf"], substruct)?;
    let mut inner_type: *const TypeInfo = ptr::null();
    let ptr_field = find_pointer_field(&["ptr"], &mut buf, &mut inner_type)?;
    let cap_field = find_int_field(&["cap"], &mut buf)?;
    buf.check_all_fields_used()?;
    let len_field = find_int_field(&["len"], substruct)?;
    let head_field = optional_field(find_int_field(&["head"], substruct))?;
    substruct.check_all_fields_used()?;

    let mut scratch: [MaybeUninit<u8>; 40] = unsafe {MaybeUninit::uninit().assume_init()};
    let slice = read_container_struct(val, &mut scratch, &context.memory)?;

    let len = get_container_usize_field(len_field, slice)?;
    let ptr = get_container_usize_field(ptr_field, slice)?;
    let cap = get_container_usize_field(cap_field, slice)?;

    let inner_size = (unsafe {&*inner_type}).calculate_size();
    if let Some(f) = head_field {
        // VecDeque.
        let head = get_container_usize_field(f, slice)?;
        if head > cap || len > cap {
            return err!(Runtime, "VecDeque out of bounds");
        }
        let mut builder = StructBuilder::default();
        builder.add_slice_field("part1", ptr + head*inner_size, len.min(cap - head), inner_type, SliceFlags::empty(), &mut state.types);
        builder.add_slice_field("part2", ptr, len.saturating_sub(cap - head), inner_type, SliceFlags::empty(), &mut state.types);
        let new_val = builder.finish("VecDeque", val.flags, &mut state.types);
        *val = Cow::Owned(new_val);
    } else {
        let is_string = inner_size == 1 && container_name_looks_like_string(val.type_, additional_names);
        make_slice(val, ptr, len, inner_type, is_string, state);
    }
    Ok(())

}

// shared_ptr: {ptr, refcount} - just remove refcount, so that field unwrapping turns this into plain pointer
fn recognize_shared_ptr(fields: &mut Cow<[StructField]>) -> Result<()> {
    if fields.len() != 2 {
        return err!(NoField, "");
    }
    let mut refcount_idx: Option<usize> = None;
    let mut found_ptr = false;
    for (i, f) in fields.iter().enumerate() {
        match trim_field_name(f.name) {
            "cntrl" | "refcount" => refcount_idx = Some(i),
            "ptr" => found_ptr = true,
            _ => return err!(NoField, ""),
        }
    }
    if let Some(i) = refcount_idx {
        if found_ptr {
            fields.to_mut().remove(i);
            return err!(NotContainer, ""); // didn't replace the value
        }
    }
    err!(NoField, "")
}

// libc++ string
// union {
//   s: {0: {is_long: 1 bit, size: 7 bits}, padding: [u8; 0], data: [u8; 23]},
//   l: {0: {is_long: 1 bit, cap: 63 bits}, size: usize, data: *u8},
//   r
// }
// (actual capacity is `cap * 2`, where 2 is `__endian_factor`)
// (ifdef _LIBCPP_ABI_ALTERNATE_STRING_LAYOUT then the fields are shuffled around a little)
fn recognize_libcpp_string(substruct: &mut ContainerSubstruct, val: &mut Cow<Value>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    let mut s = find_struct_field(&["s"], substruct)?;
    let mut l = find_struct_field(&["l"], substruct)?;
    let _ = optional_field(find_field(&["r"], substruct))?;
    substruct.check_all_fields_used()?;

    let mut inner_type: *const TypeInfo = ptr::null();
    let (s_is_long_field, short_len_field);
    // Some fields are either wrapped or not wrapped in anon struct.
    match find_struct_field(&[""], &mut s) {
        Ok(mut s_anon) => {
            s_is_long_field = find_int_field(&["is_long"], &mut s_anon)?;
            short_len_field = find_int_field(&["size"], &mut s_anon)?;
            s_anon.check_all_fields_used()?;
        }
        Err(e) if e.is_no_field() => {
            s_is_long_field = find_int_field(&["is_long"], &mut s)?;
            short_len_field = find_int_field(&["size"], &mut s)?;
        }
        Err(e) => return Err(e),
    }
    let _ = optional_field(find_field(&["padding"], &mut s))?;
    let (short_data_field, short_capacity)= find_array_field(&["data"], &mut s, &mut inner_type)?;
    s.check_all_fields_used()?;

    let l_is_long_field;
    match find_struct_field(&[""], &mut l) {
        Ok(mut l_anon) => {
            l_is_long_field = find_int_field(&["is_long"], &mut l_anon)?;
            let _ = optional_field(find_int_field(&["cap"], &mut l_anon))?;
            l_anon.check_all_fields_used()?;
        }
        Err(e) if e.is_no_field() => {
            l_is_long_field = find_int_field(&["is_long"], &mut l)?;
            let _ = optional_field(find_int_field(&["cap"], &mut l))?;
        }
        Err(e) => return Err(e),
    }
    let long_len_field = find_int_field(&["size"], &mut l)?;
    let long_ptr_field = find_pointer_field(&["data"], &mut l, &mut inner_type)?;
    l.check_all_fields_used()?;

    let mut scratch: [MaybeUninit<u8>; 256] = unsafe {MaybeUninit::uninit().assume_init()};
    let slice = read_container_struct(val, &mut scratch, &context.memory)?;

    let s_is_long = get_container_usize_field(s_is_long_field, slice)?;
    if s_is_long > 1 {
        return err!(Runtime, "bad is_long: {}", s_is_long);
    }
    let l_is_long = get_container_usize_field(l_is_long_field, slice)?;
    if s_is_long != l_is_long {
        return err!(Runtime, "s.is_long != l.is_long: {} {}", s_is_long, l_is_long);
    }

    let inner_size = unsafe {(*inner_type).calculate_size()};
    let is_string = inner_size == 1;

    if l_is_long != 0 {
        let (ptr, len) = (get_container_usize_field(long_ptr_field, slice)?, get_container_usize_field(long_len_field, slice)?);
        make_slice(val, ptr, len, inner_type, is_string, state);
        Ok(())
    } else {
        let len = get_container_usize_field(short_len_field, slice)?;
        if len > short_capacity {
            return err!(Runtime, "short len > capacity: {} {}", len, short_capacity);
        }
        return make_array_or_slice(val, short_data_field.start/8, len, inner_type, is_string, state);
    }
}

fn make_array_or_slice(val: &mut Cow<Value>, byte_offset: usize, len: usize, inner_type: *const TypeInfo, is_string: bool, state: &mut EvalState) -> Result<()> {
    let new_val = match &val.val {
        AddrOrValueBlob::Addr(addr) => {
            let slice_type = state.types.add_slice(inner_type, if is_string {SliceFlags::UTF_STRING} else {SliceFlags::empty()});
            let v = AddrOrValueBlob::Blob(ValueBlob::from_two_usizes([addr + byte_offset, len]));
            Value {val: v, type_: slice_type, flags: val.flags}
        }
        AddrOrValueBlob::Blob(blob) => {
            let array_type = state.types.add_array(inner_type, len, if is_string {ArrayFlags::UTF_STRING} else {ArrayFlags::empty()});
            let inner_size = unsafe {(*inner_type).calculate_size()};
            Value {val: AddrOrValueBlob::Blob(blob.byte_range(byte_offset..byte_offset + len*inner_size)?), type_: array_type, flags: val.flags}
        }
    };
    *val = Cow::Owned(new_val);
    Ok(())
}

// absl::InlinedVector: {metadata_: usize, data_: union {allocated: {allocated_data: *T, allocated_capacity: usize}, inlined: {inlined_data: [i8; cap]}}}
// metadata_ & 1 - is_allocated
// metadata_ >> 1 - len
fn recognize_absl_inlined_vector(substruct: &mut ContainerSubstruct, val: &mut Cow<Value>, additional_names: &[&str], warning: &mut Option<Error>, state: &mut EvalState, context: &EvalContext) -> Result<()> {
    let metadata_field = find_int_field(&["metadata"], substruct)?;
    let mut data = find_struct_field(&["data"], substruct)?;
    substruct.check_all_fields_used()?;
    let mut allocated = find_struct_field(&["allocated"], &mut data)?;
    let mut inlined_type: *const TypeInfo = ptr::null();
    let (inlined_field, inlined_capacity) = find_array_field(&["inlined"], &mut data, &mut inlined_type)?;
    data.check_all_fields_used()?;
    let mut inner_type: *const TypeInfo = ptr::null();
    let allocated_data_field = find_pointer_field(&["allocated_data"], &mut allocated, &mut inner_type)?;
    let _ = find_int_field(&["allocated_capacity"], &mut allocated)?;
    allocated.check_all_fields_used()?;


    let mut scratch: [MaybeUninit<u8>; 1024] = unsafe {MaybeUninit::uninit().assume_init()};
    let slice = read_container_struct(val, &mut scratch, &context.memory)?;

    let metadata = get_container_usize_field(metadata_field, slice)?;
    let (is_long, len) = (metadata & 1 != 0, metadata >> 1);
    if is_long {
        let ptr = get_container_usize_field(allocated_data_field, slice)?;
        make_slice(val, ptr, len, inner_type, false, state);
        Ok(())
    } else {
        let inner_size = unsafe {(*inner_type).calculate_size()};
        let inlined_size = unsafe {(*inlined_type).calculate_size()};
        if len * inner_size > inlined_capacity * inlined_size {
            return err!(Runtime, "short len > capacity: {}*{} > {}*{}", len, inner_size, inlined_capacity, inlined_size);
        }
        return make_array_or_slice(val, inlined_field.start/8, len, inner_type, false, state);
    }
}
