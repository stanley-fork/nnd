use crate::{*, types::*, error::*, expr::*, util::*, settings::*, common_ui::*};
use std::{mem, fmt::Write};

// Apply pretty-printers and other transformations to a value. Used both for printing and expression evaluaion.
// E.g. this function may turn an std::vector<int> into a *[int; 123], making it appear as an array both when printed whole
// and when used in expression like v[10] (will index the array) or v._M_begin (will fail even if std::vector has field _M_begin).
pub fn prettify_value(val: &Value, state: &mut EvalState, context: &EvalContext) -> Result<(Option<Value>, Option</*warning*/ Error>)> {
    // Inline fields from base class, remove fields of size 0, handle discriminated unions, unwrap single-field structs. Repeat to convergence.
    // Main practical cases to cover:
    //  * Unwrap nested boilerplate wrappers - structs with just one field. Common in Rust and C++.
    //    E.g. Atomic/atomic, Box/unique_ptr (and the crazy tuple nonsense C++ does inside unique_ptr, pair, etc),
    //    NonzeroUsize, Pin, Rust's strong typedefs.
    //    This transformation automatically turns e.g. unique_ptr into a raw pointer, no separate pretty-printer needed.
    //  * Omit empty base class. Common in C++. Often combined with boilerplate wrappers.
    //  * Show base class's fields as if they're this class's fields. That's just what people are used to.
    //     ** But if the base class has custom pretty-printer (e.g. it's a container), inlining its fields into this class may
    //        prevent it from being pretty-printed. We should detect that and don't inline the fields in this case.
    //        If this class has no other fields, unwrapping will take precedence and pretty-printing will work automatically.
    //  * For Rust enum, look at discriminant and keep only the field corresponding to the active variant.

    if val.flags.contains(ValueFlags::NO_UNWRAPPING_INTERNAL) {
        return Ok((None, None));
    }

    let t = unsafe {&*val.type_};
    let s = match &t.t {
        Type::Struct(s) => s,
        _ => return Ok((None, None)),
    };
    let (mut has_inheritance, mut has_vptr, mut has_discriminant, mut empty_fields) = (false, false, false, 0usize);
    for f in s.fields() {
        has_inheritance |= f.flags.contains(FieldFlags::INHERITANCE);
        has_discriminant |= f.flags.contains(FieldFlags::DISCRIMINANT);
        has_vptr |= is_field_vtable_ptr(f);
        if is_field_uninformative(f) {
            empty_fields += 1;
        }
    }
    if !has_inheritance && !has_vptr && !has_discriminant && empty_fields == 0 && s.fields().len() != 1 {
        // Fast path.
        return Ok((None, None));
    }

    let mut val = val.clone();
    let mut unwrap_limit = 100usize;
    let mut warning: Option<Error> = None;

    // Downcast to concrete type.
    if has_inheritance || has_vptr {
        if let Some(off) = find_vtable_ptr_field_offset(val.type_) {
            val.flags.insert(ValueFlags::SHOW_TYPE_NAME);
            unwrap_limit = 0;
            match downcast_to_concrete_type(&val, off, context) {
                Ok(v) => val = v,
                Err(e) => warning = Some(e),
            }
        }
    }

    loop {
        unwrap_limit = unwrap_limit.saturating_sub(1);

        let t = unsafe {&*val.type_};
        let s = match &t.t {
            Type::Struct(s) => s,
            _ => return Ok((Some(val), warning)),
        };

        let mut new_fields: Vec<StructField> = s.fields().iter().cloned().collect();

        // For Rust enums, currently we report the active variant as a single field, without unwrapping. Otherwise the variant name wouldn't be reported.
        // Maybe we should make format_value() recognize this and print such single-field struct as "Some(42)" instead of "{Some: 42}" (need to handle empty, tuple, and struct variants).
        let is_discriminated_union = resolve_discriminated_union(&val, &mut new_fields, context, &mut warning)?;

        if !is_discriminated_union {
            let mut temp_fields: Vec<StructField> = Vec::new();
            let mut should_unwrap = false;
            for fields_pass in 0..100 {
                new_fields.retain(|f| !is_field_uninformative(f));
                if new_fields.is_empty() {
                    break;
                }
                // Check for single-field struct before inlining base classes.
                // This is the only reason why we do the field inlining in passes instead of as a more efficient DFS.
                if new_fields.len() == 1 && !new_fields[0].flags.contains(FieldFlags::ARTIFICIAL) && unwrap_limit > 0 {
                    let field_val = get_struct_field(&val.val, &new_fields[0], context.memory)?;
                    val = Value {val: field_val, type_: new_fields[0].type_, flags: val.flags};
                    should_unwrap = true;
                    break;
                }

                mem::swap(&mut new_fields, &mut temp_fields);
                new_fields.clear();
                let mut changed = false;
                for f in &temp_fields {
                    // TODO: When we have custom pretty-printers, check here that f's type doesn't have one, and don't inline if it does.
                    let mut inlined = false;
                    if f.flags.contains(FieldFlags::INHERITANCE) {
                        let field_type = unsafe {&*f.type_};
                        if let Type::Struct(field_struct) = &field_type.t {
                            for mut ff in field_struct.fields().iter().cloned() {
                                ff.bit_offset += f.bit_offset;
                                new_fields.push(ff);
                            }
                            inlined = true;
                            changed = true;
                        }
                    }
                    if !inlined {
                        new_fields.push(f.clone());
                    }
                }
                if !changed {
                    break;
                }
            }
            if should_unwrap {
                continue;
            }
        }

        let mut new_type = t.clone();
        let new_struct = new_type.t.as_struct_mut().unwrap();
        let new_fields = state.types.fields_arena.add_slice(&new_fields);
        new_struct.set_fields(new_fields);
        let new_type = state.types.types_arena.add(new_type);
        return Ok((Some(Value {val: val.val.clone(), type_: new_type, flags: val.flags}), warning));
    }
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
            let t: *const TypeInfo = unsafe {mem::transmute(val.val.blob_ref().unwrap().get_usize().unwrap())};
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
                    builder.add_usize_field("type", unsafe {mem::transmute(p.type_)}, state.builtin_types.meta_type);
                }
                Type::Array(a) => {
                    if let Some((text, palette)) = &mut out {print_type_name(t, *text, *palette, 0);}
                    builder.add_usize_field("type", unsafe {mem::transmute(a.type_)}, state.builtin_types.meta_type);
                }
                Type::Struct(s) => {
                    styled_write_maybe!(out, keyword, "{}", if s.flags.contains(StructFlags::UNION) {"union"} else {"struct"});
                    show_size = true;
                    let mut fields = StructBuilder::default();
                    for f in s.fields() {
                        fields.add_usize_field(f.name, unsafe {mem::transmute(f)}, state.builtin_types.meta_field);
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
                    builder.add_usize_field("type", unsafe {mem::transmute(e.type_)}, state.builtin_types.meta_type);
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
            let f: *const StructField = unsafe {mem::transmute(val.val.blob_ref().unwrap().get_usize().unwrap())};
            let f = unsafe {&*f};
            builder.add_str_field("name", f.name, &mut state.types, &state.builtin_types);
            builder.add_usize_field("type", unsafe {mem::transmute(f.type_)}, state.builtin_types.meta_type);
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
        // Here's a partial check for that.
        // It doesn't cover the case where the struct has multiple uninformative fields (and may have size > 1 byte).
        let t = unsafe {&*f.type_};
        f = match &t.t {
            Type::Struct(s) if s.fields().len() == 0 => return true,
            Type::Struct(s) if s.fields().len() == 1 => &s.fields()[0],
            _ => return false,
        };
    }
    false
}

fn is_field_vtable_ptr(f: &StructField) -> bool {
    f.flags.contains(FieldFlags::ARTIFICIAL) && f.name.starts_with("_vptr$")
}

fn find_vtable_ptr_field_offset(t: *const TypeInfo) -> Option<usize> {
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

fn downcast_to_concrete_type(val: &Value, vtable_ptr_field_offset: usize, context: &EvalContext) -> Result<Value> {
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
    Ok(Value {val: AddrOrValueBlob::Addr(addr), type_, flags: val.flags})
}

fn resolve_discriminated_union(val: &Value, fields: &mut Vec<StructField>, context: &EvalContext, warning: &mut Option<Error>) -> Result<bool> {
    let mut discriminant: Option<usize> = None;
    for field in &mut *fields {
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

            let val = get_struct_field(&val.val, field, context.memory)?;
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
    fields.retain(|f| {
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
    if !found && has_default {
        found = true;
        fields.retain(|f| !f.flags.contains(FieldFlags::VARIANT));
    }
    if found && !has_nonvariant {
        fields.retain(|f| !f.flags.contains(FieldFlags::DISCRIMINANT));
    }
    Ok(true)
}
