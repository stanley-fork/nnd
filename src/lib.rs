#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

// This is only needed on the 'use gimli::*' statements (for constants like DW_AT_name), but it doesn't work there.
#![allow(non_upper_case_globals)]

// For the offsetof macro in util.rs, because Rust doesn't support attaching an [allow()] on an individual unsafe{} block (which in turn is only needed because Rust doesn't have offsetof, ugh).
#![allow(unused_unsafe)]

pub mod error;
pub mod elf;
pub mod debugger;
pub mod util;
pub mod ui;
pub mod log;
pub mod process_info;
pub mod symbols;
pub mod symbols_registry;
pub mod procfs;
pub mod unwind;
pub mod range_index;
pub mod registers;
pub mod disassembly;
pub mod pool;
pub mod layout;
pub mod settings;
pub mod types;
pub mod expr;
pub mod search;
pub mod widgets;
pub mod arena;
pub mod executor;
pub mod context;
pub mod interp;
pub mod pretty;
pub mod persistent;
pub mod doc;
pub mod common_ui;
pub mod imgui;
pub mod terminal;
pub mod dwarf;
