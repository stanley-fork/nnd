use std::{mem, fmt};
use crate::{*, error::*};

#[derive(Clone)]
pub struct Registers {
    pub ints: [u64; RegisterIdx::INT_COUNT],
    // Can add floating-point and vector registers similarly. Probably use shared indexing, with ranges corresponding to types.

    // Bitmask saying which values are populated in the array(s) above (including dubious values).
    pub mask: u64,
    // Which of the values are just guesses.
    pub dubious_mask: u64,
}

#[repr(u8)]
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum RegisterIdx {
    // x86-64 registers.
    Rax = 0,
    Rdx = 1,
    Rcx = 2,
    Rbx = 3,
    Rsi = 4,
    Rdi = 5,
    Rbp = 6,
    Rsp = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
    Rip = 16,
    Cs = 17,
    Ss = 18,
    Fs = 19,
    Gs = 20,
    FsBase = 21,
    GsBase = 22,

    // Other things in user_regs_struct. Not actually registers.
    Flags = 23,
    OrigRax = 24, // as in user_regs_struct

    // Other things for unwinding. Not actually registers.
    Cfa = 25, // Canonical Frame Address from .eh_frame
    Ret = 26, // return address according to .eh_frame
}

impl RegisterIdx {
    pub const INT_COUNT: usize = RegisterIdx::Ret as usize + 1;
    pub const TOTAL_COUNT: usize = RegisterIdx::Ret as usize + 1;

    pub fn from_dwarf(r: gimli::Register) -> Option<RegisterIdx> {
        match r.0 {
            0..=16 => Some(unsafe {mem::transmute(r.0 as u8)}),
            49 => Some(RegisterIdx::Flags),
            51 => Some(RegisterIdx::Cs),
            52 => Some(RegisterIdx::Ss),
            54 => Some(RegisterIdx::Fs),
            55 => Some(RegisterIdx::Gs),
            58 => Some(RegisterIdx::FsBase),
            59 => Some(RegisterIdx::GsBase),
            _ => None,
        }
    }

    pub fn all() -> &'static [RegisterIdx] {
        &REGISTER_IDXS
    }

    pub fn name(self) -> &'static str {
        REGISTER_NAMES[self as usize]
    }

    pub fn parse_ignore_case(s: &str) -> Option<Self> {
        REGISTER_NAMES.iter().position(|n| n.eq_ignore_ascii_case(s)).map(|i| unsafe {mem::transmute(i as u8)})
    }
}

pub const REGISTER_IDXS: [RegisterIdx; RegisterIdx::TOTAL_COUNT] =  [RegisterIdx::Rax, RegisterIdx::Rdx, RegisterIdx::Rcx, RegisterIdx::Rbx, RegisterIdx::Rsi, RegisterIdx::Rdi, RegisterIdx::Rbp, RegisterIdx::Rsp, RegisterIdx::R8, RegisterIdx::R9, RegisterIdx::R10, RegisterIdx::R11, RegisterIdx::R12, RegisterIdx::R13, RegisterIdx::R14, RegisterIdx::R15, RegisterIdx::Rip, RegisterIdx::Cs, RegisterIdx::Ss, RegisterIdx::Fs, RegisterIdx::Gs, RegisterIdx::FsBase, RegisterIdx::GsBase, RegisterIdx::Flags, RegisterIdx::OrigRax, RegisterIdx::Cfa, RegisterIdx::Ret];
pub const REGISTER_NAMES: [&'static str; RegisterIdx::TOTAL_COUNT] = ["rax", "rdx", "rcx", "rbx", "rsi", "rdi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "rip", "cs", "ss", "fs", "gs", "fs_base", "gs_base", "flags", "orig_rax", "cfa", "ret"];

impl Registers {
    pub fn from_ptrace(s: &libc::user_regs_struct) -> Self {
        Registers {
            ints: [s.rax, s.rdx, s.rcx, s.rbx, s.rsi, s.rdi, s.rbp, s.rsp, s.r8, s.r9, s.r10, s.r11, s.r12, s.r13, s.r14, s.r15, s.rip, s.cs, s.ss, s.fs, s.gs, s.fs_base, s.gs_base, s.eflags, s.orig_rax, 0, 0],
            mask: (1u64 << (RegisterIdx::OrigRax as u32 + 1)) - 1,
            dubious_mask: 0,
        }
    }

    // From sigcontext_64, aka libc::mcontext_t.gregs
    pub fn from_context(r: &[u64; 23]) -> Self {
        // TODO: Take FsBase and GsBase from fpstate, it's used for TLS.
        Registers {
            ints: [r[13], r[12], r[14], r[11], r[9], r[8], r[10], r[15], r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[16], r[18] & 0xffff, r[18] >> 48, (r[18] >> 32) & 0xffff, (r[18] >> 16) & 0xffff, 0, 0, r[17], 0, 0, 0],
            mask: ((1u64 << (RegisterIdx::Flags as u32 + 1)) - 1) & !(1 << RegisterIdx::FsBase as u32) & !(1 << RegisterIdx::GsBase as u32),
            dubious_mask: 0,
        }
    }

    pub fn set_int(&mut self, reg: RegisterIdx, val: u64, dubious: bool) {
        self.ints[reg as usize] = val;
        self.mask |= 1u64 << (reg as u32);
        if dubious {
            self.dubious_mask |= 1u64 << (reg as u32);
        }
    }

    pub fn has(&self, reg: RegisterIdx) -> bool {
        self.mask & (1u64 << reg as u32) != 0
    }

    pub fn is_dubious(&self, reg: RegisterIdx) -> bool {
        self.dubious_mask & (1u64 << reg as u32) != 0
    }

    pub fn get_int(&self, reg: RegisterIdx) -> Result<(u64, /*dubious*/ bool)> {
        if self.mask & (1u64 << reg as u32) == 0 {
            err!(ProcessState, "no {}", reg)
        } else {
            Ok((self.ints[reg as usize], self.is_dubious(reg)))
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Registers {ints: unsafe {mem::zeroed()}, mask: 0, dubious_mask: 0}
    }
}

impl fmt::Display for RegisterIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", REGISTER_NAMES[*self as usize])
    }
}
