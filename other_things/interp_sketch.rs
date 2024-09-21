struct InterpState {
  // 3 addressable memory ranges.
  stack: Vec<usize>,
  heap: Vec<u8>,
  debuggee_memory: MemReader,

  call_stack: Vec<usize>,

  error: Option<Error>,

  debuggee_memory_cache: [(/*addr*/ usize, Box<[u8; 4096]>)];
}

impl Memory {
  fn read_usize<'a>(&'a mut self, addr: usize) -> usize {
    let address_space = addr >> 48;
    let addr = addr & 0xffffffffffff;
    let slice: &'a[u8] = match address_space {
      0xdebe => {
        ...debuggee_memory_cache etc;
      }
      0x57ac => {
        unsafe {std::slice::from_raw_parts(self.stack.as_ptr() as *const u8, self.stack.len() * 8)}
      }
      0x4eab => {
        &self.heap
      }
    };
    if addr + 8 > slice.len() {
      self.error = Some(error!(Runtime, "{} read out-of-bounds: {} + 8 > {}", if address_space == 0x57ac {"stack"} else {"heap"}, addr, slice.len()));
      return 0;
    }
    ...load_unaligned;
  }
}

// Inspired by https://github.com/maximecb/uvm/blob/main/vm/src/vm.rs

#[repr(u8)]
enum Instruction {
  Panic,
  Nop,

  PushU64, // <imm: usize>; push(imm);
  Pop,
  Dup, // v = stack[stack.len()-1]; push(v);
  Swap,
  GetN, // <offset: usize>; v = stack[stack.len()-1-offset]; push(v);
  SetN, // <offset: usize>; v = pop(); stack[stack.len()-1-offset] = v;

  LoadU8, // addr = pop(); v = memory.read_u8(addr); push(v);
  LoadU16,
  LoadU32,
  LoadU64,
  LoadI8, // sign-extended
  LoadI16,
  LoadI32,
  LoadF32,

  Store8, // addr = pop(); v = pop(); memory.write_u8(addr, v);
  Store16,
  Store32,
  Store64,

  SxI8, // sign-extend i8 -> i64
  SxI16,
  SxI32,
  TruncU8,
  TruncU16,
  TruncU32,

  IToF, // i64 -> f64
  UToF, // u64 -> f64
  FToI,

  Add,
  Sub, // y = pop(); x = pop(); push(x - y);
  Mul,
  UDiv,
  UMod,
  IDiv,
  IMod,
  FAdd,
  FSub,
  FMul,
  FDiv,
  FMod,
  And,
  Or,
  Xor,
  Not,
  LShift,
  RShift,
  IRShift,

  Eq,
  Ne,
  ULt, // y = pop(); x = pop(); v = if x < y {!0usize} else {0}; push(v);
  UGt,
  ULe,
  UGe,
  ILt,
  IGt,
  ILe,
  IGe,
  FLt,
  FGt,
  FLe,
  FGe,

  Jmp, // <offset: i32>; pc += offset
  Jz, // <offset: i32>; f = pop(); if f == 0 {pc += offset;}
  Jnz,

  Call, // function_idx: u32
  Ret,

  Register, // <idx: u8>; push(registers.get_int(idx));
  Alloc, // <size: u32>; <allocate `size` bytes on the heap>; push(address);
  Memcpy, // size = pop(); to = pop(); from = pop(); <copy>
}



#pretty("DB::PODArray")
fn pretty_PODArray(a: $A) -> *[A::value_type] {
  use T = A::value_type;
  a.c_begin as *T .. a.c_end as *T
}

#pretty("::*std::*list", fields=["__end_"])
fn pretty_list_libcpp(list: $L) {
    use T = L::value_type;
    let end = &list.__end_; // if list is a copy, this is silently wrong!
    let elem = end.__next_;
    pretty_value("std::list of size {}", list.__size_alloc_);
    while elem != end {
        pretty_element((elem + 1) as *T);
        elem = elem.__next_;
    }
}

#pretty("::*std::*list", fields=["_M_next"])
fn pretty_list_libstdcpp(list: $L) {
    use T = L::value_type;
    pretty_value("std::list of size {}", list._M_size);
    let first = list._M_next;
    let elem = first;
    while elem->_M_next != first {
        pretty_element((elem + 1) as *T);
        elem = elem._M_next;
    }
}

watch expressions as auto-template functions; template over types of all identifiers and types; and over whether addresses are known;
variables persisting across watch expressions;
downcasting; where does auto-downcasting fit in?;
passing big values to pretty printers - can be done without copying?;
how to prevent taking address of local variables whose address is not known;
prevent assignment to program variables and registers at compile time;
dynamic arrays syntax and methods;
strings;
sorting?;
yield (?); pagination?;
pretty-printers for types introduced in the script itself;
template structs;
pretty-printers for synthetic types;
ui to check which pretty-printer was applied, see template instantiation errors and runtime errors;
printing, eprintln (shown in tooltip?)
