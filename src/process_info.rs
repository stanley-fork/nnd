use crate::{*, debugger::*, error::*, util::*, symbols::*, symbols_registry::*, procfs::*, unwind::*, registers::*, log::*};
use std::collections::{HashMap, hash_map::Entry};
use std::mem;
use libc::pid_t;

pub struct ProcessInfo {
    pub maps: MemMapsInfo,
    // Pointers to symbols for all mapped binaries. Guaranteed to be present in SymbolsRegistry.
    pub binaries: HashMap<BinaryId, BinaryInfo>,
}

pub struct ThreadInfo {
    pub name: String,

    pub regs: Registers,

    // These are calculated lazily and cleared when the thread switches from Running to Suspended. None means it wasn't requested yet.
    // Errors are reported through StackTrace.truncated field.
    //
    // The "partial" stack trace is intended to be shown in the list of threads, for each thread.
    // Usually debuggers show the current function name there, but that function is usually something boring like epoll or futex wait.
    // Maybe we can do better and also show the most "interesting" function from the stack trace - maybe the first function from main
    // binary, or maybe let the user specify a regex for functions to exclude, or something.
    pub partial_stack: Option<StackTrace>,
    pub stack: Option<StackTrace>,
}

impl ProcessInfo {
    pub fn new() -> Self {
        Self {maps: Default::default(), binaries: HashMap::new()}
    }

    pub fn addr_to_binary(&self, addr: usize) -> Result<&BinaryInfo> {
        let idx = self.maps.maps.partition_point(|m| m.start + m.len <= addr);
        if idx == self.maps.maps.len() || self.maps.maps[idx].start > addr {
            return err!(ProcessState, "address not mapped");
        }
        let id = match &self.maps.maps[idx].binary_id {
            None => return err!(ProcessState, "address not mapped to executable file"),
            Some(b) => b
        };
        Ok(self.binaries.get(id).unwrap())
    }

    pub fn clear(&mut self) {
        self.maps.clear();
        self.binaries.clear();
    }
}

impl ThreadInfo {
    pub fn new() -> Self {
        Self {name: String::new(), regs: Registers::default(), partial_stack: None, stack: None}
    }

    pub fn invalidate(&mut self) {
        self.regs = Registers::default();
        self.partial_stack = None;
        self.stack = None;
    }
}

pub fn refresh_maps_and_binaries_info(debugger: &mut Debugger) {
    let maps = match MemMapsInfo::read_proc_maps(debugger.pid) {
        Err(e) => {
            eprintln!("error: failed to read maps: {:?}", e);
            return;
        }
        Ok(m) => m
    };

    let mut binaries: HashMap<BinaryId, BinaryInfo> = HashMap::new();

    // Avoid returning (including '?') in this scope.
    {
        let mut prev_binaries = mem::take(&mut debugger.info.binaries);
        for map in &maps.maps {
            let id = match &map.binary_id {
                None => continue,
                Some(b) => b,
            };
            let new_entry = match binaries.entry(id.clone()) {
                Entry::Occupied(mut e) => {
                    let bin = e.get_mut();
                    if let Ok(elf) = &bin.elf {
                        bin.addr_map.update(map, elf, &id.path);
                    }
                    continue;
                }
                Entry::Vacant(v) => v,
            };

            let latest = debugger.symbols.get_or_load(id, &debugger.memory);

            let mut binary = match prev_binaries.remove(id) {
                Some(mut bin) => {
                    bin.elf = bin.elf.or(latest.elf);
                    bin.symbols = bin.symbols.or(latest.symbols);
                    bin.unwind = bin.unwind.or(latest.unwind);
                    bin
                }
                None => latest,
            };

            if let Ok(elf) = &binary.elf {
                binary.addr_map.update(map, elf, &id.path);
            }

            new_entry.insert(binary);
        }

        debugger.info.maps = maps;
        debugger.info.binaries = binaries;
    }
}

pub fn refresh_thread_info(t: &mut Thread, regs: Option<Registers>, prof: Option<&mut Profiling>) {
    if !t.exiting {
        t.info.name = std::fs::read_to_string(format!("/proc/{}/comm", t.tid)).unwrap_or_else(|e| format!("error: {}", e));
    }

    if t.state == ThreadState::Suspended {
        if let Some(regs) = regs {
            t.info.regs = regs;
        } else {
            t.info.regs = match ptrace_getregs(t.tid, prof) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("error: GETREGS failed: {:?}", e);
                    Registers::default()
                }
            }
        }
    }
}

pub fn ptrace_getregs(tid: pid_t, prof: Option<&mut Profiling>) -> Result<Registers> {
    unsafe {
        let mut regs: libc::user_regs_struct = mem::zeroed();
        ptrace(libc::PTRACE_GETREGS, tid, 0, &mut regs as *mut _ as u64, prof)?;
        Ok(Registers::from_ptrace(&regs))
    }
}
