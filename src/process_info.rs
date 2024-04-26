use crate::{*, debugger::*, error::*, util::*, symbols::*, symbols_registry::*, procfs::*, unwind::*, registers::*, log::*};
use std::{collections::{HashMap, hash_map::Entry}, time::Instant};
use std::mem;
use libc::pid_t;

pub struct ProcessInfo {
    pub maps: MemMapsInfo,
    // Pointers to symbols for all mapped binaries. Guaranteed to be present in SymbolsRegistry.
    pub binaries: HashMap<BinaryId, BinaryInfo>,

    // CPU and memory usage, recalculated periodically (
    pub resource_stats: ResourceStats,
}

pub struct ThreadInfo {
    pub name: Result<String>,

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

    pub resource_stats: ThreadResourceStats,
}

pub struct ResourceStats {
    pub period_ns: usize,
    pub rss_bytes: usize,
    pub all_threads: ThreadResourceStats,

    pub last_timestamp: Option<Instant>,
}
impl ResourceStats {
    pub fn new() -> Self { Self {period_ns: 0, rss_bytes: 0, all_threads: ThreadResourceStats::new(), last_timestamp: None} }

    pub fn cpu_percentage(&self) -> f64 {
        self.all_threads.cpu_percentage(self.period_ns)
    }

    fn update(&mut self, now: Instant, s: &ProcStat) {
        self.period_ns = (now - mem::replace(&mut self.last_timestamp, Some(now)).unwrap_or(now).min(now)).as_nanos() as usize;
        self.rss_bytes = s.rss * sysconf_PAGE_SIZE();
        self.all_threads.update(s);
    }
}

pub struct ThreadResourceStats {
    // CPU time during the last ResourceStats.period_ns period.
    pub cpu_user_ns: usize,
    pub cpu_system_ns: usize,
    pub state: char,

    last_utime: Option<usize>,
    last_stime: Option<usize>,
}
impl ThreadResourceStats {
    pub fn new() -> Self { Self {cpu_user_ns: 0, cpu_system_ns: 0, last_utime: None, last_stime: None, state: ' '} }

    pub fn cpu_percentage(&self, period_ns: usize) -> f64 {
        if period_ns == 0 {
            return 0.0;
        }
        (self.cpu_user_ns + self.cpu_system_ns) as f64 / period_ns as f64 * 100.0
    }

    fn update(&mut self, s: &ProcStat) {
        self.state = s.state;
        self.cpu_user_ns = (s.utime - mem::replace(&mut self.last_utime, Some(s.utime)).unwrap_or(s.utime)) * (1_000_000_000 / sysconf_SC_CLK_TCK());
        self.cpu_system_ns = (s.stime - mem::replace(&mut self.last_stime, Some(s.stime)).unwrap_or(s.utime)) * (1_000_000_000 / sysconf_SC_CLK_TCK());
    }
}

impl ProcessInfo {
    pub fn new() -> Self {
        Self {maps: Default::default(), binaries: HashMap::new(), resource_stats: ResourceStats::new()}
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
        Self {name: Ok(String::new()), regs: Registers::default(), partial_stack: None, stack: None, resource_stats: ThreadResourceStats::new()}
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

pub fn refresh_thread_info(pid: pid_t, t: &mut Thread, regs: Option<Registers>, prof: Option<&mut Profiling>) {
    if !t.exiting {
        // Refresh thread name and state.
        // State is also refreshed every second (or periodic_timer_seconds), but we want it to update immediately when the user suspends or resumes the process.
        match ProcStat::parse(&format!("/proc/{}/task/{}/stat", pid, t.tid)) {
            Ok(s) => {
                t.info.name = s.comm().map(|s| s.to_string());
                t.info.resource_stats.state = s.state;
            }
            Err(e) => t.info.name = Err(e),
        }
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

pub fn refresh_resource_stats(pid: pid_t, my_stats: &mut ResourceStats, debuggee_stats: &mut ResourceStats, threads: &mut HashMap<pid_t, Thread>) -> Result<()> {
    let now = Instant::now();
    my_stats.update(now, &ProcStat::parse("/proc/self/stat")?);
    debuggee_stats.update(now, &ProcStat::parse(&format!("/proc/{}/stat", pid))?);

    for (tid, t) in threads {
        if !t.exiting {
            t.info.resource_stats.update(&ProcStat::parse(&format!("/proc/{}/task/{}/stat", pid, tid))?)
        }
    }

    Ok(())
}

pub fn ptrace_getregs(tid: pid_t, prof: Option<&mut Profiling>) -> Result<Registers> {
    unsafe {
        let mut regs: libc::user_regs_struct = mem::zeroed();
        ptrace(libc::PTRACE_GETREGS, tid, 0, &mut regs as *mut _ as u64, prof)?;
        Ok(Registers::from_ptrace(&regs))
    }
}
