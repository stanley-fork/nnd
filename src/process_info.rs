use crate::{*, debugger::*, error::*, util::*, symbols::*, symbols_registry::*, procfs::*, unwind::*, registers::*, log::*, settings::*};
use std::{collections::{HashMap, hash_map::Entry}, time::Instant, fs, os::unix::fs::MetadataExt};
use std::mem;
use libc::pid_t;

#[derive(Default)]
pub struct ProcessInfo {
    pub maps: MemMapsInfo,
    pub exe_inode: u64,

    // CPU and memory usage, total across all threads, recalculated periodically.
    pub total_resource_stats: ResourceStats,
}

#[derive(Default)]
pub struct ThreadInfo {
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

    pub resource_stats: ResourceStats,
}

#[derive(Default)]
pub struct ResourceStatsBucket {
    utime: usize,
    stime: usize,
    duration_ns: usize,
}

// Information about thread's recent CPU usage, excluding periods of time when the thread was suspended by the debugger.
#[derive(Default)]
pub struct ResourceStats {
    pub latest: ProcStat,
    // Time when `latest` was collected. None if the thread is suspended by the debugger.
    time: Option<Instant>,
    pub error: Option<Error>, // if we failed to read /proc/.../stat last time we tried

    // If all stats updates happened by periodic timer every 250ms then calculating current CPU usage would be simple:
    // subtract the stat values between current and previous tick. But we want to (1) exclude periods when the thread
    // was suspended by the debugger, and (2) show updated stats immediately when the thread is suspended or resumed.
    // Suppose the user suspends the program 1ms after a periodic refresh. Should we show CPU usage for the 1ms?
    // Or ignore the 1ms and show usage for previous 250ms? Ideally we'd show usage for the whole 251ms. That's why
    // we're keeping two buckets of history here. If the latest bucket is big enough we show stats from it, otherwise
    // we show merged stats from two buckets.
    // (Perhaps this is overengineered, and it would be better to just have a threshold and ignore the 1ms.)
    bucket: ResourceStatsBucket,
    prev_bucket: ResourceStatsBucket,
}
impl ResourceStats {
    pub fn update(&mut self, s: Result<ProcStat>, now: Instant, suspended: bool, periodic_timer_ns: usize) {
        self.error = None;
        let s = match s {
            Err(e) => {
                self.error = Some(e);
                return;
            }
            Ok(s) => s,
        };
        if let &Some(t) = &self.time {
            let ns = (now - t).as_nanos() as usize;
            if (self.bucket.duration_ns + ns) * 2 > periodic_timer_ns {
                self.prev_bucket = mem::take(&mut self.bucket);
            }
            self.bucket.duration_ns += ns;
            self.bucket.utime += s.utime - self.latest.utime;
            self.bucket.stime += s.stime - self.latest.stime;
        }
        self.latest = s;
        self.time = if suspended {None} else {Some(now)};
    }

    pub fn cpu_percentage(&self, periodic_timer_ns: usize) -> f64 {
        let mut t = self.bucket.duration_ns;
        let mut cpu = self.bucket.utime + self.bucket.stime;
        if self.bucket.duration_ns * 2 <= periodic_timer_ns {
            t += self.prev_bucket.duration_ns;
            cpu += self.prev_bucket.utime + self.prev_bucket.stime;
        }
        if t == 0 {
            return 0.0;
        }
        cpu as f64 * 1e9 / sysconf_SC_CLK_TCK() as f64 / t as f64 * 100.0
    }
}

impl ProcessInfo {
    pub fn addr_to_binary_id(&self, addr: usize) -> Result<usize> {
        let idx = self.maps.maps.partition_point(|m| m.start + m.len <= addr);
        if idx == self.maps.maps.len() || self.maps.maps[idx].start > addr {
            return err!(ProcessState, "address not mapped");
        }
        match self.maps.maps[idx].binary_id.clone() {
            None => err!(ProcessState, "address not mapped to executable file"),
            Some(id) => Ok(id),
        }
    }

    pub fn clear(&mut self) {
        self.maps.clear();
    }
}

impl ThreadInfo {
    pub fn invalidate(&mut self) {
        self.regs = Registers::default();
        self.partial_stack = None;
        self.stack = None;
    }
}

pub fn refresh_maps_and_binaries_info(debugger: &mut Debugger) -> /*binaries_added*/ bool {
    if debugger.info.exe_inode == 0 {
        let path = format!("/proc/{}/exe", debugger.pid);
        let m = match fs::metadata(&path) {
            Err(e) => {
                eprintln!("error: failed to read {}: {}", path, e);
                return false;
            }
            Ok(x) => x };
        debugger.info.exe_inode = m.ino();
        if debugger.info.exe_inode == 0 {
            eprintln!("error: stat({}) returned inode number 0", path);
            debugger.info.exe_inode = 1;
        }
    }

    let mut maps = match MemMapsInfo::read_proc_maps(debugger.pid) {
        Err(e) => {
            eprintln!("error: failed to read maps: {}", e);
            return false;
        }
        Ok(m) => m };

    let prev_mapped = debugger.symbols.mark_all_as_unmapped();

    for (idx, map) in maps.maps.iter_mut().enumerate() {
        let locator = match &map.binary_locator {
            None => continue,
            Some(b) => b,
        };
        let bin = match debugger.symbols.locator_to_id.get(locator) {
            Some(id) => debugger.symbols.get_mut(*id).unwrap(),
            None => {
                let mut additional_elf_paths: Vec<String> = Vec::new();
                let custom_path = if locator.inode == debugger.info.exe_inode {
                    if let Some(p) = debugger.context.settings.unstripped_executable_path.clone() {
                        additional_elf_paths.push(p);
                    }

                    // Use this special symlink instead of the regular path because it's available even after the file is deleted.
                    // Useful when recompiling the program without closing the debugger.
                    Some(format!("/proc/{}/exe", debugger.pid))
                } else {
                    None
                };
                debugger.symbols.add(locator.clone(), &debugger.memory, custom_path, additional_elf_paths)
            }
        };
        map.binary_id = Some(bin.id);
        bin.is_mapped = true;
        if let Ok(elf) = &bin.elf {
            bin.addr_map.update(map, elf, &locator.path);
        }
        bin.mmap_idx = idx;
    }
    debugger.symbols.update_priority_order();

    debugger.info.maps = maps;

    // Check if any of the mapped binaries are new or changed address.
    for b in debugger.symbols.iter() {
        if b.is_mapped && prev_mapped.get(&b.id) != Some(&b.addr_map.diff) {
            eprintln!("info: new binaries mapped");
            return true;
        }
    }
    false
}

// Must be called when the thread gets suspended (and not immediately resumed) - to assign registers, so we can unwind the stack.
// Should also be called when the thread is created (to assign thread name) or resumed (to update stats).
// We skip calling this if the thread was suspended and immediately resumed, e.g. skipped conditional breakpoints or passed-through user signals.
pub fn refresh_thread_info(pid: pid_t, t: &mut Thread, prof: &mut ProfileBucket, settings: &Settings) {
    if !t.exiting {
        let s = ProcStat::parse(&format!("/proc/{}/task/{}/stat", pid, t.tid), prof);
        t.info.resource_stats.update(s, Instant::now(), t.state == ThreadState::Suspended, settings.periodic_timer_ns);
    }

    if t.state == ThreadState::Suspended {
        t.info.regs = match ptrace_getregs(t.tid, prof) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("error: GETREGS failed: {:?}", e);
                Registers::default()
            }
        }
    }
}

// Called periodically to refresh stats for all threads, and totals. Stats are also refreshed by refresh_thread_info() when threads are suspended or resumed.
pub fn refresh_all_resource_stats(pid: pid_t, my_stats: &mut ResourceStats, debuggee_stats: &mut ResourceStats, threads: &mut HashMap<pid_t, Thread>, prof: &mut ProfileBucket, settings: &Settings) -> Option<Error> {
    let now = Instant::now();
    my_stats.update(ProcStat::parse("/proc/self/stat", prof), now, false, settings.periodic_timer_ns);
    let mut any_error = my_stats.error.clone();
    if !threads.is_empty() {
        debuggee_stats.update(ProcStat::parse(&format!("/proc/{}/stat", pid), prof), now, false, settings.periodic_timer_ns);
        any_error = any_error.or_else(|| debuggee_stats.error.clone());
    } else {
        *debuggee_stats = ResourceStats::default();
    }

    for (tid, t) in threads {
        if !t.exiting {
            let s = ProcStat::parse(&format!("/proc/{}/task/{}/stat", pid, tid), prof);
            t.info.resource_stats.update(s, Instant::now(), t.state == ThreadState::Suspended, settings.periodic_timer_ns);
            any_error = any_error.or_else(|| t.info.resource_stats.error.clone());
        }
    }

    any_error
}

pub fn ptrace_getregs(tid: pid_t, prof: &mut ProfileBucket) -> Result<Registers> {
    unsafe {
        let mut regs: libc::user_regs_struct = mem::zeroed();
        ptrace(libc::PTRACE_GETREGS, tid, 0, &mut regs as *mut _ as u64, prof)?;
        Ok(Registers::from_ptrace(&regs))
    }
}
