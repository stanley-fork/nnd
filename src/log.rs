use crate::{*, util::*};
use std::{collections::VecDeque, time::Instant, mem, fmt::Write};
use core::arch::x86_64::_rdtsc;
use tui::style::{Style, Color, Modifier};

// We should rework logging. I initially thought we'd want to show a very minimal log on screen.
// (Not at all like a typical server log, where you spam 1 MB/s of garbage.)
// That's what this current struct is about.
// But that's probably not very useful, and instead we should have two separate things:
//  1. Showing nice errors in intelligently chosen places in the UI. E.g. if a binary is missing debug symbols, show that in the list of binaries. We already do this.
//  2. Optionally writing various warnings to a log file, intended for the developer (me). E.g. if DWARF contains something unexpected or is missing something important.
//     Currently I'm using stderr for that, which may or may not be good enough.
pub struct Log {
    pub lines: VecDeque<String>,
    pub prof: Profiling,
}

const MAX_LINES: usize = 100;

impl Log {
    pub fn new() -> Log {
        Log {lines: VecDeque::new(), prof: Profiling::new()}
    }

    pub fn add_line(&mut self, line: String) {
        self.lines.push_back(line);
        while self.lines.len() > MAX_LINES {
            self.lines.pop_front();
        }
    }

    pub fn clear(&mut self) {
        self.lines.clear();
    }
}
#[macro_export]
macro_rules! log {
    ($log:expr, $($arg:tt)*) => (
        ($log).add_line(format!($($arg)*))
    );
}

// A very minimal profiling thing. Prints to stderr in destructor.
pub struct ProfileScope {
    name: String,
    start: Instant,
    start_tsc: u64,
    total_tsc: u64,
    threshold_secs: f64,
    active: bool,
    multi: bool,
}

fn rdtsc() -> u64 {
    unsafe {_rdtsc()} // why is it unsafe?
}

impl ProfileScope {
    pub fn new(name: String) -> Self {
        ProfileScope {start: Instant::now(), start_tsc: 0, total_tsc: 0, name: name, threshold_secs: 0.0, active: true, multi: false}
    }

    pub fn with_threshold(secs: f64, name: String) -> Self {
        ProfileScope {start: Instant::now(), start_tsc: 0, total_tsc: 0, name: name, threshold_secs: secs, active: true, multi: false}
    }

    pub fn multi(name: String) -> Self {
        ProfileScope {start: Instant::now(), start_tsc: rdtsc(), total_tsc: 0, name: name, threshold_secs: 0.0, active: true, multi: true}
    }

    pub fn multi_with_threshold(secs: f64, name: String) -> Self {
        ProfileScope {start: Instant::now(), start_tsc: rdtsc(), total_tsc: 0, name: name, threshold_secs: secs, active: true, multi: true}
    }

    pub fn add(&mut self, tsc: u64) {
        assert!(self.multi);
        self.total_tsc += tsc;
    }
}

impl Drop for ProfileScope {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        let mut secs = self.start.elapsed().as_secs_f64();
        if self.multi {
            let end_tsc = rdtsc();
            secs = secs * (self.total_tsc as f64 / ((end_tsc - self.start_tsc) as f64));
        }
        if self.threshold_secs <= 0.0 || secs >= self.threshold_secs {
            eprintln!("info: {} took {:.3}s", self.name, secs);
        }
    }
}

pub struct TscScope {
    start: u64,
}

impl TscScope {
    pub fn new() -> Self {
        Self {start: rdtsc()}
    }

    pub fn finish(&self) -> u64 {
        rdtsc() - self.start
    }
}

pub struct Profiling {
    pub syscall_count: usize,
    pub syscall_tsc: u64,
    pub debugger_event_count: usize,
    pub terminal_tsc: u64,

    // Debugger often receives many ptrace events in quick succession, e.g. one event per thread when stopping all threads.
    // They might take multiple epoll iterations to arrive. We group those iterations to report the whole burst of activity as one item.
    // A burst starts with a debugger event (iteration_debugger()) and ends when two consecutive renders had no debugger events in between.
    // To detect the end of a burst, we request an additional re-render whenever there were debugger events between two consecutive renders,
    // i.e. we render an additional frame at the end of each burst, which is also useful for showing the information about the burst.
    // (Maybe we should disable this extra rendering when profiler is not shown.)
    burst_tsc: u64,
    burst_syscall_count: usize,
    burst_syscall_tsc: u64,
    burst_event_count: usize,
    burst_iterations: usize,
    had_debugger_events_since_last_render: bool,

    start_time: Instant,
    start_tsc: u64,
    ms_per_tsc: Option<f64>,

    history: VecDeque<StyledText>,
}
impl Profiling {
    pub fn new() -> Self { Self {syscall_count: 0, syscall_tsc: 0, debugger_event_count: 0, terminal_tsc: 0, burst_tsc: 0, burst_syscall_count: 0, burst_syscall_tsc: 0, burst_event_count: 0, burst_iterations: 0, had_debugger_events_since_last_render: false, start_time: Instant::now(), start_tsc: rdtsc(), history: VecDeque::new(), ms_per_tsc: None} }

    pub fn format_summary(&mut self, out: &mut StyledText, width: usize) {
        while self.history.len() > MAX_LINES {
            self.history.pop_front();
        }
        for t in self.history.iter().rev() {
            if t.chars.len() + 1 <= width && out.unclosed_line_len() + t.chars.len() + 1 > width {
                out.close_line();
            }
            styled_write!(out, Style::default(), " ");
            out.append_to_unclosed_line(t);
        }
        out.close_line();
    }

    pub fn iteration_render_start(&mut self) -> /*should_render_again*/ bool {
        if mem::take(&mut self.had_debugger_events_since_last_render) {
            return true;
        }
        if self.burst_iterations == 0 {
            return false;
        }
        let ms_per_tsc = self.get_ms_per_tsc();
        let event_ms = (mem::take(&mut self.burst_tsc) - self.burst_syscall_tsc) as f64 * ms_per_tsc;
        let syscall_ms = mem::take(&mut self.burst_syscall_tsc) as f64 * ms_per_tsc;
        let mut out = StyledText::new();
        styled_write!(out, Style::default().fg(Color::Magenta), "d:");
        styled_write!(out, Style::default(), "{:.0}", event_ms);
        styled_write!(out, Style::default().add_modifier(Modifier::DIM), "/{}/{}:", mem::take(&mut self.burst_event_count), mem::take(&mut self.burst_iterations));
        styled_write!(out, Style::default(), "{:.0}", syscall_ms);
        styled_write!(out, Style::default().add_modifier(Modifier::DIM), "/{}", mem::take(&mut self.burst_syscall_count));
        self.history.push_back(out);
        false
    }
    pub fn iteration_render_end(&mut self, tsc: u64) {
        let ms_per_tsc = self.get_ms_per_tsc();
        let render_ms = (tsc - self.terminal_tsc) as f64 * ms_per_tsc;
        let terminal_ms = self.terminal_tsc as f64 * ms_per_tsc;
        let mut out = StyledText::new();
        styled_write!(out, Style::default().fg(Color::Green), "r:");
        styled_write!(out, Style::default(), "{:.0}", render_ms);
        styled_write!(out, Style::default().add_modifier(Modifier::DIM), ":");
        styled_write!(out, Style::default(), "{:.0}", terminal_ms);
        self.history.push_back(out);
        self.clear_iteration_counters();
    }
    pub fn iteration_debugger(&mut self, tsc: u64) {
        self.burst_tsc += tsc;
        self.burst_syscall_count += self.syscall_count;
        self.burst_syscall_tsc += self.syscall_tsc;
        self.burst_event_count += self.debugger_event_count;
        self.burst_iterations += 1;
        self.had_debugger_events_since_last_render = true;
        self.clear_iteration_counters();
    }
    pub fn iteration_other(&mut self, tsc: u64) {
        let ms = tsc as f64 * self.get_ms_per_tsc();
        if ms < 10.0 {
            return;
        }
        let mut out = StyledText::new();
        styled_write!(out, Style::default().fg(Color::Cyan), "o:");
        styled_write!(out, Style::default(), "{:.0}", ms);
        self.history.push_back(out);
        self.clear_iteration_counters();
    }

    fn clear_iteration_counters(&mut self) {
        (self.syscall_count, self.syscall_tsc, self.debugger_event_count, self.terminal_tsc) = (0, 0, 0, 0);
    }

    fn get_ms_per_tsc(&mut self) -> f64 {
        if let Some(x) = &self.ms_per_tsc {
            return *x;
        }
        let tsc = rdtsc() - self.start_tsc;
        if tsc == 0 {
            return 0.0;
        }
        let ms = self.start_time.elapsed().as_secs_f64() * 1000.0;
        let x = ms / tsc as f64;
        if ms > 1000.0 {
            self.ms_per_tsc = Some(x);
        }
        x
    }
}
