use crate::{*, debugger::*, error::*, log::*, symbols::*, symbols_registry::*, util::*, registers::*, procfs::*, unwind::*, disassembly::*, pool::*, layout::*, settings::*, context::*, types::*, expr::*, widgets::*, search::*, arena::*, interp::*};
use termion::screen::{ToAlternateScreen, ToMainScreen, AlternateScreen, IntoAlternateScreen};
use termion::input::{self, TermRead};
use termion::event::{Event, Key};
use std::{io::{self, Write, BufRead, BufReader, Read}, mem::{self, take}, collections::{HashSet, HashMap, hash_map::Entry}, os::fd::AsRawFd, path, path::{Path, PathBuf}, fs::File, fmt::Write as FmtWrite, borrow::Cow, ops::Range, str, os::unix::ffi::OsStrExt, sync::{Arc}};
use tui::{self, backend::TermionBackend, Terminal, widgets::{Widget, Block, Borders, List, ListItem, Table, TableState, Row, Cell, Paragraph, Wrap, Tabs, Clear}, layout::{self, Constraint, Direction, Rect, Alignment, Margin}, style::{Style, Color, Modifier}, text::{Span, Spans, Text}};
use libc::{self, pid_t};

pub struct UI {
    terminal: Terminal<Backend>,
    input_events: input::Events<NonblockingRead>,
    state: UIState,
    layout: Layout,
    keys: Vec<Key>,
}

#[derive(Debug)]
pub struct UIUpdateResult {
    pub quit: bool,
    pub drop_caches: bool,
    pub redraw: bool,
    pub loading: bool, // redraw periodically
}
impl Default for UIUpdateResult { fn default() -> Self { Self {quit: false, drop_caches: false, redraw: false, loading: false} } }

pub struct UIState {
    last_error: String, // reset on next key press
    hints: [StyledText; 2], // 2 columns
    profiler_enabled: bool,

    selected_thread: pid_t,
    selected_frame: usize,
    selected_subframe: usize,

    // Selected in disassembly window.
    selected_addr: Option<(BinaryId, /*function_idx*/ usize, /*addr*/ usize)>,

    // Redundant copies of information from Debugger. Updated on every frame. Used for convenience and for consistent ordering.
    binaries: Vec<BinaryId>, // assigned by BinariesWindow
    stack: StackTrace, // assigned by StackWindow

    // Communication between windows, for interactions like:
    //  * when switching stack frames (for any of a number of reasons, including the above), scroll to the corresponding source file+line and disassembly instruction,
    //  * when selecting a breakpoint in the breakpoints window, scroll to the corresponding line in source and instruction and disassembly.
    // Currently this type of information only flows in one direction, with no circular dependencies: breakpoints -> threads -> stack -> (source, disassembly).
    // But that might change in future, e.g. maybe source and disassembly window will be able to both scroll each other when requested.
    // The only_if_error_tab solves the following; typically the should_scroll_* are first requested before symbols are loaded; that makes source and disassembly windows initially show errors;
    // when symbols are loaded, we want to try again and stop showing errors; but maybe the user already opened some non-error file and is looking at it - then we shouldn't forcefully switch to current file;
    // so we set this flag, which tells the windows to scroll only if the error tab is selected.
    should_scroll_source: Option<(Option<SourceScrollTarget>, /*only_if_on_error_tab*/ bool)>,
    should_scroll_disassembly: Option<(Result<DisassemblyScrollTarget>, /*only_if_on_error_tab*/ bool)>,

    // If window A makes a change that may be visible in window B (e.g. source window adds a breakpoint, which will appear in breakpoints window),
    // and A's WindowType > B's WindowType, then A should set this flag to true to trigger another frame to be rendered immediately after this one.
    should_redraw: bool,
    // If there's a progress bar on screen, redraw periodically. This is separate from being notified when loading completes - that happens without delay through event fd.
    loading: bool,
}

struct SourceScrollTarget {
    path: PathBuf,
    version: FileVersionInfo,
    line: usize,
}

struct DisassemblyScrollTarget {
    binary_id: BinaryId,
    function_idx: usize,
    addr: usize,
    subfunction_level: u16,
}

pub trait WindowContent {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: Vec<Key>, f: Option<&mut Frame>, area: Rect);

    // Called after some symbols finished loading, or if the user requested a redraw.
    fn drop_caches(&mut self) {}

    fn get_hints(&self, hints: &mut StyledText) {}

    // We have two cases of "modal" interaction:
    //  * Text input inside a window, e.g. editing a watch expression.
    //  * Modal dialog, e.g. opening a file. Rendered on top of all windows, but it still "belongs" to a window (e.g. open file dialog belongs to the source code window).
    // At most one modal interaction can be active at a time. The interaction gets to process keyboard inputs before any windows or global hotkeys (in particular, for text input vs single-key hotkeys).
    // Only the active window can have a modal interaction; if the window becomes inactive, the interaction is cancelled.

    // Called for active window before processing input or updating any other windows.
    fn update_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {}
    // Called when window ceases to be active.
    fn cancel_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger) {}
    // Called for active window after rendering all windows. May call f.set_cursor() to show the cursor. It's also ok to call set_cursor() from modal window's update_and_render(), if that's more convenient.
    fn render_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {}

    fn has_persistent_state(&self) -> bool {false}
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {panic!("unreachable")}
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {panic!("unreachable")}
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Copy, Hash)]
#[repr(u8)]
pub enum WindowType {
    // Some windows must be updated in a specific order. E.g. threads window may change ui.selected_thread and therefore must be rendered before any windows that depend on current thread.
    // This enum defines the order in which windows should be rendered.
    // If there are circular dependencies (e.g. breakpoints window wants to scroll the code window, and the code window may add breakpoints), the later window can set should_redraw = true to ask an earlier window to refresh.
    Binaries = 0,
    Breakpoints = 1,
    Threads = 2,
    Stack = 3,

    Code = 4,
    Disassembly = 5,

    Locations = 6,
    Watches = 7,

    Other = 8,

    Status = 9,
}

impl UI {
    pub fn new() -> Result<Self> {
        let stdout = io::stdout();
        let backend = TermionBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let nonblocking_stdin = NonblockingRead {fd: io::stdin().as_raw_fd()};
        let input_events = nonblocking_stdin.events();

        let layout = Self::default_layout();

        Ok(UI {terminal, input_events, layout, keys: Vec::new(), state: UIState {last_error: String::new(), hints: [StyledText::new(), StyledText::new()], profiler_enabled: false, selected_thread: 0, selected_addr: None, binaries: Vec::new(), stack: StackTrace::error(error!(Loading, "")), selected_frame: 0, selected_subframe: 0, should_scroll_source: None, should_scroll_disassembly: None, should_redraw: false, loading: false}})
    }

    pub fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        for (id, w) in self.layout.sorted_windows() {
            if !w.content.has_persistent_state() {
                continue;
            }
            out.write_u8(w.type_ as u8)?;
            w.content.save_state(out)?;
        }
        out.write_u8(255)?;
        Ok(())
    }

    pub fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        for (id, w) in self.layout.sorted_windows_mut() {
            if !w.content.has_persistent_state() {
                continue;
            }
            let t = inp.read_u8()?;
            if t != w.type_ as u8 {
                return err!(Environment, "unexpected window type in save file: {} instead of {}", t, w.type_ as u8);
            }
            w.content.load_state(inp)?;
        }
        let t = inp.read_u8()?;
        if t != 255 {
            return err!(Environment, "unexpected extra window ({}) in save file", t);
        }
        Ok(())
    }

    fn default_layout() -> Layout {
        // +---------+-------+-------------+
        // |  hints  |       |  binaries   |
        // |         |       | breakpoints |
        // +---------+ disas +-------------+
        // | status  |       |             |
        // +---------+-------+    stack    |
        // | locals  |       |             |
        // | watches | code  +-------------+
        // |  regs   |       |   threads   |
        // |  locs   |       |             |
        // +---------+-------+-------------+
        //
        // Other windows we could add: terminal, memory, detached watch.

        let mut layout = Layout::new();
        let cols = layout.split(layout.root, Direction::Horizontal, vec![0.3, 0.75]);

        let rows = layout.split(cols[0], Direction::Vertical, vec![0.3, 0.4]);
        layout.new_window(Some(rows[0]), WindowType::Other, true, "cheat sheet".to_string(), Box::new(HintsWindow::default()));
        layout.rigidify(rows[0], 8);
        layout.new_window(Some(rows[1]), WindowType::Status, true, "status".to_string(), Box::new(StatusWindow::default()));
        layout.rigidify(rows[1], 7);
        layout.new_window(Some(rows[2]), WindowType::Watches, true, "locals".to_string(), Box::new(WatchesWindow::new(true)));
        layout.new_window(Some(rows[2]), WindowType::Watches, true, "watches".to_string(), Box::new(WatchesWindow::new(false)));
        layout.new_window(Some(rows[2]), WindowType::Watches, true, "registers".to_string(), Box::new(RegistersWindow::default()));
        layout.new_window(Some(rows[2]), WindowType::Locations, true, "locations".to_string(), Box::new(LocationsWindow::default()));
        layout.set_hotkey_number(rows[2], 1);

        let rows = layout.split(cols[1], Direction::Vertical, vec![0.4]);
        layout.new_window(Some(rows[0]), WindowType::Disassembly, true, "disassembly".to_string(), Box::new(DisassemblyWindow::default()));
        layout.set_hotkey_number(rows[0], 2);
        layout.new_window(Some(rows[1]), WindowType::Code, true, "code".to_string(), Box::new(CodeWindow::default()));
        layout.set_hotkey_number(rows[1], 3);

        let rows = layout.split(cols[2], Direction::Vertical, vec![0.25, 0.75]);
        layout.new_window(Some(rows[0]), WindowType::Binaries, true, "binaries".to_string(), Box::new(BinariesWindow::default()));
        layout.new_window(Some(rows[0]), WindowType::Breakpoints, true, "breakpoints".to_string(), Box::new(BreakpointsWindow::default()));
        layout.set_hotkey_number(rows[0], 4);
        layout.new_window(Some(rows[1]), WindowType::Stack, true, "stack".to_string(), Box::new(StackWindow::default()));
        layout.set_hotkey_number(rows[1], 5);
        let threads_window = layout.new_window(Some(rows[2]), WindowType::Threads, true, "threads".to_string(), Box::new(ThreadsWindow::default()));
        layout.set_hotkey_number(rows[2], 6);

        layout.active_window = Some(threads_window);

        layout
    }

    pub fn buffer_input(&mut self) -> Result<()> {
        while let Some(event) = self.input_events.next() {
            let event = match event {
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                Err(e) => return Err(Error::from_io_error(e, "stdin read failed".to_string())),
                Ok(e) => e,
            };
            match event {
                Event::Key(key) => self.keys.push(key),
                Event::Mouse(_) => (),
                Event::Unsupported(_) => (),
            }
        }
        Ok(())
    }

    pub fn update_and_render(&mut self, debugger: &mut Debugger) -> Result<UIUpdateResult> {
        let mut res = UIUpdateResult::default();
        let mut keys = mem::take(&mut self.keys);
        if !keys.is_empty() {
            self.state.last_error = String::new();
        }
        /* this sounded like a good idea, but it breaks copy-pasting into the terminal
        if keys.len() > 100 {
            self.state.last_error = format!("dropping {} inputs", keys.len());
            eprintln!("warning: dropping {} inputs", keys.len());
            keys.clear();
        }*/

        let active_window_id = self.layout.active_window.clone();
        if let &Some(id) = &active_window_id {
            let w = self.layout.windows.get_mut(id);
            w.content.update_modal(&mut self.state, debugger, &mut keys);
        }

        let mut remaining_keys: Vec<Key> = Vec::new();
        let mut additional_keys_for_window: HashMap<WindowType, Vec<Key>> = HashMap::new();
        let mut deferred_step_keys: Vec<KeyAction> = Vec::new();
        for key in keys {
            match debugger.context.settings.keys.map.get(&key) {
                Some(KeyAction::Quit) => return Ok(UIUpdateResult {quit: true, ..Default::default()}),
                Some(KeyAction::Run) => {
                    let r = debugger.start_child();
                    report_result(&mut self.state, &r);
                }
                Some(KeyAction::Continue) => {
                    let r = debugger.resume();
                    report_result(&mut self.state, &r);
                }
                Some(KeyAction::Suspend) => {
                    let r = debugger.suspend();
                    report_result(&mut self.state, &r);
                }
                Some(KeyAction::Kill) => {
                    let r = debugger.murder();
                    report_result(&mut self.state, &r);
                }
                Some(x) if [KeyAction::StepIntoLine, KeyAction::StepIntoInstruction, KeyAction::StepOverLine, KeyAction::StepOverColumn, KeyAction::StepOverInstruction, KeyAction::StepOut, KeyAction::StepOutNoInline].contains(x) => {
                    // Initiate step later, after StackWindow updates selected_subframe.
                    // Otherwise rapid repeated step-over can effectively step-into instead of -over because selected_subframe is 0 and not assigned yet.
                    // This is kind of a hack, maybe there's a better way to think about input handling in general (separate update from render again?),
                    // but this is working fine for now.
                    deferred_step_keys.push(*x);
                }

                Some(KeyAction::DropCaches) => {
                    self.terminal.clear()?;
                    res.drop_caches = true;
                }

                Some(KeyAction::WindowLeft) => self.layout.switch_to_adjacent_window(-1, 0),
                Some(KeyAction::WindowUp) => self.layout.switch_to_adjacent_window(0, -1),
                Some(KeyAction::WindowDown) => self.layout.switch_to_adjacent_window(0, 1),
                Some(KeyAction::WindowRight) => self.layout.switch_to_adjacent_window(1, 0),
                Some(KeyAction::Window(idx)) => self.layout.switch_to_window_with_hotkey_number(*idx),

                Some(KeyAction::NextTab) => self.layout.switch_tab_in_active_window(1),
                Some(KeyAction::PreviousTab) => self.layout.switch_tab_in_active_window(-1),

                Some(KeyAction::PreviousStackFrame) => if let Some(k) = debugger.context.settings.keys.find(KeyAction::CursorUp) {additional_keys_for_window.entry(WindowType::Stack).or_default().push(k)},
                Some(KeyAction::NextStackFrame) => if let Some(k) = debugger.context.settings.keys.find(KeyAction::CursorDown) {additional_keys_for_window.entry(WindowType::Stack).or_default().push(k)},
                Some(KeyAction::PreviousThread) => if let Some(k) = debugger.context.settings.keys.find(KeyAction::CursorUp) {additional_keys_for_window.entry(WindowType::Threads).or_default().push(k)},
                Some(KeyAction::NextThread) => if let Some(k) = debugger.context.settings.keys.find(KeyAction::CursorDown) {additional_keys_for_window.entry(WindowType::Threads).or_default().push(k)},

                Some(KeyAction::ToggleProfiler) => self.state.profiler_enabled ^= true,

                _ => remaining_keys.push(key),
            }
        }

        // Notify window that it lost focus.
        // (This only covers losing focus due to keyboard input. If more cases are added in future, e.g. auto-focusing windows depending on the process state, make sure to move this around to cover them too.)
        if self.layout.active_window != active_window_id {
            if let &Some(id) = &active_window_id {
                if let Some(w) = self.layout.windows.try_get_mut(id) {
                    w.content.cancel_modal(&mut self.state, debugger);
                }
            }
        }

        // Hints window content. Keep it brief, there's not much space.
        let mut hints = [StyledText::new(), StyledText::new()];
        if debugger.target_state == ProcessState::NoProcess {
            styled_write!(hints[0], Style::default(), "q - quit"); hints[0].close_line();
        } else if debugger.mode == RunMode::Attach {
            styled_write!(hints[0], Style::default(), "q - detach and quit"); hints[0].close_line();
        } else {
            styled_write!(hints[0], Style::default(), "q - kill and quit"); hints[0].close_line();
        }
        styled_write!(hints[1], Style::default(), "[0-9]/C-wasd - switch window"); hints[1].close_line();
        styled_write!(hints[1], Style::default(), "C-t/C-b - switch tab"); hints[1].close_line();
        match debugger.target_state {
            ProcessState::Running | ProcessState::Stepping => {
                styled_write!(hints[0], Style::default(), "C - suspend"); hints[0].close_line();
            }
            ProcessState::Suspended => {
                styled_write!(hints[0], Style::default(), "]/[/}}/{{ - switch frame/thread"); hints[0].close_line();
                styled_write!(hints[0], Style::default(), "c - continue"); hints[0].close_line();
                styled_write!(hints[0], Style::default(), "s/n/f - step into/over/out"); hints[0].close_line();
                styled_write!(hints[0], Style::default(), "m - step over column"); hints[0].close_line();
                styled_write!(hints[0], Style::default(), "S/N - step into/over instruction"); hints[0].close_line();
            }
            ProcessState::NoProcess if debugger.mode == RunMode::Run => {
                styled_write!(hints[0], Style::default(), "r - start"); hints[0].close_line();
            }
            _ => (),
        }
        if debugger.mode == RunMode::Run && debugger.target_state != ProcessState::NoProcess {
            styled_write!(hints[0], Style::default(), "k - kill"); hints[0].close_line();
        }
        if let &Some(id) = &self.layout.active_window {
            if let Some(w) = self.layout.windows.try_get(id) {
                hints[1].close_line();
                w.content.get_hints(&mut hints[1]);
            }
        }
        self.state.hints = hints;

        let mut terminal_prof = TscScope::new(); // separately time the things that draw() does before and after calling our function, which is mostly interacting with OS terminal
        self.terminal.draw(|f| {
            debugger.log.prof.terminal_tsc += terminal_prof.finish();
            self.layout.layout_and_render_peripherals(f, f.size(), &debugger.context.settings.palette);

            let active_window = self.layout.active_window.as_ref().copied();
            for (win_id, win) in self.layout.sorted_windows_mut() {
                let mut keys_for_window = if &Some(win_id) == &active_window { mem::take(&mut remaining_keys) } else { Vec::new() };
                if let Some(k) = additional_keys_for_window.get(&win.type_) {
                    keys_for_window.extend_from_slice(k);
                }
                if let &Some(area) = &win.area {
                    win.content.update_and_render(&mut self.state, debugger, keys_for_window, Some(f), area);
                } else {
                    win.content.update_and_render(&mut self.state, debugger, keys_for_window, None, Rect::default());
                }
            }

            if let &Some(id) = &active_window {
                let win = self.layout.windows.get_mut(id);
                win.content.render_modal(&mut self.state, debugger, f, win.area.as_ref().copied().unwrap_or(Rect::default()), f.size());
            }

            terminal_prof = TscScope::new();
        })?;
        debugger.log.prof.terminal_tsc += terminal_prof.finish();

        for x in deferred_step_keys {
            let (kind, by_instructions, use_line_number_with_column) = match x {
                KeyAction::StepIntoLine => (StepKind::Into, false, false),
                KeyAction::StepIntoInstruction => (StepKind::Into, true, false),
                KeyAction::StepOverLine => (StepKind::Over, false, false),
                KeyAction::StepOverColumn => (StepKind::Over, false, true),
                KeyAction::StepOverInstruction => (StepKind::Over, true, false),
                KeyAction::StepOut => (StepKind::Out, false, false),
                KeyAction::StepOutNoInline => (StepKind::Out, true, false),
                _ => panic!("huh"),
            };
            let r = debugger.step(self.state.selected_thread, self.state.selected_subframe, kind, by_instructions, use_line_number_with_column);
            report_result(&mut self.state, &r);
            self.state.should_redraw = true;
        }

        res.redraw = mem::take(&mut self.state.should_redraw);
        res.loading = mem::take(&mut self.state.loading);
        Ok(res)
    }

    pub fn drop_caches(&mut self) {
        for (win_id, win) in self.layout.windows.iter_mut() {
            win.content.drop_caches();
        }
    }
}

fn report_result<R>(ui: &mut UIState, r: &Result<R>) {
    if let Err(e) = r {
        ui.last_error = format!("{}", e);
        if !e.is_usage() {
            eprintln!("warning: {}", e);
        }
    }
}

struct RegistersWindow {
    scroll: Scroll,
}

impl Default for RegistersWindow { fn default() -> Self { Self {scroll: Scroll::new()} } }

impl WindowContent for RegistersWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let mut rows: Vec<Row> = Vec::new();
        if let Some(frame) = ui.stack.frames.get(ui.selected_frame) {
            let regs = &frame.regs;
            for reg in RegisterIdx::all() {
                if let Ok((v, dubious)) = regs.get_int(*reg) {
                    rows.push(Row::new(vec![
                        Cell::from(format!("{}", reg)),
                        Cell::from(format!("{:x}", v)).style(if dubious {palette.value_dubious} else {palette.value}),
                    ]));
                }
            }
        }

        let range = self.scroll.update_cursorless(rows.len(), area.height.saturating_sub(1), &mut keys, &debugger.context.settings.keys);
        let rows = rows[range].to_vec();
        let table = Table::new(rows)
            .header(Row::new(vec!["reg", "value"]).style(palette.table_header))
            .widths(&[Constraint::Length(7), Constraint::Length(16)]);

        f.render_widget(table, area);
     }
}

pub struct ValueTreeNode {
    name: &'static str, // points into Symbols, or into ValueTree, or static
    value: Result<Value>,
    dubious: bool,

    // Identifier this node among its siblings. For purposes of remembering which subtrtees were expanded and where the cursor was, in a way that survives recalculation or partial change of the values.
    // E.g. array index, or hash of (field name, field idx), or watch id.
    child_id: usize,

    parent: usize, // index in previous level

    // These are populated lazily.
    formatted_line: [Option<usize>; 2], // [collapsed, expanded], line index in ValueTree.text
    has_children: Option<bool>,
    children: Option<Range<usize>>, // indices in next level
}
impl Default for ValueTreeNode { fn default() -> Self { Self {name: "", value: err!(Internal, "if you're seeing this, there's a bug" /*(seeing in ui or in log, not in code, obviously)*/), dubious: false, child_id: 0, parent: 0, formatted_line: [None; 2], has_children: None, children: None} } }

pub struct ValueTree {
    // Level 0 is a dummy root, level 1 is the list of watches or local variables, levels 2+ are expanded struct/array/pointer/container/etc contents.
    levels: Vec<Vec<ValueTreeNode>>,
    text: StyledText,
    arena: Arena,
}
impl ValueTree {
    fn new() -> Self { Self {levels: vec![Vec::new(), Vec::new()], text: StyledText::new(), arena: Arena::new()} }
    fn clear(&mut self) { *self = Self::new(); }
}

struct WatchesWindow {
    is_locals_window: bool,
    next_watch_id: usize,

    // When this changes, we recalculate everything.
    // When the first two change, we clear expanded_paths.
    seen: (/*thread*/ pid_t, /*selected_subframe*/ usize, /*stop_count*/ usize),

    expressions: Vec<(/*child_id*/ usize, String)>,
    text_input: Option<(/*child_id*/ usize, Rect, TextInput)>, // if editing watch expression

    eval_state: EvalState,

    tree: ValueTree,
    expanded_paths: HashSet<Vec<usize>>,
    cursor_path: Vec<usize>,

    scroll: Scroll,
    lines: Vec<(/*level*/ usize, /*idx*/ usize, /*expanded*/ bool)>, // line index -> tree node
}

impl WatchesWindow {
    fn new(is_locals_window: bool) -> Self { Self {is_locals_window, next_watch_id: 1, seen: (0, usize::MAX, usize::MAX), expressions: Vec::new(), eval_state: EvalState::new(), tree: ValueTree::new(), expanded_paths: HashSet::new(), cursor_path: Vec::new(), scroll: Scroll::new(), lines: Vec::new(), text_input: None} }

    fn clear_tree(&mut self) {
        self.lines.clear();
        self.tree.clear();
        self.eval_state.clear();
    }

    fn eval_locals(&mut self, context: &EvalContext) {
        let (dwarf_context, function) = match self.eval_state.make_local_dwarf_eval_context(context, context.selected_subframe) {
            Ok(x) => x,
            Err(e) => {
                self.tree.levels[1].push(ValueTreeNode {value: Err(e), ..Default::default()});
                return;
            }
        };
        let symbols = dwarf_context.symbols.unwrap();
        let subframe = &context.stack.subframes[context.selected_subframe];
        let pseudo_addr = context.stack.frames[subframe.frame_idx].pseudo_addr;
        let static_pseudo_addr = dwarf_context.addr_map.dynamic_to_static(pseudo_addr);

        let mut idxs_per_name: HashMap<&str, usize> = HashMap::new();
        let subfunction = &subframe.subfunction.as_ref().unwrap().0;
        for v in symbols.local_variables_in_subfunction(subfunction, function.shard_idx()) {
            if !v.range().contains(&(static_pseudo_addr)) {
                continue;
            }
            let (value, dubious) = match eval_dwarf_expression(v.expr, &dwarf_context) {
                Ok((val, dub)) => (Ok(val), dub),
                Err(e) => (Err(e), false),
            };
            let name = unsafe {v.name()};
            let idx_per_name = *idxs_per_name.entry(name).and_modify(|x| *x += 1).or_insert(1) - 1;
            self.tree.levels[1].push(ValueTreeNode {name, value: value.map(|val| Value {val, type_: v.type_, flags: ValueFlags::empty()}), dubious, child_id: hash(&(name, idx_per_name)), ..Default::default()});
        }
    }

    fn eval_watches(&mut self, context: &EvalContext) {
        for (child_id, expr) in &self.expressions {
            let (value, dubious) = if expr.is_empty() {
                (err!(ValueTreePlaceholder, ""), false)
            } else {
                match eval_watch_expression(expr, &mut self.eval_state, context) {
                    Ok((val, dub)) => (Ok(val), dub),
                    Err(e) => (Err(e), false),
                }
            };
            self.tree.levels[1].push(ValueTreeNode {name: self.tree.arena.add_str(expr), value, dubious, child_id: *child_id, ..Default::default()});
        }
    }

    fn populate_lines(&mut self, context: Option<&EvalContext>, palette: &Palette) {
        assert_eq!(self.tree.levels[0].len(), 1);
        self.lines.clear();
        let mut idxs: Vec<usize> = vec![0, 0];
        let mut path: Vec<usize> = vec![0];
        let mut cursor_match = 1usize;
        let mut max_cursor_match = 0usize;
        self.scroll.cursor = 0;
        // DFS. `idxs` is the path (idx of node at each level in `levels`) of the next node to visit. Node idxs.last() is not added to `path` yet, and may be above the end of the parent's list of children.
        while idxs.len() > 1 {
            let (lvl, &idx) = (idxs.len()-1, idxs.last().unwrap());

            {
                let parent = &self.tree.levels[lvl-1][idxs[idxs.len()-2]];
                let children_end = parent.children.as_ref().map_or(0, |r| r.end);
                if idx >= children_end {
                    path.pop();
                    idxs.pop();
                    *idxs.last_mut().unwrap() += 1;
                    continue;
                }
            }

            let node = &self.tree.levels[lvl][idx];
            cursor_match = cursor_match.min(lvl);
            if cursor_match == lvl && self.cursor_path.get(lvl) == Some(&node.child_id) {
                cursor_match += 1;
                if cursor_match > max_cursor_match {
                    max_cursor_match = cursor_match;
                    self.scroll.cursor = self.lines.len();
                }
            }

            path.push(node.child_id);
            let expanded = lvl > 0 && self.expanded_paths.contains(&path);

            self.lines.push((lvl, idx, expanded));

            if !expanded {
                // Skip children.
                idxs.push(usize::MAX);
                continue;
            }

            let node = &self.tree.levels[lvl][idx];
            if node.children.is_none() && node.value.is_ok() && context.is_some() {
                // List children.
                assert!(node.formatted_line[1].is_none());
                let (has_children, children) = format_value(node.value.as_ref().unwrap(), true, &mut self.eval_state, context.as_ref().unwrap(), &mut self.tree.arena, &mut self.tree.text, palette);
                if lvl+1 >= self.tree.levels.len() {
                    self.tree.levels.push(Vec::new());
                }
                let start_idx = self.tree.levels[lvl+1].len();
                let node = &mut self.tree.levels[lvl][idx];
                node.has_children = Some(has_children);
                node.formatted_line[1] = Some(self.tree.text.num_lines());
                self.tree.text.close_line();
                node.children = Some(start_idx..start_idx+children.len());
                let dubious = node.dubious;
                for (name, child_id, value) in children {
                    self.tree.levels[lvl+1].push(ValueTreeNode {name, value, dubious, child_id, parent: idx, ..Default::default()});
                }
            }

            // Go to the first child.
            idxs.push(self.tree.levels[lvl][idx].children.as_ref().map_or(usize::MAX, |c| c.start));
        }
    }

    fn node_path(&self, mut lvl: usize, mut idx: usize) -> Vec<usize> {
        let mut path: Vec<usize> = Vec::new();
        while lvl > 0 {
            let node = &self.tree.levels[lvl][idx];
            path.push(node.child_id);
            idx = node.parent;
            lvl -= 1;
        }
        path.push(0);
        path.reverse();
        path
    }

    fn populate_tree_with_placeholders(&mut self) {
        if self.is_locals_window {
            self.tree.levels[1].push(ValueTreeNode {value: err!(ValueTreePlaceholder, ""), ..Default::default()});
        } else {
            for (child_id, expr) in &self.expressions {
                self.tree.levels[1].push(ValueTreeNode {name: self.tree.arena.add_str(expr), child_id: *child_id, value: err!(ValueTreePlaceholder, ""), ..Default::default()});
            }
        }
    }

    fn delete_watch(&mut self, child_id: usize) {
        assert!(!self.is_locals_window);
        self.expressions.retain(|(id, _)| *id != child_id);
        if self.cursor_path.get(1) == Some(&child_id) && self.tree.levels.len() > 1 {
            if let Some(i) = self.tree.levels[1].iter().position(|n| n.child_id == child_id) {
                if i + 1 < self.tree.levels[1].len() {
                    self.cursor_path = vec![0, self.tree.levels[1][i+1].child_id];
                } else if i > 0 {
                    self.cursor_path = vec![0, self.tree.levels[1][i-1].child_id];
                }
            }
        }
        // Clear the tree to trigger recalculating it.
        // This is not ideal when editing/deleting watches while running because remembered values and subtrees turn into "<running>" placeholder.
        // But fixing it doesn't seem the extra code and risk of bugs (we'd have two code paths for editing the tree, one of them used infrequently).
        self.clear_tree();
    }
}

impl WindowContent for WatchesWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        if let Some((_, area, _)) = &mut self.text_input {
            *area = Rect::default();
        }

        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let mut refresh_lines = false;

        keys.retain(|key| {
            if self.text_input.is_some() {
                return false;
            }
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::CursorRight) => match self.lines.get(self.scroll.cursor) {
                    Some(&(lvl, idx, _)) if self.tree.levels[lvl][idx].has_children == Some(true) => {
                        self.expanded_paths.insert(self.node_path(lvl, idx));
                        refresh_lines = true;
                    }
                    _ => (),
                }
                Some(KeyAction::CursorLeft) => match self.lines.get(self.scroll.cursor) {
                    Some(&(lvl, idx, expanded)) => {
                        let mut path = self.node_path(lvl, idx);
                        if !expanded {
                            path.pop();
                        }
                        self.expanded_paths.remove(&path);
                        refresh_lines = true;
                    }
                    None => (),
                }
                Some(KeyAction::Enter) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(lvl, idx, _)) if lvl == 1 => {
                        let node = &self.tree.levels[lvl][idx];
                        if let Some((_, text)) = self.expressions.iter().find(|(id, _)| id == &node.child_id) {
                            self.text_input = Some((node.child_id, Rect::default(), TextInput::with_text(text.clone())));
                        }
                    }
                    _ => (),
                }
                Some(KeyAction::DeleteRow) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(lvl, idx, _)) if lvl == 1 => self.delete_watch(self.tree.levels[lvl][idx].child_id),
                    _ => (),
                }
                Some(KeyAction::DuplicateRow) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(lvl, idx, _)) if lvl == 1 => {
                        let node = &self.tree.levels[lvl][idx];
                        if let Some(i) = self.expressions.iter().position(|(id, _)| id == &node.child_id) {
                            let id = self.next_watch_id;
                            let s = self.expressions[i].1.clone();
                            self.next_watch_id += 1;
                            self.expressions.insert(i + 1, (id, s));
                            self.cursor_path = vec![0, id];
                            self.clear_tree();
                        }
                    }
                    _ => (),
                }
                _ => return true,
            }
            false
        });

        let mut unavailable_reason: Option<String> = None;
        let mut refresh_values = false;
        let mut context = None;
        match debugger.threads.get(&ui.selected_thread) {
            None => unavailable_reason = Some("<no process>".to_string()),
            Some(thread) => if thread.state != ThreadState::Suspended || ui.stack.frames.is_empty() {
                unavailable_reason = Some("<running>".to_string());
            } else {
                let t = (ui.selected_thread, ui.selected_subframe, thread.stop_count);
                if t != self.seen {
                    if self.is_locals_window && (t.0, t.1) != (self.seen.0, self.seen.1) {
                        //self.expanded_paths.clear();
                    }
                    self.seen = t;
                    refresh_values = true;
                }
                context = Some(debugger.make_eval_context(&ui.stack, ui.selected_subframe));
            }
        }

        if !self.expressions.last().is_some_and(|(_, s)| s.is_empty()) {
            // Add the "<add watch>" placeholder watch.
            self.expressions.push((self.next_watch_id, String::new()));
            self.cursor_path = vec![0, self.next_watch_id];
            self.next_watch_id += 1;
            self.clear_tree();
        }

        if context.is_some() && (refresh_values || self.tree.levels[0].is_empty()) {
            let context = context.as_ref().unwrap();
            self.clear_tree();
            self.eval_state.update(context);
            if self.is_locals_window {
                self.eval_locals(context);
            } else {
                self.eval_watches(context);
            }
        } else if self.tree.levels[0].is_empty() {
            self.populate_tree_with_placeholders();
            self.seen.2 = usize::MAX;
        }

        if self.tree.levels[0].is_empty() {
            let count = self.tree.levels[1].len();
            self.tree.levels[0].push(ValueTreeNode {children: Some(0..count), ..Default::default()});
            refresh_lines = true;
        }
        if refresh_lines {
            self.populate_lines(context.as_ref(), palette);
        }

        let mut no_keys = Vec::new();
        let range = self.scroll.update(self.lines.len(), area.height.saturating_sub(1), if self.text_input.is_none() {&mut keys} else {&mut no_keys}, &debugger.context.settings.keys);
        self.cursor_path = match self.lines.get(self.scroll.cursor) {
            None => Vec::new(),
            Some(&(lvl, idx, _)) => self.node_path(lvl, idx),
        };

        let name_column_width = area.width / 3;
        let mut rows: Vec<Row> = Vec::new();
        for line_idx in range.clone() {
            let &(lvl, idx, expanded) = &self.lines[line_idx];
            assert!(lvl > 0);
            let node = &mut self.tree.levels[lvl][idx];
            let expanded = expanded && node.has_children == Some(true);
            if node.formatted_line[expanded as usize].is_none() && context.is_some() && node.value.is_ok() {
                let (has_children, _) = format_value(node.value.as_ref().unwrap(), expanded, &mut self.eval_state, context.as_ref().unwrap(), &mut self.tree.arena, &mut self.tree.text, palette);
                node.has_children = Some(has_children);
                node.formatted_line[expanded as usize] = Some(self.tree.text.num_lines());
                self.tree.text.close_line();
            }
            let node = &self.tree.levels[lvl][idx];
            let parent = &self.tree.levels[lvl-1][node.parent];
            // The "<add watch>" line at the end.
            let is_add_watch_placeholder = !self.is_locals_window && lvl == 1 && self.expressions.last().is_some_and(|(id, s)| s.is_empty() && *id == node.child_id);

            // Indentation, expansion arrow, name.
            let mut spans = vec![Span::styled("⸽".to_string().repeat(lvl - 1), palette.default_dim)];
            if node.has_children.unwrap_or(expanded) {
                spans.push(Span::raw(if expanded {"▾ "} else {"▸ "}));
            } else {
                spans.push(Span::raw("  "));
            }
            spans.push(Span::raw(if is_add_watch_placeholder {"<add watch>".to_string()} else {node.name.to_string()}));
            // Dim the name if parent's value is dubious or the existence of this item comes from cache.
            if parent.dubious || (context.is_none() && (lvl > 1 || self.is_locals_window)) || is_add_watch_placeholder {
                for span in &mut spans {
                    span.style = palette.default_dim;
                }
            }

            let mut cells = vec![Cell::from(Spans::from(spans))];

            let mut spans: Vec<Span> = Vec::new();
            if let &Some(line_idx) = &node.formatted_line[expanded as usize] {
                self.tree.text.line_out_copy(line_idx, &mut spans);
            } else if node.value.as_ref().is_err_and(|e| !e.is_value_tree_placeholder()) {
                spans.push(Span::styled(format!("<{}>", node.value.as_ref().err().unwrap()), palette.error));
            } else if !is_add_watch_placeholder {
                spans.push(Span::raw(unavailable_reason.as_ref().unwrap().clone()));
            }
            if node.dubious || context.is_none() {
                for span in &mut spans {
                    span.style = span.style.add_modifier(Modifier::DIM);
                }
            }
            cells.push(Cell::from(Spans::from(spans)));
            let mut row = Row::new(cells);

            // Highlight selected line.
            if line_idx == self.scroll.cursor {
                row = row.style(palette.selected_text_line);
            }

            if let Some((id, a, _)) = &mut self.text_input {
                if lvl == 1 && *id == node.child_id {
                    *a = Rect {x: area.x + 2, y: area.y + 1 + (line_idx - range.start) as u16, width: name_column_width.saturating_sub(1), height: 1};
                }
            }

            rows.push(row);
        }

        let column_widths = [Constraint::Length(name_column_width), Constraint::Length(area.width - name_column_width)];
        let table = Table::new(rows)
            .header(Row::new(vec!["  name", "value"]).style(palette.table_header))
            .widths(&column_widths);

        f.render_widget(table, area);
    }

    fn drop_caches(&mut self) {
        self.clear_tree();
        self.seen.2 = usize::MAX;
    }

    fn update_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
        if let Some((child_id, _, input)) = &mut self.text_input {
            let event = input.update(keys, &debugger.context.settings.keys);
            match event {
                TextInputEvent::None | TextInputEvent::Open => (),
                TextInputEvent::Cancel => self.text_input = None,
                TextInputEvent::Done => {
                    if let Some(i) = self.expressions.iter().position(|(id, _)| id == child_id) {
                        if input.text.is_empty() {
                            if i + 1 != self.expressions.len() {
                                let child_id = *child_id;
                                self.delete_watch(child_id);
                            }
                        } else {
                            self.expressions[i].1 = input.text.clone();
                            if i + 1 < self.expressions.len() {
                                self.cursor_path = vec![0, self.expressions[i+1].0];
                            }
                            self.clear_tree();
                        }
                    }
                    self.text_input = None;
                }
            }
        }
    }

    fn render_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {
        if let Some((_, area, input)) = &mut self.text_input {
            f.render_widget(Clear, *area);
            input.render(f, *area, &debugger.context.settings.palette);
        }
    }

    fn cancel_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger) {
        self.text_input = None;
    }

    fn get_hints(&self, hints: &mut StyledText) {
        styled_write!(hints, Style::default(), "ret - edit"); hints.close_line();
        styled_write!(hints, Style::default(), "d - duplicate"); hints.close_line();
        styled_write!(hints, Style::default(), "backspace - delete"); hints.close_line();
    }

    fn has_persistent_state(&self) -> bool {
        !self.is_locals_window
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        out.write_usize(self.expressions.len())?;
        for (_id, expr) in &self.expressions {
            out.write_str(&expr)?;
        }
        Ok(())
    }
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        for i in 0..inp.read_usize()? {
            let expr = inp.read_str()?;
            let id = self.next_watch_id;
            self.next_watch_id += 1;
            self.expressions.push((id, expr));
        }
        Ok(())
    }
}

struct LocationsWindow {
    scroll: Scroll,
}

impl Default for LocationsWindow { fn default() -> Self { Self {scroll: Scroll::new()} } }

impl WindowContent for LocationsWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, mut area: Rect) {
        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        if area.height > 0 {
            let mut a = area;
            a.height = 1;
            let paragraph = Paragraph::new(Text {lines: vec![Spans::from(vec![Span::styled("(for address selected in disassembly window)", palette.default_dim)])]});
            f.render_widget(paragraph, a);
            area.y += a.height;
            area.height -= a.height;
        }

        let rows = match self.fill_table(ui, debugger) {
            Ok(rows) => {
                let range = self.scroll.update_cursorless(rows.len(), area.height.saturating_sub(1), &mut keys, &debugger.context.settings.keys);
                rows[range].to_vec()
            }
            Err(e) => vec![Row::new(vec![Cell::from(""), Cell::from(""), Cell::from(format!("{}", e)).style(palette.error), Cell::from("")])],
        };
        let table = Table::new(rows)
            .header(Row::new(vec!["name", "type", "expression", "die"]).style(palette.table_header))
            .widths(&[Constraint::Percentage(25), Constraint::Percentage(30), Constraint::Percentage(30), Constraint::Length(9)]);

        f.render_widget(table, area);
    }
}

impl LocationsWindow {
    fn fill_table(&mut self, ui: &mut UIState, debugger: &Debugger) -> Result<Vec<Row<'static>>> {
        let palette = &debugger.context.settings.palette;
        let (binary_id, function_idx, addr) = match ui.selected_addr.clone() { Some(x) => x, None => return Ok(Vec::new()) };
        let binary = match debugger.info.binaries.get(&binary_id) {
            Some(x) => x,
            None => return err!(ProcessState, "binary not mapped"),
        };
        let static_addr = binary.addr_map.dynamic_to_static(addr);
        let symbols = binary.symbols.as_ref_clone_error()?;
        let function = &symbols.functions[function_idx];
        let encoding = match function.debug_info_offset() {
            Some(off) => symbols.find_unit(off).map(|u| u.unit.header.encoding()),
            None => err!(Internal, "internal error: function has no debug info") /* should be no local variables in this case */ };

        let mut rows: Vec<Row> = Vec::new();
        for level in 0..function.num_levels() {
            let ranges = symbols.subfunction_ranges_at_level(level, function);
            for (i, range) in ranges.iter().enumerate() {
                if range.prev_range_idx < i {
                    continue;
                }
                let sf = &symbols.shards[function.shard_idx()].subfunctions[range.subfunction_idx];
                for v in symbols.local_variables_in_subfunction(sf, function.shard_idx()) {
                    if !v.range().contains(&static_addr) {
                        continue;
                    }
                    let expr_str = match &encoding {
                        Ok(e) => format_dwarf_expression(v.expr, *e),
                        Err(e) => Err(e.clone()) };
                    let mut text = StyledText::new();
                    print_type_name(v.type_, &mut text, palette, 0);
                    rows.push(Row::new(vec![
                        Cell::from(format!("{}", unsafe {v.name()})),
                        Cell::from(text.into_line()),
                        match expr_str {
                            Ok(s) => Cell::from(s),
                            Err(e) => Cell::from(format!("{}", e)).style(palette.error),
                        },
                        Cell::from(format!("{:x}", v.offset().0)),
                    ]));
                }
            }
        }
        Ok(rows)
    }
}

struct DisassemblyFunctionLocator {
    binary_id: BinaryId,
    mangled_name: Vec<u8>,
    demangled_name: String,
    addr: FunctionAddr,
}

enum DisassemblyTabStatus {
    Function {locator: DisassemblyFunctionLocator, function_idx: usize},
    // Tab info was loaded from save file, and we didn't look up the corresponding function in symbols yet.
    Unresolved {locator: DisassemblyFunctionLocator, cached_error: Option<Error>},
    // Couldn't find function for current address.
    Err(Error),
}
impl DisassemblyTabStatus {
    fn is_err(&self) -> bool { match self { Self::Err(_) => true, _ => false } }
}

struct DisassemblyTab {
    title: String,
    status: DisassemblyTabStatus,

    scroll: Scroll,
    selected_subfunction_level: u16,
    hscroll: u16,
    pinned: bool,
}
impl Default for DisassemblyTab {
    fn default() -> Self { Self {status: DisassemblyTabStatus::Err(error!(Internal, "uninitialized")), title: String::new(), scroll: Scroll::new(), selected_subfunction_level: u16::MAX, hscroll: 0, pinned: false} }
}

struct DisassemblyWindow {
    tabs: Vec<DisassemblyTab>,
    cache: HashMap<(BinaryId, usize), Disassembly>,
    selected_tab: usize,
    search_dialog: Option<SearchDialog>,
    source_scrolled_to: Option<(BinaryId, /*function_idx*/ usize, /*disas_line*/ usize, /*selected_subfunction_level*/ u16)>,
}

impl Default for DisassemblyWindow { fn default() -> Self { Self {tabs: Vec::new(), cache: HashMap::new(), selected_tab: 0, search_dialog: None, source_scrolled_to: None} } }

// (A lot of tab management code is copy-pasted from CodeWindow. Would be nice to deduplicate it, but it's not obvious what's a good way to do it.)
impl DisassemblyWindow {
    fn open_function(&mut self, target: Result<DisassemblyScrollTarget>, debugger: &Debugger) -> Result<()> {
        let target = match target {
            Ok(x) => x,
            Err(e) => {
                self.tabs.push(DisassemblyTab {status: DisassemblyTabStatus::Err(e), title: "[unknown]".to_string(), ..Default::default()});
                self.selected_tab = self.tabs.len() - 1;
                return Ok(());
            }
        };
        let mut found = false;
        for i in 0..self.tabs.len() {
            self.resolve_function_for_tab(i, debugger);
            match &self.tabs[i].status {
                DisassemblyTabStatus::Function {locator, function_idx: f, ..} if locator.binary_id == target.binary_id && *f == target.function_idx => {
                    self.selected_tab = i;
                    found = true;
                    break;
                }
                _ => (),
            }
        }
        if !found {
            let binary = match debugger.info.binaries.get(&target.binary_id) {
                Some(x) => x,
                None => return err!(ProcessState, "binary not mapped"),
            };
            let symbols = binary.symbols.as_ref_clone_error()?;
            let function = &symbols.functions[target.function_idx];
            let demangled_name = function.demangle_name();
            let title = Self::make_title(&demangled_name);
            self.tabs.push(DisassemblyTab {status: DisassemblyTabStatus::Function {locator: DisassemblyFunctionLocator {binary_id: target.binary_id, mangled_name: function.mangled_name().to_owned(), addr: function.addr, demangled_name}, function_idx: target.function_idx}, title, ..Default::default()});
            self.selected_tab = self.tabs.len() - 1;
        }
        Ok(())
    }

    fn make_title(demangled_name: &str) -> String {
        let limit = 30;
        if demangled_name.len() <= limit {
            return demangled_name.to_string();
        }
        // Some crude approximation of removing namespaces and parent classes.
        let mut res = demangled_name;
        let first_template = demangled_name.char_indices().find(|(_, c)| *c == '<');
        if let Some((last_ns, _)) = demangled_name.char_indices().rev().find(|(_, c)| *c == ':') {
            let last_template = demangled_name.char_indices().rev().find(|(_, c)| *c == '>');
            if !last_template.is_some_and(|(i, _)| i > last_ns) && last_ns + 1 < demangled_name.len() {
                // E.g. Foo<Bar>::doit  ->  doit
                res = &demangled_name[last_ns+1..];
            } else if let Some((first_template, _)) = first_template {
                if let Some((last_ns_before_templates, _)) = demangled_name[..first_template].char_indices().rev().skip(1).find(|(_, c)| *c == ':') {
                    // E.g. std::__1::nonsense::more_nonsense::bs<std::bs_traits<int> >::frob<std::bs_allocator>  ->  bs<std::bs_traits<int> >::frob<std::bs_allocator>
                    res = &demangled_name[last_ns_before_templates+1..];
                }
            }
        } else if let Some((first_template, _)) = first_template {
            // E.g. foo<Bar>  ->  foo
            if first_template > 0 {
                res = &demangled_name[..first_template];
            }
        }
        let mut i = res.len().min(30);
        while !res.is_char_boundary(i) {
            i += 1;
        }
        let mut r = res[..i].to_string();
        if i < res.len() {
            r += "…";
        }
        r
    }

    fn resolve_function_for_tab(&mut self, tab_idx: usize, debugger: &Debugger) {
        match &mut self.tabs[tab_idx].status {
            DisassemblyTabStatus::Unresolved {locator, cached_error} if cached_error.is_none() => {
                let (binary_id, binary) = match debugger.info.binaries.iter().find(|(id, _)| id.matches_incomplete(&locator.binary_id)) {
                    Some(x) => x,
                    None => {*cached_error = Some(error!(NoFunction, "binary not mapped: {}", locator.binary_id.path)); return;}
                };
                let symbols = match binary.symbols.as_ref() {
                    Ok(x) => x,
                    Err(e) => {*cached_error = Some(e.clone()); return;}
                };
                let function_idx = match symbols.find_nearest_function(&locator.mangled_name, locator.addr) {
                    Some(x) => x,
                    None => {*cached_error = Some(error!(NoFunction, "function not found: {}", locator.demangled_name)); return;}
                };
                let function = &symbols.functions[function_idx];
                let locator = DisassemblyFunctionLocator {binary_id: binary_id.clone(), mangled_name: function.mangled_name().to_vec(), demangled_name: function.demangle_name(), addr: function.addr};
                self.tabs[tab_idx].status = DisassemblyTabStatus::Function {locator, function_idx};
            }
            _ => (),
        }
    }

    fn find_or_disassemble_function<'a>(cache: &'a mut HashMap<(BinaryId, usize), Disassembly>, binary_id: BinaryId, function_idx: usize, debugger: &Debugger) -> &'a Disassembly {
        cache.entry((binary_id.clone(), function_idx)).or_insert_with(|| {
            // Would be nice to also support disassembling arbitrary memory, regardless of functions or binaries. E.g. for JIT-generated code.
            match Self::disassemble_function(&binary_id, function_idx, debugger) {
                Ok(d) => d,
                Err(e) => Disassembly::new().with_error(e, &debugger.context.settings.palette),
            }
        })
    }

    // Would be nice to also support disassembling arbitrary memory, regardless of functions or binaries. E.g. for JIT-generated code.
    fn disassemble_function(binary_id: &BinaryId, function_idx: usize, debugger: &Debugger) -> Result<Disassembly> {
        let palette = &debugger.context.settings.palette;
        let binary = match debugger.info.binaries.get(binary_id) {
            Some(x) => x,
            None => return err!(ProcessState, "binary not mapped"),
        };
        let symbols = binary.symbols.clone()?;
        let ranges = symbols.function_addr_ranges(function_idx);
        let function = &symbols.functions[function_idx];

        let mut prelude = StyledText::new();
        styled_write!(prelude, palette.default_dim, "{}", binary_id.path);
        prelude.close_line();
        match function.debug_info_offset() {
            Some(off) => styled_write!(prelude, palette.default_dim, "dwarf offset: 0x{:x}", off.0),
            None => styled_write!(prelude, palette.default_dim, "function from symtab"),
        }
        prelude.close_line();
        prelude.close_line();
        // TODO: Print declaration site. Scroll code window to it when selected.
        // TODO: Print number of inlined call sites. Allow setting breakpoint on it.

        Ok(disassemble_function(function_idx, ranges, Some(symbols.as_ref()), &binary.addr_map, &debugger.memory, prelude, &debugger.context.settings.palette))
    }

    fn close_error_tab(&mut self) -> Option<usize> {
        if self.tabs.is_empty() {
            return None;
        }
        let i = match self.tabs.iter().position(|t| match &t.status {DisassemblyTabStatus::Err(_) => true, _ => false}) {
            None => return Some(self.selected_tab),
            Some(x) => x,
        };
        self.tabs.remove(i);
        if self.selected_tab == i {
            self.selected_tab = 0;
            return None;
        }
        if self.selected_tab > i {
            self.selected_tab -= 1;
        }
        Some(self.selected_tab)
    }

    fn garbage_collect(&mut self) { // (copied from CodeWindow)
        let unpinned_tabs = self.tabs.iter().filter(|t| !t.pinned).count();
        if unpinned_tabs > 1 {
            let mut j = 0usize;
            for i in 0..self.tabs.len() {
                if self.tabs[i].pinned || j == self.selected_tab {
                    self.tabs.swap(i, j);
                    j += 1;
                } else if j < self.selected_tab {
                    self.selected_tab -= 1;
                }
            }
            self.tabs.truncate(j);
        }

        if self.cache.len().saturating_sub(self.tabs.len()) > 100 {
            let mut in_use: HashSet<(BinaryId, usize)> = HashSet::new();
            for t in &self.tabs {
                match &t.status {
                    DisassemblyTabStatus::Function {locator, function_idx, ..} => {in_use.insert((locator.binary_id.clone(), *function_idx));}
                    _ => (),
                }
            }
            let mut i = 0;
            self.cache.retain(
                |k, _|
                if in_use.contains(k) {
                    true
                } else {
                    i += 1;
                    i % 2 == 0
                }
            );
        }
    }

    fn switch_tab(&mut self, delta: isize) {
        self.selected_tab = (self.selected_tab as isize + delta).rem_euclid(self.tabs.len().max(1) as isize) as usize;
    }
}

impl WindowContent for DisassemblyWindow {
    fn update_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
        if let Some(d) = &mut self.search_dialog {
            let event = d.update(keys, &debugger.context.settings.keys, &debugger.symbols, Some(debugger.info.binaries.keys().cloned().collect()));
            let mut function_to_open = None;
            match event {
                SearchDialogEvent::None => (),
                SearchDialogEvent::Cancel => self.search_dialog = None,
                SearchDialogEvent::Open(res) => function_to_open = Some(res),
                SearchDialogEvent::Done(res) => {
                    function_to_open = Some(res);
                    self.search_dialog = None;
                }
            }
            if let Some(res) = function_to_open {
                match self.open_function(Ok(DisassemblyScrollTarget {binary_id: res.binary, function_idx: res.id, addr: 0, subfunction_level: u16::MAX}), debugger) {
                    Ok(()) => self.tabs[self.selected_tab].pinned = true,
                    Err(e) => log!(debugger.log, "{}", e),
                }
            }
            if self.search_dialog.is_some() {
                return;
            }
        }

        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::NextTab) => self.switch_tab(1),
                Some(KeyAction::PreviousTab) => self.switch_tab(-1),
                Some(KeyAction::PinTab) => if !self.tabs.is_empty() {
                    let t = &mut self.tabs[self.selected_tab];
                    if !t.status.is_err() || t.pinned {
                        t.pinned ^= true;
                    }
                }
                _ => return true,
            }
            false
        });
    }

    fn render_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {
        if let Some(d) = &mut self.search_dialog {
            ui.loading |= d.render(f, screen_area, "find function (by mangled name)", &debugger.context.settings.palette);
        }
    }

    fn cancel_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger) {
        self.search_dialog = None;
    }

    fn get_hints(&self, hints: &mut StyledText) {
        hints.chars.push_str("o - find function"); hints.close_span(Style::default()); hints.close_line();
        hints.chars.push_str("C-y - pin/unpin tab"); hints.close_span(Style::default()); hints.close_line();
        hints.chars.push_str(",/. - select level"); hints.close_span(Style::default()); hints.close_line();
        // TODO: hints.chars.push_str("b - toggle breakpoint"); hints.close_span(Style::default()); hints.close_line();
    }

    fn has_persistent_state(&self) -> bool {
        true
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        for (idx, tab) in self.tabs.iter().enumerate() {
            let locator = match &tab.status {
                DisassemblyTabStatus::Function {locator, ..} => locator,
                DisassemblyTabStatus::Unresolved {locator, ..} => locator,
                DisassemblyTabStatus::Err(_) => continue,
            };
            out.write_u8(if idx == self.selected_tab {2} else {1})?;
            locator.binary_id.save_state_incomplete(out)?;
            out.write_slice(&locator.mangled_name)?;
            out.write_str(&locator.demangled_name)?; // can't just demangle on load because we don't know the language (alernatively we could save the language here)
            out.write_usize(locator.addr.0)?;
            tab.scroll.save_state(out)?;
            out.write_u16(tab.selected_subfunction_level)?;
            out.write_u16(tab.hscroll)?;
            out.write_u8(tab.pinned as u8)?;
        }
        out.write_u8(0)?;
        Ok(())
    }
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        loop {
            let select_this_tab = match inp.read_u8()? {
                0 => break,
                2 => true,
                _ => false,
            };
            let locator = DisassemblyFunctionLocator {binary_id: BinaryId::load_state_incomplete(inp)?, mangled_name: inp.read_slice()?, demangled_name: inp.read_str()?, addr: FunctionAddr(inp.read_usize()?)};
            let title = Self::make_title(&locator.demangled_name);
            self.tabs.push(DisassemblyTab {title, status: DisassemblyTabStatus::Unresolved {locator, cached_error: None}, scroll: Scroll::load_state(inp)?, selected_subfunction_level: inp.read_u16()?, hscroll: inp.read_u16()?, pinned: inp.read_u8()? != 0});
            if select_this_tab {
                self.selected_tab = self.tabs.len() - 1;
            }
        }
        // TODO: Prevent scrolling code window before first input.
        Ok(())
    }

    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, mut area: Rect) {
        ui.selected_addr = ui.stack.frames.get(ui.selected_frame).and_then(|f| {
            let sf = &ui.stack.subframes[f.subframes.end-1];
            match (&f.binary_id, &sf.function) {
                (Some(b), Ok((_, function_idx))) => Some((b.clone(), *function_idx, f.pseudo_addr)),
                _ => None,
            }
        });

        let f = match f {
            Some(f) => f,
            None => return,
        };

        let mut scroll_to_addr: Option<(usize, u16)> = None;
        let suppress_code_autoscroll = ui.should_scroll_disassembly.is_some();
        if let Some((target, only_if_on_error_tab)) = mem::take(&mut ui.should_scroll_disassembly) {
            let mut tab_to_restore: Option<usize> = None;
            if only_if_on_error_tab {
                tab_to_restore = self.close_error_tab();
            }

            if let Ok(target) = &target {
                if tab_to_restore.is_none() {
                    scroll_to_addr = Some((target.addr, target.subfunction_level));
                } else {
                    // (Not ideal that we don't scroll when the tab is not selected, but meh.)
                }
            }

            self.open_function(target, debugger).unwrap();

            if let Some(i) = tab_to_restore {
                self.selected_tab = i;
            }
        }

        if self.search_dialog.is_some() {
            keys.clear();
        }

        self.garbage_collect();

        if self.tabs.is_empty() {
            return;
        }

        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::Open) => {
                    self.search_dialog = Some(SearchDialog::new(Arc::new(FunctionSearcher), debugger.context.clone()));
                    self.update_modal(ui, debugger, &mut Vec::new()); // kick off initial search with empty query
                }
                _ => return true
            }
            false
        });

        let palette = &debugger.context.settings.palette;
        let mut header = StyledText::new();
        let mut disas: Option<(BinaryId, usize, &Disassembly)> = None;
        self.resolve_function_for_tab(self.selected_tab, debugger);
        let indent = "";
        styled_write!(header, Style::default(), "{}", indent);
        match &self.tabs[self.selected_tab].status {
            DisassemblyTabStatus::Err(e) => {
                styled_write!(header, palette.error, "{}", e);
            }
            DisassemblyTabStatus::Unresolved {locator, cached_error} => {
                styled_write!(header, palette.default_dim, "{}", locator.demangled_name);
                header.close_line();
                styled_write!(header, Style::default(), "{}", indent);
                styled_write!(header, palette.error, "couldn't find function: {}", cached_error.as_ref().unwrap());
            }
            DisassemblyTabStatus::Function {locator, function_idx} => {
                styled_write!(header, palette.function_name, "{}", locator.demangled_name);
                disas = Some((locator.binary_id.clone(), *function_idx, Self::find_or_disassemble_function(&mut self.cache, locator.binary_id.clone(), *function_idx, debugger)));
            }
        }
        header.close_line();

        let width = area.width.saturating_sub(2);
        let widest_line = header.widest_line().max(disas.as_ref().map_or(0, |(_, _, d)| d.widest_line));
        let tab = &mut self.tabs[self.selected_tab];
        let mut toggle_breakpoint = false;
        let mut subfunction_level_delta = 0isize;
        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::CursorRight) => tab.hscroll = tab.hscroll.saturating_add(width),
                Some(KeyAction::CursorLeft) => tab.hscroll = tab.hscroll.saturating_sub(width),
                Some(KeyAction::ToggleBreakpoint) => toggle_breakpoint ^= true,
                Some(KeyAction::PreviousMatch) => subfunction_level_delta -= 1,
                Some(KeyAction::NextMatch) => subfunction_level_delta += 1,
                _ => return true
            }
            false
        });
        tab.hscroll = tab.hscroll.min(widest_line.saturating_sub(width as usize).try_into().unwrap_or(u16::MAX));

        if area.height > 0 {
            let mut a = area;
            a.height = 1;
            let tabs = Tabs::new(self.tabs.iter().map(
                |t| Spans::from(vec![
                    if t.pinned {
                        Span::styled("📌 ".to_string() + &t.title, palette.tab_title_active)
                    } else {
                        Span::styled(t.title.clone(), palette.tab_title)
                    }])).collect())
                .select(self.selected_tab).highlight_style(palette.tab_title_selected);
            f.render_widget(tabs, a);

            area.y += 1;
            area.height -= 1;
        }
        let tab = &mut self.tabs[self.selected_tab];

        {
            let mut a = area;
            a.height = a.height.min(header.num_lines() as u16);
            let paragraph = Paragraph::new(Text {lines: header.to_lines()}).scroll((0, tab.hscroll));
            f.render_widget(paragraph, a);

            area.y += a.height;
            area.height -= a.height;
        }

        if let Some((binary_id, function_idx, disas)) = disas {
            let mut lines: Vec<Spans> = Vec::new();

            if toggle_breakpoint {
                // TODO: disassembly breakpoints
            }
            
            let mut ip_lines: Vec<(usize, /*selected*/ bool)> = Vec::new();
            for (idx, frame) in ui.stack.frames.iter().enumerate() {
                if let Some(line) = disas.pseudo_addr_to_line(frame.pseudo_addr) {
                    ip_lines.push((line, idx == ui.selected_frame));
                }
            }
            ip_lines.sort_unstable_by_key(|k| (k.0, !k.1));

            let range = if let Some((addr, subfunction_level)) = scroll_to_addr {
                tab.hscroll = 0;
                tab.selected_subfunction_level = subfunction_level;
                tab.scroll.set(disas.text.num_lines(), area.height, disas.pseudo_addr_to_line(addr).unwrap_or(0))
            } else {
                tab.scroll.update(disas.text.num_lines(), area.height, &mut keys, &debugger.context.settings.keys)
            };

            let mut selected_subfunction_idx: Option<usize> = None;
            let mut source_line: Option<SourceScrollTarget> = None;
            let mut symbols_shard: Option<&SymbolsShard> = None;

            if let Some(disas_line) = disas.lines.get(tab.scroll.cursor) {
                if disas_line.addr != 0 && disas_line.addr != usize::MAX {
                    ui.selected_addr = Some((binary_id.clone(), function_idx, disas_line.addr));
                }

                if let Some(binary) = debugger.info.binaries.get(&binary_id) {
                    if let Ok(symbols) = &binary.symbols {
                        let function = &symbols.functions[function_idx];
                        let shard = &symbols.shards[function.shard_idx()];
                        symbols_shard = Some(shard);
                        assert!(disas.symbols_shard == Some((symbols as &Symbols as *const Symbols, function.shard_idx())));

                        let max_level = disas_line.subfunction.map_or(0isize, |i| shard.subfunctions[i].level as isize);
                        let x = tab.selected_subfunction_level as isize;
                        if subfunction_level_delta != 0 {
                            tab.selected_subfunction_level = if subfunction_level_delta < 0 {
                                (x.min(max_level + 1) + subfunction_level_delta).max(1) as u16
                            } else if x + subfunction_level_delta > max_level {
                                u16::MAX
                            } else {
                                (x + subfunction_level_delta).min(u16::MAX as isize) as u16
                            };
                        }
                        tab.selected_subfunction_level = tab.selected_subfunction_level.max(1);

                        let mut source_line_info = disas_line.leaf_line.clone();
                        if let &Some(mut sf_idx) = &disas_line.subfunction {
                            while shard.subfunctions[sf_idx].level > tab.selected_subfunction_level {
                                sf_idx = shard.subfunctions[sf_idx].parent;
                            }
                            if shard.subfunctions[sf_idx].level == tab.selected_subfunction_level {
                                selected_subfunction_idx = Some(sf_idx);
                                source_line_info = Some(shard.subfunctions[sf_idx].call_line.clone());
                            }
                        }
                        if let Some(line) = source_line_info {
                            let file = &symbols.files[line.file_idx().unwrap()];
                            source_line = Some(SourceScrollTarget {path: file.path.to_owned(), version: file.version.clone(), line: line.line()});
                        }
                    }
                }
            }

            let key = (binary_id.clone(), function_idx, tab.scroll.cursor, tab.selected_subfunction_level);
            if !suppress_code_autoscroll && self.source_scrolled_to.as_ref() != Some(&key) {
                self.source_scrolled_to = Some(key);
                if let Some(target) = source_line {
                    ui.should_scroll_source = Some((Some(target), false));
                    ui.should_redraw = true;
                }
            }

            for i in range {
                let mut spans: Vec<Span> = Vec::new();
                let line_addr = &disas.lines[i];

                // (Comparing line number instead of address because the "instruction pointer" pseudoaddress may be in between instructions.)
                let ip_idx = ip_lines.partition_point(|x| x.0 < i);
                if ip_idx == ip_lines.len() || ip_lines[ip_idx].0 != i {
                    spans.push(Span::raw("  "));
                } else if ip_lines[ip_idx].1 {
                    spans.push(Span::styled("⮕ ", palette.instruction_pointer));
                } else {
                    spans.push(Span::styled("⮕ ", palette.additional_instruction_pointer));
                }

                let loc_idx = debugger.breakpoint_locations.partition_point(|loc| loc.addr < line_addr.addr);
                let mut marker = "  ";
                if line_addr.kind == DisassemblyLineKind::Instruction && loc_idx < debugger.breakpoint_locations.len() {
                    let loc = &debugger.breakpoint_locations[loc_idx];
                    if loc.addr == line_addr.addr && loc.breakpoints.iter().any(|b| match b { BreakpointRef::Id {..} => true, BreakpointRef::Step(_) => false }) {
                        marker = if loc.active { "● " } else { "○ " }
                    }
                }
                spans.push(Span::styled(marker, palette.secondary_breakpoint));

                let indent_span_idx = spans.len() + line_addr.indent_span_idx;
                disas.text.line_out(i, &mut spans);

                if let (&Some(selected_subfunction_idx), &Some(mut cur_subfunction_idx)) = (&selected_subfunction_idx, &line_addr.subfunction) {
                    let shard = symbols_shard.clone().unwrap();
                    let levels = shard.subfunctions[cur_subfunction_idx].level as usize;
                    assert!(levels > 0);
                    while cur_subfunction_idx != selected_subfunction_idx && shard.subfunctions[cur_subfunction_idx].level > 0 {
                        cur_subfunction_idx = shard.subfunctions[cur_subfunction_idx].parent;
                    }
                    if cur_subfunction_idx == selected_subfunction_idx {
                        let highlight_level = shard.subfunctions[cur_subfunction_idx].level as usize;
                        assert!(highlight_level > 0);
                        let span = std::mem::replace(&mut spans[indent_span_idx], Span::raw(""));
                        assert!(span.content.len() % levels == 0);
                        let bytes_per_level = span.content.len() / levels;
                        let (start, end) = (bytes_per_level * (highlight_level - 1), bytes_per_level * highlight_level);
                        assert!(span.content.is_char_boundary(start) && span.content.is_char_boundary(end));
                        spans.splice(indent_span_idx..indent_span_idx+1, [
                            Span {content: span.content[..start].to_string().into(), style: span.style},
                            Span {content: span.content[start..end].to_string().into(), style: span.style.remove_modifier(Modifier::DIM).add_modifier(Modifier::BOLD)},
                            Span {content: span.content[end..].to_string().into(), style: span.style}]);
                    }
                }

                if i == tab.scroll.cursor && line_addr.kind != DisassemblyLineKind::Error {
                    spans.push(Span::raw(format!("{: >0$}", widest_line.max(area.width as usize) + 10))); // fill the rest of the line with spaces
                    for span in &mut spans[1..] {
                        span.style.bg = palette.selected_text_line.bg.clone();
                    }
                }

                lines.push(Spans::from(spans));
            }

            let paragraph = Paragraph::new(Text {lines: lines}).scroll((0, tab.hscroll));

            f.render_widget(paragraph, area);
        }
    }

    fn drop_caches(&mut self) {
        self.cache.clear();
        for tab in &mut self.tabs {
            match &mut tab.status {
                DisassemblyTabStatus::Unresolved {cached_error, ..} => *cached_error = None,
                _ => (),
            }
        }
    }
}

struct StatusWindow {}

impl Default for StatusWindow { fn default() -> Self { Self {} } }

impl WindowContent for StatusWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let mut items: Vec<ListItem> = Vec::new();

        items.push(match debugger.target_state {
            ProcessState::NoProcess => ListItem::new("no process").style(palette.default_dim),
            ProcessState::Starting => ListItem::new("starting").style(palette.state_other),
            ProcessState::Exiting => ListItem::new("exiting").style(palette.state_other),
            ProcessState::Stepping => ListItem::new("stepping").style(palette.state_other),
            ProcessState::Running => ListItem::new("running").style(palette.state_in_progress),
            ProcessState::Suspended => ListItem::new("suspended").style(palette.state_suspended),
        });

        if debugger.target_state != ProcessState::NoProcess {
            items.push(ListItem::new(Text::from(Spans::from(vec![Span::styled("pid: ", palette.default_dim), Span::raw(format!("{}", debugger.pid)), Span::styled(format!(" cpu {:.0}% mem {}", debugger.info.resource_stats.cpu_percentage(), PrettySize(debugger.info.resource_stats.rss_bytes)), palette.default_dim)]))));
        } else {
            items.push(ListItem::new(Text::from(Spans::from(vec![Span::styled("pid: none", palette.default_dim)]))));
        }
        items.push(ListItem::new(Text::from(Spans::from(vec![Span::styled(format!("nnd pid: {} cpu {:.0}% mem {}", my_pid(), debugger.my_resource_stats.cpu_percentage(), PrettySize(debugger.my_resource_stats.rss_bytes)), palette.default_dim)]))));
        match &debugger.persistent.path {
            Ok(p) => items.push(ListItem::new(format!("{}/", p.display())).style(palette.default_dim)),
            Err(e) => items.push(ListItem::new(format!("{}", e)).style(palette.error)),
        }

        if ui.last_error != "" {
            items.push(ListItem::new(ui.last_error.clone()).style(palette.status_error));
            items.push(ListItem::new(" "));
        }

        let space_left = (area.height as usize).saturating_sub(2 + items.len());

        let log_lines = debugger.log.lines.len();
        for line in &debugger.log.lines.make_contiguous()[log_lines.saturating_sub(space_left)..] {
            items.push(ListItem::new(&line[..]));
        }
        
        let list = List::new(items);
        f.render_widget(list, area);
    }
}

struct HintsWindow {}

impl Default for HintsWindow { fn default() -> Self { Self {} } }

impl WindowContent for HintsWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        if ui.profiler_enabled {
            let mut prof = StyledText::new();
            debugger.log.prof.format_summary(&mut prof, area.width as usize);
            let prof_lines = prof.to_lines();
            let mut items: Vec<ListItem> = Vec::new();
            items.push(ListItem::new("C-p to hide profiler").style(palette.default_dim));
            for line in prof_lines {
                items.push(ListItem::new(line));
            }
            let list = List::new(items);
            f.render_widget(list, area);
        } else {
            let mut x = area.x + 1;
            for i in 0..2 {
                let subarea = Rect {x, y: area.y, width: (area.width.saturating_sub(1) + i as u16)/2, height: area.height};
                x += subarea.width;
                let mut lines = ui.hints[i].to_lines();
                if lines.len() > subarea.height as usize && subarea.height > 0 {
                    lines[subarea.height as usize - 1] = Spans::from(vec![Span::raw("…")]);
                }
                for line in &mut lines {
                    for span in &mut line.0 {
                        if span.style == Style::default() {
                            span.style = debugger.context.settings.palette.default_dim;
                        }
                    }
                }
                let paragraph = Paragraph::new(Text {lines});
                f.render_widget(paragraph, subarea);
            }
        }
    }
}

struct BinariesWindow {
    scroll: Scroll,
}

impl Default for BinariesWindow { fn default() -> Self { Self {scroll: Scroll::new()} } }

impl WindowContent for BinariesWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        // Listed in order of mmap address, so the main executable is usually first, which is nice.
        ui.binaries = debugger.info.maps.list_binaries();
        // List previously seen unloaded binaries too, because they're visible to file/function open dialogs, especially if there's no debuggee process.
        for id in debugger.symbols.list() {
            if !debugger.info.binaries.contains_key(&id) {
                ui.binaries.push(id);
            }
        }

        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let range = self.scroll.update_cursorless(ui.binaries.len(), area.height.saturating_sub(1), &mut keys, &debugger.context.settings.keys);

        let mut rows: Vec<Row> = Vec::new();
        let mut column_widths = [3, 0, 11, 9];
        column_widths[1] = area.width.saturating_sub(column_widths.iter().sum::<u16>() + column_widths.len() as u16 - 1);
        for idx in range {
            let id = &ui.binaries[idx];
            let binary = debugger.symbols.get_if_present(id).unwrap();
            let is_mapped = debugger.info.binaries.contains_key(id);
            let mut cells: Vec<Cell> = vec![
                Cell::from(format!("{}", idx + 1)).style(palette.default_dim),
                Cell::from(""),
                // ELF or unwind error/loading/stats.
                match &binary.elf {
                    Err(e) if e.is_loading() => Cell::from(Span::styled("loading", palette.state_in_progress)),
                    Err(e) => Cell::from(Span::styled("error", palette.error)),
                    Ok(elf) => Cell::from(format!("{}", PrettySize(elf.data().len()))).style(palette.default_dim),
                },
                if binary.unwind.as_ref().is_err_and(|e| e.is_loading()) || binary.symbols.as_ref().is_err_and(|e| e.is_loading()) {
                    Cell::from(Span::styled("loading", palette.state_in_progress))
                } else if binary.unwind.is_ok() && binary.symbols.is_ok() {
                    Cell::from("ok").style(palette.default_dim)
                } else if binary.unwind.as_ref().map_or_else(|e| e.is_missing_symbols(), |_| true) && binary.symbols.as_ref().map_or_else(|e| e.is_missing_symbols(), |_| true) {
                    Cell::from(Span::styled("no", palette.warning_dim))
                } else {
                    Cell::from(Span::styled("error", palette.error_dim))
                }];
            // The wide cell may have multiple lines with progress bar or errors.
            let mut lines: Vec<Spans> = vec![Spans::from(vec![Span::raw(format!("{}", id.path))])];
            if binary.symbols.as_ref().is_err_and(|e| e.is_loading()) {
                ui.loading = true;
                let (progress_ppm, loading_stage) = debugger.symbols.get_progress(id);
                let mut text = format!("{}% ({})", progress_ppm / 10000, loading_stage);
                let wid = column_widths[1] as usize;
                let len = text.chars().count();
                if len <= wid {
                    // Center. Need len strictly > wid for the char_indices() thing below.
                    let pad = wid + 1 - len;
                    text.insert_str(0, &" ".to_string().repeat((pad-1)/2));
                    text.push_str(&" ".to_string().repeat(pad/2+1));
                    assert!(text.chars().count() == wid + 1);
                }
                // Text and progress bar combined. We assume there's no unicode.
                let bar = wid * progress_ppm.min(1000000) / 1000000;
                let pos = text.char_indices().skip(bar).next().unwrap().0;
                lines.push(Spans::from(vec![Span::styled(text[..pos].to_string(), palette.progress_bar_done), Span::styled(text[pos..].to_string(), palette.progress_bar_remaining)]));
            }
            if binary.elf.as_ref().is_err_and(|e| !e.is_loading()) {
                lines.push(Spans::from(vec![Span::styled(format!("{}", binary.elf.as_ref().err().unwrap()), palette.error)]));
            } else {
                if binary.unwind.as_ref().is_err_and(|e| !e.is_loading()) {
                    let e = binary.unwind.as_ref().err().unwrap();
                    lines.push(Spans::from(vec![Span::styled(format!("{}", e), if e.is_missing_symbols() {palette.default_dim} else {palette.error})]));
                }
                if binary.symbols.as_ref().is_err_and(|e| !e.is_loading()) {
                    let e = binary.symbols.as_ref().err().unwrap();
                    lines.push(Spans::from(vec![Span::styled(format!("{}", e), if e.is_missing_symbols() {palette.default_dim} else {palette.error})]))
                }
            }
            let h = lines.len();
            cells[1] = Cell::from(Text {lines});
            let mut row = Row::new(cells).height(h as u16);
            if !is_mapped {
                row = row.style(palette.default_dim);
            }
            rows.push(row);
        }
        let column_widths: Vec<Constraint> = column_widths.iter().map(|w| Constraint::Length(*w)).collect();
        let table = Table::new(rows)
            .header(Row::new(vec!["idx", "name", "file", "symbols"]).style(palette.table_header))
            .widths(&column_widths);

        f.render_widget(table, area);
    }
}

struct ThreadsFilter {
    bar: SearchBar,
    cache_key: String, // when this changes, drop cached_results
    cached_results: Vec<(/*thread_idx*/ usize, /*stop_count*/ usize, /*passes_filter*/ bool)>, // sorted by thread_idx
}
impl ThreadsFilter {
    fn new() -> Self { Self {bar: SearchBar::new(), cache_key: String::new(), cached_results: Vec::new()} }

    // tids must be sorted
    fn filter(&mut self, tids: Vec<(usize, usize, pid_t)>, debugger: &mut Debugger) -> Vec<pid_t> {
        let query = if self.bar.visible {self.bar.text.text.clone()} else {String::new()};
        if query != self.cache_key {
            self.cache_key = query.clone();
            self.cached_results.clear();
        }
        let (mut i, mut j) = (0, 0);
        let mut new_cached: Vec<(usize, usize, bool)> = Vec::new();
        let mut res: Vec<pid_t> = Vec::new();
        while i < tids.len() || j < self.cached_results.len() {
            if i < tids.len() && (j == self.cached_results.len() || tids[i].0 < self.cached_results[j].0 || (tids[i].0 == self.cached_results[j].0 && tids[i].1 != self.cached_results[j].1)) {
                // Cache miss.
                let (thread_idx, stop_count, tid) = tids[i].clone();
                let pass = Self::thread_passes(tid, &query, debugger);
                new_cached.push((thread_idx, stop_count, pass));
                if pass {
                    res.push(tid);
                }
                if j < self.cached_results.len() && tids[i].0 == self.cached_results[j].0 {
                    j += 1;
                }
                i += 1;
            } else if j < self.cached_results.len() && (i == tids.len() || self.cached_results[j].0 < tids[i].0) {
                // Thread disappeared, remove it from cache.
                j += 1;
            } else {
                // Cache hit.
                assert!(tids[i].1 == self.cached_results[j].1);
                new_cached.push(self.cached_results[j].clone());
                if self.cached_results[j].2 {
                    res.push(tids[i].2);
                }
                i += 1;
                j += 1;
            }
        }
        self.cached_results = new_cached;
        res
    }

    fn thread_passes(tid: pid_t, query: &str, debugger: &mut Debugger) -> bool {
        if query.is_empty() {
            return true;
        }
        // This is very primitive right now, but I'm not sure in which direction to take it to be most useful.
        let stack = debugger.get_stack_trace(tid, false);
        for subframe in &stack.subframes {
            if let Some(f) = &subframe.function_name {
                if f.find(query).is_some() {
                    return true;
                }
            }
            if let Some(line) = &subframe.line {
                if let Ok(s) = str::from_utf8(line.path.as_os_str().as_bytes()) {
                    if s.find(query).is_some() {
                        return true;
                    }
                }
            }
        }
        false
    }
}

struct ThreadsWindow {
    scroll: Scroll,

    seen_stop_counts: HashMap<pid_t, usize>,
    filter: ThreadsFilter,
}

impl Default for ThreadsWindow { fn default() -> Self { Self {scroll: Scroll::new(), seen_stop_counts: HashMap::new(), filter: ThreadsFilter::new() } } }

impl WindowContent for ThreadsWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        keys.retain(|key| {
            if self.filter.bar.editing {
                return true;
            }
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::Find) => self.filter.bar.start_editing(),
                Some(KeyAction::Cancel) => self.filter.bar.visible = false,
                _ => return true,
            }
            false
        });
        
        let mut tids: Vec<(usize, usize, pid_t)> = debugger.threads.values().map(|t| (t.idx, t.stop_count, t.tid)).collect();
        tids.sort_by_key(|t| t.0);
        let mut filtered_tids: Vec<pid_t> = self.filter.filter(tids, debugger);

        // If some thread hit a breakpoint or fatal signal, switch to it.
        self.seen_stop_counts.retain(|tid, _| debugger.threads.contains_key(tid));
        for (tid, thread) in &debugger.threads {
            if thread.stop_reasons.is_empty() {
                continue;
            }
            let seen = match self.seen_stop_counts.entry(*tid) {
                Entry::Vacant(v) => {
                    v.insert(thread.stop_count);
                    false
                }
                Entry::Occupied(mut o) => {
                    mem::replace(o.get_mut(), thread.stop_count) == thread.stop_count
                }
            };
            if seen {
                continue;
            }
            ui.selected_thread = *tid;
        }

        // If selected thread doesn't pass the filter, awkwardly add it to the list anyway, greyed out.
        let mut selected_thread_filtered_out = false;
        if debugger.threads.get(&ui.selected_thread).is_some() {
            if let Some(i) = filtered_tids.iter().position(|x| *x == ui.selected_thread) {
                self.scroll.cursor = i;
            } else {
                selected_thread_filtered_out = true;
                filtered_tids.push(ui.selected_thread);
                self.scroll.cursor = filtered_tids.len() - 1;
            }
        }

        let height = area.height.saturating_sub(1 + self.filter.bar.height());
        let mut range = self.scroll.update(filtered_tids.len(), height, &mut keys, &debugger.context.settings.keys);

        ui.selected_thread = filtered_tids.get(self.scroll.cursor).copied().unwrap_or(0);
        if selected_thread_filtered_out && ui.selected_thread != *filtered_tids.last().unwrap() {
            selected_thread_filtered_out = false;
            filtered_tids.pop();
            range = self.scroll.range(filtered_tids.len(), height as usize);
        }

        let f = match f { Some(f) => f, None => return };
        let context_temp = debugger.context.clone();
        let palette = &context_temp.settings.palette;

        let area = self.filter.bar.render("filter: ", "", f, area, palette);

        let tids: Vec<libc::pid_t> = filtered_tids[range.clone()].to_vec();
        let selected_idx = self.scroll.cursor - range.start;

        let rows: Vec<Row> = tids.iter().map(|tid| {
            let stack = debugger.get_stack_trace(*tid, /* partial */ true);
            let t = debugger.threads.get(tid).unwrap();
            let mut cells = vec![
                Cell::from(format!("{}", t.idx)).style(palette.default_dim),
                Cell::from(format!("{}", t.tid)).style(palette.default_dim),
                match t.info.name.clone() {
                    Ok(name) => Cell::from(name),
                    Err(e) => Cell::from(format!("{}", e)).style(palette.error),
                },
                Cell::from(format!("{}", t.info.resource_stats.state)).style(match t.info.resource_stats.state {
                    'R' | 'D' => palette.state_in_progress,
                    'S' | 'T' | 't' => palette.state_suspended,
                    'Z' | 'X' | 'x' => palette.error,
                    _ => palette.state_other,
                }),
                Cell::from(format!("{:.0}%", t.info.resource_stats.cpu_percentage(debugger.info.resource_stats.period_ns))).style(palette.default_dim),
            ];
            match t.state {
                ThreadState::Running => cells.extend_from_slice(&[
                    Cell::from(""),
                    Cell::from(""),
                    match &debugger.stepping {
                        Some(step) if step.tid == *tid => Cell::from(Span::styled("stepping", palette.state_other)),
                        _ => Cell::from(Span::styled("running", palette.state_in_progress)),
                    }]),
                ThreadState::Suspended if !stack.frames.is_empty() => {
                    let f = &stack.frames[0];
                    let sf = &stack.subframes[0];
                    cells.extend_from_slice(&[
                        Cell::from(format!("{:x}", f.addr)).style(palette.default_dim),
                        if let Some(binary_id) = &f.binary_id {
                            Cell::from(format!("{}", ui.binaries.iter().position(|b| b == binary_id).unwrap() + 1))
                        } else {
                            Cell::from("?").style(palette.default_dim)
                        },
                        if let Some(function_name) = &sf.function_name {
                            Cell::from(format!("{}", function_name))
                        } else {
                            Cell::from("?").style(palette.default_dim)
                        }]);
                }
                ThreadState::Suspended => cells.extend_from_slice(&[
                    Cell::from(""),
                    Cell::from(""),
                    if let Some(e) = &stack.truncated {
                        if e.is_loading() {
                            Cell::from(Span::styled("[loading]", palette.state_in_progress))
                        } else {
                            Cell::from(Span::styled(format!("err: {}", e), palette.error_dim))
                        }
                    } else {
                        Cell::from(Span::styled("[empty]", palette.default_dim))
                    }]),
            };

            if self.filter.bar.editing {
                // If no row is selected, tui-rs doesn't reserve horizontal space for highlight_symbol, so the table gets
                // shifted left and right by 3 characters every time filter editing starts and ends. Unacceptable! Add an empty column to compensate.
                cells.insert(0, Cell::from(""));
            }
            
            let mut row = Row::new(cells);
            // Highlight threads that got a fatal signal or hit a breakpoint.
            for stop in &t.stop_reasons {
                match stop {
                    StopReason::Signal(_) => {
                        row = row.style(palette.error);
                        break;
                    }
                    StopReason::Breakpoint(_) => {
                        row = row.style(palette.state_suspended);
                    }
                    StopReason::Step => (),
                }
            }
            if selected_thread_filtered_out && *tid == ui.selected_thread {
                row = row.style(palette.default_dim);
            }
            row
        }).collect();

        let highlight_symbol = ">> ";
        let mut table_state = TableState::default();
        let mut header: Vec<&'static str> = vec!["idx", "tid", "name", "s", "cpu", "addr", "bin", "function"];
        let mut widths: Vec<Constraint> = vec![Constraint::Length(5), Constraint::Length(10), Constraint::Length(15), Constraint::Length(1), Constraint::Length(4), Constraint::Length(12), Constraint::Length(3), Constraint::Percentage(100)];
        if self.filter.bar.editing {
            header.insert(0, "");
            widths.insert(0, Constraint::Length(str_width(highlight_symbol) as u16 - 1));
        } else {
            table_state.select(Some(selected_idx));
        }        
        
        let table = Table::new(rows)
            .header(Row::new(header).style(palette.table_header))
            .widths(&widths)
            .highlight_style(palette.table_selected_item).highlight_symbol(highlight_symbol);
        
        f.render_stateful_widget(table, area, &mut table_state);
    }

    fn drop_caches(&mut self) {
        self.filter.cached_results.clear();
    }

    fn update_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
        self.filter.bar.update(keys, &debugger.context.settings.keys);
    }

    fn cancel_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger) {
        self.filter.bar.editing = false;
    }

    fn get_hints(&self, hints: &mut StyledText) {
        styled_write!(hints, Style::default(), "/ - filter"); hints.close_line();
    }
}

// This window is in charge of telling source and disassembly windows when to auto-open and auto-scroll.
struct StackWindow {
    scroll: Scroll,
    // Previously selected frame in each thread, so that switching threads back and forth doesn't reset the scroll.
    // If thread's stop count changed, we switch to the top frame, unless the window is locked.
    threads: HashMap<pid_t, (/* stop_count */ usize, StableSubframeIdx)>,
    // When this changes, we scroll source and disassembly windows.
    seen: (/* tid */ pid_t, StableSubframeIdx, /* frame addr */ usize),
    // After symbols were loaded, tell disassembly and source windows to try opening the file/function again.
    rerequest_scroll: bool,
}

// Location of a stack trace subframe that tries to stay on the "same" subframe when the stack changes slightly,
// to avoid the focus jumping around unexpectedly to the user. Specifically:
//  * When symbols finish loading, some frames get expanded into multiple subframes. Focus should stay on the same frame as it was, though subframe index changes completely.
//    Hence storing frame and subframe idx separately.
//  * When the process is resumed and suspended without ever returning from the selected subfunction, keep focus on that subfunction if the stack window is locked.
//    Hence counting from the bottom of the stack rather than the top.
#[derive(Clone, Copy, PartialEq, Eq)]
struct StableSubframeIdx {
    frame_idx_rev: usize,
    subframe_subidx_rev: usize,
}
impl StableSubframeIdx {
    fn top() -> Self { Self {frame_idx_rev: usize::MAX, subframe_subidx_rev: usize::MAX} }
    fn new(stack: &StackTrace, mut subframe_idx: usize) -> Self {
        if stack.frames.is_empty() {
            return Self::top();
        }
        subframe_idx = subframe_idx.min(stack.subframes.len() - 1);
        let frame_idx = stack.subframes[subframe_idx].frame_idx;
        let frame = &stack.frames[frame_idx];
        Self {frame_idx_rev: stack.frames.len() - frame_idx, subframe_subidx_rev: frame.subframes.end - subframe_idx}
    }
    fn subframe_idx(&self, stack: &StackTrace) -> usize {
        if self.frame_idx_rev > stack.frames.len() || self.frame_idx_rev == 0 || self.subframe_subidx_rev == 0 {
            return 0;
        }
        let frame = &stack.frames[stack.frames.len() - self.frame_idx_rev];
        frame.subframes.start + frame.subframes.len().saturating_sub(self.subframe_subidx_rev)
    }
}

impl Default for StackWindow { fn default() -> Self { Self {scroll: Scroll::new(), threads: HashMap::new(), seen: (0, StableSubframeIdx::top(), 0), rerequest_scroll: false} } }

impl WindowContent for StackWindow {
    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        ui.stack = debugger.get_stack_trace(ui.selected_thread, /* partial */ false);
        let rerequest_scroll = mem::take(&mut self.rerequest_scroll);

        let header_height = 1usize;
        let height = area.height.saturating_sub(header_height as u16) / 2;
        let locked = false; // TODO: assign this after implementing window locking

        assert_eq!(ui.stack.frames.is_empty(), ui.stack.subframes.is_empty());
        let num_rows = ui.stack.subframes.len() + ui.stack.truncated.is_some() as usize; // add a fake row for error message if the stack is truncated

        if ui.stack.frames.is_empty() {
            ui.selected_frame = 0;
            ui.selected_subframe = 0;
        } else {
            // Behaviors that we're trying to implement here:
            //  * when switching threads back and forth without resuming the process, remember per-thread selected frame and auto-switch to that,
            //  * if a thread was resumed and stopped (e.g. a step or a breakpoint hit), switch to the top frame
            //     - even if the stack didn't change,
            //     - even if the thread was running for a very short time and no update_and_render() call happened during that,
            //     - even if the thread wasn't selected while it was running,
            //     - but if the stack window is locked, don't switch; keep the same position relative to the bottom of the stack,
            //  * when switching thread or stack frame, scroll the source and disassembly windows even if address didn't change (e.g. the same breakpoint was hit twice in a row)
            //     - including the switch-to-top case from above, even if top was already selected,
            //     - including the case when e.g. up-arrow key was pressed when already at the top of the stack (useful if the stack has only one frame),
            //  * if symbols loading completed and stack frames got expanded into multiple subframes of inlined functions, keep focus on the same frame,
            //  * when symbols loading completes, retry scrolling the source and disassembly windows, but only if they're currently showing an error (to avoid yanking valid file the user is looking at),
            //  * if the stack window is locked, and the process was stopped and resumed, and current frame's address changed, scroll the source and disassembly (if address didn't change, don't scroll).

            let deb_thr = debugger.threads.get(&ui.selected_thread).unwrap();
            let stop_count = deb_thr.stop_count;
            let mut switch_to_subframe: Option<usize> = None;
            let mut scroll_source_and_disassembly = false;

            let thr = self.threads.entry(ui.selected_thread).or_insert((0, StableSubframeIdx::top()));
            if thr.0 == stop_count || locked {
                self.scroll.cursor = thr.1.subframe_idx(&ui.stack);
                scroll_source_and_disassembly |= self.scroll.update_detect_movement(num_rows, height, &mut keys, &debugger.context.settings.keys).1;
            } else {
                switch_to_subframe = Some(deb_thr.subframe_to_select.unwrap_or(0));
            }
            thr.0 = stop_count;

            if let Some(i) = switch_to_subframe {
                self.scroll.set(num_rows, height, i);
                scroll_source_and_disassembly = true;
            }

            self.scroll.cursor = self.scroll.cursor.min(ui.stack.subframes.len() - 1); // move the cursor away from the fake row (but allow initially placing it there, just to scroll to it)
            ui.selected_subframe = self.scroll.cursor;
            let subframe = &ui.stack.subframes[ui.selected_subframe];
            ui.selected_frame = subframe.frame_idx;
            let frame = &ui.stack.frames[subframe.frame_idx];
            thr.1 = StableSubframeIdx::new(&ui.stack, ui.selected_subframe);

            let cur = (ui.selected_thread, thr.1, frame.addr);
            scroll_source_and_disassembly |= cur != self.seen;
            if scroll_source_and_disassembly || rerequest_scroll {
                ui.should_scroll_source = Some((subframe.line.as_ref().map(|line| SourceScrollTarget {path: line.path.clone(), version: line.version.clone(), line: line.line.line()}), !scroll_source_and_disassembly));
                ui.should_scroll_disassembly = Some((match &ui.stack.subframes[frame.subframes.end - 1].function {
                    Ok((_, function_idx)) => Ok(DisassemblyScrollTarget {binary_id: frame.binary_id.clone().unwrap(), function_idx: *function_idx, addr: frame.pseudo_addr,
                                                                         subfunction_level: if ui.selected_subframe == frame.subframes.start {u16::MAX} else {(frame.subframes.end - ui.selected_subframe) as u16}}),
                    Err(e) => Err(e.clone()),
                }, !scroll_source_and_disassembly));
            }
            self.seen = cur;
        }

        if self.threads.len() > debugger.threads.len() * 2 {
            self.threads.retain(|tid, _| { debugger.threads.contains_key(tid) });
        }

        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let mut range = self.scroll.range(num_rows, height as usize);
        range.end = num_rows.min(range.end + 1); // each row takes 2 lines, so there may be a half-row at the end - render it too

        let mut table_state = TableState::default();
        table_state.select(Some(self.scroll.cursor - range.start));

        let mut rows: Vec<Row> = Vec::new();
        for idx in range {
            if idx < ui.stack.subframes.len() {
                let subframe = &ui.stack.subframes[idx];
                let frame = &ui.stack.frames[subframe.frame_idx];
                let func_spans = vec![
                    if let Some(n) = &subframe.function_name {
                        Span::raw(format!("{}", n))
                    } else if let Err(e) = &subframe.function {
                        Span::styled(format!("{}", e), palette.error)
                    } else {
                        Span::styled("?", palette.default_dim)
                    }];
                let mut line_spans = vec![
                    if let Some(info) = &subframe.line {
                        let name = info.path.as_os_str().to_string_lossy();
                        Span::styled(format!("{}", name), palette.location_filename)
                    } else {
                        Span::styled("?", palette.location_filename.add_modifier(Modifier::DIM))
                    }];
                if let Some(info) = &subframe.line {
                    if info.line.line() != 0 {
                        line_spans.push(Span::styled(format!(":{}", info.line.line()), palette.location_line_number));
                        if info.line.column() != 0 {
                            line_spans.push(Span::styled(format!(":{}", info.line.column()), palette.location_column_number));
                        }
                    }
                }
                let idx_spans = vec![Span::styled(format!("{}", idx), palette.default_dim)];

                let is_inlined = idx != frame.subframes.end-1;

                let mut row = Row::new(vec![
                    Cell::from(Spans::from(idx_spans)),
                    Cell::from(if is_inlined {String::new()} else {format!("{:x}", frame.addr)}).style(palette.default_dim),
                    if is_inlined {
                        Cell::from("")
                    } else if let Some(b) = &frame.binary_id {
                        Cell::from(format!("{}", ui.binaries.iter().position(|x| x == b).unwrap() + 1))
                    } else {
                        Cell::from("?")
                    }.style(palette.default_dim),
                    Cell::from(Text {lines: vec![Spans::from(func_spans), Spans::from(line_spans)]}),
                ]);
                // The table widget doesn't show partial rows, so if the height is odd we have to manually set last row's height to 1 instead of 2.
                if header_height + rows.len() * 2 + 1 < area.height as usize {
                    row = row.height(2);
                }
                rows.push(row);
            } else {
                let e = ui.stack.truncated.as_ref().unwrap();
                rows.push(Row::new(vec![
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    if e.is_usage() || e.is_loading() {
                        Cell::from(Text {lines: vec![Spans::from(Span::styled(format!("{}", e), palette.default_dim)), Spans::default()]})
                    } else {
                        Cell::from(Text {lines: vec![Spans::from(Span::styled(format!("stack truncated: {}", e), palette.error)), Spans::default()]})
                    }
                ]));
            }
        }
        let table = Table::new(rows)
            .header(Row::new(vec![Text::from("idx"), Text::from("address"), Text::from("bin"), Text {lines: vec![Spans::from("location"), Spans::from(Span::styled("line", palette.location_filename))]}]).style(palette.table_header).height(/* tried 2, but it looks worse */ 1))
            .widths(&[Constraint::Length(3), Constraint::Length(12), Constraint::Length(3), Constraint::Percentage(90)])
            .highlight_style(palette.table_selected_item).highlight_symbol(">> ");

        f.render_stateful_widget(table, area, &mut table_state);
    }

    fn drop_caches(&mut self) {
        self.rerequest_scroll = true;
    }
}

struct CodeTab {
    title: String,
    path_in_symbols: PathBuf, // empty if not known
    version_in_symbols: FileVersionInfo,

    scroll: Scroll,
    hscroll: u16,
    pinned: bool,
}

struct SourceFile {
    header: StyledText, // can contain errors or warnings, not scrollable
    text: StyledText, // includes virtual space to cover all lines+columns mentioned in debug symbols
    local_path: PathBuf, // empty if error
    widest_line: usize,
    num_lines_in_local_file: usize,
}

struct CodeWindow {
    tabs: Vec<CodeTab>,
    file_cache: HashMap<(PathBuf, FileVersionInfo), SourceFile>,
    selected_tab: usize,

    search_dialog: Option<SearchDialog>,

    // When this changes (usually because the user moved the cursor around the file), we scroll disassembly to the address corresponding to the selected line.
    disassembly_scrolled_to: Option<(PathBuf, FileVersionInfo, /*cursor*/ usize)>,
}

impl Default for CodeWindow { fn default() -> Self { Self {tabs: Vec::new(), file_cache: HashMap::new(), selected_tab: 0, search_dialog: None, disassembly_scrolled_to: None} } }

impl CodeWindow {
    fn find_or_open_file<'a>(file_cache: &'a mut HashMap<(PathBuf, FileVersionInfo), SourceFile>, path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger) -> &'a SourceFile {
        file_cache.entry((path_in_symbols.to_owned(), version.clone())).or_insert_with(|| Self::open_file(path_in_symbols, version, debugger))
    }

    fn open_file(path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger) -> SourceFile {
        let palette = &debugger.context.settings.palette;
        let mut res = SourceFile {header: StyledText::new(), text: StyledText::new(), local_path: PathBuf::new(), widest_line: 0, num_lines_in_local_file: 0};

        if path_in_symbols.as_os_str().is_empty() {
            return res;
        }

        // Search for the file in all suffixes of the path, in case the code is in a different place from where the binary was built, or we're in subdirectory (e.g. build command was run from foo/build/, and we're in foo/src/).
        // This is kind of sketchy, but seems really useful and hopefully not too slow.
        let components: Vec<path::Component> = path_in_symbols.components().collect();
        for start_idx in 0..components.len().max(1) {
            let path_to_try: PathBuf = if start_idx == 0 {
                // Make sure to try the original path, in case components() -> collect() doesn't roundtrip exactly.
                path_in_symbols.to_owned()
            } else {
                components[start_idx..].iter().collect()
            };
            let mut file = match File::open(&path_to_try) {
                Ok(f) => f,
                Err(e) => continue,
            };

            res.local_path = match std::fs::canonicalize(&path_to_try) {
                Ok(p) => p,
                Err(_) => path_to_try.clone() };

            write!(res.header.chars, "{}", res.local_path.to_string_lossy()).unwrap();
            res.header.close_span(palette.location_filename);
            res.header.close_line();

            match Self::read_and_format_file(&mut file, debugger, &mut res, path_in_symbols) {
                Ok((len, md5)) => {
                    let mut warn = true;
                    if version.size != 0 && version.size as usize != len {
                        write!(res.header.chars, "file doesn't match debug symbols (different size: {} vs {})", len, version.size).unwrap();
                    } else if version.md5.is_some() && version.md5 != Some(md5) {
                        write!(res.header.chars, "file doesn't match debug symbols (md5 mismatch)").unwrap();
                    } else {
                        // We don't check modification time because it's usually not preserved by version control systems, so the check would only work if the binary was built locally.
                        warn = false;
                    }
                    if warn {
                        res.header.close_span(palette.warning);
                        res.header.close_line();
                    }
                }
                Err(e) => {
                    write!(res.header.chars, "{}", e).unwrap();
                    res.header.close_span(palette.error);
                    res.header.close_line();
                    res.text.clear();
                }
            }

            return res;
        }

        let path_str = path_in_symbols.to_string_lossy();
        write!(res.header.chars, "{}", path_str).unwrap();
        res.header.close_span(Style::default());
        res.header.close_line();
        if let Some(url) = rust_fake_path_to_url(&path_str) {
            write!(res.header.chars, "can be found here:").unwrap();
            res.header.close_span(palette.default_dim);
            res.header.close_line();
            
            write!(res.header.chars, "{}", url).unwrap();
            res.header.close_span(palette.url);
            res.header.close_line();
        } else {
            write!(res.header.chars, "file not found (tried all suffixes of this path, relative to current directory)").unwrap();
            res.header.close_span(palette.default_dim);
            res.header.close_line();
        }

        // Make a ghost file by adding LineInfo markers to empty space.
        let mut empty: &[u8] = &[];
        Self::read_and_format_file(&mut empty, debugger, &mut res, path_in_symbols).unwrap();

        res
    }

    fn read_and_format_file(input: &mut dyn Read, debugger: &Debugger, res: &mut SourceFile, path_in_symbols: &Path) -> Result<(usize, [u8; 16])> {
        let palette = &debugger.context.settings.palette;
        // Highlight all line+column locations where we can stop (statements and inlined function call sites).
        let mut markers: Vec<LineInfo> = Vec::new();
        for (_, binary) in debugger.symbols.iter() {
            let symbols = match &binary.symbols {
                Ok(s) => s,
                Err(_) => continue };
            markers.append(&mut symbols.list_lines_for_file(path_in_symbols));
        }
        markers.sort_unstable_by_key(|l| (l.line(), l.column().saturating_sub(1), !l.flags().contains(LineFlags::INLINED_FUNCTION)));

        let insert_markers_and_close_line = |line_idx: &mut usize, text: &mut StyledText, marker_idx: &mut usize| {
            assert_eq!(*text.lines.last().unwrap(), text.spans.len());
            let line_chars_start = text.spans.last().unwrap().0;
            *line_idx += 1;
            let mut prev_column = -1isize;
            while *marker_idx < markers.len() && markers[*marker_idx].line() <= *line_idx {
                let marker = &markers[*marker_idx];
                assert_eq!(marker.line(), *line_idx);
                let column = marker.column().saturating_sub(1);
                if column as isize <= prev_column {
                    assert_eq!(column as isize, prev_column);
                    *marker_idx += 1;
                    continue;
                }
                prev_column = column as isize;
                if line_chars_start + column >= text.chars.len() {
                    unsafe {text.chars.as_mut_vec().resize(line_chars_start + column + 1, b' ');}
                }
                let prev_span_end = text.spans.last().unwrap().0;
                assert!(line_chars_start + column >= prev_span_end);
                if line_chars_start + column > prev_span_end {
                    text.spans.push((line_chars_start + column, Style::default()));
                }
                text.spans.push((line_chars_start + column + 1, if marker.flags().contains(LineFlags::INLINED_FUNCTION) {palette.code_inlined_site} else {palette.code_statement}));
                *marker_idx += 1;
            }
            text.close_span(Style::default());
            text.close_line();
        };

        let mut marker_idx = 0usize;
        while marker_idx < markers.len() && markers[marker_idx].line() == 0 {
            marker_idx += 1;
        }

        let mut line_idx = 0usize;
        let mut buffered = HashingBufReader::new(input);
        loop {
            let buf = buffered.fill_buf()?;
            if buf.is_empty() {
                if res.text.spans.last().unwrap().0 < res.text.chars.len() {
                    insert_markers_and_close_line(&mut line_idx, &mut res.text, &mut marker_idx);
                }
                break;
            }
            let c = buf[0];
            if c == b'\r' || c == b'\n' {
                buffered.consume(1);
                if c == b'\r' && buffered.fill_buf()?.starts_with(b"\n") {
                    buffered.consume(1);
                }
                insert_markers_and_close_line(&mut line_idx, &mut res.text, &mut marker_idx);
                continue;
            }
            if c == b'\t' {
                buffered.consume(1);
                for i in 0..debugger.context.settings.tab_width {
                    res.text.chars.push(' ');
                }
                continue;
            }
            let n = buf.iter().copied().position(|c| c == b'\r' || c == b'\n' || c == b'\t').unwrap_or(buf.len());
            match String::from_utf8_lossy(&buf[..n]) {
                Cow::Borrowed(s) => res.text.chars.push_str(s),
                Cow::Owned(s) => res.text.chars.push_str(&s) };
            buffered.consume(n);
        }
        res.num_lines_in_local_file = res.text.num_lines();

        while marker_idx < markers.len() {
            insert_markers_and_close_line(&mut line_idx, &mut res.text, &mut marker_idx);
        }
        
        // This is width in bytes rather than glyphs, but that's good enough for our purposes here (upper bound on horizontal scrolling).
        res.widest_line = res.text.widest_line();

        Ok((buffered.count, buffered.hasher.compute().into()))
    }

    fn make_title(path_in_symbols: &Path) -> String {
        if path_in_symbols.as_os_str().is_empty() {
            "[unknown]".to_string()
        } else if let Some(f) = path_in_symbols.file_name() {
            f.to_string_lossy().into_owned()
        } else {
            "?".to_string()
        }
    }

    fn switch_to_file(&mut self, path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger) {
        if let Some(i) = self.tabs.iter().position(|t| &t.path_in_symbols == path_in_symbols && &t.version_in_symbols == version) {
            self.selected_tab = i;
            return;
        }
        let title = Self::make_title(path_in_symbols);
        self.tabs.push(CodeTab {title, path_in_symbols: path_in_symbols.to_owned(), version_in_symbols: version.clone(), scroll: Scroll::new(), hscroll: 0, pinned: false});
        self.selected_tab = self.tabs.len() - 1;
    }

    fn switch_tab(&mut self, delta: isize) {
        self.selected_tab = (self.selected_tab as isize + delta).rem_euclid(self.tabs.len().max(1) as isize) as usize;
    }

    fn toggle_breakpoint(&mut self, toggle_enabledness: bool, ui: &mut UIState, debugger: &mut Debugger) {
        if self.tabs.is_empty() {
            return;
        }
        let tab = &self.tabs[self.selected_tab];
        if tab.path_in_symbols.as_os_str().is_empty() {
            return;
        }
        ui.should_redraw = true;
        for (id, breakpoint) in debugger.breakpoints.iter() {
            match &breakpoint.on {
                BreakpointOn::Line(bp) if bp.path == tab.path_in_symbols && (bp.line == tab.scroll.cursor + 1 || bp.adjusted_line == Some(tab.scroll.cursor + 1)) => {
                    if toggle_enabledness {
                        let r = debugger.toggle_breakpoint_enabledness(id);
                        report_result(ui, &r);
                    } else {
                        debugger.remove_breakpoint(id);
                    }
                    return;
                }
                _ => (),
            }
        }
        if toggle_enabledness {
            ui.last_error = "no breakpoint".to_string();
        } else {
            let r = debugger.add_breakpoint(BreakpointOn::Line(LineBreakpoint {path: tab.path_in_symbols.clone(), file_version: tab.version_in_symbols.clone(), line: tab.scroll.cursor + 1, adjusted_line: None}));
            report_result(ui, &r);
        }
    }

    fn garbage_collect(&mut self) {
        let unpinned_tabs = self.tabs.iter().filter(|t| !t.pinned).count();
        if unpinned_tabs > 1 {
            // Remove all non-selected non-pinned tabs.
            let mut j = 0usize;
            for i in 0..self.tabs.len() {
                if self.tabs[i].pinned || j == self.selected_tab {
                    self.tabs.swap(i, j);
                    j += 1;
                } else if j < self.selected_tab {
                    self.selected_tab -= 1;
                }
            }
            self.tabs.truncate(j);
        }

        if self.file_cache.len().saturating_sub(self.tabs.len()) > 100 {
            // Drop half of the cached files that don't have tabs open.
            let in_use: HashSet<(PathBuf, FileVersionInfo)> = self.tabs.iter().map(|t| (t.path_in_symbols.clone(), t.version_in_symbols.clone())).collect();
            let mut i = 0;
            self.file_cache.retain(
                |k, _|
                if in_use.contains(k) {
                    true
                } else {
                    i += 1;
                    i % 2 == 0
                }
            );
        }
    }

    fn disassembly_scroll_key(&self) -> Option<(PathBuf, FileVersionInfo, usize)> {
        match self.tabs.get(self.selected_tab) {
            Some(t) if !t.path_in_symbols.as_os_str().is_empty() => Some((t.path_in_symbols.clone(), t.version_in_symbols.clone(), t.scroll.cursor)),
            _ => None,
        }
    }

    fn scroll_disassembly_if_needed(&mut self, suppress: bool, select: isize, ui: &mut UIState, debugger: &Debugger) {
        let key = match self.disassembly_scroll_key() {
            None => return,
            Some(key) if Some(&key) == self.disassembly_scrolled_to.as_ref() && select == 0 => return,
            Some(x) => x,
        };
        self.disassembly_scrolled_to = Some(key.clone());
        if suppress {
            return;
        }

        for (binary_id, binary) in &debugger.info.binaries {
            let symbols = match &binary.symbols {
                Ok(s) => s,
                Err(_) => continue,
            };
            let file_idx = match symbols.path_to_used_file.get(&key.0 as &Path) {
                Some(i) => *i,
                None => continue };
            let addrs0 = match symbols.line_to_addrs(file_idx, key.2 + 1, false) {
                Ok(x) => x,
                Err(_) => continue,
            };
            assert!(!addrs0.is_empty());
            let mut addrs: Vec<(/*function_idx*/ usize, /*subfunction_level*/ u16, /*addr*/ usize)> = Vec::new();
            for (line, level) in addrs0 {
                let static_addr = line.addr();
                if let Ok((_, function_idx)) = symbols.addr_to_function(static_addr) {
                    let addr = binary.addr_map.static_to_dynamic(static_addr);
                    addrs.push((function_idx, level, addr));
                }
            }
            if addrs.is_empty() {
                continue;
            }
            addrs.sort_unstable_by_key(|(_, _, a)| *a);

            // You'd think it's trivial to scroll disassembly to the address corresponding to the selected line of source code.
            // But suppose the line is in a function that is inlined at various call sites, and may also have its own machine code.
            // Which of those should we select? Always the non-inlined one? No. The user could be looking at an inlined site
            // (e.g. autoselected based on stack trace) - then jumping to a totally different function would be jarring.
            // So I guess we should pick the address that's "closest" to the disassembly cursor, where "closest" means it's is in the
            // same function and has the lowest lowest-common-ancestor in subfunction tree. That's what we do here.
            // This can be pretty annoying though: if you randomly scroll back and forth in the source file, the disassembly will probably
            // jump to a different function at some point and will forget the relevant inlined site; maybe we should take stack trace into account too?
            let mut closest_idx = addrs.iter().position(|&(function_idx, level, addr)| {
                if level == 0 {
                    return true;
                }
                if level != u16::MAX {
                    return false;
                }
                let function = &symbols.functions[function_idx];
                if function.num_levels() < 2 {
                    return true;
                }
                let ranges = symbols.subfunction_ranges_at_level(1, function);
                let idx = ranges.partition_point(|r| r.range.end <= addr);
                idx == ranges.len() || ranges[idx].range.start > addr
            }).unwrap_or(0);
            let mut selected_idx: Option<usize> = None;
            if let Some((binary_id, selected_function_idx, selected_addr)) = &ui.selected_addr {
                if let Some(binary) = debugger.info.binaries.get(binary_id) {
                    let symbols = binary.symbols.as_ref().unwrap();
                    let function = &symbols.functions[*selected_function_idx];
                    let shard = &symbols.shards[function.shard_idx()];
                    let mut subfunction_ranges: Vec<Range<usize>> = Vec::new();
                    for level in 1..function.num_levels().max(1) {
                        let pc_ranges = symbols.subfunction_ranges_at_level(level, function);
                        let idx = pc_ranges.partition_point(|r| r.range.end <= *selected_addr);
                        if idx == pc_ranges.len() || pc_ranges[idx].range.start > *selected_addr {
                            break;
                        }
                        subfunction_ranges.push(pc_ranges[idx].range.clone());
                    }
                    let mut closest = (0usize, 0usize);
                    for (idx, &(function_idx, _, addr)) in addrs.iter().enumerate() {
                        if addr == *selected_addr {
                            selected_idx = Some(idx);
                        }
                        if function_idx != *selected_function_idx {
                            continue;
                        }
                        let mut level = 0usize;
                        while level < subfunction_ranges.len() && subfunction_ranges[level].contains(&addr) {
                            level += 1;
                        }
                        if level+1 > closest.0 {
                            closest = (level+1, idx);
                        }
                    }
                    closest_idx = closest.1;
                }
            }

            let idx = match selected_idx {
                Some(i) => (i as isize + select) as usize % addrs.len(),
                None => closest_idx,
            };

            ui.should_scroll_disassembly = Some((Ok(DisassemblyScrollTarget {binary_id: binary_id.clone(), function_idx: addrs[idx].0, addr: addrs[idx].2, subfunction_level: addrs[idx].1.saturating_add(1)}), false));

            break;
        }
    }
}

// For Rust standard library code, Rust compiler uses fake paths in debug symbols, like this:
// /rustc/5680fa18feaa87f3ff04063800aec256c3d4b4be/library/core/src/hash/sip.rs
// which means the file can be found here:
// https://github.com/rust-lang/rust/blob/5680fa18feaa87f3ff04063800aec256c3d4b4be/library/core/src/hash/sip.rs
fn rust_fake_path_to_url(path: &str) -> Option<String> {
    if path.len() < "/rustc/5680fa18feaa87f3ff04063800aec256c3d4b4be/".len() { return None; }
    if !path.starts_with("/rustc/") { return None; }
    if !(&path[7..47]).chars().all(|c| c.is_digit(16)) { return None; }
    let mut res = "https://github.com/rust-lang/rust/blob/".to_string();
    res.push_str(&path[7..]);
    Some(res)
}

impl WindowContent for CodeWindow {
    fn update_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
        if let Some(d) = &mut self.search_dialog {
            let event = d.update(keys, &debugger.context.settings.keys, &debugger.symbols, None);
            let mut file_to_open = None;
            match event {
                SearchDialogEvent::None => (),
                SearchDialogEvent::Cancel => self.search_dialog = None,
                SearchDialogEvent::Open(res) => file_to_open = Some(res),
                SearchDialogEvent::Done(res) => {
                    file_to_open = Some(res);
                    self.search_dialog = None;
                }
            }
            if let Some(res) = file_to_open {
                let file = &res.symbols.files[res.id];
                self.switch_to_file(&res.file, &file.version, debugger);
                self.tabs[self.selected_tab].pinned = true;
            }
            if self.search_dialog.is_some() {
                return;
            }
        }

        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                // Code window has its own tabs and consumes all tab switching inputs when active.
                // If code window shares a region with another window, there's currently no way to switch to it. When adding layout editing mode, we should probably just disallow that.
                Some(KeyAction::NextTab) => self.switch_tab(1),
                Some(KeyAction::PreviousTab) => self.switch_tab(-1),
                Some(KeyAction::PinTab) => if !self.tabs.is_empty() {
                    let t = &mut self.tabs[self.selected_tab];
                    if !t.path_in_symbols.as_os_str().is_empty() || t.pinned {
                        t.pinned ^= true;
                    }
                }
                _ => return true,
            }
            false
        });
    }

    fn render_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {
        if let Some(d) = &mut self.search_dialog {
            ui.loading |= d.render(f, screen_area, "find file", &debugger.context.settings.palette);
        }
    }

    fn cancel_modal(&mut self, ui: &mut UIState, debugger: &mut Debugger) {
        self.search_dialog = None;
    }

    fn drop_caches(&mut self) {
        self.file_cache.clear();
    }

    fn get_hints(&self, hints: &mut StyledText) {
        styled_write!(hints, Style::default(), "o - open file"); hints.close_line();
        styled_write!(hints, Style::default(), "C-y - pin/unpin tab"); hints.close_line();
        styled_write!(hints, Style::default(), "b - toggle breakpoint"); hints.close_line();
        styled_write!(hints, Style::default(), "B - enable/disable breakpoint"); hints.close_line();
        styled_write!(hints, Style::default(), ",/. - cycle disasm addrs"); hints.close_line();
    }

    fn has_persistent_state(&self) -> bool {
        true
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        out.write_usize(self.tabs.len())?;
        for tab in &self.tabs {
            out.write_path(&tab.path_in_symbols)?;
            tab.version_in_symbols.save_state(out)?;
            tab.scroll.save_state(out)?;
            out.write_u16(tab.hscroll)?;
            out.write_u8(tab.pinned as u8)?;
        }
        out.write_usize(self.selected_tab)?;
        Ok(())
    }
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        for i in 0..inp.read_usize()? {
            let path_in_symbols = inp.read_path()?;
            let title = Self::make_title(&path_in_symbols);
            self.tabs.push(CodeTab {title, path_in_symbols, version_in_symbols: FileVersionInfo::load_state(inp)?, scroll: Scroll::load_state(inp)?, hscroll: inp.read_u16()?, pinned: inp.read_u8()? != 0});
        }
        self.selected_tab = inp.read_usize()?.min(self.tabs.len().saturating_sub(1));
        // Prevent auto-scrolling the disassembly window before any input is made.
        self.disassembly_scrolled_to = self.disassembly_scroll_key();
        Ok(())
    }

    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, mut area: Rect) {
        let suppress_disassembly_autoscroll = ui.should_scroll_source.is_some();
        let switch_to = match mem::take(&mut ui.should_scroll_source) {
            Some((to, false)) => Some(to),
            Some((to, true)) if self.tabs.is_empty() || self.tabs[self.selected_tab].path_in_symbols.as_os_str().is_empty() => Some(to),
            _ => None,
        };

        let f = match f { Some(f) => f, None => return };

        if self.search_dialog.is_some() {
            keys.clear();
        }

        let mut select_disassembly_address: isize = 0;
        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::ToggleBreakpoint) => self.toggle_breakpoint(false, ui, debugger),
                Some(KeyAction::ToggleBreakpointEnabledness) => self.toggle_breakpoint(true, ui, debugger),
                Some(KeyAction::Open) => {
                    self.search_dialog = Some(SearchDialog::new(Arc::new(FileSearcher), debugger.context.clone()));
                    self.update_modal(ui, debugger, &mut Vec::new()); // kick off initial search with empty query
                }
                Some(KeyAction::PreviousMatch) => select_disassembly_address -= 1,
                Some(KeyAction::NextMatch) => select_disassembly_address += 1,
                _ => return true
            }
            false
        });

        match switch_to {
            None => (),
            Some(None) => self.switch_to_file(Path::new(""), &FileVersionInfo::default(), debugger),
            Some(Some(target)) => {
                self.switch_to_file(&target.path, &target.version, debugger);
                let tab = &mut self.tabs[self.selected_tab];
                tab.scroll.set(usize::MAX, area.height.saturating_sub(3), target.line.saturating_sub(1));
            }
        }

        self.garbage_collect();

        // (path_in_symbols, line) -> (column, selected, top)
        let mut instruction_pointers: HashMap<(&Path, usize), (usize, bool, bool)> = HashMap::new();
        for (idx, subframe) in ui.stack.subframes.iter().enumerate() {
            if let Some(info) = &subframe.line {
                instruction_pointers.insert((&info.path, info.line.line()), (info.line.column(), idx == ui.selected_subframe, idx == 0));
            }
        }

        let mut headers = 0u16;
        let palette = &debugger.context.settings.palette;

        if area.height > 0 {
            let mut a = area;
            a.height = 1;
            let tabs = Tabs::new(self.tabs.iter().map(
                |t| Spans::from(vec![
                    if t.pinned {
                        Span::styled("📌 ".to_string() + &t.title, palette.tab_title_active)
                    } else {
                        // Italic would be better, and would remove the need for the weird pin icon, but it doesn't work in some terminals (e.g. doesn't work for me with gterm+ssh+tmux combination).
                        Span::styled(t.title.clone(), palette.tab_title)
                    }])).collect())
                .select(self.selected_tab).highlight_style(palette.tab_title_selected);
            f.render_widget(tabs, a);
            headers += 1;

            area.y += 1;
            area.height -= 1;
        }

        let tab = match self.tabs.get_mut(self.selected_tab) {
            None => return,
            Some(t) => t };
        let file = Self::find_or_open_file(&mut self.file_cache, &tab.path_in_symbols, &tab.version_in_symbols, debugger);
        headers += file.header.num_lines() as u16;

        let line_num_len = (file.text.num_lines().saturating_add(1) as f64).log10().ceil() as usize;
        let height = area.height.saturating_sub(headers);
        let width = area.width.saturating_sub(2 + 2 + line_num_len as u16 + 1);

        let range = tab.scroll.update(file.text.num_lines(), height, &mut keys, &debugger.context.settings.keys);
        keys.retain(|key| {
            match debugger.context.settings.keys.map.get(key) {
                Some(KeyAction::CursorRight) => tab.hscroll = tab.hscroll.saturating_add(width),
                Some(KeyAction::CursorLeft) => tab.hscroll = tab.hscroll.saturating_sub(width),
                _ => return true }
            false
        });
        tab.hscroll = tab.hscroll.min(file.widest_line.saturating_sub(width as usize).try_into().unwrap_or(u16::MAX));

        struct BreakpointLine {
            line: usize,
            id: BreakpointId,
            enabled: bool,
            has_locations: bool,
            active: bool,
            adjusted: bool,
        }
        let mut breakpoint_lines: Vec<BreakpointLine> = Vec::new();
        for (id, breakpoint) in debugger.breakpoints.iter() {
            match &breakpoint.on {
                BreakpointOn::Line(bp) if bp.path == tab.path_in_symbols => {
                    breakpoint_lines.push(BreakpointLine {line: bp.line, id, enabled: breakpoint.enabled, has_locations: false, active: breakpoint.active, adjusted: false});
                    if let Some(&adj) = bp.adjusted_line.as_ref() {
                        if adj != bp.line && breakpoint.enabled {
                            breakpoint_lines.push(BreakpointLine {line: adj, id, enabled: breakpoint.enabled, has_locations: false, active: breakpoint.active, adjusted: true});
                        }
                    }
                }
                _ => () };
        }
        breakpoint_lines.sort_by_key(|t| (t.id, /* should attribute locations to the adjusted line, if present */ !t.adjusted));
        for location in &debugger.breakpoint_locations {
            for br in &location.breakpoints {
                if let &BreakpointRef::Id {id, ..} = br {
                    let idx = breakpoint_lines.partition_point(|b| b.id < id);
                    if idx < breakpoint_lines.len() && breakpoint_lines[idx].id == id {
                        if location.active {
                            breakpoint_lines[idx].has_locations = true;
                        } else {
                            breakpoint_lines[idx].active = false;
                        }
                    }
                }
            }
        }
        breakpoint_lines.sort_by_key(|t| (t.line, /* if there's an adjusted and unadjusted breakpoint on the same line, show the unadjusted one */ t.adjusted));

        let mut lines = file.header.to_lines();

        if !tab.path_in_symbols.as_os_str().is_empty() {
            for i in range {
                let mut spans: Vec<Span> = Vec::new();
                let mut column_number = 0usize;

                let ip_span = match instruction_pointers.get(&(&tab.path_in_symbols, i + 1)) {
                    None => Span::raw("  "),
                    Some(&(col, is_selected, is_top)) => {
                        column_number = col;
                        Span::styled("⮕ ", if is_selected {palette.instruction_pointer} else {palette.additional_instruction_pointer})
                    }
                };
                spans.push(ip_span);

                let idx = breakpoint_lines.partition_point(|t| t.line < i + 1);
                if idx < breakpoint_lines.len() && breakpoint_lines[idx].line == i + 1 {
                    let l = &breakpoint_lines[idx];
                    if !l.enabled {
                        spans.push(Span::styled("○ ", palette.secondary_breakpoint));
                    } else {
                        let active = l.active && l.has_locations;
                        spans.push(Span::styled(if active { "● " } else { "○ " }, if l.adjusted {palette.secondary_breakpoint} else {palette.breakpoint}));
                    }
                } else {
                    spans.push(Span::raw("  "));
                }

                let line_number_span = spans.len();
                spans.push(Span::styled(format!("{: >2$}{}", i + 1, if i < file.num_lines_in_local_file {" "} else {"~"}, line_num_len), palette.default_dim));

                let contents_span = spans.len();
                file.text.line_out(i, &mut spans);

                // Underline the character at the column number.
                if column_number != 0 {
                    let mut pos = column_number - 1;
                    let mut found = false;
                    for i in contents_span..spans.len() {
                        if pos < spans[i].content.len() {
                            found = true;
                            if !spans[i].content.is_char_boundary(pos) {
                                break;
                            }

                            let span = std::mem::replace(&mut spans[i], Span::raw(""));
                            let mut end = pos + 1;
                            while !span.content.is_char_boundary(end) { end += 1; }
                            spans.splice(i..i+1, [Span {content: span.content[..pos].to_string().into(), style: span.style},
                                                  Span {content: span.content[pos..end].to_string().into(), style: span.style.add_modifier(Modifier::UNDERLINED | Modifier::BOLD)},
                                                  Span {content: span.content[end..].to_string().into(), style: span.style}]);
                            break;
                        }
                        pos -= spans[i].content.len();
                    }
                    if !found {
                        spans.push(Span::raw(" ".to_string().repeat(pos)));
                        spans.push(Span::styled(" ", Style::default().add_modifier(Modifier::UNDERLINED)));
                    }
                }

                if i == tab.scroll.cursor {
                    spans.push(Span::raw(format!("{: >0$}", file.widest_line.max(area.width as usize) + 10))); // fill the rest of the line with spaces; the +10 is needed because str_width() is not implemented
                    for span in &mut spans[line_number_span..] {
                        if span.style.bg.is_none() {
                            span.style.bg = palette.selected_text_line.bg.clone();
                        }
                    }
                }
                lines.push(Spans::from(spans));
            }
        }

        let paragraph = Paragraph::new(Text {lines}).scroll((0, tab.hscroll));

        f.render_widget(paragraph, area);

        self.scroll_disassembly_if_needed(suppress_disassembly_autoscroll, select_disassembly_address, ui, debugger);
    }
}

struct BreakpointsWindow {
    scroll: Scroll,
    selected_breakpoint: Option<BreakpointId>,
}

impl Default for BreakpointsWindow { fn default() -> Self { Self {scroll: Scroll::new(), selected_breakpoint: None} } }

impl WindowContent for BreakpointsWindow {
    fn get_hints(&self, hints: &mut StyledText) {
        hints.chars.push_str("<del> - delete breakpoint"); hints.close_span(Style::default()); hints.close_line();
        hints.chars.push_str("B - enable/disable"); hints.close_span(Style::default()); hints.close_line();
    }

    fn update_and_render(&mut self, ui: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, area: Rect) {
        let mut hit_breakpoints: Vec<BreakpointId> = Vec::new();
        for (tid, thread) in &debugger.threads {
            for reason in &thread.stop_reasons {
                if let &StopReason::Breakpoint(id) = reason {
                    hit_breakpoints.push(id);
                }
            }
        }
        hit_breakpoints.sort();
        hit_breakpoints.dedup();

        let f = match f { Some(f) => f, None => return };
        let height = area.height.saturating_sub(1);

        let mut breakpoints: Vec<BreakpointId> = debugger.breakpoints.iter().map(|p| p.0).collect();
        breakpoints.sort_by_key(|id| id.seqno);

        if let &Some(id) = &self.selected_breakpoint {
            if let Some(idx) = breakpoints.iter().position(|b| b == &id) {
                self.scroll.cursor = idx;

                keys.retain(|key| {
                    match debugger.context.settings.keys.map.get(key) {
                        Some(KeyAction::DeleteRow) => {
                            debugger.remove_breakpoint(id);
                            breakpoints.remove(self.scroll.cursor);
                        }
                        Some(KeyAction::ToggleBreakpointEnabledness) => {
                            let r = debugger.toggle_breakpoint_enabledness(id);
                            report_result(ui, &r);
                        }
                        _ => return true,
                    }
                    false
                });
            }
        }

        let mut locations: Vec<(BreakpointId, /*error*/ bool)> = Vec::new();
        for loc in &debugger.breakpoint_locations {
            for b in &loc.breakpoints {
                match b {
                    BreakpointRef::Id {id, ..} => locations.push((*id, loc.error.is_some())),
                    _ => ()}
            }
        }
        locations.sort();

        let (range, moved) = self.scroll.update_detect_movement(breakpoints.len(), height, &mut keys, &debugger.context.settings.keys);
        self.selected_breakpoint = breakpoints.get(self.scroll.cursor).copied();

        if moved && !breakpoints.is_empty() {
            match &debugger.breakpoints.get(breakpoints[self.scroll.cursor]).on {
                BreakpointOn::Line(on) => {
                    ui.should_scroll_source = Some((Some(SourceScrollTarget {path: on.path.clone(), version: on.file_version.clone(), line: on.line}), false));
                }
            }
        }

        let mut table_state = TableState::default();
        table_state.select(Some(self.scroll.cursor - range.start));

        let on_width = area.width.saturating_sub(3+4+2+5+5+4);
        let widths = [Constraint::Length(4), Constraint::Length(2), Constraint::Length(on_width), Constraint::Length(5), Constraint::Length(5)];

        let palette = &debugger.context.settings.palette;
        let mut rows: Vec<Row> = Vec::new();
        for id in &breakpoints[range] {
            let b = debugger.breakpoints.get(*id);
            let is_hit = hit_breakpoints.binary_search(id).is_ok();
            let locs_begin = locations.partition_point(|t| t.0 < *id);
            let locs_end = locations.partition_point(|t| t.0 <= *id);
            let mut cells = vec![Cell::from(format!("{}", id.seqno)).style(palette.default_dim)];
            if is_hit {
                cells.push(Cell::from("⮕ ").style(palette.instruction_pointer));
            } else if !b.enabled {
                cells.push(Cell::from("○ ").style(palette.secondary_breakpoint));
            } else if !b.active || locs_begin == locs_end {
                cells.push(Cell::from("○ ").style(palette.breakpoint));
            } else {
                cells.push(Cell::from("● ").style(palette.breakpoint));
            }
            match &b.on {
                BreakpointOn::Line(on) => {
                    let name = on.path.as_os_str().to_string_lossy();
                    let mut spans = vec![
                        Span::styled(name, palette.location_filename),
                        Span::styled(format!(":{}", on.line), palette.location_line_number)];
                    if let &Some(adj) = &on.adjusted_line {
                        spans.push(Span::styled("→", palette.default_dim));
                        spans.push(Span::styled(format!("{}", adj), palette.location_line_number));
                    }

                    // Manually cut off the text on the left side instead of right. When we rewrite the TUI library, this should just be a flag on the table cell, or something.
                    let suf: usize = spans[1..].iter().map(|s| str_width(&s.content)).sum();
                    let w = str_width(&spans[0].content);
                    let on_width = on_width as usize;
                    if w + suf > on_width {
                        let i = str_suffix_with_width(&spans[0].content, on_width.saturating_sub(suf + 1));
                        spans[0].content = ("…".to_string() + &spans[0].content[i..]).into();
                    }

                    cells.push(Cell::from(Spans::from(spans)));
                }
            }
            cells.push(Cell::from(format!("{}", locs_end - locs_begin)));
            cells.push(Cell::from(format!("{}", b.hits)));

            let error = b.addrs.as_ref().is_err_and(|e| !e.is_not_calculated()) || locations[locs_begin..locs_end].iter().any(|t| t.1);
            let mut style = if error {palette.error} else {Style::default()};
            if !b.enabled {
                style = style.add_modifier(Modifier::DIM);
            }

            rows.push(Row::new(cells).style(style));
        }

        let table = Table::new(rows)
            .header(Row::new(vec!["idx", "", "on", "locs", "hits"]).style(palette.table_header))
            .widths(&widths)
            .highlight_style(palette.table_selected_item).highlight_symbol(">> ");

        f.render_stateful_widget(table, area, &mut table_state);
    }
}
