use crate::{*, debugger::*, error::*, log::*, symbols::*, symbols_registry::*, util::*, registers::*, procfs::*, unwind::*, disassembly::*, pool::*, layout::*, settings::*, context::*, types::*, expr::*, widgets::*, search::*, arena::*, interp::*, imgui::*, common_ui::*, terminal::*};
use std::{io::{self, Write, BufRead, BufReader, Read}, mem::{self, take}, collections::{HashSet, HashMap, hash_map::Entry}, os::fd::AsRawFd, path, path::{Path, PathBuf}, fs::File, fmt::Write as FmtWrite, borrow::Cow, ops::Range, str, os::unix::ffi::OsStrExt, sync::{Arc}, time::Duration};
use libc::{self, pid_t};
use rand::random;

pub struct DebuggerUI {
    pub terminal: Terminal,
    pub ui: UI,
    input: InputReader,
    state: UIState,
    layout: Layout,

    // These are assigned by build(), to be checked and acted on by the caller.
    pub should_quit: bool,
    pub should_drop_caches: bool,
}

#[derive(Default)]
pub struct UIState {
    last_error: String, // reset on next key press
    key_hints: Vec<KeyHint>,
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
    // The only_if_error_tab solves the following; typically the should_scroll_* are first requested before symbols are loaded; that makes source and disassembly windows initially show errors;
    // when symbols are loaded, we want to try again and stop showing errors; but maybe the user already opened some non-error file and is looking at it - then we shouldn't forcefully switch to current file;
    // so we set this flag, which tells the windows to scroll only if the error tab is selected.
    should_scroll_source: Option<(Option<SourceScrollTarget>, /*only_if_on_error_tab*/ bool)>,
    should_scroll_disassembly: Option<(Result<DisassemblyScrollTarget>, /*only_if_on_error_tab*/ bool)>,
}

#[derive(Default)]
pub struct KeyHint {
    pub key_ranges: Vec<[KeyAction; 2]>,
    pub hint: &'static str,
}
impl KeyHint {
    fn key(key: KeyAction, hint: &'static str) -> Self { Self {key_ranges: vec![[key, key]], hint} }
    fn keys(keys: &[KeyAction], hint: &'static str) -> Self { Self {key_ranges: keys.iter().map(|k| [*k, *k]).collect(), hint} }
    fn ranges(ranges: &[[KeyAction; 2]], hint: &'static str) -> Self { Self {key_ranges: ranges.iter().cloned().collect(), hint} }
}

struct SourceScrollTarget {
    path: PathBuf,
    version: FileVersionInfo,
    line: usize,
}

struct DisassemblyScrollTarget {
    binary_id: BinaryId,
    symbols_identity: usize,
    function_idx: usize,
    static_pseudo_addr: usize,
    subfunction_level: u16,
}

pub trait WindowContent {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI);

    // Called after some symbols finished loading, or if the user requested a redraw.
    fn drop_caches(&mut self) {}

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {}

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
    Hints = 8,

    Other = 9,

    Status = 10,
}

impl DebuggerUI {
    pub fn new() -> Self {
        let ui = UI::default();
        let mut state = UIState::default();
        state.profiler_enabled = true;
        let layout = Self::default_layout(&state);
        // TODO: Load keys and colors from config file(s), do hot-reloading (for experimenting with colors quickly).

        Self {terminal: Terminal::new(), input: InputReader::new(), layout, ui, state, should_drop_caches: false, should_quit: false}
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

    fn default_layout(state: &UIState) -> Layout {
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
        let cols = layout.split(layout.root, Axis::X, vec![0.3, 0.75]);

        let rows = layout.split(cols[0], Axis::Y, vec![0.3, 0.4]);
        let hints_window = layout.new_window(Some(rows[0]), WindowType::Hints, true, "cheat sheet".to_string(), Box::new(HintsWindow::default()));
        let status_window = layout.new_window(Some(rows[1]), WindowType::Status, true, "status".to_string(), Box::new(StatusWindow::default()));
        layout.set_fixed_size(status_window, Axis::Y, 7);
        let watches_window = layout.new_window(Some(rows[2]), WindowType::Watches, true, "watches".to_string(), Box::new(WatchesWindow::default()));
        layout.new_window(Some(rows[2]), WindowType::Watches, true, "registers".to_string(), Box::new(RegistersWindow::default()));
        layout.new_window(Some(rows[2]), WindowType::Locations, true, "locations".to_string(), Box::new(LocationsWindow::default()));

        let rows = layout.split(cols[1], Axis::Y, vec![0.4]);
        let disassembly_window = layout.new_window(Some(rows[0]), WindowType::Disassembly, true, "disassembly".to_string(), Box::new(DisassemblyWindow::default()));
        let code_window = layout.new_window(Some(rows[1]), WindowType::Code, true, "code".to_string(), Box::new(CodeWindow::default()));

        let rows = layout.split(cols[2], Axis::Y, vec![0.25, 0.75]);
        layout.new_window(Some(rows[0]), WindowType::Binaries, true, "binaries".to_string(), Box::new(BinariesWindow::default()));
        let breakpoints_window = layout.new_window(Some(rows[0]), WindowType::Breakpoints, true, "breakpoints".to_string(), Box::new(BreakpointsWindow::default()));
        let stack_window = layout.new_window(Some(rows[1]), WindowType::Stack, true, "stack".to_string(), Box::new(StackWindow::default()));
        let threads_window = layout.new_window(Some(rows[2]), WindowType::Threads, true, "threads".to_string(), Box::new(ThreadsWindow::default()));

        layout.set_hotkey_number(watches_window, 1);
        layout.set_hotkey_number(disassembly_window, 2);
        layout.set_hotkey_number(code_window, 3);
        layout.set_hotkey_number(breakpoints_window, 4);
        layout.set_hotkey_number(stack_window, 5);
        layout.set_hotkey_number(threads_window, 6);

        layout.active_window = Some(threads_window);

        layout
    }

    pub fn buffer_input(&mut self, prof: &mut ProfileBucket) -> Result<bool> {
        let mut evs: Vec<Event> = Vec::new();
        let bytes = self.input.read(&mut evs, prof)?;
        prof.ui_input_bytes += bytes;

        let mut significant = false;
        for e in &evs {
            match e {
                Event::Key(_) => significant = true,
                Event::Mouse(m) if m.event == MouseEvent::Press => significant = true,
                _ => (),
            }
        }
        if significant {
            self.state.last_error = String::new();
        }

        Ok(self.ui.buffer_input(&evs))
    }

    pub fn update_and_render(&mut self, debugger: &mut Debugger) -> Result<()> {
        let mut timer = TscScopeExcludingSyscalls::new(&debugger.prof.bucket);
        let mut buffer = self.terminal.start_frame(self.ui.palette.default, &mut debugger.prof.bucket)?;
        let fill_tsc = timer.restart(&debugger.prof.bucket);

        self.ui.start_build(buffer.width, buffer.height);

        self.build(debugger)?;

        if self.should_quit {
            return Ok(());
        }

        self.ui.end_build(&mut buffer);
        let build_tsc = timer.restart(&debugger.prof.bucket) - self.ui.prof_render_tsc;

        let commands = self.terminal.prepare_command_buffer(&buffer, self.ui.should_show_cursor.clone());
        debugger.prof.bucket.ui_output_bytes += commands.len();
        self.terminal.present(buffer, commands, &mut debugger.prof.bucket)?;
        let fill_tsc = fill_tsc + timer.finish(&debugger.prof.bucket);

        debugger.prof.bucket.ui_build_max_tsc.set_max(build_tsc);
        debugger.prof.bucket.ui_render_max_tsc.set_max(self.ui.prof_render_tsc);
        debugger.prof.bucket.ui_fill_max_tsc.set_max(fill_tsc);
        debugger.prof.bucket.ui_max_tsc.set_max(build_tsc + self.ui.prof_render_tsc + fill_tsc);

        Ok(())
    }

    fn build(&mut self, debugger: &mut Debugger) -> Result<()> {
        self.should_drop_caches = false;

        let w = self.ui.get_mut(self.ui.content_root);
        w.flags.insert(WidgetFlags::CAPTURE_ALL_KEYS);
        let keys = w.keys.clone();

        // Handle some of the global hotkeys. Windows may also handle their global hotkeys by attaching them to the content_root widget, e.g. threads window handling global hotkeys for switching threads.
        for key in &keys {
            match self.ui.key_binds.key_to_action.get(key) {
                Some(KeyAction::Quit) => {
                    self.should_quit = true;
                    return Ok(());
                }
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

                Some(KeyAction::DropCaches) => {
                    self.terminal.clear()?; // do this only on hotkey, not when drop_caches is initiated by e.g. symbols loading
                    self.should_drop_caches = true;
                }

                Some(KeyAction::ToggleProfiler) => self.state.profiler_enabled ^= true,

                _ => (),
            }
        }

        // Special treatment for hints/profiler window. Maybe we should just make them different windows instead.
        if let Some(id) = self.layout.any_window_by_type(WindowType::Hints) {
            let w = self.layout.windows.get_mut(id);
            if self.state.profiler_enabled {
                w.fixed_size[Axis::Y] = None;
                w.title = "profiler".to_string();
            } else {
                w.fixed_size[Axis::Y] = Some(HintsWindow::NORMAL_HEIGHT);
                w.title = "controls".to_string();
            }
        }

        // Draw lines around windows, determine which window is active.
        self.layout.build(&mut self.ui);

        // Hints window content. Keep it brief, there's not much space.
        let mut hints = Vec::new();
        if debugger.target_state == ProcessState::NoProcess {
            hints.push(KeyHint::key(KeyAction::Quit, "quit"));
        } else if debugger.mode == RunMode::Attach {
            hints.push(KeyHint::key(KeyAction::Quit, "detach and quit"));
        } else {
            hints.push(KeyHint::key(KeyAction::Quit, "kill and quit"));
        }
        hints.push(KeyHint::ranges(&[[KeyAction::Window(0), KeyAction::Window(9)], [KeyAction::WindowLeft, KeyAction::WindowDown]], "switch window"));
        hints.push(KeyHint::keys(&[KeyAction::NextTab, KeyAction::PreviousTab], "switch tab"));
        match debugger.target_state {
            ProcessState::Running => hints.push(
                KeyHint::key(KeyAction::Suspend, "suspend")),
            ProcessState::Stepping => hints.extend([
                KeyHint::key(KeyAction::Suspend, "suspend"),
                KeyHint::key(KeyAction::Continue, "continue")]),
            ProcessState::Suspended => hints.extend([
                KeyHint::keys(&[KeyAction::PreviousStackFrame, KeyAction::NextStackFrame, KeyAction::PreviousThread, KeyAction::NextThread], "switch frame/thread"),
                KeyHint::key(KeyAction::Continue, "continue"),
                KeyHint::keys(&[KeyAction::StepIntoLine, KeyAction::StepOverLine, KeyAction::StepOut, KeyAction::StepOverColumn], "step into/over/out/column"),
                KeyHint::keys(&[KeyAction::StepIntoInstruction, KeyAction::StepOverInstruction], "step into/over instruction")]),
            ProcessState::NoProcess if debugger.mode == RunMode::Run => hints.push(
                KeyHint::key(KeyAction::Run, "start")),
            _ => (),
        }
        if debugger.mode == RunMode::Run && debugger.target_state != ProcessState::NoProcess {
            hints.push(KeyHint::key(KeyAction::Kill, "kill"));
        }
        if let &Some(id) = &self.layout.active_window {
            if let Some(w) = self.layout.windows.try_get(id) {
                hints.push(KeyHint::keys(&[], "")); // spacer
                w.content.get_key_hints(&mut hints);
            }
        }
        self.state.key_hints = hints;

        // Build windows.
        for (_, win) in self.layout.sorted_windows_mut() {
            with_parent!(self.ui, win.widget, {
                win.content.build(&mut self.state, debugger, &mut self.ui);
            });
        }

        // Stepping has to be handled after updating windows because selected_subframe is assigned by StackWindow.
        for key in &keys {
            let (kind, by_instructions, use_line_number_with_column) = match self.ui.key_binds.key_to_action.get(key) {
                Some(KeyAction::StepIntoLine) => (StepKind::Into, false, false),
                Some(KeyAction::StepIntoInstruction) => (StepKind::Into, true, false),
                Some(KeyAction::StepOverLine) => (StepKind::Over, false, false),
                Some(KeyAction::StepOverColumn) => (StepKind::Over, false, true),
                Some(KeyAction::StepOverInstruction) => (StepKind::Over, true, false),
                Some(KeyAction::StepOut) => (StepKind::Out, false, false),
                Some(KeyAction::StepOutNoInline) => (StepKind::Out, true, false),
                _ => continue,
            };
            let r = debugger.step(self.state.selected_thread, self.state.selected_subframe, kind, by_instructions, use_line_number_with_column);
            report_result(&mut self.state, &r);
            self.ui.should_redraw = true;
        }

        Ok(())
    }

    pub fn drop_caches(&mut self) {
        for (win_id, win) in self.layout.windows.iter_mut() {
            win.content.drop_caches();
        }
    }
}

fn report_result<R>(state: &mut UIState, r: &Result<R>) {
    if let Err(e) = r {
        state.last_error = format!("{}", e);
        if !e.is_usage() {
            eprintln!("warning: {}", e);
        }
    }
}

// TODO: Make it a subtree in locals instead.
#[derive(Default)]
struct RegistersWindow {
    table_state: TableState,
}

impl WindowContent for RegistersWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        let l = ui_writeln!(ui, default, "registers");
        ui.add(widget!().text(l));
        /*asdqwe
        let f = match f { Some(f) => f, None => return };
        let palette = &debugger.context.settings.palette;

        let mut rows: Vec<Row> = Vec::new();
        if let Some(frame) = state.stack.frames.get(state.selected_frame) {
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

        f.render_widget(table, area);*/
     }
}
/*asdqwe
#[derive(Clone, Copy)]
struct ValueTreeNodeIdx(usize);
impl ValueTreeNodeIdx {
    fn invalid() -> Self { Self(usize::MAX) }
    fn is_valid(self) -> bool { self.0 != usize::MAX }
}

struct ValueTreeNode {
    name: Range<usize>, // lines in ValueTree's `text`
    value: Result<Value>,
    dubious: bool,

    // Unique identifier of this node. Hash of identities (field names or array indices or similar) of nodes on the path from the root to here.
    // For remembering which subtrtees were expanded and where the cursor was, in a way that survives recalculation or partial change of the values.
    identity: usize,

    depth: usize,
    parent: ValueTreeNodeIdx,

    // These are populated lazily.
    formatted_line: [Option<Range<usize>>; 3], // collapsed, expanded, line-wrapped; ranges in ValueTree's `text`
    has_children: Option<bool>, // populated together with either element of formatted_line
    children: Option<Range<ValueTreeNodeIdx>>, // populated together with formatted_line[1]
}
impl Default for ValueTreeNode { fn default() -> Self { Self {name: "", value: err!(Internal, "if you're seeing this, there's a bug"), dubious: false, identity: 0, depth: 0, parent: ValueTreeNodeIdx::invalid(), formatted_line: [None; 2], has_children: None, children: None} } }

// An element of depth-first traversal of tree nodes, descending only into expanded ones. The cursor stands on one of these.
#[derive(Clone, Default)]
struct ValueTreeRow {
    node: ValueTreeNodeIdx,
    expanded: bool,

    widget: WidgetIdx, // invalid() if we didn't create it yet
    // Information needed to create the widget.
    parent_widget: WidgetIdx,
    rel_y: isize,
    indent_built: bool,
    // If false, widget for this row should be created by build_tree(). Otherwise widget may be created later, after layout and visibility check.
    deferred: bool,
}

#[derive(Default)]
struct ValueTree {
    nodes: Vec<ValueTreeNode>,
    roots: Vec<ValueTreeNodeIdx>, // (so it's technically a forest, but ValueTree sounds nicer, and you can always imagine an implicit root node that has these "root" nodes as children)
    text: StyledText,
    rows: Vec<ValueTreeRow>, // if empty, we should repopulate it
    arena: Arena,
}
impl ValueTree {
    fn clear(&mut self) { *self = Self::default(); }

    fn add(&mut self, mut node: ValueTreeNode) -> ValueTreeNodeIdx {
        assert!(node.depth == 0);
        if node.parent != usize::MAX {
            let parent = &self.nodes[node.parent];
            node.identity = hash(&(parent.identity, node.identity));
            node.depth = parent.depth + 1;
        } else {
            self.roots.push(self.nodes.len());
        }
        self.nodes.push(node);
        ValueTreeNodeIdx(self.nodes.len() - 1)
    }
}*/

#[derive(Default)]
struct WatchesWindow {/*asdqwe
    tree: ValueTree,
    expanded_nodes: HashSet<usize>,

    cursor_path: Vec<usize>,
    cursor_idx: usize,
    scroll: isize,
    scroll_to_cursor: bool,

    // Layout information. We do layout and line wrapping manually to avoid creating widgets for invisible rows.
    name_width: usize,
    value_width: usize,
    row_height_limit: usize,
    indent_width: usize,
    max_indent: usize,

    // When this changes, we recalculate everything.
    seen: (/*thread*/ pid_t, /*selected_subframe*/ usize, /*stop_count*/ usize),

    next_watch_id: usize,
    expressions: Vec<(/*identity*/ usize, String)>, // watch expressions; parallel to tree.roots
    text_input: Option<(/*identity*/ usize, TextInput)>, // if editing watch expression
    text_input_built: bool,

    eval_state: EvalState,*/
}
impl WatchesWindow {/*asdqwe
    fn clear_tree(&mut self) {
        self.tree.clear();
        self.eval_state.clear();
    }

    fn eval_locals(&mut self, context: &EvalContext, root_idx: ValueTreeNodeIdx) {
        let root = &mut self.tree.nodes[root_idx.0];
        root.has_children = Some(true);
        root.children = Some(0..0);
        /*
        asdqwe;
        let (dwarf_context, _) = match self.eval_state.make_local_dwarf_eval_context(context, context.selected_subframe) {
            Ok(x) => x,
            Err(e) => {
                self.tree.add(ValueTreeNode {value: Err(e), ..Default::default()});
                return;
            }
        };
        let symbols = dwarf_context.symbols.unwrap();
        let subframe = &context.stack.subframes[context.selected_subframe];
        let pseudo_addr = context.stack.frames[subframe.frame_idx].pseudo_addr;
        let static_pseudo_addr = dwarf_context.addr_map.dynamic_to_static(pseudo_addr);

        let mut idxs_per_name: HashMap<&str, usize> = HashMap::new();
        for v in drarf_context.local_variables {
            if !v.range().contains(&(static_pseudo_addr)) {
                continue;
            }
            let (value, dubious) = match eval_dwarf_expression(v.expr, &dwarf_context) {
                Ok((val, dub)) => (Ok(val), dub),
                Err(e) => (Err(e), false),
            };
            let name = unsafe {v.name()};
            let idx_per_name = *idxs_per_name.entry(name).and_modify(|x| *x += 1).or_insert(1) - 1;
            self.tree.add(ValueTreeNode {name, value: value.map(|val| Value {val, type_: v.type_, flags: ValueFlags::empty()}), dubious, identity: hash(&(name, idx_per_name)), ..Default::default()});
        }*/
    }

    fn eval_registers(&mut self, context: EvalContext, root_idx: ValueTreeNodeIdx) {
        let root = &mut self.tree.nodes[root_idx.0];
        root.has_children = Some(true);
        root.children = Some(0..0);
        //asdqwe
    }

    fn eval_watches(&mut self, context: &EvalContext, ui: &mut UI) {
        for &(identity, ref expr) in &self.expressions {
            styled_write!(self.tree.text, ui.palette.default, "{}", expr);
            let l = text.close_line();
            let name = self.tree.text.split_by_newline_character(l, None);
            let root_idx = self.tree.add(ValueTreeNode {name, identity, ..D!()});
            let root = &mut self.tree.nodes[root_idx.0];

            if expr.is_empty() {
                root.has_children = Some(false);
            } else if expr == "<locals>" {
                self.eval_locals(context, root);
            } else if expr == "<registers>" {
                self.eval_registers(context, root);
            } else {
                match eval_watch_expression(expr, &mut self.eval_state, context) {
                    Ok((val, dub)) => {
                        root.value = val;
                        root.dubious = dub;
                    }
                    Err(e) => root.value = Err(e),
                }
            }
        }
    }
*/
    /*asdqwe
    fn populate_lines(&mut self, context: Option<&EvalContext>, palette: &Palette) {
        self.lines.clear();
        let mut max_cursor_match = 0usize;
        self.scroll.cursor = 0;
        // DFS.
        for root_idx in self.tree.roots.clone() {
            let mut stack: Vec<usize> = vec![root_idx];
            while let Some(idx) = stack.pop() {
                let node = &self.tree.nodes[idx];

                if node.depth + 1 > max_cursor_match && self.cursor_path.get(node.depth) == Some(&node.identity) {
                    max_cursor_match = node.depth + 1;
                    self.scroll.cursor = self.lines.len();
                }

                let expanded = self.expanded_nodes.contains(&node.identity);
                self.lines.push((idx, expanded));

                if !expanded {
                    // Skip children.
                    continue;
                }

                if node.formatted_line[1].is_none() && node.value.is_ok() && context.is_some() {
                    // List children.
                    let (expandable, children) = format_value(node.value.as_ref().unwrap(), true, &mut self.eval_state, context.as_ref().unwrap(), &mut self.tree.arena, &mut self.tree.text, palette);
                    let start_idx = self.tree.nodes.len();
                    let node = &mut self.tree.nodes[idx];
                    node.formatted_line[1] = Some(self.tree.text.num_lines());
                    node.expandable = Some(expandable);
                    node.children = start_idx..start_idx+children.len();
                    let dubious = node.dubious;
                    self.tree.text.close_line();
                    for (name, identity, value) in children {
                        self.tree.add(ValueTreeNode {name, value, dubious, identity, parent: idx, ..Default::default()});
                    }
                }

                let node = &self.tree.nodes[idx];
                for c in node.children.clone().rev() {
                    stack.push(c);
                }
            }
        }
    }

    fn populate_tree_with_placeholders(&mut self) {
        if self.is_locals_window {
            self.tree.add(ValueTreeNode {value: err!(ValueTreePlaceholder, ""), ..Default::default()});
        } else {
            for (identity, expr) in &self.expressions {
                let name = self.tree.arena.add_str(expr);
                self.tree.add(ValueTreeNode {name, identity: *identity, value: err!(ValueTreePlaceholder, ""), ..Default::default()});
            }
        }
    }

    fn delete_watch(&mut self, identity: usize) {
        assert!(!self.is_locals_window);
        let i = match self.expressions.iter().position(|(id, _)| *id == identity) {
            None => return,
            Some(x) => x };
        self.expressions.remove(i);
        if !self.tree.roots.is_empty() {
            self.tree.roots.remove(i);

            if self.cursor_path.get(0) == Some(&identity) && !self.tree.roots.is_empty() {
                let i = i.min(self.tree.roots.len() - 1);
                self.cursor_path = vec![self.tree.nodes[self.tree.roots[i]].identity];
            }
        }
    }*/
}

impl WindowContent for WatchesWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {/*asdqwe
        // Plan:
        //  1. Handle expand/collapse/edit-start/edit-stop inputs using last frame rows and tree.
        //  2. Recalculate the tree if needed.
        //  3. Build widget tree and rows list.
        //     For nodes whose height isn't known asdqwe
        //     For nodes that affect layout of other nodes (expanded nodes, text input node) the widgets are created immediately.
        //     For other nodes, widget creation is deferred to a later stage, after layout, to avoid building invisible nodes.
        //     Locate cursor (map cursor_path to cursor_idx). Build TextInput, close it on focus loss.
        //  4. layout_subtree(). Now we know the heights and coordinates of all rows.
        //  5. Cursor navigation.
        //  6. Scrolling navigation. Now we know which rows are visible.
        //  7. Build the visible rows and the selected row.
        //  8. Focus and highlight selected row.

        styled_write!(ui.text, ui.palette.default, "{}", if self.is_locals_window {"locals"} else {"watches"});
        let l = ui.text.close_line();
        ui.add(widget!().text(l));
        /*asdqwe
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
                    Some(&(idx, _)) => {
                        let node = &self.tree.nodes[idx];
                        if node.expandable == Some(true) {
                            self.expanded_nodes.insert(node.identity);
                            refresh_lines = true;
                        }
                    }
                    _ => (),
                }
                Some(KeyAction::CursorLeft) => match self.lines.get(self.scroll.cursor) {
                    Some(&(idx, expanded)) => {
                        let node = &self.tree.nodes[idx];
                        if expanded {
                            self.expanded_nodes.remove(&node.identity);
                        } else if node.depth > 0 {
                            self.expanded_nodes.remove(&self.tree.nodes[node.parent].identity);
                        };
                        refresh_lines = true;
                    }
                    None => (),
                }
                Some(KeyAction::Enter) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(idx, _)) => {
                        let node = &self.tree.nodes[idx];
                        if node.depth == 0 {
                            if let Some((_, text)) = self.expressions.iter().find(|(id, _)| id == &node.identity) {
                                self.text_input = Some((node.identity, Rect::default(), TextInput::with_text(text.clone())));
                            }
                        }
                    }
                    _ => (),
                }
                Some(KeyAction::DeleteRow) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(idx, _)) => {
                        let node = &self.tree.nodes[idx];
                        if node.depth == 0 {
                            self.delete_watch(node.identity);
                            refresh_lines = true;
                        }
                    }
                    _ => (),
                }
                Some(KeyAction::DuplicateRow) if !self.is_locals_window => match self.lines.get(self.scroll.cursor) {
                    Some(&(idx, _)) => {
                        let node = &self.tree.nodes[idx];
                        if node.depth == 0 {
                            if let Some(i) = self.expressions.iter().position(|(id, _)| id == &node.identity) {
                                let id = self.next_watch_id;
                                let s = self.expressions[i].1.clone();
                                self.next_watch_id += 1;
                                self.expressions.insert(i + 1, (id, s));
                                self.cursor_path = vec![id];
                                self.clear_tree();
                            }
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
        match debugger.threads.get(&state.selected_thread) {
            None => unavailable_reason = Some("<no process>".to_string()),
            Some(thread) => if thread.state != ThreadState::Suspended || state.stack.frames.is_empty() {
                unavailable_reason = Some("<running>".to_string());
            } else {
                let t = (state.selected_thread, state.selected_subframe, thread.stop_count);
                if t != self.seen {
                    if self.is_locals_window && (t.0, t.1) != (self.seen.0, self.seen.1) {
                        //self.expanded_nodes.clear();
                    }
                    self.seen = t;
                    refresh_values = true;
                }
                context = Some(debugger.make_eval_context(&state.stack, state.selected_subframe));
            }
        }

        if !self.expressions.last().is_some_and(|(_, s)| s.is_empty()) {
            // Add the "<add watch>" placeholder watch.
            self.expressions.push((self.next_watch_id, String::new()));
            self.cursor_path = vec![self.next_watch_id];
            self.next_watch_id += 1;
            self.clear_tree();
        }

        if context.is_some() && (refresh_values || self.tree.nodes.is_empty()) {
            let context = context.as_ref().unwrap();
            self.clear_tree();
            self.eval_state.update(context);
            if self.is_locals_window {
                self.eval_locals(context);
            } else {
                self.eval_watches(context);
            }
            refresh_lines = true;
        } else if self.tree.nodes.is_empty() {
            self.populate_tree_with_placeholders();
            self.seen.2 = usize::MAX;
            refresh_lines = true;
        }

        if refresh_lines {
            self.populate_lines(context.as_ref(), palette);
        }

        let mut no_keys = Vec::new();
        let range = self.scroll.update(self.lines.len(), area.height.saturating_sub(1), if self.text_input.is_none() {&mut keys} else {&mut no_keys}, &debugger.context.settings.keys);
        self.cursor_path.clear();
        if let Some(&(mut idx, _)) = self.lines.get(self.scroll.cursor) {
            while idx != usize::MAX {
                let node = &self.tree.nodes[idx];
                self.cursor_path.push(node.identity);
                idx = node.parent;
            }
            self.cursor_path.reverse();
        };

        let name_column_width = area.width / 3;
        let mut rows: Vec<Row> = Vec::new();
        for line_idx in range.clone() {
            let &(idx, expanded) = &self.lines[line_idx];
            let node = &mut self.tree.nodes[idx];
            let expanded = expanded && node.expandable == Some(true);
            if node.formatted_line[expanded as usize].is_none() && context.is_some() && node.value.is_ok() {
                assert!(!expanded); // (if this doesn't hold, we have to also populate node.children here)
                let (expandable, _) = format_value(node.value.as_ref().unwrap(), expanded, &mut self.eval_state, context.as_ref().unwrap(), &mut self.tree.arena, &mut self.tree.text, palette);
                node.expandable = Some(expandable);
                node.formatted_line[expanded as usize] = Some(self.tree.text.num_lines());
                self.tree.text.close_line();
            }
            let node = &self.tree.nodes[idx];
            // Is this the "<add watch>" line at the end.
            let is_add_watch_placeholder = !self.is_locals_window && node.depth == 0 && self.expressions.last().is_some_and(|(id, s)| s.is_empty() && *id == node.identity);

            // Indentation, expansion arrow, name.
            let mut spans = vec![Span::styled("┆".to_string().repeat(node.depth), palette.default_dim)];
            if node.expandable.unwrap_or(expanded) {
                spans.push(Span::raw(if expanded {"▾ "} else {"▸ "}));
            } else {
                spans.push(Span::raw("  "));
            }
            spans.push(Span::raw(if is_add_watch_placeholder {"<add watch>".to_string()} else {node.name.to_string()}));
            // Dim the name if parent's value is dubious or the existence of this item comes from cache.
            if (node.depth > 0 && self.tree.nodes[node.parent].dubious) || (context.is_none() && (node.depth > 0 || self.is_locals_window)) || is_add_watch_placeholder {
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
                if node.depth == 0 && *id == node.identity {
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

    fn update_modal(&mut self, state: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
        if let Some((identity, _, input)) = &mut self.text_input {
            let event = input.update(keys, &debugger.context.settings.keys);
            match event {
                TextInputEvent::None | TextInputEvent::Open => (),
                TextInputEvent::Cancel => self.text_input = None,
                TextInputEvent::Done => {
                    if let Some(i) = self.expressions.iter().position(|(id, _)| id == identity) {
                        if input.text.is_empty() {
                            if i + 1 != self.expressions.len() {
                                let identity = *identity;
                                self.delete_watch(identity);
                                self.clear_tree(); // some kind of populate_lines() would be enough, but meh
                            }
                        } else {
                            self.expressions[i].1 = input.text.clone();
                            if i + 1 < self.expressions.len() {
                                self.cursor_path = vec![self.expressions[i+1].0];
                            }
                            self.clear_tree();
                        }
                    }
                    self.text_input = None;
                }
            }
        }
    }

    fn render_modal(&mut self, state: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {
        if let Some((_, area, input)) = &mut self.text_input {
            f.render_widget(Clear, *area);
            input.render(f, *area, &debugger.context.settings.palette);
        }
    }

    fn cancel_modal(&mut self, state: &mut UIState, debugger: &mut Debugger) {
        self.text_input = None;
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {}
        styled_write!(hints, ui.palette.default_dim, "ret - edit"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, "d - duplicate"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, "backspace - delete"); hints.close_line();
    }

    fn has_persistent_state(&self) -> bool {
        !self.is_locals_window
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        out.write_usize(self.expressions.len())?;
        for (_, expr) in &self.expressions {
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
    }*/ */
    }
}

#[derive(Default)]
struct LocationsWindow {
    table_state: TableState,
}
impl WindowContent for LocationsWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        styled_write!(ui.text, ui.palette.default, "locations");
        let l = ui.text.close_line();
        ui.add(widget!().text(l));
        /*
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

        f.render_widget(table, area);*/
    }
}

impl LocationsWindow {/*asdqwe
    fn fill_table(&mut self, state: &mut UIState, debugger: &Debugger) -> Result<Vec<Row<'static>>> {
        let palette = &debugger.context.settings.palette;
        let (binary_id, function_idx, addr) = match state.selected_addr.clone() { Some(x) => x, None => return Ok(Vec::new()) };
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
                    let mut text = StyledText::default();
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
    }*/
}

// Identifying information about a function, suitable for writing to the save file.
// We want both more specific and less specific information here.
// Less specific (e.g. name) to be likely to find the function if the binary changed a little.
// Specific (e.g. address) to be able to find the exactly correct function if the binary hasn't changed.
struct DisassemblyFunctionLocator {
    binary_id: BinaryId,
    mangled_name: Vec<u8>,
    demangled_name: String,
    addr: FunctionAddr,
}

#[derive(Default)]
struct DisassemblyTab {
    title: String,
    identity: usize,
    ephemeral: bool,

    // Cases:
    //  * locator is None, error is Some - couldn't find function for current address; this is a tab for the error message about that,
    //  * locator is Some, error is Some - couldn't find function, but may try again later (drop_caches() unsets error),
    //  * locator is Some, error is None, cached_function_idx is None - didn't try finding the function yet (loaded from save file) or the Symbols was deloaded,
    //  * locator is Some, error is None, cached_function_idx is Some - found the function is Symbols.
    // The symbols_identity needs to be re-checked before using the function_idx, in case Symbols gets reloaded for the same BinaryId (which invalidates function_idx due to nondeterminism).
    locator: Option<DisassemblyFunctionLocator>,
    error: Option<Error>,
    cached_function_idx: Option<(/*symbols_identity*/ usize, /*function_idx*/ usize)>,

    // When moving cursor through disassembly, we automatically scroll source code to the corresponding line.
    // But if the cursor is inside some inlined functions, which of them do we scroll to? This depth number determines that.
    // This indentation level is highlighted.
    selected_subfunction_level: u16,

    area_state: AreaState,
}

struct DisassemblyWindow {
    tabs: Vec<DisassemblyTab>,
    cache: HashMap<(/*symbols_identity*/ usize, /*function_idx*/ usize), Disassembly>,
    tabs_state: TabsState,
    search_dialog: Option<SearchDialog>,
    source_scrolled_to: Option<(/*symbols_identity*/ usize, /*function_idx*/ usize, /*disas_line*/ usize, /*selected_subfunction_level*/ u16)>,
}

impl Default for DisassemblyWindow { fn default() -> Self { Self {tabs: Vec::new(), cache: HashMap::new(), tabs_state: TabsState::default(), search_dialog: None, source_scrolled_to: None} } }

impl DisassemblyWindow {
    fn open_function(&mut self, target: Result<DisassemblyScrollTarget>, debugger: &Debugger) -> Result<()> {
        let target = match target {
            Ok(x) => x,
            Err(e) => {
                self.tabs.push(DisassemblyTab {identity: random(), error: Some(e), title: "[unknown]".to_string(), ephemeral: true, ..Default::default()});
                self.tabs_state.select(self.tabs.len() - 1);
                return Ok(());
            }
        };
        for i in 0..self.tabs.len() {
            if let Some((binary, function_idx)) = self.resolve_function_for_tab(i, debugger) {
                if binary.id == target.binary_id && function_idx == target.function_idx && binary.symbols.as_ref().unwrap().identity == target.symbols_identity {
                    self.tabs_state.select(i);
                    return Ok(());
                }
            }
        }

        let binary = Self::find_binary(&target.binary_id, debugger)?;
        let symbols = binary.symbols.as_ref_clone_error()?;
        let function = &symbols.functions[target.function_idx];
        let demangled_name = function.demangle_name();
        let title = Self::make_title(&demangled_name);
        self.tabs.push(DisassemblyTab {
            identity: random(), locator: Some(DisassemblyFunctionLocator {binary_id: binary.id.clone(), mangled_name: function.mangled_name().to_owned(), addr: function.addr, demangled_name}),
            cached_function_idx: Some((symbols.identity, target.function_idx)), title, ephemeral: true, selected_subfunction_level: u16::MAX, ..Default::default()});
        self.tabs_state.select(self.tabs.len() - 1);

        Ok(())
    }

    fn make_title(demangled_name: &str) -> String {
        if demangled_name.len() <= 30 {
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

    fn find_binary<'a>(binary_id: &BinaryId, debugger: &'a Debugger) -> Result<&'a BinaryInfo> {
        Ok(if let Some(binary) = debugger.info.binaries.get(binary_id) {
            binary
        } else if let Some(binary) = debugger.symbols.get_if_present(binary_id) {
            binary
        } else if let Some((_, binary)) = debugger.info.binaries.iter().find(|(id, _)| id.matches_incomplete(binary_id)) {
            binary
        } else if let Some((_, binary)) = debugger.info.binaries.iter().find(|(id, _)| id.matches_incomplete(binary_id)) {
            binary
        } else {
            return err!(NoFunction, "binary not mapped: {}", binary_id.path);
        })
    }
    
    fn resolve_function_for_tab<'a>(&mut self, tab_idx: usize, debugger: &'a Debugger) -> Option<(&'a BinaryInfo, /*function_idx*/ usize)> {
        let tab = &mut self.tabs[tab_idx];
        if tab.error.is_some() {
            return None;
        }
        let locator = match &mut tab.locator {
            None => return None,
            Some(x) => x };

        // If binary is mapped, get BinaryInfo from debugger.info, where it has AddrMap. Otherwise take it from SymbolsLoader, and disassembly will show static addresses.
        let binary = match Self::find_binary(&locator.binary_id, debugger) {
            Err(e) => {
                tab.error = Some(e);
                return None;
            }
            Ok(x) => x };
        locator.binary_id = binary.id.clone();

        let symbols = match binary.symbols.as_ref() {
            Ok(x) => x,
            Err(e) => {
                tab.error = Some(e.clone());
                return None;
            }
        };

        if let &Some((symbols_identity, function_idx)) = &tab.cached_function_idx {
            if symbols_identity == symbols.identity {
                return Some((binary, function_idx));
            }
            tab.cached_function_idx = None;
        }

        let function_idx = match symbols.find_nearest_function(&locator.mangled_name, locator.addr) {
            Some(x) => x,
            None => {
                tab.error = Some(error!(NoFunction, "function not found: {}", locator.demangled_name));
                return None;
            }
        };

        let function = &symbols.functions[function_idx];
        locator.mangled_name = function.mangled_name().to_vec();
        locator.demangled_name = function.demangle_name();
        locator.addr = function.addr;

        Some((binary, function_idx))
    }

    fn find_or_disassemble_function<'a>(cache: &'a mut HashMap<(usize, usize), Disassembly>, binary: &BinaryInfo, function_idx: usize, palette: &Palette) -> &'a Disassembly {
        let indent_width = str_width(&palette.tree_indent.0);
        let e = cache.entry((binary.symbols.as_ref().unwrap().identity, function_idx));
        match e {
            Entry::Occupied(o) if o.get().indent_width == indent_width => o.into_mut(),
            _ => {
                // Would be nice to also support disassembling arbitrary memory, regardless of functions or binaries. E.g. for JIT-generated code.
                let d = match Self::disassemble_function(binary, function_idx, palette) {
                    Ok(d) => d,
                    Err(e) => Disassembly::new().with_error(e, palette),
                };
                match e {
                    Entry::Occupied(mut o) => {
                        *o.get_mut() = d;
                        o.into_mut()
                    }
                    Entry::Vacant(v) => v.insert(d),
                }
            }
        }
    }

    // Would be nice to also support disassembling arbitrary memory, regardless of functions or binaries. E.g. for JIT-generated code.
    fn disassemble_function(binary: &BinaryInfo, function_idx: usize, palette: &Palette) -> Result<Disassembly> {
        let symbols = binary.symbols.as_ref().unwrap();
        let ranges = symbols.function_addr_ranges(function_idx);
        let function = &symbols.functions[function_idx];

        let mut prelude = StyledText::default();
        styled_write!(prelude, palette.default_dim, "{}", binary.id.path);
        prelude.close_line();
        match function.debug_info_offset() {
            Some(off) => styled_write!(prelude, palette.default_dim, "dwarf offset: 0x{:x}", off.0),
            None => styled_write!(prelude, palette.default_dim, "function from symtab"),
        }
        prelude.close_line();
        prelude.close_line();
        // TODO: Print declaration site. Scroll code window to it when selected.
        // TODO: Print number of inlined call sites. Allow setting breakpoint on it.

        Ok(disassemble_function(function_idx, ranges, Some(symbols.as_ref()), None, prelude, palette))
    }

    fn close_error_tab(&mut self) -> Option<usize> {
        if self.tabs.is_empty() {
            return None;
        }
        if let Some(i) = self.tabs.iter().position(|t| t.locator.is_none()) {
            self.tabs.remove(i);
            if self.tabs_state.selected == i {
                return None;
            }
            self.tabs_state.closed(i);
        }
        Some(self.tabs_state.selected)
    }

    fn evict_cache(&mut self) {
        if self.cache.len().saturating_sub(self.tabs.len()) > 100 {
            let mut in_use: HashSet<(usize, usize)> = HashSet::new();
            for t in &self.tabs {
                if let Some(x) = t.cached_function_idx.clone() {
                    in_use.insert(x);
                }
            }
            let mut i = 0;
            self.cache.retain(|k, _| {
                if in_use.contains(k) {
                    true
                } else {
                    i += 1;
                    i % 2 == 0
                }
            });
        }
    }

    fn build_search_dialog(&mut self, create: bool, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        let dialog_widget = match make_dialog_frame(create, AutoSize::Remainder(0.75), AutoSize::Remainder(0.83), ui.palette.dialog, ui.palette.function_name, "find function (by mangled name)", ui) {
            None => {
                self.search_dialog = None;
                return;
            }
            Some(x) => x };

        let d = self.search_dialog.get_or_insert_with(|| SearchDialog::new(Arc::new(FunctionSearcher), debugger.context.clone()));
        with_parent!(ui, dialog_widget, {
            d.build(&debugger.symbols, Some(debugger.info.binaries.keys().cloned().collect()), ui);
        });

        if d.should_close_dialog {
            ui.close_dialog();
        }

        if let Some(res) = mem::take(&mut d.should_open_document) {
            match self.open_function(Ok(DisassemblyScrollTarget {binary_id: res.binary, symbols_identity: res.symbols.identity, function_idx: res.id, static_pseudo_addr: 0, subfunction_level: u16::MAX}), debugger) {
                Ok(()) => self.tabs[self.tabs_state.selected].ephemeral = false,
                Err(e) => log!(debugger.log, "{}", e),
            }
        }
    }
}

impl WindowContent for DisassemblyWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        // First do things that may open or close tabs.

        let mut open_dialog = false;
        for action in ui.check_keys(&[KeyAction::Open, KeyAction::CloseTab]) {
            match action {
                KeyAction::Open if self.search_dialog.is_none() => {
                    open_dialog = true;
                }
                KeyAction::CloseTab if self.tabs.get(self.tabs_state.selected).is_some() => {
                    let tab = &mut self.tabs[self.tabs_state.selected];
                    if tab.ephemeral {
                        tab.ephemeral = false;
                    } else {
                        self.tabs.remove(self.tabs_state.selected);
                    }
                }
                _ => (),
            }
        }

        self.build_search_dialog(open_dialog, state, debugger, ui);

        let mut scroll_to_addr: Option<(usize, u16)> = None;
        let suppress_code_autoscroll = state.should_scroll_disassembly.is_some();
        if let Some((target, only_if_on_error_tab)) = mem::take(&mut state.should_scroll_disassembly) {
            let mut tab_to_restore: Option<usize> = None;
            if only_if_on_error_tab {
                tab_to_restore = self.close_error_tab();
            }

            if let Ok(target) = &target {
                if tab_to_restore.is_none() {
                    scroll_to_addr = Some((target.static_pseudo_addr, target.subfunction_level));
                } else {
                    // (Not ideal that we don't scroll when the tab is not selected, but meh.)
                }
            }

            self.open_function(target, debugger).unwrap();

            if let Some(i) = tab_to_restore {
                self.tabs_state.selected = i;
            }
        }

        close_excess_ephemeral_tabs(&mut self.tabs, &mut self.tabs_state, |t| t.ephemeral);
        self.evict_cache();

        // Now the set of tabs is final.

        ui.cur_mut().set_vstack();
        with_parent!(ui, ui.add(widget!().fixed_height(1)), {
            ui.focus();
            let mut tabs = Tabs::new(mem::take(&mut self.tabs_state), ui);
            for tab in &self.tabs {
                let full_title = match &tab.locator {
                    Some(locator) => locator.demangled_name.clone(),
                    None => String::new(),
                };
                tabs.add(Tab {identity: tab.identity, short_title: tab.title.clone(), full_title, ephemeral: tab.ephemeral, ..Default::default()}, ui);
            }
            self.tabs_state = tabs.finish(ui);
        });
        let content_root = ui.add(widget!().height(AutoSize::Remainder(1.0)));
        with_parent!(ui, content_root, {
            ui.multifocus();
        });
        ui.layout_children(Axis::Y);

        // Now the selected tab is final.

        // (We'll reassign it below if there's no error.)
        state.selected_addr = state.stack.frames.get(state.selected_frame).and_then(|f| {
            let sf = &state.stack.subframes[f.subframes.end-1];
            match (&f.binary_id, &sf.function_idx) {
                (Some(b), Ok(function_idx)) => Some((b.clone(), *function_idx, f.pseudo_addr)),
                _ => None,
            }
        });

        let tab = match self.tabs.get_mut(self.tabs_state.selected) {
            None => return,
            Some(x) => x };

        let start = ui.text.num_lines();
        if let Some(locator) = &tab.locator {
            ui_writeln!(ui, function_name, "{}", locator.demangled_name);
        }

        let r = self.resolve_function_for_tab(self.tabs_state.selected, debugger);
        let tab = self.tabs.get_mut(self.tabs_state.selected).unwrap();
        let (binary, function_idx) = match r {
            None => {
                ui_writeln!(ui, error, "{}", tab.error.as_ref().unwrap());
                let end = ui.text.num_lines();
                with_parent!(ui, content_root, {
                    // Horizontally scrollable error message.
                    build_biscrollable_area_with_header(start..end, [0, 0], &mut tab.area_state, ui);
                });
                return;
            }
            Some(x) => x };

        let disas = Self::find_or_disassemble_function(&mut self.cache, binary, function_idx, &ui.palette);

        if let Some((static_pseudo_addr, subfunction_level)) = scroll_to_addr {
            if let Some(line) = disas.static_pseudo_addr_to_line(static_pseudo_addr) {
                tab.selected_subfunction_level = subfunction_level;
                tab.area_state.select(line);
            }
        }

        let rel_addr_digits = (((disas.max_abs_relative_addr as f64 + 1.0).log2() / 4.0).ceil() as usize).max(1); // how many hex digits to use in the "<+1abc>" things
        let prefix_width = 2 + 2 + 12+1 + rel_addr_digits+4 + 2 + 1; // ip, breakpoint, addr, rel_addr, jump_indicator, space

        let end = ui.text.num_lines();
        let (content, visible_y) = with_parent!(ui, content_root, {
            build_biscrollable_area_with_header(start..end, [prefix_width + disas.widest_line, disas.lines.len()], &mut tab.area_state, ui)
        });

        // Now the cursor position is final.

        let symbols = binary.symbols.as_ref().unwrap();
        let function = &symbols.functions[function_idx];
        assert_eq!(disas.symbols_shard, Some(function.shard_idx()));
        let symbols_shard = &symbols.shards[function.shard_idx()];

        let mut selected_subfunction_idx: Option<usize> = None;
        let mut source_line: Option<SourceScrollTarget> = None;

        if let Some(disas_line) = disas.lines.get(tab.area_state.cursor) {
            let has_addr = disas_line.static_addr != 0 && disas_line.static_addr != usize::MAX;
            if has_addr {
                state.selected_addr = Some((binary.id.clone(), function_idx, binary.addr_map.static_to_dynamic(disas_line.static_addr)));
            }

            for action in ui.check_keys(&[KeyAction::ToggleBreakpoint, KeyAction::PreviousMatch, KeyAction::NextMatch]) {
                match action {
                    KeyAction::ToggleBreakpoint if has_addr => {
                        // TODO: Instruction breakpoints. Probably store static addr + binary id, not dynamic addr. But make it work without requiring Symbols, just based on mmaps.
                    }
                    KeyAction::PreviousMatch => tab.selected_subfunction_level = tab.selected_subfunction_level.saturating_sub(1).min(disas_line.subfunction_level),
                    KeyAction::NextMatch => {
                        tab.selected_subfunction_level = tab.selected_subfunction_level.saturating_add(1);
                        if tab.selected_subfunction_level > disas_line.subfunction_level {
                            tab.selected_subfunction_level = u16::MAX;
                        }
                    }
                    _ => (),
                }
            }

            let mut source_line_info = disas_line.leaf_line.clone();
            if let &Some(mut sf_idx) = &disas_line.subfunction {
                while symbols_shard.subfunctions[sf_idx].level > tab.selected_subfunction_level {
                    sf_idx = symbols_shard.subfunctions[sf_idx].parent;
                }
                if symbols_shard.subfunctions[sf_idx].level == tab.selected_subfunction_level {
                    selected_subfunction_idx = Some(sf_idx);
                    source_line_info = Some(symbols_shard.subfunctions[sf_idx].call_line.clone());
                }
            }
            if let Some(line) = source_line_info {
                let file = &symbols.files[line.file_idx().unwrap()];
                source_line = Some(SourceScrollTarget {path: file.path.to_owned(), version: file.version.clone(), line: line.line()});
            }
        }
        let key = (symbols.identity, function_idx, tab.area_state.cursor, tab.selected_subfunction_level);
        if !suppress_code_autoscroll && self.source_scrolled_to.as_ref() != Some(&key) {
            self.source_scrolled_to = Some(key);
            if let Some(target) = source_line {
                state.should_scroll_source = Some((Some(target), false));
                ui.should_redraw = true;
            }
        }

        let mut ip_lines: Vec<(usize, /*selected*/ bool)> = Vec::new();
        for (idx, frame) in state.stack.frames.iter().enumerate() {
            if frame.binary_id.as_ref() == Some(&binary.id) {
                let static_pseudo_addr = frame.pseudo_addr.wrapping_sub(frame.addr_static_to_dynamic);
                if let Some(line) = disas.static_pseudo_addr_to_line(static_pseudo_addr) {
                    ip_lines.push((line, idx == state.selected_frame));
                }
            }
        }
        ip_lines.sort_unstable_by_key(|k| (k.0, !k.1));

        with_parent!(ui, content, {
            let line_range = visible_y.start.max(0) as usize .. (visible_y.end.max(0) as usize).min(disas.lines.len());
            for i in line_range.clone() {
                let line = &disas.lines[i];

                if line.kind == DisassemblyLineKind::Instruction {
                    let addr = binary.addr_map.static_to_dynamic(line.static_addr);

                    // (Comparing line number instead of address because the "instruction pointer" pseudoaddress may be in between instructions.)
                    let ip_idx = ip_lines.partition_point(|x| x.0 < i);
                    if ip_idx == ip_lines.len() || ip_lines[ip_idx].0 != i {
                        ui_write!(ui, default, "  ");
                    } else if ip_lines[ip_idx].1 {
                        ui_write!(ui, instruction_pointer, "⮕ ");
                    } else {
                        ui_write!(ui, additional_instruction_pointer, "⮕ ");
                    }

                    let loc_idx = debugger.breakpoint_locations.partition_point(|loc| loc.addr < addr);
                    let mut marker = "  ";
                    if line.kind == DisassemblyLineKind::Instruction && loc_idx < debugger.breakpoint_locations.len() {
                        let loc = &debugger.breakpoint_locations[loc_idx];
                        if loc.addr == addr && loc.breakpoints.iter().any(|b| match b { BreakpointRef::Id {..} => true, BreakpointRef::Step(_) => false }) {
                            marker = if loc.active { "● " } else { "○ " }
                        }
                    }
                    ui_write!(ui, secondary_breakpoint, "{}", marker);

                    ui_write!(ui, default_dim, "{:012x} ", addr);
                    ui_write!(ui, disas_relative_address, "<{: >+1$x}> ", line.relative_addr, rel_addr_digits + 1);
                    ui_write!(ui, disas_jump_arrow, " {} ", line.jump_indicator);

                    assert_eq!(ui.text.unclosed_line_width(), prefix_width);
                } else if line.kind != DisassemblyLineKind::Intro {
                    ui_write!(ui, default, "{:1$}", "", prefix_width);
                }

                // Reserve space for indentation. These characters won't be visible, we'll cover them with clickable vertical-line widgets below.
                if line.subfunction_level != 0 {
                    ui_write!(ui, default, "{:1$}", "", line.subfunction_level as usize * disas.indent_width);
                }

                let l = ui.text.import_lines(&disas.text, i..i+1).start;

                let mut w = widget!().identity(&('l', i)).fixed_height(1).fixed_y(i as isize).text(l).fill(' ', ui.palette.default).flags(WidgetFlags::HSCROLL_INDICATOR_RIGHT).highlight_on_hover();
                if i == tab.area_state.cursor {
                    w.style_adjustment.update(ui.palette.selected);
                }
                ui.add(w);
            }

            // Draw indentation lines.
            let mut change_subfunction_level = tab.selected_subfunction_level;
            for level in 1.. {
                let mut found = false;
                let mut start = line_range.start;
                while start < line_range.end {
                    let line = &disas.lines[start];
                    if line.subfunction_level < level {
                        start += 1;
                        continue;
                    }
                    found = true;

                    let line_sf_at_cur_level = |line: usize| -> usize {
                        let mut idx = disas.lines[line].subfunction.clone().unwrap();
                        while symbols_shard.subfunctions[idx].level > level {
                            idx = symbols_shard.subfunctions[idx].parent;
                        }
                        idx
                    };

                    let cur_sf = line_sf_at_cur_level(start);
                    let mut end = start + 1;
                    while end < line_range.end && disas.lines[end].subfunction_level >= level && line_sf_at_cur_level(end) == cur_sf {
                        end += 1;
                    }

                    let is_selected = selected_subfunction_idx == Some(cur_sf);
                    let (symbol, style) = if is_selected {&ui.palette.tree_indent_selected} else {&ui.palette.tree_indent};
                    for i in start..end {
                        let mut s = *style;
                        // Manually highlight selected row because there's no transparency.
                        if i == tab.area_state.cursor {
                            s = ui.palette.selected.apply(s);
                        }
                        styled_writeln!(ui.text, s, "{}", symbol);
                    }
                    let lines = ui.text.num_lines()-(end-start)..ui.text.num_lines();

                    let w = widget!().identity(&('i', start, level)).fixed_width(disas.indent_width).fixed_x(prefix_width as isize + level as isize - 1).fixed_y(start as isize).fixed_height(end - start).text_lines(lines).highlight_on_hover();
                    with_parent!(ui, ui.add(w), {
                        if ui.check_mouse(MouseActions::CLICK) {
                            change_subfunction_level = level;
                        }
                    });

                    start = end;
                }
                if !found {
                    break;
                }
            }
            if change_subfunction_level != tab.selected_subfunction_level {
                tab.selected_subfunction_level = change_subfunction_level;
                ui.should_redraw = true;
            }
        });
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.extend([
            KeyHint::key(KeyAction::Open, "find function"),
            KeyHint::key(KeyAction::CloseTab, "close/pin tab"),
            KeyHint::keys(&[KeyAction::PreviousMatch, KeyAction::NextMatch], "select level")]);
        // TODO: KeyAction::ToggleBreakpoint, KeyAction::DisableBreakpoint
    }

    fn has_persistent_state(&self) -> bool {
        true
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        for (idx, tab) in self.tabs.iter().enumerate() {
            let locator = match &tab.locator {
                None => continue,
                Some(x) => x };
            out.write_u8(if idx == self.tabs_state.selected {2} else {1})?;
            locator.binary_id.save_state_incomplete(out)?;
            out.write_slice(&locator.mangled_name)?;
            out.write_str(&locator.demangled_name)?; // can't just demangle on load because we don't know the language (alernatively we could save the language here)
            out.write_usize(locator.addr.0)?;
            tab.area_state.save_state(out)?;
            out.write_u16(tab.selected_subfunction_level)?;
            out.write_u8(tab.ephemeral as u8)?;
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
            self.tabs.push(DisassemblyTab {identity: random(), title, locator: Some(locator), error: None, area_state: AreaState::load_state(inp)?, selected_subfunction_level: inp.read_u16()?, ephemeral: inp.read_u8()? != 0, cached_function_idx: None});
            if select_this_tab {
                self.tabs_state.select(self.tabs.len() - 1);
            }
        }
        // TODO: Prevent scrolling code window before first input.
        Ok(())
    }

    fn drop_caches(&mut self) {
        self.cache.clear();
        for tab in &mut self.tabs {
            if tab.locator.is_some() {
                tab.error = None;
                tab.cached_function_idx = None;
            }
        }
    }
}

fn close_excess_ephemeral_tabs<T, F: FnMut(&T) -> bool>(tabs: &mut Vec<T>, tabs_state: &mut TabsState, mut is_ephemeral: F) {
    let ephemeral_tabs = tabs.iter().filter(|t| is_ephemeral(t)).count();
    if ephemeral_tabs <= 1 {
        return;
    }
    let mut i = 0;
    tabs.retain(|t| {
        if is_ephemeral(t) && i != tabs_state.selected {
            tabs_state.closed(i);
            false
        } else {
            i += 1;
            true
        }
    });
}

#[derive(Default)]
struct StatusWindow {}
impl WindowContent for StatusWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        ui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);

        let l = match debugger.target_state {
            ProcessState::NoProcess => ui_writeln!(ui, default_dim, "no process"),
            ProcessState::Starting => ui_writeln!(ui, state_other, "starting"),
            ProcessState::Exiting => ui_writeln!(ui, state_other, "exiting"),
            ProcessState::Stepping => ui_writeln!(ui, state_other, "stepping"),
            ProcessState::Running => ui_writeln!(ui, state_running, "running"),
            ProcessState::Suspended => ui_writeln!(ui, state_suspended, "suspended"),
        };
        let style = ui.text.spans.last().clone().unwrap().1;
        ui.add(widget!().height(AutoSize::Text).text(l).fill(' ', style));

        let start = ui.text.num_lines();
        if debugger.target_state != ProcessState::NoProcess {
            ui_write!(ui, default_dim, "pid: ");
            ui_write!(ui, default, "{}", debugger.pid);
            ui_writeln!(ui, default_dim, " cpu {:.0}% mem {}", debugger.info.resource_stats.cpu_percentage(), PrettySize(debugger.info.resource_stats.rss_bytes));
        } else {
            ui_writeln!(ui, default_dim, "pid: none");
        }
        ui_writeln!(ui, default_dim, "nnd pid: {} cpu {:.0}% mem {}", my_pid(), debugger.my_resource_stats.cpu_percentage(), PrettySize(debugger.my_resource_stats.rss_bytes));
        match &debugger.persistent.path {
            Ok(p) => ui_writeln!(ui, default_dim, "{}/", p.display()),
            Err(e) => ui_writeln!(ui, error, "{}", e),
        };

        if state.last_error != "" {
            ui_writeln!(ui, error, "{}", state.last_error);
        }
        ui.text.close_line();
        let end = ui.text.num_lines();

        ui.add(widget!().text_lines(start..end).height(AutoSize::Text));
        let log_widget = ui.add(widget!().height(AutoSize::Remainder(1.0)));
        ui.layout_children(Axis::Y);

        with_parent!(ui, log_widget, {
            let space_left = ui.cur().axes[Axis::Y].get_fixed_size();
            let log_lines = debugger.log.lines.len();
            let start = ui.text.num_lines();
            for line in &debugger.log.lines.make_contiguous()[log_lines.saturating_sub(space_left)..] {
                ui_writeln!(ui, default, "{}", line);
            }
            let end = ui.text.num_lines();
            ui.cur_mut().draw_text = Some(start..end);
        });
    }
}

#[derive(Default)]
struct HintsWindow {
    scroll: isize,
}

impl HintsWindow {
    const NORMAL_HEIGHT: usize = 8;
    const PROFILER_HEIGHT: usize = 17;

    fn build_profiling_charts(&mut self, debugger: &mut Debugger, ui: &mut UI) {
        ui.cur_mut().set_hstack();
        let viewport = ui.add(widget!().width(AutoSize::Remainder(1.0)));
        let scroll_bar = ui.add(widget!().fixed_width(1));
        ui.layout_children(Axis::X);
        let content_widget = ui.add(widget!().parent(viewport).fixed_y(0).height(AutoSize::Children).vstack());

        with_parent!(ui, content_widget, {
            styled_write!(ui.text, ui.palette.default, "frame: {:<10} widgets: {:<10} bucket: {:<10}", ui.frame_idx, ui.prev_tree.len(), PrettyDuration(debugger.context.settings.periodic_timer_seconds));
            let l = ui.text.close_line();
            ui.add(widget!().height(AutoSize::Text).text(l));

            enum Type {
                Count,
                Tsc,
                Bytes,
            }
            struct Chart {
                title: &'static [&'static str],
                data: Vec<usize>,
                type_: Type,
            }

            let chart_height = 2;
            let mut charts = [
                Chart {title: &["ui"     , "max"  ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["deb"    , "time" ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["deb"    , "count"], type_: Type::Count, data: Vec::new()},
                Chart {title: &["syscall", "time" ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["syscall", "count"], type_: Type::Count, data: Vec::new()},
                Chart {title: &["other"  , "time" ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["build"  , "max"  ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["render" , "max"  ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["fill"   , "max"  ], type_: Type::Tsc  , data: Vec::new()},
                Chart {title: &["input"  , "bytes"], type_: Type::Bytes, data: Vec::new()},
                Chart {title: &["output" , "bytes"], type_: Type::Bytes, data: Vec::new()},
            ];

            let title_width = charts.iter().map(|c| c.title.iter().map(|t| str_width(*t)).max().unwrap()).max().unwrap();
            let value_width = PrettyCount::MAX_LEN.max(PrettySize::MAX_LEN).max(PrettyDuration::MAX_LEN);
            let total_width = ui.cur().axes[Axis::X].get_fixed_size();
            let chart_width = total_width.saturating_sub(title_width + value_width + 2).max(1);
            for c in &mut charts {
                c.data = vec![0; chart_width];
            }

            let s_per_tsc = if debugger.prof.buckets.is_empty() {
                1.0
            } else {
                let a = &debugger.prof.buckets[debugger.prof.buckets.len().saturating_sub(chart_width)];
                let b = debugger.prof.buckets.back().unwrap();
                (b.end_time - a.start_time).as_secs_f64() / (b.end_tsc - a.start_tsc) as f64
            };

            for i in 0..chart_width.min(debugger.prof.buckets.len()) {
                let b = &debugger.prof.buckets[debugger.prof.buckets.len() - i - 1];
                let j = chart_width - i - 1;

                charts[ 0].data[j] = b.ui_max_tsc as usize;
                charts[ 1].data[j] = b.debugger_tsc as usize;
                charts[ 2].data[j] = b.debugger_count;
                charts[ 3].data[j] = b.syscall_tsc as usize;
                charts[ 4].data[j] = b.syscall_count;
                charts[ 5].data[j] = b.other_tsc as usize;
                charts[ 6].data[j] = b.ui_build_max_tsc as usize;
                charts[ 7].data[j] = b.ui_render_max_tsc as usize;
                charts[ 8].data[j] = b.ui_fill_max_tsc as usize;
                charts[ 9].data[j] = b.ui_input_bytes;
                charts[10].data[j] = b.ui_output_bytes;
            }

            let draw_hline = |left_corner: char, right_corner: char, ui: &mut UI| {
                with_parent!(ui, ui.add(widget!().fixed_height(1).hstack()), {
                    ui.add(widget!().fixed_width(1).fixed_x(title_width as isize).fill(left_corner, ui.palette.default));
                    ui.add(widget!().fixed_width(chart_width).fill('─', ui.palette.default));
                    ui.add(widget!().fixed_width(1).fill(right_corner, ui.palette.default));
                });
            };

            draw_hline('╭', '╮', ui);
            for idx in 0..charts.len() {
                if idx > 0 {
                    draw_hline('├', '┤', ui);
                }
                let chart = &charts[idx];
                let max = chart.data.iter().copied().max().unwrap();
                let max1 = max.max(1);
                with_parent!(ui, ui.add(widget!().fixed_height(chart_height).hstack()), {
                    let start = ui.text.num_lines();
                    for s in chart.title {
                        ui_writeln!(ui, default, "{}", s);
                    }
                    let end = ui.text.num_lines();
                    ui.add(widget!().fixed_width(title_width).text_lines(start..end));
                    ui.add(widget!().fixed_width(1).fill('│', ui.palette.default));

                    let start = ui.text.num_lines();
                    for i in 0..chart_height {
                        let h = (chart_height - i - 1) * 8;
                        for j in 0..chart_width {
                            let v = (chart.data[j] * (chart_height * 8) + max/2) / max1;
                            let c = match v {
                                v if v <= h   => ' ',
                                v if v == h+1 => '▁',
                                v if v == h+2 => '▂',
                                v if v == h+3 => '▃',
                                v if v == h+4 => '▄',
                                v if v == h+5 => '▅',
                                v if v == h+6 => '▆',
                                v if v == h+7 => '▇',
                                _             => '█',
                            };
                            ui.text.chars.push(c);
                        }
                        ui.text.close_span(ui.palette.default);
                        ui.text.close_line();
                    }
                    let end = ui.text.num_lines();
                    ui.add(widget!().fixed_width(chart_width).text_lines(start..end));

                    ui.add(widget!().fixed_width(1).fill('│', ui.palette.default));
                    let start = ui.text.num_lines();
                    for val in [max, *chart.data.last().unwrap()] {
                        match chart.type_ {
                            Type::Count => ui_writeln!(ui, default, "{}", PrettyCount(val)),
                            Type::Tsc => ui_writeln!(ui, default, "{}", PrettyDuration(s_per_tsc * val as f64)),
                            Type::Bytes => ui_writeln!(ui, default, "{}", PrettySize(val)),
                        };
                    }
                    let end = ui.text.num_lines();
                    ui.add(widget!().text_lines(start..end));
                });
            }
            draw_hline('╰', '╯', ui);
        });

        let content_height = ui.calculate_size(content_widget, Axis::Y);
        let viewport_height = ui.calculate_size(viewport, Axis::Y);

        let root = ui.cur_parent;
        with_parent!(ui, viewport, {
            ui.focus();
            cursorless_scrolling_navigation(&mut self.scroll, None, root, scroll_bar, ui);
        });
    }
}
impl WindowContent for HintsWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        if state.profiler_enabled {
            self.build_profiling_charts(debugger, ui);
            return;
        }

        let start = ui.text.num_lines();
        for hint in &state.key_hints {
            if hint.key_ranges.is_empty() { // spacer
                ui.text.close_line();
                continue;
            }
            for (i, range) in hint.key_ranges.iter().enumerate() {
                if i != 0 {
                    ui_write!(ui, default_dim, "/");
                }
                styled_write!(ui.text, ui.palette.hotkey.apply(ui.palette.default_dim), "{}", ui.key_binds.action_to_key_name(range[0]));
                if range[0] != range[1] {
                    ui_write!(ui, default_dim, "…");
                    styled_write!(ui.text, ui.palette.hotkey.apply(ui.palette.default_dim), "{}", ui.key_binds.action_to_key_name(range[1]));
                }
            }
            ui_writeln!(ui, default_dim, " - {}", hint.hint);
        }
        let end = ui.text.num_lines();

        ui.cur_mut().set_hstack();
        let viewport = ui.add(widget!().width(AutoSize::Remainder(1.0)));
        let scroll_bar = ui.add(widget!().fixed_width(1));
        ui.layout_children(Axis::X);
        let content = ui.add(widget!().parent(viewport).height(AutoSize::Children).hstack());

        let num_columns = 2;
        let lines_per_column = (end - start + num_columns - 1) / num_columns;
        with_parent!(ui, content, {
            for i in 0..num_columns {
                if i != 0 {
                    ui.add(widget!().fixed_width(1).fixed_height(0));
                }
                let mut range = start + i*lines_per_column .. start + (i+1)*lines_per_column;
                range.end = range.end.min(end);
                range.start = range.start.min(range.end);
                ui.add(widget!().width(AutoSize::Remainder(2.0)).height(AutoSize::Text).text_lines(range).flags(WidgetFlags::LINE_WRAP));
            }
            ui.layout_children(Axis::X);
        });

        let root = ui.cur_parent;
        with_parent!(ui, viewport, {
            ui.focus();
            cursorless_scrolling_navigation(&mut self.scroll, None, root, scroll_bar, ui);
        });
    }
}

#[derive(Default)]
struct BinariesWindow {
    table_state: TableState,
}
impl WindowContent for BinariesWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        // Listed in order of mmap address, so the main executable is usually first, which is nice.
        state.binaries = debugger.info.maps.list_binaries();
        // List previously seen unloaded binaries too, because they're visible to file/function open dialogs, especially if there's no debuggee process.
        for id in debugger.symbols.list() {
            if !debugger.info.binaries.contains_key(&id) {
                state.binaries.push(id);
            }
        }

        let mut table = Table::new(mem::take(&mut self.table_state), ui, vec![Column::new("idx", AutoSize::Fixed(3)), Column::new("name", AutoSize::Remainder(1.0)), Column::new("file", AutoSize::Fixed(PrettySize::MAX_LEN))]);
        table.hide_cursor_if_unfocused = true;
        for (idx, id) in state.binaries.iter().enumerate() {
            let binary = debugger.symbols.get_if_present(id).unwrap();
            let is_mapped = debugger.info.binaries.contains_key(id);
            table.start_row(hash(id), ui);

            ui_writeln!(ui, default_dim, "{}", idx + 1);
            table.text_cell(ui);

            // Path, progress bar, error message.
            with_parent!(ui, table.start_cell(ui), {
                ui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);

                // Path.
                let style = if is_mapped {ui.palette.default} else {ui.palette.default_dim};
                let l = styled_writeln!(ui.text, style, "{}", id.path);
                ui.add(widget!().height(AutoSize::Text).flags(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT).text(l));

                // Progress bar.
                let mut indicated_loading = false;
                if binary.symbols.as_ref().is_err_and(|e| e.is_loading()) {
                    let (progress_ppm, loading_stage) = debugger.symbols.get_progress(id);
                    let l = ui_writeln!(ui, default, "{}% ({})", (progress_ppm + 5000) / 10000, loading_stage);
                    let mut w = widget!().height(AutoSize::Text).text(l);
                    w.draw_progress_bar = Some((progress_ppm as f64 / 1e6, ui.palette.progress_bar));
                    ui.add(w);
                    indicated_loading = true;
                }

                // Error.
                let start = ui.text.num_lines();
                let mut print_error = |e: &Error| {
                    if e.is_loading() {
                        if !indicated_loading {
                            ui_writeln!(ui, default_dim, "loading");
                            indicated_loading = true;
                        }
                        return;
                    }
                    let style = if e.is_missing_symbols() {ui.palette.default_dim} else {ui.palette.error};
                    styled_writeln!(ui.text, style, "{}", e);
                };
                if let Err(e) = &binary.elf {
                    print_error(e);
                } else {
                    if let Err(e) = &binary.unwind {
                        print_error(e);
                    }
                    if let Err(e) = &binary.symbols {
                        print_error(e);
                    }
                }
                let end = ui.text.num_lines() - start;
                if end > start {
                    ui.add(widget!().height(AutoSize::Text).text_lines(start..end));
                }
            });

            // ELF or unwind error/loading/stats.
            match &binary.elf {
                Err(e) if e.is_loading() => ui_writeln!(ui, running, "loading"),
                Err(e) => ui_writeln!(ui, error, "error"),
                Ok(elf) => ui_writeln!(ui, default_dim, "{}", PrettySize(elf.data().len())),
            };
            table.text_cell(ui);
        }

        self.table_state = table.finish(ui);
    }
}

#[derive(Default)]
struct ThreadsFilter {
    bar: SearchBar,
    cache_key: String, // when this changes, drop cached_results
    cached_results: Vec<(/*thread_idx*/ usize, /*stop_count*/ usize, /*passes_filter*/ bool)>, // sorted by thread_idx
}
impl ThreadsFilter {
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
            if subframe.function_idx.is_ok() {
                if subframe.function_name.find(query).is_some() {
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

#[derive(Default)]
struct ThreadsWindow {
    table_state: TableState,

    seen_stop_counts: HashMap<pid_t, usize>,
    filter: ThreadsFilter,
}
impl WindowContent for ThreadsWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        ui.cur_mut().set_vstack();
        if ui.check_key(KeyAction::Find) {
            self.filter.bar.start_editing();
        }
        with_parent!(ui, ui.add(widget!().identity(&'s').fixed_height(if self.filter.bar.visible {1} else {0})), {
            ui.focus();
            if self.filter.bar.visible {
                let l = ui_writeln!(ui, default_dim, "filter: ");
                self.filter.bar.build(Some(l), None, ui);
            }
        });
        let table_widget = ui.add(widget!().identity(&'t').height(AutoSize::Remainder(1.0)));
        with_parent!(ui, table_widget, {
            ui.multifocus();
        });
        ui.layout_children(Axis::Y);

        let mut table = with_parent!(ui, table_widget, {
            Table::new(mem::take(&mut self.table_state), ui, vec![
                Column::new("idx", AutoSize::Fixed(5)),
                Column::new("tid", AutoSize::Fixed(10)),
                Column::new("name", AutoSize::Fixed(15)),
                Column::new("s", AutoSize::Fixed(1)),
                Column::new("cpu", AutoSize::Fixed(4)),
                Column::new("function", AutoSize::Remainder(1.0)),
                Column::new("addr", AutoSize::Fixed(12)),
                Column::new("bin", AutoSize::Fixed(3)),
            ])
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
            state.selected_thread = *tid;
        }

        // If selected thread doesn't pass the filter, awkwardly add it to the list anyway, greyed out.
        let mut selected_thread_filtered_out = false;
        let mut cursor = table.state.cursor;
        if debugger.threads.get(&state.selected_thread).is_some() {
            if let Some(i) = filtered_tids.iter().position(|x| *x == state.selected_thread) {
                cursor = i;
            } else {
                selected_thread_filtered_out = true;
                filtered_tids.push(state.selected_thread);
                cursor = filtered_tids.len() - 1;
            }
        }

        // Global hotkeys.
        with_parent!(ui, ui.content_root, {
            for key in ui.check_keys(&[KeyAction::PreviousThread, KeyAction::NextThread]) {
                match key {
                    KeyAction::PreviousThread => cursor = cursor.saturating_sub(1),
                    KeyAction::NextThread => cursor += 1,
                    _ => panic!("huh"),
                }
            }
        });

        if cursor != table.state.cursor {
            table.state.select(cursor);
        }

        let range = table.lazy(filtered_tids.len(), 1, ui);

        state.selected_thread = filtered_tids.get(table.state.cursor).copied().unwrap_or(0);
        if selected_thread_filtered_out && state.selected_thread != *filtered_tids.last().unwrap() {
            ui.should_redraw = true;
        }

        for i in range {
            let tid = filtered_tids[i];
            let stack = debugger.get_stack_trace(tid, /* partial */ true);
            let t = debugger.threads.get(&tid).unwrap();
            let row_widget = table.start_row(hash(&tid), ui);
            
            ui_writeln!(ui, default_dim, "{}", t.idx);
            table.text_cell(ui);
            
            ui_writeln!(ui, default_dim, "{}", t.tid);
            table.text_cell(ui);

            let is_filtered_out = selected_thread_filtered_out && tid == state.selected_thread;
            match t.info.name.clone() {
                Ok(name) => styled_writeln!(ui.text, if is_filtered_out {ui.palette.default_dim} else {ui.palette.default}, "{}", name),
                Err(e) => ui_writeln!(ui, error, "{}", e),
            };
            table.text_cell(ui);

            let style = match t.info.resource_stats.state {
                'R' | 'D' => ui.palette.running,
                'S' | 'T' | 't' => ui.palette.suspended,
                'Z' | 'X' | 'x' => ui.palette.error,
                _ => ui.palette.state_other,
            };
            styled_writeln!(ui.text, style, "{}", t.info.resource_stats.state);
            table.text_cell(ui);

            ui_writeln!(ui, default_dim, "{:.0}%", t.info.resource_stats.cpu_percentage(debugger.info.resource_stats.period_ns));
            table.text_cell(ui);

            match t.state {
                ThreadState::Running => {
                    match &debugger.stepping {
                        Some(step) if step.tid == tid => ui_writeln!(ui, state_other, "stepping"),
                        _ => ui_writeln!(ui, state_running, "running"),
                    };
                    table.text_cell(ui);

                    ui.text.close_line();
                    table.text_cell(ui);
                    table.text_cell(ui);
                }
                ThreadState::Suspended if !stack.frames.is_empty() => {
                    let f = &stack.frames[0];
                    let sf = &stack.subframes[0];
                    if sf.function_idx.is_ok() {
                        ui_writeln!(ui, function_name, "{}", sf.function_name);
                    } else {
                        ui_writeln!(ui, default_dim, "?");
                    }
                    table.text_cell(ui);
                    
                    ui_writeln!(ui, default_dim, "{:x}", f.addr);
                    table.text_cell(ui);

                    match &f.binary_id {
                        Some(binary_id) => ui_writeln!(ui, default_dim, "{}", state.binaries.iter().position(|b| b == binary_id).unwrap() + 1),
                        None => ui_writeln!(ui, default_dim, "?"),
                    };
                    table.text_cell(ui);
                }
                ThreadState::Suspended => {
                    match &stack.truncated {
                        Some(e) if e.is_loading() => ui_writeln!(ui, running, "[loading]"),
                        Some(e) => ui_writeln!(ui, error, "<{}>", e),
                        None => ui_writeln!(ui, default_dim, "[empty]"),
                    };
                    table.text_cell(ui);

                    ui.text.close_line();
                    table.text_cell(ui);
                    table.text_cell(ui);
                }
            }

            // Highlight threads that got a fatal signal or hit a breakpoint.
            let mut style_adjustment = StyleAdjustment::default();
            for stop in &t.stop_reasons {
                match stop {
                    StopReason::Signal(_) => {
                        style_adjustment = ui.palette.thread_crash;
                        break;
                    }
                    StopReason::Breakpoint(_) => style_adjustment = ui.palette.thread_breakpoint_hit,
                    StopReason::Step => (),
                }
            }
            ui.get_mut(row_widget).style_adjustment.update(style_adjustment);
        }

        self.table_state = table.finish(ui);
    }

    fn drop_caches(&mut self) {
        self.filter.cached_results.clear();
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.push(KeyHint::key(KeyAction::Find, "filter"));
    }
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

// This window is in charge of telling source and disassembly windows when to auto-open and auto-scroll.
struct StackWindow {
    table_state: TableState,
    // Previously selected frame in each thread, so that switching threads back and forth doesn't reset the scroll.
    // If thread's stop count changed, we switch to the top frame, unless the window is locked.
    threads: HashMap<pid_t, (/* stop_count */ usize, StableSubframeIdx)>,
    // When this changes, we scroll source and disassembly windows.
    seen: (/* tid */ pid_t, StableSubframeIdx, /* frame addr */ usize),
    // After symbols were loaded, tell disassembly and source windows to try opening the file/function again.
    rerequest_scroll: bool,
}
impl Default for StackWindow { fn default() -> Self { Self {table_state: TableState::default(), threads: HashMap::new(), seen: (0, StableSubframeIdx::top(), 0), rerequest_scroll: false} } }
impl WindowContent for StackWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        state.stack = debugger.get_stack_trace(state.selected_thread, /* partial */ false);
        let rerequest_scroll = mem::take(&mut self.rerequest_scroll);

        assert_eq!(state.stack.frames.is_empty(), state.stack.subframes.is_empty());
        let num_rows = state.stack.subframes.len();
        let locked = false; // TODO: window locking

        // Behaviors that we're trying to implement here:
        //  * when switching threads back and forth without resuming the process, remember per-thread selected frame and auto-switch to that,
        //  * if a thread was resumed and stopped (e.g. a step or a breakpoint hit), switch to the top frame
        //     - even if the stack didn't change,
        //     - even if the thread was running for a very short time and no build() call happened during that,
        //     - even if the thread wasn't selected while it was running,
        //     - but if the stack window is locked, don't switch; keep the same position relative to the bottom of the stack,
        //  * when switching thread or stack frame, scroll the source and disassembly windows even if address didn't change (e.g. the same breakpoint was hit twice in a row)
        //     - including the switch-to-top case from above, even if top was already selected,
        //     - including the case when e.g. up-arrow key was pressed when already at the top of the stack (useful if the stack has only one frame),
        //  * if symbols loading completed and stack frames got expanded into multiple subframes of inlined functions, keep focus on the same frame,
        //  * when symbols loading completes, retry scrolling the source and disassembly windows, but only if they're currently showing an error (to avoid yanking valid file the user is looking at),
        //  * if the stack window is locked, and the process was stopped and resumed, and current frame's address changed, scroll the source and disassembly (if address didn't change, don't scroll).

        if !state.stack.frames.is_empty() {
            let deb_thr = debugger.threads.get(&state.selected_thread).unwrap();
            let stop_count = deb_thr.stop_count;

            let thr = self.threads.entry(state.selected_thread).or_insert((0, StableSubframeIdx::top()));
            if thr.0 == stop_count || locked {
                self.table_state.cursor = thr.1.subframe_idx(&state.stack);
            } else {
                self.table_state.select(deb_thr.subframe_to_select.unwrap_or(0));
            }
            thr.0 = stop_count;
        }

        // Global hotkeys.
        with_parent!(ui, ui.content_root, {
            for action in ui.check_keys(&[KeyAction::PreviousStackFrame, KeyAction::NextStackFrame]) {
                match action {
                    KeyAction::PreviousStackFrame => self.table_state.select(self.table_state.cursor.saturating_sub(1)),
                    KeyAction::NextStackFrame => self.table_state.select(self.table_state.cursor + 1),
                    _ => panic!("huh"),
                }
            }
        });

        let mut table = Table::new(mem::take(&mut self.table_state), ui, vec![
            Column::new("idx", AutoSize::Fixed(3)),
            Column::new("location", AutoSize::Remainder(1.0)),
            Column::new("address", AutoSize::Fixed(12)),
            Column::new("bin", AutoSize::Fixed(3)),
        ]);

        let write_stack_truncated_error = |e: &Error, ui: &mut UI| -> usize {
            if e.is_usage() || e.is_loading() {
                ui_writeln!(ui, default_dim, "{}", e)
            } else {
                ui_writeln!(ui, error, "stack truncated: {}", e)
            }
        };
        for (idx, subframe) in state.stack.subframes.iter().enumerate() {
            table.start_row(idx, ui);
            let frame = &state.stack.frames[subframe.frame_idx];

            ui_writeln!(ui, default_dim, "{}", idx);
            table.text_cell(ui);

            with_parent!(ui, table.start_cell(ui), {
                ui.cur_mut().set_vstack();

                let (l, align_right) = match &subframe.function_idx {
                    Ok(_) => (ui_writeln!(ui, function_name, "{}", subframe.function_name), false),
                    Err(e) => (ui_writeln!(ui, error, "{}", e), false),
                };
                ui.add(widget!().height(AutoSize::Text).text(l).flags(if align_right {WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT} else {WidgetFlags::empty()}));

                if let Some(info) = &subframe.line {
                    let name = info.path.as_os_str().to_string_lossy();
                    ui_write!(ui, filename, "{}", name);
                } else {
                    ui_write!(ui, default_dim, "?");
                }
                if let Some(info) = &subframe.line {
                    if info.line.line() != 0 {
                        ui_write!(ui, line_number, ":{}", info.line.line());
                        if info.line.column() != 0 {
                            ui_write!(ui, column_number, ":{}", info.line.column());
                        }
                    }
                }
                let l = ui.text.close_line();
                ui.add(widget!().height(AutoSize::Text).text(l).flags(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT));

                if idx + 1 == state.stack.subframes.len() {
                    if let Some(e) = &state.stack.truncated {
                        let l = write_stack_truncated_error(e, ui);
                        ui.add(widget!().height(AutoSize::Text).text(l));
                    }
                }
            });

            let is_inlined = idx != frame.subframes.end-1;
            if is_inlined {
                ui.text.close_line();
            } else {
                ui_writeln!(ui, default_dim, "{:x}", frame.addr);
            }
            table.text_cell(ui);

            if is_inlined {
                ui.text.close_line();
            } else if let Some(b) = &frame.binary_id {
                ui_writeln!(ui, default_dim, "{}", state.binaries.iter().position(|x| x == b).unwrap() + 1);
            } else {
                ui_writeln!(ui, default_dim, "?");
            }
            table.text_cell(ui);
        }
        if state.stack.subframes.is_empty() {
            if let Some(e) = &state.stack.truncated {
                table.start_row(0, ui);
                ui.text.close_line();
                table.text_cell(ui);

                write_stack_truncated_error(e, ui);
                table.text_cell(ui);

                ui.text.close_line();
                table.text_cell(ui);
                table.text_cell(ui);
            }
        }

        self.table_state = table.finish(ui);
        let mut scroll_source_and_disassembly = self.table_state.did_scroll_to_cursor;

        if state.stack.frames.is_empty() {
            state.selected_frame = 0;
            state.selected_subframe = 0;
        } else {
            state.selected_subframe = self.table_state.cursor;
            let subframe = &state.stack.subframes[state.selected_subframe];
            state.selected_frame = subframe.frame_idx;
            let frame = &state.stack.frames[subframe.frame_idx];
            let thr = self.threads.entry(state.selected_thread).or_insert((0, StableSubframeIdx::top()));
            thr.1 = StableSubframeIdx::new(&state.stack, state.selected_subframe);

            let cur = (state.selected_thread, thr.1, frame.addr);
            scroll_source_and_disassembly |= cur != self.seen;
            if scroll_source_and_disassembly || rerequest_scroll {
                state.should_scroll_source = Some((subframe.line.as_ref().map(|line| SourceScrollTarget {path: line.path.clone(), version: line.version.clone(), line: line.line.line()}), !scroll_source_and_disassembly));
                state.should_scroll_disassembly = Some((match &state.stack.subframes[frame.subframes.end - 1].function_idx {
                    &Ok(function_idx) => Ok(DisassemblyScrollTarget {binary_id: frame.binary_id.clone().unwrap(), symbols_identity: frame.symbols_identity, function_idx, static_pseudo_addr: frame.pseudo_addr.wrapping_sub(frame.addr_static_to_dynamic),
                                                                         subfunction_level: (frame.subframes.end - state.selected_subframe - 1) as u16}),
                    Err(e) => Err(e.clone()),
                }, !scroll_source_and_disassembly));
            }
            self.seen = cur;
        }

        if self.threads.len() > debugger.threads.len() * 2 {
            self.threads.retain(|tid, _| { debugger.threads.contains_key(tid) });
        }
    }

    fn drop_caches(&mut self) {
        self.rerequest_scroll = true;
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

struct CodeTab {
    title: String,
    path_in_symbols: PathBuf, // empty if not known
    version_in_symbols: FileVersionInfo,

    //asdqwe scroll, cursor, hscroll
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

//asdqwe check no tab title truncation happens in CodeWindow and DisassemblyWindow
impl CodeWindow {
    fn find_or_open_file<'a>(file_cache: &'a mut HashMap<(PathBuf, FileVersionInfo), SourceFile>, path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger, palette: &Palette) -> &'a SourceFile {
        file_cache.entry((path_in_symbols.to_owned(), version.clone())).or_insert_with(|| Self::open_file(path_in_symbols, version, debugger, palette))
    }

    fn open_file(path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger, palette: &Palette) -> SourceFile {
        let mut res = SourceFile {header: StyledText::default(), text: StyledText::default(), local_path: PathBuf::new(), widest_line: 0, num_lines_in_local_file: 0};

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
            res.header.close_span(palette.filename);
            res.header.close_line();

            match Self::read_and_format_file(&mut file, debugger, &mut res, path_in_symbols, palette) {
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
        res.header.close_span(palette.default);
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
        Self::read_and_format_file(&mut empty, debugger, &mut res, path_in_symbols, palette).unwrap();

        res
    }

    fn read_and_format_file(input: &mut dyn Read, debugger: &Debugger, res: &mut SourceFile, path_in_symbols: &Path, palette: &Palette) -> Result<(usize, [u8; 16])> {
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
                    text.spans.push((line_chars_start + column, palette.default));
                }
                text.spans.push((line_chars_start + column + 1, if marker.flags().contains(LineFlags::INLINED_FUNCTION) {palette.code_inlined_site} else {palette.code_statement}));
                *marker_idx += 1;
            }
            text.close_span(palette.default);
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
        
        res.widest_line = res.text.widest_line(0..res.text.num_lines());

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
/*
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

    fn toggle_breakpoint(&mut self, toggle_enabledness: bool, state: &mut UIState, debugger: &mut Debugger) {
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

    fn scroll_disassembly_if_needed(&mut self, suppress: bool, select: isize, state: &mut UIState, debugger: &Debugger) {
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

            state.should_scroll_disassembly = Some((Ok(DisassemblyScrollTarget {binary_id: binary_id.clone(), function_idx: addrs[idx].0, addr: addrs[idx].2, subfunction_level: addrs[idx].1.saturating_add(1)}), false));

            break;
        }
    }*/
}

impl WindowContent for CodeWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        styled_write!(ui.text, ui.palette.default, "code");
        let l = ui.text.close_line();
        ui.add(widget!().text(l));
        /*asdqwe
    fn update_modal(&mut self, state: &mut UIState, debugger: &mut Debugger, keys: &mut Vec<Key>) {
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

    fn render_modal(&mut self, state: &mut UIState, debugger: &mut Debugger, f: &mut Frame, window_area: Rect, screen_area: Rect) {
        if let Some(d) = &mut self.search_dialog {
            ui.loading |= d.render(f, screen_area, "find file", &debugger.context.settings.palette);
        }
    }

    fn cancel_modal(&mut self, state: &mut UIState, debugger: &mut Debugger) {
        self.search_dialog = None;
    }

    fn drop_caches(&mut self) {
        self.file_cache.clear();
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        styled_write!(hints, ui.palette.default_dim, "o - open file"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, "C-y - pin/unpin tab"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, "b - toggle breakpoint"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, "B - enable/disable breakpoint"); hints.close_line();
        styled_write!(hints, ui.palette.default_dim, ",/. - cycle disasm addrs"); hints.close_line();
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

    fn update_and_render(&mut self, state: &mut UIState, debugger: &mut Debugger, mut keys: Vec<Key>, f: Option<&mut Frame>, mut area: Rect) {
        let suppress_disassembly_autoscroll = state.should_scroll_source.is_some();
        let switch_to = match mem::take(&mut state.should_scroll_source) {
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
        for (idx, subframe) in state.stack.subframes.iter().enumerate() {
            if let Some(info) = &subframe.line {
                instruction_pointers.insert((&info.path, info.line.line()), (info.line.column(), idx == state.selected_subframe, idx == 0));
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
        let file = Self::find_or_open_file(&mut self.file_cache, &tab.path_in_symbols, &tab.version_in_symbols, debugger, &ui.palette);
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
                        spans.push(Span::styled(" ", ui.palette.ip_column.apply(ui.palette.default)));
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

        self.scroll_disassembly_if_needed(suppress_disassembly_autoscroll, select_disassembly_address, ui, debugger);*/
    }
}

#[derive(Default)]
struct BreakpointsWindow {
    table_state: TableState,
    selected_breakpoint: Option<BreakpointId>,
}
impl WindowContent for BreakpointsWindow {
    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.extend([KeyHint::key(KeyAction::DeleteRow, "delete breakpoint"), KeyHint::key(KeyAction::DisableBreakpoint, "disable breakpoint")]);
    }

    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        styled_write!(ui.text, ui.palette.default, "breakpoints");
        let l = ui.text.close_line();
        ui.add(widget!().text(l));
        /*asdqwe
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

        if let &Some(id) = &self.selected_breakpoint {
            keys.retain(|key| {
                match debugger.context.settings.keys.map.get(key) {
                    Some(KeyAction::DeleteRow) => {debugger.remove_breakpoint(id);}
                    Some(KeyAction::ToggleBreakpointEnabledness) => {
                        let r = debugger.toggle_breakpoint_enabledness(id);
                        report_result(ui, &r);
                    }
                    _ => return true,
                }
                false
            });
        }

        let mut breakpoints: Vec<BreakpointId> = debugger.breakpoints.iter().map(|p| p.0).collect();
        breakpoints.sort_by_key(|id| id.seqno);

        if let &Some(id) = &self.selected_breakpoint {
            if let Some(idx) = breakpoints.iter().position(|b| b == &id) {
                self.scroll.cursor = idx;
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
                    state.should_scroll_source = Some((Some(SourceScrollTarget {path: on.path.clone(), version: on.file_version.clone(), line: on.line}), false));
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
                        Span::styled(name, palette.filename),
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
            let mut style = if error {palette.error} else {palette.default};
            if !b.enabled {
                style = style.add_modifier(Modifier::DIM);
            }

            rows.push(Row::new(cells).style(style));
        }

        let table = Table::new(rows)
            .header(Row::new(vec!["idx", "", "on", "locs", "hits"]).style(palette.table_header))
            .widths(&widths)
            .highlight_style(palette.table_selected_item).highlight_symbol("➤ ");

        f.render_stateful_widget(table, area, &mut table_state);*/
    }
}
