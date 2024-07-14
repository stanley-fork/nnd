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
        let state = UIState::default();
        //state.profiler_enabled = true;
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

#[derive(Clone, Copy, Eq, PartialEq)]
enum SpecialWatch {
    Locals,
    Registers,
    AddWatch,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct ValueTreeNodeIdx(usize);
impl ValueTreeNodeIdx {
    fn invalid() -> Self { Self(usize::MAX) }
    fn is_valid(self) -> bool { self.0 != usize::MAX }
}

struct ValueTreeNode {
    name: Range<usize>, // lines in ValueTree's `text`
    line_wrapped_name: Option<Range<usize>>,
    value: Result<Value>,
    dubious: bool,
    special: Option<SpecialWatch>,

    // Unique identifier of this node. Hash of identities (field names or array indices or similar) of nodes on the path from the root to here.
    // For remembering which subtrtees were expanded and where the cursor was, in a way that survives recalculation or partial change of the values.
    identity: usize,

    depth: usize,
    parent: ValueTreeNodeIdx,

    // These are populated lazily.
    formatted_value: [Option<Range<usize>>; 3], // collapsed, expanded, line-wrapped; ranges in ValueTree's `text`; if value is Err, the error message is formatted into here
    has_children: bool, // populated together with either element of formatted_value
    children: Range<usize>, // indices in `nodes`; populated together with formatted_value[1]

    // Layout information, recalculated every frame, valid only for reachable nodes.
    start_y: isize,
    node_end_y: isize,
    subtree_end_y: isize,
    expanded: bool,
    is_text_input: bool,
    // These are just for asserts.
    layout_frame_idx: usize, // when this node's layout information was updated
    name_line_wrap_width: usize, // what width was used for line_wrapped_name
}
impl Default for ValueTreeNode { fn default() -> Self { Self {name: 0..0, line_wrapped_name: None, value: err!(ValueTreePlaceholder, ""), dubious: false, special: None, identity: 0, depth: 0, parent: ValueTreeNodeIdx::invalid(), formatted_value: [None, None, None], has_children: false, children: 0..0, start_y: 0, node_end_y: 0, subtree_end_y: 0, expanded: false, is_text_input: false, layout_frame_idx: 0, name_line_wrap_width: 0} } }

#[derive(Default)]
struct ValueTree {
    nodes: Vec<ValueTreeNode>,
    roots: Vec<ValueTreeNodeIdx>, // (so it's technically a forest, but ValueTree sounds nicer, and you can always imagine an implicit root node that has these "root" nodes as children)
    // An element of depth-first traversal of tree nodes, descending only into expanded ones. The cursor stands on one of these.
    rows: Vec<ValueTreeNodeIdx>,

    text: StyledText, // arena for various text
    temp_text: StyledText,
    arena: Arena,
}
impl ValueTree {
    fn clear(&mut self) { *self = Self::default(); }

    fn add(&mut self, mut node: ValueTreeNode) -> ValueTreeNodeIdx {
        assert!(node.depth == 0);
        if node.parent.is_valid() {
            let parent = &self.nodes[node.parent.0];
            node.identity = hash(&(parent.identity, node.identity));
            node.depth = parent.depth + 1;
        } else {
            self.roots.push(ValueTreeNodeIdx(self.nodes.len()));
        }
        self.nodes.push(node);
        ValueTreeNodeIdx(self.nodes.len() - 1)
    }
}

struct WatchExpression {
    identity: usize,
    text: String,
    special: Option<SpecialWatch>,
}
impl WatchExpression {
    fn is_editable(&self) -> bool { !self.special.is_some_and(|s| s != SpecialWatch::AddWatch) }
}

struct WatchesWindow {
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

    expressions: Vec<WatchExpression>, // watch expressions; parallel to tree.roots
    text_input: Option<(/*identity*/ usize, /*height*/ usize, TextInput)>, // if editing watch expression
    text_input_built: bool,

    eval_state: EvalState,
}
impl Default for WatchesWindow {
    fn default() -> Self {
        let mut r = Self {tree: ValueTree::default(), expanded_nodes: HashSet::new(), cursor_path: Vec::new(), cursor_idx: 0, scroll: 0, scroll_to_cursor: false, name_width: 0, value_width: 0, row_height_limit: 0, indent_width: 0, max_indent: 0, seen: (0, usize::MAX, usize::MAX), expressions: Vec::new(), text_input: None, text_input_built: false, eval_state: EvalState::new() };

        let locals_identity: usize = random();
        r.expanded_nodes.insert(locals_identity);
        r.expressions.push(WatchExpression {identity: locals_identity, text: "[locals]".to_string(), special: Some(SpecialWatch::Locals)});
        r.expressions.push(WatchExpression {identity: random(), text: "[registers]".to_string(), special: Some(SpecialWatch::Registers)});
        r
    }
}
impl WatchesWindow {
    fn eval_locals(&mut self, context: &EvalContext, parent: ValueTreeNodeIdx, palette: &Palette) {
        let (dwarf_context, _) = match self.eval_state.make_local_dwarf_eval_context(context, context.selected_subframe) {
            Ok(x) => x,
            Err(e) => {
                self.tree.nodes[parent.0].value = Err(e);
                return;
            }
        };
        let symbols = dwarf_context.symbols.unwrap();
        let subframe = &context.stack.subframes[context.selected_subframe];
        let pseudo_addr = context.stack.frames[subframe.frame_idx].pseudo_addr;
        let static_pseudo_addr = dwarf_context.addr_map.dynamic_to_static(pseudo_addr);

        let mut idxs_per_name: HashMap<&str, usize> = HashMap::new();
        for v in dwarf_context.local_variables {
            if !v.range().contains(&(static_pseudo_addr)) {
                continue;
            }
            if v.flags().contains(LocalVariableFlags::FRAME_BASE) {
                // Pseudo-variable internal to eh_frame unwind mechanism. We don't list it here, but it's available as `#frame_base` in watch expressions.
                continue;
            }
            let (value, dubious) = match eval_dwarf_expression(v.expr, &dwarf_context) {
                Ok((val, dub)) => (Ok(val), dub),
                Err(e) => (Err(e), false),
            };
            let name = unsafe {v.name()};
            let l = styled_writeln!(self.tree.text, palette.default, "{}", name);
            let idx_per_name = *idxs_per_name.entry(name).and_modify(|x| *x += 1).or_insert(1) - 1;
            self.tree.add(ValueTreeNode {name: l..l+1, value: value.map(|val| Value {val, type_: v.type_, flags: ValueFlags::empty()}), dubious, identity: hash(&(name, idx_per_name)), parent, ..Default::default()});
        }
    }

    fn eval_registers(&mut self, context: &EvalContext, parent: ValueTreeNodeIdx, palette: &Palette) {
        if let Some(sf) = context.stack.subframes.get(context.selected_subframe) {
            let regs = &context.stack.frames[sf.frame_idx].regs;
            for reg in RegisterIdx::all() {
                if let Ok((v, dubious)) = regs.get_int(*reg) {
                    let l = styled_writeln!(self.tree.text, palette.default, "{}", reg);
                    let value = Value {val: AddrOrValueBlob::Blob(ValueBlob::new(v as usize)), type_: self.eval_state.builtin_types.u64_, flags: ValueFlags::HEX};
                    self.tree.add(ValueTreeNode {name: l..l+1, value: Ok(value), dubious, identity: hash(&reg), parent, ..D!()});
                }
            }
        }
    }

    fn eval_watches(&mut self, context: Option<&EvalContext>, palette: &Palette) {
        for i in 0..self.expressions.len() {
            let expr = &self.expressions[i];
            let style = if expr.special.is_some() {palette.default_dim} else {palette.default};
            let l = styled_writeln!(self.tree.text, style, "{}", expr.text);
            let name = self.tree.text.split_by_newline_character(l, None);
            let node_idx = self.tree.add(ValueTreeNode {name, identity: expr.identity, special: expr.special.clone(), ..D!()});
            let node = &mut self.tree.nodes[node_idx.0];

            if expr.special.is_some() {
                continue;
            }
            
            let context = match &context {
                None => continue,
                &Some(x) => x };
            match eval_watch_expression(&expr.text, &mut self.eval_state, context) {
                Ok((val, dub)) => {
                    node.value = Ok(val);
                    node.dubious = dub;
                }
                Err(e) => node.value = Err(e),
            }
        }
    }

    // If !node.expanded: populates formatted_value[0], has_children.
    // If  node.expanded: populates formatted_value[1], has_children, children.
    // If already populated, does nothing.
    // If context is none, may not populate; in this case the caller should show the error from `context` (e.g. "running") instead of formatted_value.
    fn ensure_node_info(&mut self, node_idx: ValueTreeNodeIdx, context: Option<&EvalContext>, palette: &Palette) {
        let node = &mut self.tree.nodes[node_idx.0];
        let i = node.expanded as usize;
        if node.formatted_value[i].is_some() {
            return;
        }
        match (&node.special, &node.value) {
            (&Some(SpecialWatch::AddWatch), _) => {
                node.formatted_value = [Some(0..0), Some(0..0), Some(0..0)];
            }
            (&Some(special), _) => if let &Some(context) = &context {
                node.formatted_value[0] = Some(0..0);
                node.has_children = true;
                if node.expanded {
                    node.formatted_value[1] = Some(0..0);
                    let start = self.tree.nodes.len();
                    let what = match special {
                        SpecialWatch::Locals => {
                            self.eval_locals(context, node_idx, palette);
                            "locals"
                        }
                        SpecialWatch::Registers => {
                            self.eval_registers(context, node_idx, palette);
                            "registers"
                        }
                        SpecialWatch::AddWatch => panic!("huh"),
                    };
                    let end = self.tree.nodes.len();
                    let node = &mut self.tree.nodes[node_idx.0];
                    node.children = start..end;
                    if node.children.is_empty() {
                        let l = styled_writeln!(self.tree.text, palette.default_dim, "no {}", what);
                        node.formatted_value[1] = Some(l..l+1);
                    }
                }
            }
            (&None, Err(e)) if e.is_value_tree_placeholder() => (),
            (&None, Err(e)) => {
                let l = styled_writeln!(self.tree.text, palette.error, "<{}>", e);
                node.formatted_value[0] = Some(l..l+1);
                node.formatted_value[1] = Some(l..l+1);
            }
            (&None, Ok(value)) => if let &Some(context) = &context {
                let (has_children, children) = format_value(value, node.expanded, &mut self.eval_state, context, &mut self.tree.temp_text, &mut self.tree.text, palette);
                let l = self.tree.temp_text.close_line();
                let l = self.tree.text.import_lines(&self.tree.temp_text, l..l+1);
                node.formatted_value[i] = Some(l);
                node.has_children = has_children;

                if i == 1 {
                    let dubious = node.dubious;
                    let start = self.tree.nodes.len();
                    for c in children {
                        self.tree.add(ValueTreeNode {name: c.name_line..c.name_line+1, identity: c.identity, value: c.value, dubious, parent: node_idx, ..D!()});
                    }
                    let end = self.tree.nodes.len();
                    self.tree.nodes[node_idx.0].children = start..end;
                }
            }
        }
    }

    fn do_layout(&mut self, context: Option<&EvalContext>, palette: &Palette, frame_idx: usize) -> usize {
        self.cursor_idx = 0;
        self.tree.rows.clear();
        let mut y = 0isize;
        let mut stack: Vec<(ValueTreeNodeIdx, u8)> = self.tree.roots.iter().map(|i| (*i, 0)).rev().collect();
        while let Some((node_idx, pass)) = stack.pop() {
            let node = &mut self.tree.nodes[node_idx.0];
            if pass == 1 {
                node.subtree_end_y = y;
                continue;
            }
            let row_idx = self.tree.rows.len();
            self.tree.rows.push(node_idx);
            node.layout_frame_idx = frame_idx;
            node.start_y = y;
            node.expanded = self.expanded_nodes.contains(&node.identity);
            node.is_text_input = self.text_input.as_ref().is_some_and(|(identity, _, _)| *identity == node.identity);
            if self.cursor_path.get(node.depth) == Some(&node.identity) {
                self.cursor_idx = row_idx;
            }

            // Calculate height. If expanded, this requires formatting the value and doing line wrap.
            let mut height = node.name.len().max(1);
            if node.is_text_input {
                height.set_max(self.text_input.as_ref().unwrap().1);
            }
            let mut pushed_children_to_stack = false;
            if node.expanded {
                if !node.is_text_input {
                    let name_line_wrap_width = self.name_width.saturating_sub(node.depth.min(self.max_indent) * self.indent_width + 2);
                    if node.line_wrapped_name.is_some() {
                        assert_eq!(node.name_line_wrap_width, name_line_wrap_width);
                    } else {
                        node.name_line_wrap_width = name_line_wrap_width;
                        node.line_wrapped_name = Some(self.tree.text.line_wrap(node.name.clone(), name_line_wrap_width, self.row_height_limit, &palette.line_wrap_indicator, &palette.truncation_indicator, None));
                    }
                    height.set_max(node.line_wrapped_name.clone().unwrap().len());
                }

                self.ensure_node_info(node_idx, context, palette);
                let node = &mut self.tree.nodes[node_idx.0];

                if node.formatted_value[2].is_none() && node.formatted_value[1].is_some() {
                    node.formatted_value[2] = Some(self.tree.text.line_wrap(node.formatted_value[1].clone().unwrap(), self.value_width, self.row_height_limit, &palette.line_wrap_indicator, &palette.truncation_indicator, None));
                }
                if let Some(v) = &node.formatted_value[2] {
                    height.set_max(v.len());
                }

                if node.has_children && !node.children.is_empty() {
                    pushed_children_to_stack = true;
                    stack.push((node_idx, 1));
                    for idx in node.children.clone().rev() {
                        stack.push((ValueTreeNodeIdx(idx), 0));
                    }
                }
            }
            let node = &mut self.tree.nodes[node_idx.0];

            y += height as isize;
            node.node_end_y = y;
            if !pushed_children_to_stack {
                node.subtree_end_y = y;
            }
        }

        y as usize
    }

    fn build_widgets(&mut self, visible_y: Range<isize>, context: &Result<EvalContext>, ui: &mut UI) {
        let mut stack: Vec<ValueTreeNodeIdx> = self.tree.roots.iter().copied().rev().collect();
        while let Some(node_idx) = stack.pop() {
            let node = &self.tree.nodes[node_idx.0];
            assert_eq!(node.layout_frame_idx, ui.frame_idx);
            if node.start_y >= visible_y.end || node.subtree_end_y <= visible_y.start {
                continue;
            }

            if node.expanded {
                for idx in node.children.clone().rev() {
                    stack.push(ValueTreeNodeIdx(idx));
                }
            }

            let indent = node.depth.min(self.max_indent) * self.indent_width;

            // Order of operations:
            //  1. Format the value and determine whether the node has children.
            //  2. Create indentation arrow+line widget.
            //  3. Create the row widget. It may partially cover the indentation widget if indent width is > 2 cells in palette.
            //    3a. Create value widget.
            //    3b. Create name or text input widget. It may partially cover the value widget if it's a text input.

            let formatted = if node.expanded {
                node.formatted_value[2].clone()
            } else {
                self.ensure_node_info(node_idx, context.as_ref().ok(), &ui.palette);
                self.tree.nodes[node_idx.0].formatted_value[0].clone()
            };
            let node = &self.tree.nodes[node_idx.0];
            let value_start = ui.text.num_lines();
            if let Some(lines) = formatted {
                ui.text.import_lines(&self.tree.text, lines);
            } else {
                // We don't do line wrap on this error message, but that's ok, it's short (something like "<running>" or "<no process>").
                ui_writeln!(ui, default_dim, "<{}>", context.as_ref().err().unwrap());
            }
            let value_end = ui.text.num_lines();

            // Draw vertical line showing indentation, and arrow showing whether the node is expanded or expandable.
            // This must be below the `node.has_children` assignment above (where we lazily format the value).
            with_parent!(ui, ui.add(widget!().identity(&('|', node.identity)).fixed_x(indent as isize).fixed_y(node.start_y).fixed_width(1).fixed_height(1).vstack().highlight_on_hover()), {
                if ui.check_mouse(MouseActions::CLICK) {
                    if node.expanded {
                        self.expanded_nodes.remove(&node.identity);
                    } else {
                        self.expanded_nodes.insert(node.identity);
                    }
                    self.scroll_to_cursor = true;
                    ui.should_redraw = true;
                }

                let arrow = if !node.has_children {
                    ' '
                } else if node.expanded {
                    '▾'
                } else {
                    '▸'
                };
                if node.has_children && node.expanded && node.depth < self.max_indent {
                    // Draw the line.
                    let w = ui.cur_mut();
                    w.axes[Axis::X].set_fixed_size(self.indent_width.max(1));
                    w.axes[Axis::Y].set_fixed_size((node.subtree_end_y - node.start_y) as usize);
                    ui.add(widget!().fixed_width(1).fixed_height(1).fill('▾', ui.palette.default));
                    let line = if node.depth + 1 < self.max_indent {
                        styled_writeln!(ui.text, ui.palette.tree_indent.1, "{}", ui.palette.tree_indent.0)
                    } else {
                        styled_writeln!(ui.text, ui.palette.tree_indent_limit_reached.1, "{}", ui.palette.tree_indent_limit_reached.0)
                    };
                    ui.add(widget!().text(line).flags(WidgetFlags::REPEAT_TEXT_VERTICALLY));
                } else {
                    // Draw only an arrow, or a ' ' if the node doesn't have children.
                    // A node can be expanded even if it doesn't have children, to enable line wrap. We currently don't have any indicator this, but still draw a clickable ' ' as a sort of secret feature.
                    // Would be nice to detect that the text doesn't fit and show some special kind of arrow instead of ' ' in that case.
                    let s = ui.palette.default;
                    ui.cur_mut().draw_fill = Some((arrow, s));
                }
            });

            if node.node_end_y > visible_y.start {
                let row_x = indent + 2;
                with_parent!(ui, ui.add(widget!().identity(&node.identity).fixed_x(row_x as isize).fixed_width((self.name_width + self.value_width + 1).saturating_sub(row_x)).fixed_height((node.node_end_y - node.start_y) as usize).fixed_y(node.start_y).fill(' ', ui.palette.default).highlight_on_hover()), {
                    if self.cursor_path.last() == Some(&node.identity) {
                        let a = ui.palette.selected;
                        ui.cur_mut().style_adjustment.update(a);
                    }

                    if ui.check_mouse(MouseActions::CLICK) {
                        self.set_cursor_path_from_node(node_idx);
                        ui.should_redraw = true;
                    }
                    let node = &self.tree.nodes[node_idx.0];

                    let name_effective_width = self.name_width.saturating_sub(row_x);

                    let mut w = widget!().fixed_x(name_effective_width as isize + 1).fixed_width(self.value_width).text_lines(value_start..value_end);
                    if node.dubious || context.is_err() {
                        w.style_adjustment.update(ui.palette.value_dubious);
                    }
                    ui.add(w);

                    if node.is_text_input {
                        let (identity, height, input) = self.text_input.as_mut().unwrap();
                        assert_eq!(*identity, node.identity);
                        let input_widget = ui.add(widget!().identity(&'i').width(AutoSize::Children).min_width(name_effective_width).max_width(name_effective_width + self.value_width + 1).height(AutoSize::Children).max_height(self.row_height_limit));
                        with_parent!(ui, input_widget, {
                            ui.focus();
                            let changed = input.build(ui);
                            self.scroll_to_cursor |= changed && (visible_y.start > node.start_y || visible_y.end < node.node_end_y);
                            self.text_input_built = true;
                        });
                        let new_height = ui.calculate_size(input_widget, Axis::Y);
                        if new_height != *height {
                            *height = new_height;
                            self.scroll_to_cursor = true;
                            ui.should_redraw = true;
                        }
                    } else {
                        let lines = if node.expanded {
                            assert_eq!(node.name_line_wrap_width, name_effective_width);
                            node.line_wrapped_name.clone().unwrap()
                        } else {
                            node.name.clone()
                        };
                        let lines = ui.text.import_lines(&self.tree.text, lines);
                        let mut w = widget!().text_lines(lines).fixed_width(name_effective_width);
                        // Dim the *name* if parent's value is dubious, i.e. the existence of this node is dubious.
                        if node.depth > 0 && (self.tree.nodes[node.parent.0].dubious || context.is_err()) {
                            w.style_adjustment.update(ui.palette.value_dubious);
                        }
                        ui.add(w);
                    }
                });
            }
        }
    }

    fn set_cursor_path_from_node(&mut self, mut node_idx: ValueTreeNodeIdx) {
        self.cursor_path.clear();
        while node_idx.is_valid() {
            let node = &self.tree.nodes[node_idx.0];
            self.cursor_path.push(node.identity);
            node_idx = node.parent;
        }
        self.cursor_path.reverse();
    }
}

impl WindowContent for WatchesWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        // Plan:
        //  1. Handle expand/collapse/edit-start/edit-stop inputs using last frame rows and tree.
        //  2. Recalculate the tree if needed.
        //  3. Recalculate layout and rows list if needed.
        //  4. Handle cursor movement and scrolling. Now we know which rows are visible.
        //  5. Build widgets for visible rows (and for selected row with text input, even if not visible).

        // Keyboard input.
        let mut refresh_data = self.tree.roots.is_empty();
        for action in ui.check_keys(&[KeyAction::Cancel, KeyAction::CursorRight, KeyAction::CursorLeft, KeyAction::Enter, KeyAction::DeleteRow, KeyAction::DuplicateRow]) {
            self.scroll_to_cursor = true;

            if action == KeyAction::Cancel {
                self.text_input = None;
                continue;
            }

            let node_idx = match self.tree.rows.get(self.cursor_idx) {
                None => continue,
                Some(x) => *x };
            let node = &self.tree.nodes[node_idx.0];
            let expr_idx = if node.depth == 0 {
                self.tree.roots.iter().position(|ix| *ix == node_idx)
            } else {
                None
            };
            match action {
                KeyAction::CursorRight => {self.expanded_nodes.insert(node.identity);}
                KeyAction::CursorLeft => {
                    self.expanded_nodes.remove(&node.identity);
                    if node.depth > 0 && (!node.expanded || !node.has_children) {
                        self.expanded_nodes.remove(&self.tree.nodes[node.parent.0].identity);
                    }
                }
                // TODO: For non-root nodes, on Enter add a new watch for the corresponding field/element.
                KeyAction::Enter if expr_idx.is_some_and(|i| self.expressions[i].is_editable()) => {
                    let expr = &mut self.expressions[expr_idx.unwrap()];
                    if let Some((identity, _, input)) = &mut self.text_input {
                        if node.identity == *identity {
                            expr.text = input.text.clone();
                            let empty = expr.text.is_empty();
                            expr.special = None;
                            let i = expr_idx.unwrap();
                            if i + 1 < self.expressions.len() {
                                self.cursor_path = vec![self.expressions[i+1].identity];
                            }

                            if empty {
                                self.expressions.remove(i);
                                self.tree.roots.remove(i);
                            } else {
                                refresh_data = true;
                            }
                        }
                        self.text_input = None;
                    } else {
                        let height_estimate = (node.node_end_y - node.start_y).max(1) as usize;
                        let text = if expr.special.is_some() {String::new()} else {expr.text.clone()};
                        self.text_input = Some((node.identity, height_estimate, TextInput::new_multiline(text)));
                    }
                }
                KeyAction::DeleteRow if expr_idx.is_some_and(|i| self.expressions[i].special.is_none()) => {
                    let i = expr_idx.unwrap();
                    self.expressions.remove(i);
                    self.tree.roots.remove(i); // this way we don't have to refresh_data
                    if !self.expressions.is_empty() {
                        self.cursor_path = vec![self.expressions[i.min(self.expressions.len()-1)].identity];
                    }
                }
                KeyAction::DuplicateRow if expr_idx.is_some_and(|i| !self.expressions[i].special.is_none()) => {
                    let i = expr_idx.unwrap();
                    let text = self.expressions[i].text.clone();
                    let identity: usize = random();
                    self.expressions.push(WatchExpression {identity, text, special: None});
                    self.cursor_path = vec![identity];
                    refresh_data = true;
                }
                _ => (),
            }
        }

        // Add the "<add watch>" placeholder watch.
        if self.expressions.iter().position(|e| &e.special == &Some(SpecialWatch::AddWatch)).is_none() {
            let identity: usize = random();
            self.expressions.push(WatchExpression {identity, text: "<add watch>".to_string(), special: Some(SpecialWatch::AddWatch)});
            self.cursor_path = vec![identity];
            refresh_data = true;
        }

        // See if we're able to evaluate expressions.
        let eval_context: Result<EvalContext> = match debugger.threads.get(&state.selected_thread) {
            None => err!(ProcessState, "no process"),
            Some(thread) if thread.state != ThreadState::Suspended || state.stack.frames.is_empty() => err!(ProcessState, "running"),
            Some(thread) => {
                let t = (state.selected_thread, state.selected_subframe, thread.stop_count);
                if t != self.seen {
                    self.seen = t;
                    refresh_data = true;
                }
                Ok(debugger.make_eval_context(&state.stack, state.selected_subframe))
            }
        };

        // Recalculate everything if needed.
        if refresh_data {
            self.tree.clear();
            self.eval_state.clear();
            self.scroll_to_cursor = true;

            if let Ok(context) = &eval_context {
                self.eval_state.update(context);
            }
            self.eval_watches(eval_context.as_ref().ok(), &ui.palette);
        }

        // Create container widgets.
        ui.cur_mut().set_hstack();
        let header_and_viewport = ui.add(widget!().width(AutoSize::Remainder(1.0)).vstack());
        let scroll_bar = ui.add(widget!().fixed_width(1));
        ui.layout_children(Axis::X);
        let viewport_width = ui.calculate_size(header_and_viewport, Axis::X);
        let name_width = viewport_width * 4 / 10;
        let value_width = viewport_width.saturating_sub(name_width + 1);
        let viewport;
        with_parent!(ui, header_and_viewport, {
            if !ui.check_focus() {
                self.text_input = None;
            }
            ui.focus();

            with_parent!(ui, ui.add(widget!().fixed_height(1)), {
                let l = ui_writeln!(ui, table_header, "  name");
                ui.add(widget!().text(l).fixed_width(name_width));
                let l = ui_writeln!(ui, table_header, "value");
                ui.add(widget!().text(l).fixed_width(value_width).fixed_x(name_width as isize + 1));
            });

            viewport = ui.add(widget!().height(AutoSize::Remainder(1.0)));
            ui.layout_children(Axis::Y);
        });
        let indent_width = str_width(&ui.palette.tree_indent.0).max(1);
        let max_indent = name_width / 2 / indent_width;
        let row_height_limit = ui.calculate_size(viewport, Axis::Y);

        if (name_width, value_width, indent_width, max_indent, row_height_limit) != (self.name_width, self.value_width, self.indent_width, self.max_indent, self.row_height_limit) {
            (self.name_width, self.value_width, self.indent_width, self.max_indent, self.row_height_limit) = (name_width, value_width, indent_width, max_indent, row_height_limit);
            self.scroll_to_cursor = true;

            // Clear line wrapping cache.
            for node in &mut self.tree.nodes {
                node.line_wrapped_name = None;
                node.formatted_value[2] = None;
            }
        }

        // Determine positions and sizes of all nodes, do line wrapping (unless cached). Also assign cursor_idx.
        // We currently do this on each frame because determining if anything changed would be error-prone.
        let content_height = self.do_layout(eval_context.as_ref().ok(), &ui.palette, ui.frame_idx);

        // Do cursor movement and scrolling.
        let visible_y: Range<isize>;
        let content_widget;
        let root_widget = ui.cur_parent;
        with_parent!(ui, viewport, {
            content_widget = ui.add(widget!().fixed_height(content_height));

            ui.focus();
            self.scroll_to_cursor |= list_cursor_navigation_with_variable_row_height(&mut self.cursor_idx, self.tree.rows.len(), |i, ui| {
                let n = &self.tree.nodes[self.tree.rows[i].0];
                (n.node_end_y - n.start_y) as usize
            }, ui);

            let scroll_to = if mem::take(&mut self.scroll_to_cursor) && !self.tree.rows.is_empty() {
                let n = &self.tree.nodes[self.tree.rows[self.cursor_idx].0];
                Some(n.start_y..n.node_end_y)
            } else {
                None
            };
            visible_y = scrolling_navigation(&mut self.scroll, scroll_to, root_widget, scroll_bar, ui);
        });

        // Convert cursor_idx back to cursor_path.
        if !self.tree.rows.is_empty() {
            self.set_cursor_path_from_node(self.tree.rows[self.cursor_idx]);
        }

        // Create widgets for visible nodes, handle mouse clicks.
        with_parent!(ui, content_widget, {
            self.build_widgets(visible_y, &eval_context, ui);
        });

        if self.text_input.is_some() && !self.text_input_built {
            self.text_input = None;
            ui.should_redraw = true;
        }
    }

    fn drop_caches(&mut self) {
        self.tree.clear();
        self.eval_state.clear();
        self.seen.2 = usize::MAX;
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.extend([
            KeyHint::key(KeyAction::Enter, "edit"),
            KeyHint::key(KeyAction::DuplicateRow, "duplicate"),
            KeyHint::key(KeyAction::DeleteRow, "delete"),
            KeyHint::keys(&[KeyAction::CursorRight, KeyAction::CursorLeft], "expand/collapse"),
        ]);
    }

    fn has_persistent_state(&self) -> bool {
        true
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        for expr in &self.expressions {
            if expr.special.is_none() {
                out.write_u8(1)?;
                out.write_str(&expr.text)?;
            }
        }
        out.write_u8(0)?;
        Ok(())
    }
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        while inp.read_u8()? != 0 {
            let text = inp.read_str()?;
            self.expressions.push(WatchExpression {identity: random(), text, special: None});
        }
        Ok(())
    }
}

// TODO: Add Type::MetaVariable instead and allow inspecting variables in watches window like types and fields.
#[derive(Default)]
struct LocationsWindow {
    table_state: TableState,
}
impl WindowContent for LocationsWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        ui.cur_mut().set_vstack();
        let l = ui_writeln!(ui, default_dim, "(for address selected in disassembly window)");
        ui.add(widget!().text(l).height(AutoSize::Text));
        let table_widget = ui.add(widget!().height(AutoSize::Remainder(1.0)));
        ui.layout_children(Axis::Y);

        with_parent!(ui, table_widget, {
            let mut table = Table::new(mem::take(&mut self.table_state), ui, vec![
                Column::new("name", AutoSize::Remainder(0.25)),
                Column::new("type", AutoSize::Remainder(0.3)),
                Column::new("expression", AutoSize::Remainder(0.3)),
                Column::new("die", AutoSize::Fixed(9)),
            ]);
            if let Err(e) = self.fill_table(&mut table, state, debugger, ui) {
                table.start_row(hash(&'e'), ui);
                table.empty_cell(ui);
                table.empty_cell(ui);
                ui_writeln!(ui, error, "{}", e);
                table.text_cell(ui);
                table.empty_cell(ui);
            }
            self.table_state = table.finish(ui);
        });
    }
}

impl LocationsWindow {
    fn fill_table(&mut self, table: &mut Table, state: &mut UIState, debugger: &Debugger, ui: &mut UI) -> Result<()> {
        let (binary_id, function_idx, addr) = match state.selected_addr.clone() { Some(x) => x, None => return Ok(()) };
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

        let mut row_idx = 0;
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

                    table.start_row(row_idx, ui);
                    row_idx += 1;

                    ui_writeln!(ui, default, "{}", unsafe {v.name()});
                    table.text_cell(ui);

                    print_type_name(v.type_, &mut ui.text, &ui.palette, 0);
                    ui.text.close_line();
                    table.text_cell(ui);

                    match expr_str {
                        Ok(s) => ui_writeln!(ui, default, "{}", s),
                        Err(e) => ui_writeln!(ui, error, "{}", e),
                    };
                    table.text_cell(ui);

                    ui_writeln!(ui, default, "{:x}", v.offset().0);
                    table.text_cell(ui);
                }
            }
        }
        Ok(())
    }
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
                    if tab.ephemeral && tab.locator.is_some() {
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
                    KeyAction::PreviousMatch => tab.selected_subfunction_level = tab.selected_subfunction_level.min(disas_line.subfunction_level).saturating_sub(1),
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
            let level = tab.selected_subfunction_level.min(disas_line.subfunction_level);
            if let &Some(mut sf_idx) = &disas_line.subfunction {
                while symbols_shard.subfunctions[sf_idx].level > level.saturating_add(1) {
                    sf_idx = symbols_shard.subfunctions[sf_idx].parent;
                }
                if symbols_shard.subfunctions[sf_idx].level == level.saturating_add(1) {
                    source_line_info = Some(symbols_shard.subfunctions[sf_idx].call_line.clone());
                }

                while symbols_shard.subfunctions[sf_idx].level > level {
                    sf_idx = symbols_shard.subfunctions[sf_idx].parent;
                }
                if symbols_shard.subfunctions[sf_idx].level == level {
                    selected_subfunction_idx = Some(sf_idx);
                }
            }
            if let Some(line) = source_line_info {
                let file = &symbols.files[line.file_idx().unwrap()];
                source_line = Some(SourceScrollTarget {path: file.path.to_owned(), version: file.version.clone(), line: line.line()});
            }
        }
        let key = (symbols.identity, function_idx, tab.area_state.cursor, tab.selected_subfunction_level);
        if self.source_scrolled_to.as_ref() != Some(&key) {
            self.source_scrolled_to = Some(key);
            if !suppress_code_autoscroll {
                if let Some(target) = source_line {
                    state.should_scroll_source = Some((Some(target), false));
                    ui.should_redraw = true;
                }
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
            if tab.ephemeral {
                continue;
            }
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
            self.tabs.push(DisassemblyTab {identity: random(), title, locator: Some(locator), error: None, area_state: AreaState::load_state(inp)?, selected_subfunction_level: inp.read_u16()?, ephemeral: false, cached_function_idx: None});
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

        let mut table = Table::new(mem::take(&mut self.table_state), ui, vec![Column::new("idx", AutoSize::Fixed(3)), Column::new("name", AutoSize::Remainder(1.0)), Column::new("offset", AutoSize::Fixed(12)), Column::new("file", AutoSize::Fixed(PrettySize::MAX_LEN))]);
        table.hide_cursor_if_unfocused = true;
        for (idx, id) in state.binaries.iter().enumerate() {
            let binary = debugger.symbols.get_if_present(id).unwrap();
            let mapped_binary = debugger.info.binaries.get(id);
            table.start_row(hash(id), ui);

            ui_writeln!(ui, default_dim, "{}", idx + 1);
            table.text_cell(ui);

            // Path, progress bar, error message.
            with_parent!(ui, table.start_cell(ui), {
                ui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);

                // Path.
                let style = if mapped_binary.is_some() {ui.palette.default} else {ui.palette.default_dim};
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

            if let &Some(b) = &mapped_binary {
                ui_writeln!(ui, default_dim, "{:>12x}", b.addr_map.static_to_dynamic(0) & 0xffffffffffff);
            }
            table.text_cell(ui);

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

#[derive(Default)]
struct CodeTab {
    title: String,
    identity: usize,
    path_in_symbols: PathBuf, // empty if not known
    version_in_symbols: FileVersionInfo,
    ephemeral: bool,
    area_state: AreaState,
}

struct SourceFile {
    header: StyledText, // can contain errors or warnings, not scrollable
    text: StyledText, // includes virtual space to cover all lines+columns mentioned in debug symbols
    local_path: PathBuf, // empty if error
    widest_line: usize,
    num_lines_in_local_file: usize,
}

#[derive(Default)]
struct CodeWindow {
    tabs: Vec<CodeTab>,
    tabs_state: TabsState,
    file_cache: HashMap<(PathBuf, FileVersionInfo), SourceFile>,

    search_dialog: Option<SearchDialog>,

    // When this changes (usually because the user moved the cursor around the file), we scroll disassembly to the address corresponding to the selected line.
    disassembly_scrolled_to: Option<(PathBuf, FileVersionInfo, /*cursor*/ usize)>,
}
// TODO: Column cursor mode, to set breakpoints on columns. And/or clickable statements.
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
            assert_eq!(*text.lines.last().unwrap(), text.num_spans());
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

    fn switch_to_file(&mut self, path_in_symbols: &Path, version: &FileVersionInfo, debugger: &Debugger) {
        if let Some(i) = self.tabs.iter().position(|t| &t.path_in_symbols == path_in_symbols && &t.version_in_symbols == version) {
            self.tabs_state.select(i);
            return;
        }
        let title = Self::make_title(path_in_symbols);
        self.tabs.push(CodeTab {identity: random(), title, path_in_symbols: path_in_symbols.to_owned(), version_in_symbols: version.clone(), area_state: AreaState::default(), ephemeral: true});
        self.tabs_state.select(self.tabs.len() - 1);
    }

    fn toggle_breakpoint(&mut self, disable: bool, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        if self.tabs.is_empty() {
            return;
        }
        let tab = &self.tabs[self.tabs_state.selected];
        if tab.path_in_symbols.as_os_str().is_empty() {
            return;
        }
        ui.should_redraw = true;
        for (id, breakpoint) in debugger.breakpoints.iter() {
            match &breakpoint.on {
                BreakpointOn::Line(bp) if bp.path == tab.path_in_symbols && (bp.line == tab.area_state.cursor + 1 || bp.adjusted_line == Some(tab.area_state.cursor + 1)) => {
                    if disable {
                        let r = debugger.set_breakpoint_enabled(id, false);
                        report_result(state, &r);
                    } else if !breakpoint.enabled {
                        let r = debugger.set_breakpoint_enabled(id, true);
                        report_result(state, &r);
                    } else {
                        debugger.remove_breakpoint(id);
                    }
                    return;
                }
                _ => (),
            }
        }
        if disable {
            state.last_error = "no breakpoint".to_string();
        } else {
            let r = debugger.add_breakpoint(BreakpointOn::Line(LineBreakpoint {path: tab.path_in_symbols.clone(), file_version: tab.version_in_symbols.clone(), line: tab.area_state.cursor + 1, adjusted_line: None}));
            report_result(state, &r);
        }
    }

    fn evict_cache(&mut self) {
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
        match self.tabs.get(self.tabs_state.selected) {
            Some(t) if !t.path_in_symbols.as_os_str().is_empty() => Some((t.path_in_symbols.clone(), t.version_in_symbols.clone(), t.area_state.cursor)),
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
            let mut addrs: Vec<(/*function_idx*/ usize, /*subfunction_level*/ u16, /*static_addr*/ usize)> = Vec::new();
            for (line, level) in addrs0 {
                let static_addr = line.addr();
                if let Ok((_, function_idx)) = symbols.addr_to_function(static_addr) {
                    addrs.push((function_idx, level, static_addr));
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
            let mut closest_idx = addrs.iter().position(|&(function_idx, level, static_addr)| {
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
                let idx = ranges.partition_point(|r| r.range.end <= static_addr);
                idx == ranges.len() || ranges[idx].range.start > static_addr
            }).unwrap_or(0);
            let mut selected_idx: Option<usize> = None;
            if let Some((binary_id, selected_function_idx, selected_addr)) = &state.selected_addr {
                if let Some(binary) = debugger.info.binaries.get(binary_id) {
                    let selected_static_addr = binary.addr_map.dynamic_to_static(*selected_addr);
                    let symbols = binary.symbols.as_ref().unwrap();
                    let function = &symbols.functions[*selected_function_idx];
                    let shard = &symbols.shards[function.shard_idx()];
                    let mut subfunction_ranges: Vec<Range<usize>> = Vec::new();
                    for level in 1..function.num_levels().max(1) {
                        let pc_ranges = symbols.subfunction_ranges_at_level(level, function);
                        let idx = pc_ranges.partition_point(|r| r.range.end <= selected_static_addr);
                        if idx == pc_ranges.len() || pc_ranges[idx].range.start > selected_static_addr {
                            break;
                        }
                        subfunction_ranges.push(pc_ranges[idx].range.clone());
                    }
                    let mut closest = (0usize, 0usize);
                    for (idx, &(function_idx, _, static_addr)) in addrs.iter().enumerate() {
                        if static_addr == selected_static_addr {
                            selected_idx = Some(idx);
                        }
                        if function_idx != *selected_function_idx {
                            continue;
                        }
                        let mut level = 0usize;
                        while level < subfunction_ranges.len() && subfunction_ranges[level].contains(&static_addr) {
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

            state.should_scroll_disassembly = Some((Ok(DisassemblyScrollTarget {binary_id: binary_id.clone(), symbols_identity: symbols.identity, function_idx: addrs[idx].0, static_pseudo_addr: addrs[idx].2, subfunction_level: addrs[idx].1}), false));

            break;
        }
    }

    fn build_search_dialog(&mut self, create: bool, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        let dialog_widget = match make_dialog_frame(create, AutoSize::Remainder(0.75), AutoSize::Remainder(0.83), ui.palette.dialog, ui.palette.filename, "find file (by path from debug info)", ui) {
            None => {
                self.search_dialog = None;
                return;
            }
            Some(x) => x };

        let d = self.search_dialog.get_or_insert_with(|| SearchDialog::new(Arc::new(FileSearcher), debugger.context.clone()));
        with_parent!(ui, dialog_widget, {
            d.build(&debugger.symbols, None, ui);
        });

        if d.should_close_dialog {
            ui.close_dialog();
        }

        if let Some(res) = mem::take(&mut d.should_open_document) {
            let file = &res.symbols.files[res.id];
            self.switch_to_file(&res.file, &file.version, debugger);
            self.tabs[self.tabs_state.selected].ephemeral = false;
        }
    }
}
impl WindowContent for CodeWindow {
    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
        let mut open_dialog = false;
        // TODO: Move CloseTab logic into Tabs, next to reordering logic. Also close tabs with middle click.
        for action in ui.check_keys(&[KeyAction::Open, KeyAction::CloseTab]) {
            match action {
                KeyAction::Open if self.search_dialog.is_none() => open_dialog = true,
                KeyAction::CloseTab if self.tabs.get(self.tabs_state.selected).is_some() => {
                    let tab = &mut self.tabs[self.tabs_state.selected];
                    if tab.ephemeral && !tab.path_in_symbols.as_os_str().is_empty() {
                        tab.ephemeral = false;
                    } else {
                        self.tabs.remove(self.tabs_state.selected);
                    }
                }
                _ => (),
            }
        }

        self.build_search_dialog(open_dialog, state, debugger, ui);

        let suppress_disassembly_autoscroll = state.should_scroll_source.is_some();
        let switch_to = match mem::take(&mut state.should_scroll_source) {
            Some((to, false)) => Some(to),
            Some((to, true)) if self.tabs.is_empty() || self.tabs[self.tabs_state.selected].path_in_symbols.as_os_str().is_empty() => Some(to),
            _ => None,
        };

        let mut select_disassembly_address: isize = 0;
        for action in ui.check_keys(&[KeyAction::ToggleBreakpoint, KeyAction::DisableBreakpoint, KeyAction::PreviousMatch, KeyAction::NextMatch]) {
            match action {
                KeyAction::ToggleBreakpoint => self.toggle_breakpoint(false, state, debugger, ui),
                KeyAction::DisableBreakpoint => self.toggle_breakpoint(true, state, debugger, ui),
                KeyAction::PreviousMatch => select_disassembly_address -= 1,
                KeyAction::NextMatch => select_disassembly_address += 1,
                _ => (),
            }
        }

        match switch_to {
            None => (),
            Some(None) => self.switch_to_file(Path::new(""), &FileVersionInfo::default(), debugger),
            Some(Some(target)) => {
                self.switch_to_file(&target.path, &target.version, debugger);
                let tab = &mut self.tabs[self.tabs_state.selected];
                tab.area_state.select(target.line.saturating_sub(1));
            }
        }

        close_excess_ephemeral_tabs(&mut self.tabs, &mut self.tabs_state, |t| t.ephemeral);
        self.evict_cache();

        ui.cur_mut().set_vstack();
        with_parent!(ui, ui.add(widget!().fixed_height(1)), {
            ui.focus();
            let mut tabs = Tabs::new(mem::take(&mut self.tabs_state), ui);
            for tab in &self.tabs {
                let full_title = tab.path_in_symbols.as_os_str().to_string_lossy().into_owned();
                tabs.add(Tab {identity: tab.identity, short_title: tab.title.clone(), full_title, ephemeral: tab.ephemeral, ..Default::default()}, ui);
            }
            self.tabs_state = tabs.finish(ui);
        });
        let content_root = ui.add(widget!().height(AutoSize::Remainder(1.0)));
        with_parent!(ui, content_root, {
            ui.multifocus();
        });
        ui.layout_children(Axis::Y);

        // (path_in_symbols, line) -> (column, selected, top)
        let mut instruction_pointers: HashMap<(&Path, usize), (usize, bool, bool)> = HashMap::new();
        for (idx, subframe) in state.stack.subframes.iter().enumerate() {
            if let Some(info) = &subframe.line {
                instruction_pointers.insert((&info.path, info.line.line()), (info.line.column(), idx == state.selected_subframe, idx == 0));
            }
        }

        let tab = match self.tabs.get_mut(self.tabs_state.selected) {
            None => return,
            Some(t) => t };
        let file = Self::find_or_open_file(&mut self.file_cache, &tab.path_in_symbols, &tab.version_in_symbols, debugger, &ui.palette);

        let line_num_len = (file.text.num_lines().saturating_add(1) as f64).log10().ceil() as usize;
        let prefix_width = line_num_len + 2 + 2 + 1;

        let (content, visible_y) = with_parent!(ui, content_root, {
            let header = ui.text.import_lines(&file.header, 0..file.header.num_lines());
            build_biscrollable_area_with_header(header, [prefix_width + file.widest_line, file.text.num_lines()], &mut tab.area_state, ui)
        });

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

        with_parent!(ui, content, {
            let line_range = visible_y.start.max(0) as usize .. (visible_y.end.max(0) as usize).min(file.text.num_lines());
            for i in line_range.clone() {
                let mut column_number = 0usize;

                match instruction_pointers.get(&(&tab.path_in_symbols, i + 1)) {
                    None => ui_write!(ui, default, "  "),
                    Some(&(col, is_selected, is_top)) => {
                        column_number = col;
                        let style = if is_selected {ui.palette.instruction_pointer} else {ui.palette.additional_instruction_pointer};
                        styled_write!(ui.text, style, "⮕ ");
                    }
                };

                let idx = breakpoint_lines.partition_point(|t| t.line < i + 1);
                if idx < breakpoint_lines.len() && breakpoint_lines[idx].line == i + 1 {
                    let l = &breakpoint_lines[idx];
                    if !l.enabled {
                        ui_write!(ui, secondary_breakpoint, "○ ");
                    } else {
                        let active = l.active && l.has_locations;
                        let style = if l.adjusted {ui.palette.secondary_breakpoint} else {ui.palette.breakpoint};
                        let s = if active { "● " } else { "○ " };
                        styled_write!(ui.text, style, "{}", s);
                    }
                } else {
                    ui_write!(ui, default, "  ");
                }

                ui_write!(ui, default_dim, "{: >2$}{}", i + 1, if i < file.num_lines_in_local_file {" "} else {"~"}, line_num_len);

                // Underline the character at the column number.
                let s = file.text.get_line_str(i);
                let spans = file.text.get_line(i);
                if column_number == 0 {
                    ui.text.import_spans(&file.text, spans);
                } else if column_number - 1 >= s.len() {
                    ui.text.import_spans(&file.text, spans);
                    ui_write!(ui, default, "{:<1$}", "", column_number - 1 - s.len());
                    let style = ui.palette.code_instruction_pointer_column.apply(ui.palette.default);
                    styled_write!(ui.text, style, " ");
                } else {
                    let mut start = column_number - 1;
                    while !s.is_char_boundary(start) {
                        start -= 1;
                    }
                    let mut end = start + 1;
                    while !s.is_char_boundary(end) {
                        end += 1;
                    }
                    let spans = file.text.get_line(i);
                    ui.text.import_substring(&file.text, spans.clone(), 0..start);
                    let mid = ui.text.import_substring(&file.text, spans.clone(), start..end);
                    ui.text.import_substring(&file.text, spans.clone(), end..s.len());
                    ui.text.adjust_spans_style(mid, ui.palette.code_instruction_pointer_column);
                }

                let l = ui.text.close_line();
                let mut w = widget!().fixed_height(1).fixed_y(i as isize).text(l).fill(' ', ui.palette.default).flags(WidgetFlags::HSCROLL_INDICATOR_RIGHT).highlight_on_hover();
                if i == tab.area_state.cursor {
                    w.style_adjustment.update(ui.palette.selected);
                }
                ui.add(w);
            }
        });

        self.scroll_disassembly_if_needed(suppress_disassembly_autoscroll, select_disassembly_address, state, debugger);
    }

    fn drop_caches(&mut self) {
        self.file_cache.clear();
    }

    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.extend([
            KeyHint::key(KeyAction::Open, "open file"),
            KeyHint::key(KeyAction::CloseTab, "close/pin tab"),
            KeyHint::keys(&[KeyAction::ToggleBreakpoint, KeyAction::DisableBreakpoint], "toggle/disable breakpoint"),
            KeyHint::keys(&[KeyAction::PreviousMatch, KeyAction::NextMatch], "cycle disasm addrs"),
        ]);
    }

    fn has_persistent_state(&self) -> bool {
        true
    }
    fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        for (i, tab) in self.tabs.iter().enumerate() {
            if tab.ephemeral {
                continue;
            }
            out.write_u8(if i == self.tabs_state.selected {2} else {1})?;
            out.write_path(&tab.path_in_symbols)?;
            tab.version_in_symbols.save_state(out)?;
            tab.area_state.save_state(out)?;
        }
        out.write_u8(0)?;
        Ok(())
    }
    fn load_state(&mut self, inp: &mut &[u8]) -> Result<()> {
        loop {
            match inp.read_u8()? {
                1 => (),
                2 => self.tabs_state.select(self.tabs.len()),
                _ => break,
            }
            let path_in_symbols = inp.read_path()?;
            let title = Self::make_title(&path_in_symbols);
            self.tabs.push(CodeTab {identity: random(), title, path_in_symbols, version_in_symbols: FileVersionInfo::load_state(inp)?, area_state: AreaState::load_state(inp)?, ephemeral: false});
        }
        // Prevent auto-scrolling the disassembly window before any input is made.
        self.disassembly_scrolled_to = self.disassembly_scroll_key();
        Ok(())
    }
}

#[derive(Default)]
struct BreakpointsWindow {
    table_state: TableState,
    selected_breakpoint: Option<BreakpointId>,
}
impl WindowContent for BreakpointsWindow {
    fn get_key_hints(&self, out: &mut Vec<KeyHint>) {
        out.extend([KeyHint::key(KeyAction::DeleteRow, "delete breakpoint"), KeyHint::key(KeyAction::ToggleBreakpoint, "enable/disable breakpoint")]);
    }

    fn build(&mut self, state: &mut UIState, debugger: &mut Debugger, ui: &mut UI) {
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

        let mut table = Table::new(mem::take(&mut self.table_state), ui, vec![
            Column::new("idx", AutoSize::Text),
            Column::new("", AutoSize::Fixed(2)),
            Column::new("on", AutoSize::Remainder(1.0)),
            Column::new("locs", AutoSize::Text),
            Column::new("hits", AutoSize::Text),
        ]);
        table.hide_cursor_if_unfocused = true;

        for action in ui.check_keys(&[KeyAction::DeleteRow, KeyAction::ToggleBreakpoint, KeyAction::DisableBreakpoint, KeyAction::Enter]) {
            let id = match &self.selected_breakpoint {
                None => continue,
                &Some(x) => x };
            let breakpoint = match debugger.breakpoints.try_get(id) {
                None => continue,
                Some(x) => x };
            match action {
                KeyAction::DeleteRow => {debugger.remove_breakpoint(id);}
                KeyAction::ToggleBreakpoint | KeyAction::DisableBreakpoint | KeyAction::Enter => {
                    let enable = !breakpoint.enabled;
                    let r = debugger.set_breakpoint_enabled(id, enable);
                    report_result(state, &r);
                }
                _ => (),
            }
        }

        let mut breakpoints: Vec<BreakpointId> = debugger.breakpoints.iter().map(|p| p.0).collect();
        breakpoints.sort_by_key(|id| id.seqno);

        if let &Some(id) = &self.selected_breakpoint {
            if let Some(idx) = breakpoints.iter().position(|b| b == &id) {
                self.table_state.cursor = idx;
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

        for &id in &breakpoints {
            let b = debugger.breakpoints.get(id);
            let is_hit = hit_breakpoints.binary_search(&id).is_ok();
            let locs_begin = locations.partition_point(|t| t.0 < id);
            let locs_end = locations.partition_point(|t| t.0 <= id);

            let row_widget = table.start_row(id.seqno, ui);

            ui_writeln!(ui, default_dim, "{}", id.seqno);
            table.text_cell(ui);

            if is_hit {
                ui_writeln!(ui, instruction_pointer, "⮕ ");
            } else if !b.enabled {
                ui_writeln!(ui, secondary_breakpoint, "○ ");
            } else if !b.active || locs_begin == locs_end {
                ui_writeln!(ui, breakpoint, "○ ");
            } else {
                ui_writeln!(ui, breakpoint, "● ");
            }
            with_parent!(ui, table.text_cell(ui), {
                ui.cur_mut().flags.insert(WidgetFlags::HIGHLIGHT_ON_HOVER);
                if ui.check_mouse(MouseActions::CLICK) {
                    let enabled = b.enabled;
                    let r = debugger.set_breakpoint_enabled(id, !enabled);
                    report_result(state, &r);
                    ui.should_redraw = true;
                }
            });
            let b = debugger.breakpoints.get(id);

            match &b.on {
                BreakpointOn::Line(on) => {
                    let name = on.path.as_os_str().to_string_lossy();
                    ui_write!(ui, filename, "{}", name);
                    ui_write!(ui, line_number, ":{}", on.line);
                    if let &Some(adj) = &on.adjusted_line {
                        ui_write!(ui, default_dim, "→");
                        ui_write!(ui, line_number, "{}", adj);
                    }
                }
            }
            let l = ui.text.close_line();
            with_parent!(ui, table.start_cell(ui), {
                ui.cur_mut().flags.insert(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT);
                ui.cur_mut().axes[Axis::Y].auto_size = AutoSize::Text;
                ui.cur_mut().draw_text = Some(l..l+1);
            });

            ui_writeln!(ui, default_dim, "{}", locs_end - locs_begin);
            table.text_cell(ui);

            ui_writeln!(ui, default_dim, "{}", b.hits);
            table.text_cell(ui);

            let error = b.addrs.as_ref().is_err_and(|e| !e.is_not_calculated()) || locations[locs_begin..locs_end].iter().any(|t| t.1);
            if error {
                let a = ui.palette.breakpoint_error;
                ui.get_mut(row_widget).style_adjustment.update(a);
            }
        }

        self.table_state = table.finish(ui);

        self.selected_breakpoint = breakpoints.get(self.table_state.cursor).copied();

        if self.table_state.did_scroll_to_cursor {
            if let &Some(id) = &self.selected_breakpoint {
                match &debugger.breakpoints.get(id).on {
                    BreakpointOn::Line(on) => {
                        state.should_scroll_source = Some((Some(SourceScrollTarget {path: on.path.clone(), version: on.file_version.clone(), line: on.line}), false));
                    }
                }
            }
        }
    }
}
