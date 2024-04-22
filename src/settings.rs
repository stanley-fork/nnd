use termion::event::Key;
use tui::style::Style;
use std::collections::HashMap;

pub struct Settings {
    pub tab_width: usize,
    pub stop_on_initial_exec: bool,
    pub fps: f64,
    pub loading_fps: f64, // how often to re-render when there's a progress bar on screen
    pub max_threads: usize,
    pub save_period_seconds: f64, // how often to save state to file (watches, breakpoints, etc); also saved on clean exit (not on panic or crash)
    pub keys: KeyBindings,

    pub stdin_file: Option<String>,
    pub stdout_file: Option<String>,
}

impl Default for Settings {
    fn default() -> Self { Settings {
        tab_width: 2,
        stop_on_initial_exec: true,
        fps: 144.0,
        loading_fps: 10.0,
        max_threads: 128,
        save_period_seconds: 1.0,
        keys: KeyBindings::defaults(),

        stdin_file: None,
        stdout_file: None,
    } }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub enum KeyAction {
    Quit,
    Run,
    Continue,
    Suspend,
    Kill,

    WindowUp,
    WindowDown,
    WindowLeft,
    WindowRight,
    Window(usize),

    // Arrow keys.
    CursorUp,
    CursorDown,
    CursorLeft,
    CursorRight,
    Enter, // return
    DeleteRow, // del
    Cancel, // escape
    PageUp,
    PageDown,
    Home,
    End,

    StepIntoLine,
    StepIntoInstruction,
    StepOverLine,
    StepOverColumn,
    StepOverInstruction,
    StepOut,
    StepOutNoInline,

    NextTab,
    PreviousTab,
    PinTab,
    NextStackFrame,
    PreviousStackFrame,
    NextThread,
    PreviousThread,
    NextMatch,
    PreviousMatch,

    Open,
    ToggleBreakpoint,
    ToggleBreakpointEnabledness,
    Find,

    DropCaches,
    ToggleProfiler,
}

//asdqwe use everywhere
pub struct KeyBindings {
    pub map: HashMap<Key, KeyAction>,
}

impl KeyBindings {
    pub fn defaults() -> Self {
        Self {map: HashMap::from([
            (Key::Char('q'), KeyAction::Quit),
            (Key::Char('r'), KeyAction::Run),
            (Key::Char('c'), KeyAction::Continue),
            (Key::Char('C'), KeyAction::Suspend),
            (Key::Ctrl('c'), KeyAction::Suspend),
            (Key::Ctrl('w'), KeyAction::WindowUp),
            (Key::Ctrl('s'), KeyAction::WindowDown),
            (Key::Ctrl('a'), KeyAction::WindowLeft),
            (Key::Ctrl('d'), KeyAction::WindowRight),
            (Key::Char('0'), KeyAction::Window(0)),
            (Key::Char('1'), KeyAction::Window(1)),
            (Key::Char('2'), KeyAction::Window(2)),
            (Key::Char('3'), KeyAction::Window(3)),
            (Key::Char('4'), KeyAction::Window(4)),
            (Key::Char('5'), KeyAction::Window(5)),
            (Key::Char('6'), KeyAction::Window(6)),
            (Key::Char('7'), KeyAction::Window(7)),
            (Key::Char('8'), KeyAction::Window(8)),
            (Key::Char('9'), KeyAction::Window(9)),
            (Key::Up, KeyAction::CursorUp),
            (Key::Down, KeyAction::CursorDown),
            (Key::Left, KeyAction::CursorLeft),
            (Key::Right, KeyAction::CursorRight),
            (Key::Char('\n'), KeyAction::Enter),
            (Key::Delete, KeyAction::DeleteRow),
            (Key::Backspace, KeyAction::DeleteRow),
            (Key::Esc, KeyAction::Cancel),
            (Key::Ctrl('g'), KeyAction::Cancel),
            (Key::PageUp, KeyAction::PageUp),
            (Key::PageDown, KeyAction::PageDown),
            (Key::Home, KeyAction::Home),
            (Key::End, KeyAction::End),
            (Key::Char('s'), KeyAction::StepIntoLine),
            (Key::Char('S'), KeyAction::StepIntoInstruction),
            (Key::Char('n'), KeyAction::StepOverLine),
            (Key::Char('m'), KeyAction::StepOverColumn),
            (Key::Char('N'), KeyAction::StepOverInstruction),
            (Key::Char('f'), KeyAction::StepOut),
            (Key::Char('F'), KeyAction::StepOutNoInline),
            (Key::Char('t'), KeyAction::NextTab),
            (Key::Char('b'), KeyAction::PreviousTab),
            (Key::Char('y'), KeyAction::PinTab),
            (Key::Char(']'), KeyAction::NextStackFrame),
            (Key::Char('['), KeyAction::PreviousStackFrame),
            (Key::Char('}'), KeyAction::NextThread),
            (Key::Char('{'), KeyAction::PreviousThread),
            (Key::Char('.'), KeyAction::NextMatch),
            (Key::Char(','), KeyAction::PreviousMatch),
            (Key::Char('o'), KeyAction::Open),
            (Key::Char('b'), KeyAction::ToggleBreakpoint),
            (Key::Char('B'), KeyAction::ToggleBreakpointEnabledness),
            (Key::Char('/'), KeyAction::Find),
            (Key::Ctrl('l'), KeyAction::DropCaches),
            (Key::Ctrl('p'), KeyAction::ToggleProfiler),
        ])}
    }
}

struct Palette {
    background: Style,
    window_border: Style,
    window_border_active: Style,
    error: Style,
    quiet_error: Style,
    status_error: Style,
    type_name: Style,
    value: Style,
    value_dim: Style,
    function_name: Style,
    filename: Style,
    line_number: Style,
    selected_list_item: Style,
    selected_text_line: Style,
    code_statement_start: Style,
    code_inlined_site: Style,
    code_instruction_pointer_column: Style,
    //asdqwe
}
