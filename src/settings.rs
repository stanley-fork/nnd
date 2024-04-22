use termion::event::Key;
use tui::style::{Style, Modifier, Color};
use std::collections::HashMap;

pub struct Settings {
    pub tab_width: usize,
    pub stop_on_initial_exec: bool,
    pub fps: f64,
    pub loading_fps: f64, // how often to re-render when there's a progress bar on screen
    pub max_threads: usize,
    pub save_period_seconds: f64, // how often to save state to file (watches, breakpoints, etc); also saved on clean exit (not on panic or crash)
    // TODO: Load keys and colors from config file(s). Maybe move them out of Settings to allow hot-reloading (for experimenting with colors quickly). Probably use separate files to make it easier to share themes.
    pub keys: KeyBindings,
    pub palette: Palette,

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
        palette: Palette::rgb_dark_theme(),

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

    DuplicateRow,

    DropCaches,
    ToggleProfiler,
}

pub struct KeyBindings {
    pub map: HashMap<Key, KeyAction>,
    // Can add a separate map for text editing, and maybe maps for other modes of some kind.
    // Currently most of the text editing keys are hardcoded.
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
            (Key::Alt('v'), KeyAction::PageUp),
            (Key::PageDown, KeyAction::PageDown),
            (Key::Ctrl('v'), KeyAction::PageDown),
            (Key::Home, KeyAction::Home),
            (Key::End, KeyAction::End),
            (Key::Char('s'), KeyAction::StepIntoLine),
            (Key::Char('S'), KeyAction::StepIntoInstruction),
            (Key::Char('n'), KeyAction::StepOverLine),
            (Key::Char('m'), KeyAction::StepOverColumn),
            (Key::Char('N'), KeyAction::StepOverInstruction),
            (Key::Char('f'), KeyAction::StepOut),
            (Key::Char('F'), KeyAction::StepOutNoInline),
            (Key::Ctrl('t'), KeyAction::NextTab),
            (Key::Ctrl('b'), KeyAction::PreviousTab),
            (Key::Ctrl('y'), KeyAction::PinTab),
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
            (Key::Char('d'), KeyAction::DuplicateRow),
            (Key::Ctrl('l'), KeyAction::DropCaches),
            (Key::Ctrl('p'), KeyAction::ToggleProfiler),
        ])}
    }

    pub fn find(&self, action: KeyAction) -> Option<Key> {
        self.map.iter().find(|(k, a)| **a == action).map(|(k, a)| *k)
    }
}

pub struct Palette {
    pub default: Style,
    pub default_dim: Style,
    pub window_border: Style,
    pub window_border_active: Style,
    pub window_hotkey: Style,
    pub error: Style,
    pub error_dim: Style,
    pub warning: Style,
    pub warning_dim: Style,
    pub status_error: Style,
    pub type_name: Style,
    pub value: Style,
    pub value_dubious: Style,
    pub value_warning: Style,
    pub value_error: Style,
    pub value_field_name: Style,
    pub value_misc: Style,
    pub value_misc_dim: Style,
    pub function_name: Style,
    pub location_filename: Style,
    pub location_line_number: Style,
    pub location_column_number: Style,
    pub table_header: Style,
    pub table_selected_item: Style,
    pub selected_text_line: Style,
    pub code_statement: Style,
    pub code_inlined_site: Style,
    pub code_line_number: Style,
    pub instruction_pointer: Style,
    pub additional_instruction_pointer: Style,
    pub breakpoint: Style,
    pub secondary_breakpoint: Style,
    pub dialog: Style,
    pub tab_title: Style,
    pub tab_title_active: Style,
    pub tab_title_selected: Style,
    pub state_in_progress: Style,
    pub state_suspended: Style,
    pub state_other: Style,
    pub progress_bar_done: Style,
    pub progress_bar_remaining: Style,
    pub search_bar_dim: Style,
    pub url: Style,
    pub disas_default: Style,
    pub disas_keyword: Style,
    pub disas_mnemonic: Style,
    pub disas_register: Style,
    pub disas_number: Style,
    pub disas_function: Style,
    pub disas_jump_arrow: Style,
    pub disas_relative_address: Style,
}

impl Palette {
    fn rgb_dark_theme() -> Self {
        // If you want a theme that uses terminal colors instead of hard-coded RGB, this can be a starting point:
        // replace these variables with corresponding Color::* named colors, and find some replacements for the remaining Color::Rgb-s below.
        let black = Color::Rgb(0, 0, 0);
        let white = Color::Rgb(0xff, 0xff, 0xff);
        let red = Color::Rgb(0xaa, 0, 0);
        let green = Color::Rgb(0, 0xaa, 0);
        let blue = Color::Rgb(0x63, 0x84, 0xff);
        let yellow = Color::Rgb(0xaa, 0x55, 0);
        let cyan = Color::Rgb(0, 0xaa, 0xaa);
        let magenta = Color::Rgb(0xaa, 0, 0xaa);
        let dark_gray = Color::Rgb(0x55, 0x55, 0x55);
        let light_green = Color::Rgb(0x55, 0xff, 0x55);
        let light_blue = Color::Rgb(0x55, 0x55, 0xff);
        Self {
            default: Style::default(),
            default_dim: Style::default().add_modifier(Modifier::DIM),
            window_border: Style::default().add_modifier(Modifier::DIM),
            window_border_active: Style::default().add_modifier(Modifier::BOLD),
            window_hotkey: Style::default().add_modifier(Modifier::UNDERLINED),
            error: Style::default().bg(red).fg(black),
            error_dim: Style::default().bg(red).fg(black).add_modifier(Modifier::DIM),
            status_error: Style::default().add_modifier(Modifier::BOLD).fg(red),
            warning: Style::default().bg(yellow).fg(black),
            warning_dim: Style::default().bg(yellow).fg(black).add_modifier(Modifier::DIM),
            type_name: Style::default(),
            value: Style::default(),
            value_dubious: Style::default().add_modifier(Modifier::DIM),
            value_warning: Style::default().fg(yellow),
            value_error: Style::default().fg(red),
            value_field_name: Style::default().fg(green),
            value_misc: Style::default(),
            value_misc_dim: Style::default().add_modifier(Modifier::DIM),
            function_name: Style::default(),
            location_filename: Style::default().fg(cyan),
            location_line_number: Style::default().fg(green),
            location_column_number: Style::default().fg(green).add_modifier(Modifier::DIM),
            table_header: Style::default().add_modifier(Modifier::DIM),
            table_selected_item: Style::default().add_modifier(Modifier::BOLD),
            selected_text_line: Style::default().bg(Color::Rgb(30, 30, 30)),
            code_statement: Style::default().bg(dark_gray),
            code_inlined_site: Style::default().bg(light_blue),
            instruction_pointer: Style::default().fg(green).add_modifier(Modifier::BOLD),
            additional_instruction_pointer: Style::default().fg(blue),
            breakpoint: Style::default(),
            secondary_breakpoint: Style::default().fg(blue),
            code_line_number: Style::default(),
            dialog: Style::default().bg(Color::Rgb(20, 20, 20)),
            tab_title: Style::default().add_modifier(Modifier::DIM),
            tab_title_active: Style::default(),
            tab_title_selected: Style::default().bg(white).fg(black),
            state_in_progress: Style::default().bg(blue).fg(black),
            state_suspended: Style::default().bg(light_green).fg(black),
            state_other: Style::default().bg(yellow).fg(black),
            progress_bar_done: Style::default().bg(blue).fg(black),
            progress_bar_remaining: Style::default().bg(Color::Rgb(30, 30, 30)),
            search_bar_dim: Style::default().bg(Color::Rgb(30, 30, 30)).add_modifier(Modifier::DIM),
            url: Style::default().fg(blue),
            disas_default: Style::default(),
            disas_keyword: Style::default().add_modifier(Modifier::DIM),
            disas_mnemonic: Style::default().fg(green).add_modifier(Modifier::BOLD),
            disas_register: Style::default().fg(blue),
            disas_number: Style::default().fg(cyan),
            disas_function: Style::default().fg(magenta),
            disas_jump_arrow: Style::default(),
            disas_relative_address: Style::default().fg(cyan).add_modifier(Modifier::DIM),
        }
    }
}
