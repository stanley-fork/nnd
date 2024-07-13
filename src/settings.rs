use crate::{*, terminal::*, common_ui::*};
use std::collections::HashMap;

pub struct Settings {
    pub tab_width: usize,
    pub stop_on_initial_exec: bool,
    pub fps: f64,
    pub max_threads: usize,
    // We currently use one timer for a few periodic tasks:
    //  * refreshing resource stats (cpu and memory usage) for each thread and total,
    //  * saving state to file (watches, breakpoints, etc) if changed (state is also saved on clean exit, but not on panic or crash),
    //  * rendering progress bar movement, // TODO: use animation system instead (after implementing animation system)
    //  * rotating profiling time series buckets.
    pub periodic_timer_seconds: f64,
    pub mouse_mode: MouseMode,

    pub stdin_file: Option<String>,
    pub stdout_file: Option<String>,
    pub stderr_file: Option<String>,
}

impl Default for Settings {
    fn default() -> Self { Settings {
        tab_width: 2,
        stop_on_initial_exec: true,
        fps: 144.0,
        max_threads: 128,
        periodic_timer_seconds: 0.25,
        mouse_mode: MouseMode::Full,

        stdin_file: None,
        stdout_file: None,
        stderr_file: None,
    } }
}

pub struct Palette {
    pub default: Style,
    pub default_dim: Style,
    pub error: Style,
    pub warning: Style,

    pub running: Style,
    pub suspended: Style,
    pub function_name: Style,
    pub filename: Style,
    pub line_number: Style,
    pub column_number: Style,
    pub type_name: Style,
    pub field_name: Style,
    pub keyword: Style,
    pub hotkey: StyleAdjustment,

    pub state_running: Style,
    pub state_suspended: Style,
    pub state_other: Style,

    // How to highlight things like selected table row or selected code line.
    pub selected: StyleAdjustment,
    // How to highlight clickable widgets on mouse hover.
    pub hovered: StyleAdjustment,
    // How to draw widgets with REDRAW_IF_VISIBLE flag, as a sort of loading indicator. It should be visible very rarely and only for one frame.
    pub placeholder_fill: Option<(char, Style)>,
    // What to prepend/append when text is truncated on the left/right. E.g. '…'.
    pub truncation_indicator: (/*left*/ String, /*right*/ String, Style),
    // What to show at left and right ends of something horizontally scrollable. E.g.: '<', '>'
    pub hscroll_indicator: (/*left*/ String, /*right*/ String, Style),
    pub line_wrap_indicator: (/*start_of_line*/ String, /*end_of_line*/ String, Style),

    pub table_header: Style,
    pub striped_table: StyleAdjustment,

    pub tab_selected: Style,
    pub tab_ephemeral: (String, Style),
    pub tab_deselected: Style,
    pub tab_separator: (String, Style),

    // fg - left side, bg - right side.
    pub progress_bar: Style,

    pub scroll_bar_background: Style,
    pub scroll_bar_slider: Style,

    pub tooltip: StyleAdjustment,
    pub dialog: StyleAdjustment,

    pub text_input: Style,
    pub text_input_selected: Style,

    pub tree_indent: (String, Style),
    pub tree_indent_selected: (String, Style),
    pub tree_indent_limit_reached: (String, Style),

    pub window_border: Style,
    pub window_border_active: Style,
    pub window_title_active: Style,
    pub window_title_selected: Style,
    pub window_title_deselected: Style,
    pub window_title_separator: (String, Style),

    pub value: Style,
    pub value_misc: Style,
    pub value_dubious: StyleAdjustment,

    pub code_statement: Style,
    pub code_inlined_site: Style,
    pub code_instruction_pointer_column: StyleAdjustment,
    pub instruction_pointer: Style,
    pub additional_instruction_pointer: Style,
    pub breakpoint: Style,
    pub secondary_breakpoint: Style,
    pub code_line_number: Style,
    pub url: Style,
    pub disas_default: Style,
    pub disas_keyword: Style,
    pub disas_mnemonic: Style,
    pub disas_register: Style,
    pub disas_number: Style,
    pub disas_function: Style,
    pub disas_jump_arrow: Style,
    pub disas_relative_address: Style,
    pub disas_filename: Style,

    pub thread_breakpoint_hit: StyleAdjustment,
    pub thread_crash: StyleAdjustment,
    pub breakpoint_error: StyleAdjustment,
}
impl Default for Palette {
    fn default() -> Self {
        let black = Color(0, 0, 0);
        let white = Color(0xff, 0xff, 0xff);
        let red = Color(255, 50, 50);
        let green = Color(0, 0xaa, 0);
        let blue = Color(0x63, 0x84, 0xff);
        let yellow = Color(0xaa, 0x55, 0);
        let cyan = Color(0, 0xaa, 0xaa);
        let magenta = Color(0xaa, 0, 0xaa);
        let dark_gray = Color(0x55, 0x55, 0x55);
        let light_green = Color(0x55, 0xff, 0x55);
        let light_blue = Color(0x55, 0x55, 0xff);

        Self {
            default: Style {fg: white, ..D!()},
            default_dim: Style {fg: white.darker(), ..D!()},
            error: Style {fg: red, ..D!()},
            warning: Style {fg: yellow, ..D!()},

            running: Style {fg: blue, ..D!()},
            suspended: Style {fg: light_green, ..D!()},

            function_name: Style {fg: white, ..D!()},
            filename: Style {fg: cyan, ..D!()},
            line_number: Style {fg: green, ..D!()},
            column_number: Style {fg: green.darker(), ..D!()},
            type_name: Style {fg: white, ..D!()},
            field_name: Style {fg: green, ..D!()},
            keyword: Style {fg: white, ..D!()},
            hotkey: StyleAdjustment {add_modifier: Modifier::UNDERLINED, ..D!()},

            state_running: Style {bg: blue, fg: black, ..D!()},
            state_suspended: Style {bg: light_green, fg: black, ..D!()},
            state_other: Style {bg: yellow, fg: black, ..D!()},

            selected: StyleAdjustment {add_fg: (50, 50, 50), add_bg: (50, 50, 50), ..D!()},
            hovered: StyleAdjustment {add_fg: (20, 20, 20), add_bg: (25, 25, 25), ..D!()},

            table_header: Style {fg: white.darker(), ..D!()},
            //striped_table: StyleAdjustment {add_fg: (0, 0, 0), add_bg: (20, 20, 20), ..D!()},
            striped_table: StyleAdjustment::default(),

            tab_selected: Style {fg: white, bg: Color(40, 40, 40), modifier: Modifier::BOLD, ..D!()},
            tab_ephemeral: ("∗".to_string(), Style {fg: white, ..D!()}),
            tab_deselected: Style {fg: white.darker(), ..D!()},
            tab_separator: (" | ".to_string(), Style {fg: white.darker(), ..D!()}),

            placeholder_fill: Some(('.', Style {fg: white.darker(), bg: black, ..D!()})),
            truncation_indicator: (("…".to_string(), "…".to_string(), Style {fg: white.darker(), ..D!()})),
            hscroll_indicator: (("❮".to_string(), "❯".to_string(), Style {fg: white.darker(), ..D!()})),
            line_wrap_indicator: ("\\".to_string(), "\\".to_string(), Style {fg: white.darker(), ..D!()}),

            progress_bar: Style {fg: blue, bg: Color(30, 30, 30), ..D!()},
            scroll_bar_background: Style {fg: white.darker(), ..D!()},
            scroll_bar_slider: Style {fg: white.darker(), ..D!()},

            tooltip: StyleAdjustment {add_bg: (30, 40, 50), ..D!()},
            dialog: StyleAdjustment {add_fg: (10, 10, 10), add_bg: (20, 20, 20), ..D!()},

            text_input: Style {fg: white, ..D!()},
            text_input_selected: Style {fg: white, bg: blue, ..D!()},

            tree_indent: ("┆".to_string(), Style {fg: white.darker(), ..D!()}),
            tree_indent_selected: ("┇".to_string(), Style {fg: white, ..D!()}),
            tree_indent_limit_reached: ("┼".to_string(), Style {fg: white, ..D!()}),

            window_border: Style {fg: white.darker(), ..D!()},
            window_border_active: Style {fg: white, modifier: Modifier::BOLD, ..D!()},
            window_title_active: Style {fg: white, bg: Color(30, 30, 30), modifier: Modifier::BOLD, ..D!()},
            window_title_selected: Style {fg: white, bg: Color(30, 30, 30), ..D!()},
            window_title_deselected: Style {fg: white.darker(), ..D!()},
            window_title_separator: (" | ".to_string(), Style {fg: white.darker(), ..D!()}),

            value: Style {fg: white, ..D!()},
            value_misc: Style {fg: white.darker(), ..D!()},
            value_dubious: StyleAdjustment {add_fg: (-80, -80, -80), ..D!()},

            code_statement: Style {fg: white, bg: dark_gray, ..D!()},
            code_inlined_site: Style {fg: white, bg: light_blue, ..D!()},
            code_instruction_pointer_column: StyleAdjustment {add_bg: (0, 100, 0), add_modifier: Modifier::UNDERLINED, ..D!()},
            instruction_pointer: Style {fg: green, modifier: Modifier::BOLD, ..D!()},
            additional_instruction_pointer: Style {fg: blue, ..D!()},
            breakpoint: Style {fg: red, ..D!()},
            secondary_breakpoint: Style {fg: blue, ..D!()},
            code_line_number: Style {fg: white, ..D!()},
            url: Style {fg: blue, ..D!()},
            disas_default: Style {fg: white, ..D!()},
            disas_keyword: Style {fg: white.darker(), ..D!()},
            disas_mnemonic: Style {fg: green, modifier: Modifier::BOLD, ..D!()},
            disas_register: Style {fg: blue, ..D!()},
            disas_number: Style {fg: cyan, ..D!()},
            disas_function: Style {fg: magenta, ..D!()},
            disas_jump_arrow: Style {fg: white, ..D!()},
            disas_relative_address: Style {fg: cyan.darker(), ..D!()}, 
            disas_filename: Style {fg: cyan.darker(), ..D!()},

            thread_breakpoint_hit: StyleAdjustment {add_fg: (100, 100, 100), add_bg: (10, 100, 10), ..D!()},
            thread_crash: StyleAdjustment {add_fg: (100, 100, 100), add_bg: (150, 50, 50), ..D!()},
            breakpoint_error: StyleAdjustment {add_fg: (100, 100, 100), add_bg: (150, 50, 50), ..D!()},
       }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub enum KeyAction {
    Quit,

    Run,
    Continue,
    Suspend,
    Kill,

    StepIntoLine,
    StepIntoInstruction,
    StepOverLine,
    StepOverColumn,
    StepOverInstruction,
    StepOut,
    StepOutNoInline,

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
    Tooltip,

    NextTab,
    PreviousTab,
    CloseTab,
    NextStackFrame,
    PreviousStackFrame,
    NextThread,
    PreviousThread,
    NextMatch,
    PreviousMatch,

    Open,
    ToggleBreakpoint,
    DisableBreakpoint,
    Find,

    DuplicateRow,

    DropCaches,
    ToggleProfiler,

    // Text input.
    Undo,
    Redo,
    Copy,
    Paste,
    Cut,
    NewLine,
}

pub struct KeyBinds {
    pub key_to_action: HashMap<KeyEx, KeyAction>,
    pub action_to_keys: HashMap<KeyAction, Vec<KeyEx>>,
    pub text_input_key_to_action: HashMap<KeyEx, KeyAction>,
    pub vscroll_sensitivity: isize,
    pub hscroll_sensitivity: isize,
}
impl KeyBinds {
    pub fn actions_to_keys(&self, actions: &[KeyAction]) -> Vec<KeyEx> {
        let mut r: Vec<KeyEx> = Vec::new();
        for action in actions {
            if let Some(keys) = self.action_to_keys.get(action) {
                r.extend_from_slice(keys);
            }
        }
        r
    }

    pub fn keys_to_actions(&self, keys: &[KeyEx]) -> Vec<KeyAction> {
        let mut res: Vec<KeyAction> = Vec::new();
        for key in keys {
            if let Some(action) = self.key_to_action.get(key) {
                res.push(action.clone());
            }
        }
        res
    }

    pub fn action_to_key_name(&self, action: KeyAction) -> String {
        match self.action_to_keys.get(&action) {
            None => "<unbound>".to_string(),
            Some(v) if v.is_empty() => "<unbound>".to_string(),
            Some(v) => format!("{}", v[0]),
        }
    }

    fn new(key_to_action: &[(KeyEx, KeyAction)], text_input_key_to_action: &[(KeyEx, KeyAction)]) ->  Self {
        let mut r = KeyBinds {key_to_action: key_to_action.iter().cloned().collect(), text_input_key_to_action: text_input_key_to_action.iter().cloned().collect(), action_to_keys: HashMap::new(), vscroll_sensitivity: 8, hscroll_sensitivity: 20};
        // Order somewhat matters here: if there are multiple keys for the same action, we show only the first one in the hints window.
        // So here we add keys in the same order in which the keys were specified in the input, not in HashMap iteration order.
        for (key, action) in key_to_action {
            r.action_to_keys.entry(*action).or_default().push(*key);
        }
        r
    }
}
impl Default for KeyBinds {
    fn default() -> Self {
        Self::new(&[
            (Key::Char('q').plain(), KeyAction::Quit),
            (Key::Char('r').plain(), KeyAction::Run),
            (Key::Char('c').plain(), KeyAction::Continue),
            (Key::Char('C').plain(), KeyAction::Suspend),
            (Key::Char('c').ctrl(), KeyAction::Suspend),
            (Key::Char('k').plain(), KeyAction::Kill),
            (Key::Up.alt(), KeyAction::WindowUp),
            (Key::Down.alt(), KeyAction::WindowDown),
            (Key::Left.alt(), KeyAction::WindowLeft),
            (Key::Right.alt(), KeyAction::WindowRight),
            (Key::Char('0').plain(), KeyAction::Window(0)),
            (Key::Char('1').plain(), KeyAction::Window(1)),
            (Key::Char('2').plain(), KeyAction::Window(2)),
            (Key::Char('3').plain(), KeyAction::Window(3)),
            (Key::Char('4').plain(), KeyAction::Window(4)),
            (Key::Char('5').plain(), KeyAction::Window(5)),
            (Key::Char('6').plain(), KeyAction::Window(6)),
            (Key::Char('7').plain(), KeyAction::Window(7)),
            (Key::Char('8').plain(), KeyAction::Window(8)),
            (Key::Char('9').plain(), KeyAction::Window(9)),
            (Key::Up.plain(), KeyAction::CursorUp),
            (Key::Down.plain(), KeyAction::CursorDown),
            (Key::Left.plain(), KeyAction::CursorLeft),
            (Key::Right.plain(), KeyAction::CursorRight),
            (Key::Char('\n').plain(), KeyAction::Enter),
            (Key::Delete.plain(), KeyAction::DeleteRow),
            (Key::Backspace.plain(), KeyAction::DeleteRow),
            (Key::Escape.plain(), KeyAction::Cancel),
            (Key::Char('g').ctrl(), KeyAction::Cancel),
            (Key::PageUp.plain(), KeyAction::PageUp),
            (Key::Char('v').alt(), KeyAction::PageUp),
            (Key::PageDown.plain(), KeyAction::PageDown),
            (Key::Char('v').ctrl(), KeyAction::PageDown),
            (Key::Home.plain(), KeyAction::Home),
            (Key::End.plain(), KeyAction::End),
            (Key::Char('i').ctrl(), KeyAction::Tooltip),
            (Key::Char('s').plain(), KeyAction::StepIntoLine),
            (Key::Char('S').plain(), KeyAction::StepIntoInstruction),
            (Key::Char('n').plain(), KeyAction::StepOverLine),
            (Key::Char('m').plain(), KeyAction::StepOverColumn),
            (Key::Char('N').plain(), KeyAction::StepOverInstruction),
            (Key::Char('f').plain(), KeyAction::StepOut),
            (Key::Char('F').plain(), KeyAction::StepOutNoInline),
            // (Ctrl+tab and ctrl+shift+tab are unrepresentable in ansi escape codes.)
            (Key::Char('t').ctrl(), KeyAction::NextTab),
            (Key::Char('b').ctrl(), KeyAction::PreviousTab),
            (Key::Char('w').ctrl(), KeyAction::CloseTab),
            (Key::Char(']').plain(), KeyAction::NextStackFrame),
            (Key::Char('[').plain(), KeyAction::PreviousStackFrame),
            (Key::Char('}').plain(), KeyAction::NextThread),
            (Key::Char('{').plain(), KeyAction::PreviousThread),
            (Key::Char('.').plain(), KeyAction::NextMatch),
            (Key::Char(',').plain(), KeyAction::PreviousMatch),
            (Key::Char('o').plain(), KeyAction::Open),
            (Key::Char('b').plain(), KeyAction::ToggleBreakpoint),
            (Key::Char('B').plain(), KeyAction::DisableBreakpoint),
            (Key::Char('/').plain(), KeyAction::Find),
            (Key::Char('d').plain(), KeyAction::DuplicateRow),
            (Key::Char('l').ctrl(), KeyAction::DropCaches),
            (Key::Char('p').ctrl(), KeyAction::ToggleProfiler),
        ], &[
            (Key::Char('7').ctrl(), KeyAction::Undo), // ctrl+/ is indistinguishable from ctrl+7
            (Key::Char('_').alt(), KeyAction::Redo),
            (Key::Char('y').ctrl(), KeyAction::Paste),
            (Key::Char('c').ctrl(), KeyAction::Copy),
            (Key::Char('x').ctrl(), KeyAction::Cut),
            (Key::Char('v').ctrl(), KeyAction::Paste),
            (Key::Char('\n').alt(), KeyAction::NewLine),
        ])
    }
}
