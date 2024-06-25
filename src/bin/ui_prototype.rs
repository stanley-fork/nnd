#![allow(unused_imports)]
extern crate nnd;
use nnd::{*, error::*, util::*};
use std::{time::{Duration, Instant, SystemTime}, thread, mem, cell::UnsafeCell, sync::atomic::{AtomicBool, Ordering}, io, io::{Write, Read}, str, os::fd::AsRawFd, ops::Range, collections::{HashMap}, panic, process, fmt::Write as fmtWrite, result, hash::Hash};
use bitflags::*;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

// =============================================== util.rs ========================================================

#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub struct Rect {
    x: isize,
    y: isize,
    w: usize,
    h: usize,
}
impl Rect {
    pub fn right(&self) -> isize { self.x + self.w as isize }
    pub fn bottom(&self) -> isize { self.y + self.h as isize }

    pub fn intersection(&self, r: Rect) -> Rect {
        let x = self.x.max(r.x);
        let y = self.y.max(r.y);
        Rect {x, y, w: (self.right().min(r.right()) - x).max(0) as usize, h: (self.bottom().min(r.bottom()) - y).max(0) as usize}
    }

    pub fn is_empty(&self) -> bool { self.w == 0 || self.h == 0 }

    pub fn contains(&self, pos: [isize; 2]) -> bool { self.x <= pos[0] && self.right() > pos[0] && self.y <= pos[1] && self.bottom() > pos[1] }
}

// RGB. (We always use RGB instead of terminal palette colors because the palette is not big enough for us, and mixing palette with RGB colors would look terrible with non-default palette.)
#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub struct Color(u8, u8, u8);
impl Color {
    pub fn white() -> Self { Self(255, 255, 255) }
    pub fn black() -> Self { Self(0, 0, 0) }
    pub fn dim(self) -> Self {
        let f = |x| (x as usize * 2 / 3) as u8;
        Self(f(self.0), f(self.1), f(self.2))
    }
}

bitflags! {
#[derive(Default)]
pub struct Modifier: u8 {
    // These would correspond to various ANSI escape codes for text style... but pretty much
    // none of such codes are universally supported across even the top few popular terminals,
    // so we only have the bare minimum here.

    // Avoid using this in a load-bearing way, in case some terminals don't support it.
    // E.g. undim the fg color or alter bg color in addition to using the modifier.
    const BOLD = 0x1;

    // We use this in a load-bearing way occasionally (e.g. for current column in source code window), but maybe we shouldn't.
    const UNDERLINED = 0x2;

    // ITALIC would be nice, but it's not widely supported, e.g. doesn't work over ssh+tmux. So we avoid it.
    // DIM is also not widely supported. We manually alter the RGB colors instead.
}}

#[derive(Clone, Copy, Eq, PartialEq, Default)]
pub struct Style {
    pub fg: Color,
    pub bg: Color,
    pub modifier: Modifier,
}
impl Style {
    pub fn flip(self) -> Self { Self {fg: self.bg, bg: self.fg, modifier: self.modifier} }
    pub fn add_modifier(mut self, m: Modifier) -> Self { self.modifier.insert(m); self }
}

#[derive(Clone, Copy, Default)]
pub struct StyleAdjustment {
    pub add_fg: (i16, i16, i16),
    pub add_bg: (i16, i16, i16),
    pub add_modifier: Modifier,
    pub remove_modifier: Modifier,
}
impl StyleAdjustment {
    fn apply(self, mut s: Style) -> Style {
        let add = |x: &mut u8, y: i16| {
            *x = y.saturating_add(*x as i16).max(0).min(255) as u8;
        };
        let add3 = |x: &mut Color, y: &(i16, i16, i16)| {
            add(&mut x.0, y.0);
            add(&mut x.1, y.1);
            add(&mut x.2, y.2);
        };
        add3(&mut s.fg, &self.add_fg);
        add3(&mut s.bg, &self.add_bg);
        s.modifier = s.modifier & !self.remove_modifier | self.add_modifier;
        s
    }

    fn combine(self, other: Self) -> Self {
        let add3 = |x: (i16, i16, i16), y: (i16, i16, i16)| {
            (x.0.saturating_add(y.0), x.1.saturating_add(y.1), x.2.saturating_add(y.2))
        };
        Self {add_fg: add3(self.add_fg, other.add_fg), add_bg: add3(self.add_bg, other.add_bg), add_modifier: self.add_modifier.union(other.add_modifier), remove_modifier: self.remove_modifier.union(other.remove_modifier)}
    }

    fn update(&mut self, other: Self) {
        *self = self.combine(other);
    }
}

// Horizontal space that the string would occupy on the screen.
pub fn str_width(s: &str) -> usize {
    // Calculate width the same way as ScreenBuffer.put_text().
    // We could just do s.width(), but there are probably some unicode shenanigans that sometimes make string width different from the sum of its grapheme widths.
    s.graphemes(true).map(|c| c.width()).sum()
}
// Max i such that str_width(&s[..i]) <= width.
pub fn str_prefix_with_width(s: &str, width: usize) -> (/*bytes*/ usize, /*width*/ usize) {
    let mut w = 0usize;
    for (i, c) in s.grapheme_indices(true) {
        let ww = w + c.width();
        if ww > width {
            return (i, w);
        }
        w = ww;
    }
    (s.len(), w)
}
// Min i such that str_width(&s[i..]) <= width.
pub fn str_suffix_with_width(s: &str, width: usize) -> (/*start_byte_idx*/ usize, /*width*/ usize) {
    let mut w = 0usize;
    for (i, c) in s.grapheme_indices(true).rev() {
        let ww = w + c.width();
        if ww > width {
            return (i + c.len(), w);
        }
        w = ww;
    }
    (0, w)
}

pub struct StyledText {
    // Each array describes ranges of indiced in the next array. This is to have just 3 memory allocations instead of lots of tiny allocations in Vec<Vec<String>>.
    // A span can't straddle lines. There can be an "unclosed" last line, and "unclosed" last span; by convention, consumers (e.g. Widget) usually expect the relevant spans and lines to be closed.
    pub lines: Vec<usize>, // i-th line contains spans [lines[i], lines[i+1])
    pub spans: Vec<(usize, Style)>, // i-th span contains chars [spans[i].0, spans[i+1].0) with style spans[i+1].1
    pub chars: String,
}

impl Default for StyledText { fn default() -> Self { Self {chars: String::new(), spans: vec![(0, Style::default())], lines: vec![0]} } }
impl StyledText {
    // Number of closed spans.
    pub fn num_spans(&self) -> usize {
        self.spans.len() - 1
    }
    
    // Number of closed lines.
    pub fn num_lines(&self) -> usize {
        self.lines.len() - 1
    }

    // If span is empty, doesn't add it. (But note that a "nonempty" span may still have rendered width 0, e.g. if it's a "zero width space" character.)
    pub fn close_span(&mut self, style: Style) {
        if self.spans.last().unwrap().0 != self.chars.len() {
            self.spans.push((self.chars.len(), style));
        }
    }

    pub fn close_line(&mut self) -> usize {
        self.lines.push(self.spans.len() - 1);
        self.lines.len() - 2
    }

    // Must be closed.
    pub fn get_span(&self, i: usize) -> (&str, Style) {
        (&self.chars[self.spans[i].0..self.spans[i+1].0], self.spans[i+1].1)
    }

    // Range of spans for i-th line. The line must be closed.
    pub fn get_line(&self, i: usize) -> Range<usize> {
        self.lines[i]..self.lines[i+1]
    }

    pub fn get_line_str(&self, i: usize) -> &str {
        &self.chars[self.spans[self.lines[i]].0..self.spans[self.lines[i+1]].0]
    }

    pub fn widest_line(&self) -> usize {
        (0..self.num_lines()).map(|i| str_width(self.get_line_str(i))).max().unwrap_or(0)
    }

    pub fn clear(&mut self) {
        self.chars.clear();
        self.spans.truncate(1);
        self.lines.truncate(1);
    }

    pub fn unclosed_line_width(&self) -> usize {
        str_width(&self.chars[self.spans[*self.lines.last().unwrap()].0..])
    }

    // If t has multiple lines, they're mashed together.
    pub fn append_to_unclosed_line(&mut self, t: &StyledText) {
        let (n, m) = (self.spans.len(), self.chars.len());
        self.chars.push_str(&t.chars);
        self.spans.extend_from_slice(&t.spans);
        for s in &mut self.spans[n..] {
            s.0 += m;
        }
    }

    pub fn line_wrap(&mut self, lines: Range<usize>, width: usize, max_lines: usize, line_wrap_indicator: &(String, String, Style), truncation_indicator: &(String, String, Style)) -> Range<usize> {
        let start = self.num_lines();
        for line_idx in lines {
            let spans_end = self.lines[line_idx + 1];
            let mut span_idx = self.lines[line_idx];
            let line_end = self.spans[spans_end].0;
            let line_start = self.spans[span_idx].0;
            let mut pos = line_start;
            let mut remaining_width = str_width(&self.chars[pos..line_end]);
            if pos == line_end {
                self.close_line();
                continue;
            }
            while pos < line_end {
                let width = if pos == line_start {
                    width
                } else {
                    styled_write!(self, line_wrap_indicator.2, "{}", line_wrap_indicator.0);
                    width.saturating_sub(str_width(&line_wrap_indicator.0))
                };
                let last_line = self.num_lines() - start + 1 >= max_lines;
                let end_indicator = if last_line {truncation_indicator} else {line_wrap_indicator};
                let (n, w) = if remaining_width <= width {
                    (line_end - pos, remaining_width)
                } else {
                    str_prefix_with_width(&self.chars[pos..line_end], width.saturating_sub(str_width(&end_indicator.1)))
                };

                let wrapped_end = pos + n;
                while pos < wrapped_end {
                    assert!(span_idx < spans_end);
                    let (e, s) = self.spans[span_idx + 1].clone();
                    if e > pos {
                        let new_pos = e.min(wrapped_end);
                        unsafe {self.chars.as_mut_vec().extend_from_within(pos..new_pos)};
                        self.spans.push((self.chars.len(), s));
                        pos = new_pos;
                    }
                    if e <= wrapped_end {
                        span_idx += 1;
                    }
                }
                remaining_width -= w;

                if pos < line_end {
                    styled_write!(self, end_indicator.2, "{}", end_indicator.1);
                    if last_line {
                        self.close_line();
                        return start..self.num_lines();
                    }
                }
                self.close_line();
            }
        }
        start..self.num_lines()
    }
}

// Append a span to StyledText:
// styled_write!(out, palette.default_dim, "foo: {}, bar: {}", foo, bar);
#[macro_export]
macro_rules! styled_write {
    ($out:expr, $style:expr, $($arg:tt)*) => (
        {
            let _ = write!(($out).chars, $($arg)*);
            ($out).close_span($style);
        }
    );
}

// ====================================================== terminal.rs ===================================================================


#[derive(Clone, Copy, Eq, PartialEq)]
pub struct ScreenCell {
    pub style: Style,

    // Encodes a string containing the "extended grapheme cluster" for this cell. (Stored inline if short, in long_graphemes if long.)
    // If the symbol is wider than one cell (e.g. some icons), it lives in the leftmost of the cells, and the other cells contain ' '.
    // (Terminal advances the cursor by only one cell when printing a wide character, but the character visually covers the next n-1 cells.)
    // If a wide symbol is partially clipped or covered (e.g. partially overwritten in ScreenBuffer's array), it's impossible to render correctly,
    // and we don't do any special handling for this case - the symbol just lives in the first cell, and the subsequent cells are spaces.
    len: u8,
    data: [u8; 4],
}
impl Default for ScreenCell { fn default() -> Self { Self {style: Style::default(), len: 1, data: [b' ', 0, 0, 0]} } }

#[derive(Default, Clone)]
pub struct ScreenBuffer {
    pub width: usize,
    pub height: usize,
    pub cells: Vec<ScreenCell>, // width * height
    pub long_graphemes: Vec<u8>,
}
impl ScreenBuffer {
    pub fn resize_and_clear(&mut self, width: usize, height: usize, style: Style) {
        (self.width, self.height) = (width, height);
        self.long_graphemes.clear();
        let cell = ScreenCell {style, ..Default::default()};
        // Clear cells to make sure no dangling references into long_graphemes exist.
        self.cells.clear();
        self.cells.resize(width*height, cell);
    }

    pub fn rect(&self) -> Rect { Rect {x: 0, y: 0, w: self.width, h: self.height} }

    fn pack_grapheme(&mut self, mut s: &str, style: Style) -> ScreenCell {
        if s.len() > 255 {
            let mut i = 255;
            while !s.is_char_boundary(i) {
                i -= 1;
            }
            s = &s[..i];
        }

        let mut data = [0u8; 4];
        if s.len() <= 4 {
            data[..s.len()].copy_from_slice(s.as_bytes());
        } else {
            assert!(self.long_graphemes.len() + s.len() < u32::MAX as usize);
            data = (self.long_graphemes.len() as u32).to_le_bytes();
            self.long_graphemes.extend_from_slice(s.as_bytes());
        }

        ScreenCell {style, len: s.len() as u8, data}
    }

    fn unpack_grapheme<'a>(&'a self, cell: &'a ScreenCell) -> &'a str {
        let slice = if cell.len <= 4 {
            &cell.data
        } else {
            &self.long_graphemes[u32::from_le_bytes(cell.data.clone()) as usize..]
        };
        unsafe {str::from_utf8_unchecked(&slice[..cell.len as usize])}
    }

    pub fn fill(&mut self, rect: Rect, grapheme: &str, style: Style) {
        assert!(str_width(grapheme) == 1);
        let cell = self.pack_grapheme(grapheme, style);
        let rect = rect.intersection(self.rect());
        for y in rect.y as usize..rect.bottom() as usize {
            self.cells[(y*self.width + rect.x as usize)..(y*self.width + rect.right() as usize)].fill(cell);
        }
    }

    // Returns new x, possibly cut short by the clipping rectangle.
    pub fn put_text(&mut self, text: &str, style: Style, mut x: isize, y: isize, clip: Rect) -> isize {
        let clip = clip.intersection(self.rect());
        if y >= clip.bottom() || y < clip.y {
            return x;
        }
        let row_start = y as usize * self.width;
        for s in text.graphemes(true) {
            if x >= clip.right() {
                break;
            }

            let w = s.width();
            let start = x;
            x += w as isize;

            if x <= clip.x {
                continue;
            }

            if start >= clip.x {
                let cell = self.pack_grapheme(s, style);
                self.cells[row_start + start as usize] = cell;
            }
            if w > 1 {
                let cell = self.pack_grapheme(" ", style);
                self.cells[row_start + (start+1).max(clip.x) as usize .. row_start + x.min(clip.right()) as usize].fill(cell);
            }
        }
        x
    }

    pub fn get_cell(&self, x: usize, y: usize) -> (&str, Style) {
        let cell = &self.cells[y*self.width + x];
        (self.unpack_grapheme(cell), cell.style)
    }
}

fn terminal_size() -> Result<(/*rows*/ u16, /*columns*/ u16)> {
    let mut s: libc::winsize = unsafe {mem::zeroed()};
    let r = unsafe {libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut s as *mut _)};
    if r != 0 {
        return errno_err!("ioctl(STDOUT, TIOCGWINSZ) failed");
    }
    Ok((s.ws_row, s.ws_col))
}

// Some ANSI escape codes.
const CURSOR_BLINKING_BLOCK: &'static str = "\x1B[\x31 q";
const CURSOR_BLINKING_BAR: &'static str = "\x1B[\x35 q";
const CURSOR_HIDE: &'static str = "\x1B[?25l";
const CURSOR_SHOW: &'static str = "\x1B[?25h";
const SCREEN_ALTERNATE: &'static str = "\x1B[?1049h";
const SCREEN_MAIN: &'static str = "\x1B[?1049l";
const CLEAR_SCREEN: &'static str = "\x1B[2J";
const STYLE_RESET: &'static str = "\x1B[m";
const STYLE_RESET_TO_BLACK: &'static str = "\x1B[0;38;2;0;0;0;48;2;0;0;0m";
// Mouse parameters we use:
//  1002 - SET_BTN_EVENT_MOUSE - report mouse button presses/releases and mouse movement when any button is held (only if the cursor moved to a different cell in the terminal). OR ...
//  1003 - SET_ANY_EVENT_MOUSE - report all mouse movement (even if the cursor moved by one pixel, even though pixel coordinates are not reported).
//  1007 - SET_ALTERNATE_SCROLL - report mouse scroll wheel events (instead of scrolling the terminal).
//  1006 - SET_SGR_EXT_MODE_MOUSE - changes mouse events encoding to support coordinates > 223.
//  1004 - SET_FOCUS_EVENT_MOUSE - notify when the terminal loses and gains focus (useful if the user alt-tabs while holding mouse button - we don't get mouse release event in this case, so have to assume mouse release based on focus loss).
const MOUSE_ENABLE_COMMON: &'static str = "\x1b[?1007h\x1b[?1006h\x1b[?1004h";
const MOUSE_BUTTON_EVENT_MODE: &'static str = "\x1b[?1002h";
const MOUSE_ANY_EVENT_MODE: &'static str = "\x1b[?1003h";
const MOUSE_DISABLE: &'static str = "\x1b[?1003l\x1b[?1002l\x1b[?1007l\x1b[?1006l\x1b[?1004l";

static mut TERMINAL_STATE_TO_RESTORE: UnsafeCell<libc::termios> = UnsafeCell::new(/*rust, why doesn't mem::zeroed() just work here*/ libc::termios{c_iflag:0,c_oflag:0,c_cflag:0,c_lflag:0,c_line:0,c_cc:[0;32],__c_ispeed:0,__c_ospeed:0});
static mut TERMINAL_STATE_RESTORED: AtomicBool = AtomicBool::new(true);

// Changes terminal state to raw mode, alternate screen, and bar cursor.
// Called once at program startup. Not thread safe.
pub fn configure_terminal(mouse_mode: MouseMode) -> Result<()> {
    unsafe {
        assert!(TERMINAL_STATE_RESTORED.load(Ordering::SeqCst));
        let original_termios = TERMINAL_STATE_TO_RESTORE.get();
        let r = libc::tcgetattr(libc::STDOUT_FILENO, original_termios);
        if r != 0 { return errno_err!("tcgetarrt() failed"); }
        let mut new_termios = (*original_termios).clone();
        libc::cfmakeraw(&mut new_termios);
        let r = libc::tcsetattr(libc::STDOUT_FILENO, libc::TCSANOW, &new_termios);
        if r != 0 { return errno_err!("tcsetattr() failed"); }
        TERMINAL_STATE_RESTORED.store(false, Ordering::SeqCst);

        let (mouse1, mouse2) = match mouse_mode {
            MouseMode::Disabled => ("", ""),
            MouseMode::NoHover => (MOUSE_ENABLE_COMMON, MOUSE_BUTTON_EVENT_MODE),
            MouseMode::Full => (MOUSE_ENABLE_COMMON, MOUSE_ANY_EVENT_MODE),
        };
        write!(io::stdout(), "{}{}{}{}", SCREEN_ALTERNATE, CURSOR_BLINKING_BAR, mouse1, mouse2)?;
        io::stdout().flush()?;
        Ok(())
    }
}

// Undoes the changes made by configure_terminal(), and also unhides cursor (typically we hide it on each frame).
// If called multiple times, only the first call does anything. Can be called in parallel (but only the first call waits for the changes to be made; if there's a parallel call, it'll return immediately, possibly before the terminal was actually restored).
// It's important to try to always call this at least once before the process exits, including on panics.
// Otherwise we'll leave the terminal in a borked state for the user (but it's easy to unbork: `reset`).
// Would be nice to call this on signals too (e.g. SIGSEGV and SIGTERM) (even though it's not signal-safe), but currently we don't, because I couldn't find a way to both have a SIGSEGV handler and propagate the original fault address (si_addr), e.g. if a debugger is attached to this process.
pub fn restore_terminal() {
    unsafe {
        if TERMINAL_STATE_RESTORED.swap(true, Ordering::SeqCst) {
            return;
        }
        // Set the cursor style to blinking block because it's the most common one. Would be better to restore the original style that the cursor had on startup, but there appears to be no way to query or save that style.
        let _ = write!(io::stdout(), "{}{}{}{}\n", MOUSE_DISABLE, CURSOR_BLINKING_BLOCK, CURSOR_SHOW, SCREEN_MAIN).unwrap_or(());
        let _ = io::stdout().flush().unwrap_or(());

        let termios = TERMINAL_STATE_TO_RESTORE.get();
        let _ = libc::tcsetattr(libc::STDOUT_FILENO, libc::TCSANOW, termios);
    }
}

pub struct TerminalRestorer;
impl Drop for TerminalRestorer { fn drop(&mut self) { restore_terminal(); } }

pub struct Terminal {
    prev_buffer: ScreenBuffer,
    temp_buffer: ScreenBuffer, // reuse buffer across frames
}
impl Terminal {
    pub fn new() -> Self { Self {prev_buffer: ScreenBuffer::default(), temp_buffer: ScreenBuffer::default()} }

    pub fn start_frame(&mut self, style: Style) -> Result<ScreenBuffer> {
        let (rows, columns) = terminal_size()?;
        let mut b = mem::take(&mut self.temp_buffer);
        b.resize_and_clear(columns as usize, rows as usize, style);
        Ok(b)
    }

    pub fn clear(&mut self) -> Result<()> {
        self.prev_buffer = ScreenBuffer::default();
        write!(io::stdout(), "{}", CLEAR_SCREEN)?;
        io::stdout().flush()?;
        Ok(())
    }

    pub fn prepare_command_buffer(&self, buffer: &ScreenBuffer) -> Vec<u8> {
        let mut commands: Vec<u8> = Vec::new();
        let (mut cur_x, mut cur_y, mut cur_style) = (usize::MAX, usize::MAX, Style::default());
        write!(commands, "{}{}", CURSOR_HIDE, STYLE_RESET_TO_BLACK).unwrap();

        let mut write_range = |y: usize, x1: usize, x2: usize, buffer: &ScreenBuffer, commands: &mut Vec<u8>| {
            if cur_y == y {
                write!(commands, "\x1B[{}G", x1 + 1).unwrap();
            } else {
                write!(commands, "\x1B[{};{}H", y + 1, x1 + 1).unwrap();
            }
            for x in x1..x2 {
                let (s, style) = buffer.get_cell(x, y);

                if style != cur_style {
                    write!(commands, "\x1B[").unwrap();
                    if style.fg != cur_style.fg {
                        write!(commands, "38;2;{};{};{};", style.fg.0, style.fg.1, style.fg.2).unwrap();
                    }
                    if style.bg != cur_style.bg {
                        write!(commands, "48;2;{};{};{};", style.bg.0, style.bg.1, style.bg.2).unwrap();
                    }

                    if style.modifier.contains(Modifier::BOLD) && !cur_style.modifier.contains(Modifier::BOLD) {
                        write!(commands, "1;").unwrap();
                    }
                    if !style.modifier.contains(Modifier::BOLD) && cur_style.modifier.contains(Modifier::BOLD) {
                        write!(commands, "22;").unwrap();
                    }
                    if style.modifier.contains(Modifier::UNDERLINED) && !cur_style.modifier.contains(Modifier::UNDERLINED) {
                        write!(commands, "4;").unwrap();
                    }
                    if !style.modifier.contains(Modifier::UNDERLINED) && cur_style.modifier.contains(Modifier::UNDERLINED) {
                        write!(commands, "24;").unwrap();
                    }

                    cur_style = style;
                    *commands.last_mut().unwrap() = b'm'; // replace trailing ';' with 'm'
                }

                write!(commands, "{}", s).unwrap();
            }
            (cur_x, cur_y) = (x2, y);
        };

        if self.prev_buffer.rect() != buffer.rect() {
            write!(commands, "{}", CLEAR_SCREEN).unwrap();
            for y in 0..buffer.height {
                write_range(y, 0, buffer.width, &buffer, &mut commands);
            }
        } else {
            for y in 0..buffer.height {
                let mut x1 = 0;
                for x in 0..buffer.width {
                    let idx = y*buffer.width + x;
                    if buffer.cells[idx] == self.prev_buffer.cells[idx] {
                        if x > x1 {
                            write_range(y, x1, x, &buffer, &mut commands);
                        }
                        x1 = x + 1;
                    }
                }
                if buffer.width > x1 {
                    write_range(y, x1, buffer.width, &buffer, &mut commands);
                }
            }
        }

        write!(commands, "{}", STYLE_RESET).unwrap();
        commands
    }

    pub fn present(&mut self, buffer: ScreenBuffer, commands: Vec<u8>) -> Result<()> {
        io::stdout().write_all(&commands)?;
        io::stdout().flush()?;

        self.temp_buffer = mem::replace(&mut self.prev_buffer, buffer);
        Ok(())
    }
}


bitflags! {
#[derive(Default)]
pub struct ModKeys: u8 {
    // The values match the encoding in the ANSI escape codes, do not change.
    // SHIFT doesn't always appear as modifier: for Key::Char, it's built into the `char`, e.g. 'O' is shift+o.
    const SHIFT = 0x1;
    const ALT = 0x2;
    const CTRL = 0x4;
}}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Key {
    Char(char),
    F(u8),
    Backspace,
    Escape,
    Up,
    Down,
    Right,
    Left,
    PageUp,
    PageDown,
    Home,
    End,
    Insert,
    Delete,
}
impl Key {
    pub fn mods(self, mods: ModKeys) -> KeyEx { KeyEx {key: self, mods} }
    pub fn plain(self) -> KeyEx { KeyEx {key: self, mods: ModKeys::empty()} }
    pub fn ctrl(self) -> KeyEx { KeyEx {key: self, mods: ModKeys::CTRL} }
    pub fn shift(self) -> KeyEx { KeyEx {key: self, mods: ModKeys::SHIFT} }
    pub fn alt(self) -> KeyEx { KeyEx {key: self, mods: ModKeys::ALT} }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KeyEx {
    pub key: Key,
    pub mods: ModKeys,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MouseMode {
    // No mouse support.
    Disabled,
    // Respond to clicks and drags, but not to hovers.
    NoHover,
    // Respond to clicks, drags, and hovers.
    // Because of a weird quirk in how terminal mouse reporting works, this generates much more mouse events for us to parse and process.
    // (Specifically: there's a mode where mouse movement is reported only when a button is held down, and only when the cursor moves to a different cell in the terminal;
    //  and another mode where mouse movement is reported always, even if the mouse moved by one pixel. But pixel coordinates are not even reported, only cell coordinated,
    //  so why would the application be interested in small movement events? There's no mode to report movements regardless of button holding, but only when the cursor moves to a different cell.)
    Full,
}
impl Default for MouseMode { fn default() -> Self { Self::Full } }

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MouseEvent {
    Press,
    Release,
    Move,
    ScrollUp,
    ScrollDown,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MouseButton {
    Left,
    Middle,
    Right,
    None,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct MouseEventEx {
    pub pos: [isize; 2],
    pub button: MouseButton,
    pub event: MouseEvent,
    pub mods: ModKeys,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Event {
    Key(KeyEx),
    Mouse(MouseEventEx),
    // The terminal gained or lost focus.
    FocusIn,
    FocusOut,
}

pub struct InputReader {
    start: usize,
    end: usize,
    buf: [u8; 128],
}
impl InputReader {
    pub fn new() -> Self { Self {start: 0, end: 0, buf: [0; 128]} }
    
    // Reads readily available input from stdin, doesn't block.
    pub fn read(&mut self, out: &mut Vec<Event>) -> Result<usize> {
        let mut bytes_read = 0usize;
        loop {
            if self.start * 2 > self.buf.len() {
                self.buf.copy_within(self.start..self.end, 0);
                self.end -= self.start;
                self.start = 0;
            }
            assert!(self.end < self.buf.len()); // true as long as escape sequences are shorter than buf.len()/2, which they should be
            let n = Self::read_bytes(&mut self.buf[self.end..])?;
            if n == 0 {
                return Ok(bytes_read);
            }
            self.end += n;
            bytes_read += n;

            loop {
                let start = self.start;
                match self.parse_event() {
                    Ok(e) => out.push(e),
                    Err(false) => (), // quietly skip unrecognized escape sequence
                    Err(true) => {
                        // Do rollback here so that parse_event() can just eat chars and not worry about peek/advance.
                        self.start = start;
                        break;
                    }
                }
            }
        }
    }

    fn read_bytes(buf: &mut [u8]) -> Result<usize> {
        loop { // retry EINTR
            let mut pfd = libc::pollfd {fd: libc::STDIN_FILENO, events: libc::POLLIN, revents: 0};
            let r = unsafe {libc::poll(&mut pfd as *mut libc::pollfd, 1, 0)};
            if r < 0 {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(e.into());
            }
            if r == 0 {
                return Ok(0);
            }

            let r = unsafe {libc::read(libc::STDIN_FILENO, buf.as_ptr() as *mut libc::c_void, buf.len())};
            if r < 0 {
                let e = io::Error::last_os_error();
                if e.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(e.into());
            }
            return Ok(r as usize);
        }
    }

    // Event parsing functions use Result<..., bool>, where the bool is:
    //  * true if we reached end of buffer before getting a complete escape sequence,
    //  * false if we got an invalid or unsupported escape sequence.
    
    fn peek(&self) -> result::Result<u8, bool> {
        if self.start == self.end {
            Err(true)
        } else {
            Ok(self.buf[self.start])
        }
    }
    
    fn eat(&mut self) -> result::Result<u8, bool> {
        let c = self.peek()?;
        self.start += 1;
        Ok(c)
    }

    fn ansi_modifier(n: usize) -> result::Result<ModKeys, bool> {
        if n < 1 || n > 16 {
            Err(false)
        } else {
            Ok(ModKeys::from_bits(n as u8 - 1).unwrap())
        }
    }

    fn ansi_key_xterm(c: u8) -> result::Result<Key, bool> {
        Ok(match c {
            b'A' => Key::Up,
            b'B' => Key::Down,
            b'C' => Key::Right,
            b'D' => Key::Left,
            b'H' => Key::Home,
            b'F' => Key::End,
            c @ b'P'..=b'S' => Key::F(1 + c - b'P'), // F1-F4
            _ => return Err(false),
        })
    }

    fn ansi_key_vt(n: usize) -> result::Result<Key, bool> {
        Ok(match n {
            1 => Key::Home,
            2 => Key::Insert,
            3 => Key::Delete,
            4 => Key::End,
            5 => Key::PageUp,
            6 => Key::PageDown,
            7 => Key::Home,
            8 => Key::End,
            10..=15 => Key::F(n as u8 - 10 + 0), // F0-F5
            17..=21 => Key::F(n as u8 - 17 + 6), // F6-F10
            23..=26 => Key::F(n as u8 - 23 + 11), // F11-F14
            28..=29 => Key::F(n as u8 - 28 + 15), // F15-F16
            31..=34 => Key::F(n as u8 - 31 + 17), // F17-F20
            _ => return Err(false),
        })
    }

    fn ansi_key_char(&mut self, c: u8) -> result::Result<KeyEx, bool> {
        Ok(match c {
            b'\n' | b'\r' => Key::Char('\n').plain(),
            b'\x7f' => Key::Backspace.plain(),
            b'\x01'..=b'\x1A' => Key::Char((c - 0x1 + b'a') as char).ctrl(), // ctrl+[a..z]; shift is ignored in this case; ctrl+backspace is ctrl+H
            b'\x1c'..=b'\x1f' => Key::Char((c - 0x1c + b'4') as char).ctrl(), // crtl+[4..7]; other digits don't work
            0..=127 => Key::Char(c.into()).plain(), // fast path for ASCII
            _ => {
                let utf8_len = match c {
                    0b00000000..=0b01111111 => 1,
                    0b11000000..=0b11011111 => 2,
                    0b11100000..=0b11101111 => 3,
                    0b11110000..=0b11110111 => 4,
                    _ => return Err(false),
                };
                let end = self.start - 1 + utf8_len;
                if end > self.end {
                    return Err(true);
                }
                let s = &self.buf[self.start-1..end];
                self.start = end;
                match std::str::from_utf8(s) {
                    Ok(s) => {
                        let mut it = s.chars();
                        let c = it.next().unwrap();
                        assert!(it.next().is_none());
                        Key::Char(c).plain()
                    }
                    Err(_) => return Err(false),
                }
            }
        })
    }

    fn parse_ansi_number(&mut self) -> result::Result<usize, bool> {
        let mut n = 0usize;
        let start = self.start;
        loop {
            let c = self.peek()?;
            if c >= b'0' && c <= b'9' {
                self.start += 1;
                if n > 1000000 || (n == 0 && self.start > start + 2) {
                    return Err(false);
                }
                n = n * 10 + (c - b'0') as usize;
            } else {
                return Ok(n);
            }
        }
    }

    // Returns Err if hit end of buffer before getting a complete escape sequence.
    fn parse_event(&mut self) -> result::Result<Event, bool> {
        Ok(match self.eat()? {
            // ANSI escape codes are a mess. Some escape sequences are prefixes of other escape sequences. There's no reliable way to distinguish them, so we have to guess.
            // E.g. <esc>OP could be [escape key, shift+o, shift+p] or [alt+shift+o, shift+p] or [F1].
            //
            // Main cases (from https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences ):
            // <char> -> keypress
            // <esc> -> esc
            // <esc> <char> -> alt-keypress
            // <esc> '[' -> alt-[
            // <esc> '[' (<modifier>) <char>   -> keypress, <modifier> is a decimal number and defaults to 1 (xterm)
            // <esc> '[' (<keycode>) (';'<modifier>) '~'   -> keypress, <keycode> and <modifier> are decimal numbers and default to 1 (vt)
            // <esc> '[' '<' ...  -> mouse event

            // Escape key.
            b'\x1B' if self.start == self.end && self.end < self.buf.len() => Event::Key(Key::Escape.plain()),
            // Escape sequences.
            b'\x1B' => match self.eat()? {
                // Mashing the escape key.
                b'\x1B' => {
                    self.start -= 1;
                    Event::Key(Key::Escape.plain())
                }
                // Alt+O
                b'O' if self.start == self.end && self.end < self.buf.len() => Event::Key(Key::Char('O').alt()),
                // F1-F4
                b'O' => match self.eat()? {
                    c @ b'P'..=b'S' => Event::Key(Key::F(1 + c - b'P').plain()),
                    _ => return Err(false),
                }
                // Alt+[
                b'[' if self.start == self.end && self.end < self.buf.len() => Event::Key(Key::Char('[').alt()),
                // Mouse events.
                b'[' if self.peek()? == b'<' => {
                    self.start += 1;
                    let n = self.parse_ansi_number()?;
                    if self.eat()? != b';' {
                        return Err(false);
                    }
                    let x = self.parse_ansi_number()?;
                    if self.eat()? != b';' {
                        return Err(false);
                    }
                    let y = self.parse_ansi_number()?;
                    let c = self.eat()?;

                    let mut button = match n&3 {
                        0 => MouseButton::Left,
                        1 => MouseButton::Middle,
                        2 => MouseButton::Right,
                        3 => MouseButton::None,
                        _ => panic!("huh"),
                    };
                    let mods = ModKeys::from_bits(((n>>2)&7) as u8).unwrap();
                    let mut event = if c == b'm' {MouseEvent::Release} else {MouseEvent::Press};

                    if n&32 != 0 {
                        event = MouseEvent::Move;
                    }
                    if n&64 != 0 {
                        event = match n&3 {
                            0 => MouseEvent::ScrollUp,
                            1 => MouseEvent::ScrollDown,
                            // Maybe 2 and 3 are horizontal scrolling? Idk, my mouse doesn't have horizontal scrolling.
                            _ => return Err(false),
                        };
                        button = MouseButton::None;
                    }

                    Event::Mouse(MouseEventEx {pos: [x as isize - 1, y as isize - 1], button, event, mods})
                }
                b'[' if self.peek()? == b'I' => { self.start += 1; Event::FocusIn }
                b'[' if self.peek()? == b'O' => { self.start += 1; Event::FocusOut }
                // xterm and vt escape sequences.
                b'[' => {
                    // [number] [;[number]] char
                    let n = self.parse_ansi_number()?;
                    let mut c = self.eat()?;
                    let mut mods = ModKeys::empty();
                    if c == b';' {
                        mods = Self::ansi_modifier(self.parse_ansi_number()?)?;
                        c = self.eat()?;
                    }

                    if c == b'~' {
                        // vt
                        Event::Key(Self::ansi_key_vt(n)?.mods(mods))
                    } else {
                        // xterm
                        Event::Key(Self::ansi_key_xterm(c)?.mods(mods))
                    }
                }
                // Alt+char
                c => {
                    let mut k = self.ansi_key_char(c)?;
                    k.mods.insert(ModKeys::ALT);
                    Event::Key(k)
                }
            }
            // Char (includes some special keys, some codes for ctrl+key, shift+key if uppercase, some ctrl+shift+key).
            c => Event::Key(self.ansi_key_char(c)?),
        })
    }
}


// ===================================================== imgui.rs =========================================================================


// Immediate-mode UI library, inspired by https://www.rfleury.com/p/ui-series-table-of-contents
// Currently only does TUI, but was made with both GUI and TUI in mind, so maaaybe it wouldn't be much work to make everything work for both.

// Instructions for autolayout on how to calculate widget size.
#[derive(Clone, Copy, Debug)]
pub enum AutoSize {
    Fixed(usize),
    Text,
    // Sum of children sizes if STACK flag is set, max of children sizes otherwise.
    Children,
    // Same size as parent.
    Parent,
    // Take parent size, subtract sizes of children with Remainder sizes, split the remainder among Remainder children proportional to this number.
    Remainder(f64),
}
impl Default for AutoSize { fn default() -> Self { Self::Parent } }
impl AutoSize {
    fn is_children(&self) -> bool { match self { Self::Children => true, _ => false } }
    fn is_remainder(&self) -> bool { match self { Self::Remainder(_) => true, _ => false } }
}

bitflags! {
#[derive(Default)]
pub struct AxisFlags: u32 {
    // Children will be positioned one after another, like a stack of boxes (unrelated to stack data structure). Otherwise they'll all be at the same position.
    const STACK = 0x1;

    // Position (rel_pos) won't be calculated by autolayout (it was either provided by the builder or already calculated by autolayout).
    // Otherwise rel_pos contains value from previous frame, or 0 if this widget didn't exist.
    const POS_KNOWN = 0x2;
    // Size won't be calculated by autolayout. Otherwise `size` contains value from previous frame or 0.
    const SIZE_KNOWN = 0x4;
}}

#[derive(Default, Clone)]
pub struct Axis {
    pub flags: AxisFlags,
    pub auto_size: AutoSize,

    pub size: usize,
    pub rel_pos: isize, // relative to parent widget

    // Position relative to the screen. Calculated only at the end of the frame, when rendering. Any value assigned before that is ignored.
    abs_pos: isize,
}

bitflags! {
#[derive(Default)]
pub struct WidgetFlags : u32 {
    // By default, if text doesn't fit horizontally in its Widget's width, a "…" is shown at the end. This flag disables this behavior.
    const TEXT_TRUNCATION_INDICATOR_DISABLE = 0x2;
    // If the text doesn't fit horizontally, keep a suffix rather than a prefix, and put the "…" at the start. If the text is not truncated, it's aligned left.
    const TEXT_TRUNCATION_ALIGN_RIGHT = 0x4;
    // If the widget is cut off on the left or right by its ancestor widget rects, print a '<' or '>' symbol on the corresponding sides (usually to indicate that horizontal scrolling is available, e.g. for tabs in a tab list).
    // Disables the "…" text truncation. (Note that this is quite different from text truncation: it applies when the Widget's rect is cut off by ancestors, while text truncation doesn't care about ancestors.)
    const HSCROLL_INDICATOR_ARROWS = 0x8;
    const LINE_WRAP = 0x10;

    // StyleAdjstment won't be inherited from ancestors. E.g. for drawing text input box with standard background in a panel with tinted background.
    const RESET_STYLE_ADJUSTMENT = 0x20;

    // If any part of this rect is visible (i.e. not clipped by parents' rects), trigger a redraw. May be useful for speculative hiding of elements,
    // but currently unused because I made everything non-speculative instead.
    const TRIGGER_REDRAW_IF_VISIBLE = 0x40;

    // See focus() for explanation. This flag is similar in meaning as focus() returning true.
    const LIKELY_FOCUSED = 0x80;
}}

bitflags! {
#[derive(Default)]
pub struct MouseActions : u8 {
    // Mouse click inside this Widget's rectangle, if not captured by any descendant Widget (e.g. button inside table row) and not obstructed by a later clickable Widget with overlapping rectangle (e.g. dialog box).
    const CLICK = 0x2;
    // Mouse cursor being inside this Widget's rectangle, not captured by descendant, not obstructed. Event produced every frame, not only on mouse move.
    const HOVER = 0x4;
    // Scroll wheel movement while the cursor is inside this Widget's rectangle, not captured by descendant, not obstructed.
    const SCROLL = 0x1;
    // If a CLICK happened in this widget and then the mouse is moved without releasing the left mouse button, a DRAG event will be dispatched to this widget (even if the cursor leaves it) on each frame until the button is released.
    // If both CLICK and DRAG are requested then both are delivered on the same frame when dragging starts.
    const DRAG = 0x8;
    // CLICK on this Widget or any descendant. E.g. for focusing a window on click, even if the click is also processed by some button inside the window.
    const CLICK_SUBTREE = 0x10;
    // HOVER on this Widget or any descendant. E.g. for highlighting a table row even when hovering over some button inside that row.
    const HOVER_SUBTREE = 0x20;
}}

#[derive(Default)]
pub struct Widget {
    // Identifier for matching widgets across frames. Usually a hash of something.
    // 0 means not matched across frames (but still present in prev_tree).
    pub identity: usize,
    pub axes: [Axis; 2], // x, y
    pub flags: WidgetFlags,

    pub draw_text: Option<Range<usize>>, // lines in IMGUI.text
    line_wrapped_text: Option<Range<usize>>,

    // Tree links.
    pub parent: WidgetIdx,
    depth: usize,
    pub children: Vec<WidgetIdx>,

    // Adjust all styles in the subtree. E.g. for tinting panel background or highlighting a table row.
    // Combines with adjustments from ancestors, unless RESET_STYLE_ADJUSTMENT flag is set.
    pub style_adjustment: StyleAdjustment,

    // Fill the rect with repetitions of one character (must be 1 column wide).
    pub draw_fill: Option<(char, Style)>,

    // Renders a progress bar, 1 cell high, horizontally filling this Widget.
    // The f64 is in [0, 1]. Style fg is for the left side of the progress bar, bg is for the right side.
    // If draw_text is set, the text is drawn at the center, using the progress bar's style (text color is the opposite fg/bg color of the progress bar); the text's style is ignored, only the first line is used.
    pub draw_progress_bar: Option<(f64, Style)>,

    // Put this widget somewhere close to the given widget, like a tooltip. The given widget must be earier in the render order (tree DFS order) than this widget.
    pub position_next_to: Option<WidgetIdx>,

    // Which child should be focused if this Widget is focused.
    // I.e. we follow the `focus_child` links, starting from the root, to find the innermost focused widget. Then we walk from it to the root to dispatch input.
    focus_child: Option<WidgetIdx>,

    // Which keys should be dispatched to this Widget, if it's focused.
    pub capture_keys: Vec<KeyEx>,
    // Key press events dispatched to this Widget on this frame.
    pub keys: Vec<KeyEx>,

    // Which mouse events should be dispatched to this Widget (regardless of whether it's focused).
    pub capture_mouse: MouseActions,
    // Mouse events dispatched to this Widget on this frame.
    pub mouse: MouseActions,
    pub scroll: isize, // scroll amount if `mouse` contains SCROLL
    pub mouse_pos: [isize; 2], // mouse position, relative to this widget, if `mouse` is not empty

    // Miscellaneous.
    pub scroll_bar_drag_offset: isize, // used only by scroll bar widgets
}
impl Widget {
    // Convenience functions to assign some fields, to be used before add()ing the Widget to IMGUI.
    pub fn new() -> Self { Self::default() }
    pub fn identity<T: Hash + ?Sized>(mut self, t: &T) -> Self { self.identity = hash::<T>(t); self } // example: identity(("row", row_id))
    pub fn width(mut self, s: AutoSize) -> Self { self.axes[0].auto_size = s; self }
    pub fn height(mut self, s: AutoSize) -> Self { self.axes[1].auto_size = s; self }
    pub fn fixed_width(mut self, s: usize) -> Self { self.axes[0].auto_size = AutoSize::Fixed(s); self }
    pub fn fixed_height(mut self, s: usize) -> Self { self.axes[1].auto_size = AutoSize::Fixed(s); self }
    pub fn hstack(mut self) -> Self { self.axes[0].flags.insert(AxisFlags::STACK); self }
    pub fn vstack(mut self) -> Self { self.axes[1].flags.insert(AxisFlags::STACK); self }
    pub fn fixed_x(mut self, x: isize) -> Self { self.axes[0].rel_pos = x; self.axes[0].flags.insert(AxisFlags::POS_KNOWN); self }
    pub fn fixed_y(mut self, y: isize) -> Self { self.axes[1].rel_pos = y; self.axes[1].flags.insert(AxisFlags::POS_KNOWN); self }
    pub fn text_lines(mut self, lines: Range<usize>) -> Self { self.draw_text = Some(lines); self }
    pub fn text(mut self, line: usize) -> Self { self.draw_text = Some(line..line+1); self } // example: styled_write!(imgui.text, ...); let l = imgui.text.close_line(); Widget::new().text(l);
    pub fn fill(mut self, c: char, s: Style) -> Self { self.draw_fill = Some((c, s)); self }
    pub fn flags(mut self, f: WidgetFlags) -> Self { self.flags = f; self }

    // Copy and clear all fields related to being already part of the tree. The resulting Widget can be add()ed to the tree.
    pub fn make_questionable_copy(&self) -> Self {
        let mut r = Self {
            identity: self.identity, axes: self.axes.clone(), flags: self.flags, draw_text: self.draw_text.clone(), style_adjustment: self.style_adjustment, draw_fill: self.draw_fill.clone(), draw_progress_bar: self.draw_progress_bar.clone(),
            parent: WidgetIdx::invalid(), depth: 0, children: Vec::new(), position_next_to: None, focus_child: None, capture_keys: Vec::new(), keys: Vec::new(), capture_mouse: MouseActions::empty(), mouse: MouseActions::empty(), scroll: 0, mouse_pos: [0, 0], scroll_bar_drag_offset: 0, line_wrapped_text: None};
        r.flags.remove(WidgetFlags::LIKELY_FOCUSED);
        for axis in &mut r.axes {
            axis.flags.remove(AxisFlags::POS_KNOWN | AxisFlags::SIZE_KNOWN);
        }
        r
    }
}

#[derive(Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct WidgetIdx(usize);
impl WidgetIdx {
    pub fn invalid() -> Self { Self(0) }
    pub fn is_valid(self) -> bool { self.0 != 0 }
}

#[derive(Default)]
pub struct Palette {
    pub default: Style,
    pub default_dim: Style,
    pub error: Style,

    pub running: Style,
    pub suspended: Style,

    // How to highlight things like selected table row or selected code line.
    pub selected: StyleAdjustment,
    // How to highlight clickable widgets on mouse hover.
    pub hovered: StyleAdjustment,
    // How to draw widgets with TRIGGER_REDRAW_IF_VISIBLE flag, as a sort of loading indicator. Normally it should be visible very rarely and only for one frame.
    pub placeholder_fill: Option<(char, Style)>,
    // What to prepend/append when text is truncated on the left/right. E.g. '…'.
    pub truncation_indicator: (/*left*/ String, /*right*/ String, Style),
    // What to show at left and right ends of something horizontally scrollable. E.g.: '<', '>'
    pub hscroll_indicator: (/*left*/ String, /*right*/ String, Style),
    pub line_wrap_indicator: (/*start_of_line*/ String, /*end_of_line*/ String, Style),

    pub table_header: Style,
    pub striped_table: StyleAdjustment,

    pub tab_title: Style,
    pub tab_title_pinned: Style,
    pub tab_separator: (String, Style),

    // fg - left side, bg - right side.
    pub progress_bar: Style,

    pub scroll_bar_background: Style,
    pub scroll_bar_slider: Style,

    pub tooltip: StyleAdjustment,
}

// Usage:
//   with_parent(imgui, imgui.add(Widget {...}), {
//       ...
//   })
macro_rules! with_parent {
    ($imgui:expr, $parent_expr:expr, {$($code:tt)*}) => {{
        let p = $parent_expr;
        let prev_parent = mem::replace(&mut $imgui.cur_parent, p);
        let r = {$($code)*};
        assert!($imgui.cur_parent == p);
        $imgui.cur_parent = prev_parent;
        r
    }};
}

#[derive(Default)]
pub struct IMGUI {
    // It's recommended to assign these on startup.

    pub palette: Palette,
    pub mouse_mode: MouseMode,
    pub key_binds: KeyBinds,

    // Current frame that's being built.

    pub tree: Vec<Widget>,
    pub text: StyledText,

    pub root: WidgetIdx,
    pub cur_parent: WidgetIdx,

    // By default we add 3 layers as children of root: content, dialog, tooltip. The logic around this is quite separate from the rest of IMGUI and can be moved to something like UIState instead, if needed.
    // Main content.
    pub content_root: WidgetIdx,
    // At most one dialog window can be present. It's a child of this Widget, drawn over main content, always has focus, is "owned" by some other widget, and disappears if its owner stops requesting it.
    dialog_root: WidgetIdx,
    dialog_owner: Option<usize>,
    // At most one tooltip can be present. It's drawn over everything else, never has focus, is "owned" by some other widget, and disappears if its owner stops requesting it.
    tooltip_root: WidgetIdx,
    tooltip_owner: Option<usize>,

    // Assigned by end_build().
    pub should_redraw: bool,

    // Previous frame.

    prev_tree: Vec<Widget>,
    prev_map: HashMap<usize, WidgetIdx>,
    prev_root: WidgetIdx,
    prev_focus: WidgetIdx,

    // Key presses not dispatched to widgets yet.
    input_buffer: Vec<KeyEx>,

    // Information about mouse events that happened since last frame. If there were multiple clicks, we only keep the last.
    mouse_pos: [isize; 2],
    mouse_click: Option<[isize; 2]>, // coordinates separate from mouse_pos, to be precise if a click happened in the middle of a fast motion
    mouse_scroll: isize,
    mouse_drag_widget: Option<usize>,

    // Temporary debug thing.
    log: Vec<String>,
}
impl IMGUI {
    // Returns true if any of the input was significant enough that we should redraw.
    pub fn buffer_input(&mut self, events: &[Event]) -> bool {
        let mut any_significant = false;
        for e in events {
            let mut significant = true;
            match e {
                Event::Key(key) => self.input_buffer.push(key.clone()),
                Event::FocusIn | Event::FocusOut => self.mouse_drag_widget = None,
                Event::Mouse(MouseEventEx {pos, button, event, ..}) => {
                    if button == &MouseButton::Left && event == &MouseEvent::Press {
                        self.mouse_click = Some(pos.clone());
                    } else if button == &MouseButton::Left && event == &MouseEvent::Release {
                        significant = self.mouse_drag_widget.is_some();
                        self.mouse_drag_widget = None;
                    } else if event == &MouseEvent::ScrollUp {
                        self.mouse_scroll -= 1;
                    } else if event == &MouseEvent::ScrollDown {
                        self.mouse_scroll += 1
                    } else {
                        significant = pos != &self.mouse_pos;
                    }
                    self.mouse_pos = pos.clone();
                }
            }
            any_significant |= significant;
        }
        any_significant
    }

    pub fn start_build(&mut self, width: usize, height: usize) {
        assert!(self.tree.is_empty());
        self.dispatch_input();
        self.text.clear();
        self.tree.push(Widget::default()); // 0 is reserved as invalid index
        self.should_redraw = false;

        let mut w = Widget {identity: hash(&"root"), axes: [Axis {size: width, flags: AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN, ..D!()}, Axis {size: height, flags: AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN, ..D!()}], flags: WidgetFlags::LIKELY_FOCUSED, ..D!()};
        self.carry_widget_state_from_previous_frame(&mut w);
        self.tree.push(w);
        self.root = WidgetIdx(1);
        self.cur_parent = self.root;

        self.content_root = self.add(Widget::new().identity(&"content"));
        self.dialog_root = self.add(Widget::new().identity(&"dialog"));
        self.tooltip_root = self.add(Widget::new().identity(&"tooltip"));
        let speculative_focus = if self.dialog_owner.is_some() {self.dialog_root} else {self.content_root};
        with_parent!(self, speculative_focus, {
            self.focus();
        });

        self.cur_parent = self.content_root;
    }

    pub fn end_build(&mut self, screen: &mut ScreenBuffer) -> Duration {
        // Last chance to change focus.
        {
            if self.dialog_owner.is_some() && self.tree[self.dialog_root.0].children.is_empty() {
                self.dialog_owner = None;
            }
            if self.tooltip_owner.is_some() && self.tree[self.tooltip_root.0].children.is_empty() {
                self.tooltip_owner = None;
            }
            let focus = if self.dialog_owner.is_some() {self.dialog_root} else {self.content_root};
            let w = &mut self.tree[self.root.0];
            if w.focus_child != Some(focus) {
                w.focus_child = Some(focus);
                self.should_redraw = true;
            }
        }

        // Follow the focus pointers to find the innermost focused widget.
        let mut focus = self.root;
        let mut tooltip_owner_is_focused = false;
        loop {
            let w = &self.tree[focus.0];
            tooltip_owner_is_focused |= Some(w.identity) == self.tooltip_owner;
            focus = match w.focus_child.clone() {
                None => break,
                Some(x) => x,
            };
        }
        if !tooltip_owner_is_focused {
            self.tooltip_owner = None;
            self.tree[self.tooltip_root.0].children.clear();
        }

        // Do x before y because text height may depend on its width if line wrapping is enabled.
        self.do_layout(0);
        self.do_layout(1);

        let start_time = Instant::now();
        self.render(screen);
        let render_time = Instant::now() - start_time;

        mem::swap(&mut self.tree, &mut self.prev_tree);
        self.prev_root = self.root;
        self.prev_focus = focus;
        self.tree.clear();
        self.prev_map.clear();
        for (i, w) in self.prev_tree.iter_mut().enumerate() {
            if w.identity != 0 {
                let prev = self.prev_map.insert(w.identity, WidgetIdx(i));
                if let Some(_) = prev {
                    eprintln!("warning: duplicate widget identity: {}", w.identity);
                }
            }
            w.keys.clear();
            w.mouse = MouseActions::empty();
            w.scroll = 0;
            w.mouse_pos = [0, 0];
        }

        render_time
    }

    pub fn add(&mut self, mut w: Widget) -> WidgetIdx {
        if !w.parent.is_valid() {
            w.parent = self.cur_parent;
        }

        let new_idx = WidgetIdx(self.tree.len());

        let parent = &mut self.tree[w.parent.0];
        parent.children.push(new_idx);

        let parent = &self.tree[w.parent.0];

        // Assert that we're not adding children after calculating sizes that depend on them.
        // This check is not exhaustive, e.g. nothing prevents the caller from modifying a previously added Widget in a way that breaks layout.
        for ax in 0..2 {
            if w.axes[ax].flags.contains(AxisFlags::POS_KNOWN) {
                // Allow adding floating widgets after doing layout.
                continue;
            }
            assert!(!parent.axes[ax].auto_size.is_children() || !parent.axes[ax].flags.contains(AxisFlags::SIZE_KNOWN), "tried to add child to a widget whose size depends on children, after calculating its size");
            if parent.children.len() > 1 {
                let prev_sibling = &self.tree[parent.children[parent.children.len() - 2].0];
                assert!(!prev_sibling.axes[ax].auto_size.is_remainder() || !prev_sibling.axes[ax].flags.contains(AxisFlags::SIZE_KNOWN), "tried to add sibling of a widget whose size depends on siblings, after calculating its size");
            }
        }

        // Calculate size early if possible.
        for ax in 0..2 {
            let axis = &mut w.axes[ax];
            if !axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                match &axis.auto_size {
                    AutoSize::Fixed(_) => { Self::try_calculate_simple_size(&mut w, ax, &mut self.text, &self.palette); }
                    AutoSize::Text => (), // text is often assigned by set_text(), after add()
                    AutoSize::Parent if parent.axes[ax].flags.contains(AxisFlags::SIZE_KNOWN) => { axis.size = parent.axes[ax].size; axis.flags.insert(AxisFlags::SIZE_KNOWN); }
                    AutoSize::Children | AutoSize::Remainder(_) | AutoSize::Parent => (),
                }
            }
        }

        if w.identity == 0 {
            w.identity = parent.children.len();
        }
        w.identity = hash(&(w.identity, parent.identity));
        w.depth = parent.depth + 1;

        self.carry_widget_state_from_previous_frame(&mut w);

        self.tree.push(w);
        new_idx
    }

    // Request this widget to be focused, i.e. be a recepient of keyboard input after the end of this frame.
    // The final set of focused widgets is decided at the end of the frame. It's always a single path from some widget up to the root (along focus_child links).
    // Returns the *speculative* information about whether this widget will actually be focused. Errors in both directions are possible, but they usually last very briefly; e.g. the following are possible:
    //  * focus() returns true, but then the focus is switched to a newly opened dialog before the frame build ends,
    //  * focus() returns false because an open dialog has focus, but then the dialog is closed and focus is switched to this widget before the frame build ends.
    // So, be careful when using the return value for anything other than visuals.
    //
    // (This jankiness arises because, for better or worse, we chose this combination of features:
    //   (a) be able to open/close dialog in the middle of the build,
    //   (b) avoid a one-frame period when dialog is already opened, but the input is still dispatched to main content - this seems unacceptable,
    //       e.g. maybe the user opened a search dialog and immediately started typing, and the first typed character triggered a hotkey,
    //   (c) avoid dropping inputs for one frame when opening and closing a dialog.)
    pub fn focus(&mut self) -> bool {
        assert!(self.cur().focus_child.is_none() && !self.cur().flags.contains(WidgetFlags::LIKELY_FOCUSED));
        let mut idx = self.cur_parent;
        let mut parent_idx = self.cur().parent;
        loop {
            let w = &mut self.tree[parent_idx.0];
            if w.focus_child.is_some() {
                return false;
            }
            w.focus_child = Some(idx);
            if w.flags.contains(WidgetFlags::LIKELY_FOCUSED) {
                break;
            }
            if parent_idx == self.root {
                return false;
            }
            idx = parent_idx;
            parent_idx = w.parent;
        }
        idx = self.cur_parent;
        while idx != parent_idx {
            let w = &mut self.tree[idx.0];
            assert!(!w.flags.contains(WidgetFlags::LIKELY_FOCUSED));
            w.flags.insert(WidgetFlags::LIKELY_FOCUSED);
            idx = w.parent;
        }
        true
    }

    fn carry_widget_state_from_previous_frame(&mut self, w: &mut Widget) {
        if let Some(prev_idx) = self.prev_map.get(&w.identity) {
            let prev = &mut self.prev_tree[prev_idx.0];
            for ax in 0..2 {
                let axis = &mut w.axes[ax];
                if !axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                    axis.size = prev.axes[ax].size;
                }
                if !axis.flags.contains(AxisFlags::POS_KNOWN) {
                    axis.rel_pos = prev.axes[ax].rel_pos;
                }
                axis.abs_pos = prev.axes[ax].abs_pos;
            }
            w.keys = mem::take(&mut prev.keys);
            w.mouse = mem::take(&mut prev.mouse);
            w.scroll = mem::take(&mut prev.scroll);
            w.mouse_pos = mem::take(&mut prev.mouse_pos);
            w.scroll_bar_drag_offset = mem::take(&mut prev.scroll_bar_drag_offset);
        }        
    }

    pub fn get(&self, idx: WidgetIdx) -> &Widget {
        &self.tree[idx.0]
    }
    pub fn get_mut(&mut self, idx: WidgetIdx) -> &mut Widget {
        &mut self.tree[idx.0]
    }
    pub fn cur(&self) -> &Widget {
        &self.tree[self.cur_parent.0]
    }
    pub fn cur_mut(&mut self) -> &mut Widget {
        &mut self.tree[self.cur_parent.0]
    }

    // Calculate size if auto_size is Fixed, Text, or Children, i.e. doesn't depend on parent. If size was already calculated, returns it. Otherwise panics.
    // Traverses part of the subtree as (if children have auto_size = Children too).
    // All sizes that we manage to determine are marked as final (SIZE_KNOWN) and will be excluded from later autolayout passes.
    // Therefore modifying the subtree in a way that changes sizes isn't allowed after this call (e.g. adding children if auto_size is Children).
    pub fn calculate_size(&mut self, idx: WidgetIdx, axis_idx: usize) -> usize {
        self.calculate_bottom_up_sizes(idx, axis_idx, false);
        let axis = &self.tree[idx.0].axes[axis_idx];
        assert!(axis.flags.contains(AxisFlags::SIZE_KNOWN));
        axis.size
    }

    // Calculate sizes and relative positions of cur_parent's children. Particularly useful if some children have auto_size Remainder. Same caveats as calculate_size() - calculated sizes and positions are final.
    pub fn layout_children(&mut self, axis_idx: usize) {
        let idx = self.cur_parent;
        self.calculate_size(idx, axis_idx);
        let mut scratch = self.tree[idx.0].children.clone();
        for &c in &scratch {
            match &self.tree[c.0].axes[axis_idx].auto_size {
                AutoSize::Fixed(_) | AutoSize::Text | AutoSize::Children => self.calculate_bottom_up_sizes(c, axis_idx, false),
                AutoSize::Parent | AutoSize::Remainder(_) => (),
            }
        }
        self.calculate_children_layout(idx, axis_idx, &mut scratch);
    }

    // Convenience functions.

    // Close the unclosed line from self.text, assign that line as cur_parent's text to draw, and calculate cur_parent's size if AutoSize::Text.
    // Usage:
    // with_parent(imgui, ..., {
    //     styled_write!(imgui.text, ...);
    //     imgui.set_text();
    // });
    pub fn set_text(&mut self) {
        let i = self.text.close_line();
        let w = &mut self.tree[self.cur_parent.0];
        w.draw_text = Some(i..i+1);
        for ax in 0..2 {
            let axis = &mut w.axes[ax];
            match axis.auto_size {
                AutoSize::Text => { Self::try_calculate_simple_size(w, ax, &mut self.text, &self.palette); }
                _ => (),
            }
        }
    }

    // Requests input for cur_parent for next frame, returns input that was requested on previous frame.
    // Multiple such calls for different keys can coexist on the same Widget; each call will return only the actions that it requested.
    pub fn check_keys(&mut self, actions: &[KeyAction]) -> Vec<KeyAction> {
        let req = self.key_binds.actions_to_keys(actions);
        let w = &mut self.tree[self.cur_parent.0];
        w.capture_keys.extend_from_slice(&req);
        let mut res: Vec<KeyAction> = Vec::new();
        w.keys.retain(|key| {
            match self.key_binds.key_to_action.get(key) {
                Some(a) if actions.iter().position(|x| x == a).is_some() => {
                    res.push(*a);
                    false
                }
                _ => true
            }
        });
        res
    }

    pub fn check_key(&mut self, action: KeyAction) -> bool {
        let keys = match self.key_binds.action_to_keys.get(&action) {
            Some(x) => x,
            None => return false };
        let w = &mut self.tree[self.cur_parent.0];
        w.capture_keys.extend_from_slice(keys);
        let mut res = false;
        w.keys.retain(|key| {
            match keys.iter().position(|k| k == key) {
                None => true,
                Some(_) => {
                    res = true;
                    false
                }
            }
        });
        res
    }

    pub fn check_mouse(&mut self, e: MouseActions) -> bool {
        let w = &mut self.tree[self.cur_parent.0];
        w.capture_mouse.insert(e);
        w.mouse.contains(e)
    }
    pub fn check_scroll(&mut self) -> isize {
        let w = &mut self.tree[self.cur_parent.0];
        w.capture_mouse.insert(MouseActions::SCROLL);
        w.scroll
    }
    pub fn check_drag(&mut self) -> Option<[isize; 2]> {
        let w = &mut self.tree[self.cur_parent.0];
        w.capture_mouse.insert(MouseActions::DRAG);
        if w.mouse.contains(MouseActions::DRAG) {
            Some(w.mouse_pos)
        } else {
            None
        }
    }

    // If cur_parent owns a dialog, returns a full-screen widget to which to add the dialog as a child.
    // If `create`, the dialog will be created if no dialog exists (neither owned by this widget nor anyone else).
    // If no children are added to the returned widget before the end of the frame, the dialog is considered closed.
    pub fn check_dialog(&mut self, create: bool) -> Option<WidgetIdx> {
        let identity = self.cur().identity;
        match &self.dialog_owner {
            Some(id) if id == &identity => Some(self.dialog_root),
            None if create => {
                self.dialog_owner = Some(identity);
                Some(self.dialog_root)
            }
            _ => None,
        }
    }

    // Default behavior for opening/closing tooltip: open on KeyAction::Tooltip, close on Tooltip or Cancel, position next to cur_parent.
    pub fn check_tooltip(&mut self) -> Option<WidgetIdx> {
        let identity = self.cur().identity;
        let pressed = self.check_key(KeyAction::Tooltip);
        match self.tooltip_owner.clone() {
            Some(id) if id != identity => return None, // someone else has a tooltip
            Some(_) if pressed || self.check_key(KeyAction::Cancel) => return None, // closed
            Some(_) => (), // exists
            None if pressed => self.tooltip_owner = Some(identity), // opened
            None => return None,
        }
        let idx = self.cur_parent;
        Some(with_parent!(self, self.tooltip_root, {
            let mut w = Widget::new().width(AutoSize::Children).height(AutoSize::Children).fill(' ', self.palette.default);
            w.position_next_to = Some(idx);
            w.style_adjustment = self.palette.tooltip;
            self.add(w)
        }))
    }

    fn calculate_bottom_up_sizes(&mut self, root: WidgetIdx, ax: usize, all: bool) {
        let mut stack: Vec<(WidgetIdx, /*pass*/ u8)> = vec![(root, 0)];
        while let Some((idx, pass)) = stack.pop() {
            let w = &mut self.tree[idx.0];
            let axis = &mut w.axes[ax];
            
            if pass == 0 && !w.children.is_empty() && (all || (!axis.flags.contains(AxisFlags::SIZE_KNOWN) && axis.auto_size.is_children())) {
                // Visit children first.
                stack.push((idx, 1));
                for &c in &w.children {
                    stack.push((c, 0));
                }
                continue;
            }

            match &axis.auto_size {
                _ if axis.flags.contains(AxisFlags::SIZE_KNOWN) => (),
                AutoSize::Fixed(_) | AutoSize::Text => { Self::try_calculate_simple_size(w, ax, &mut self.text, &self.palette); }
                AutoSize::Parent => assert!(all, "can't calculate size early because it depends on parent"),
                AutoSize::Remainder(_) => assert!(all, "can't calculate size early because it depends on siblings"),
                AutoSize::Children => {
                    let mut size = 0usize;
                    let w = &self.tree[idx.0];
                    for &child_idx in &w.children {
                        let child_axis = &self.tree[child_idx.0].axes[ax];
                        if !child_axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                            continue;
                        }
                        if w.axes[ax].flags.contains(AxisFlags::STACK) {
                            size += child_axis.size;
                        } else {
                            size = size.max(child_axis.size);
                        }
                    }
                    let axis = &mut self.tree[idx.0].axes[ax];
                    axis.size = size;
                    axis.flags.insert(AxisFlags::SIZE_KNOWN);
                }
            }
        }
    }

    fn try_calculate_simple_size(w: &mut Widget, ax: usize, text: &mut StyledText, palette: &Palette) -> bool {
        let size = match w.axes[ax].auto_size {
            AutoSize::Fixed(s) => s,
            AutoSize::Text if ax == 0 => w.draw_text.clone().unwrap().map(|j| str_width(text.get_line_str(j))).max().unwrap_or(0), // text width
            AutoSize::Text if !w.flags.contains(WidgetFlags::LINE_WRAP) => w.draw_text.as_ref().unwrap().len(), // height without line wrap
            AutoSize::Text if w.axes[0].flags.contains(AxisFlags::SIZE_KNOWN) => Self::get_line_wrapped_text(w, text, palette).len(), // height with line wrap
            _ => return false,
        };
        w.axes[ax].size = size;
        w.axes[ax].flags.insert(AxisFlags::SIZE_KNOWN);
        true
    }

    fn get_line_wrapped_text(w: &mut Widget, text: &mut StyledText, palette: &Palette) -> Range<usize> {
        match &w.line_wrapped_text {
            Some(x) => x.clone(),
            None => {
                let r = text.line_wrap(w.draw_text.clone().unwrap(), w.axes[0].size, /*max_lines*/ 1000, &palette.line_wrap_indicator, &palette.truncation_indicator);
                w.line_wrapped_text = Some(r.clone());
                r
            }
        }
    }

    fn calculate_children_layout(&mut self, idx: WidgetIdx, ax: usize, scratch: &mut Vec<WidgetIdx>) {
        let w = &self.tree[idx.0];
        let axis = &w.axes[ax];
        let flags = axis.flags;
        let parent_size = axis.size;
        assert!(flags.contains(AxisFlags::SIZE_KNOWN));
        scratch.clear();
        scratch.extend_from_slice(&w.children);

        let mut remainder_helper = RemainderFractionHelper::new(parent_size);

        // Resolve AutoSize::Parent in children, add up final non-Remainder children sizes.
        for &child_idx in &*scratch {
            let child_axis = &mut self.tree[child_idx.0].axes[ax];

            if !child_axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                match &child_axis.auto_size {
                    AutoSize::Parent => { child_axis.size = parent_size; child_axis.flags.insert(AxisFlags::SIZE_KNOWN); }
                    AutoSize::Remainder(f) if !flags.contains(AxisFlags::STACK) => { child_axis.size = (parent_size as f64 * f.max(0.0) + 0.5) as usize; child_axis.flags.insert(AxisFlags::SIZE_KNOWN); }
                    AutoSize::Remainder(f) => remainder_helper.declare_remainder_fraction(*f, child_idx.0),
                    AutoSize::Children | AutoSize::Fixed(_) | AutoSize::Text => panic!("size {:?} was supposed to be resolved by now", child_axis.auto_size),
                }
            }
            if child_axis.flags.contains(AxisFlags::SIZE_KNOWN) { // (no, this can't be an `else`)
                remainder_helper.declare_fixed_part(child_axis.size);
            }
        }

        // Calculate Remainder sizes and all positions.
        let mut position = 0isize;
        for &child_idx in &*scratch {
            let child = &mut self.tree[child_idx.0];
            let child_axis = &mut child.axes[ax];
            if !child_axis.flags.contains(AxisFlags::POS_KNOWN) {
                child_axis.rel_pos = position;
            }
            if !child_axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                let f = match &child_axis.auto_size {
                    AutoSize::Remainder(f) => f,
                    _ => panic!("huh"),
                };
                child_axis.size = remainder_helper.calculate_remainder_fraction(*f, child_idx.0);
                child_axis.flags.insert(AxisFlags::SIZE_KNOWN);
            }
            if flags.contains(AxisFlags::STACK) {
                position = child_axis.rel_pos + child_axis.size as isize;
            }
        }
    }

    fn do_layout(&mut self, ax: usize) {
        // DFS 1: bottom-to-top to calculate sizes that depend on children.
        self.calculate_bottom_up_sizes(self.root, ax, true);

        // DFS 2: top-to-bottom to calculate sizes that depend on parent, and calculate positions.
        let mut stack: Vec<WidgetIdx> = vec![self.root];
        let mut scratch: Vec<WidgetIdx> = Vec::new();
        while let Some(idx) = stack.pop() {
            self.calculate_children_layout(idx, ax, &mut scratch);

            for &c in &self.tree[idx.0].children {
                stack.push(c);
            }
        }
    }

    fn render(&mut self, screen: &mut ScreenBuffer) {
        let mut stack: Vec<(WidgetIdx, /*pos*/ [isize; 2], /*clip*/ Rect, StyleAdjustment)> = vec![(self.root, [0, 0], screen.rect(), StyleAdjustment::default())];
        let mut should_redraw = false;
        while let Some((idx, pos, clip, mut style_adjustment)) = stack.pop() {
            let w = &self.tree[idx.0];
            let mut pos = [pos[0] + w.axes[0].rel_pos, pos[1] + w.axes[1].rel_pos];

            if let Some(other_idx) = w.position_next_to.clone() {
                let root = &self.tree[self.root.0];
                let other = &self.tree[other_idx.0];
                // Candidate sides relative to other: up, left, bottom, right.
                let scores = [other.axes[1].abs_pos - w.axes[1].size as isize, other.axes[0].abs_pos - w.axes[0].size as isize,
                              root.axes[1].size as isize - other.axes[1].abs_pos - other.axes[1].size as isize - w.axes[1].size as isize,
                              root.axes[0].size as isize - other.axes[0].abs_pos - other.axes[0].size as isize - w.axes[0].size as isize];
                let which = scores.iter().enumerate().max_by_key(|(_, s)| *s).unwrap().0;
                pos = [other.axes[0].abs_pos + other.axes[0].size as isize / 2 - w.axes[0].size as isize / 2,
                       other.axes[1].abs_pos + other.axes[1].size as isize / 2 - w.axes[1].size as isize / 2];
                match which {
                    0 => pos[1] = other.axes[1].abs_pos - w.axes[1].size as isize,
                    1 => pos[0] = other.axes[0].abs_pos - w.axes[0].size as isize,
                    2 => pos[1] = other.axes[1].abs_pos + other.axes[1].size as isize,
                    3 => pos[1] = other.axes[0].abs_pos + other.axes[0].size as isize,
                    _ => panic!("huh"),
                }
                pos[0] = pos[0].min(root.axes[0].size as isize - w.axes[0].size as isize).max(0);
                pos[1] = pos[1].min(root.axes[1].size as isize - w.axes[1].size as isize).max(0);
            }

            let mut draw_text = w.draw_text.clone();
            let mut show_text_truncation_indicator = !w.flags.contains(WidgetFlags::TEXT_TRUNCATION_INDICATOR_DISABLE);

            let w = &mut self.tree[idx.0];
            w.axes[0].abs_pos = pos[0];
            w.axes[1].abs_pos = pos[1];
            if draw_text.is_some() && w.flags.contains(WidgetFlags::LINE_WRAP) {
                draw_text = Some(Self::get_line_wrapped_text(w, &mut self.text, &self.palette));
                show_text_truncation_indicator = false;
            }
            
            let w = &self.tree[idx.0];

            let rect = Rect {x: pos[0], y: pos[1], w: w.axes[0].size, h: w.axes[1].size};
            let mut clip = clip.intersection(rect);

            if w.flags.contains(WidgetFlags::RESET_STYLE_ADJUSTMENT) {
                style_adjustment = StyleAdjustment::default();
            }
            style_adjustment.update(w.style_adjustment);

            if w.flags.contains(WidgetFlags::TRIGGER_REDRAW_IF_VISIBLE) && !clip.is_empty() {
                should_redraw = true;

                if let Some((c, style)) = self.palette.placeholder_fill.clone() {
                    let mut buf = [0u8; 4];
                    screen.fill(clip, c.encode_utf8(&mut buf), style);
                }
            }

            if w.flags.contains(WidgetFlags::HSCROLL_INDICATOR_ARROWS) {
                if rect.right() > clip.right() && clip.w > 0 {
                    let wid = str_width(&self.palette.hscroll_indicator.1);
                    for y in clip.y..clip.bottom() {
                        screen.put_text(&self.palette.hscroll_indicator.1, style_adjustment.apply(self.palette.hscroll_indicator.2), clip.right() - wid as isize, y, clip);
                    }
                    clip.w -= wid;
                }
                if rect.x < clip.x && rect.w > 0 {
                    let wid = str_width(&self.palette.hscroll_indicator.0);
                    for y in clip.y..clip.bottom() {
                        screen.put_text(&self.palette.hscroll_indicator.0, style_adjustment.apply(self.palette.hscroll_indicator.2), clip.x, y, clip);
                    }
                    clip.x += wid as isize;
                    clip.w -= wid;
                }
                show_text_truncation_indicator = false;
            }

            if let Some((c, style)) = w.draw_fill.clone() {
                let mut buf = [0u8; 4];
                screen.fill(clip, c.encode_utf8(&mut buf), style_adjustment.apply(style));
            }

            if let &Some((progress, style)) = &w.draw_progress_bar {
                let text = match &draw_text {
                    Some(r) if !r.is_empty() => self.text.get_line_str(r.start),
                    _ => "",
                };
                let text_x = rect.x + (rect.w as isize - str_width(text) as isize) / 2;

                let filled = (rect.w as f64 * progress.min(1.0).max(0.0) + 0.5) as usize;
                let left_rect = clip.intersection(Rect {x: rect.x, y: rect.y, w: filled, h: 1});
                let right_rect = clip.intersection(Rect {x: rect.x + filled as isize, y: rect.y, w: rect.w - filled, h: 1});

                let right_style = style_adjustment.apply(style);
                let left_style = right_style.flip();
                screen.fill(left_rect, " ", left_style);
                screen.fill(right_rect, " ", right_style);
                screen.put_text(text, left_style, text_x, rect.y, left_rect);
                screen.put_text(text, right_style, text_x, rect.y, right_rect);
                draw_text = None;
            }

            if let Some(text_range) = draw_text {
                for (y_offset, line_idx) in text_range.clone().enumerate() {
                    let mut spans = self.text.get_line(line_idx);
                    let line_str = self.text.get_line_str(line_idx);
                    let y = rect.y + y_offset as isize;
                    let mut x = rect.x;

                    if str_width(line_str) <= rect.w {
                        show_text_truncation_indicator = false;
                    } else if w.flags.contains(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT) {
                        if show_text_truncation_indicator {
                            x = screen.put_text(&self.palette.truncation_indicator.0, style_adjustment.apply(self.palette.truncation_indicator.2), x, y, clip);
                        }
                        let lim = (rect.right() - x).max(0) as usize;

                        let (mut skip, truncated_width) = str_suffix_with_width(line_str, lim);
                        x += (lim - truncated_width) as isize; // in case a wide character is half-truncated

                        // Find and print the partially truncated span.
                        while !spans.is_empty() {
                            let (s, style) = self.text.get_span(spans.start);
                            spans.start += 1;
                            if s.len() <= skip {
                                skip -= s.len();
                                continue;
                            }
                            x = screen.put_text(&s[skip..], style_adjustment.apply(style), x, y, clip);
                            break;
                        }
                        // Print the rest of the spans the normal way.
                        show_text_truncation_indicator = false;
                    }

                    for span_idx in self.text.get_line(line_idx) {
                        let (s, style) = self.text.get_span(span_idx);
                        x = screen.put_text(s, style_adjustment.apply(style), x, y, clip);
                        if x > clip.right() {
                            break;
                        }
                    }
                    if show_text_truncation_indicator && rect.w > 0 {
                        screen.put_text(&self.palette.truncation_indicator.1, style_adjustment.apply(self.palette.truncation_indicator.2), rect.right() - str_width(&self.palette.truncation_indicator.1) as isize, y, clip);
                    }
                }
            }

            for &c in w.children.iter().rev() {
                stack.push((c, pos, clip, style_adjustment.clone()));
            }
        }
        self.should_redraw |= should_redraw;
    }

    fn dispatch_input(&mut self) {
        if self.prev_tree.is_empty() {
            return;
        }

        // Walk up from innermost focused widget and assign key press events to widgets.
        let mut idx = self.prev_focus;
        let mut keys = mem::take(&mut self.input_buffer);
        while idx != self.prev_root && !keys.is_empty() {
            let w = &mut self.prev_tree[idx.0];
            if !w.capture_keys.is_empty() {
                keys.retain(|key| {
                    if w.capture_keys.iter().find(|k| k == &key).is_some() {
                        w.keys.push(key.clone());
                        false
                    } else {
                        true
                    }
                });
            }
            idx = w.parent;
        }
        // All remaining keys go to the root, ignoring its capture_keys.
        self.prev_tree[self.prev_root.0].keys = keys;

        let report_event = |w: &mut Widget, ev: MouseActions, pos: [isize; 2]| {
            w.mouse.insert(ev);
            w.mouse_pos = [pos[0] - w.axes[0].abs_pos, pos[1] - w.axes[1].abs_pos];
        };
        let hovered_idx = self.find_widget_at_cursor(self.mouse_pos);
        if let Some(click_pos) = mem::take(&mut self.mouse_click) {
            self.mouse_drag_widget = None;
            let mut idx = if click_pos == self.mouse_pos {
                hovered_idx
            } else {
                self.find_widget_at_cursor(click_pos)
            };
            let mut dispatched = false;
            while idx.is_valid() {
                let w = &mut self.prev_tree[idx.0];
                if !dispatched {
                    if w.capture_mouse.contains(MouseActions::CLICK) {
                        report_event(w, MouseActions::CLICK, click_pos);
                        dispatched = true;
                    }
                    if w.capture_mouse.contains(MouseActions::DRAG) {
                        self.mouse_drag_widget = Some(w.identity);
                        dispatched = true;
                    }
                }
                if w.capture_mouse.contains(MouseActions::CLICK_SUBTREE) {
                    report_event(w, MouseActions::CLICK_SUBTREE, click_pos);
                }
                idx = w.parent;
            }
        }

        if let Some(identity) = self.mouse_drag_widget.clone() {
            if let Some(idx) = self.prev_map.get(&identity) {
                let w = &mut self.prev_tree[idx.0];
                if w.capture_mouse.contains(MouseActions::DRAG) {
                    report_event(w, MouseActions::DRAG, self.mouse_pos);
                } else {
                    self.mouse_drag_widget = None;
                }
            } else {
                self.mouse_drag_widget = None;
            }
        }

        let mut scroll = mem::take(&mut self.mouse_scroll);
        let mut hover = true;
        if self.mouse_drag_widget.is_some() {
            scroll = 0;
            hover = false;
        }
        let mut idx = hovered_idx;
        while idx.is_valid() {
            let w = &mut self.prev_tree[idx.0];
            if hover && w.capture_mouse.contains(MouseActions::HOVER) {
                report_event(w, MouseActions::HOVER, self.mouse_pos);
                hover = false;
            }
            if self.mouse_drag_widget.is_none() && w.capture_mouse.contains(MouseActions::HOVER_SUBTREE) {
                report_event(w, MouseActions::HOVER_SUBTREE, self.mouse_pos);
            }
            if scroll != 0 && w.capture_mouse.contains(MouseActions::SCROLL) {
                report_event(w, MouseActions::SCROLL, self.mouse_pos);
                w.scroll = mem::take(&mut scroll);
            }
            idx = w.parent;
        }
    }

    fn find_widget_at_cursor(&self, cursor: [isize; 2]) -> WidgetIdx {
        let mut stack: Vec<WidgetIdx> = vec![self.prev_root];
        let mut res = WidgetIdx::invalid();
        while let Some(idx) = stack.pop() {
            let w = &self.prev_tree[idx.0];
            if cursor[0] < w.axes[0].abs_pos || cursor[1] < w.axes[1].abs_pos || cursor[0] >= w.axes[0].abs_pos + w.axes[0].size as isize || cursor[1] >= w.axes[1].abs_pos + w.axes[1].size as isize {
                continue;
            }
            // When should a later overlapping widget obstruct mouse interactions? E.g. should clicks on the tooltip be ignored or dispatched as if the tooltip is not there?
            // Currently the answer is: dispatched as if the tooltip is not there (as long as the tooltip has empty capture_mouse).
            if !w.capture_mouse.is_empty() {
                res = idx;
            }
            for &c in w.children.iter().rev() {
                stack.push(c);
            }
        }
        res
    }
}

// Utility for calculating AutoSize::Remainder(fraction) sizes.
pub struct RemainderFractionHelper {
    size_total: usize,
    fraction_total: f64,
    size_allocated: usize,
    fraction_allocated: f64,
    last_remainder_idx: usize,
}
impl RemainderFractionHelper {
    pub fn new(size_total: usize) -> Self { Self {size_total, fraction_total: 0.0, size_allocated: 0, fraction_allocated: 0.0, last_remainder_idx: 0} }
    pub fn declare_fixed_part(&mut self, size: usize) {
        self.size_total = self.size_total.saturating_sub(size);
    }
    pub fn declare_remainder_fraction(&mut self, fraction: f64, idx: usize) {
        self.fraction_total += fraction.max(0.0);
        self.last_remainder_idx = idx;
    }
    pub fn calculate_remainder_fraction(&mut self, fraction: f64, idx: usize) -> usize {
        self.fraction_allocated += fraction.max(0.0);
        let fraction_total = if self.fraction_total == 0.0 {1.0} else {self.fraction_total};
        let mut s = (self.size_total as f64 * self.fraction_allocated / fraction_total + 0.5) as usize;
        if idx == self.last_remainder_idx {
            s = self.size_total;
        }
        s = s.max(self.size_allocated).min(self.size_total);
        s - mem::replace(&mut self.size_allocated, s)
    }
}


// ======================================================= settings.rs =========================================================================


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

pub struct KeyBinds {
    pub key_to_action: HashMap<KeyEx, KeyAction>,
    pub action_to_keys: HashMap<KeyAction, Vec<KeyEx>>,
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

    fn init(&mut self) {
        for (key, action) in &self.key_to_action {
            self.action_to_keys.entry(action.clone()).or_default().push(key.clone());
        }
    }
}
impl Default for KeyBinds {
    fn default() -> Self {
        let mut res = Self {vscroll_sensitivity: 1, hscroll_sensitivity: 1, key_to_action: HashMap::from([
            (Key::Char('q').plain(), KeyAction::Quit),
            (Key::Char('r').plain(), KeyAction::Run),
            (Key::Char('c').plain(), KeyAction::Continue),
            (Key::Char('C').plain(), KeyAction::Suspend),
            (Key::Char('c').ctrl(), KeyAction::Suspend),
            (Key::Char('k').plain(), KeyAction::Kill),
            (Key::Char('w').ctrl(), KeyAction::WindowUp),
            (Key::Char('s').ctrl(), KeyAction::WindowDown),
            (Key::Char('a').ctrl(), KeyAction::WindowLeft),
            (Key::Char('d').ctrl(), KeyAction::WindowRight),
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
            (Key::Char('t').ctrl(), KeyAction::NextTab),
            (Key::Char('b').ctrl(), KeyAction::PreviousTab),
            (Key::Char('y').ctrl(), KeyAction::PinTab),
            (Key::Char(']').plain(), KeyAction::NextStackFrame),
            (Key::Char('[').plain(), KeyAction::PreviousStackFrame),
            (Key::Char('}').plain(), KeyAction::NextThread),
            (Key::Char('{').plain(), KeyAction::PreviousThread),
            (Key::Char('.').plain(), KeyAction::NextMatch),
            (Key::Char(',').plain(), KeyAction::PreviousMatch),
            (Key::Char('o').plain(), KeyAction::Open),
            (Key::Char('b').plain(), KeyAction::ToggleBreakpoint),
            (Key::Char('B').plain(), KeyAction::ToggleBreakpointEnabledness),
            (Key::Char('/').plain(), KeyAction::Find),
            (Key::Char('d').plain(), KeyAction::DuplicateRow),
            (Key::Char('l').ctrl(), KeyAction::DropCaches),
            (Key::Char('p').ctrl(), KeyAction::ToggleProfiler),
        ]), action_to_keys: HashMap::new()};
        res.init();
        res
    }
}

// ======================================================= widgets.rs =========================================================================

// React to keys like up/down/pageup/pagedown/home/end by moving `cursor`, within range [0, count).
// `cur_parent` is used for input; the caller should make sure it's focused for navigation to work.
// `viewport_height` and `row_height` are used for pageup/pagedown jump size; if None, use height of `cur_parent`.
// Returns true iff any relevant keys were pressed, even if it didn't move the cursor (e.g. trying to move up when already at the top) - useful as a trigger to scroll the view to the cursor.
pub fn list_cursor_navigation(cursor: &mut usize, count: usize, viewport_height: Option<usize>, row_height: usize, imgui: &mut IMGUI) -> bool {
    list_cursor_navigation_with_variable_row_height(cursor, count, viewport_height, |_, _| row_height, imgui)
}

pub fn list_cursor_navigation_with_variable_row_height<F: FnMut(usize, &mut IMGUI) -> usize>(cursor: &mut usize, count: usize, viewport_height: Option<usize>, mut row_height_fn: F, imgui: &mut IMGUI) -> bool {
    let viewport_height = match viewport_height {
        Some(x) => x,
        None => imgui.cur().axes[1].size,
    };
    let actions = imgui.check_keys(&[KeyAction::CursorUp, KeyAction::CursorDown, KeyAction::PageUp, KeyAction::PageDown, KeyAction::Home, KeyAction::End]);
    let moved = !actions.is_empty();
    if count == 0 {
        return moved;
    }
    for action in actions {
        match action {
            KeyAction::CursorUp => *cursor = cursor.saturating_sub(1),
            KeyAction::CursorDown => *cursor = (*cursor + 1).min(count - 1),
            KeyAction::PageDown => {
                // Take current row's top y coordinate, add viewport height, find the row that covers that y coordinate.
                let mut offset = viewport_height;
                let mut i = (*cursor).min(count - 1);
                while i + 1 < count {
                    let h = row_height_fn(i, imgui);
                    if h > offset {
                        if i == *cursor {
                            i += 1;
                        }
                        break;
                    }
                    offset -= h;
                    i += 1;
                }
                *cursor = i;
            }
            KeyAction::PageUp => {
                let mut offset = viewport_height;
                let mut i = (*cursor).min(count - 1);
                while i > 0 {
                    let h = row_height_fn(i - 1, imgui);
                    if h > offset {
                        if i == *cursor {
                            i -= 1;
                        }
                        break;
                    }
                    offset -= h;
                    i -= 1;
                }
                *cursor = i;
            }
            KeyAction::Home => *cursor = 0,
            KeyAction::End => *cursor = count - 1,
            _ => (),
        }
    }
    moved
}

// Manages a vertically scrollable area. Widgets involved:
//  * `imgui.cur_parent` - the viewport
//  * `imgui.cur_parent`'s only child - content, moved vertically according to scroll position
//  * `container` - respond to scroll wheel when the mouse is over this widget (usually an ancestor of the viewport that also includes things like scroll bar and table header)
//  * `scroll_bar` - tall widget of width 1 to the right of the viewport; should be initially empty, populated by this function; pass WidgetIdx::invalid() to disable
// If `scroll_to` is set, we'll scroll to the nearest position such that this range of y coordinates is visible (or as much as possible is visible, if it's taller than the viewport).
// Returns the visible range of y coordinates.
pub fn scrolling_navigation(scroll: &mut isize, scroll_to: Option<Range<isize>>, container: WidgetIdx, scroll_bar: WidgetIdx, imgui: &mut IMGUI) -> Range<isize> {
    with_parent!(imgui, container, {
        *scroll += imgui.check_scroll() * imgui.key_binds.vscroll_sensitivity;
    });

    let w = imgui.cur();
    assert_eq!(w.children.len(), 1);
    assert!(w.axes[1].flags.contains(AxisFlags::SIZE_KNOWN), "scrollable viewport height must be calculated in advance");
    let viewport_height = w.axes[1].size;
    let content_height = imgui.calculate_size(w.children[0], 1);

    if let Some(r) = scroll_to {
        scroll_to_range(scroll, r, viewport_height);
    }

    *scroll = (*scroll).min(content_height as isize - viewport_height as isize).max(0);

    // Handle scroll bar.
    // ┳  ╷  ╷
    // ╇  ╈  │
    // │  ╇  ╈
    // ╵  ╵  ┻
    if scroll_bar.is_valid() && content_height > viewport_height {
        with_parent!(imgui, scroll_bar, {
            assert!(imgui.cur().axes[1].flags.contains(AxisFlags::SIZE_KNOWN), "scroll bar widget height should be calculated in advance");
            let bar_height = imgui.cur().axes[1].size.saturating_sub(1); // -1 because top and bottom half-characters are empty
            if bar_height >= 3 {
                // Rounded down because we want non-full scrollbar when the content is just slightly taller than viewport.
                let slider_height = (bar_height * viewport_height / content_height).max(2);
                assert!(slider_height < bar_height);

                // Set slider position from scroll position.
                let scroll_range = (content_height - viewport_height) as isize; // 0 <= *scroll <= scroll_range
                let slider_range = (bar_height - slider_height) as isize; // 0 <= slider_y <= slider_range
                let mut slider_y = (slider_range * *scroll + scroll_range/2) / scroll_range;
                assert!(slider_y >= 0 && slider_y <= slider_range);

                let clicked = imgui.check_mouse(MouseActions::CLICK);
                if let Some([_, y]) = imgui.check_drag() {
                    let a = imgui.palette.selected;
                    let w = imgui.cur_mut();
                    w.style_adjustment.update(a);

                    let mid = slider_height as isize / 2;
                    if clicked {
                        // Started dragging, save the cursor offset from slider center.
                        w.scroll_bar_drag_offset = if y >= slider_y && y <= slider_y + slider_height as isize {
                            y - slider_y - mid
                        } else {
                            0
                        };
                    }
                    // Move the slider to the cursor.
                    slider_y = (y - w.scroll_bar_drag_offset - mid).min(slider_range).max(0);
                    // Set scroll position from slider position. Make sure min/max slider positions correspond to min/max scroll positions.
                    *scroll = (slider_y * scroll_range + slider_range/2) / slider_range;
                    assert!(*scroll >= 0 && *scroll <= scroll_range);
                }

                if imgui.check_mouse(MouseActions::HOVER) {
                    let a = imgui.palette.hovered;
                    imgui.cur_mut().style_adjustment.update(a);
                }

                let start = imgui.text.num_lines();
                for i in 0..bar_height + 1 {
                    if i == 0 && slider_y == 0 {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "┳");
                    } else if i == 0 {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_background, "╷");
                    } else if i as isize == slider_y {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "╈");
                    } else if i == bar_height && slider_y == slider_range {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "┻");
                    } else if i == bar_height {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_background, "╵");
                    } else if i as isize == slider_y + slider_height as isize {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "╇");
                    } else if i as isize > slider_y && (i as isize) < slider_y + slider_height as isize {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "┃");
                    } else {
                        styled_write!(imgui.text, imgui.palette.scroll_bar_slider, "│");
                    }
                    imgui.text.close_line();
                }
                imgui.cur_mut().draw_text = Some(start..imgui.text.num_lines());
            }
        });
    }

    let idx = imgui.cur().children[0];
    let w = imgui.get_mut(idx);
    w.axes[1].flags.insert(AxisFlags::POS_KNOWN);
    w.axes[1].rel_pos = -*scroll;

    *scroll..*scroll+viewport_height as isize
}

pub fn scroll_to_range(scroll: &mut isize, r: Range<isize>, viewport: usize) {
    if r.end - r.start > viewport as isize {
        *scroll = (*scroll).max(r.start).min(r.end - viewport as isize);
    } else {
        let margin = (viewport as isize / 4).min(5).min((viewport as isize - r.end + r.start) / 2);
        *scroll = (*scroll).max(r.end - viewport as isize + margin).min(r.start - margin);
    }
}

#[derive(Default)]
pub struct Column {
    pub title: &'static str,
    pub auto_width: AutoSize, // Fixed, Text, Children, Remainder are supported
    width: usize,
    pos: isize,
    // TODO: sortable: bool,
}
impl Column {
    pub fn new(title: &'static str, auto_width: AutoSize) -> Self { Self {auto_width, title, width: str_width(title), pos: 0} }
}

#[derive(Default, Clone)]
pub struct TableState {
    pub cursor: usize,
    pub scroll: isize,
    // TODO: pub auto_tooltip: bool,
    // TODO: sort_by: Vec<usize>,
}

// For columns with auto_width = Text/Children, the width negotiation is a bit complicated:
//  1. Initially each cell+header of the column is created with the corresponding AutoSize.
//  2. After the whole table is populated, finish_layout() calls calculate_size() for all cells and calculates the final column width.
//     Then it iterates over all these cells again and changes their size (despite AxisFlags::SIZE_KNOWN!) to the column width.
//  3. Then the auto-tooltip code may clone cell's Widget subtree, change the cell width (usually to make it much wider), and recalculate layout inside the cell (e.g. a progress bar may fill the parent and become wider).
pub struct Table {
    pub state: TableState,
    pub enable_selection_icon: bool, // draw a '➤' to the left of the selected row
    pub hide_cursor_if_unfocused: bool,
    pub enable_tooltip: bool,
    columns: Vec<Column>,

    pub scroll_to_cursor: bool,

    root: WidgetIdx,
    viewport: WidgetIdx,
    rows_container: WidgetIdx,
    scroll_bar: WidgetIdx,

    finished_layout: [bool; 2], // horizontal, vertical

    // Lazy mode.
    lazy: bool,
    row_idxs: Range<usize>,
    total_rows: usize,
    fixed_row_height: usize,
}
impl Table {
    pub fn new(state: TableState, imgui: &mut IMGUI, columns: Vec<Column>) -> Self {
        let root = imgui.cur_parent;
        let w = imgui.get_mut(root);
        w.axes[1].flags.insert(AxisFlags::STACK);
        assert!(w.axes[0].flags.contains(AxisFlags::SIZE_KNOWN) && w.axes[1].flags.contains(AxisFlags::SIZE_KNOWN), "Table widget requires the size to be known in advance");
        let (width, height) = (w.axes[0].size, w.axes[1].size);
        let (viewport, rows_container, scroll_bar);

        with_parent!(imgui, root, {
            // Header.
            with_parent!(imgui, imgui.add(Widget::new().hstack().fixed_height(1)), {
                for i in 0..columns.len() {
                    with_parent!(imgui, imgui.add(Widget::new().width(AutoSize::Text).fixed_height(1)), {
                        styled_write!(imgui.text, imgui.palette.table_header, "{}", columns[i].title);
                        imgui.set_text();
                    });
                }
            });

            with_parent!(imgui, imgui.add(Widget::new().hstack().fixed_height(height.saturating_sub(1))), {
                // Clipping rectangle for the scrollable area.
                viewport = imgui.add(Widget::new().fixed_width(width.saturating_sub(1)));
                with_parent!(imgui, viewport, {
                    imgui.focus();
                    // Widget containing all rows, moves up and down when scrolling.
                    rows_container = imgui.add(Widget::new().height(AutoSize::Children).vstack().fixed_y(0));
                });
                scroll_bar = imgui.add(Widget::new().fixed_width(1));
            });
        });

        Self {state, columns, root, viewport, rows_container, scroll_bar, finished_layout: [false; 2], enable_selection_icon: true, hide_cursor_if_unfocused: false, enable_tooltip: true, scroll_to_cursor: false,
              lazy: false, row_idxs: 0..0, total_rows: 0, fixed_row_height: 0}
    }

    // Switches the table into "lazy mode": input is processed and vertical layout is determined right here, before rows are created, so that only visible rows have to be created.
    // Returns the range of rows that need to be created.
    pub fn lazy(&mut self, num_rows: usize, row_height: usize, imgui: &mut IMGUI) -> Range<usize> {
        assert!(row_height > 0);
        self.lazy = true;
        (self.total_rows, self.fixed_row_height) = (num_rows, row_height);
        with_parent!(imgui, self.rows_container, {
            imgui.cur_mut().axes[1].auto_size = AutoSize::Fixed(num_rows * row_height);
            if imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                let y = imgui.cur().mouse_pos[1];
                if y >= 0 && (y as usize) < num_rows * row_height {
                    self.state.cursor = y as usize / row_height;
                    self.scroll_to_cursor = true;
                }
            }
        });
        self.scroll_to_cursor |= with_parent!(imgui, self.viewport, {
            list_cursor_navigation(&mut self.state.cursor, num_rows, None, row_height, imgui)
        });
        let scroll_to = if self.scroll_to_cursor && num_rows > 0 {
            Some((self.state.cursor*row_height) as isize..((self.state.cursor+1)*row_height) as isize)
        } else {
            None
        };
        let y_range = with_parent!(imgui, self.viewport, {
            scrolling_navigation(&mut self.state.scroll, scroll_to, self.root, self.scroll_bar, imgui)
        });
        self.row_idxs = (y_range.start as usize)/row_height..(y_range.end as usize + row_height - 1)/row_height;
        self.row_idxs.clone()
    }

    pub fn start_row(&mut self, id: usize, imgui: &mut IMGUI) -> WidgetIdx {
        self.finish_row(imgui);
        with_parent!(imgui, self.rows_container, {
            let x = if self.enable_selection_icon {2} else {0}; // excluding the icon from selection highlighting looks slightly better (to me)
            let row_height = if self.lazy {AutoSize::Fixed(self.fixed_row_height)} else {AutoSize::Children};
            with_parent!(imgui, imgui.add(Widget::new().identity(&id).hstack().fixed_x(x).height(row_height).fill(' ', imgui.palette.default)), {
                if !self.lazy && imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                    self.state.cursor = imgui.get(self.rows_container).children.len() - 1;
                    self.scroll_to_cursor = true;
                }
                if imgui.check_mouse(MouseActions::HOVER_SUBTREE) {
                    let a = imgui.palette.hovered;
                    imgui.cur_mut().style_adjustment.update(a);
                }
                imgui.cur_parent
            })
        })
    }

    fn finish_row(&mut self, imgui: &mut IMGUI) {
        if let Some(&row) = imgui.get(self.rows_container).children.last() {
            assert_eq!(imgui.get(row).children.len(), self.columns.len(), "wrong number of cells in row");
        }
    }

    // By default the cell's height is set to AutoSize::Text. You may change it.
    pub fn start_cell(&mut self, imgui: &mut IMGUI) -> WidgetIdx {
        let w = *imgui.get(self.rows_container).children.last().unwrap();
        let i = imgui.get(w).children.len();
        assert!(i < self.columns.len());
        with_parent!(imgui, w, {
            imgui.add(Widget::new().width(self.columns[i].auto_width).height(AutoSize::Text))
        })
    }

    // Convenience function for simple single-line text cells.
    // Usage:
    //   styled_write!(imgui.text, ...);
    //   table.text_cell(imgui);
    pub fn text_cell(&mut self, imgui: &mut IMGUI) -> WidgetIdx {
        with_parent!(imgui, self.start_cell(imgui), {
            imgui.set_text();
            imgui.cur_parent
        })
    }

    pub fn finish_horizontal_layout(&mut self, imgui: &mut IMGUI) {
        assert!(!self.finished_layout[0]);
        self.finished_layout[0] = true;
        self.finish_row(imgui);
        let num_rows = imgui.get(self.rows_container).children.len();

        // Look at all cells to calculate Children and Text column widths.
        for row_idx in 0..num_rows {
            let row = imgui.get(self.rows_container).children[row_idx];
            for (col_idx, col) in self.columns.iter_mut().enumerate() {
                match col.auto_width {
                    AutoSize::Fixed(_) | AutoSize::Remainder(_) => continue,
                    AutoSize::Children | AutoSize::Text => (),
                    _ => panic!("unexpected auto_width for column {}", col.title),
                }
                let cell = imgui.get(row).children[col_idx];
                col.width = col.width.max(imgui.calculate_size(cell, 0));
            }
        }

        let reserved = if self.enable_selection_icon {2usize} else {0};

        // Calculate Remainder column widths.
        let mut remainder_helper = RemainderFractionHelper::new(imgui.get(self.rows_container).axes[0].size);
        remainder_helper.declare_fixed_part(self.columns.len().saturating_sub(1) + reserved);
        for (col_idx, col) in self.columns.iter_mut().enumerate() {
            match col.auto_width {
                AutoSize::Remainder(f) => remainder_helper.declare_remainder_fraction(f, col_idx),
                AutoSize::Children | AutoSize::Text => remainder_helper.declare_fixed_part(col.width),
                AutoSize::Fixed(x) => {
                    col.width = x;
                    remainder_helper.declare_fixed_part(col.width);
                }
                _ => panic!("unexpected auto_width for column {}", col.title),
            }
        }
        let mut pos = 0isize;
        for (col_idx, col) in self.columns.iter_mut().enumerate() {
            match col.auto_width {
                AutoSize::Remainder(f) => col.width = remainder_helper.calculate_remainder_fraction(f, col_idx),
                _ => (),
            }
            col.pos = pos;
            pos += 1 + col.width as isize;
        }

        // Update all cells with final column widths and positions. (Instead of assigning positions manually we could add spacer widgets, but that would be slightly slower and no less code.)
        // Also apply striping to rows.
        let header = imgui.get(self.root).children[0];
        for (col_idx, col) in self.columns.iter().enumerate() {
            let cell = imgui.get(header).children[col_idx];
            let w = imgui.get_mut(cell);
            w.axes[0].size = col.width;
            w.axes[0].rel_pos = reserved as isize + col.pos;
            w.axes[0].flags.insert(AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN);
        }
        for row_idx in 0..num_rows {
            let row = imgui.get(self.rows_container).children[row_idx];
            if row_idx % 2 == 0 {
                let adj = imgui.palette.striped_table;
                let w = imgui.get_mut(row);
                w.style_adjustment.update(adj);
            }
            for (col_idx, col) in self.columns.iter().enumerate() {
                let cell = imgui.get(row).children[col_idx];
                let w = imgui.get_mut(cell);
                w.axes[0].auto_size = AutoSize::Fixed(col.width);
                w.axes[0].size = col.width;
                w.axes[0].rel_pos = col.pos;
                w.axes[0].flags.insert(AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN);
            }
        }
    }

    pub fn finish_vertical_layout(&mut self, imgui: &mut IMGUI) {
        assert!(!self.finished_layout[1]);
        self.finished_layout[1] = true;
        with_parent!(imgui, self.rows_container, {
            let w = imgui.cur_mut();
            if !self.lazy {
                self.row_idxs = 0..w.children.len();
            } else {
                w.axes[1].auto_size = AutoSize::Fixed(self.total_rows * self.fixed_row_height);
                assert_eq!(w.children.len(), self.row_idxs.len(), "number of rows created doesn't match the range returned from lazy()");
                if !w.children.is_empty() {
                    let r = w.children[0];
                    let w = imgui.get_mut(r);
                    w.axes[1].flags.insert(AxisFlags::POS_KNOWN);
                    w.axes[1].rel_pos = (self.row_idxs.start * self.fixed_row_height) as isize;
                }
            }
            imgui.layout_children(1);
        });
    }

    pub fn finish(mut self, imgui: &mut IMGUI) -> TableState {
        if !self.finished_layout[0] {
            self.finish_horizontal_layout(imgui);
        }
        if !self.finished_layout[1] {
            self.finish_vertical_layout(imgui);
        }

        if !self.lazy {
            let num_rows = imgui.get(self.rows_container).children.len();
            self.scroll_to_cursor |= with_parent!(imgui, self.viewport, {
                list_cursor_navigation_with_variable_row_height(&mut self.state.cursor, num_rows, None, |i, imgui| imgui.get(imgui.get(self.rows_container).children[i]).axes[1].size, imgui)
            });
            let scroll_to = if self.scroll_to_cursor && num_rows > 0 {
                let ax = &imgui.get(imgui.get(self.rows_container).children[self.state.cursor]).axes[1];
                Some(ax.rel_pos..ax.rel_pos + ax.size as isize)
            } else {
                None
            };
            with_parent!(imgui, self.viewport, {
                scrolling_navigation(&mut self.state.scroll, scroll_to, self.root, self.scroll_bar, imgui);
            });
        }

        if self.row_idxs.contains(&self.state.cursor) && (!self.hide_cursor_if_unfocused || imgui.get(self.viewport).flags.contains(WidgetFlags::LIKELY_FOCUSED)) {
            with_parent!(imgui, imgui.get(self.rows_container).children[self.state.cursor - self.row_idxs.start], {
                imgui.focus();
                let a = imgui.palette.selected;
                let row = imgui.cur_mut();
                row.style_adjustment.update(a);
                if self.enable_selection_icon {
                    let y = row.axes[1].rel_pos;
                    with_parent!(imgui, self.rows_container, {
                        imgui.add(Widget::new().fixed_width(1).fixed_x(0).fixed_height(1).fixed_y(y).fill('➤', imgui.palette.default));
                    });
                }

                self.handle_tooltip(imgui);
            });
        }

        self.state
    }

    fn handle_tooltip(&mut self, imgui: &mut IMGUI) {
        if !self.enable_tooltip {
            return;
        }
        let tooltip = match imgui.check_tooltip() {
            Some(x) => x,
            None => return,
        };
        let row = imgui.cur_parent;
        with_parent!(imgui, tooltip, {
            imgui.cur_mut().axes[1].flags.insert(AxisFlags::STACK);
            let mut width = 0usize;
            for (col_idx, col) in self.columns.iter().enumerate() {
                width = width.max(str_width(col.title));
                styled_write!(imgui.text, imgui.palette.table_header, "{}", col.title);
                let l = imgui.text.close_line();
                imgui.add(Widget::new().height(AutoSize::Text).text(l).flags(WidgetFlags::LINE_WRAP));
                
                let cell = imgui.get(row).children[col_idx];
                let w = Self::questionable_autotooltip(imgui, cell);
                width = width.max(w);
                
                if col_idx + 1 < self.columns.len() {
                    imgui.add(Widget::new().fixed_height(1));
                }
            }
            let parent = imgui.cur().parent;
            width = width.min(imgui.get(parent).axes[0].size);
            imgui.cur_mut().axes[0].auto_size = AutoSize::Fixed(width);
        });
    }

    fn questionable_autotooltip(imgui: &mut IMGUI, cell: WidgetIdx) -> usize {
        let mut width = 0usize;
        let mut stack: Vec<(WidgetIdx, WidgetIdx)> = vec![(cell, imgui.cur_parent)];
        while let Some((from, to)) = stack.pop() {
            let mut w = imgui.get(from).make_questionable_copy();
            if let Some(lines) = w.draw_text.clone() {
                for line in lines {
                    width = width.max(str_width(imgui.text.get_line_str(line)));
                }
            }
            w.flags.insert(WidgetFlags::LINE_WRAP);
            w.parent = to;
            if from == cell {
                w.axes[0].auto_size = AutoSize::Parent;
            }
            let new_to = imgui.add(w);
            for c in imgui.get(from).children.iter().rev() {
                stack.push((*c, new_to));
            }
        }
        width
    }
}


pub struct TabsState {
    pub selected: usize,
    pub hscroll: isize,
}

pub struct Tabs {
    pub scroll_to_selected_tab: bool,
    tabs: Vec<(/*tab*/ WidgetIdx, /*title*/ WidgetIdx, /*short_title_line*/ usize, /*full_title_line*/ usize)>,
    state: TabsState,
    viewport: WidgetIdx,
    content: WidgetIdx,
}
impl Tabs {
    pub fn new(state: TabsState, imgui: &mut IMGUI) -> Self {
        let content = imgui.add(Widget::new().width(AutoSize::Children).fixed_height(1).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS).hstack());
        Self {scroll_to_selected_tab: false, tabs: Vec::new(), state, viewport: imgui.cur_parent, content}
    }

    pub fn add(&mut self, full_title: &str, short_title: &str, pinned: bool, imgui: &mut IMGUI) {
        with_parent!(imgui, self.content, {
            if !self.tabs.is_empty() {
                styled_write!(imgui.text, imgui.palette.tab_separator.1, "{}", imgui.palette.tab_separator.0);
                let l = imgui.text.close_line();
                imgui.add(Widget::new().width(AutoSize::Text).text(l));
            }
            
            let tab = imgui.add(Widget::new().identity(full_title).width(AutoSize::Children));
            with_parent!(imgui, tab, {
                if imgui.check_mouse(MouseActions::CLICK) {
                    self.state.selected = self.tabs.len();
                    self.scroll_to_selected_tab = true;
                }

                let style = if pinned {
                    styled_write!(imgui.text, imgui.palette.tab_title_pinned, "📌 ");
                    let l = imgui.text.close_line();
                    imgui.add(Widget::new().width(AutoSize::Text).text(l));
                    imgui.palette.tab_title_pinned
                } else {
                    imgui.palette.tab_title
                };

                styled_write!(imgui.text, style, "{}", short_title);
                let short_title_line = imgui.text.close_line();
                styled_write!(imgui.text, style, "{}", full_title);
                let full_title_line = imgui.text.close_line();

                let title = imgui.add(Widget::new().width(AutoSize::Text).text(short_title_line));

                self.tabs.push((tab, title, short_title_line, full_title_line));
            });
        });
    }

    pub fn finish(mut self, keys_widget: WidgetIdx, imgui: &mut IMGUI) -> TabsState {
        self.disambiguate_titles(imgui);
        with_parent!(imgui, keys_widget, {
            let keys = imgui.check_keys(&[KeyAction::PreviousTab, KeyAction::NextTab]);
            self.scroll_to_selected_tab |= !keys.is_empty();
            for key in keys {
                match key {
                    KeyAction::PreviousTab => self.state.selected = self.state.selected.saturating_sub(1),
                    KeyAction::NextTab => self.state.selected += 1,
                    _ => panic!("huh"),
                }
            }
        });
        if !self.tabs.is_empty() && self.state.selected >= self.tabs.len() {
            self.state.selected = self.tabs.len() - 1;
            self.scroll_to_selected_tab = true;
        }
        self.state.hscroll += with_parent!(imgui, self.viewport, {
            imgui.check_scroll() * imgui.key_binds.hscroll_sensitivity
        });
        let viewport_width = imgui.calculate_size(self.viewport, 0);
        let content_width = imgui.calculate_size(self.content, 0);
        with_parent!(imgui, self.content, {
            imgui.layout_children(0);
            if self.scroll_to_selected_tab && !self.tabs.is_empty() {
                let w = imgui.get(self.tabs[self.state.selected].0);
                scroll_to_range(&mut self.state.hscroll, w.axes[0].rel_pos..w.axes[0].rel_pos + w.axes[0].size as isize, viewport_width);
            }
        });
        self.state.hscroll = self.state.hscroll.min(content_width as isize - viewport_width as isize).max(0);
        let w = imgui.get_mut(self.content);
        w.axes[0].rel_pos = -self.state.hscroll;
        w.axes[0].flags.insert(AxisFlags::POS_KNOWN);
        self.state
    }

    fn disambiguate_titles(&mut self, imgui: &mut IMGUI) {
        //asdqwe
    }
}


// ============================================== mostly toy code, some ui.rs =================================================================


#[derive(Default)]
struct State {
    profiling_message: Vec<String>,
    selected_window: usize,

    binaries_table_state: TableState,
    threads_table_state: TableState,
}

fn build(imgui: &mut IMGUI, state: &mut State) -> bool {
    let w = imgui.cur_mut();
    w.axes[0].flags.insert(AxisFlags::STACK);

    let mut windows: Vec<WidgetIdx> = Vec::new();

    let columns = [
        imgui.add(Widget::new().width(AutoSize::Remainder(0.3)).vstack()),
        imgui.add(Widget::new().fill('|', imgui.palette.default_dim).fixed_width(1)),
        imgui.add(Widget::new().width(AutoSize::Remainder(0.5)).vstack()),
        imgui.add(Widget::new().fill('|', imgui.palette.default_dim).fixed_width(1)),
        imgui.add(Widget::new().width(AutoSize::Remainder(0.2)).vstack()),
    ];
    imgui.layout_children(0);

    // Left column.
    with_parent!(imgui, columns[0], {
        styled_write!(imgui.text, imgui.palette.default_dim, "cheat sheet");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Children)));

        styled_write!(imgui.text, imgui.palette.default_dim, "status");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().fixed_height(6).vstack()));

        styled_write!(imgui.text, imgui.palette.default_dim, "watches ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "1");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(1.0))));
    });
    // Middle column.
    with_parent!(imgui, columns[2], {
        styled_write!(imgui.text, imgui.palette.default_dim, "disassembly ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "2");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.6))));

        styled_write!(imgui.text, imgui.palette.default_dim, "code ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "3");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.4)).vstack()));
        imgui.layout_children(1);
    });
    // Right column.
    with_parent!(imgui, columns[4], {
        styled_write!(imgui.text, imgui.palette.default_dim, "binaries ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "4");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.2))));

        styled_write!(imgui.text, imgui.palette.default_dim, "breakpoints ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "7");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.3))));

        styled_write!(imgui.text, imgui.palette.default_dim, "stack ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "5");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.3))));

        styled_write!(imgui.text, imgui.palette.default_dim, "threads ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "6");
        let l = imgui.text.close_line();
        imgui.add(Widget::new().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(Widget::new().height(AutoSize::Remainder(0.2))));
        imgui.layout_children(1);
    });

    let hotkey_to_window = [2, 3, 4, 5, 7, 8, 6];
    for action in imgui.check_keys(&[KeyAction::Quit, KeyAction::Window(1), KeyAction::Window(2), KeyAction::Window(3), KeyAction::Window(4), KeyAction::Window(5), KeyAction::Window(6), KeyAction::Window(7)]) {
        match action {
            KeyAction::Quit => return true,
            KeyAction::Window(i) => state.selected_window = hotkey_to_window[i - 1],
            _ => (),
        }
    }
    for (i, &w) in windows.iter().enumerate() {
        with_parent!(imgui, w, {
            if imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                state.selected_window = i;
            }
        });
    }
    with_parent!(imgui, windows[state.selected_window], {
        imgui.focus();
    });

    // Hints window.
    with_parent!(imgui, windows[0], {
        let start = imgui.text.num_lines();
        for s in &state.profiling_message {
            styled_write!(imgui.text, Style {fg: Color(100, 200, 100), ..D!()}, "{}", s);
            imgui.text.close_line();
        }
        let end = imgui.text.num_lines();
        let w = imgui.cur_mut();
        w.draw_text = Some(start..end);
        w.axes[1].auto_size = AutoSize::Text;
    });
    // Status window.
    with_parent!(imgui, windows[1], {
        with_parent!(imgui, imgui.add(Widget::new().height(AutoSize::Text)), {
            let style = Style {fg: Color(0, 0, 0), bg: Color(100, 100, 200), ..D!()};
            imgui.cur_mut().draw_fill = Some((' ', style));
            styled_write!(imgui.text, style, "status goes here");
            imgui.set_text();
        });

        with_parent!(imgui, imgui.add(Widget::new()), {
            styled_write!(imgui.text, imgui.palette.default, "hello world");
            imgui.set_text();
        });
    });

    with_parent!(imgui, columns[0], {
        imgui.layout_children(1);
    });

    with_parent!(imgui, windows[2], {
        build_watches(imgui);
    });
    with_parent!(imgui, windows[3], {
        build_disassembly(imgui);
    });
    with_parent!(imgui, windows[4], {
        // (Code window is similar enough to disassembly, so we print other things here instead.)
        let start = imgui.text.num_lines();
        for e in imgui.log.iter().rev() {
            styled_write!(imgui.text, Style {fg: Color(150, 150, 250), ..D!()}, "{}", e);
            imgui.text.close_line();
        }
        let end = imgui.text.num_lines();
        let w = imgui.cur_mut();
        w.draw_text = Some(start..end);
    });
    with_parent!(imgui, windows[5], {
        build_binaries(imgui, state);
    });
    with_parent!(imgui, windows[6], {
        build_breakpoints(imgui);
    });
    with_parent!(imgui, windows[7], {
        build_stack(imgui);
    });
    with_parent!(imgui, windows[8], {
        build_threads(imgui, state);
    });

    false
}

fn build_watches(imgui: &mut IMGUI) {
    
}

fn build_disassembly(imgui: &mut IMGUI) {

}

fn build_binaries(imgui: &mut IMGUI, state: &mut State) {
    let mut table = Table::new(mem::take(&mut state.binaries_table_state), imgui, vec![
        Column::new("idx", AutoSize::Fixed(3)),
        Column::new("path", AutoSize::Remainder(1.0)),
        Column::new("file", AutoSize::Fixed(PrettySize::MAX_LEN)),
    ]);
    table.hide_cursor_if_unfocused = true;

    let paths = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum", "[vdso]", "/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2/test/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2"];
    let sizes = ["420 MiB", "12.3 MiB", "0 B", "1023 GiB", "1.00 KiB"];
    let errors = [("", Style::default()), ("debuglink file not found: /usr/lib/debug/.build-id/2a/263ab9d91c6906db746c05b6aa33619cf5ed29.debug", imgui.palette.default_dim), ("oh noes", imgui.palette.error), ("in progress", Style::default())];
    let progress_stages = ["1/4: frobbing the glorbs", "2/4: globbing the florbs", "3/4: picking the nose", "4/4: coiling the hose"];
    for i in 0..60 {
        table.start_row(i, imgui);
        styled_write!(imgui.text, imgui.palette.default_dim, "{}", i + 1);
        table.text_cell(imgui);
        with_parent!(imgui, table.start_cell(imgui), {
            let w = imgui.cur_mut();
            w.axes[1].flags.insert(AxisFlags::STACK);
            w.axes[1].auto_size = AutoSize::Children;
            with_parent!(imgui, imgui.add(Widget::new().height(AutoSize::Text).flags(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT)), {
                styled_write!(imgui.text, imgui.palette.default, "{}", paths[i%paths.len()]);
                imgui.set_text();
            });
            let (err, style) = errors[i%errors.len()];
            if err == "in progress" {
                let progress = (SystemTime::UNIX_EPOCH.elapsed().unwrap().as_secs_f64() / 20.0 * (2.0 + (i as f64).sin())) % 1.0;
                let stage = progress_stages[(progress * progress_stages.len() as f64) as usize % progress_stages.len()];
                with_parent!(imgui, imgui.add(Widget {draw_progress_bar: Some((progress, imgui.palette.progress_bar)), .. D!()}.height(AutoSize::Text)), {
                    styled_write!(imgui.text, imgui.palette.default, "{}", stage);
                    imgui.set_text();
                });
            } else if err != "" {
                with_parent!(imgui, imgui.add(Widget::new().height(AutoSize::Text)), {
                    styled_write!(imgui.text, style, "{}", err);
                    imgui.set_text();
                });
            }
        });
        styled_write!(imgui.text, imgui.palette.default_dim, "{}", sizes[i%sizes.len()]);
        table.text_cell(imgui);
    }

    state.binaries_table_state = table.finish(imgui);
}

fn build_breakpoints(imgui: &mut IMGUI) {
    
}

fn build_stack(imgui: &mut IMGUI) {
    
}

fn build_threads(imgui: &mut IMGUI, state: &mut State) {
    let mut table = Table::new(mem::take(&mut state.threads_table_state), imgui, vec![
        Column::new("idx", AutoSize::Fixed(5)),
        Column::new("tid", AutoSize::Fixed(10)),
        Column::new("name", AutoSize::Fixed(15)),
        Column::new("s", AutoSize::Fixed(1)),
        Column::new("cpu", AutoSize::Fixed(4)),
        Column::new("addr", AutoSize::Fixed(12)),
        Column::new("bin", AutoSize::Fixed(3)),
        Column::new("function", AutoSize::Remainder(1.0)),
    ]);
    let names = ["foo", "t12345678901234", "", "worker", "whatevs"];
    let funcs = ["", "?", "Lorem Ipsum छपाई और अक्षर योजन उद्योग का एक साधारण डमी पाठ है. Lorem Ipsum सन १५०० के बाद से अभी तक इस उद्योग का मानक डमी पाठ मन गया, जब एक अज्ञात मुद्रक ने नमूना लेकर एक नमूना किताब बनाई. यह न केवल पाँच सदियों से जीवित रहा बल्कि इसने इलेक्ट्रॉनिक मीडिया में छलांग लगाने के बाद भी मूलतः अपरिवर्तित रहा. यह 1960 के दशक में Letraset Lorem Ipsum अंश युक्त पत्र के रिलीज के साथ लोकप्रिय हुआ, और हाल ही में Aldus PageMaker Lorem Ipsum के संस्करणों सहित तरह डेस्कटॉप प्रकाशन सॉफ्टवेयर के साथ अधिक प्रचलित हुआ."];
    let states = [('r', imgui.palette.running.flip()), ('t', imgui.palette.suspended.flip())];
    let cpus = ["0%", "100%", "50%"];
    let addrs = ["0123456789ab", "ba9876543210"];
    let bins = ["1", "2", "666"];
    let count = 1000;
    for i in table.lazy(count, 1, imgui) {
        table.start_row(i, imgui);
        styled_write!(imgui.text, imgui.palette.default_dim, "{}", i + 1);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default_dim, "{}", i * 11 + 12345678);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default, "{}", names[i%names.len()]);
        table.text_cell(imgui);
        let (state, style) = states[i%states.len()].clone();
        styled_write!(imgui.text, style, "{}", state);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default, "{}", cpus[i%cpus.len()]);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default, "{}", addrs[i%addrs.len()]);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default, "{}", bins[i%bins.len()]);
        table.text_cell(imgui);
        styled_write!(imgui.text, imgui.palette.default, "{}", funcs[i%funcs.len()]);
        table.text_cell(imgui);
    }
    state.threads_table_state = table.finish(imgui);
}

fn main() -> Result<()> {
    {
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            restore_terminal();
            default_hook(info);
            process::exit(2);
        }));
    }

    let _restorer = TerminalRestorer;
    configure_terminal(MouseMode::Full)?;

    let mut terminal = Terminal::new();
    let mut input = InputReader::new();
    let mut state = State::default();

    let mut prev_time = Instant::now();
    let mut frames = 0usize;
    let mut command_bytes = 0usize;
    let mut input_bytes = 0usize;
    let mut input_secs = 0f64;
    let mut clear_secs = 0f64;
    let mut build_secs = 0f64;
    let mut layout_secs = 0f64;
    let mut render_secs = 0f64;
    let mut diff_secs = 0f64;
    let mut write_secs = 0f64;
    
    let mut imgui = IMGUI::default();
    imgui.mouse_mode = MouseMode::Full;
    imgui.palette = Palette {
        default: Style {fg: Color::white(), ..D!()},
        default_dim: Style {fg: Color::white().dim(), ..D!()},
        error: Style {fg: Color(255, 50, 50), ..D!()},
        running: Style {fg: Color(70, 70, 180), ..D!()},
        suspended: Style {fg: Color(70, 180, 70), ..D!()},

        selected: StyleAdjustment {add_fg: (20, 20, 20), add_bg: (50, 50, 50), ..D!()},
        hovered: StyleAdjustment {add_fg: (20, 20, 20), add_bg: (25, 25, 25), ..D!()},
        tooltip: StyleAdjustment {add_bg: (30, 40, 50), ..D!()},

        table_header: Style {fg: Color::white().dim(), ..D!()},
        //striped_table: StyleAdjustment {add_fg: (0, 0, 0), add_bg: (20, 20, 20), ..D!()},
        striped_table: StyleAdjustment::default(),

        tab_title: Style {fg: Color::white().dim(), ..D!()},
        tab_title_pinned: Style {fg: Color::white(), ..D!()},
        tab_separator: (" | ".to_string(), Style {fg: Color::white().dim(), ..D!()}),

        placeholder_fill: Some(('.', Style {fg: Color::white().dim(), bg: Color::black(), ..D!()})),
        truncation_indicator: (("…".to_string(), "…".to_string(), Style {fg: Color::white().dim(), ..D!()})),
        hscroll_indicator: (("❮".to_string(), "❯".to_string(), Style {fg: Color::white().dim(), ..D!()})),
        line_wrap_indicator: (String::new(), "\\".to_string(), Style {fg: Color::white().dim(), ..D!()}),

        progress_bar: Style {fg: Color(70, 90, 200), bg: Color(20, 30, 50), ..D!()},
        scroll_bar_background: Style {fg: Color::white().dim(), ..D!()},
        scroll_bar_slider: Style {fg: Color::white().dim(), ..D!()},
    };

    loop {
        frames += 1;
        if frames == 100 {
            let now = Instant::now();
            let t = (now - prev_time).as_secs_f64();
            state.profiling_message = vec![
                format!("fps: {:.0}", frames as f64 / t),
                format!("input: {:.3}us", input_secs / frames as f64 * 1e6),
                format!("clear: {:.3}us", clear_secs / frames as f64 * 1e6),
                format!("build: {:.3}us", build_secs / frames as f64 * 1e6),
                format!("layout: {:.3}us", layout_secs / frames as f64 * 1e6),
                format!("render: {:.3}us", render_secs / frames as f64 * 1e6),
                format!("diff: {:.3}us", diff_secs / frames as f64 * 1e6),
                format!("write: {:.3}us", write_secs / frames as f64 * 1e6),
                format!("other: {:.3}us", (t - build_secs - clear_secs - layout_secs - render_secs - diff_secs - write_secs) / frames as f64 * 1e6),
                format!("input bytes/s: {:.0}", input_bytes as f64 / t),
                format!("command bytes/s: {:.0}", command_bytes as f64 / t),
                format!("command bytes/frame: {:.0}", command_bytes as f64 / frames as f64),
            ];
            prev_time = now;
            (frames, command_bytes, input_bytes, input_secs, clear_secs, build_secs, layout_secs, render_secs, diff_secs, write_secs) = (0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        }

        let mut t = Instant::now();

        {
            let mut evs: Vec<Event> = Vec::new();
            input_bytes += input.read(&mut evs)?;
            let _should_redraw = imgui.buffer_input(&evs);
            for e in evs {
                imgui.log.push(format!("{:?}", e));
            }
            if imgui.log.len() > 200 {
                imgui.log.drain(0..imgui.log.len()-100);
            }
        }

        let now = Instant::now();
        input_secs += (now - mem::replace(&mut t, now)).as_secs_f64();

        let mut buffer = terminal.start_frame(imgui.palette.default)?;

        let now = Instant::now();
        clear_secs += (now - mem::replace(&mut t, now)).as_secs_f64();

        imgui.start_build(buffer.width, buffer.height);
        let quit = build(&mut imgui, &mut state);
        if quit {
            break;
        }

        let now = Instant::now();
        build_secs += (now - mem::replace(&mut t, now)).as_secs_f64();

        let render = imgui.end_build(&mut buffer).as_secs_f64();

        render_secs += render;
        let now = Instant::now();
        layout_secs += (now - mem::replace(&mut t, now)).as_secs_f64() - render;

        let commands = terminal.prepare_command_buffer(&buffer);

        command_bytes += commands.len();
        let now = Instant::now();
        diff_secs += (now - mem::replace(&mut t, now)).as_secs_f64();

        terminal.present(buffer, commands)?;

        let now = Instant::now();
        write_secs += (now - mem::replace(&mut t, now)).as_secs_f64();
    }

    Ok(())
}

// Plan:
//  * search dialog
//  * tabs
//  * watches (at least decide how it'll work with line wrapping and windowing)
//  * text edit
