#![allow(unused_imports)]
extern crate nnd;
use nnd::{*, error::*, util::*};
use std::{time::{Duration, Instant, SystemTime}, thread, mem, cell::UnsafeCell, sync::atomic::{AtomicBool, Ordering}, io, io::{Write, Read}, str, os::fd::AsRawFd, ops::Range, collections::{HashMap, HashSet}, panic, process, fmt::Write as fmtWrite, result, hash::Hash};
use bitflags::*;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use rand::{self, random, Rng};

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
    pub fn darker(self) -> Self {
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

// Text. Consists of lines. Each line consists of spans. Each span has one Style.
// We usually use this struct as a sort of arena that contains many unrelated pieces of text, each piece identified by range of line numbers (Range<usize>).
// There are num_lines() "closed" lines, and an "unclosed" line at the end (which may be empty).
// The unclosed line is used only as a staging area for building a new line; by convention, it should be closed before the line is used, and before anyone else tries to append other lines to the StyledText.
// Use styled_write!(text, style, "...", ...) to add spans to the unclosed line. Or append to `chars` and call close_span().
// Use close_line() to close the unclosed line.
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

    pub fn get_line_char_range(&self, i: usize) -> Range<usize> {
        self.spans[self.lines[i]].0..self.spans[self.lines[i+1]].0
    }

    pub fn widest_line(&self, lines: Range<usize>) -> usize {
        lines.map(|i| str_width(self.get_line_str(i))).max().unwrap_or(0)
    }

    pub fn clear(&mut self) {
        self.chars.clear();
        self.spans.truncate(1);
        self.lines.truncate(1);
    }

    pub fn unclosed_line_width(&self) -> usize {
        str_width(&self.chars[self.spans[*self.lines.last().unwrap()].0..])
    }

    pub fn import_lines(&mut self, from: &StyledText, lines: Range<usize>) -> Range<usize> {
        let start = self.lines.len();
        let offset = self.num_spans().wrapping_sub(from.lines[lines.start]);
        self.lines.extend_from_slice(&from.lines[lines.start+1..lines.end+1]);
        for x in &mut self.lines[start..] {
            *x = x.wrapping_add(offset);
        }

        let spans = from.lines[lines.start]..from.lines[lines.end];
        let start = self.spans.len();
        let offset = self.chars.len().wrapping_sub(from.spans[spans.start].0);
        self.spans.extend_from_slice(&from.spans[spans.start+1..spans.end+1]);
        for (x, _) in &mut self.spans[start..] {
            *x = x.wrapping_add(offset);
        }

        let chars = from.spans[spans.start].0..from.spans[spans.end].0;
        self.chars.push_str(&from.chars[chars]);

        self.num_lines()-lines.len() .. self.num_lines()
    }

    pub fn line_wrap(&mut self, lines: Range<usize>, width: usize, max_lines: usize, line_wrap_indicator: &(String, String, Style), truncation_indicator: &(String, String, Style), mut out_lines: Option<&mut Vec<(/*start_x*/ isize, /*line_idx*/ usize, /*bytes*/ Range<usize>)>>) -> Range<usize> {
        let start = self.num_lines();
        for line_idx in lines.clone() {
            let spans_end = self.lines[line_idx + 1];
            let mut span_idx = self.lines[line_idx];
            let line_end = self.spans[spans_end].0;
            let line_start = self.spans[span_idx].0;
            let mut pos = line_start;
            let mut remaining_width = str_width(&self.chars[pos..line_end]);
            if pos == line_end {
                if let Some(v) = &mut out_lines {
                    v.push((0, line_idx - lines.start, 0..0));
                }
                if self.num_lines() - start + 1 >= max_lines && line_idx + 1 < lines.end {
                    styled_write!(self, truncation_indicator.2, "{}", truncation_indicator.1);
                }
                self.close_line();
                continue;
            }
            while pos < line_end {
                let out_line_start_pos = pos;
                let out_line_start_x = if pos == line_start {
                    0
                } else {
                    styled_write!(self, line_wrap_indicator.2, "{}", line_wrap_indicator.0);
                    str_width(&line_wrap_indicator.0)
                };
                let width = width.saturating_sub(out_line_start_x);
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

                if let Some(v) = &mut out_lines {
                    v.push((out_line_start_x as isize, line_idx - lines.start, out_line_start_pos - line_start .. pos - line_start));
                }

                if pos < line_end || (last_line && line_idx + 1 < lines.end) {
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

    fn split_by_newline_character(&mut self, line: usize, mut out_lines: Option<&mut Vec</*bytes*/ Range<usize>>>) -> /*lines*/ Range<usize> {
        let res_lines_start = self.num_lines();
        let src_chars_start = self.spans[self.lines[line]].0;

        let mut line_start_byte = 0;
        let mut do_close_line = |text: &mut StyledText, i: usize| {
            text.close_line();
            let i = i - src_chars_start;
            if let Some(v) = &mut out_lines {
                v.push(line_start_byte..i);
            }
            line_start_byte = i;
        };

        for span_idx in self.lines[line]..self.lines[line+1] {
            let mut i = self.spans[span_idx].0;
            let (span_end, style) = self.spans[span_idx+1].clone();
            while let Some(n) = self.chars[i..span_end].find('\n') {
                unsafe {self.chars.as_mut_vec().extend_from_within(i..i+n)};
                self.chars.push(' '); // empty cell at the end of line representing the '\n', useful in TextInput for indicating whether the \n is selected
                self.close_span(style);

                i += n + 1;
                do_close_line(self, i);
            }
            unsafe {self.chars.as_mut_vec().extend_from_within(i..span_end)};
            self.close_span(style);
        }
        do_close_line(self, self.spans[self.lines[line+1]].0);

        res_lines_start..self.num_lines()
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

    // Encodes a string containing the "extended grapheme cluster" for this cell, and its width. Stored inline if short, in long_graphemes if long.
    // If the symbol is wider than one cell (e.g. some icons), it lives in the leftmost of the cells, and the other cells contain ' '.
    // The high 2 bits of len_and_width are width-1 of the symbol.
    // If a wide symbol is partially clipped or covered (e.g. partially overwritten in ScreenBuffer's array), it's impossible to render correctly,
    // and we don't do any special handling for this case - the symbol just lives in the first cell, and the subsequent cells are spaces.
    len_and_width: u8,
    data: [u8; 4],
}
impl Default for ScreenCell { fn default() -> Self { Self {style: Style::default(), len_and_width: 1, data: [b' ', 0, 0, 0]} } }

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

    fn pack_grapheme(&mut self, mut s: &str, wid: usize, style: Style) -> ScreenCell {
        if s.len() > 63 {
            let mut i = 63;
            while !s.is_char_boundary(i) {
                i -= 1;
            }
            s = &s[..i];
        }
        let len_and_width = if wid <= 1 {
            s.len() as u8
        } else {
            s.len() as u8 | ((wid.min(4).max(1) as u8 - 1) << 6)
        };

        let mut data = [0u8; 4];
        if s.len() <= 4 {
            data[..s.len()].copy_from_slice(s.as_bytes());
        } else {
            assert!(self.long_graphemes.len() + s.len() < u32::MAX as usize);
            data = (self.long_graphemes.len() as u32).to_le_bytes();
            self.long_graphemes.extend_from_slice(s.as_bytes());
        }

        ScreenCell {style, len_and_width, data}
    }

    fn unpack_grapheme<'a>(&'a self, cell: &'a ScreenCell) -> (&'a str, /*extra_width*/ u8) {
        let len = cell.len_and_width & 63;
        let slice = if len <= 4 {
            &cell.data
        } else {
            &self.long_graphemes[u32::from_le_bytes(cell.data.clone()) as usize..]
        };
        (unsafe {str::from_utf8_unchecked(&slice[..len as usize])}, cell.len_and_width >> 6)
    }

    pub fn fill(&mut self, rect: Rect, grapheme: &str, style: Style) {
        assert!(str_width(grapheme) == 1);
        let cell = self.pack_grapheme(grapheme, 1, style);
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
                let cell = self.pack_grapheme(s, w, style);
                self.cells[row_start + start as usize] = cell;
            }
            if w > 1 {
                let cell = self.pack_grapheme(" ", 1, style);
                self.cells[row_start + (start+1).max(clip.x) as usize .. row_start + x.min(clip.right()) as usize].fill(cell);
            }
        }
        x
    }

    pub fn get_cell(&self, x: usize, y: usize) -> (&str, /*extra_width*/ u8, Style) {
        let cell = &self.cells[y*self.width + x];
        let (s, extra_width) = self.unpack_grapheme(cell);
        (s, extra_width, cell.style)
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

    pub fn prepare_command_buffer(&self, buffer: &ScreenBuffer, show_cursor: Option<[isize; 2]>) -> Vec<u8> {
        let mut commands: Vec<u8> = Vec::new();
        let (mut cur_x, mut cur_y, mut cur_style) = (usize::MAX, usize::MAX, Style::default());
        write!(commands, "{}{}", CURSOR_HIDE, STYLE_RESET_TO_BLACK).unwrap();

        let mut write_range = |y: usize, x1: usize, x2: usize, buffer: &ScreenBuffer, commands: &mut Vec<u8>| -> /*x*/ usize {
            if cur_y == y {
                write!(commands, "\x1B[{}G", x1 + 1).unwrap();
            } else {
                write!(commands, "\x1B[{};{}H", y + 1, x1 + 1).unwrap();
            }
            let mut x = x1;
            while x < x2 {
                let (s, extra_width, style) = buffer.get_cell(x, y);

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
                x += 1 + extra_width as usize;
            }
            (cur_x, cur_y) = (x, y);
            x
        };

        if self.prev_buffer.rect() != buffer.rect() {
            write!(commands, "{}", CLEAR_SCREEN).unwrap();
            for y in 0..buffer.height {
                write_range(y, 0, buffer.width, &buffer, &mut commands);
            }
        } else {
            for y in 0..buffer.height {
                let (mut x1, mut x) = (0, 0);
                let width = buffer.width;
                while x < width {
                    let idx = y*buffer.width + x;
                    if buffer.cells[idx] == self.prev_buffer.cells[idx] {
                        if x > x1 {
                            let xx = write_range(y, x1, x, &buffer, &mut commands);
                            if xx > x {
                                x = xx;
                                x1 = xx;
                                continue;
                            }
                        }
                        x1 = x + 1;
                    }
                    x += 1;
                }
                if buffer.width > x1 {
                    write_range(y, x1, buffer.width, &buffer, &mut commands);
                }
            }
        }

        write!(commands, "{}", STYLE_RESET).unwrap();

        if let Some([x, y]) = show_cursor {
            if x >= 0 && y >= 0 && x <= buffer.width as isize && y <= buffer.height as isize {
                write!(commands, "\x1B[{};{}H{}", y + 1, x + 1, CURSOR_SHOW).unwrap();
            }
        }

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
    // SHIFT doesn't always appear as modifier: for Key::Char, it's built into the `char`, e.g. 'E' is shift+e.
    const SHIFT = 0x1;
    const ALT = 0x2;
    const CTRL = 0x4;

    // Special value that can only be used in capture_keys. Means accept any combination of modifier keys.
    const ANY = 0x8;
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
    pub fn any_mods(self) -> KeyEx { KeyEx {key: self, mods: ModKeys::ANY} }
    pub fn is_ordinary_char(self) -> bool {
        match self {
            Self::Char('\t') | Self::Char('\n') | Self::Char('\r') => false,
            Self::Char(_) => true,
            _ => false,
        }
    }
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
            b'\x08' => Key::Backspace.ctrl(), // ctrl+backspace is indistinguishable from ctrl+h
            b'\x01'..=b'\x1A' => Key::Char((c - 0x1 + b'a') as char).ctrl(), // ctrl+[a..z]; shift is ignored in this case
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
    // Put this widget at the center of the parent. Not compatible with STACK on the parent.
    const CENTER = 0x2;

    // Position (rel_pos) won't be calculated by autolayout (it was either provided by the builder or already calculated by autolayout).
    // Otherwise rel_pos contains value from previous frame, or 0 if this widget didn't exist.
    const POS_KNOWN = 0x4;
    // Size won't be calculated by autolayout. Otherwise `size` contains value from previous frame or 0.
    const SIZE_KNOWN = 0x8;
}}

#[derive(Clone)]
pub struct Axis {
    pub flags: AxisFlags,
    pub auto_size: AutoSize,
    pub min_size: usize,
    pub max_size: usize,

    pub size: usize,
    pub rel_pos: isize, // relative to parent widget

    // Position relative to the screen. Calculated only at the end of the frame, when rendering. Any value assigned before that is ignored.
    abs_pos: isize,
}
impl Default for Axis { fn default() -> Self { Self {flags: AxisFlags::empty(), auto_size: AutoSize::Parent, min_size: 0, max_size: usize::MAX, size: 0, rel_pos: 0, abs_pos: 0} } }
impl Axis {
    // Index in `axes` list.
    pub const X: usize = 0;
    pub const Y: usize = 1;

    pub fn reset_size(&mut self) {
        self.flags.remove(AxisFlags::SIZE_KNOWN);
    }
}

bitflags! {
#[derive(Default)]
pub struct WidgetFlags : u32 {
    // By default, if text doesn't fit horizontally or vertically in its Widget's width, a "…" is shown at the end. This flag disables this behavior.
    const TEXT_TRUNCATION_INDICATOR_DISABLE = 0x1;
    // If the text doesn't fit horizontally, keep a suffix rather than a prefix, and put the "…" at the start. If the text is not truncated, it's aligned left.
    const TEXT_TRUNCATION_ALIGN_RIGHT = 0x2;
    // Disables text truncation indicator and enables an oddly specific behavior instead:
    // assume that the "contents" of this Widget cover the X range [0, right x coordinate of the last child, or width of draw_text if there are no children];
    // if part of this range is cut off by ancestors (e.g. hscroll area viewport), render a '<'/'>'-like symbols there, meant to show that horizontal scrolling is available.
    // This Widget itself is usually wider than its "contents". (Why not put a flag on the child instead, to avoid the awkward logic for content width?
    // Because that wouldn't show a left arrow when scrolled past the end of that child.)
    const HSCROLL_INDICATOR_ARROWS = 0x4;
    const LINE_WRAP = 0x8;

    // StyleAdjstment won't be inherited from ancestors. E.g. for drawing text input box with standard background in a panel with tinted background.
    const RESET_STYLE_ADJUSTMENT = 0x10;

    // If any part of this rect is visible (i.e. not clipped by parents' rects), trigger a redraw. May be useful for speculative hiding of elements,
    // but currently unused because I made everything non-speculative instead.
    const REDRAW_IF_VISIBLE = 0x20;

    // Any simple character key presses (no modifier keys, no special keys like tab or enter) are captured by this Widget as if they were in capture_keys.
    // Text navigation keys like arrow keys, home/end, etc, are not included, they should be requested through capture_keys as usual.
    const CAPTURE_TEXT_INPUT_KEYS = 0x40;

    // Trigger redraw is this Widget gets focused or unfocused.
    const REDRAW_IF_FOCUS_CHANGES = 0x80;

    // Equivalent to:
    //   if imgui.check_mouse(MouseActions::HOVER_SUBTREE) {
    //       widget.style_adjustment.update(imgui.palette.hovered);
    //   }
    const HIGHLIGHT_ON_HOVER = 0x100;

    // This widget's identity won't be hashed into children identities. Instead, the nearest ancestor without this flag will be hashed. As if this widget didn't exist and its children were its parent's children.
    const SKIP_IDENTITY = 0x200;
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
    // Line of the source code that created this widget. Useful for debugging.
    pub source_line: u32,
    
    pub axes: [Axis; 2], // X, Y
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

    pub draw_cursor_if_focused: Option<[isize; 2]>,

    // Put this widget somewhere close to the given widget, like a tooltip. The given widget must be earier in the render order (tree DFS order) than this widget.
    pub position_next_to: Option<WidgetIdx>,

    // Which child should be focused if this Widget is focused. If multiple, they'll all be focused, see multifocus().
    // These links connect the Widget-s into a second tree that's a subset of the main tree.
    // Input is dispatched in order of DFS post-order traversal of that tree.
    // Most of the time there's at most one focused child, and the focus tree is just a path from some widget to the root in the main tree.
    // Multifocus is useful e.g. in the search dialog where the text input and the search results table are both interactable at the same time (with different keys).
    focus_children: Vec<WidgetIdx>,
    // When was the last time this Widget was focused.
    focus_frame_idx: usize,

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
    pub fn identity<T: Hash + ?Sized>(mut self, t: &T) -> Self { self.identity = hash::<T>(t); self } // example: identity(("row", row_id))
    pub fn source_line(mut self, l: u32) -> Self { self.source_line = l; self }
    pub fn parent(mut self, parent: WidgetIdx) -> Self { self.parent = parent; self }
    pub fn width(mut self, s: AutoSize) -> Self { self.axes[Axis::X].auto_size = s; self }
    pub fn height(mut self, s: AutoSize) -> Self { self.axes[Axis::Y].auto_size = s; self }
    pub fn fixed_width(mut self, s: usize) -> Self { self.axes[Axis::X].auto_size = AutoSize::Fixed(s); self }
    pub fn fixed_height(mut self, s: usize) -> Self { self.axes[Axis::Y].auto_size = AutoSize::Fixed(s); self }
    pub fn min_width(mut self, s: usize) -> Self { self.axes[Axis::X].min_size = s; self }
    pub fn min_height(mut self, s: usize) -> Self { self.axes[Axis::Y].min_size = s; self }
    pub fn max_width(mut self, s: usize) -> Self { self.axes[Axis::X].max_size = s; self }
    pub fn max_height(mut self, s: usize) -> Self { self.axes[Axis::Y].max_size = s; self }
    pub fn hstack(mut self) -> Self { self.axes[0].flags.insert(AxisFlags::STACK); self }
    pub fn vstack(mut self) -> Self { self.axes[1].flags.insert(AxisFlags::STACK); self }
    pub fn fixed_x(mut self, x: isize) -> Self { self.axes[Axis::X].rel_pos = x; self.axes[0].flags.insert(AxisFlags::POS_KNOWN); self }
    pub fn fixed_y(mut self, y: isize) -> Self { self.axes[Axis::Y].rel_pos = y; self.axes[1].flags.insert(AxisFlags::POS_KNOWN); self }
    pub fn hcenter(mut self) -> Self { self.axes[Axis::X].flags.insert(AxisFlags::CENTER); self }
    pub fn vcenter(mut self) -> Self { self.axes[Axis::Y].flags.insert(AxisFlags::CENTER); self }
    pub fn text_lines(mut self, lines: Range<usize>) -> Self { self.draw_text = Some(lines); self }
    pub fn text(mut self, line: usize) -> Self { self.draw_text = Some(line..line+1); self } // example: styled_write!(imgui.text, ...); let l = imgui.text.close_line(); Widget::new().text(l);
    pub fn fill(mut self, c: char, s: Style) -> Self { self.draw_fill = Some((c, s)); self }
    pub fn flags(mut self, f: WidgetFlags) -> Self { self.flags.insert(f); self }
    pub fn style_adjustment(mut self, a: StyleAdjustment) -> Self { self.style_adjustment = a; self }
    pub fn highlight_on_hover(mut self) -> Self { self.flags.insert(WidgetFlags::HIGHLIGHT_ON_HOVER); self }

    // Copy and clear all fields related to being already part of the tree. The resulting Widget can be add()ed to the tree.
    pub fn make_questionable_copy(&self) -> Self {
        let mut r = Self {
            identity: self.identity, source_line: self.source_line, axes: self.axes.clone(), flags: self.flags, draw_text: self.draw_text.clone(), style_adjustment: self.style_adjustment, draw_fill: self.draw_fill.clone(), draw_progress_bar: self.draw_progress_bar.clone(),
            parent: WidgetIdx::invalid(), depth: 0, children: Vec::new(), position_next_to: None, focus_children: Vec::new(), capture_keys: Vec::new(), keys: Vec::new(), capture_mouse: MouseActions::empty(), mouse: MouseActions::empty(), scroll: 0, mouse_pos: [0, 0], scroll_bar_drag_offset: 0, line_wrapped_text: None, draw_cursor_if_focused: None, focus_frame_idx: 0};
        r.flags.remove(WidgetFlags::REDRAW_IF_FOCUS_CHANGES);
        for axis in &mut r.axes {
            axis.flags.remove(AxisFlags::POS_KNOWN | AxisFlags::SIZE_KNOWN);
        }
        r
    }
}

// Use this to create a Widget, instead of Widget::default().
macro_rules! widget {
    () => {Widget::default().source_line(line!())};
}

#[derive(Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct WidgetIdx(usize);
impl WidgetIdx {
    pub fn invalid() -> Self { Self(0) }
    pub fn is_valid(self) -> bool { self.0 != 0 }
}

// Usage:
//   with_parent(imgui, imgui.add(widget!()...), {
//       ...
//   })
// Warning: return/continue/break out of this macro is not allowed, it would bypass the `$imgui.cur_parent = prev_parent` cleanup.
//          This isn't checked at compile time and usually causes an assertion failure later.
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
    pub key_binds: KeyBinds,

    // Current frame that's being built.

    pub tree: Vec<Widget>,
    pub text: StyledText,

    pub root: WidgetIdx,
    pub cur_parent: WidgetIdx,

    pub frame_idx: usize,

    // By default we add 3 layers as children of root: content, dialog, tooltip. The logic around this is quite separate from the rest of IMGUI and can be moved to something like UIState instead, if needed.
    // Main content.
    pub content_root: WidgetIdx,
    // At most one dialog window can be present. It's a child of this Widget, drawn over main content, always has focus, is "owned" by some other widget, and disappears if its owner stops requesting it.
    dialog_root: WidgetIdx,
    dialog_owner: Option<usize>,
    // At most one tooltip can be present. It's drawn over everything else, never has focus, is "owned" by some other widget, and disappears if its owner stops requesting it or loses focus.
    tooltip_root: WidgetIdx,
    tooltip_owner: Option<usize>,

    // Assigned by end_build().
    pub should_redraw: bool,
    pub show_cursor: Option<[isize; 2]>,

    // Previous frame.

    prev_tree: Vec<Widget>,
    prev_map: HashMap<usize, WidgetIdx>,
    prev_root: WidgetIdx,
    prev_focus_chain: Vec<WidgetIdx>,

    // Key presses not dispatched to widgets yet.
    input_buffer: Vec<KeyEx>,

    // Information about mouse events that happened since last frame. If there were multiple clicks, we only keep the last.
    mouse_pos: [isize; 2],
    mouse_click: Option<[isize; 2]>, // coordinates separate from mouse_pos, to be precise if a click happened in the middle of a fast motion
    mouse_scroll: isize,
    mouse_drag_widget: Option<usize>,

    // Other state.

    pub clipboard: String,

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
        self.frame_idx += 1;

        let mut w = Widget {identity: hash(&"root"), source_line: line!(), axes: [Axis {size: width, flags: AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN, ..D!()}, Axis {size: height, flags: AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN, ..D!()}], ..D!()};
        self.carry_widget_state_from_previous_frame(&mut w);
        self.tree.push(w);
        self.root = WidgetIdx(1);
        self.cur_parent = self.root;

        self.content_root = self.add(widget!().identity(&"content"));
        self.dialog_root = self.add(widget!().identity(&"dialog"));
        self.tooltip_root = self.add(widget!().identity(&"tooltip"));

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
            w.focus_children = vec![focus];
        }

        self.determine_focus_chain();

        if let &Some(identity) = &self.tooltip_owner {
            if !self.prev_focus_chain.iter().any(|idx| self.tree[idx.0].identity == identity) {
                self.tooltip_owner = None;
            }
        }
        if self.tooltip_owner.is_none() {
            self.tree[self.tooltip_root.0].children.clear();
        }

        // Do x before y because text height may depend on its width if line wrapping is enabled.
        self.layout_subtree(self.root, Axis::X);
        self.layout_subtree(self.root, Axis::Y);

        let start_time = Instant::now();
        self.render(screen);
        let render_time = Instant::now() - start_time;

        mem::swap(&mut self.tree, &mut self.prev_tree);
        self.prev_root = self.root;
        self.tree.clear();
        self.prev_map.clear();
        for (i, w) in self.prev_tree.iter_mut().enumerate() {
            if w.identity != 0 {
                let prev = self.prev_map.insert(w.identity, WidgetIdx(i));
                if let Some(_) = prev {
                    eprintln!("warning: duplicate widget identity: {} (line {})", w.identity, w.source_line);
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
                    AutoSize::Parent if parent.axes[ax].flags.contains(AxisFlags::SIZE_KNOWN) => { axis.size = parent.axes[ax].size.max(axis.min_size).min(axis.max_size); axis.flags.insert(AxisFlags::SIZE_KNOWN); }
                    AutoSize::Remainder(_) if axis.flags.contains(AxisFlags::STACK) && (axis.min_size != 0 || axis.max_size != usize::MAX) => panic!("min_size/max_size with AutoSize::Remainder is not supported"),
                    AutoSize::Children | AutoSize::Remainder(_) | AutoSize::Parent => (),
                }
            }
        }

        if w.identity == 0 {
            w.identity = parent.children.len();
        }
        {
            let mut idx = w.parent;
            while idx.is_valid() {
                let p = &self.tree[idx.0];
                if !p.flags.contains(WidgetFlags::SKIP_IDENTITY) {
                    w.identity = hash(&(w.identity, p.identity));
                    break;
                }
                idx = p.parent;
            }
        }
        w.depth = parent.depth + 1;

        self.carry_widget_state_from_previous_frame(&mut w);

        self.tree.push(w);
        new_idx
    }

    // Request this widget to be focused, i.e. be a recepient of keyboard input after the end of this frame.
    // Whether the widget actually ends up focused is not determined until the end of the frame build. E.g. a dialog may be added later and steal focus from everyone else.
    // This function assigns the focus_children links of ancestors to point towards this Widget, stopping if an ancestor already has a focus link pointing elsewhere - that link won't be redirected.
    // E.g. if a different window is already focused, focus() on a widget inside the window won't focus this window.
    pub fn focus(&mut self) {
        let mut idx = self.cur_parent;
        let mut parent_idx = self.cur().parent;
        while parent_idx.is_valid() {
            let w = &mut self.tree[parent_idx.0];
            if !w.focus_children.is_empty() {
                break;
            }
            w.focus_children.push(idx);
            idx = parent_idx;
            parent_idx = w.parent;
        }
    }

    // Like focus(), but allows the direct parent to have multiple focused children.
    pub fn multifocus(&mut self) {
        let parent_idx = self.tree[self.cur_parent.0].parent;
        if !parent_idx.is_valid() {
            return;
        }
        let parent = &mut self.tree[parent_idx.0];
        if parent.focus_children.is_empty() {
            self.focus();
        } else if !parent.focus_children.iter().any(|c| *c == self.cur_parent) {
            parent.focus_children.push(self.cur_parent);
        }
    }

    // Focus current widget within the subtree of its ancestor. If that ancestor ends up focused, this widget will be focused.
    pub fn relative_focus(&mut self, ancestor: WidgetIdx) {
        let mut idx = self.cur_parent;
        let mut parent_idx = self.cur().parent;
        while parent_idx.is_valid() {
            let w = &mut self.tree[parent_idx.0];
            if !w.focus_children.is_empty() {
                return;
            }
            w.focus_children.push(idx);
            if parent_idx == ancestor {
                return;
            }
            idx = parent_idx;
            parent_idx = w.parent;
        }
        panic!("ancestor is not an ancestor");
    }

    // Checks whether this widget was focused on the previous frame. Sets REDRAW_IF_FOCUS_CHANGES flag.
    // Useful for auto-closing modal elements (e.g. watch text input) when they lose focus.
    // For auto-closing, this has to be used on some longer-lived ancestor widget; for a widget that was created+focus()ed on this frame, this returns false.
    pub fn check_focus(&mut self) -> bool {
        let w = &mut self.tree[self.cur_parent.0];
        w.flags.insert(WidgetFlags::REDRAW_IF_FOCUS_CHANGES);
        w.focus_frame_idx == self.frame_idx
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
            w.focus_frame_idx = mem::take(&mut prev.focus_frame_idx);
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

    pub fn layout_subtree(&mut self, root: WidgetIdx, ax: usize) {
        // DFS 1: bottom-to-top to calculate sizes that depend on children.
        self.calculate_bottom_up_sizes(root, ax, true);

        // DFS 2: top-to-bottom to calculate sizes that depend on parent, and calculate positions.
        let mut stack: Vec<WidgetIdx> = vec![root];
        let mut scratch: Vec<WidgetIdx> = Vec::new();
        while let Some(idx) = stack.pop() {
            self.calculate_children_layout(idx, ax, &mut scratch);

            for &c in &self.tree[idx.0].children {
                stack.push(c);
            }
        }
    }

    // Adds up rel_pos values on the path from ancestor to descendant. All these positions must have been already calculated, e.g. using layout_subtree().
    pub fn relative_position(&self, ancestor: WidgetIdx, mut descendant: WidgetIdx, ax: usize) -> isize {
        let mut res = 0isize;
        while descendant != ancestor {
            assert!(descendant.is_valid(), "relative_position(): ancestor is not an ancestor");
            let w = &self.tree[descendant.0];
            assert!(w.axes[ax].flags.contains(AxisFlags::POS_KNOWN), "relative_position(): layout not calculated");
            res += w.axes[ax].rel_pos;
            descendant = w.parent;
        }
        res
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
    pub fn close_dialog(&mut self) {
        self.dialog_owner = None;
        self.tree[self.dialog_root.0].children.clear();
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
            let mut w = widget!().width(AutoSize::Children).height(AutoSize::Children).fill(' ', self.palette.default);
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
                    axis.size = size.max(axis.min_size).min(axis.max_size);
                    axis.flags.insert(AxisFlags::SIZE_KNOWN);
                }
            }
        }
    }

    fn try_calculate_simple_size(w: &mut Widget, ax: usize, text: &mut StyledText, palette: &Palette) -> bool {
        let size = match w.axes[ax].auto_size {
            AutoSize::Fixed(s) => s,
            AutoSize::Text if ax == Axis::X && !w.draw_text.as_ref().is_some_and(|r| !r.is_empty()) => panic!("widget on line {} has width is AutoSize::Text, but no lines in draw_text", w.source_line),
            AutoSize::Text if ax == Axis::X => text.widest_line(w.draw_text.clone().unwrap()), // text width
            AutoSize::Text if !w.flags.contains(WidgetFlags::LINE_WRAP) => w.draw_text.as_ref().unwrap().len(), // height without line wrap
            AutoSize::Text if w.axes[0].flags.contains(AxisFlags::SIZE_KNOWN) => Self::get_line_wrapped_text(w, text, palette).len(), // height with line wrap
            _ => return false,
        };
        let size = size.max(w.axes[ax].min_size).min(w.axes[ax].max_size);
        w.axes[ax].size = size;
        w.axes[ax].flags.insert(AxisFlags::SIZE_KNOWN);
        true
    }

    fn get_line_wrapped_text(w: &mut Widget, text: &mut StyledText, palette: &Palette) -> Range<usize> {
        match &w.line_wrapped_text {
            Some(x) => x.clone(),
            None => {
                let max_lines = if w.axes[Axis::Y].flags.contains(AxisFlags::SIZE_KNOWN) {
                    w.axes[Axis::Y].size
                } else if w.axes[Axis::Y].max_size != usize::MAX {
                    w.axes[Axis::Y].max_size
                } else {
                    1000
                };
                let r = text.line_wrap(w.draw_text.clone().unwrap(), w.axes[Axis::X].size, max_lines, &palette.line_wrap_indicator, &palette.truncation_indicator, None);
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
                    AutoSize::Parent => { child_axis.size = parent_size.max(child_axis.min_size).min(child_axis.max_size); child_axis.flags.insert(AxisFlags::SIZE_KNOWN); }
                    AutoSize::Remainder(f) if !flags.contains(AxisFlags::STACK) => { child_axis.size = ((parent_size as f64 * f.max(0.0) + 0.5) as usize).max(child_axis.min_size).min(child_axis.max_size); child_axis.flags.insert(AxisFlags::SIZE_KNOWN); }
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
            if !child_axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                let f = match &child_axis.auto_size {
                    AutoSize::Remainder(f) => f,
                    _ => panic!("huh"),
                };
                child_axis.size = remainder_helper.calculate_remainder_fraction(*f, child_idx.0).max(child_axis.min_size).min(child_axis.max_size);
                child_axis.flags.insert(AxisFlags::SIZE_KNOWN);
            }
            if !child_axis.flags.contains(AxisFlags::POS_KNOWN) {
                if child_axis.flags.contains(AxisFlags::CENTER) {
                    child_axis.rel_pos = (parent_size as isize - child_axis.size as isize) / 2;
                } else {
                    child_axis.rel_pos = position;
                }
                child_axis.flags.insert(AxisFlags::POS_KNOWN);
            }
            if flags.contains(AxisFlags::STACK) {
                position = child_axis.rel_pos + child_axis.size as isize;
            }
        }
    }

    fn render(&mut self, screen: &mut ScreenBuffer) {
        let mut stack: Vec<(WidgetIdx, /*pos*/ [isize; 2], /*clip*/ Rect, StyleAdjustment)> = vec![(self.root, [0, 0], screen.rect(), StyleAdjustment::default())];
        let mut should_redraw = false;
        let mut show_cursor: Option<[isize; 2]> = None;
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
            let mut show_horizontal_text_truncation_indicator = !w.flags.contains(WidgetFlags::TEXT_TRUNCATION_INDICATOR_DISABLE);
            let mut show_vertical_text_truncation_indicator = !w.flags.contains(WidgetFlags::TEXT_TRUNCATION_INDICATOR_DISABLE);

            let w = &mut self.tree[idx.0];
            w.axes[0].abs_pos = pos[0];
            w.axes[1].abs_pos = pos[1];
            if draw_text.is_some() && w.flags.contains(WidgetFlags::LINE_WRAP) {
                draw_text = Some(Self::get_line_wrapped_text(w, &mut self.text, &self.palette));
                show_horizontal_text_truncation_indicator = false;
                show_vertical_text_truncation_indicator = false;
            }
            if w.flags.contains(WidgetFlags::HIGHLIGHT_ON_HOVER) {
                w.capture_mouse.insert(MouseActions::HOVER_SUBTREE);
            }

            let w = &self.tree[idx.0];

            let rect = Rect {x: pos[0], y: pos[1], w: w.axes[0].size, h: w.axes[1].size};
            let mut clip = clip.intersection(rect);

            if w.flags.contains(WidgetFlags::RESET_STYLE_ADJUSTMENT) {
                style_adjustment = StyleAdjustment::default();
            }
            style_adjustment.update(w.style_adjustment);
            if w.flags.contains(WidgetFlags::HIGHLIGHT_ON_HOVER) && w.mouse.contains(MouseActions::HOVER_SUBTREE) {
                style_adjustment.update(self.palette.hovered);
            }

            if w.flags.contains(WidgetFlags::REDRAW_IF_VISIBLE) && !clip.is_empty() {
                should_redraw = true;

                if let Some((c, style)) = self.palette.placeholder_fill.clone() {
                    let mut buf = [0u8; 4];
                    screen.fill(clip, c.encode_utf8(&mut buf), style);
                }
            }

            if w.flags.contains(WidgetFlags::HSCROLL_INDICATOR_ARROWS) {
                let content_width = if let Some(idx) = w.children.last() {
                    (self.tree[idx.0].axes[Axis::X].rel_pos + self.tree[idx.0].axes[Axis::X].size as isize).max(0)
                } else if let Some(r) = w.draw_text.clone() {
                    self.text.widest_line(r) as isize
                } else {
                    0
                };
                if rect.x + content_width > clip.right() && clip.w > 0 {
                    let wid = str_width(&self.palette.hscroll_indicator.1);
                    for y in clip.y..clip.bottom() {
                        screen.put_text(&self.palette.hscroll_indicator.1, style_adjustment.apply(self.palette.hscroll_indicator.2), clip.right() - wid as isize, y, clip);
                    }
                    clip.w -= wid;
                }
                if clip.x > rect.x && rect.w > 0 {
                    let wid = str_width(&self.palette.hscroll_indicator.0);
                    for y in clip.y..clip.bottom() {
                        screen.put_text(&self.palette.hscroll_indicator.0, style_adjustment.apply(self.palette.hscroll_indicator.2), clip.x, y, clip);
                    }
                    clip.x += wid as isize;
                    clip.w -= wid;
                }
                show_horizontal_text_truncation_indicator = false;
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
                    if y_offset >= rect.h {
                        break;
                    }
                    let mut spans = self.text.get_line(line_idx);
                    let line_str = self.text.get_line_str(line_idx);
                    let y = rect.y + y_offset as isize;
                    let mut x = rect.x;
                    let indicate_vertical_truncation = show_vertical_text_truncation_indicator && y_offset + 1 == rect.h && line_idx + 1 < text_range.end;
                    let mut indicate_horizontal_truncation = show_horizontal_text_truncation_indicator;

                    if str_width(line_str) <= rect.w {
                        indicate_horizontal_truncation = false;
                    } else if w.flags.contains(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT) {
                        if indicate_horizontal_truncation {
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
                        indicate_horizontal_truncation = false;
                    }

                    for span_idx in self.text.get_line(line_idx) {
                        let (s, style) = self.text.get_span(span_idx);
                        x = screen.put_text(s, style_adjustment.apply(style), x, y, clip);
                        if x > clip.right() {
                            break;
                        }
                    }
                    if (indicate_vertical_truncation || indicate_horizontal_truncation) && rect.w > 0 {
                        screen.put_text(&self.palette.truncation_indicator.1, style_adjustment.apply(self.palette.truncation_indicator.2), rect.right() - str_width(&self.palette.truncation_indicator.1) as isize, y, clip);
                    }
                }
            }

            if let &Some([x, y]) = &w.draw_cursor_if_focused {
                let (x, y) = (rect.x + x, rect.y + y);
                if w.focus_frame_idx == self.frame_idx + 1 && x >= clip.x && y >= clip.y && x <= clip.right() && y < clip.bottom() { // can't use clip.contains([x, y]) because of the `<=` on this line (because we use bar cursor rather than block cursor)
                    show_cursor = Some([x, y]);
                }
            }

            for &c in w.children.iter().rev() {
                stack.push((c, pos, clip, style_adjustment.clone()));
            }
        }
        self.should_redraw |= should_redraw;
        self.show_cursor = show_cursor;
    }

    fn determine_focus_chain(&mut self) {
        self.prev_focus_chain.clear();
        let mut stack: Vec<(WidgetIdx, /*pass*/ u8)> = vec![(self.root, 0)];
        while let Some((idx, pass)) = stack.pop() {
            let w = &mut self.tree[idx.0];
            if pass == 1 || w.focus_children.is_empty() {
                self.prev_focus_chain.push(idx);
                if w.focus_frame_idx < self.frame_idx && w.flags.contains(WidgetFlags::REDRAW_IF_FOCUS_CHANGES) {
                    self.should_redraw = true;
                }
                w.focus_frame_idx = self.frame_idx + 1;
                continue;
            }
            stack.push((idx, 1));
            for &c in w.focus_children.iter().rev() {
                stack.push((c, 0));
            }
        }
        for w in &self.tree {
            if w.flags.contains(WidgetFlags::REDRAW_IF_FOCUS_CHANGES) && w.focus_frame_idx == self.frame_idx {
                self.should_redraw = true;
            }
        }
    }
    
    fn dispatch_input(&mut self) {
        if self.prev_tree.is_empty() {
            return;
        }

        let mut keys = mem::take(&mut self.input_buffer);
        for &idx in &self.prev_focus_chain {
            if keys.is_empty() {
                break;
            }
            let w = &mut self.prev_tree[idx.0];
            let text_input = w.flags.contains(WidgetFlags::CAPTURE_TEXT_INPUT_KEYS);
            if !w.capture_keys.is_empty() || text_input {
                keys.retain(|key| {
                    if (text_input && key.mods.is_empty() && key.key.is_ordinary_char()) || w.capture_keys.iter().find(|k| k.key == key.key && (k.mods.contains(ModKeys::ANY) || k.mods == key.mods)).is_some() {
                        w.keys.push(key.clone());
                        false
                    } else {
                        true
                    }
                });
            }
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

        let mut scroll = mem::take(&mut self.mouse_scroll);
        if let Some(identity) = self.mouse_drag_widget.clone() {
            if let Some(idx) = self.prev_map.get(&identity) {
                let w = &mut self.prev_tree[idx.0];
                if w.capture_mouse.contains(MouseActions::DRAG) {
                    report_event(w, MouseActions::DRAG, self.mouse_pos);

                    if w.capture_mouse.contains(MouseActions::SCROLL) {
                        report_event(w, MouseActions::SCROLL, self.mouse_pos);
                        w.scroll = mem::take(&mut scroll);
                    }
                } else {
                    self.mouse_drag_widget = None;
                }
            } else {
                self.mouse_drag_widget = None;
            }
        }

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


#[derive(Default)]
pub struct Palette {
    pub default: Style,
    pub default_dim: Style,
    pub error: Style,

    pub running: Style,
    pub suspended: Style,
    pub function: Style,

    // How to highlight things like selected table row or selected code line.
    pub selected: StyleAdjustment,
    // How to highlight clickable widgets on mouse hover.
    pub hovered: StyleAdjustment,
    // How to draw widgets with REDRAW_IF_VISIBLE flag, as a sort of loading indicator. Normally it should be visible very rarely and only for one frame.
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
    pub dialog: StyleAdjustment,

    pub text_input: Style,
    pub text_input_selected: Style,

    pub tree_indent: Style,
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

    fn init(&mut self) {
        for (key, action) in &self.key_to_action {
            self.action_to_keys.entry(action.clone()).or_default().push(key.clone());
        }
    }
}
impl Default for KeyBinds {
    fn default() -> Self {
        let mut res = Self {
            vscroll_sensitivity: 1, hscroll_sensitivity: 10, key_to_action: HashMap::from([
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
                // (Ctrl+tab and ctrl+shift+tab are unrepresentable in ansi escape codes.)
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
            ]),
            text_input_key_to_action: HashMap::from([
                (Key::Char('7').ctrl(), KeyAction::Undo), // ctrl+/ is indistinguishable from ctrl+7
                (Key::Char('_').alt(), KeyAction::Redo),
                (Key::Char('y').ctrl(), KeyAction::Paste),
                (Key::Char('c').ctrl(), KeyAction::Copy),
                (Key::Char('x').ctrl(), KeyAction::Cut),
                (Key::Char('v').ctrl(), KeyAction::Paste),
                (Key::Char('\n').alt(), KeyAction::NewLine),
            ]),
            action_to_keys: HashMap::new()};
        res.init();
        res
    }
}

// ======================================================= widgets.rs =========================================================================

// React to keys like up/down/pageup/pagedown/home/end by moving `cursor`, within range [0, count).
// `cur_parent` is the "viewport" widget; its height should be known (for handling PageUp/PageDown keys); it needs to be focused for the input to work.
// `row_height` is used for PageUp/PageDown jump size.
// Returns true iff any relevant keys were pressed, even if it didn't move the cursor (e.g. trying to move up when already at the top) - useful as a trigger to scroll the view to the cursor.
pub fn list_cursor_navigation(cursor: &mut usize, count: usize, row_height: usize, imgui: &mut IMGUI) -> bool {
    list_cursor_navigation_with_variable_row_height(cursor, count, |_, _| row_height, imgui)
}

pub fn list_cursor_navigation_with_variable_row_height<F: FnMut(usize, &mut IMGUI) -> usize>(cursor: &mut usize, count: usize, mut row_height_fn: F, imgui: &mut IMGUI) -> bool {
    assert!(imgui.cur().axes[Axis::Y].flags.contains(AxisFlags::SIZE_KNOWN));
    let viewport_height = imgui.cur().axes[Axis::Y].size;
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
                let mut offset = viewport_height.saturating_sub(2);
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
                let mut offset = viewport_height.saturating_sub(2);
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

    let viewport_height = imgui.calculate_size(imgui.cur_parent, Axis::Y);
    let w = imgui.cur();
    assert_eq!(w.children.len(), 1);
    let content_height = imgui.calculate_size(w.children[0], Axis::Y);

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
            let bar_height = imgui.calculate_size(scroll_bar, Axis::Y).saturating_sub(1); // -1 because top and bottom half-characters are empty
            imgui.cur_mut().flags.insert(WidgetFlags::HIGHLIGHT_ON_HOVER);
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
    if scroll_bar.is_valid() {
        let s = imgui.palette.default;
        let w = imgui.get_mut(scroll_bar);
        if w.draw_text.is_none() {
            w.draw_fill = Some((' ', s));
        }
    }

    let idx = imgui.cur().children[0];
    let w = imgui.get_mut(idx);
    w.axes[Axis::Y].flags.insert(AxisFlags::POS_KNOWN);
    w.axes[Axis::Y].rel_pos = -*scroll;

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

// Simple horizontal scrolling on left/right arrow keys.
pub fn hscrolling_navigation(hscroll: &mut isize, imgui: &mut IMGUI) {
    let w = imgui.cur();
    assert_eq!(w.children.len(), 1, "scrollable viewport must have exactly one child (line {})", w.source_line);
    assert!(w.axes[Axis::X].flags.contains(AxisFlags::SIZE_KNOWN), "scrollable viewport width must be calculated in advance (line {})", w.source_line);
    let viewport_width = w.axes[Axis::X].size;
    let content_width = imgui.calculate_size(w.children[0], Axis::X);

    // Scroll by viewport width-2 with each key press, the 2 to fit the HSCROLL_INDICATOR_ARROWS.
    let step = viewport_width.saturating_sub(2).max(1) as isize;
    for action in imgui.check_keys(&[KeyAction::CursorLeft, KeyAction::CursorRight]) {
        match action {
            KeyAction::CursorLeft => *hscroll = hscroll.saturating_sub(step),
            KeyAction::CursorRight => *hscroll += step,
            _ => panic!("huh"),
        }
    }
    *hscroll = (*hscroll).min(content_width as isize - viewport_width as isize).max(0);

    let idx = imgui.cur().children[0];
    let w = imgui.get_mut(idx);
    w.axes[Axis::X].flags.insert(AxisFlags::POS_KNOWN);
    w.axes[Axis::X].rel_pos = -*hscroll;
}

pub fn make_dialog_frame(dialog_root: WidgetIdx, width: AutoSize, height: AutoSize, style_adjustment: StyleAdjustment, border: Style, title: &str, imgui: &mut IMGUI) -> Option<WidgetIdx> {
    let dialog = with_parent!(imgui, dialog_root, {
        if imgui.check_mouse(MouseActions::CLICK) || imgui.check_key(KeyAction::Cancel) {
            None
        } else {
            Some(imgui.add(widget!().parent(dialog_root).width(width).height(height).hcenter().vcenter().fill(' ', imgui.palette.default).style_adjustment(style_adjustment)))
        }
    });
    let dialog = match dialog {
        None => {
            imgui.close_dialog();
            return None;
        }
        Some(x) => x,
    };
    imgui.layout_children(Axis::X);
    imgui.layout_children(Axis::Y);
    with_parent!(imgui, dialog, {
        imgui.check_mouse(MouseActions::CLICK); // clicking inside the dialog shouldn't close it
        let (w, h) = (imgui.cur().axes[Axis::X].size, imgui.cur().axes[Axis::Y].size);
        imgui.add(widget!().fixed_width(1).fixed_height(1).fill('┌', border));
        styled_write!(imgui.text, imgui.palette.default, "{}", title);
        let l = imgui.text.close_line();
        imgui.add(widget!().fixed_width(w.saturating_sub(2)).fixed_height(1).fixed_x(1).fill('─', border).text(l));
        imgui.add(widget!().fixed_width(1).fixed_height(1).fixed_x(w as isize - 1).fill('┐', border));
        imgui.add(widget!().fixed_width(1).fixed_height(h.saturating_sub(2)).fixed_y(1).fill('│', border));
        imgui.add(widget!().fixed_width(1).fixed_height(h.saturating_sub(2)).fixed_y(1).fixed_x(w as isize - 1).fill('│', border));
        imgui.add(widget!().fixed_width(1).fixed_height(1).fixed_y(h as isize - 1).fill('└', border));
        imgui.add(widget!().fixed_width(w.saturating_sub(2)).fixed_height(1).fixed_x(1).fixed_y(h as isize - 1).fill('─', border));
        imgui.add(widget!().fixed_width(1).fixed_height(1).fixed_x(w as isize - 1).fixed_y(h as isize - 1).fill('┘', border));

        with_parent!(imgui, imgui.add(widget!().fixed_width(w.saturating_sub(4)).fixed_height(h.saturating_sub(4)).fixed_x(2).fixed_y(2)), {
            imgui.focus();
            Some(imgui.cur_parent)
        })
    })
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
        w.axes[Axis::Y].flags.insert(AxisFlags::STACK);
        assert!(w.axes[0].flags.contains(AxisFlags::SIZE_KNOWN) && w.axes[1].flags.contains(AxisFlags::SIZE_KNOWN), "Table widget requires the size to be known in advance");
        let (width, height) = (w.axes[Axis::X].size, w.axes[Axis::Y].size);
        let (viewport, rows_container, scroll_bar);

        with_parent!(imgui, root, {
            // Header.
            with_parent!(imgui, imgui.add(widget!().hstack().fixed_height(1)), {
                for i in 0..columns.len() {
                    with_parent!(imgui, imgui.add(widget!().width(AutoSize::Text).fixed_height(1)), {
                        styled_write!(imgui.text, imgui.palette.table_header, "{}", columns[i].title);
                        imgui.set_text();
                    });
                }
            });

            with_parent!(imgui, imgui.add(widget!().hstack().fixed_height(height.saturating_sub(1))), {
                // Clipping rectangle for the scrollable area.
                viewport = imgui.add(widget!().fixed_width(width.saturating_sub(1)));
                with_parent!(imgui, viewport, {
                    imgui.focus();
                    // Widget containing all rows, moves up and down when scrolling.
                    rows_container = imgui.add(widget!().height(AutoSize::Children).vstack().fixed_y(0));
                });
                scroll_bar = imgui.add(widget!().fixed_width(1));
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
            imgui.cur_mut().axes[Axis::Y].auto_size = AutoSize::Fixed(num_rows * row_height);
            if imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                let y = imgui.cur().mouse_pos[1];
                if y >= 0 && (y as usize) < num_rows * row_height {
                    self.state.cursor = y as usize / row_height;
                    self.scroll_to_cursor = true;
                }
            }
        });
        self.scroll_to_cursor |= with_parent!(imgui, self.viewport, {
            list_cursor_navigation(&mut self.state.cursor, num_rows, row_height, imgui)
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
            with_parent!(imgui, imgui.add(widget!().identity(&id).hstack().fixed_x(x).height(row_height).fill(' ', imgui.palette.default).highlight_on_hover()), {
                if !self.lazy && imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                    self.state.cursor = imgui.get(self.rows_container).children.len() - 1;
                    self.scroll_to_cursor = true;
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
            imgui.add(widget!().width(self.columns[i].auto_width).height(AutoSize::Text))
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
                col.width = col.width.max(imgui.calculate_size(cell, Axis::X));
            }
        }

        let reserved = if self.enable_selection_icon {2usize} else {0};

        // Calculate Remainder column widths.
        let mut remainder_helper = RemainderFractionHelper::new(imgui.get(self.rows_container).axes[Axis::X].size);
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
            w.axes[Axis::X].size = col.width;
            w.axes[Axis::X].rel_pos = reserved as isize + col.pos;
            w.axes[Axis::X].flags.insert(AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN);
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
                w.axes[Axis::X].auto_size = AutoSize::Fixed(col.width);
                w.axes[Axis::X].size = col.width;
                w.axes[Axis::X].rel_pos = col.pos;
                w.axes[Axis::X].flags.insert(AxisFlags::SIZE_KNOWN | AxisFlags::POS_KNOWN);
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
                w.axes[Axis::Y].auto_size = AutoSize::Fixed(self.total_rows * self.fixed_row_height);
                assert_eq!(w.children.len(), self.row_idxs.len(), "number of rows created doesn't match the range returned from lazy()");
                if !w.children.is_empty() {
                    let r = w.children[0];
                    let w = imgui.get_mut(r);
                    w.axes[Axis::Y].flags.insert(AxisFlags::POS_KNOWN);
                    w.axes[Axis::Y].rel_pos = (self.row_idxs.start * self.fixed_row_height) as isize;
                }
            }
            imgui.layout_children(Axis::Y);
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
                list_cursor_navigation_with_variable_row_height(&mut self.state.cursor, num_rows, |i, imgui| imgui.get(imgui.get(self.rows_container).children[i]).axes[Axis::Y].size, imgui)
            });
            let scroll_to = if self.scroll_to_cursor && num_rows > 0 {
                let ax = &imgui.get(imgui.get(self.rows_container).children[self.state.cursor]).axes[Axis::Y];
                Some(ax.rel_pos..ax.rel_pos + ax.size as isize)
            } else {
                None
            };
            with_parent!(imgui, self.viewport, {
                scrolling_navigation(&mut self.state.scroll, scroll_to, self.root, self.scroll_bar, imgui);
            });
        }

        let hide_cursor = with_parent!(imgui, self.viewport, {
            self.hide_cursor_if_unfocused && !imgui.check_focus()
        });
        if self.row_idxs.contains(&self.state.cursor) && !hide_cursor {
            with_parent!(imgui, imgui.get(self.rows_container).children[self.state.cursor - self.row_idxs.start], {
                imgui.focus();
                let a = imgui.palette.selected;
                let row = imgui.cur_mut();
                row.style_adjustment.update(a);
                if self.enable_selection_icon {
                    let y = row.axes[Axis::Y].rel_pos;
                    with_parent!(imgui, self.rows_container, {
                        imgui.add(widget!().fixed_width(1).fixed_x(0).fixed_height(1).fixed_y(y).fill('➤', imgui.palette.default));
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
            imgui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);
            let mut width = 0usize;
            for (col_idx, col) in self.columns.iter().enumerate() {
                width = width.max(str_width(col.title));
                styled_write!(imgui.text, imgui.palette.table_header, "{}", col.title);
                let l = imgui.text.close_line();
                imgui.add(widget!().height(AutoSize::Text).text(l).flags(WidgetFlags::LINE_WRAP));
                
                let cell = imgui.get(row).children[col_idx];
                let w = Self::questionable_autotooltip(imgui, cell);
                width = width.max(w);
                
                if col_idx + 1 < self.columns.len() {
                    imgui.add(widget!().fixed_height(1));
                }
            }
            let parent = imgui.cur().parent;
            width = width.min(imgui.get(parent).axes[Axis::X].size);
            imgui.cur_mut().axes[Axis::X].auto_size = AutoSize::Fixed(width);
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
                w.axes[Axis::X].auto_size = AutoSize::Parent;
            }
            let new_to = imgui.add(w);
            for c in imgui.get(from).children.iter().rev() {
                stack.push((*c, new_to));
            }
        }
        width
    }
}


#[derive(Default)]
pub struct TabsState {
    pub selected: usize,
    pub hscroll: isize,
}

pub struct Tabs {
    pub scroll_to_selected_tab: bool,
    tabs: Vec<(WidgetIdx, /*short_title_idx*/ usize, /*full_title_idx*/ usize)>,
    state: TabsState,
    viewport: WidgetIdx,
    content: WidgetIdx,
}
impl Tabs {
    pub fn new(state: TabsState, imgui: &mut IMGUI) -> Self {
        let content = imgui.add(widget!().width(AutoSize::Children).fixed_height(1).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS).hstack());
        Self {scroll_to_selected_tab: false, tabs: Vec::new(), state, viewport: imgui.cur_parent, content}
    }

    pub fn add(&mut self, full_title: &str, short_title: &str, pinned: bool, imgui: &mut IMGUI) {
        with_parent!(imgui, self.content, {
            if self.tabs.is_empty() {
                styled_write!(imgui.text, imgui.palette.tab_separator.1, " ");
            } else {
                styled_write!(imgui.text, imgui.palette.tab_separator.1, "{}", imgui.palette.tab_separator.0);
            }
            let l = imgui.text.close_line();
            imgui.add(widget!().width(AutoSize::Text).text(l));

            with_parent!(imgui, imgui.add(widget!().identity(&(full_title, self.tabs.len())).width(AutoSize::Children).hstack().highlight_on_hover()), {
                if imgui.check_mouse(MouseActions::CLICK) {
                    self.state.selected = self.tabs.len();
                    self.scroll_to_selected_tab = true;
                }

                let style = if pinned {
                    styled_write!(imgui.text, imgui.palette.tab_title_pinned, "📌 ");
                    let l = imgui.text.close_line();
                    imgui.add(widget!().width(AutoSize::Text).text(l));
                    imgui.palette.tab_title_pinned
                } else {
                    imgui.palette.tab_title
                };

                styled_write!(imgui.text, style, "{}", short_title);
                let short_title_line = imgui.text.close_line();
                styled_write!(imgui.text, style, "{}", full_title);
                let full_title_line = imgui.text.close_line();

                imgui.add(widget!().width(AutoSize::Text).max_width(35).text(short_title_line));

                self.tabs.push((imgui.cur_parent, short_title_line, full_title_line));
            });
        });
    }

    pub fn finish(mut self, imgui: &mut IMGUI) -> TabsState {
        self.disambiguate_titles(imgui);

        let keys = imgui.check_keys(&[KeyAction::PreviousTab, KeyAction::NextTab]);
        self.scroll_to_selected_tab |= !keys.is_empty();
        for key in keys {
            match key {
                KeyAction::PreviousTab => self.state.selected = self.state.selected.saturating_sub(1),
                KeyAction::NextTab => self.state.selected += 1,
                _ => panic!("huh"),
            }
        }

        if !self.tabs.is_empty() {
            if self.state.selected >= self.tabs.len() {
                self.state.selected = self.tabs.len() - 1;
                self.scroll_to_selected_tab = true;
            }
            let adj = imgui.palette.selected;
            imgui.get_mut(self.tabs[self.state.selected].0).style_adjustment.update(adj);
        }

        self.state.hscroll += with_parent!(imgui, self.viewport, {
            imgui.check_scroll() * imgui.key_binds.hscroll_sensitivity
        });
        let viewport_width = imgui.calculate_size(self.viewport, Axis::X);
        let content_width = imgui.calculate_size(self.content, Axis::X);
        with_parent!(imgui, self.content, {
            imgui.layout_children(Axis::X);
            if self.scroll_to_selected_tab && !self.tabs.is_empty() {
                let w = imgui.get(self.tabs[self.state.selected].0);
                scroll_to_range(&mut self.state.hscroll, w.axes[Axis::X].rel_pos..w.axes[Axis::X].rel_pos + w.axes[Axis::X].size as isize, viewport_width);
            }
        });
        self.state.hscroll = self.state.hscroll.min(content_width as isize - viewport_width as isize).max(0);
        let w = imgui.get_mut(self.content);
        w.axes[Axis::X].rel_pos = -self.state.hscroll;
        w.axes[Axis::X].flags.insert(AxisFlags::POS_KNOWN);
        self.state
    }

    fn disambiguate_titles(&mut self, imgui: &mut IMGUI) {
        let mut tabs = self.tabs.clone();
        tabs.sort_by_key(|t| imgui.text.get_line_str(t.1));
        let mut start = 0;
        while start < tabs.len() {
            let mut end = start + 1;
            let short_title = imgui.text.get_line_str(tabs[start].1);
            while end < tabs.len() && imgui.text.get_line_str(tabs[end].1) == short_title {
                end += 1;
            }
            if end > start + 1 {
                let full_title = imgui.text.get_line_str(tabs[start].2);
                let (mut common_prefix, mut common_suffix) = (full_title.len(), full_title.len());
                for i in start+1..end {
                    let t = imgui.text.get_line_str(tabs[i].2);
                    common_prefix = longest_common_prefix(&full_title[..common_prefix], t);
                    common_suffix = longest_common_suffix(&full_title[full_title.len()-common_suffix..], t);
                }
                for (i, &(widget_idx, _, full_title_idx)) in tabs[start..end].iter().enumerate() {
                    with_parent!(imgui, widget_idx, {
                        styled_write!(imgui.text, imgui.palette.tab_title, " [");
                        let l = imgui.text.close_line();
                        imgui.add(widget!().width(AutoSize::Text).text(l));

                        let mut range = imgui.text.get_line_char_range(full_title_idx);
                        if common_prefix + common_suffix < range.len() {
                            range.start += common_prefix;
                            range.end -= common_suffix;
                            unsafe {imgui.text.chars.as_mut_vec().extend_from_within(range)};
                            imgui.text.close_span(imgui.palette.tab_title);
                        } else {
                            styled_write!(imgui.text, imgui.palette.tab_title, "{}", i - start + 1);
                        }
                        let l = imgui.text.close_line();
                        imgui.add(widget!().width(AutoSize::Text).max_width(35).text(l));

                        styled_write!(imgui.text, imgui.palette.tab_title, "]");
                        let l = imgui.text.close_line();
                        imgui.add(widget!().width(AutoSize::Text).text(l));
                    });
                }
            }
            start = end;
        }
    }
}

fn longest_common_prefix(a: &str, b: &str) -> usize {
    let mut i = 0;
    while i < a.len().min(b.len()) && a.as_bytes()[i] == b.as_bytes()[i] {
        i += 1;
    }
    while !a.is_char_boundary(i) {
        i -= 1;
    }
    i
}
fn longest_common_suffix(a: &str, b: &str) -> usize {
    let mut i = 0;
    while i < a.len().min(b.len()) && a.as_bytes()[a.len()-1-i] == b.as_bytes()[b.len()-1-i] {
        i += 1;
    }
    while !a.is_char_boundary(a.len()-i) {
        i -= 1;
    }
    i
}


pub struct TextInput {
    pub text: String,
    pub cursor: usize,
    pub mark: usize,

    pub multiline: bool,
    pub line_wrap: bool,
    pub capture_vertical_movement_keys: bool, // Up/Down arrow keys, PageUp/PageDown
    pub capture_return_key: bool, // Char('\n'); but ctrl+return is always captured

    pub scroll_to_cursor: bool,
    // true if right arrow key was pressed when the cursor was already at the end.
    // For open dialog it's used as a signal to open file/function without closing the dialog.
    pub moved_past_the_end: bool,

    undo: Vec<(String, /*old_cursor*/ usize, /*new_cursor*/ usize)>,
    undo_idx: usize,

    scroll: isize,
    hscroll: isize,
    // How the `text` characters are laid out on screen. Used for moving cursor up and down.
    lines: Vec<(/*start_x*/ isize, /*bytes*/ Range<usize>)>,
    viewport_height: usize,

    // If moving vertically through: long line -> short line -> long line, remember the cursor horizontal position even if it's beyond the short line's width.
    saved_x: Option<isize>,
}
impl Default for TextInput {
    fn default() -> Self { Self {text: String::new(), cursor: 0, mark: 0, multiline: false, line_wrap: false, capture_vertical_movement_keys: false, capture_return_key: false, undo: vec![(String::new(), 0, 0)], undo_idx: 0, scroll: 0, hscroll: 0, scroll_to_cursor: false, moved_past_the_end: false, lines: Vec::new(), saved_x: None, viewport_height: 0} }
}
impl TextInput {
    pub fn new_with_text(text: String) -> Self { Self {cursor: text.len(), mark: text.len(), multiline: false, line_wrap: false, capture_vertical_movement_keys: false, capture_return_key: false, undo: vec![(text.clone(), text.len(), text.len())], text, undo_idx: 0, scroll: 0, hscroll: 0, scroll_to_cursor: true, moved_past_the_end: false, lines: Vec::new(), saved_x: None, viewport_height: 0} }
    pub fn new_multiline(text: String) -> Self { Self {cursor: text.len(), mark: text.len(), multiline: true, line_wrap: true, capture_vertical_movement_keys: true, capture_return_key: false, undo: vec![(text.clone(), text.len(), text.len())], text, undo_idx: 0, scroll: 0, hscroll: 0, scroll_to_cursor: true, moved_past_the_end: false, lines: Vec::new(), saved_x: None, viewport_height: 0} }

    // Returns true if there was any input (or if scroll_to_cursor was set to true from the outside).
    // cur_parent's axes determine scrolling/resizing behavior:
    //  * If AutoSize::Fixed, the viewport size is fixed, and the text is scrollable inside it.
    //  * If AutoSize::Children, the viewport size is set to text size. If text is bigger than the Axis's max_size, the viewport size is clamped and scrolling is enabled.
    // In multiline mode, space for vertical scroll bar is reserved even if the text fits (except AutoSize::Children with max_size = MAX).
    pub fn build(&mut self, imgui: &mut IMGUI) -> bool {
        for ax in 0..2 {
            let axis = &imgui.cur().axes[ax];
            assert!(axis.flags.contains(AxisFlags::SIZE_KNOWN) || axis.auto_size.is_children(), "only Fixed or Children viewport sizes are supported by TextInput; got {:?}", axis.auto_size);
        }

        {
            let s = imgui.palette.default;
            let w = imgui.cur_mut();
            w.flags.insert(WidgetFlags::RESET_STYLE_ADJUSTMENT);
            w.draw_fill = Some((' ', s));
        }

        let enable_scroll_bar = self.multiline && (imgui.cur().axes[1].flags.contains(AxisFlags::SIZE_KNOWN) || imgui.cur().axes[1].max_size < usize::MAX);
        let (mut viewport_widget, mut scroll_bar) = (imgui.cur_parent, WidgetIdx::invalid());
        if enable_scroll_bar {
            imgui.cur_mut().axes[0].flags.insert(AxisFlags::STACK);
            let mut w = widget!();
            for ax in 0..2 {
                let sub = if ax == 0 {1} else {0};
                let axis = &imgui.cur().axes[ax];
                if axis.flags.contains(AxisFlags::SIZE_KNOWN) {
                    w.axes[ax].auto_size = AutoSize::Fixed(axis.size.saturating_sub(sub));
                } else {
                    w.axes[ax].auto_size = AutoSize::Children;
                    w.axes[ax].min_size = axis.min_size.saturating_sub(sub);
                    w.axes[ax].max_size = axis.max_size;
                    if axis.max_size < usize::MAX {
                        w.axes[ax].max_size = axis.max_size.saturating_sub(sub);
                    }
                }
            }

            viewport_widget = imgui.add(w);
            scroll_bar = imgui.add(widget!().fixed_width(1).fixed_height(0));
        };

        let content_widget = imgui.add(widget!().parent(viewport_widget).height(AutoSize::Text).width(AutoSize::Text).fixed_x(0).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS));

        self.update(content_widget, imgui);
        with_parent!(imgui, viewport_widget, {
            self.render(content_widget, scroll_bar, imgui);
        });
        mem::take(&mut self.scroll_to_cursor)
    }

    fn update(&mut self, content_widget: WidgetIdx, imgui: &mut IMGUI) {
        self.moved_past_the_end = false;

        with_parent!(imgui, content_widget, {
            imgui.focus();

            let clicked = imgui.check_mouse(MouseActions::CLICK);
            if let Some(pos) = imgui.check_drag() {
                self.cursor = self.coordinates_to_offset(pos);
                if clicked {
                    self.mark = self.cursor;
                }
            }
        });

        imgui.cur_mut().flags.insert(WidgetFlags::CAPTURE_TEXT_INPUT_KEYS);
        // Most text navigation keys are currently not customizable by KeyBinds as it doesn't seem useful and there are too many.
        imgui.cur_mut().capture_keys.extend_from_slice(&[
            Key::Left.any_mods(), Key::Right.any_mods(), Key::Char('b').ctrl(), Key::Char('f').ctrl(), Key::Char('b').alt(), Key::Char('f').alt(), Key::Char('B').alt(), Key::Char('F').alt(),
            Key::Home.any_mods(), Key::End.any_mods(), Key::Char('a').ctrl(), Key::Char('e').ctrl(), Key::Char('A').ctrl(), Key::Char('E').ctrl(),
            Key::Char('<').alt(), Key::Char('>').alt(),
            Key::Backspace.any_mods(), Key::Delete.any_mods(), Key::Char('d').ctrl(), Key::Char('d').alt(), Key::Char('w').ctrl(),
            Key::Char('k').ctrl(), Key::Char('u').ctrl(),
        ]);
        if self.capture_return_key {
            imgui.cur_mut().capture_keys.push(Key::Char('\n').plain());
        }
        if self.capture_vertical_movement_keys {
            imgui.cur_mut().capture_keys.extend_from_slice(&[
                Key::Up.plain(), Key::Up.shift(), Key::Down.plain(), Key::Down.shift(), Key::Char('n').ctrl(), Key::Char('p').ctrl(),
                Key::PageUp.plain(), Key::PageUp.shift(), Key::PageDown.plain(), Key::PageDown.shift(),
                // The ctrl+v is a collision between page-down (emacs style) and paste (in text_input_key_to_action by default). The text_input_key_to_action takes precedence, but the user may unbind it and get the emacs-like behavior.
                // (Ctrl+shift+v is unrepresentable in ansi escape codes.)
                Key::Char('v').ctrl(), Key::Char('v').alt(), Key::Char('V').alt(),
            ]);
        }
        let req: Vec<KeyEx> = imgui.key_binds.text_input_key_to_action.keys().copied().collect();
        imgui.cur_mut().capture_keys.extend_from_slice(&req);

        let mut keys = mem::take(&mut imgui.cur_mut().keys);
        keys.retain(|key| {
            let mut moved = true; // scroll to cursor
            let mut edited = false; // add undo entry
            let saved_x = mem::take(&mut self.saved_x);
            let old_cursor = self.cursor;
            match imgui.key_binds.text_input_key_to_action.get(key) {
                Some(KeyAction::Undo) => {
                    if self.undo_idx > 0 {
                        self.cursor = self.undo[self.undo_idx].1;
                        self.undo_idx -= 1;
                        self.text = self.undo[self.undo_idx].0.clone();
                        self.mark = self.cursor;
                    } else {
                        moved = false;
                    }
                }
                Some(KeyAction::Redo) => {
                    if self.undo_idx + 1 < self.undo.len() {
                        self.undo_idx += 1;
                        (self.text, _, self.cursor) = self.undo[self.undo_idx].clone();
                        self.mark = self.cursor;
                    } else {
                        moved = false;
                    }
                }
                Some(KeyAction::Copy) => {
                    let r = self.selection();
                    if !r.is_empty() {
                        imgui.clipboard = self.text[r].to_string();
                    }
                    moved = false;
                }
                Some(KeyAction::Cut) => {
                    if !self.selection().is_empty() {
                        self.delete_selection(true, imgui);
                        edited = true;
                    }
                }
                Some(KeyAction::Paste) => {
                    if !imgui.clipboard.is_empty() {
                        self.delete_selection(false, imgui);
                        self.text.insert_str(self.cursor, &imgui.clipboard);
                        self.cursor += imgui.clipboard.len();
                        self.mark = self.cursor;
                        edited = true;
                    }
                }
                Some(KeyAction::NewLine) if self.multiline => {
                    self.delete_selection(false, imgui);
                    self.text.insert(self.cursor, '\n');
                    self.move_cursor(false, true);
                    self.mark = self.cursor;
                    edited = true;
                }
                _ => match key.key {
                    Key::Char(c) if (key.mods.is_empty() || c == '\n') && (c != '\n' || self.multiline) => {
                        self.delete_selection(false, imgui);
                        self.text.insert(self.cursor, c);
                        self.move_cursor(false, true);
                        self.mark = self.cursor;
                        edited = true;
                    }
                    Key::Left | Key::Right | Key::Char('b') | Key::Char('f') | Key::Char('B') | Key::Char('F') => {
                        let select = key.mods.contains(ModKeys::SHIFT) || key.key == Key::Char('B') || key.key == Key::Char('F');
                        let arrow = key.key == Key::Left || key.key == Key::Right;
                        let word = key.mods.contains(ModKeys::ALT) || (arrow && key.mods.contains(ModKeys::CTRL));
                        let right = key.key == Key::Right || key.key == Key::Char('f') || key.key == Key::Char('F');
                        self.move_cursor(word, right);
                        if !select {
                            self.mark = self.cursor;
                        }
                    }
                    Key::Home | Key::End | Key::Char('a') | Key::Char('e') | Key::Char('A') | Key::Char('E') | Key::Char('<') | Key::Char('>') => {
                        let select = key.mods.contains(ModKeys::SHIFT) || key.key == Key::Char('A') || key.key == Key::Char('E');
                        let whole_text = key.key == Key::Char('<') || key.key == Key::Char('>') || (key.mods.contains(ModKeys::CTRL) && (key.key == Key::Home || key.key == Key::End));
                        let home = key.key == Key::Home || key.key == Key::Char('a') || key.key == Key::Char('A') || key.key == Key::Char('<');

                        self.cursor = match (whole_text, home) {
                            (true, true) => 0,
                            (true, false) => self.text.len(),
                            (false, home) => {
                                let mut pos = self.offset_to_coordinates(self.cursor);
                                pos[0] = if home {0} else {isize::MAX};
                                self.coordinates_to_offset(pos)
                            }
                        };
                        if !select {
                            self.mark = self.cursor;
                        }
                    }
                    Key::Backspace | Key::Delete | Key::Char('d') | Key::Char('w') => {
                        let to_clipboard = key.mods.contains(ModKeys::ALT) || key == &Key::Char('w').ctrl(); // backspace/delete/ctrl+backspace/ctrl+delete don't put the string into clipboard, while more emacs-like alt+backspace/alt+d/etc do
                        if self.cursor == self.mark {
                            let word = key.mods.contains(ModKeys::ALT) || (key.mods.contains(ModKeys::CTRL) && key.key != Key::Char('d'));
                            let right = key.key == Key::Delete || key.key == Key::Char('d');
                            self.move_cursor(word, right);
                        }
                        self.delete_selection(to_clipboard, imgui);
                        edited = true;
                    }
                    Key::Char('k') | Key::Char('u') if key.mods == ModKeys::CTRL => {
                        self.mark = self.cursor;
                        let mut pos = self.offset_to_coordinates(self.cursor);
                        pos[0] = if key.key == Key::Char('k') {isize::MAX} else {0};
                        self.cursor = self.coordinates_to_offset(pos);
                        self.delete_selection(true, imgui);
                        edited = true;
                    }
                    Key::Up | Key::Down | Key::Char('n') | Key::Char('p') | Key::PageUp | Key::PageDown | Key::Char('v') | Key::Char('V') => {
                        let select = key.mods.contains(ModKeys::SHIFT);
                        let dy = match key.key {
                            Key::Up | Key::Char('p') => -1,
                            Key::Down | Key::Char('n') => 1,
                            k if k == Key::PageUp || (k == Key::Char('v') && key.mods.contains(ModKeys::ALT)) => -(self.viewport_height.saturating_sub(2) as isize),
                            _ => self.viewport_height.saturating_sub(2) as isize,
                        };

                        let mut pos = self.offset_to_coordinates(self.cursor);

                        pos[0] = saved_x.unwrap_or(pos[0]);
                        self.saved_x = Some(pos[0]);

                        pos[1] += dy;
                        self.cursor = self.coordinates_to_offset(pos);
                        if !select {
                            self.mark = self.cursor;
                        }
                    }
                    _ => return true,
                }
            }
            if moved {
                self.scroll_to_cursor = true;
            }
            if edited {
                self.undo_idx += 1;
                self.undo.truncate(self.undo_idx);
                self.undo.push((self.text.clone(), old_cursor, self.cursor));
            }
            false
        });
        imgui.cur_mut().keys = keys;

        if self.undo.len() > 1000 {
            let n = (self.undo.len() - 500).min(self.undo_idx);
            self.undo.drain(..n);
            self.undo_idx -= n;
        }
    }

    fn selection(&self) -> Range<usize> {
        self.mark.min(self.cursor)..self.mark.max(self.cursor)
    }
        
    fn move_cursor(&mut self, word: bool, right: bool) {
        match (word, right) {
            (false, false) if self.cursor > 0 => self.cursor = self.text[..self.cursor].char_indices().rev().next().unwrap().0,
            (false, true) if self.cursor == self.text.len() => self.moved_past_the_end = true,
            (false, true) => self.cursor = self.text[self.cursor..].char_indices().nth(1).map_or(self.text.len(), |t| self.cursor + t.0),
            (true, false) => {self.text[..self.cursor].char_indices().rev().skip_while(|t| !Self::is_word_character(t.1)).skip_while(|t| if Self::is_word_character(t.1) { self.cursor = t.0; true } else { false }).next();}
            (true, true) => self.cursor = self.text[self.cursor..].char_indices().skip_while(|t| !Self::is_word_character(t.1)).skip_while(|t| Self::is_word_character(t.1)).next().map_or(self.text.len(), |t| self.cursor + t.0),
            _ => (),
        }
    }

    fn is_word_character(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
            _ => false }
    }

    fn delete_selection(&mut self, to_clipboard: bool, imgui: &mut IMGUI) {
        let r = self.selection();
        if to_clipboard {
            imgui.clipboard = self.text[r.clone()].to_string();
        }
        self.text.replace_range(r.clone(), "");
        self.cursor = r.start;
        self.mark = self.cursor;
    }

    fn offset_to_coordinates(&self, offset: usize) -> [isize; 2] {
        if self.lines.is_empty() {
            return [0, 0];
        }
        let i = self.lines.partition_point(|(_, r)| r.end <= offset).min(self.lines.len() - 1);
        let (start_x, range) = self.lines[i].clone();
        let w = str_width(&self.text[range.start..offset]);
        [start_x + w as isize, i as isize]
    }

    fn coordinates_to_offset(&self, pos: [isize; 2]) -> usize {
        if pos[1] < 0 {
            return 0;
        }
        if pos[1] >= self.lines.len() as isize {
            return self.text.len();
        }
        let i = pos[1].max(0).min(self.lines.len() as isize - 1) as usize;
        let (start_x, mut range) = self.lines[i].clone();
        if i + 1 < self.lines.len() {
            // Exclude the '\n'.
            range.end -= 1;
        }
        let (j, _) = str_prefix_with_width(&self.text[range.clone()], (pos[0] - start_x).max(0) as usize);
        range.start + j
    }

    fn render(&mut self, content_widget: WidgetIdx, scroll_bar: WidgetIdx, imgui: &mut IMGUI) {
        let selection = self.selection();
        styled_write!(imgui.text, imgui.palette.text_input, "{}", &self.text[..selection.start]);
        styled_write!(imgui.text, imgui.palette.text_input_selected, "{}", &self.text[selection.clone()]);
        styled_write!(imgui.text, imgui.palette.text_input, "{}", &self.text[selection.end..]);
        let l = imgui.text.close_line();

        let mut line_map: Vec<Range<usize>> = Vec::new();
        let mut line_range = if self.multiline {
            imgui.text.split_by_newline_character(l, Some(&mut line_map))
        } else {
            line_map.push(0..self.text.len());
            l..l+1
        };

        imgui.get_mut(content_widget).draw_text = Some(line_range.clone());
        let mut content_width = imgui.calculate_size(content_widget, 0);
        let viewport_width = imgui.calculate_size(imgui.cur_parent, 0);

        self.lines.clear();
        if self.line_wrap && content_width > viewport_width {
            let mut wrap_map: Vec<(isize, usize, Range<usize>)> = Vec::new();
            line_range = imgui.text.line_wrap(line_range, viewport_width, 10000, &imgui.palette.line_wrap_indicator, &imgui.palette.truncation_indicator, Some(&mut wrap_map));
            for (start_x, line_idx, r) in wrap_map {
                self.lines.push((start_x, r.start + line_map[line_idx].start .. r.end + line_map[line_idx].start));
            }
            imgui.get_mut(content_widget).draw_text = Some(line_range);
            imgui.get_mut(content_widget).axes[Axis::X].reset_size();
            content_width = imgui.calculate_size(content_widget, 0);
        } else {
            for r in line_map {
                self.lines.push((0, r));
            }
        }

        self.viewport_height = imgui.calculate_size(imgui.cur_parent, Axis::Y);
        if scroll_bar.is_valid() {
            imgui.get_mut(scroll_bar).axes[Axis::Y].size = self.viewport_height;
        }
        
        let cursor_pos = self.offset_to_coordinates(self.cursor);
        imgui.get_mut(content_widget).draw_cursor_if_focused = Some(cursor_pos);

        // Vertical and horizontal scrolling.
        let scroll_wheel_widget = content_widget; // same widget where MouseEvent::DRAG is captured, to make scrolling work while dragging
        if self.multiline {
            let scroll_to = if self.scroll_to_cursor {
                Some(cursor_pos[1]..cursor_pos[1]+1)
            } else {
                None
            };
            scrolling_navigation(&mut self.scroll, scroll_to, scroll_wheel_widget, scroll_bar, imgui);
        } else {
            with_parent!(imgui, scroll_wheel_widget, {
                self.hscroll += imgui.check_scroll() * imgui.key_binds.hscroll_sensitivity;
            });
        }
        if self.scroll_to_cursor {
            scroll_to_range(&mut self.hscroll, cursor_pos[0]..cursor_pos[0], viewport_width);
        }
        self.hscroll = self.hscroll.min(content_width as isize - viewport_width as isize).max(0);
        imgui.get_mut(content_widget).axes[Axis::X].rel_pos = -self.hscroll;
    }
}


// ============================================== mostly toy code, some ui.rs =================================================================


#[derive(Default)]
struct State {
    quit: bool,
    drop_caches: bool,
    
    profiling_message: Vec<String>,
    selected_window: usize,

    binaries_table_state: TableState,
    threads_table_state: TableState,
    disassembly_tabs_state: TabsState,
    // (irl these would be part of the tab state instead)
    disassembly_cursor: usize,
    disassembly_scroll: isize,
    disassembly_hscroll: isize,
    open_function_text: TextInput,
    open_function_table_state: TableState,

    watches: WatchesWindow,
}

fn build(imgui: &mut IMGUI, state: &mut State) {
    (state.quit, state.drop_caches) = (false, false);
    let w = imgui.cur_mut();
    w.axes[Axis::X].flags.insert(AxisFlags::STACK);

    let mut windows: Vec<WidgetIdx> = Vec::new();

    let columns = [
        imgui.add(widget!().width(AutoSize::Remainder(0.3)).vstack()),
        imgui.add(widget!().fill('|', imgui.palette.default_dim).fixed_width(1)),
        imgui.add(widget!().width(AutoSize::Remainder(0.5)).vstack()),
        imgui.add(widget!().fill('|', imgui.palette.default_dim).fixed_width(1)),
        imgui.add(widget!().width(AutoSize::Remainder(0.2)).vstack()),
    ];
    imgui.layout_children(Axis::X);

    // Left column.
    with_parent!(imgui, columns[0], {
        styled_write!(imgui.text, imgui.palette.default_dim, "cheat sheet");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Children)));

        styled_write!(imgui.text, imgui.palette.default_dim, "status");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().fixed_height(6).vstack()));

        styled_write!(imgui.text, imgui.palette.default_dim, "watches ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "1");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(1.0))));
    });
    // Middle column.
    with_parent!(imgui, columns[2], {
        styled_write!(imgui.text, imgui.palette.default_dim, "disassembly ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "2");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.6))));

        styled_write!(imgui.text, imgui.palette.default_dim, "code ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "3");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.4)).vstack()));
        imgui.layout_children(Axis::Y);
    });
    // Right column.
    with_parent!(imgui, columns[4], {
        styled_write!(imgui.text, imgui.palette.default_dim, "binaries ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "4");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.2))));

        styled_write!(imgui.text, imgui.palette.default_dim, "breakpoints ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "7");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.3))));

        styled_write!(imgui.text, imgui.palette.default_dim, "stack ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "5");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.3))));

        styled_write!(imgui.text, imgui.palette.default_dim, "threads ");
        styled_write!(imgui.text, imgui.palette.default_dim.add_modifier(Modifier::UNDERLINED), "6");
        let l = imgui.text.close_line();
        imgui.add(widget!().fill('-', imgui.palette.default_dim).text(l).fixed_height(1));

        windows.push(imgui.add(widget!().height(AutoSize::Remainder(0.2))));
        imgui.layout_children(Axis::Y);
    });

    let hotkey_to_window = [2, 3, 4, 5, 7, 8, 6];
    for action in imgui.check_keys(&[KeyAction::Quit, KeyAction::DropCaches, KeyAction::Window(1), KeyAction::Window(2), KeyAction::Window(3), KeyAction::Window(4), KeyAction::Window(5), KeyAction::Window(6), KeyAction::Window(7)]) {
        match action {
            KeyAction::Quit => {state.quit = true; return;}
            KeyAction::DropCaches => state.drop_caches = true,
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
        w.axes[Axis::Y].auto_size = AutoSize::Text;
    });
    // Status window.
    with_parent!(imgui, windows[1], {
        with_parent!(imgui, imgui.add(widget!().height(AutoSize::Text)), {
            let style = Style {fg: Color(0, 0, 0), bg: Color(100, 100, 200), ..D!()};
            imgui.cur_mut().draw_fill = Some((' ', style));
            styled_write!(imgui.text, style, "status goes here");
            imgui.set_text();
        });

        with_parent!(imgui, imgui.add(widget!()), {
            styled_write!(imgui.text, imgui.palette.default, "hello world");
            imgui.set_text();
        });
    });

    with_parent!(imgui, columns[0], {
        imgui.layout_children(Axis::Y);
    });

    with_parent!(imgui, windows[2], {
        state.watches.build(imgui);
    });
    with_parent!(imgui, windows[3], {
        build_disassembly(imgui, state);
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
}


struct ValueTreeNode {
    name: Range<usize>,
    value: usize,
    identity: usize,
    children: Option<Range<usize>>,
    expandable: bool,
    depth: usize,
    parent: usize,
}
impl Default for ValueTreeNode { fn default() -> Self { Self {name: 0..0, value: usize::MAX, identity: 0, children: None, expandable: false, depth: 0, parent: usize::MAX} } }

#[derive(Clone)]
struct ValueTreeRow {
    node_idx: usize,
    expanded: bool,

    widget: WidgetIdx, // invalid if we didn't create it yet
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
    roots: Vec<(usize, String)>,
    text: StyledText,
    rows: Vec<ValueTreeRow>,
    expanded_nodes: HashSet<usize>,
}

#[derive(Default)]
struct WatchesWindow {
    tree: ValueTree,

    name_width: usize,
    value_width: usize,
    row_height_limit: usize,

    cursor_path: Vec<usize>,
    cursor_idx: usize,
    scroll: isize,
    scroll_to_cursor: bool,

    text_input: Option<(/*identity*/ usize, TextInput)>,
    text_input_built: bool,
}
impl WatchesWindow {
    fn build(&mut self, imgui: &mut IMGUI) {
        // Plan:
        //  1. Handle expand/collapse/edit-start/edit-stop inputs using last frame rows and tree.
        //  2. Recalculate the tree if needed.
        //  3. Build widget tree and rows list.
        //     For nodes that affect layout of other nodes (expanded nodes, text input node) the widgets are created immediately.
        //     For other nodes, widget creation is deferred to a later stage, after layout, to avoid building invisible nodes.
        //     Locate cursor (map cursor_path to cursor_idx). Build TextInput, close it on focus loss.
        //  4. layout_subtree(). Now we know the heights and coordinates of all rows.
        //  5. Cursor navigation.
        //  6. Scrolling navigation. Now we know which rows are visible.
        //  7. Build the visible rows and the selected row.
        //  8. Focus and highlight selected row.

        // Create outer widgets.
        assert!(imgui.cur().axes[0].flags.contains(AxisFlags::SIZE_KNOWN) && imgui.cur().axes[1].flags.contains(AxisFlags::SIZE_KNOWN));
        imgui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);
        let root_widget = imgui.cur_parent;
        let header_widget = imgui.add(widget!().fixed_height(1).hstack());
        let container_widget = imgui.add(widget!().height(AutoSize::Remainder(1.0)).hstack());
        imgui.layout_children(Axis::Y);
        let (viewport_widget, scroll_bar, content_widget);
        with_parent!(imgui, container_widget, {
            viewport_widget = imgui.add(widget!().width(AutoSize::Remainder(1.0)));
            scroll_bar = imgui.add(widget!().fixed_width(1));
            imgui.layout_children(Axis::X);
        });
        let width = imgui.calculate_size(viewport_widget, Axis::X);
        self.name_width = width * 4 / 10;
        self.value_width = width.saturating_sub(self.name_width + 1);
        with_parent!(imgui, viewport_widget, {
            imgui.focus();
            content_widget = imgui.add(widget!().height(AutoSize::Children).vstack());
            self.row_height_limit = imgui.cur().axes[Axis::Y].size;
        });
        with_parent!(imgui, header_widget, {
            styled_write!(imgui.text, imgui.palette.table_header, "  name");
            let l = imgui.text.close_line();
            imgui.add(widget!().fixed_width(self.name_width + 1).text(l));
            styled_write!(imgui.text, imgui.palette.table_header, "value");
            let l = imgui.text.close_line();
            imgui.add(widget!().fixed_width(self.value_width).text(l));
        });

        // Handle input that may change the tree and doesn't need to know the layout.
        for action in imgui.check_keys(&[KeyAction::CursorLeft, KeyAction::CursorRight, KeyAction::Enter, KeyAction::DeleteRow, KeyAction::DuplicateRow, KeyAction::Cancel]) {
            if action == KeyAction::DuplicateRow {
                let (node, expr) = Self::random_node(&mut self.tree.text, imgui);
                self.tree.nodes.push(node);
                self.tree.roots.push((self.tree.nodes.len() - 1, expr));
                continue;
            }
            let idx = match self.tree.rows.get(self.cursor_idx) {
                None => continue,
                Some(x) => x.node_idx };
            let node = &mut self.tree.nodes[idx];
            match action {
                KeyAction::CursorLeft if self.text_input.is_none() => {
                    self.tree.expanded_nodes.remove(&node.identity);
                    self.scroll_to_cursor = true;
                }
                KeyAction::CursorRight if self.text_input.is_none() => {
                    self.tree.expanded_nodes.insert(node.identity);
                    self.scroll_to_cursor = true;
                }
                KeyAction::Enter if node.depth == 0 => {
                    if let Some(i) = self.tree.roots.iter().position(|(ix, _)| *ix == idx) {
                        if let Some((identity, input)) = &mut self.text_input {
                            if node.identity == *identity {
                                self.tree.roots[i].1 = input.text.clone();
                                node.name = Self::format_name(&mut self.tree.text, &input.text, imgui);
                            }
                            self.text_input = None;
                        } else {
                            self.text_input = Some((node.identity, TextInput::new_multiline(self.tree.roots[i].1.clone())));
                        }
                        self.scroll_to_cursor = true;
                    }
                }
                KeyAction::Cancel if self.text_input.is_some() => {
                    self.text_input = None;
                    self.scroll_to_cursor = true;
                }
                KeyAction::DeleteRow if self.text_input.is_none() && node.depth == 0 => {
                    self.tree.roots.retain(|(ix, _)| *ix != idx);
                    self.scroll_to_cursor = true;
                }
                _ => (),
            }
        }

        // (irl reevaluate the tree here)

        // Build the list of rows and most of the widget tree.
        with_parent!(imgui, content_widget, {
            self.build_tree(imgui);
        });

        imgui.layout_subtree(content_widget, Axis::Y);

        let visible_rows;
        with_parent!(imgui, viewport_widget, {
            let get_row_height = |row: &ValueTreeRow, imgui: &mut IMGUI| -> usize {
                if !row.widget.is_valid() {
                    return 1;
                }
                let w = imgui.get(row.widget);
                assert!(w.axes[Axis::Y].flags.contains(AxisFlags::SIZE_KNOWN));
                w.axes[Axis::Y].size
            };
            
            // Move the cursor.
            self.scroll_to_cursor |= list_cursor_navigation_with_variable_row_height(&mut self.cursor_idx, self.tree.rows.len(), |row_idx, imgui| get_row_height(&self.tree.rows[row_idx], imgui), imgui);

            let get_row_y_range = |row: &ValueTreeRow, imgui: &mut IMGUI| -> Range<isize> {
                let start = imgui.relative_position(content_widget, row.parent_widget, Axis::Y) + row.rel_y;
                let height = get_row_height(row, imgui);
                start..start+height as isize
            };

            // Handle scrolling.
            let mut scroll_to: Option<Range<isize>> = None;
            if mem::take(&mut self.scroll_to_cursor) && !self.tree.rows.is_empty() {
                scroll_to = Some(get_row_y_range(&self.tree.rows[self.cursor_idx], imgui));
            }
            let visible_y = scrolling_navigation(&mut self.scroll, scroll_to, root_widget, scroll_bar, imgui);

            visible_rows =
                self.tree.rows.partition_point(|row| get_row_y_range(row, imgui).end <= visible_y.start) ..
                self.tree.rows.partition_point(|row| get_row_y_range(row, imgui).start < visible_y.end);
        });

        for row_idx in visible_rows {
            if !self.tree.rows[row_idx].widget.is_valid() {
                self.build_row(row_idx, None, imgui);
            }
        }
        if !self.tree.rows.is_empty() {
            if !self.tree.rows[self.cursor_idx].widget.is_valid() {
                self.build_row(self.cursor_idx, None, imgui);
            }
            with_parent!(imgui, self.tree.rows[self.cursor_idx].widget, {
                imgui.focus();
                let a = imgui.palette.selected;
                imgui.cur_mut().style_adjustment.update(a);
            });
        }

        if !mem::take(&mut self.text_input_built) && self.text_input.is_some() {
            self.text_input = None;
            imgui.should_redraw = true;
        }

        // Convert cursor_idx to cursor_path to make it robust to changing the tree.
        // Do it late because build_row() may change cursor_idx (on click).
        self.cursor_path.clear();
        if !self.tree.rows.is_empty() {
            let mut node_idx = self.tree.rows[self.cursor_idx].node_idx;
            while node_idx != usize::MAX {
                let node = &self.tree.nodes[node_idx];
                self.cursor_path.push(node.identity);
                node_idx = node.parent;
            }
            self.cursor_path.reverse();
        }
    }

    fn build_tree(&mut self, imgui: &mut IMGUI) {
        self.tree.rows.clear();

        let mut stack: Vec<ValueTreeRow> = Vec::new();
        let roots = mem::take(&mut self.tree.roots);
        self.prebuild_children(0..roots.len(), Some(&roots), &mut stack, imgui);
        self.tree.roots = roots;

        let mut clicked_row: Option<usize> = None;
        while let Some(row) = stack.pop() {
            self.tree.rows.push(row.clone());
            let row_idx = self.tree.rows.len() - 1;

            let node = &self.tree.nodes[row.node_idx];

            if self.cursor_path.get(node.depth) == Some(&node.identity) {
                self.cursor_idx = row_idx;
            }

            if row.deferred {
                continue;
            }

            if row.expanded && node.expandable {
                let subtree_content;
                with_parent!(imgui, row.parent_widget, {
                    let indent_limit = self.name_width / 2;
                    let mut offset = 0;
                    if node.depth < indent_limit {
                        offset = 1;
                        with_parent!(imgui, imgui.add(widget!().identity(&('e', node.identity)).fixed_width(1).vstack().highlight_on_hover()), {
                            if imgui.check_mouse(MouseActions::CLICK) {
                                self.tree.expanded_nodes.remove(&node.identity);
                                imgui.should_redraw = true;
                            }
                            imgui.add(widget!().fixed_height(1).fill('▾', imgui.palette.default));
                            let bar = if node.depth + 1 == indent_limit {'┼'} else {'┆'};
                            imgui.add(widget!().fill(bar, imgui.palette.tree_indent));
                        });
                    }

                    assert!(imgui.cur().axes[Axis::X].flags.contains(AxisFlags::SIZE_KNOWN));
                    let remaining_width = imgui.cur().axes[Axis::X].size.saturating_sub(offset);
                    subtree_content = imgui.add(widget!().identity(&('s', node.identity)).fixed_width(remaining_width).fixed_x(offset as isize).height(AutoSize::Children).vstack());

                    let row = &mut self.tree.rows[row_idx];
                    row.parent_widget = subtree_content;
                    row.indent_built = true;
                });

                let mut clicked = false;
                self.build_row(row_idx, Some(&mut clicked), imgui);
                if clicked {
                    clicked_row = Some(row_idx);
                }
                let node = &self.tree.nodes[row.node_idx];

                if node.children.is_none() {
                    let start = self.tree.nodes.len();
                    let depth = node.depth;
                    while random::<u8>() < 200 {
                        let (mut n, _) = Self::random_node(&mut self.tree.text, imgui);
                        n.depth = depth + 1;
                        n.parent = row.node_idx;
                        self.tree.nodes.push(n);
                    }
                    let end = self.tree.nodes.len();
                    self.tree.nodes[row.node_idx].children = Some(start..end);
                }
                let children = self.tree.nodes[row.node_idx].children.clone().unwrap();
                with_parent!(imgui, subtree_content, {
                    self.prebuild_children(children, None, &mut stack, imgui);
                });
            } else {
                // Line wrapping but no children.
                let mut clicked = false;
                self.build_row(row_idx, Some(&mut clicked), imgui);
                if clicked {
                    clicked_row = Some(row_idx);
                }
            }
        }
        if let Some(i) = clicked_row {
            self.cursor_idx = i;
        }
    }

    // Detect runs of consecutive simple-layout nodes (unexpanded, no text input). Create parent widgets for them, to be filled out later if visible. Push all nodes to the stack to be visited by DFS.
    fn prebuild_children(&mut self, range: Range<usize>, nodes_list: Option<&Vec<(usize, String)>>, stack: &mut Vec<ValueTreeRow>, imgui: &mut IMGUI) {
        let stack_start = stack.len();
        let mut run: Option<(WidgetIdx, usize)> = None;
        let end_run = |run: Option<(WidgetIdx, usize)>, imgui: &mut IMGUI| {
            if let Some((w, n)) = run {
                imgui.get_mut(w).axes[Axis::Y].size = n;
            }
        };
        for idx0 in range {
            let node_idx = if let Some(v) = nodes_list.clone() {
                v[idx0].0
            } else {
                idx0
            };

            let node = &self.tree.nodes[node_idx];
            let expanded = self.tree.expanded_nodes.contains(&node.identity);
            let is_text_input = self.text_input.as_ref().is_some_and(|(identity, _)| *identity == node.identity);
            let deferred = !expanded && !is_text_input && node.name.len() <= 1;

            let mut row = ValueTreeRow {node_idx, expanded, widget: WidgetIdx::invalid(), parent_widget: WidgetIdx::invalid(), rel_y: 0, indent_built: false, deferred};

            if deferred {
                let (w, n) = run.get_or_insert_with(|| {
                    (imgui.add(widget!().flags(WidgetFlags::SKIP_IDENTITY).fixed_height(0)), 0)
                });
                row.parent_widget = *w;
                row.rel_y = *n as isize;
                *n += 1;
            } else {
                end_run(mem::take(&mut run), imgui);
                row.parent_widget = imgui.add(widget!().flags(WidgetFlags::SKIP_IDENTITY).height(AutoSize::Children));
            }

            stack.push(row);
        }
        end_run(run, imgui);
        stack[stack_start..].reverse();
    }

    fn build_row(&mut self, row_idx: usize, mut out_clicked: Option<&mut bool>, imgui: &mut IMGUI) {
        let row = &self.tree.rows[row_idx];
        let node = &self.tree.nodes[row.node_idx];
        let row_widget;

        // Draw '▸' if needed and create row widget.
        with_parent!(imgui, row.parent_widget, {
            let mut offset = 1;
            if !row.indent_built {
                offset = 2;
                if node.expandable {
                    with_parent!(imgui, imgui.add(widget!().identity(&('e', node.identity)).fixed_width(1).fixed_height(1).fixed_y(row.rel_y).fill('▸', imgui.palette.default).highlight_on_hover()), {
                        if imgui.check_mouse(MouseActions::CLICK) {
                            self.tree.expanded_nodes.insert(node.identity);
                            imgui.should_redraw = true;
                        }
                    });
                }
            }
            assert!(imgui.cur().axes[Axis::X].flags.contains(AxisFlags::SIZE_KNOWN));
            let remaining_width = imgui.cur().axes[Axis::X].size.saturating_sub(offset);
            row_widget = imgui.add(widget!().identity(&node.identity).fixed_x(offset as isize).fixed_width(remaining_width).height(AutoSize::Children).fixed_y(row.rel_y).fill(' ', imgui.palette.default).highlight_on_hover());
        });

        // Draw name and value, update text input if needed.
        with_parent!(imgui, row_widget, {
            if imgui.check_mouse(MouseActions::CLICK) {
                if let Some(c) = &mut out_clicked {
                    **c = true;
                } else {
                    self.cursor_idx = row_idx;
                    imgui.should_redraw = true;
                }
                self.scroll_to_cursor = true;
            }

            let name_width = imgui.cur().axes[Axis::X].size.saturating_sub(self.value_width + 1);
            let flags = if row.expanded {WidgetFlags::LINE_WRAP} else {WidgetFlags::empty()};

            // Add value first so that name text input can obstruct it if it grows wide.
            let lines = imgui.text.import_lines(&self.tree.text, node.value..node.value+1);
            imgui.add(widget!().fixed_width(self.value_width).fixed_x(name_width as isize + 1).height(AutoSize::Text).max_height(self.row_height_limit).flags(flags).text_lines(lines));

            if self.text_input.as_ref().is_some_and(|(identity, _)| *identity == node.identity) {
                let focused = imgui.check_focus();
                with_parent!(imgui, imgui.add(widget!().identity(&"input").width(AutoSize::Children).min_width(name_width).max_width(name_width + self.value_width + 1).height(AutoSize::Children).max_height(self.row_height_limit)), {
                    imgui.relative_focus(row_widget);
                    if focused {
                        let input = &mut self.text_input.as_mut().unwrap().1;
                        self.scroll_to_cursor |= input.build(imgui);
                        self.text_input_built = true;
                    }
                });
            } else {
                let lines = imgui.text.import_lines(&self.tree.text, node.name.clone());
                assert!(lines.start <= lines.end);
                assert!(lines.end <= imgui.text.num_lines());
                imgui.add(widget!().fixed_width(name_width).height(AutoSize::Text).max_height(self.row_height_limit).flags(flags).text_lines(lines));
            }
        });

        self.tree.rows[row_idx].widget = row_widget;
    }

    fn random_node(text: &mut StyledText, imgui: &mut IMGUI) -> (ValueTreeNode, String) {
        let append_random_string = |out: &mut String| {
            let n = ((random::<f64>() * 6.).exp() as usize).saturating_sub(1);
            for _ in 0..n {
                out.push((b'a' + random::<u8>()%26) as char);
            }
        };
        let mut name_str = String::new();
        append_random_string(&mut name_str);
        append_random_string(&mut text.chars);
        text.close_span(imgui.palette.default);
        let value = text.close_line();
        let name = Self::format_name(text, &name_str, imgui);
        (ValueTreeNode {name, value, identity: random(), children: None, expandable: random::<u8>() < 200, depth: 0, parent: usize::MAX}, name_str)
    }

    fn format_name(text: &mut StyledText, name: &str, imgui: &mut IMGUI) -> Range<usize> {
        styled_write!(text, imgui.palette.default, "{}", name);
        let l = text.close_line();
        text.split_by_newline_character(l, None)
    }
}

fn build_open_function_dialog(imgui: &mut IMGUI, state: &mut State) {
    let create_dialog = imgui.check_key(KeyAction::Open);
    let dialog_root = match imgui.check_dialog(create_dialog) {
        Some(x) => x,
        None => return };
    let dialog = match make_dialog_frame(dialog_root, AutoSize::Remainder(0.75), AutoSize::Remainder(0.83), imgui.palette.dialog, imgui.palette.function, "find function (by mangled name)", imgui) {
        Some(x) => x,
        None => return };
    with_parent!(imgui, dialog, {
        imgui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);

        with_parent!(imgui, imgui.add(widget!().identity("search input").fixed_height(1)), {
            imgui.focus();
            state.open_function_text.build(imgui);
        });

        let start = imgui.text.close_line();
        styled_write!(imgui.text, imgui.palette.default, "info line");
        let end = imgui.text.close_line() + 1;
        imgui.add(widget!().height(AutoSize::Text).text_lines(start..end));

        let table_widget = imgui.add(widget!().identity("table").height(AutoSize::Remainder(1.0)));
        imgui.layout_children(Axis::Y);
        with_parent!(imgui, table_widget, {
            imgui.multifocus();
            let mut table = Table::new(mem::take(&mut state.open_function_table_state), imgui, vec![Column::new("", AutoSize::Remainder(1.0))]);
            for i in 0..300 {
                table.start_row(i, imgui);
                styled_write!(imgui.text, imgui.palette.function, "row {}", i);
                table.text_cell(imgui);
            }
            state.open_function_table_state = table.finish(imgui);
        });
    });
}

fn build_disassembly(imgui: &mut IMGUI, state: &mut State) {
    build_open_function_dialog(imgui, state);

    imgui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);
    let tabs_widget = imgui.add(widget!().fixed_height(1));
    let body_widget = imgui.add(widget!().height(AutoSize::Remainder(1.0)).hstack());
    imgui.layout_children(Axis::Y);

    with_parent!(imgui, tabs_widget, {
        imgui.focus();
        let mut tabs = Tabs::new(mem::take(&mut state.disassembly_tabs_state), imgui);
        for _ in 0..10 {
            tabs.add("long long title A::", "foo", false, imgui);
            tabs.add("long long title B::", "foo", true, imgui);
            tabs.add("hello world", "bar long long long long title blah blah blah blah blah blah", false, imgui);
        }
        state.disassembly_tabs_state = tabs.finish(imgui);
    });

    let funcs = ["A::foo", "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum", "bar"];
    let lines = ["hello", "world", "lalala", "blah blah", "relatively long line here Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"];
    let num_lines = [(100, 5), (1000, 4), (10, 5)];

    let tab_idx = state.disassembly_tabs_state.selected;
    let func = funcs[tab_idx%funcs.len()];
    let (num_lines, lines_mod) = num_lines[tab_idx%num_lines.len()];
    let widest_line = lines[..lines_mod].iter().map(|s| str_width(s)).max().unwrap().max(str_width(func));

    let (h_viewport, scroll_bar);
    with_parent!(imgui, body_widget, {
        imgui.multifocus();
        h_viewport = imgui.add(widget!().width(AutoSize::Remainder(1.0)));
        scroll_bar = imgui.add(widget!().fixed_width(1));
        imgui.layout_children(Axis::X);
    });
    with_parent!(imgui, h_viewport, {
        let h_content = imgui.add(widget!().identity(&(func, tab_idx)).fixed_width(widest_line).vstack());
        with_parent!(imgui, h_content, {
            styled_write!(imgui.text, imgui.palette.function, "{}", func);
            let l = imgui.text.close_line();
            with_parent!(imgui, imgui.add(widget!().fixed_height(1).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS)), {
                imgui.add(widget!().width(AutoSize::Text).text(l));
            });

            let v_viewport = imgui.add(widget!().height(AutoSize::Remainder(1.0)));
            imgui.layout_children(Axis::Y);

            with_parent!(imgui, v_viewport, {
                imgui.focus();

                let v_content = imgui.add(widget!().fixed_height(num_lines).vstack());
                let mut scroll_to_cursor = false;
                with_parent!(imgui, v_content, {
                    if imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                        let y = imgui.cur().mouse_pos[1];
                        if y >= 0 && (y as usize) < num_lines {
                            state.disassembly_cursor = y as usize;
                            scroll_to_cursor = true;
                        }
                    }
                });

                scroll_to_cursor |= list_cursor_navigation(&mut state.disassembly_cursor, num_lines, 1, imgui);
                let scroll_to = if scroll_to_cursor {Some(state.disassembly_cursor as isize..state.disassembly_cursor as isize+1)} else {None};
                let range = scrolling_navigation(&mut state.disassembly_scroll, scroll_to, body_widget, scroll_bar, imgui);

                with_parent!(imgui, v_content, {
                    for line_idx in range {
                        let line_idx = line_idx as usize;
                        if line_idx >= num_lines {
                            break;
                        }
                        styled_write!(imgui.text, imgui.palette.default, "{}", lines[line_idx % lines_mod]);
                        let l = imgui.text.close_line();
                        with_parent!(imgui, imgui.add(widget!().identity(&line_idx).fixed_height(1).fixed_y(line_idx as isize).fill(' ', imgui.palette.default).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS).highlight_on_hover()), {
                            if line_idx == state.disassembly_cursor {
                                let adj = imgui.palette.selected;
                                imgui.cur_mut().style_adjustment.update(adj);
                            }
                            imgui.add(widget!().fixed_height(1).width(AutoSize::Text).text(l));
                        });
                    }
                });
            });
        });

        hscrolling_navigation(&mut state.disassembly_hscroll, imgui);
    });
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
            w.axes[Axis::Y].flags.insert(AxisFlags::STACK);
            w.axes[Axis::Y].auto_size = AutoSize::Children;
            with_parent!(imgui, imgui.add(widget!().height(AutoSize::Text).flags(WidgetFlags::TEXT_TRUNCATION_ALIGN_RIGHT)), {
                styled_write!(imgui.text, imgui.palette.default, "{}", paths[i%paths.len()]);
                imgui.set_text();
            });
            let (err, style) = errors[i%errors.len()];
            if err == "in progress" {
                let progress = (SystemTime::UNIX_EPOCH.elapsed().unwrap().as_secs_f64() / 20.0 * (2.0 + (i as f64).sin())) % 1.0;
                let stage = progress_stages[(progress * progress_stages.len() as f64) as usize % progress_stages.len()];
                with_parent!(imgui, imgui.add(Widget {source_line: line!(), draw_progress_bar: Some((progress, imgui.palette.progress_bar)), .. D!()}.height(AutoSize::Text)), {
                    styled_write!(imgui.text, imgui.palette.default, "{}", stage);
                    imgui.set_text();
                });
            } else if err != "" {
                with_parent!(imgui, imgui.add(widget!().height(AutoSize::Text)), {
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
    styled_write!(imgui.text, imgui.palette.default, "📌");
    styled_write!(imgui.text, imgui.palette.default_dim, "abcdefg");
    let start = imgui.text.close_line();
    styled_write!(imgui.text, imgui.palette.default, "📌📌📌📌📌");
    styled_write!(imgui.text, imgui.palette.default_dim, "abcdefg");
    imgui.text.close_line();
    styled_write!(imgui.text, imgui.palette.default, "{}", str_width("📌"));
    let end = imgui.text.close_line()+1;
    imgui.cur_mut().draw_text = Some(start..end);
}

fn build_stack(_imgui: &mut IMGUI) {
    
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
    imgui.palette = Palette {
        default: Style {fg: Color::white(), ..D!()},
        default_dim: Style {fg: Color::white().darker(), ..D!()},
        error: Style {fg: Color(255, 50, 50), ..D!()},
        running: Style {fg: Color(70, 70, 180), ..D!()},
        suspended: Style {fg: Color(70, 180, 70), ..D!()},
        function: Style {fg: Color::white(), ..D!()},

        selected: StyleAdjustment {add_fg: (20, 20, 20), add_bg: (50, 50, 50), ..D!()},
        hovered: StyleAdjustment {add_fg: (20, 20, 20), add_bg: (25, 25, 25), ..D!()},

        table_header: Style {fg: Color::white().darker(), ..D!()},
        //striped_table: StyleAdjustment {add_fg: (0, 0, 0), add_bg: (20, 20, 20), ..D!()},
        striped_table: StyleAdjustment::default(),

        tab_title: Style {fg: Color::white().darker(), ..D!()},
        tab_title_pinned: Style {fg: Color::white(), ..D!()},
        tab_separator: (" | ".to_string(), Style {fg: Color::white().darker(), ..D!()}),

        placeholder_fill: Some(('.', Style {fg: Color::white().darker(), bg: Color::black(), ..D!()})),
        truncation_indicator: (("…".to_string(), "…".to_string(), Style {fg: Color::white().darker(), ..D!()})),
        hscroll_indicator: (("❮".to_string(), "❯".to_string(), Style {fg: Color::white().darker(), ..D!()})),
        line_wrap_indicator: (String::new(), "\\".to_string(), Style {fg: Color::white().darker(), ..D!()}),

        progress_bar: Style {fg: Color(70, 90, 200), bg: Color(20, 30, 50), ..D!()},
        scroll_bar_background: Style {fg: Color::white().darker(), ..D!()},
        scroll_bar_slider: Style {fg: Color::white().darker(), ..D!()},

        tooltip: StyleAdjustment {add_bg: (30, 40, 50), ..D!()},
        dialog: StyleAdjustment {add_fg: (10, 10, 10), add_bg: (20, 20, 20), ..D!()},

        text_input: Style {fg: Color::white(), ..D!()},
        text_input_selected: Style {fg: Color::white(), bg: Color(30, 30, 200), ..D!()},

        tree_indent: Style {fg: Color::white().darker(), ..D!()},
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
        build(&mut imgui, &mut state);
        if state.quit {
            break;
        }
        if state.drop_caches {
            terminal.clear()?;
        }

        let now = Instant::now();
        build_secs += (now - mem::replace(&mut t, now)).as_secs_f64();

        let render = imgui.end_build(&mut buffer).as_secs_f64();

        render_secs += render;
        let now = Instant::now();
        layout_secs += (now - mem::replace(&mut t, now)).as_secs_f64() - render;

        let commands = terminal.prepare_command_buffer(&buffer, imgui.show_cursor.clone());

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
//  * figure out how to do the "stack truncated" thing in stack window; just make it a normal selectable row, to allow tooltip for full error message?
//  * disassembly inline level lines - not gonna be clickable?
