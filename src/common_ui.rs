use crate::{imgui::Axis};
use std::{ops::Range, fmt::Write as fmtWrite};
use bitflags::*;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub struct Rect {
    pub pos: [isize; 2],
    pub size: [usize; 2],
}
impl Rect {
    pub fn end(&self, axis: usize) -> isize { self.pos[axis] + self.size[axis] as isize }
    
    pub fn x(&self) -> isize { self.pos[0] }
    pub fn y(&self) -> isize { self.pos[1] }
    pub fn width(&self) -> usize { self.size[0] }
    pub fn height(&self) -> usize { self.size[1] }
    pub fn right(&self) -> isize { self.end(Axis::X) }
    pub fn bottom(&self) -> isize { self.end(Axis::Y) }
    pub fn center(&self) -> [isize; 2] { [self.pos[0] + self.size[0] as isize / 2, self.pos[1] + self.size[1] as isize / 2] }
    pub fn y_range(&self) -> Range<isize> { self.pos[Axis::Y]..self.pos[Axis::Y]+self.size[Axis::Y] as isize }

    pub fn intersection(mut self, r: Rect) -> Rect {
        for axis in 0..2 {
            let p = self.pos[axis].max(r.pos[axis]);
            self.size[axis] = (self.end(axis).min(r.end(axis)) - p).max(0) as usize;
            self.pos[axis] = p;
        }
        self
    }

    pub fn is_empty(&self) -> bool { self.size[0] == 0 && self.size[1] == 0 }

    pub fn contains(&self, pos: [isize; 2]) -> bool { self.pos[0] <= pos[0] && self.end(0) > pos[0] && self.pos[1] <= pos[1] && self.end(1) > pos[1] }
}

// RGB. (We always use RGB instead of terminal palette colors because the palette is not big enough for us, and mixing palette with RGB colors would look terrible with non-default palette.)
#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub struct Color(pub u8, pub u8, pub u8);
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

// TODO: Make this general enough to allow inverting colors, use it at least for thread_breakpoint_hit and thread_crash, maybe for highlighting ip and column breakpoints in source code.
#[derive(Clone, Copy, Default)]
pub struct StyleAdjustment {
    pub add_fg: (i16, i16, i16),
    pub add_bg: (i16, i16, i16),
    pub add_modifier: Modifier,
    pub remove_modifier: Modifier,
}
impl StyleAdjustment {
    pub fn apply(self, mut s: Style) -> Style {
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

    pub fn combine(self, other: Self) -> Self {
        let add3 = |x: (i16, i16, i16), y: (i16, i16, i16)| {
            (x.0.saturating_add(y.0), x.1.saturating_add(y.1), x.2.saturating_add(y.2))
        };
        Self {add_fg: add3(self.add_fg, other.add_fg), add_bg: add3(self.add_bg, other.add_bg), add_modifier: self.add_modifier.union(other.add_modifier), remove_modifier: self.remove_modifier.union(other.remove_modifier)}
    }

    pub fn update(&mut self, other: Self) {
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

// Append a span to StyledText:
// styled_write!(out, palette.default_dim, "foo: {}, bar: {}", foo, bar);
#[macro_export]
macro_rules! styled_write {
    ($out:expr, $style:expr, $($arg:tt)*) => {{
        use std::fmt::Write as f;
        let _ = write!(($out).chars, $($arg)*);
        ($out).close_span($style);
    }};
}

#[macro_export]
macro_rules! styled_writeln {
    ($out:expr, $style:expr, $($arg:tt)*) => {{
        use std::fmt::Write as f;
        let _ = write!(($out).chars, $($arg)*);
        ($out).close_span($style);
        ($out).close_line()
    }};
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

        self.import_spans(from, from.lines[lines.start]..from.lines[lines.end]);

        self.num_lines()-lines.len() .. self.num_lines()
    }

    // Adds the spans to the unclosed line, doesn't close it.
    pub fn import_spans(&mut self, from: &StyledText, spans: Range<usize>) {
        let start = self.spans.len();
        let offset = self.chars.len().wrapping_sub(from.spans[spans.start].0);
        self.spans.extend_from_slice(&from.spans[spans.start+1..spans.end+1]);
        for (x, _) in &mut self.spans[start..] {
            *x = x.wrapping_add(offset);
        }

        let chars = from.spans[spans.start].0..from.spans[spans.end].0;
        self.chars.push_str(&from.chars[chars]);
    }

    pub fn unclose_line(&mut self) {
        assert_eq!(self.lines.last(), Some(&(self.spans.len() - 1)));
        self.lines.pop();
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

    pub fn split_by_newline_character(&mut self, line: usize, mut out_lines: Option<&mut Vec</*bytes*/ Range<usize>>>) -> /*lines*/ Range<usize> {
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
