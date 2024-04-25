use crate::{*, search::*, pool::*, symbols_registry::*, util::*, error::*, procfs::*, settings::*, context::*};
use tui::{self, layout::{Rect, Margin, Constraint}, widgets::{Paragraph, Block, Borders, Clear, Gauge, TableState, Table, Row, Cell}, style::{Style, Color}, backend::TermionBackend, text::{Span, Spans, Text}};
use termion::{event::{Event, Key}, raw::RawTerminal};
use std::{io, ops::Range, sync::Arc};

pub type Backend = TermionBackend<io::Stdout>;
pub type Frame<'a> = tui::Frame<'a, Backend>;

pub struct Scroll {
    pub scroll: usize,
    pub cursor: usize,
}

impl Scroll {
    pub fn new() -> Self {
        Self {scroll: 0, cursor: 0}
    }
    
    pub fn update(&mut self, count: usize, height: u16, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> Range<usize> {
        self.update_detect_movement(count, height, keys, key_binds).0
    }

    pub fn update_cursorless(&mut self, count: usize, height: u16, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> Range<usize> {
        let height = height as usize;
        let s = TryInto::<isize>::try_into(self.scroll).unwrap_or(isize::MAX).saturating_add(Self::delta(height, keys, key_binds));
        self.scroll = (s.max(0) as usize).min(count.saturating_sub(1));
        self.range(count, height)
    }

    // Returns true if any cursor movement keys were pressed (even if cursor wasn't moved).
    pub fn update_detect_movement(&mut self, count: usize, height: u16, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> (Range<usize>, bool) {
        let height = height as usize;
        let d = Self::delta(height, keys, key_binds);
        let c = TryInto::<isize>::try_into(self.cursor).unwrap_or(isize::MAX).saturating_add(d);
        self.cursor = (c.max(0) as usize).min(count.saturating_sub(1));
        self.clamp_scroll(count, height);
        (self.range(count, height), d != 0)
    }

    // Like update(usize::MAX), but the End key puts cursor at `count - 1` instead of some huge number.
    pub fn update_with_virtual_space(&mut self, count: usize, height: u16, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> Range<usize> {
        let height = height as usize;
        let d = Self::delta(height, keys, key_binds);
        if d == isize::MAX {
            self.cursor = count.saturating_sub(1);
        } else {
            self.cursor = TryInto::<isize>::try_into(self.cursor).unwrap_or(isize::MAX).saturating_add(d).max(0) as usize;
        }
        self.clamp_scroll(usize::MAX, height);
        self.range(usize::MAX, height)
    }

    pub fn set(&mut self, count: usize, height: u16, pos: usize) -> Range<usize> {
        let height = height as usize;
        self.cursor = pos.min(count.saturating_sub(1));
        self.clamp_scroll(count, height);
        self.range(count, height)
    }

    fn delta(height: usize, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> isize {
        let mut cur = 0isize;
        let mut res: Option<isize> = None;
        keys.retain(|key| {
            match key_binds.map.get(key) {
                Some(KeyAction::CursorUp) => cur -= 1,
                Some(KeyAction::CursorDown) => cur += 1,
                Some(KeyAction::PageUp) => cur -= height as isize,
                Some(KeyAction::PageDown) => cur += height as isize,
                Some(KeyAction::Home) => { res.get_or_insert(isize::MIN); },
                Some(KeyAction::End) => { res.get_or_insert(isize::MAX); },
                _ => return true,
            }
            false
        });
        res.unwrap_or(cur)
    }

    fn clamp_scroll(&mut self, count: usize, height: usize) {
        let margin = Self::scroll_margin(height);
        self.scroll = self.scroll.min(self.cursor.saturating_sub(margin)).max(self.cursor.saturating_sub(height.saturating_sub(margin + 1)));
        self.scroll = self.scroll.min(count.saturating_sub(height));
    }

    pub fn range(&self, count: usize, height: usize) -> Range<usize> {
        self.scroll..(self.scroll + height).min(count)
    }

    fn scroll_margin(height: usize) -> usize {
        (height / 3).min(5)
    }

    pub fn save_state(&self, out: &mut Vec<u8>) -> Result<()> {
        out.write_usize(self.scroll)?;
        out.write_usize(self.cursor)?;
        Ok(())
    }

    pub fn load_state(inp: &mut &[u8]) -> Result<Self> {
        Ok(Self {scroll: inp.read_usize()?, cursor: inp.read_usize()?})
    }
}

pub struct TextInput {
    pub text: String,
    pub cursor: usize, // byte index into `text`, must be in range and at char boundary
    pub hscroll: usize,
    // TODO: Undo.
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TextInputEvent {
    None,
    Done, // return key
    Cancel, // escape key
    Open, // right arrow key when the cursor is at the end
}

impl TextInput {
    pub fn new() -> Self { Self {text: String::new(), cursor: 0, hscroll: 0} }
    pub fn with_text(text: String) -> Self { Self {cursor: text.len(), text, hscroll: 0} }

    pub fn update(&mut self, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> TextInputEvent {
        let mut ret = TextInputEvent::None;
        keys.retain(|key| {
            if ret != TextInputEvent::None {
                return false;
            }
            match (key_binds.map.get(key).cloned(), *key) {
                (Some(KeyAction::Enter), _) => ret = TextInputEvent::Done,
                (Some(KeyAction::Cancel), _) => ret = TextInputEvent::Cancel,

                // Moving.
                (a, k) if a == Some(KeyAction::CursorLeft) || k == Key::Ctrl('b') => { self.step_back(); }
                (a, k) if a == Some(KeyAction::CursorRight) || k == Key::Ctrl('f') => if !self.step_forward() { ret = TextInputEvent::Open; }
                (_, Key::Alt('f')) => self.cursor = self.word_end(),
                (_, Key::Alt('b')) => self.cursor = self.word_start(),
                (a, k) if a == Some(KeyAction::Home) || k == Key::Ctrl('a') => self.cursor = 0,
                (a, k) if a == Some(KeyAction::End) || k == Key::Ctrl('e') => self.cursor = self.text.len(),

                // Deleting.
                (_, Key::Backspace) => {
                    if self.step_back() {
                        self.text.remove(self.cursor);
                    }
                }
                (_, Key::Delete) | (_, Key::Ctrl('d')) => {
                    if self.cursor < self.text.len() {
                        self.text.remove(self.cursor);
                    }
                }
                (_, Key::Ctrl('k')) => self.text.replace_range(self.cursor.., ""),
                (_, Key::Ctrl('u')) => {
                    self.text.replace_range(..self.cursor, "");
                    self.cursor = 0;
                }
                (_, Key::Ctrl('w')) | (_, /*alt+backspace*/ Key::Alt('\x7f')) => {
                    let w = self.word_start();
                    self.text.replace_range(w..self.cursor, "");
                    self.cursor = w;
                }
                (_, Key::Alt('d')) => self.text.replace_range(self.cursor..self.word_end(), ""),

                // Typing.
                (_, Key::Char(c)) => {
                    self.text.insert(self.cursor, c);
                    self.step_forward();
                }

                // Other things we could support:
                //  * Undo (C-/). Something for redo: maybe another key combination (e.g. M-_), maybe the emacs-style weirdness where you can undo an undo.
                //  * M-u/M-l/M-c to uppercase/lowercase/capitalize the word.
                //  * Paste (C-y), clipboard shared across all widgets.
                //  * Selection (C-space).
                _ => return true,
            }
            false
        });
        ret
    }

    pub fn render(&mut self, f: &mut Frame, area: Rect, palette: &Palette) {
        let width = str_width(&self.text);
        let cursor_pos = str_width(&self.text[..self.cursor]);
        let margin = (width/3).min(5);
        self.hscroll = self.hscroll.min(cursor_pos.saturating_sub(margin)).max(cursor_pos.saturating_sub((area.width as usize).saturating_sub(margin + 1)));
        self.hscroll = self.hscroll.min(width.saturating_sub(area.width as usize));
        let paragraph = Paragraph::new(self.text.clone()).block(Block::default().style(palette.default)).scroll((0, self.hscroll as u16));
        f.render_widget(paragraph, area);
        if cursor_pos >= self.hscroll && cursor_pos - self.hscroll <= area.width as usize {
            f.set_cursor(area.x + (cursor_pos - self.hscroll) as u16, area.y);
        }
    }

    fn step_back(&mut self) -> bool {
        if self.cursor == 0 { return false; }
        self.cursor = self.text[..self.cursor].char_indices().rev().next().unwrap().0;
        true
    }
    fn step_forward(&mut self) -> bool {
        if self.cursor == self.text.len() { return false; }
        self.cursor = self.text[self.cursor..].char_indices().nth(1).map_or(self.text.len(), |t| self.cursor + t.0);
        true
    }
    fn is_word_character(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
            _ => false }
    }
    fn word_end(&self) -> usize {
        self.text[self.cursor..].char_indices().skip_while(|t| !Self::is_word_character(t.1)).skip_while(|t| Self::is_word_character(t.1)).next().map_or(self.text.len(), |t| self.cursor + t.0)
    }
    fn word_start(&self) -> usize {
        let mut res = 0;
        self.text[..self.cursor].char_indices().rev().skip_while(|t| !Self::is_word_character(t.1)).skip_while(|t| if Self::is_word_character(t.1) { res = t.0; true } else { false }).next();
        res
    }
}

pub struct SearchDialog {
    input: TextInput,
    scroll: Scroll,
    search: SymbolSearcher,
    height: u16,
}

#[derive(Clone)]
pub enum SearchDialogEvent {
    None,
    Done(SearchResultInfo),
    Cancel,
    Open(SearchResultInfo),
}

impl SearchDialog {
    pub fn new(searcher: Arc<dyn Searcher>, context: Arc<Context>) -> Self { Self {input: TextInput::new(), scroll: Scroll::new(), search: SymbolSearcher::new(searcher, context), height: 0} }

    pub fn update(&mut self, keys: &mut Vec<Key>, key_binds: &KeyBindings, registry: &SymbolsRegistry, binaries: Option<Vec<BinaryId>>) -> SearchDialogEvent {
        let event = self.input.update(keys, key_binds);
        let results = self.search.get_results();
        self.scroll.update(results.results.len(), self.height, keys, key_binds);
        let res = match event {
            TextInputEvent::None => SearchDialogEvent::None,
            TextInputEvent::Cancel => return SearchDialogEvent::Cancel,
            TextInputEvent::Done if results.results.is_empty() => return SearchDialogEvent::Cancel,
            TextInputEvent::Open if results.results.is_empty() => SearchDialogEvent::None,
            TextInputEvent::Done | TextInputEvent::Open => {
                let r = &results.results[self.scroll.cursor.min(results.results.len() - 1)];
                let res = self.search.format_result(r);
                match event {
                    TextInputEvent::Done => return SearchDialogEvent::Done(res),
                    _ => SearchDialogEvent::Open(res) }
            }
        };

        if self.search.update(registry, binaries, &self.input.text) {
            self.scroll = Scroll::new();
        }

        res
    }

    pub fn render(&mut self, f: &mut Frame, screen_area: Rect, title: &str, palette: &Palette) -> /*loading*/ bool {
        let area = screen_area.inner(&Margin {vertical: screen_area.height / 12, horizontal: screen_area.width / 8});
        let input_area = Rect {x: area.x + 2, y: area.y + 2, width: area.width.saturating_sub(4), height: 1};
        let status_area = Rect {x: area.x + 2, y: area.y + 4, width: area.width.saturating_sub(4), height: 1};
        let results_area = Rect {x: area.x + 2, y: area.y + 6, width: area.width.saturating_sub(4), height: area.height.saturating_sub(7)};
        let properties = self.search.searcher.properties();
        let lines_per_result = properties.have_names as u16 + properties.have_files as u16;
        self.height = results_area.height / lines_per_result;

        // TODO: Subtly different background color for file/function/variable dialogs
        let block = Block::default().title(title.to_string()).borders(Borders::ALL).style(palette.dialog);
        f.render_widget(Clear, area);
        f.render_widget(block, area);
        f.render_widget(Clear, input_area);
        self.input.render(f, input_area, palette);

        let res = self.search.get_results();

        if self.search.waiting_for_symbols {
            f.render_widget(Paragraph::new(Span::styled("waiting for symbols to load (see 'binaries' window for progress)", palette.warning)), status_area);
        } else if res.complete {
            let text = if res.results.len() == res.total_results {
                format!("{} matches ({} searched)", res.results.len(), PrettySize(res.bytes_done))
            } else {
                format!("showing {}/{} matches ({} searched)", res.results.len(), res.total_results, PrettySize(res.bytes_done))
            };
            f.render_widget(Paragraph::new(text), status_area);
        } else {
            f.render_widget(Gauge::default().ratio(res.items_done as f64 / res.items_total.max(1) as f64).label(format!("{}", PrettySize(res.bytes_done))).use_unicode(false).gauge_style(palette.progress_bar_remaining.fg(palette.progress_bar_done.bg.unwrap_or(Color::Reset))), status_area);
        }

        let range = self.scroll.range(res.results.len(), self.height as usize);
        let mut table_state = TableState::default();
        table_state.select(Some(self.scroll.cursor - range.start));

        let mut rows: Vec<Row> = Vec::new();
        for r in self.search.format_results(&res.results[range]) {
            let mut lines: Vec<Spans> = Vec::new();
            if properties.have_names {
                lines.push(Spans::from(vec![Span::raw(format!("{}\n", r.name))]));
            }
            if properties.have_files {
                let mut spans: Vec<Span> = Vec::new();
                spans.push(Span::styled(format!("{}", r.file.as_os_str().to_string_lossy()), palette.location_filename));
                if r.line.file_idx().is_some() && r.line.line() != 0 {
                    spans.push(Span::styled(format!(":{}", r.line.line()), palette.location_line_number));
                    if r.line.column() != 0 {
                        spans.push(Span::styled(format!(":{}", r.line.column()), palette.location_column_number));
                    }
                }
                lines.push(Spans::from(spans));
            }
            rows.push(Row::new(vec![Cell::from(Text::from(lines))]).height(lines_per_result));
        }

        let table = Table::new(rows)
            .widths(&[Constraint::Percentage(100)])
            .highlight_style(palette.table_selected_item).highlight_symbol(">> ");
        f.render_stateful_widget(table, results_area, &mut table_state);

        !res.complete
    }
}

pub struct SearchBar {
    pub text: TextInput,
    pub visible: bool,
    pub editing: bool,
}
impl SearchBar {
    pub fn new() -> Self { Self {text: TextInput::new(), visible: false, editing: false} }

    pub fn update(&mut self, keys: &mut Vec<Key>, key_binds: &KeyBindings) -> TextInputEvent {
        if !self.editing {
            return TextInputEvent::None;
        }
        let mut ev = self.text.update(keys, key_binds);
        match ev {
            TextInputEvent::None => (),
            TextInputEvent::Done => self.editing = false,
            TextInputEvent::Cancel => {
                self.editing = false;
                self.visible = false;
            }
            TextInputEvent::Open => ev = TextInputEvent::None,
        }
        if !self.editing && self.text.text.is_empty() {
            self.visible = false;
        }
        ev
    }

    pub fn render(&mut self, left_text: &str, right_text: &str, f: &mut Frame, area: Rect, palette: &Palette) -> Rect {
        if !self.visible || area.height == 0 {
            return area;
        }
        let right_text_width = str_width(right_text) as u16;
        let left_area = Rect {x: area.x, y: area.y, width: area.width.min(str_width(left_text) as u16), height: 1};
        let right_area = Rect {x: (area.x + area.width).saturating_sub(right_text_width), y: area.y, width: right_text_width, height: 1};
        let text_area = Rect {x: area.x + left_area.width, y: area.y, width: area.width.saturating_sub(left_area.width).saturating_sub(right_area.width), height: 1};
        let remaining_area = Rect {x: area.x, y: area.y + 1, width: area.width, height: area.height - 1};

        let paragraph = Paragraph::new(Text {lines: vec![Spans::from(vec![Span::styled(left_text, palette.search_bar_other)])]});
        f.render_widget(paragraph, left_area);
        let paragraph = Paragraph::new(Text {lines: vec![Spans::from(vec![Span::styled(right_text, palette.search_bar_other)])]});
        f.render_widget(paragraph, right_area);

        if self.editing {
            self.text.render(f, text_area, palette);
        } else {
            let mut spans = vec![Span::styled(&self.text.text, palette.search_bar_query)];
            if str_width(&self.text.text) < text_area.width as usize {
                spans.push(Span::styled(format!("{: >0$}", text_area.width as usize + 10), palette.search_bar_query));
            }
            let paragraph = Paragraph::new(Text {lines: vec![Spans::from(spans)]});
            f.render_widget(paragraph, text_area);
        }

        remaining_area
    }

    pub fn start_editing(&mut self) {
        self.visible = true;
        self.editing = true;
        self.text.cursor = self.text.text.len();
    }

    pub fn height(&self) -> u16 {
        if self.visible {1} else {0}
    }
}
