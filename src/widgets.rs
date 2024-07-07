use crate::{*, search::*, pool::*, symbols_registry::*, util::*, error::*, procfs::*, settings::*, context::*, imgui::*, common_ui::*, terminal::*};
use std::{io, ops::Range, sync::Arc, fmt::Write as fmtWrite, mem};

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
    pub scroll_to_selected_tab: bool,
}
impl TabsState {
    pub fn select(&mut self, tab_idx: usize) {
        self.selected = tab_idx;
        self.scroll_to_selected_tab = true;
    }
}

pub struct Tabs {
    tabs: Vec<(WidgetIdx, /*short_title_idx*/ usize, /*full_title_idx*/ usize)>,
    state: TabsState,
    viewport: WidgetIdx,
    content: WidgetIdx,
}
impl Tabs {
    pub fn new(state: TabsState, imgui: &mut IMGUI) -> Self {
        let content = imgui.add(widget!().width(AutoSize::Children).fixed_height(1).flags(WidgetFlags::HSCROLL_INDICATOR_ARROWS).hstack());
        Self {tabs: Vec::new(), state, viewport: imgui.cur_parent, content}
    }

    pub fn add(&mut self, full_title: &str, short_title: &str, pinned: bool, hotkey_number: Option<usize>, imgui: &mut IMGUI) {
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
                    self.state.select(self.tabs.len());
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
                if let Some(n) = hotkey_number {
                    styled_write!(imgui.text, style, " ");
                    styled_write!(imgui.text, imgui.palette.hotkey, "{}", n);
                }
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
        for key in keys {
            match key {
                KeyAction::PreviousTab => self.state.select(self.state.selected.saturating_sub(1)),
                KeyAction::NextTab => self.state.select(self.state.selected + 1),
                _ => panic!("huh"),
            }
        }

        if !self.tabs.is_empty() {
            if self.state.selected >= self.tabs.len() {
                self.state.select(self.tabs.len() - 1);
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
            if mem::take(&mut self.state.scroll_to_selected_tab) && !self.tabs.is_empty() {
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
        // TODO: Use bracketed paste mode to handle pasting multiline text when capture_return_key is false.
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
                        //asdqwe group consecutive kills into one clipboard string, like in emacs kill ring; same for other kill operations
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
                        //asdqwe if at start/end, remove the '\n'
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


/*asdqwe delet
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
}*/

pub struct SearchDialog {
    input: TextInput,
    search: SymbolSearcher,
}

#[derive(Clone)]
pub enum SearchDialogEvent {
    None,
    Done(SearchResultInfo),
    Cancel,
    Open(SearchResultInfo),
}

impl SearchDialog {
    pub fn new(searcher: Arc<dyn Searcher>, context: Arc<Context>) -> Self { Self {input: TextInput::default(), search: SymbolSearcher::new(searcher, context)} }
/*asdqwe
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
            .highlight_style(palette.table_selected_item).highlight_symbol("➤ ");
        f.render_stateful_widget(table, results_area, &mut table_state);

        !res.complete
    }*/
}

#[derive(Default)]
pub struct SearchBar {
    pub text: TextInput,
    pub visible: bool,
    pub editing: bool,
}
impl SearchBar {
/*asdqwe
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
    }*/
}
