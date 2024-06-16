figure out input;
focus path, assembled from pieces;
use the focus mechanism for table selection?;
do multiple renders if keys dispatch to different boxes, up to some time limit (have timestamp with each key, discard if too old);
think more about context menus, main menu, mouse, gui;

// Immediate-mode UI library, inspired by https://www.rfleury.com/p/ui-series-table-of-contents

struct Rect {
    x: usize,
    y: usize,
    w: usize,
    h: usize,
}

bitflags! { pub struct WidgetFlags : u32 {
    WIDTH_FROM_CONTENT,
    HEIGHT_FROM_CONTENT,
    FLOATING_X,
    FLOATING_Y,

    SCROLL_X = 0x8,
    SCROLL_Y = 0x10,
    SCROLLBAR_Y = 0x20, // even when the scrollbar is not visible, one column of width is reserved for it
    SCROLL_HERE, // the nearest ancestor scrollable area should scroll such that this widget is visible

    LINE_WRAP,
    ELLIPSIS_FOR_TRUNCATED_TEXT,
    HORIZONTAL_CLIPPING_ARROWS, // if the line of text is clipped by parent scroll area, show arrows at the clipped end(s)
    REPEAT_TEXT_VERTICALLY,
    REPEAT_TEXT_HORIZONTALLY,
    FILL_BACKGROUND,
}}

struct Widget {
    // State that may be used across frames.

    identity: usize, // identifier for recognizing the widget across frames; 0 means unset and not preserved across frames
    rect: Rect,
    flags: WidgetFlags,

    parent: WidgetIdx,
    first_child: WidgetIdx,
    last_child: WidgetIdx,
    prev_sibling: WidgetIdx,
    next_sibling: WidgetIdx,

    // State only used for current frame.

    background: Option<Color>,
    text: Range<usize>, // range of lines in all_text
    line_wrap_limit: usize, // max height when LINE_WRAP is enabled
}
impl Default for Widget;
struct WidgetIdx(usize);

struct UI {
    // State preserved across frames.
    prev_widgets: Vec<Widget>, // sorted by identity

    all_text: StyledText,
    widgets: Vec<Widget>,
    parent_stack: Vec<WidgetIdx>,
    
}
impl Frame {
    fn add(&mut self, w: Widget);
}


hints
    panel has fixed height
    horizontal scrolling
    always 2 columns
    two text areas with widths determined by content;
status
    panel has fixed height
    state line is a full-width widget with background color
    other info is just a text area;
tabs
    scrollable x, stack x
    scroll indicator arrows
    auto scroll to selected tab
    each tab header has width determined by content
    separators;
watches
    table header
    name and value columns, widths as percentage of parent
    row has two flags: expanded, line-wrapped
    if has children, right arrow goes collapsed -> expanded -> expanded+wrapped; left arrow always goes to collapsed+non-wrapped
    line-wrapped rows wrap to at most ~20 lines, in each column separately
    non-wrapped rows truncate text with ellipsis
    arrow and indentation are their own widgets, so that name column line-wraps correctly, and to make the arrow clickable in future
    non-wrapped lines grouped into placeholders when offscreen
    wrapped lines always emitted individually and height recalculated every frame even if offscreen
    scrollable y, scrollbar
    auto-scroll to selected line
    when editing a watch, temporarily disable line wrapping for its row;
disassembly
    horizontally scrollable area, inside it the header row and the main vertically scrollable area
    scrollable y, scrollbar
    each visible line is a text area, sized by content, with arrow truncation indicators (HORIZONTAL_CLIPPING_ARROWS)
    placeholders for offscreen lines, use longest line width as width
    tabs
    header line;
code
    same as disassembly: tabs, header line, text area per line, etc;
binaries, breakpoints, stack, threads
    table widget
    h scroll area, inside it are table header and v scrollable area
    no placeholders
    all columns have content-based width, long column is last
    optional min width for each column (e.g. for address and idx to avoid jumping around too much)
    binaries: optional second line for error message, maybe with merged rows
    binaries: progress bar, merge cells, use parent width (don't bother excluding it from horizontal scrolling; no special treatment in Table)
    stack: truncation message is just a row
    windowed mode: early layout, placeholders;
threads
    some min column widths manually calculated based on full list of threads
    filter: box, text input, affects table contents
    sorting by any column, highlight it in the header if not default;
windows
    border lines are widgets with repeated text
    just keep layout.rs for now;
search dialog
    separate tree
    borders and title
    progress bar
    windowed table with one column and no header;
maximizing a panel: move to separate tree;

struct Table;
impl Table {
    fn new(frame: &mut Frame) -> Self;
    fn enable_selected_line_indicator(&mut self);
    fn header_cell(&mut self, text: usize, min_width: usize) -> WidgetIdx;
    fn finish_header(&mut self);
    fn hide_header(&mut self);
    fn enable_windowed_mode(&mut self, num_rows: usize, selected_row: usize) -> Range<usize>;
    fn cell(&mut self, text: usize) -> WidgetIdx;
    fn merged_cell(&mut self, text: usize, count: usize) -> WidgetIdx;
    fn finish_subrow(&mut self);
    fn select_current_row(&mut self);
    fn finish_row(&mut self);
    fn finish(&mut self, &mut Frame);
}
