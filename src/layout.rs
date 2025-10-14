use crate::{*, ui::*, widgets::*, pool::*, settings::*, imgui::*, common_ui::*, util::*};
use std::{mem, collections::HashSet, hash::{Hash, Hasher}, ops::Range};

// (This file is about the UI panes, not layout in a general sense.)

// TODO: Drag and drop windows.
// TODO: Save state.
pub struct Layout {
    pub windows: Pool<Window>,
    pub active_window: Option<WindowId>,
    pub regions: Pool<Region>,
    pub root: RegionId,
    // Which walls surrounding the whole UI to draw. [axis][-/+]
    // The top ([1][0]) is strongly recommended, otherwise there's nowhere to put titles of single-window regions.
    pub outer_walls: [[bool; 2]; 2],

    // Assigned on every frame.
    active_leaf: RegionId,
    root_widget: WidgetIdx,
    wall_widgets: Vec<WidgetIdx>,
    // Information used for deciding which of the Box Drawing unicode characters (https://en.wikipedia.org/wiki/Box_Drawing)
    // to draw where. Screen coordinates -> bitset describing styles for the 4 cardinal directions (left, right, up, down):
    //  * bits 0-3 - which lines to draw,
    //  * bits 4-7 - which lines are bold.
    wall_masks: Vec<u8>,
    wall_area: Rect, // area covered by wall_masks
}

pub type WindowId = Id;
pub type RegionId = Id;

pub struct Window {
    pub type_: WindowType,
    pub region: Option<RegionId>, // if None, this is a detached window not in any region, in a sort of clipboard (not implemented yet)
    pub required: bool,
    pub title: String,
    pub content: Box<dyn WindowContent>,
    pub hotkey_number: Option<usize>,
    // Try to keep this window this absolute size along one or both axes. Useful for windows with fixed amount of information to show, e.g. hints.
    // This is not guaranteed, the window should still be able to build itself at any size.
    // E.g. this is ignored if the window shared a region with other windows (as tabs) or if its parent split is along a different axis (even if there's a higher ancestor split along the correct axis).
    pub fixed_size: [Option<usize>; 2],

    // These are assigned on every frame.
    pub visible: bool,
    pub area: Rect,
    pub widget: WidgetIdx, // created even for invisible windows
}
impl Window {
    fn clear_layout(&mut self) { self.visible = false; self.area = Rect::default(); self.widget = WidgetIdx::invalid(); }
}

pub struct Region {
    pub parent: Option<RegionId>,
    pub content: RegionContent,
    pub relative_size: usize, // along the parent split axis; not in any particular units, normalized by the sum

    // These are assigned on every frame.
    pub area: Rect,
    // Surrounding walls. May be shared with other regions. Not included in `area`. May be invalid() if at the edge of the screen.
    pub outer_walls: [[WidgetIdx; 2]; 2], // [axis][-/+]
}
impl Region {
    fn clear_layout(&mut self) { self.area = Rect::default(); self.outer_walls = [[WidgetIdx::invalid(); 2]; 2]; match &mut self.content { RegionContent::Leaf(leaf) => (leaf.tabs_widget, leaf.content_widget) = (WidgetIdx::invalid(), WidgetIdx::invalid()), _ => () } }
}

pub struct RegionSplit {
    pub axis: usize,
    pub children: Vec<RegionId>,
}

#[derive(Default)]
pub struct RegionLeaf {
    pub tabs: Vec<WindowId>,
    pub tabs_state: TabsState,

    // Assigned on every frame.
    pub tabs_widget: WidgetIdx,
    pub content_widget: WidgetIdx,
}

pub enum RegionContent {
    Split(RegionSplit),
    Leaf(RegionLeaf),
}

impl RegionContent {
    pub fn is_leaf(&self) -> bool { match self { RegionContent::Leaf(_) => true, _ => false }  }
    pub fn as_split(&self) -> &RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
    pub fn as_leaf(&self) -> &RegionLeaf { match self { RegionContent::Leaf(x) => x, RegionContent::Split(_) => panic!("not a leaf"), } }
    pub fn as_leaf_mut(&mut self) -> &mut RegionLeaf { match self { RegionContent::Leaf(x) => x, RegionContent::Split(_) => panic!("not a leaf"), } }
    pub fn as_split_mut(&mut self) -> &mut RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
}

impl Layout {
    pub fn new() -> Layout {
        let mut regions: Pool<Region> = Pool::new();
        let root = regions.add(Region {parent: None, area: Rect::default(), content: RegionContent::Leaf(RegionLeaf::default()), relative_size: 1000000, outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0;
        let outer_walls = [[true, true], [true, true]]; // all walls along the edge of the screen
        // let outer_walls = [[false, false], [true, false]]; // only the top wall
        Layout {windows: Pool::new(), active_window: None, regions, root, outer_walls, root_widget: WidgetIdx::invalid(), wall_area: Rect::default(), wall_masks: Vec::new(), wall_widgets: Vec::new(), active_leaf: RegionId::default()}
    }

    pub fn split(&mut self, region_id: RegionId, axis: usize, walls: Vec<f64>) -> Vec<RegionId> {
        for i in 1..walls.len() {
            assert!(walls[i] >= walls[i-1]);
            assert!(walls[i] <= 1.0);
            assert!(walls[i-1] >= 0.0);
        }

        let region = self.regions.get_mut(region_id);

        match &region.content {
            // This check ensures that none of the `children` below get simplified out. Otherwise we wouldn't be able to return a meaningful list of regions from this function.
            RegionContent::Split(split) if split.axis == axis => panic!("splitting in same direction as the same region is already split is not allowed"),
            _ => () }

        let parent = region.parent.take();
        let relative_size = region.relative_size;
        let new_region_id = self.regions.add(Region {parent: parent.clone(), content: RegionContent::Split(RegionSplit {axis, children: Vec::new()}), relative_size, area: Rect::default(), outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0;

        let mut children = vec![region_id];
        let denominator = 1000000.0;
        for i in 0..walls.len() {
            let end = if i + 1 < walls.len() {walls[i+1]} else {1.0};
            let relative_size = (((end - walls[i]) * denominator) as usize).max(1);
            children.push(self.regions.add(Region {parent: Some(new_region_id), content: RegionContent::Leaf(RegionLeaf {tabs: Vec::new(), tabs_state: TabsState::default(), tabs_widget: WidgetIdx::invalid(), content_widget: WidgetIdx::invalid()}), relative_size, area: Rect::default(), outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0);
        }
        let new_region = self.regions.get_mut(new_region_id);
        new_region.content.as_split_mut().children = children.clone();

        let region = self.regions.get_mut(region_id);
        region.parent = Some(new_region_id);
        region.relative_size = ((walls[0] * denominator) as usize).max(1);

        match parent {
            None => self.root = new_region_id,
            Some(parent_id) => {
                let parent_split = self.regions.get_mut(parent_id).content.as_split_mut();
                let i = parent_split.children.iter().position(|x| x == &region_id).unwrap();
                parent_split.children[i] = new_region_id;
            }
        }

        self.simplify_splits(new_region_id);

        children
    }

    pub fn set_fixed_size(&mut self, window_id: WindowId, axis: usize, size: usize) {
        self.windows.get_mut(window_id).fixed_size[axis] = Some(size);
    }

    pub fn set_hotkey_number(&mut self, window_id: WindowId, number: usize) {
        self.windows.get_mut(window_id).hotkey_number = Some(number);
    }

    // Collapses splits that have only one child. Collapses nested splits with the same direction.
    // Starts at the given region and walks up while there are things to simplify.
    pub fn simplify_splits(&mut self, mut region_id: RegionId) {
        loop {
            let region = self.regions.get(region_id);
            let split = match &region.content {
                RegionContent::Split(s) => s,
                RegionContent::Leaf(_) => break };
            if let &Some(parent_id) = &region.parent {
                let parent = self.regions.get(parent_id);
                let parent_split = parent.content.as_split();
                if parent_split.axis != split.axis && split.children.len() > 1 {
                    break;
                }
                let relative_size = region.relative_size;
                let idx = parent_split.children.iter().position(|x| x == &region_id).unwrap();
                let split = self.regions.get_mut(region_id).content.as_split_mut();
                let children = mem::take(&mut split.children);

                let mut total_size = 0usize;
                for &child_id in &children {
                    let child = self.regions.get_mut(child_id);
                    child.parent = Some(parent_id);
                    total_size += child.relative_size;
                }
                // Rescale the children to add up to the size of the region they used to comprise.
                for &child_id in &children {
                    let child = self.regions.get_mut(child_id);
                    child.relative_size = child.relative_size * relative_size / total_size;
                }

                let parent_split = self.regions.get_mut(parent_id).content.as_split_mut();
                parent_split.children.splice(idx..idx+1, children.clone());

                self.regions.remove(region_id);

                region_id = match children.len() {
                    0 => parent_id, // parent might be down to one child now
                    1 => children[0], // child and parent may have the same split direction
                    _ => break,
                };
            } else {
                assert!(region_id == self.root);
                if split.children.is_empty() { panic!("deleting the root is not allowed"); }
                if split.children.len() == 1 {
                    self.root = split.children[0];
                    self.regions.get_mut(self.root).parent = None;
                    self.regions.remove(region_id).unwrap();
                }
                break;
            }
        }
    }

    pub fn attach_window(&mut self, leaf_id: RegionId, win_id: WindowId) {
        assert!(self.windows.get(win_id).region.is_none());
        self.regions.get_mut(leaf_id).content.as_leaf_mut().tabs.push(win_id);
        self.windows.get_mut(win_id).region = Some(leaf_id);
    }

    pub fn new_window(&mut self, parent: Option<RegionId>, type_: WindowType, required: bool, content: Box<dyn WindowContent>) -> WindowId {
        let win = Window {type_, region: parent.clone(), visible: false, area: Rect::default(), widget: WidgetIdx::invalid(), required, title: type_.title().to_string(), content, hotkey_number: None, fixed_size: [None; 2]};
        let win_id = self.windows.add(win).0;
        if let Some(id) = parent {
            self.regions.get_mut(id).content.as_leaf_mut().tabs.push(win_id);
        }
        win_id
    }

    pub fn sorted_windows(&self) -> Vec<(WindowId, &Window)> {
        let mut v: Vec<(WindowId, &Window)> = self.windows.iter().collect();
        v.sort_by_key(|(id, w)| w.type_);
        v
    }

    pub fn sorted_windows_mut(&mut self) -> Vec<(WindowId, &mut Window)> {
        let mut v: Vec<(WindowId, &mut Window)> = self.windows.iter_mut().collect();
        v.sort_by_key(|(id, w)| w.type_);
        v
    }

    pub fn any_window_by_type(&self, t: WindowType) -> Option<WindowId> {
        for (id, win) in self.windows.iter() {
            if win.type_ == t {
                return Some(id);
            }
        }
        None
    }

    pub fn leaf_at_point(&self, p: [isize; 2]) -> Option<WindowId> {
        for (id, region) in self.regions.iter() {
            if region.content.is_leaf() && region.area.contains(p.clone()) {
                return Some(id);
            }
        }
        None
    }

    pub fn switch_to_adjacent_leaf(&mut self, axis: usize, side: usize) {
        // Take a point just to the left/right/up/down of the rect, next to the middle of the corresponding edge.
        // Then find a window containing that point.
        let area = self.regions.get(self.active_leaf).area.clone();
        let mut p = area.center();
        p[axis] = if side == 0 {area.pos[axis] - 2} else {area.end(axis) + 1};
        if let Some(id) = self.leaf_at_point(p) {
            self.active_leaf = id;
            return;
        }
        // Maybe point landed on a wall. Nudge it in perpendicular direction and try again.
        p[1-axis] += 1;
        if let Some(id) = self.leaf_at_point(p) {
            self.active_leaf = id;
        }
    }

    // build_wall() makes a widget for inner wall that splits `area` along the given axis (so the wall itself is perpendicular to this axis).
    // trace_wall() records the corresponding wall in wall_masks (which determines which characters end up in the wall widgets later).
    // Wall widgets contain whole cells, while the traced line connects centers of cells; the two cells containing line endpoints are not included in the created widget,
    // they're sticking out into the parent region's wall widgets, where they form junctions (or into corners of the screen).
    // (Why separate wall_masks from wall_widgets? To make junctions work. Why have separate widgets for individual walls instead of one big canvas widget? To make walls draggable and hoverable.)
    fn build_wall(axis: usize, area: Rect, pos: isize, identity: usize, wall_widgets: &mut Vec<WidgetIdx>, ui: &mut UI) -> WidgetIdx {
        let mut w = widget!().fixed_width(1).fixed_height(1).fixed_x(area.x()).fixed_y(area.y()).fill('?', ui.palette.error);
        w.axes[1-axis].set_fixed_size(area.size[1-axis]);
        w.axes[axis].rel_pos += pos;
        w.identity = identity;
        let idx = ui.add(w);
        wall_widgets.push(idx);
        idx                                                                                                                                                                               
    }
    #[inline]
    fn wall_masks_idx(x: isize, y: isize, wall_area: &Rect) -> usize {
        if wall_area.contains([x, y]) {
            (x - wall_area.x()) as usize + (y - wall_area.y()) as usize * wall_area.width()
        } else {
            wall_area.width() * wall_area.height()
        }
    }
    fn trace_wall(axis: usize, area: Rect, pos: isize, bold: bool, wall_masks: &mut Vec<u8>, wall_area: &Rect) {
        let len = area.size[1-axis];
        let mut p = area.pos;
        p[axis] += pos;

        let write_wall_mask = |p: [isize; 2], val: u8, masks: &mut Vec<u8>, area: &Rect| {
            masks[(p[0]-area.pos[0]) as usize + (p[1]-area.pos[1]) as usize * area.width()] |= val;
        };

        let base = if bold {4} else {0};
        let mask = match axis {
            Axis::Y => {
                wall_masks[Self::wall_masks_idx(p[0] - 1, p[1], wall_area)] |= 2 << base;
                wall_masks[Self::wall_masks_idx(p[0] + len as isize, p[1], wall_area)] |= 1 << base;
                3 << base
            }
            _ => {
                wall_masks[Self::wall_masks_idx(p[0], p[1] - 1, wall_area)] |= 8 << base;
                wall_masks[Self::wall_masks_idx(p[0], p[1] + len as isize, wall_area)] |= 4 << base;
                12 << base
            }
        };
        for i in 0..len {
            let mut pp = p;
            pp[1-axis] += i as isize;
            wall_masks[Self::wall_masks_idx(pp[0], pp[1], wall_area)] |= mask;
        }
    }

    pub fn build(&mut self, ui: &mut UI) {
        // Maybe change active_window.
        let mut switched_window = false;
        let window_keys = [KeyAction::Window0, KeyAction::Window1, KeyAction::Window2, KeyAction::Window3, KeyAction::Window4, KeyAction::Window5, KeyAction::Window6, KeyAction::Window7, KeyAction::Window8, KeyAction::Window9];
        for action in ui.check_keys(&window_keys) {
            let n = window_keys.iter().position(|k| *k == action).unwrap();
            if let Some((id, _)) = self.windows.iter().find(|(_, win)| win.region.is_some() && win.hotkey_number == Some(n)) {
                self.active_window = Some(id);
                switched_window = true;
            }
        }

        // User of Layout mostly cares about windows and not regions, so we have public active_window field.
        // But the build() procedure mostly cares about regions and not windows.
        // So we map active_window to active_leaf here, then map it back near the end of build().
        let mut found = false;
        if let &Some(window_id) = &self.active_window {
            let win = self.windows.get(window_id);
            if let &Some(region_id) = &win.region {
                let leaf = self.regions.get_mut(region_id).content.as_leaf_mut();
                let i = leaf.tabs.iter().position(|w| w == &window_id).unwrap();
                leaf.tabs_state.selected = i;
                leaf.tabs_state.scroll_to_selected_tab |= switched_window;
                self.active_leaf = region_id;
                found = true;
            }
        }
        if !found {
            for (id, region) in self.regions.iter() {
                if region.content.is_leaf() {
                    self.active_leaf = id;
                    found = true;
                }
            }
            assert!(found);
        }

        // Maybe change active leaf.
        for action in ui.check_keys(&[KeyAction::WindowLeft, KeyAction::WindowRight, KeyAction::WindowDown, KeyAction::WindowUp]) {
            match action {
                KeyAction::WindowLeft => self.switch_to_adjacent_leaf(Axis::X, 0),
                KeyAction::WindowRight => self.switch_to_adjacent_leaf(Axis::X, 1),
                KeyAction::WindowUp => self.switch_to_adjacent_leaf(Axis::Y, 0),
                KeyAction::WindowDown => self.switch_to_adjacent_leaf(Axis::Y, 1),
                _ => panic!("huh"),
            }
        }

        self.root_widget = ui.cur_parent;
        self.wall_widgets.clear();
        for (_, win) in self.windows.iter_mut() {
            win.clear_layout();
        }
        for (_, region) in self.regions.iter_mut() {
            region.clear_layout();
        }

        // Build walls along the edges of the screen if needed.
        //
        // There's a subtlety about corners. Consider the top wall when all 4 walls are enabled vs when only the top wall is enabled.
        // In top-only mode we want the wall to go all the way to the edge of the screen, not to the middle of the first and last cell of the screen ('─', not '╴').
        // But in all-walls mode we want the wall to go to the center of the first and last cell ('╴' + '╷' = '┐', not '─' + '│' = '┼').
        // Trick: always build the whole frame, but extend the root rect such that unneeded sides are just off-screen.
        let mut area = Rect {pos: [0, 0], size: [ui.cur().axes[0].get_fixed_size(), ui.cur().axes[1].get_fixed_size()]};
        for axis in 0..2 {
            for side in 0..2 {
                if self.outer_walls[axis][side] && area.size[axis] > 0 {
                    area.size[axis] -= 1;
                    if side == 0 {
                        area.pos[axis] += 1;
                    }
                }
            }
        }
        self.wall_area = Rect {pos: [area.pos[0]-1, area.pos[1]-1], size: [area.size[0]+2, area.size[1]+2]}; // add space for outer wall
        self.wall_masks.clear();
        self.wall_masks.resize(self.wall_area.width() * self.wall_area.height() + 1, 0u8);
        let root = self.regions.get_mut(self.root);
        root.area = area;
        for axis in 0..2 {
            for side in 0..2 {
                let pos = if side == 0 {-1} else {area.size[axis] as isize};
                let w = Self::build_wall(axis, area, pos, hash(&('o', axis, side)), &mut self.wall_widgets, ui);
                Self::trace_wall(axis, area, pos, false, &mut self.wall_masks, &self.wall_area);
                root.outer_walls[axis][side] = w;
                // Cover the corners.
                if axis == 0 {
                    let w = ui.get_mut(w);
                    w.axes[1-axis].rel_pos -= 1;
                    w.axes[1-axis].size += 2;
                }
            }
        }

        // Do layout, build inner walls, make container widgets for leaf regions.
        // After this, all areas and active_leaf are final, but active window is not (tab switch may happen later).
        let mut stack = vec![self.root];
        let mut visited_regions: HashSet<RegionId> = HashSet::new();
        let mut visited_windows: HashSet<WindowId> = HashSet::new();
        while let Some(region_id) = stack.pop() {
            let ins = visited_regions.insert(region_id);
            assert!(ins);
            let region = self.regions.get_mut(region_id);
            let area = region.area;

            match &mut region.content {
                RegionContent::Leaf(leaf) => {
                    for &window_id in &leaf.tabs {
                        let ins = visited_windows.insert(window_id);
                        assert!(ins);
                        assert_eq!(self.windows.get(window_id).region, Some(region_id));
                    }

                    // Tabs widget, also acting as window title, overlaps the wall above this region. Make its width match the tabs width, to leave some length of draggable wall. That's why we can't put table_widget and content_widget inside a container widget.
                    // Create these two widgets early to be able to switch active_leaf on click before we drew the bold frame around it.
                    leaf.tabs_widget = ui.add(widget!().identity(&('t', region_id)).fixed_height(1).width(AutoSize::Children).max_width(region.area.width().saturating_sub(1)).fixed_x(region.area.x()).fixed_y(region.area.y() - 1));
                    leaf.content_widget = ui.add(widget!().identity(&('w', region_id)).fixed_rect(region.area));
                    with_parent!(ui, leaf.tabs_widget, {
                        if ui.check_mouse(MouseActions::CLICK_SUBTREE) {
                            self.active_leaf = region_id;
                        }
                    });
                    with_parent!(ui, leaf.content_widget, {
                        if ui.check_mouse(MouseActions::CLICK_SUBTREE) {
                            self.active_leaf = region_id;
                        }
                    });
                }
                RegionContent::Split(_) => {
                    self.build_split(region_id, ui);
                    for &child_id in self.regions.get(region_id).content.as_split().children.iter().rev() {
                        assert_eq!(self.regions.get(child_id).parent, Some(region_id));
                        stack.push(child_id);
                    }
                }
            }
        }

        // Some invariant checks.
        for (id, _) in self.regions.iter() {
            assert!(visited_regions.contains(&id));
        }
        for (id, win) in self.windows.iter() {
            assert!(visited_windows.contains(&id));
        }

        // Trace bold border around active region.
        let area = self.regions.get(self.active_leaf).area;
        for axis in 0..2 {
            for side in 0..2 {
                let pos = if side == 0 {-1} else {area.size[axis] as isize};
                let w = Self::trace_wall(axis, area, pos, true, &mut self.wall_masks, &self.wall_area);
            }
        }

        // Draw walls: aggregate the information in wall_masks and put chars into wall widgets.
        for &idx in &self.wall_widgets {
            let w = ui.get(idx);
            let x_range = w.axes[Axis::X].get_fixed_range();
            let start_line = ui.text.num_lines();
            for y in w.axes[Axis::Y].get_fixed_range() {
                for x in x_range.clone() {
                    let mask = self.wall_masks[Self::wall_masks_idx(x, y, &self.wall_area)];
                    let c = BOX_DRAWING_LUT[mask as usize];
                    let style = if mask >= 16 {ui.palette.window_border_active} else {ui.palette.window_border};
                    styled_write!(ui.text, style, "{}", c);
                }
                ui.text.close_line();
            }
            let end_line = ui.text.num_lines();
            ui.get_mut(idx).draw_text = Some(start_line..end_line);
        }

        // Build tabs (aka window titles) for leaf regions, assign active_window.
        self.active_window = None;
        for (region_id, region) in self.regions.iter_mut() {
            if !region.content.is_leaf() {
                continue;
            }
            let leaf = region.content.as_leaf_mut();
            let is_active = self.active_leaf == region_id;

            with_parent!(ui, leaf.content_widget, {
                if is_active {
                    // Focus window content before region title tabs, so that the window can intercept tab switching keys if it wants.
                    ui.multifocus();
                }
            });

            with_parent!(ui, leaf.tabs_widget, {
                if is_active {
                    ui.multifocus();
                }

                let mut tabs = Tabs::new(mem::take(&mut leaf.tabs_state), ui);
                let style = if is_active {ui.palette.window_title_active} else {ui.palette.window_title_selected};
                tabs.custom_styles = Some((style, (String::new(), ui.palette.window_title_deselected), ui.palette.window_title_deselected, ui.palette.window_title_separator.clone()));
                for &window_id in &leaf.tabs {
                    let win = self.windows.get(window_id);
                    tabs.add(Tab {identity: hash(&window_id), short_title: win.title.clone(), hotkey_number: win.hotkey_number.clone(), ..D!()}, ui);
                }
                (leaf.tabs_state, _) = tabs.finish(ui);
            });

            for (i, &window_id) in leaf.tabs.iter().enumerate() {
                let win = self.windows.get_mut(window_id);
                if i == leaf.tabs_state.selected {
                    win.visible = true;
                    win.area = region.area;
                    win.widget = leaf.content_widget;
                    if is_active {
                        self.active_window = Some(window_id);
                    }
                } else {
                    win.widget = ui.add(widget!().identity(&('u', window_id)).fixed_rect(Rect::default()));
                }
            }
        }

        if self.active_window.is_none() {
            assert!(self.regions.get(self.active_leaf).content.as_leaf().tabs.is_empty());
            for (id, win) in self.windows.iter() {
                if win.visible {
                    self.active_window = Some(id);
                    ui.should_redraw = true;
                    break;
                }
            }
        }
    }

    fn build_split(&mut self, region_id: RegionId, ui: &mut UI) {
        let region = self.regions.get(region_id);
        let split = region.content.as_split();
        assert!(!split.children.is_empty());
        let axis = split.axis;

        // walls[i] is between children[i-1] and children[i].
        // children[i] is between walls[i] and walls[i+1].
        let mut walls = vec![(WidgetIdx::invalid(), 0isize); split.children.len() + 1];
        walls[0] = (region.outer_walls[axis][0], -1);
        *walls.last_mut().unwrap() = (region.outer_walls[axis][1], region.area.size[axis] as isize);

        let mut children: Vec<(/*is_fixed*/ bool, /*size*/ usize, RegionId)> = split.children.iter().map(|child_id| {
            let child = self.regions.get(*child_id);
            match &child.content {
                RegionContent::Leaf(l) if l.tabs.len() == 1 => if let &Some(s) = &self.windows.get(l.tabs[0]).fixed_size[axis] {
                    return (true, s, *child_id);
                }
                _ => (),
            }
            (false, child.relative_size, *child_id)
        }).collect();

        // Cumulative sums of fixed and flexible sizes. cumsum[i] describes the space before walls[i].
        let calculate_cumsum = |children: &[(bool, usize, RegionId)]| -> Vec<(/*fixed*/ usize, /*flexible*/ usize)> {
            let mut res: Vec<(usize, usize)> = Vec::new();
            res.reserve(children.len() + 1);
            res.push((0, 0));
            for (i, &(is_fixed, size, _)) in children.iter().enumerate() {
                let mut s = res.last().unwrap().clone();
                if is_fixed {
                    s.0 += size;
                } else {
                    s.1 += size;
                }
                if i > 0 {
                    s.0 += 1; // wall
                }
                res.push(s);
            }
            res
        };

        let size = region.area.size[axis];
        let mut cumsum = calculate_cumsum(&children);

        let calculate_wall_pos = |i: usize, cumsum: &[(usize, usize)]| -> (isize, /*draggable*/ bool) {
            // Fixed size on the left (not counting this wall) and in total (counting this wall of size 1).
            let (fixed_left, fixed) = (cumsum[i].0, cumsum.last().unwrap().0);
            // Flexible size on the left and total.
            let (flexible_left, flexible) = (cumsum[i].1, cumsum.last().unwrap().1);

            if fixed >= size || flexible_left == 0 {
                return (fixed_left as isize, false);
            }
            ((fixed_left + ((size - fixed) * flexible_left + flexible/2) / flexible) as isize, flexible_left < flexible)
        };

        // Create inner walls, handle mouse-drag resizing.
        let mut resized = false;
        for i in 1..walls.len()-1 {
            let (pos, draggable) = calculate_wall_pos(i, &cumsum);
            walls[i] = (Self::build_wall(axis, region.area, pos, hash(&('w', region_id, i)), &mut self.wall_widgets, ui), pos);

            if !draggable {
                continue;
            }
            with_parent!(ui, walls[i].0, {
                ui.cur_mut().flags.insert(WidgetFlags::HIGHLIGHT_ON_HOVER);
                if let Some(p) = ui.check_drag_out(DragWhat::NoDrop) {
                    let p = p[axis];
                    if p != 0 {
                        let (fixed_left, fixed) = (cumsum[i].0, cumsum.last().unwrap().0);
                        let (flexible_left, flexible) = (cumsum[i].1, cumsum.last().unwrap().1);

                        let p = pos + p; // where we want the wall to be
                        let p = (p.max(fixed_left as isize) as usize).min(size - fixed + fixed_left); // clamp
                        // Find the flexible_left that solves the linear equation calculate_wall_pos(...) = p.
                        let target_left = flexible * (p - fixed_left) / (size - fixed);

                        // Adjust flexible sizes of the two children on either side of this wall (skipping fixed-size children).
                        let (mut prev, mut next) = (i-1, i);
                        while children[prev].0 {
                            prev -= 1;
                        }
                        while children[next].0 {
                            next += 1;
                        }
                        assert_eq!(cumsum[next+1].1 - cumsum[prev].1, children[prev].1 + children[next].1);
                        let target_left = target_left.max(cumsum[prev].1 + 1).min(cumsum[next+1].1 - 1);
                        children[prev].1 = target_left - cumsum[prev].1;
                        children[next].1 = cumsum[next+1].1 - target_left;
                        
                        resized = true;
                        cumsum = calculate_cumsum(&children);
                    }
                }
            });
        }
        // Move wall widgets into final positions and add the lines.
        for i in 1..walls.len()-1 {
            let pos = calculate_wall_pos(i, &cumsum).0;
            walls[i].1 = pos;
            ui.get_mut(walls[i].0).axes[axis].rel_pos = region.area.pos[axis] + pos;
            Self::trace_wall(axis, region.area, pos, false, &mut self.wall_masks, &self.wall_area);
        }

        // Propagate information to children.
        let region_area = region.area;
        let outer_walls = region.outer_walls.clone();
        for (i, &(is_fixed, child_size, child_id)) in children.iter().enumerate() {
            let child = self.regions.get_mut(child_id);
            if resized && !is_fixed {
                child.relative_size = child_size;
            }

            child.area = region_area;
            child.area.pos[axis] += walls[i].1 + 1;
            child.area.size[axis] = (walls[i+1].1 - walls[i].1 - 1).max(0) as usize;

            child.outer_walls = outer_walls.clone();
            child.outer_walls[axis] = [walls[i].0, walls[i+1].0];
        }
    }
}

// Bitmask -> char.
// Bits 0-3 - which of the 4 directions are single lines (left, right, up, down).
// Bits 4-7 - which of the present directions are altered. Single line gets altered into bold single line. Non-single-line gets altered to double line.
// Unicode has all 3^4 = 81 combinations of regular and bold lines, but only some combinations with double lines, and no combinations of double+bold lines.
// This table has '?' for combinations that don't have a character in the Box Drawing unicode block.
const BOX_DRAWING_LUT: [char; 256] = [
    /* ' ' */ ' ', '╴', '╶', '─', '╵', '┘', '└', '┴', '╷', '┐', '┌', '┬', '│', '┤', '├', '┼',
    /* '╸' */ '?', '╸', '?', '╾', '╛', '┙', '?', '┵', '╕', '┑', '?', '┭', '╡', '┥', '?', '┽',
    /* '╺' */ '?', '?', '╺', '╼', '╘', '?', '┕', '┶', '╒', '?', '┍', '┮', '╞', '?', '┝', '┾',
    /* '━' */ '═', '?', '?', '━', '╧', '?', '?', '┷', '╤', '?', '?', '┯', '╪', '?', '?', '┿',
    /* '╹' */ '?', '╜', '╙', '╨', '╹', '┚', '┖', '┸', '?', '?', '?', '?', '╿', '┦', '┞', '╀',
    /* '┛' */ '╝', '?', '?', '?', '?', '┛', '?', '┹', '?', '?', '?', '?', '?', '┩', '?', '╃',
    /* '┗' */ '╚', '?', '?', '?', '?', '?', '┗', '┺', '?', '?', '?', '?', '?', '?', '┡', '╄',
    /* '┻' */ '╩', '?', '?', '?', '?', '?', '?', '┻', '?', '?', '?', '?', '?', '?', '?', '╇',
    /* '╻' */ '?', '╖', '╓', '╥', '?', '?', '?', '?', '╻', '┒', '┎', '┰', '╽', '┧', '┟', '╁',
    /* '┓' */ '╗', '?', '?', '?', '?', '?', '?', '?', '?', '┓', '?', '┱', '?', '┪', '?', '╅',
    /* '┏' */ '╔', '?', '?', '?', '?', '?', '?', '?', '?', '?', '┏', '┲', '?', '?', '┢', '╆',
    /* '┳' */ '╦', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '┳', '?', '?', '?', '╈',
    /* '┃' */ '║', '╢', '╟', '╫', '?', '?', '?', '?', '?', '?', '?', '?', '┃', '┨', '┠', '╂',
    /* '┫' */ '╣', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '┫', '?', '╉',
    /* '┣' */ '╠', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '┣', '╊',
    /* '╋' */ '╬', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '╋',
];
