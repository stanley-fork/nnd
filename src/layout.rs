use crate::{*, ui::*, widgets::*, pool::*, settings::*, imgui::*, common_ui::*, util::*};
use std::{mem, collections::HashSet, hash::{Hash, Hasher}, ops::Range};

pub struct Layout {
    pub windows: Pool<Window>,
    pub active_window: Option<WindowId>,
    pub regions: Pool<Region>,
    pub root: RegionId,
    // Which walls surrounding the whole UI to draw. [axis][-/+]
    // The top ([1][0]) is strongly recommended, otherwise there's nowhere to put titles of single-window regions.
    pub outer_walls: [[bool; 2]; 2],
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
    fn clear_layout(&mut self) { self.area = Rect::default(); self.outer_walls = [[WidgetIdx::invalid(); 2]; 2]; match &mut self.content { RegionContent::Leaf(leaf) => leaf.widget = WidgetIdx::invalid(), _ => () } }
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
    pub widget: WidgetIdx,
}

pub enum RegionContent {
    Split(RegionSplit),
    Leaf(RegionLeaf),
}

impl RegionContent {
    pub fn as_split(&self) -> &RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
    pub fn as_leaf(&self) -> &RegionLeaf { match self { RegionContent::Leaf(x) => x, RegionContent::Split(_) => panic!("not a leaf"), } }
    pub fn as_leaf_mut(&mut self) -> &mut RegionLeaf { match self { RegionContent::Leaf(x) => x, RegionContent::Split(_) => panic!("not a leaf"), } }
    pub fn as_split_mut(&mut self) -> &mut RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
}

impl Layout {
    pub fn new() -> Layout {
        let mut regions: Pool<Region> = Pool::new();
        let root = regions.add(Region {parent: None, area: Rect::default(), content: RegionContent::Leaf(RegionLeaf::default()), relative_size: 1000000, outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0;
        Layout {windows: Pool::new(), active_window: None, regions, root, outer_walls: [[false, false], [true, false]]}
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
        let new_region = self.regions.add(Region {parent: parent.clone(), content: RegionContent::Split(RegionSplit {axis, children: Vec::new()}), relative_size, area: Rect::default(), outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0;

        let mut children = vec![region_id];
        let denominator = 1000000.0;
        for i in 0..walls.len() {
            let end = if i + 1 < walls.len() {walls[i+1]} else {1.0};
            let relative_size = (((end - walls[i]) * denominator) as usize).max(1);
            children.push(self.regions.add(Region {parent: Some(new_region), content: RegionContent::Leaf(RegionLeaf {tabs: Vec::new(), tabs_state: TabsState::default(), widget: WidgetIdx::invalid()}), relative_size, area: Rect::default(), outer_walls: [[WidgetIdx::invalid(); 2]; 2]}).0);
        }
        let region = self.regions.get_mut(region_id);
        region.relative_size = ((walls[0] * denominator) as usize).max(1);
        region.content.as_split_mut().children = children.clone();
        region.parent = Some(new_region);

        match parent {
            None => self.root = new_region,
            Some(parent_id) => {
                let parent_split = self.regions.get_mut(parent_id).content.as_split_mut();
                let i = parent_split.children.iter().position(|x| x == &region_id).unwrap();
                parent_split.children[i] = new_region;
            }
        }

        self.simplify_splits(new_region);

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

    pub fn new_window(&mut self, parent: Option<RegionId>, type_: WindowType, required: bool, title: String, content: Box<dyn WindowContent>) -> WindowId {
        let win = Window {type_, region: parent.clone(), visible: false, area: Rect::default(), widget: WidgetIdx::invalid(), required, title, content, hotkey_number: None, fixed_size: [None; 2]};
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

    pub fn activate_any_visible_window_if_none_active(&mut self) -> bool {
        if let &Some(id) = &self.active_window {
            let win = self.windows.get(id);
            if win.visible {
                return true;
            }
            if let &Some(region_id) = &win.region {
                let region = self.regions.get(region_id);
                for &w in &region.content.as_leaf().tabs {
                    if self.windows.get(w).visible {
                        self.active_window = Some(w);
                        return true;
                    }
                }
            }
        }
        for (id, win) in self.windows.iter() {
            if win.visible {
                self.active_window = Some(id);
                return true;
            }
        }
        false
    }

    pub fn window_at_point(&self, p: [isize; 2]) -> Option<WindowId> {
        for (id, win) in self.windows.iter() {
            if win.area.contains(p.clone()) {
                return Some(id);
            }
        }
        None
    }

    pub fn switch_to_adjacent_window(&mut self, dx: isize, dy: isize) {
        if !self.activate_any_visible_window_if_none_active() {
            return;
        }
        let a = self.windows.get(self.active_window.unwrap()).area.clone();
        // Take a point just to the left/right/up/down of the rect, next to the middle of the corresponding edge.
        // Then find a window containing that point.
        let point = match (dx, dy) {
            (-1, 0) => [a.x() - 2, a.y() + a.height() as isize / 2],
            (1, 0) => [a.right() + 2, a.y() + a.height() as isize / 2],
            (0, -1) => [a.x() + a.width() as isize / 2, a.y() - 2],
            (0, 1) => [a.x() + a.width() as isize / 2, a.bottom() - 2],
            _ => panic!("unexpected args to switch_to_adjacent_window"),
        };
        if let Some(id) = self.window_at_point(point) {
            self.active_window = Some(id);
            return;
        }
        // Maybe point landed on a wall. Nudge it in perpendicular direction and try again.
        let point = [point[0] + dy, point[1] - dx];
        if let Some(id) = self.window_at_point(point) {
            self.active_window = Some(id);
        }        
    }

    pub fn switch_to_window_with_hotkey_number(&mut self, number: usize) {
        for (id, win) in self.windows.iter() {
            if win.hotkey_number == Some(number) {
                self.active_window = Some(id);
                return;
            }
        }
    }

    // (this function is considering leaving this codebase to pursue a career in politics)
    fn build_wall(axis: usize, area: Rect, pos: isize, identity: usize, imgui: &mut IMGUI) -> WidgetIdx {
        let mut w = widget!().fixed_width(0).fixed_height(0).fixed_x(0).fixed_y(0);
        w.identity = identity;
        match axis {
            Axis::X => w = w.fill('│', imgui.palette.window_border).fixed_width(1).fixed_height(area.height()).fixed_x(area.x() + pos).fixed_y(area.y()),
            Axis::Y => w = w.fill('─', imgui.palette.window_border).fixed_height(1).fixed_width(area.width()).fixed_y(area.y() + pos).fixed_x(area.x()),
            _ => panic!("huh"),
        }
        imgui.add(w)
    }

    pub fn build(&mut self, imgui: &mut IMGUI) {
        //asdqwe handle window switching keys

        let root = self.regions.get_mut(self.root);
        root.clear_layout();
        root.area = imgui.cur().get_rect_assume_fixed();

        // Build outer walls if needed.
        for axis in 0..2 {
            for side in 0..2 {
                if !self.outer_walls[axis][side] || root.area.size[axis] == 0 {
                    continue;
                }
                let w = Self::build_wall(axis, root.area, if side == 0 {0} else {root.area.end(axis)-1}, hash(&('o', axis, side)), imgui);
                root.outer_walls[axis][side] = w;
                root.area.size[axis] -= 1;
                if side == 0 {
                    root.area.pos[axis] += 1;
                }
            }
        }

        // Do layout and build walls.
        let mut stack = vec![self.root];
        let mut visited_regions: HashSet<RegionId> = HashSet::new();
        let mut visited_windows: HashSet<WindowId> = HashSet::new();
        while let Some(region_id) = stack.pop() {
            let ins = visited_regions.insert(region_id);
            assert!(ins);
            let region = self.regions.get(region_id);
            let area = region.area;

            match &region.content {
                RegionContent::Leaf(leaf) => {
                    for &window_id in &leaf.tabs {
                        let ins = visited_windows.insert(window_id);
                        assert!(ins);
                        assert_eq!(self.windows.get(window_id).region, Some(region_id));
                    }
                    self.build_leaf(region_id, imgui);
                }
                RegionContent::Split(_) => {
                    self.build_split(region_id, imgui);
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

        self.activate_any_visible_window_if_none_active();

        //asdqwe draw window titles, fuse two-sided junctions, overdraw active window outline, add window widgets, reorder leaf widgets to put the active on last
    }

    fn build_split(&mut self, region_id: RegionId, imgui: &mut IMGUI) {
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
                if i + 1 < children.len() {
                    s.0 += 1; // wall
                }
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
            walls[i] = (Self::build_wall(axis, region.area, pos, hash(&('w', region_id, i)), imgui), pos);

            if !draggable {
                continue;
            }
            with_parent!(imgui, walls[i].0, {
                imgui.cur_mut().flags.insert(WidgetFlags::HIGHLIGHT_ON_HOVER);
                if let Some(p) = imgui.check_drag() {
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
        if resized { // move the wall widgets if we adjusted children sizes
            for i in 1..walls.len()-1 {
                let pos = calculate_wall_pos(i, &cumsum).0;
                walls[i].1 = pos;
                imgui.get_mut(walls[i].0).axes[axis].rel_pos = region.area.pos[axis] + pos;
            }
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
            child.area.pos[axis] = walls[i].1 + 1;
            child.area.size[axis] = (walls[i+1].1 - walls[i].1 - 1).max(0) as usize;

            child.outer_walls = outer_walls.clone();
            child.outer_walls[axis] = [walls[i].0, walls[i+1].0];
        }

        //asdqwe add junctions
    }

    fn build_leaf(&mut self, region_id: RegionId, imgui: &mut IMGUI) {
        let region = self.regions.get_mut(region_id);
        let leaf = region.content.as_leaf_mut();

        leaf.widget = imgui.add(widget!().identity(&('l', region_id)).fixed_rect(region.area));
        let mut content_widget = leaf.widget;
        with_parent!(imgui, leaf.widget, {
            let mut content_area = region.area;
            if leaf.tabs.len() == 1 {
                leaf.tabs_state.select(0);
            } else if leaf.tabs.len() > 1 {
                imgui.cur_mut().axes[Axis::Y].flags.insert(AxisFlags::STACK);
                let tabs_widget = imgui.add(widget!().identity(&'t').fixed_height(1));
                content_widget = imgui.add(widget!().identity(&'w').height(AutoSize::Remainder(1.0)));
                imgui.layout_children(Axis::Y);
                if content_area.height() > 0 {
                    content_area.size[Axis::Y] -= 1;
                    content_area.pos[Axis::Y] += 1;
                }

                with_parent!(imgui, content_widget, {
                    imgui.relative_focus(leaf.widget);
                });
                with_parent!(imgui, tabs_widget, {
                    imgui.multifocus();
                    let mut tabs = Tabs::new(mem::take(&mut leaf.tabs_state), imgui);
                    for &window_id in &leaf.tabs {
                        let window = self.windows.get(window_id);
                        tabs.add(&window.title, &window.title, false, window.hotkey_number.clone(), imgui);
                    }
                    leaf.tabs_state = tabs.finish(imgui);
                });
            }

            for (i, &window_id) in leaf.tabs.iter().enumerate() {
                let window = self.windows.get_mut(window_id);
                if i == leaf.tabs_state.selected {
                    window.visible = true;
                    window.area = content_area;
                    window.widget = content_widget;

                    if imgui.check_mouse(MouseActions::CLICK_SUBTREE) {
                        self.active_window = Some(window_id);
                    }
                } else {
                    window.widget = imgui.add(widget!().identity(&('u', window_id)).fixed_width(0).fixed_height(0));
                }
            }
        });
    }
}
