use crate::{ui::*, widgets::*, pool::*};
use tui::{self, widgets::{Block, Borders, Tabs, BorderType}, layout::{self, Constraint, Direction, Rect, Alignment}, style::{Style, Color, Modifier}, text::{Span, Spans, Text}};
use std::{mem::take, collections::HashSet, hash::{Hash, Hasher}, ops::Range};

pub struct Layout {
    pub windows: Pool<Window>,
    pub active_window: Option<WindowId>,
    pub regions: Pool<Region>,
    pub root: RegionId,
}

pub type WindowId = Id;
pub type RegionId = Id;

pub struct Window {
    pub type_: WindowType,
    pub parent: Option<RegionId>,
    pub outer_area: Option<Rect>,
    pub area: Option<Rect>,
    pub required: bool,
    pub title: String,
    pub content: Box<dyn WindowContent>,
}

pub struct Region {
    pub parent: Option<RegionId>,
    pub area: Rect,
    pub content: RegionContent,

    // A very crude mechanism for making some windows have fixed height instead of scaling with terminal size. Important for hints and status windows.
    // Not integrated well with `walls`, won't work with arbitrary layout (e.g. if rigid children are not at the start) and when the user resizes stuff,
    // will need to be fixed when implementing dynamic layout controls.
    pub rigid_size: Option<usize>,
}

pub struct RegionSplit {
    pub direction: Direction,
    pub walls: Vec<f64>,
    pub children: Vec<RegionId>,
}

pub struct RegionLeaf {
    pub tabs: Vec<WindowId>,
    pub active_tab: usize,
}

pub enum RegionContent {
    Split(RegionSplit),
    Leaf(RegionLeaf),
}

impl RegionContent {
    pub fn as_split(&self) -> &RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
    pub fn as_leaf_mut(&mut self) -> &mut RegionLeaf { match self { RegionContent::Leaf(x) => x, RegionContent::Split(_) => panic!("not a leaf"), } }
    pub fn as_split_mut(&mut self) -> &mut RegionSplit { match self { RegionContent::Split(x) => x, RegionContent::Leaf(_) => panic!("not a split"), } }
}

impl RegionSplit {
    pub fn wall(&self, idx: usize) -> f64 {
        match idx {
            0 => 0.0,
            i if i == self.walls.len()+1 => 1.0,
            i => *self.walls.get(i-1).unwrap(),
        }
    }

    pub fn splice(&mut self, to_remove: Range<usize>, new_children: Vec<RegionId>, mut new_relative_walls: Vec<f64>) {
        // Example:
        //
        //    w0   w1   w2   w3  w4
        // c0 | c1 | c2 | c3 | c4 | c5
        //           ^^^^^^^
        // to_remove: [2, 4)
        // adding 3 children (2 walls)
        //
        //    w0   w1   u0   u1   w3   w4
        // c0 | c1 | n0 | n1 | n2 | c4 | c5
        //           ^^^^^^^^^^^^

        let left = self.wall(to_remove.start);
        let right = self.wall(to_remove.end);
        
        if new_children.is_empty() {
            assert!(new_relative_walls.is_empty());
            if to_remove.end == self.children.len() {
                if to_remove.start == 0 {
                    self.walls.clear();
                } else {
                    self.walls.splice(to_remove.start-1..to_remove.end-1, []);
                }
            } else {
                self.walls.splice(to_remove.clone(), []);
                if to_remove.start != 0 {
                    // Split the removed space in half between the two neighbors.
                    self.walls[to_remove.start-1] = (left + right) * 0.5;
                }
            }
            self.children.splice(to_remove, []);
            return;
        }

        assert!(new_relative_walls.len() == new_children.len() - 1);
        for x in &mut new_relative_walls {
            *x = left + (right - left) * *x;
        }
        self.walls.splice(to_remove.start..to_remove.end-1, new_relative_walls);
        self.children.splice(to_remove, new_children);
    }
}

impl Layout {
    pub fn new() -> Layout {
        let mut regions: Pool<Region> = Pool::new();
        let root = regions.add(Region {parent: None, area: Rect::default(), rigid_size: None, content: RegionContent::Leaf(RegionLeaf {tabs: Vec::new(), active_tab: 0})}).0;
        Layout {windows: Pool::new(), active_window: None, regions: regions, root: root}
    }

    pub fn split(&mut self, region_id: RegionId, direction: Direction, walls: Vec<f64>) -> Vec<RegionId> {
        for i in 1..walls.len() {
            assert!(walls[i] >= walls[i-1]);
            assert!(walls[i] <= 1.0);
            assert!(walls[i-1] >= 0.0);
        }

        let region = self.regions.get_mut(region_id);

        match &region.content {
            // This check ensures that none of the `children` below get simplified out. Otherwise we wouldn't be able to return a meaningful list of regions from this function.
            RegionContent::Split(split) if split.direction == direction => panic!("splitting in same direction is not allowed"),
            _ => () }

        let parent = region.parent.take();

        let mut children = vec![region_id];
        for i in 0..walls.len() {
            children.push(self.regions.add(Region {parent: None, area: Rect::default(), rigid_size: None, content: RegionContent::Leaf(RegionLeaf {tabs: Vec::new(), active_tab: 0})}).0);
        }

        let new_region = self.regions.add(Region {parent: parent.clone(), area: Rect::default(), rigid_size: None, content: RegionContent::Split(RegionSplit {direction: direction, walls: walls, children: children.clone()})}).0;

        for &child_id in &children {
            self.regions.get_mut(child_id).parent = Some(new_region);
        }

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

    pub fn rigidify(&mut self, region_id: RegionId, size: usize) {
        self.regions.get_mut(region_id).rigid_size = Some(size);
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
                if parent_split.direction != split.direction && split.children.len() > 1 {
                    break;
                }
                let idx = parent_split.children.iter().position(|x| x == &region_id).unwrap();
                let split = self.regions.get_mut(region_id).content.as_split_mut();
                let children = take(&mut split.children);
                let walls = take(&mut split.walls);

                for &child_id in &children {
                    let child = self.regions.get_mut(child_id);
                    child.parent = Some(parent_id);
                }

                let parent_split = self.regions.get_mut(parent_id).content.as_split_mut();
                parent_split.splice(idx..idx+1, children.clone(), walls);

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
        self.regions.get_mut(leaf_id).content.as_leaf_mut().tabs.push(win_id);
        self.windows.get_mut(win_id).parent = Some(leaf_id);
    }

    pub fn new_window(&mut self, parent: Option<RegionId>, type_: WindowType, required: bool, title: String, content: Box<dyn WindowContent>) -> WindowId {
        let win = Window {type_, outer_area: None, area: None, parent: parent.clone(), required, title, content};
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
            if win.area.is_some() {
                return true;
            }
        }
        for (id, win) in self.windows.iter() {
            if win.area.is_some() {
                self.active_window = Some(id);
                return true;
            }
        }
        false
    }

    pub fn window_at_point(&self, p: (u16, u16)) -> Option<WindowId> {
        for (id, win) in self.windows.iter() {
            if let &Some(a) = &win.outer_area {
                if a.x <= p.0 && a.right() > p.0 && a.y <= p.1 && a.bottom() > p.1 {
                    return Some(id);
                }
            }
        }
        None
    }

    pub fn switch_to_adjacent_window(&mut self, dx: isize, dy: isize) {
        if !self.activate_any_visible_window_if_none_active() {
            return;
        }
        let area = self.windows.get(self.active_window.unwrap()).outer_area.clone().unwrap();
        // Take a point just to the left/right/up/down of the rect, next to the middle of the corresponding edge.
        // Then find a window containing that point.
        let point = match (dx, dy) {
            (-1, 0) => (area.x.wrapping_sub(1), area.y + area.height/2),
            (1, 0) => (area.right().saturating_add(1), area.y + area.height/2),
            (0, -1) => (area.x + area.width/2, area.y.wrapping_sub(1)),
            (0, 1) => (area.x + area.width/2, area.bottom().saturating_add(1)),
            _ => panic!("unexpected args to switch_to_adjacent_window"),
        };
        if let Some(id) = self.window_at_point(point) {
            self.active_window = Some(id);
        }
    }

    pub fn switch_to_numbered_window(&mut self, idx: usize) {
        //asdqwe
    }

    pub fn switch_tab_in_active_window(&mut self, delta: isize) {
        let win_id = match &self.active_window {
            &Some(id) => id,
            &None => return };
        let win = self.windows.get_mut(win_id);
        let region_id = match &win.parent {
            &Some(id) => id,
            &None => return };
        let leaf = self.regions.get_mut(region_id).content.as_leaf_mut();
        leaf.active_tab = (leaf.active_tab as isize + delta).rem_euclid(leaf.tabs.len().max(1) as isize) as usize;
        self.active_window = Some(leaf.tabs[leaf.active_tab]);
    }

    pub fn layout_and_render_peripherals(&mut self, f: &mut Frame, area: Rect) {
        let mut reachable_regions: HashSet<RegionId> = HashSet::new();
        let mut stack = vec![(area, self.root, None)];
        while let Some((area, region_id, parent_id)) = stack.pop() {
            reachable_regions.insert(region_id);
            let region = self.regions.get_mut(region_id);
            assert!(region.parent == parent_id);
            region.area = area;
            let region = self.regions.get(region_id);
            match &region.content {
                RegionContent::Leaf(leaf) if !leaf.tabs.is_empty() => {
                    let mut block = Block::default().borders(Borders::LEFT | Borders::RIGHT | Borders::BOTTOM | Borders::TOP);
                    let show_tabs = leaf.tabs.len() > 1;
                    let area = block.inner(region.area);
                    let mut inner = area;
                    if show_tabs {
                        inner.y += 1;
                        inner.height = inner.height.saturating_sub(1);
                    }

                    let mut is_active = false;
                    let mut tabs: Vec<Spans> = Vec::new();
                    for i in 0..leaf.tabs.len() {
                        let win_id = leaf.tabs[i];
                        let win = self.windows.get_mut(win_id);
                        if i == leaf.active_tab {
                            win.outer_area = Some(region.area);
                            win.area = Some(inner);
                        } else {
                            win.outer_area = None;
                            win.area = None;
                        }
                        let mut style = Style::default();
                        if self.active_window == Some(win_id) {
                            is_active = true;
                        } else {
                            style = style.add_modifier(Modifier::DIM);
                        }
                        let spans = Spans::from(Span::styled(win.title.clone(), style));
                        if leaf.tabs.len() == 1 {
                            block = block.title(spans);
                        } else {
                            tabs.push(spans);
                        }
                    }

                    let style;
                    let border_type;
                    if is_active {
                        style = Style::default().add_modifier(Modifier::BOLD);
                        border_type = BorderType::Thick;
                    } else {
                        style = Style::default().add_modifier(Modifier::DIM);
                        border_type = BorderType::Rounded;
                    }
                    block = block.border_style(style);
                    block = block.border_type(border_type);

                    f.render_widget(block, region.area);
                    if show_tabs {
                        let mut a = area.clone();
                        if a.height > 0 {
                            let tabs = Tabs::new(tabs).select(leaf.active_tab).highlight_style(style.bg(Color::White).fg(Color::Black));
                            a.height = 1;
                            f.render_widget(tabs, a);
                        }
                    }
                }
                RegionContent::Leaf(_) => (),
                RegionContent::Split(split) => {
                    let size = match split.direction { Direction::Horizontal => area.width, Direction::Vertical => area.height };
                    let mut prev = 0u16;
                    for i in 0..split.children.len() {
                        let mut wall = split.walls.get(i).map_or(size, |x| (size as f64 * x).round() as u16);
                        wall = wall.max(prev);

                        let child = self.regions.get(split.children[i]);
                        if let &Some(s) = &child.rigid_size {
                            wall = prev + (s + 2).min((size - prev) as usize) as u16;
                        }
                        
                        let mut subarea = area;
                        match split.direction {
                            Direction::Horizontal => { subarea.x = prev; subarea.width = wall - prev; }
                            Direction::Vertical => { subarea.y = prev; subarea.height = wall - prev; }
                        }
                        prev = wall;
                        stack.push((subarea, split.children[i], Some(region_id)));
                    }
                }
            }
        }

        // Some invariant checks.
        for (id, _) in self.regions.iter() {
            if !reachable_regions.contains(&id) {
                panic!("unreachable region detected");
            }
        }
        for (id, win) in self.windows.iter() {
            assert!(win.outer_area.is_some() == win.area.is_some());
            assert!(win.parent.is_some() || win.area.is_none());
        }
    }
}
