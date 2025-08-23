use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Id {
    pub slot: usize,
    pub seqno: usize,
}

struct Slot<T> {
    seqno: usize,
    val: Option<T>,
}

pub struct Pool<T> {
    slots: Vec<Slot<T>>,
    vacant: Vec<usize>,
    next_seqno: usize,
}

impl Default for Id {
    fn default() -> Id {
        Id {slot: usize::MAX, seqno: usize::MAX}
    }
}

impl<T> Pool<T> {
    pub fn new() -> Self {
        Self {slots: Vec::new(), vacant: Vec::new(), next_seqno: 1}
    }

    pub fn add(&mut self, x: T) -> (Id, &mut T) {
        let seqno = self.next_seqno;
        self.next_seqno += 1;
        if let Some(slot) = self.vacant.pop() {
            let s = &mut self.slots[slot];
            s.seqno = seqno;
            s.val = Some(x);
            (Id {slot, seqno}, s.val.as_mut().unwrap())
        } else {
            self.slots.push(Slot {seqno, val: Some(x)});
            (Id {slot: self.slots.len() - 1, seqno},  self.slots.last_mut().unwrap().val.as_mut().unwrap())
        }
    }

    pub fn remove(&mut self, id: Id) -> Option<T> {
        let s = match self.slots.get_mut(id.slot) {
            None => return None,
            Some(s) => s };
        if s.seqno != id.seqno {
            return None;
        }
        let v = s.val.take();
        if v.is_some() {
            self.vacant.push(id.slot);
        }
        v
    }

    pub fn clear(&mut self) {
        for slot in 0..self.slots.len() {
            if self.slots[slot].val.take().is_some() {
                self.vacant.push(slot);
            }
        }
    }

    pub fn try_get(&self, id: Id) -> Option<&T> {
        let s = match self.slots.get(id.slot) {
            None => return None,
            Some(s) => s };
        if s.seqno != id.seqno {
            return None;
        }
        s.val.as_ref()
    }

    pub fn try_get_mut(&mut self, id: Id) -> Option<&mut T> {
        let s = match self.slots.get_mut(id.slot) {
            None => return None,
            Some(s) => s };
        if s.seqno != id.seqno {
            return None;
        }
        s.val.as_mut()
    }

    pub fn get(&self, id: Id) -> &T { self.try_get(id).unwrap() }
    pub fn get_mut(&mut self, id: Id) -> &mut T { self.try_get_mut(id).unwrap() }

    pub fn iter(&self) -> PoolIter<'_, T> {
        PoolIter {pool: self, slot: 0}
    }

    pub fn iter_mut(&mut self) -> PoolIterMut<'_, T> {
        PoolIterMut {iter: self.slots.iter_mut(), idx: 0, slot: 0}
    }
}

pub struct PoolIter<'a, T> {
    pool: &'a Pool<T>,
    slot: usize,
}

impl<'a, T> Iterator for PoolIter<'a, T> {
    type Item = (Id, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let mut slot = self.slot;
        while slot < self.pool.slots.len() && self.pool.slots[slot].val.is_none() {
            slot += 1;
        }
        self.slot = slot + 1;
        match self.pool.slots.get(slot) {
            None => None,
            Some(s) => Some((Id {slot, seqno: s.seqno}, s.val.as_ref().unwrap()))
        }
    }
}

pub struct PoolIterMut<'a, T> where T: 'a {
    iter: std::slice::IterMut<'a, Slot<T>>,
    idx: usize,
    slot: usize,
}

impl<'a, T: 'a> Iterator for PoolIterMut<'a, T> {
    type Item = (Id, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let idx = self.idx;
            self.idx += 1;
            match self.iter.next() {
                None => return None,
                Some(slot) => match &mut slot.val {
                    None => (),
                    Some(val) => return Some((Id {slot: idx, seqno: slot.seqno}, val)),
                }
            }
        }
    }
}
