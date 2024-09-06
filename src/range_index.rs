use crate::{util::*};
use std::ops::Range;

pub struct RangeIndexEntry<T> {
    pub start: usize,
    pub end: usize,
    // Max value of `end` among this entry and all entries before it.
    pub max_end: usize,
    pub value: T,
}

pub struct RangeIndex<T> {
    // Sorted by (start, value).
    pub ranges: Vec<RangeIndexEntry<T>>,
}

impl<T: Ord + Clone> RangeIndex<T> {
    pub fn new(mut ranges: Vec<RangeIndexEntry<T>>, name_for_logging: &str) -> Self {
        ranges.shrink_to_fit();
        // Sort by value when start is equal. This makes inlined function work correctly automatically: the innermost function will be returned first.
        ranges.sort_unstable_by_key(|r| (r.start, r.value.clone()));
        let mut max_end = 0usize;
        let mut max_idx = 0usize;
        for i in 0..ranges.len() {
            if i - max_idx > 100000 {
                // We don't expect any address range (variable, function, or inlined function) to overlap a lot of other ranges.
                // If it does, this data structure gets very inefficient. If this turns out to happen in practice, we should switch
                // to a different data structure (and maybe we should switch anyway, this doesn't seem super efficient or compact even in good conditions).
                // This sanity check limits how slow we can get, but the index lookups will be missing entries (not just the bad entry
                // that we're skipping, some other entries too because we're reverting max_end to current range end rather than the second-max).
                eprintln!("warning: range {}-{} covers over {} other ranges (in {}). This is unexpectedly many, please investigate.", ranges[max_idx].start, ranges[max_idx].end, i - max_idx - 1, name_for_logging);
                max_end = ranges[i].end;
                max_idx = i;
            } else if ranges[i].end >= max_end {
                max_end = ranges[i].end;
                max_idx = i;
            }
            ranges[i].max_end = max_end;
        }

        Self {ranges: ranges}
    }

    // Index of the first entry with start > addr (or len() if there are none).
    // To get all entries containing addr, walk backwards:
    //   while idx > 0 && ranges[idx-1].max_end > addr {
    //       idx -= 1;
    //       if ranges[idx].end > addr {
    //           // ranges[idx] contains addr
    //       }
    //   }
    // Maybe we should make an iterator for this.
    pub fn upper_bound(&self, addr: usize) -> usize {
        self.ranges.partition_point(|r| r.start <= addr)
    }

    pub fn find(&self, addr: usize) -> Option<&RangeIndexEntry<T>> {
        let mut idx = self.upper_bound(addr);
        while idx > 0 && self.ranges[idx-1].max_end > addr {
            idx -= 1;
            if self.ranges[idx].end > addr {
                return Some(&self.ranges[idx]);
            }
        }
        return None;
    }

    pub fn find_ranges(&self, sorted_addr_ranges: &Vec<Range<usize>>) -> Vec<&RangeIndexEntry<T>> {
        assert!(sorted_addr_ranges.windows(2).all(|a| a[0].end < a[1].start));
        let mut res: Vec<&RangeIndexEntry<T>> = Vec::new();
        let mut idx = 0usize;
        let mut steps = 0usize;
        for range in sorted_addr_ranges {
            idx.set_max(self.ranges.partition_point(|r| r.max_end <= range.start));
            while idx < self.ranges.len() && self.ranges[idx].start < range.end {
                steps += 1;
                if steps > 100000 {
                    eprintln!("warning: lookup ranges overlap over {} index ranges", steps-1);
                    return res;
                }
                
                if self.ranges[idx].end > range.start {
                    res.push(&self.ranges[idx]);
                }
                idx += 1;
            }
        }
        res
    }
}
