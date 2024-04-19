use crate::{*, symbols::*, arena::*, procfs::*, symbols_registry::*, util::*};
use std::{mem, path::PathBuf, sync::Arc, collections::HashSet, cmp::Ordering, ops::Range, os::unix::ffi::OsStrExt, path::Path};

pub struct SymbolSearcher {
    pub searcher: Box<dyn Searcher>,

    symbols: Vec<(BinaryId, Arc<Symbols>)>,
    pub results: Vec<SearchResult>,
    pub total_results: usize,
    pub waiting_for_symbols: bool, // if true, the `symbols` array is incomplete, but we may still be searching and have some results
    pub complete: bool, // `results` are complete; otherwise we're still searching

    // Progress indication.
    pub items_done: usize,
    pub items_total: usize,
    pub bytes_done: usize,

    searched_query: SearchQuery,
    searched_num_symbols: usize,
}

// Small and fast struct for a search match. We retrieve the slow full information (SearchResultInfo) only for the items on screen.
#[derive(Debug, Clone, PartialEq, Eq, Ord)]
pub struct SearchResult {
    score: i64,
    symbols_idx: usize, // index in SymbolSearcher's `symbols`
    id: usize,
}

impl PartialOrd for SearchResult {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.score != other.score {
            Some(self.score.cmp(&other.score).reverse())
        } else {
            Some((self.symbols_idx, self.id).cmp(&(other.symbols_idx, other.id)))
        }
    }
}

#[derive(Clone)]
pub struct SearchResultInfo {
    pub id: usize,
    pub binary: BinaryId,
    pub symbols: Arc<Symbols>,
    
    pub name: String,
    pub file: PathBuf,
    pub line: LineInfo,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SearchQuery {
    subsequence: String,
    // TODO: Add searching by declaration filename + optional line number (syntax: "@foo.cpp:42" in the search bar)
}

impl SearchQuery {
    fn new() -> Self { Self {subsequence: String::new()} }
    fn parse(s: &str) -> Self { Self {subsequence: s.to_string()} }
}

impl SymbolSearcher {
    pub fn new(searcher: Box<dyn Searcher>) -> Self { SymbolSearcher {searcher, symbols: Vec::new(), results: Vec::new(), total_results: 0, waiting_for_symbols: false, complete: false, items_done: 0, items_total: 0, bytes_done: 0, searched_query: SearchQuery::new(), searched_num_symbols: 0} }

    // Returns true if a new search was started; the caller should scroll to top in this case.
    pub fn update(&mut self, registry: &SymbolsRegistry, binaries: Option<Vec<BinaryId>>, query: &str) -> bool {
        let seen_binary_ids: HashSet<BinaryId> = self.symbols.iter().map(|t| t.0.clone()).collect();
        self.waiting_for_symbols = false;
        let binaries = match binaries {
            Some(x) => x,
            None => registry.list(),
        };
        for id in binaries {
            if seen_binary_ids.contains(&id) {
                continue;
            }
            let bin = registry.get_if_present(&id).unwrap();
            match &bin.symbols {
                Ok(s) => self.symbols.push((id.clone(), s.clone())),
                Err(e) if e.is_loading() => self.waiting_for_symbols = true,
                Err(_) => (),
            }
        }

        // TODO: Poll background tasks here. Merge new results into self.results, do quickselect to limit the size (do quickselect in each task too), sort the merged results.

        let parsed_query = SearchQuery::parse(query);
        if (&self.searched_query, self.searched_num_symbols) == (&parsed_query, self.symbols.len()) {
            return false;
        }

        self.results.clear();
        self.total_results = 0;
        self.complete = false;
        (self.items_done, self.items_total, self.bytes_done) = (0, 0, 0);
        self.searched_query = parsed_query;
        self.searched_num_symbols = self.symbols.len();

        for idx in 0..self.symbols.len() {
            // TODO: Schedule background tasks instead of searching right here.
            // TODO: Search in multiple threads.
            // TODO: Search by file+line if query starts with '@'; pre-filter file table here and call a different method.
            self.searcher.search(&self.symbols[idx].1, idx, &self.searched_query, &mut |mut res: Vec<SearchResult>, delta_items_done: usize, delta_items_total: usize, delta_bytes_done: usize| -> bool {
                self.total_results += res.len();
                self.results.append(&mut res);
                self.items_done = self.items_done.wrapping_add(delta_items_done);
                self.items_total = self.items_total.wrapping_add(delta_items_total);
                self.bytes_done = self.bytes_done.wrapping_add(delta_bytes_done);
                true
            });
        }
        self.complete = true;

        self.results.sort();

        true
    }

    pub fn format_result(&self, idx: usize) -> SearchResultInfo {
        let r = &self.results[idx];
        let s = &self.symbols[r.symbols_idx];
        self.searcher.format_result(s.0.clone(), &s.1, &self.searched_query, r)
    }
    
    pub fn format_results(&self, range: Range<usize>) -> Vec<SearchResultInfo> {
        let mut res: Vec<SearchResultInfo> = Vec::new();
        for r in &self.results[range] {
            let s = &self.symbols[r.symbols_idx];
            res.push(self.searcher.format_result(s.0.clone(), &s.1, &self.searched_query, r));
        }
        res
    }
}

// Called periodically during the search (e.g. every few MBs) to report new results, report progress, and check cancellation.
// Returns false if the search is cancelled.
pub type SearchCallback<'a> = dyn FnMut(Vec<SearchResult>, /*delta_items_done*/ usize, /*delta_items_total*/ usize, /*delta_bytes_done*/ usize) -> bool + 'a;

pub trait Searcher {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, query: &SearchQuery, callback: &mut SearchCallback);
    fn format_result(&self, binary: BinaryId, symbols: &Arc<Symbols>, query: &SearchQuery, res: &SearchResult) -> SearchResultInfo;
    fn have_names_and_files(&self) -> (bool, bool);
}

pub struct FileSearcher;

impl Searcher for FileSearcher {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, query: &SearchQuery, callback: &mut SearchCallback) {
        let items_total = symbols.path_to_used_file.len();
        callback(Vec::new(), 0, items_total, 0);
        let mut res: Vec<SearchResult> = Vec::new();
        let mut bytes_done = 0usize;
        for (path, &id) in &symbols.path_to_used_file {
            let slice = path.as_os_str().as_bytes();
            if let Some(score) = fuzzy_match(slice, &query.subsequence) {
                res.push(SearchResult {score, id, symbols_idx});
            }
            bytes_done += slice.len();
        }
        callback(res, items_total, 0, bytes_done);
    }

    fn format_result(&self, binary: BinaryId, symbols: &Arc<Symbols>, query: &SearchQuery, res: &SearchResult) -> SearchResultInfo {
        let file = &symbols.files[res.id];
        SearchResultInfo {id: res.id, binary, symbols: symbols.clone(), name: String::new(), file: file.path.to_owned(), line: LineInfo::invalid()}
    }

    fn have_names_and_files(&self) -> (bool, bool) {
        (false, true)
    }
}

pub struct FunctionSearcher;

impl Searcher for FunctionSearcher {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, query: &SearchQuery, callback: &mut SearchCallback) {
        callback(Vec::new(), 0, symbols.functions.len(), 0);
        let mut items_done = 0usize;
        let mut bytes_done = 0usize;
        let mut res: Vec<SearchResult> = Vec::new();
        for (idx, function) in symbols.functions.iter().enumerate() {
            if function.flags.contains(FunctionFlags::SENTINEL) {
                continue;
            }
            let slice = function.mangled_name();
            if let Some(score) = fuzzy_match(slice, &query.subsequence) {
                res.push(SearchResult {score, id: idx, symbols_idx});
            }
            items_done += 1;
            bytes_done += slice.len();
            if items_done > (1 << 17) {
                if !callback(mem::take(&mut res), mem::take(&mut items_done), 0, mem::take(&mut bytes_done)) {
                    return;
                }
            }
        }
        callback(res, items_done, 0, bytes_done);
    }

    fn format_result(&self, binary: BinaryId, symbols: &Arc<Symbols>, query: &SearchQuery, res: &SearchResult) -> SearchResultInfo {
        let function = &symbols.functions[res.id];
        let mut res = SearchResultInfo {id: res.id, binary, symbols: symbols.clone(), name: function.demangle_name(), file: PathBuf::new(), line: LineInfo::invalid()};
        if let Some((sf, _)) = symbols.root_subfunction(function) {
            if let Some(file_idx) = sf.call_line.file_idx() {
                res.file = symbols.files[file_idx].path.to_owned();
                res.line = sf.call_line.clone();
            }
        }
        res
    }

    fn have_names_and_files(&self) -> (bool, bool) {
        (true, true)
    }
}

fn fuzzy_match(haystack: &[u8], needle: &str) -> Option<i64> {
    // Scoring:
    //  * Score 3 if the string is exactly equal to the query string.
    //  * Score 2 if the query string is a suffix of the string.
    //  * Score 1 if the query string is a substring.
    //  * Score -k if the query string appears as a subsequence in the string, with k contiguous pieces.
    //    Checking if it's a subsequence at all is trivial, but minimizing k takes O(n*m) time.
    //    We should do some approximation instead. Maybe find a subsequence greedily, then do one forward and one backward pass greedily coalescing the pieces.
    //  * If the query string is not a subsequence of the string, it's not included in the results at all.

    // Rust standard library doesn't provide a good substring search implementation for [u8], only for str (because the implementation takes some extra steps to ensure the results are aligned with utf8 codepoints).
    // We recklessly convert strings from DWARF (file paths, function names, variable names) to str without checking if it's valid utf8 (in practice it always is). If this turns out to be a problem, we should implement our own substring search instead.
    let haystack_str = unsafe {std::str::from_utf8_unchecked(haystack)};
    if let Some(pos) = haystack_str.rfind(needle) {
        let score = if haystack.len() == needle.len() { 3 } else if pos + needle.len() == haystack.len() { 2 } else { 1 };
        return Some(score);
    }

    let needle = needle.as_bytes();
    let mut i = 0usize;
    // TODO: Do greedy coalescing of pieces.
    let mut pieces = 0i64;
    let mut prev_matched = false;
    for &c in haystack {
        if i == needle.len() {
            break;
        }
        if c == needle[i] {
            i += 1;
            if !prev_matched {
                prev_matched = true;
                pieces += 1;
            }
        } else {
            prev_matched = false;
        }
    }
    if i < needle.len() {
        return None;
    }
    return Some(-pieces);
}
