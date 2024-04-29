use crate::{*, symbols::*, arena::*, procfs::*, symbols_registry::*, util::*, context::*, executor::*};
use std::{mem, path::PathBuf, sync::{Arc, Mutex, atomic::{AtomicBool, AtomicUsize, Ordering}}, collections::HashSet, cmp, ops::Range, os::unix::ffi::OsStrExt, path::Path};

pub struct SymbolSearcher {
    pub searcher: Arc<dyn Searcher>,
    context: Arc<Context>,

    symbols: Vec<(BinaryId, Arc<Symbols>)>,
    pub waiting_for_symbols: bool, // if true, the `symbols` array is incomplete, but we may still be searching and have some results

    state: Arc<SearchState>,

    searched_query: SearchQuery,
    searched_num_symbols: usize,
}

const MAX_RESULTS: usize = 1000;

#[derive(Clone)]
pub struct SearchResults {
    pub results: Vec<SearchResult>, // limited to MAX_RESULTS
    pub total_results: usize, // not limited to MAX_RESULTS

    pub items_done: usize,
    pub items_total: usize, // not necessarily known in advance, may change during the search
    pub bytes_done: usize,

    pub complete: bool,
}
impl SearchResults {
    fn new() -> Self { Self {results: Vec::new(), total_results: 0, items_done: 0, items_total: 0, bytes_done: 0, complete: false} }
}

// Small and fast struct for a search match. We retrieve the slow full information (SearchResultInfo) only for the items on screen.
#[derive(Debug, Clone, PartialEq, Eq, Ord)]
pub struct SearchResult {
    score: i64,
    symbols_idx: usize, // index in SymbolSearcher's `symbols`
    id: usize,
}

impl PartialOrd for SearchResult {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
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

struct SearchState {
    results: Mutex<SearchResults>,
    cancel: AtomicBool,
    tasks_remaining: AtomicUsize,
}
impl SearchState {
    fn new() -> Self { Self {results: Mutex::new(SearchResults::new()), cancel: AtomicBool::new(false), tasks_remaining: AtomicUsize::new(0)} }
}

fn search_task(state: Arc<SearchState>, query: SearchQuery, symbols: Arc<Symbols>, symbols_idx: usize, shard_idx: usize, searcher: Arc<dyn Searcher>, context: Arc<Context>) {
    searcher.search(&symbols, symbols_idx, shard_idx, &query, &mut |mut res: Vec<SearchResult>, delta_items_done: usize, delta_items_total: usize, delta_bytes_done: usize| -> bool {
        if state.cancel.load(Ordering::SeqCst) {
            return false;
        }

        let original_count = res.len();
        if res.len() > MAX_RESULTS {
            sort_and_truncate_results(&mut res);
        }

        let mut r = state.results.lock().unwrap();
        if !res.is_empty() {
            r.results.append(&mut res);
            sort_and_truncate_results(&mut r.results);
        }
        r.total_results += original_count;
        r.items_done = r.items_done.wrapping_add(delta_items_done);
        r.items_total = r.items_total.wrapping_add(delta_items_total);
        r.bytes_done = r.bytes_done.wrapping_add(delta_bytes_done);
        true
    });

    if !state.cancel.load(Ordering::SeqCst) && state.tasks_remaining.fetch_sub(1, Ordering::SeqCst) == 1 {
        {
            let mut r = state.results.lock().unwrap();
            r.complete = true;
        }

        context.wake_main_thread.write(1);
    }
}

fn sort_and_truncate_results(v: &mut Vec<SearchResult>) {
    v.sort_unstable_by_key(|r| -r.score);
    v.truncate(MAX_RESULTS);
}

impl SymbolSearcher {
    pub fn new(searcher: Arc<dyn Searcher>, context: Arc<Context>) -> Self {
        let s = SymbolSearcher {searcher, context, symbols: Vec::new(), waiting_for_symbols: false, state: Arc::new(SearchState::new()), searched_query: SearchQuery::new(), searched_num_symbols: 0};
        s.state.results.lock().unwrap().complete = true;
        s
    }

    // Returns true if a new search started; the caller should scroll to top in this case.
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

        let parsed_query = SearchQuery::parse(query);
        if (&self.searched_query, self.searched_num_symbols) == (&parsed_query, self.symbols.len()) {
            return false;
        }

        self.state.cancel.store(true, Ordering::SeqCst);
        self.state = Arc::new(SearchState::new());

        self.searched_query = parsed_query;
        self.searched_num_symbols = self.symbols.len();

        let mut tasks: Vec<(/*symbols_idx*/ usize, /*shard_idx*/ usize)> = Vec::new();
        let properties = self.searcher.properties();
        for idx in 0..self.symbols.len() {
            if properties.parallel {
                for shard_idx in 0..self.symbols[idx].1.shards.len() {
                    tasks.push((idx, shard_idx));
                }
            } else {
                tasks.push((idx, 0));
            }
        }
        self.state.tasks_remaining.store(tasks.len(), Ordering::SeqCst); // must happen before starting the tasks
        for (symbols_idx, shard_idx) in tasks {
            // TODO: Search by file+line if query starts with '@'; pre-filter file table and call a different method of Searcher.
            let (state, query, symbols, searcher, context) = (self.state.clone(), self.searched_query.clone(), self.symbols[symbols_idx].1.clone(), self.searcher.clone(), self.context.clone());
            self.context.executor.add(move || search_task(state, query, symbols, symbols_idx, shard_idx, searcher, context));
        }

        true
    }

    pub fn get_results(&self) -> SearchResults {
        (*self.state.results.lock().unwrap()).clone()
    }

    pub fn format_result(&self, r: &SearchResult) -> SearchResultInfo {
        let s = &self.symbols[r.symbols_idx];
        self.searcher.format_result(s.0.clone(), &s.1, &self.searched_query, r)
    }

    pub fn format_results(&self, results: &[SearchResult]) -> Vec<SearchResultInfo> {
        let mut res: Vec<SearchResultInfo> = Vec::new();
        for r in results {
            let s = &self.symbols[r.symbols_idx];
            res.push(self.searcher.format_result(s.0.clone(), &s.1, &self.searched_query, r));
        }
        res
    }
}
impl Drop for SymbolSearcher {
    fn drop(&mut self) {
        self.state.cancel.store(true, Ordering::SeqCst);
    }
}

// Called periodically during the search (e.g. every few MBs) to report new results, report progress, and check cancellation.
// Returns false if the search is cancelled.
pub type SearchCallback<'a> = dyn FnMut(Vec<SearchResult>, /*delta_items_done*/ usize, /*delta_items_total*/ usize, /*delta_bytes_done*/ usize) -> bool + 'a;

#[derive(Clone, Debug)]
pub struct SearcherProperties {
    pub have_names: bool,
    pub have_files: bool,
    // If true, we schedule one task per SymbolsShard, otherwise one task per Symbols.
    pub parallel: bool,
}

pub trait Searcher: Sync + Send {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, shard_idx: usize, query: &SearchQuery, callback: &mut SearchCallback);
    fn format_result(&self, binary: BinaryId, symbols: &Arc<Symbols>, query: &SearchQuery, res: &SearchResult) -> SearchResultInfo;
    fn properties(&self) -> SearcherProperties;
}

pub struct FileSearcher;

impl Searcher for FileSearcher {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, shard_idx: usize, query: &SearchQuery, callback: &mut SearchCallback) {
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

    fn properties(&self) -> SearcherProperties { SearcherProperties {have_names: false, have_files: true, parallel: false} }
}

pub struct FunctionSearcher;

impl Searcher for FunctionSearcher {
    fn search(&self, symbols: &Symbols, symbols_idx: usize, shard_idx: usize, query: &SearchQuery, callback: &mut SearchCallback) {
        let range = (symbols.functions.len()*shard_idx/symbols.shards.len())..(symbols.functions.len()*(shard_idx+1)/symbols.shards.len());
        callback(Vec::new(), 0, range.len(), 0);
        let mut items_done = 0usize;
        let mut bytes_done = 0usize;
        let mut res: Vec<SearchResult> = Vec::new();
        for idx in range {
            let function = &symbols.functions[idx];
            if function.flags.contains(FunctionFlags::SENTINEL) {
                continue;
            }
            let slice = function.mangled_name();
            if let Some(score) = fuzzy_match(slice, &query.subsequence) {
                res.push(SearchResult {score, id: idx, symbols_idx});
            }
            items_done += 1;
            bytes_done += slice.len();
            if items_done > (1 << 16) {
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

    fn properties(&self) -> SearcherProperties { SearcherProperties {have_names: true, have_files: true, parallel: true} }
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
