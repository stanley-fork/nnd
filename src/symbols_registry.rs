use crate::{*, error::*, elf::*, symbols::*, procfs::*, util::*, unwind::*, log::*, context::*};
use std::{fs::File, collections::{HashMap, HashSet, hash_map::Entry, VecDeque}, rc::Rc, sync::{Arc, Mutex, Condvar, Weak}, sync::atomic::{AtomicBool, AtomicUsize, Ordering}, thread::{self, JoinHandle}, mem, path::Path};
use std::os::unix::fs::MetadataExt;
use memmap2::Mmap;

#[derive(Clone)]
pub struct Binary {
    pub locator: BinaryLocator,
    // Unique identifier of this bundle of ElfFile, Symbols, etc. In particular, this number changes if we destroy and re-load the Symbols.
    pub id: usize,

    // These 3 things can be loaded asynchronously separately. While loading, Err(Loading).
    pub elf: Result<Arc<ElfFile>>, // the file
    pub symbols: Result<Arc<Symbols>>, // .debug_info, .debug_line, .symtab, etc
    pub unwind: Result<Arc<UnwindInfo>>, // .eh_frame, .eh_frame_hdr, .debug_frame

    // If this binary is currently loaded by the debuggee.
    pub is_mapped: bool,
    pub addr_map: AddrMap,
    // Last seen index among mapped binaries, in order of first mmap address.
    pub mmap_idx: usize,
    // Index in `priority_order`.
    pub priority_idx: usize,
}

pub struct SymbolsRegistry {
    binaries: Vec<Option<(Binary, Arc<SymbolsLoadingStatus>)>>, // id -> binary
    pub locator_to_id: HashMap<BinaryLocator, usize>,
    // Binaries in order in which they should be displayed and searched. E.g. the main executable is usually first, unmapped binaries are after mapped ones, etc.
    pub priority_order: Vec<usize>,
    shared: Arc<Shared>,
}

impl SymbolsRegistry {
    pub fn new(context: Arc<Context>) -> Self { Self {binaries: Vec::new(), locator_to_id: HashMap::new(), priority_order: Vec::new(), shared: Arc::new(Shared {context, to_main_thread: Mutex::new(VecDeque::new()), wake_main_thread: Arc::new(EventFD::new())})} }

    pub fn get(&self, id: usize) -> Option<&Binary> {
        match self.binaries.get(id) {
            Some(Some((b, _))) => Some(b),
            _ => None,
        }
    }
    pub fn get_mut(&mut self, id: usize) -> Option<&mut Binary> {
        match self.binaries.get_mut(id) { Some(Some((b, _))) => Some(b), _ => None }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a Binary> + 'a {
        assert_eq!(self.priority_order.len(), self.locator_to_id.len());
        self.priority_order.iter().map(|id| &self.binaries[*id].as_ref().unwrap().0)
    }

    pub fn add(&mut self, locator: BinaryLocator, memory: &MemReader, custom_path: Option<String>, additional_elf_paths: Vec<String>) -> &mut Binary {
        let id = self.binaries.len();
        let mut elf_contents_maybe: Result<Vec<u8>> = err!(Internal, "no contents");
        match &locator.special {
            SpecialSegmentId::None => (),
            SpecialSegmentId::Vdso((_, range)) => {
                let mut buf: Vec<u8> = vec![0; range.len()];
                elf_contents_maybe = match memory.read(range.start, &mut buf) {
                    Err(e) => Err(e),
                    Ok(()) => Ok(buf),
                };
            }
        }

        // Kick off symbols loading.
        let status = Arc::new(SymbolsLoadingStatus::new());
        *status.stage.lock().unwrap() = "opening ELF".to_string();
        let binary = Binary {locator: locator.clone(), id, elf: err!(Loading, "loading symbols"), symbols: err!(Loading, "loading symbols"), unwind: err!(Loading, "loading symbols"), is_mapped: false, addr_map: AddrMap::default(), mmap_idx: 0, priority_idx: 0};
        self.binaries.push(Some((binary, status.clone())));
        let inserted = self.locator_to_id.insert(locator.clone(), id).is_none();
        assert!(inserted);
        let shared_clone = self.shared.clone();
        self.shared.context.executor.add(move || task_load_elf(shared_clone, locator, id, status, elf_contents_maybe, custom_path, additional_elf_paths));
        &mut self.binaries.last_mut().unwrap().as_mut().unwrap().0
    }

    pub fn mark_all_as_unmapped(&mut self) {
        for (_, id) in &self.locator_to_id {
            let b = &mut self.binaries[*id].as_mut().unwrap().0;
            b.is_mapped = false;
            b.addr_map = AddrMap::default();
        }
    }

    pub fn update_priority_order(&mut self) {
        self.priority_order.clear();
        for (_, id) in &self.locator_to_id {
            self.priority_order.push(*id);
        }
        self.priority_order.sort_unstable_by_key(|id| {
            let b = &self.binaries[*id].as_ref().unwrap().0;
            (!b.is_mapped, b.mmap_idx)
        });
        for (idx, id) in self.priority_order.iter().enumerate() {
            self.binaries[*id].as_mut().unwrap().0.priority_idx = idx;
        }
    }

    pub fn get_progress(&self, id: usize) -> (/*ppm*/ usize, String) {
        let s = &self.binaries.get(id).unwrap().as_ref().unwrap().1;
        (s.progress_ppm.load(Ordering::Relaxed), s.stage.lock().unwrap().clone())
    }

    // Put it in the main epoll.
    pub fn event_fd(&self) -> Arc<EventFD> {
        self.shared.wake_main_thread.clone()
    }

    // Call after event_fd() was notified.
    // Returns true if something was loaded and the caller should drop caches.
    pub fn process_events(&mut self) -> bool {
        {
            let n = self.shared.wake_main_thread.read();
            assert!(n > 0);
        }
        let mut lock = self.shared.to_main_thread.lock().unwrap();
        while let Some(message) = lock.pop_front() {
            match message {
                Message::Elf {id, elf} => if let Some((bin, _)) = &mut self.binaries[id] {
                    bin.elf = elf;
                    if let Err(e) = &bin.elf {
                        bin.symbols = Err(e.clone());
                        bin.unwind = Err(e.clone());
                    }
                }
                Message::Symbols {id, symbols} => if let Some((bin, _)) = &mut self.binaries[id] {
                    bin.symbols = symbols.map(|x| Arc::new(x));
                }
                Message::Unwind {id, unwind} => if let Some((bin, _)) = &mut self.binaries[id] {
                    bin.unwind = unwind.map(|x| Arc::new(x));
                }
            }
        }
        true
        //self.binaries.iter().all(|(_, (b, _))| !b.elf.as_ref().is_err_and(|e| e.is_loading()) && !b.symbols.as_ref().is_err_and(|e| e.is_loading()) && !b.unwind.as_ref().is_err_and(|e| e.is_loading()))
    }

    // Considerations:
    //  * If the executable was replaced (e.g. recompiled) and restarted, we want to unload symbols for the old version.
    //    This is to save memory and to prevent duplicate old files/functions/etc from showing up in search dialogs etc.
    //  * If the program exited, we want to keep the symbols loaded, to allow opening files/functions/etc.
    pub fn do_eviction(&mut self) -> /*drop_caches*/ bool {
        let mapped_paths: HashSet<&String> = self.iter().filter(|b| b.is_mapped).map(|b| &b.locator.path).collect();
        // (This handles "[vdso]" correctly: evict previous process's vdso if a new process with vdso exists.)
        let ids_to_evict: Vec<usize> = self.iter().filter(|b| !b.is_mapped && mapped_paths.contains(&b.locator.path)).map(|b| b.id).collect();
        if ids_to_evict.is_empty() {
            return false;
        }
        for id in ids_to_evict {
            let (b, status) = self.binaries[id].as_ref().unwrap();
            status.cancel.store(true, Ordering::SeqCst);
            let removed = self.locator_to_id.remove(&b.locator).is_some();
            assert!(removed);
            self.binaries[id] = None;
        }
        self.update_priority_order();
        true
    }
}

struct Shared {
    context: Arc<Context>,
    to_main_thread: Mutex<VecDeque<Message>>,
    wake_main_thread: Arc<EventFD>,
}

enum Message {
    Elf {id: usize, elf: Result<Arc<ElfFile>>},
    Symbols {id: usize, symbols: Result<Symbols>},
    Unwind {id: usize, unwind: Result<UnwindInfo>},
}

fn task_load_elf(shared: Arc<Shared>, locator: BinaryLocator, id: usize, status: Arc<SymbolsLoadingStatus>, elf_contents_maybe: Result<Vec<u8>>, custom_path: Option<String>, additional_elf_paths: Vec<String>) {
    let elf;
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("opening elf {}", locator.path));
        elf = load_elf(&locator, elf_contents_maybe, custom_path);
    }

    {
        let mut lock = shared.to_main_thread.lock().unwrap();
        lock.push_back(Message::Elf {id, elf: elf.clone().into()});
        shared.wake_main_thread.write(1);
    }

    if let Ok(elf) = elf {
        let (shared_clone, elf_clone) = (shared.clone(), elf.clone());
        shared.context.executor.add(move || task_load_unwind(shared_clone, id, elf_clone));
        LoadScheduler::start(id, elf, additional_elf_paths, shared, status);
    }
}

fn task_load_unwind(shared: Arc<Shared>, id: usize, elf: Arc<ElfFile>) {
    let unwind;
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("loading unwind {}", elf.name));
        unwind = UnwindInfo::load(elf.clone());
    }
    if let Err(e) = &unwind {
        eprintln!("warning: failed to load unwind for {}: {}", elf.name, e);
    }

    let mut lock = shared.to_main_thread.lock().unwrap();
    lock.push_back(Message::Unwind {id, unwind});
    shared.wake_main_thread.write(1);
}

fn load_elf(locator: &BinaryLocator, contents_maybe: Result<Vec<u8>>, custom_path: Option<String>) -> Result<Arc<ElfFile>> {
    let elf = match &locator.special {
        SpecialSegmentId::None => {
            let path = custom_path.as_ref().unwrap_or(&locator.path);
            let file = File::open(path)?;
            let metadata = file.metadata()?;
            if metadata.ino() != locator.inode && custom_path.is_none() {
                return err!(Usage, "binary changed");
            }
            let mmap = unsafe { Mmap::map(&file)? };
            ElfFile::from_mmap(locator.path.clone(), mmap)?
        }
        SpecialSegmentId::Vdso(_) => {
            ElfFile::from_contents(locator.path.clone(), contents_maybe?)?
        }
    };
    Ok(Arc::new(elf))
}

// Drives a SymbolsLoader.
struct LoadScheduler {
    id: usize,
    name: String,
    loader: SyncUnsafeCell<Option<SymbolsLoader>>,
    num_shards: usize,

    shared: Arc<Shared>,
    status: Arc<SymbolsLoadingStatus>,

    failed: AtomicBool,
    stage: AtomicUsize,
    tasks_left: AtomicUsize,
}

impl LoadScheduler {
    fn start(id: usize, elf: Arc<ElfFile>, additional_elf_paths: Vec<String>, shared: Arc<Shared>, status: Arc<SymbolsLoadingStatus>) {
        let mut scheduler = LoadScheduler {id, name: elf.name.clone(), loader: SyncUnsafeCell::new(None), num_shards: 0, shared: shared.clone(), status: status.clone(), failed: AtomicBool::new(false), stage: AtomicUsize::new(0), tasks_left: AtomicUsize::new(1)};
        let max_shards = (shared.context.executor.num_threads - 1).max(1);
        let loader = match SymbolsLoader::new(elf, additional_elf_paths, id, max_shards, status) {
            Ok(l) => l,
            Err(e) => {
                scheduler.handle_fail(e);
                return;
            }
        };
        scheduler.num_shards = loader.num_shards;
        *scheduler.loader.get_mut() = Some(loader);
        let scheduler = Arc::new(scheduler);
        scheduler.schedule();
    }

    fn schedule(self: Arc<Self>) {
        if self.tasks_left.fetch_sub(1, Ordering::SeqCst) != 1 {
            return;
        }
        let stage = self.stage.fetch_add(1, Ordering::Relaxed) + 1;
        if self.check_cancellation() { return; }

        match unsafe {&mut *self.loader.get()}.as_mut().unwrap().prepare_stage(stage) {
            Err(e) => {
                self.handle_fail(e);
                return;
            }
            Ok(false) => { // complete
                let symbols = mem::take(unsafe {&mut *self.loader.get()}).unwrap().into_result();
                self.complete(Ok(symbols));
                return;
            }
            Ok(true) => (),
        }
        if self.check_cancellation() { return; }

        self.tasks_left.store(self.num_shards, Ordering::Relaxed);
        for i in 0..self.num_shards {
            let self_ = self.clone();
            self.shared.context.executor.add(move || {
                match unsafe {&*self_.loader.get()}.as_ref().unwrap().run(stage, i) {
                    Ok(()) => self_.schedule(),
                    Err(e) => self_.handle_fail(e),
                }
            });
        }
    }

    fn handle_fail(&self, e: Error) {
        if !self.failed.swap(true, Ordering::SeqCst) {
            self.status.cancel.store(true, Ordering::SeqCst);
            self.complete(Err(e));
        }
    }

    fn check_cancellation(&self) -> bool {
        if !self.status.cancel.load(Ordering::Relaxed) {
            return false;
        }
        if !self.failed.swap(true, Ordering::SeqCst) {
            // This only does anything if cancellation came from inside SymbolsLoader, which shouldn't happen.
            // Otherwise there's no need to notify the main thread because we either already did it or the main thread is not interested anymore.
            // But why not.
            self.complete(err!(Cancelled, "cancelled"));
        }
        true
    }

    // Called exactly once, by whoever sets self.failed to true, or at the end of successful loading.
    fn complete(&self, res: Result<Symbols>) {
        if let Err(e) = &res {
            eprintln!("warning: failed to load symbols for {}: {}", self.name, e);
        }
        let mut lock = self.shared.to_main_thread.lock().unwrap();
        lock.push_back(Message::Symbols {id: self.id, symbols: res});
        self.shared.wake_main_thread.write(1);
    }
}
