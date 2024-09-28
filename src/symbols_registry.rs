use crate::{*, error::*, elf::*, symbols::*, procfs::*, util::*, unwind::*, log::*, context::*};
use std::{fs::File, collections::{HashMap, HashSet, hash_map::Entry, VecDeque}, rc::Rc, sync::{Arc, Mutex, Condvar, Weak}, sync::atomic::{AtomicBool, AtomicUsize, Ordering}, thread::{self, JoinHandle}, mem, path::Path};
use std::os::unix::fs::MetadataExt;
use memmap2::Mmap;

#[derive(Clone)]
pub struct BinaryInfo {
    pub id: BinaryId,
    // These 3 things can be loaded asynchronously separately. While loading, Err(Loading).
    pub elf: Result<Arc<ElfFile>>, // the file
    pub symbols: Result<Arc<Symbols>>, // .debug_info, .debug_line, .symtab, etc
    pub unwind: Result<Arc<UnwindInfo>>, // .eh_frame, .eh_frame_hdr, .debug_frame

    // Not known by SymbolsRegistry, populated after taking the BinaryInfo from SymbolsRegistry.
    pub addr_map: AddrMap,

    // Just for sorting in UI.
    pub initial_idx_in_proc_maps: usize,
}

pub struct SymbolsRegistry {
    binaries: HashMap<BinaryId, (BinaryInfo, Arc<SymbolsLoadingStatus>)>,
    shared: Arc<Shared>,
}

impl SymbolsRegistry {
    pub fn new(context: Arc<Context>) -> Self { Self {binaries: HashMap::new(), shared: Arc::new(Shared {context, to_main_thread: Mutex::new(VecDeque::new()), wake_main_thread: Arc::new(EventFD::new())})} }

    pub fn get_or_load(&mut self, id: &BinaryId, memory: &MemReader, custom_path: Option<String>, idx_in_proc_maps: usize) -> BinaryInfo {
        self.binaries.entry(id.clone()).or_insert_with(|| {
            let mut elf_contents_maybe: Result<Vec<u8>> = err!(Internal, "no contents");
            match &id.special {
                SpecialSegmentId::None => (),
                SpecialSegmentId::Vdso(range) => {
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
            let (shared_clone, id_clone, status_clone) = (self.shared.clone(), id.clone(), status.clone());
            self.shared.context.executor.add(move || task_load_elf(shared_clone, id_clone, status_clone, elf_contents_maybe, custom_path));
            (BinaryInfo {id: id.clone(), elf: err!(Loading, "loading symbols"), symbols: err!(Loading, "loading symbols"), unwind: err!(Loading, "loading symbols"), addr_map: AddrMap::new(), initial_idx_in_proc_maps: idx_in_proc_maps}, status)
        }).0.clone()
    }

    pub fn get_if_present(&self, id: &BinaryId) -> Option<&BinaryInfo> {
        self.binaries.get(id).map(|(info, status)| info)
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a BinaryId, &'a BinaryInfo)> + 'a {
        self.binaries.iter().map(|(id, (info, _))| (id, info))
    }

    pub fn get_progress(&self, id: &BinaryId) -> (/*ppm*/ usize, String) {
        let s = &self.binaries.get(id).unwrap().1;
        (s.progress_ppm.load(Ordering::Relaxed), s.stage.lock().unwrap().clone())
    }

    pub fn list(&self) -> Vec<BinaryId> {
        self.binaries.iter().map(|(id, _)| id.clone()).collect()
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
                Message::Elf {id, elf} => {
                    self.binaries.entry(id.clone()).and_modify(|(bin, _)| {
                        bin.elf = elf;
                        if let Err(e) = &bin.elf {
                            bin.symbols = Err(e.clone());
                            bin.unwind = Err(e.clone());
                        }
                    });
                }
                Message::Symbols {id, symbols} => {
                    self.binaries.entry(id.clone()).and_modify(|(bin, _)| {
                        bin.symbols = symbols.map(|x| Arc::new(x));
                    });
                }
                Message::Unwind {id, unwind} => {
                    self.binaries.entry(id.clone()).and_modify(|(bin, _)| {
                        bin.unwind = unwind.map(|x| Arc::new(x));
                    });
                }
            }
        }
        true
        //self.binaries.iter().all(|(_, (b, _))| !b.elf.as_ref().is_err_and(|e| e.is_loading()) && !b.symbols.as_ref().is_err_and(|e| e.is_loading()) && !b.unwind.as_ref().is_err_and(|e| e.is_loading()))
    }

    pub fn do_eviction(&mut self, in_use: &HashMap<BinaryId, BinaryInfo>) -> /*drop_caches*/ bool {
        let paths_in_use: HashSet<&String> = in_use.iter().map(|(id, _)| &id.path).collect();
        let mut evicted = false;
        self.binaries.retain(|id, (_, status)| {
            if !in_use.contains_key(id) && paths_in_use.contains(&id.path) {
                evicted = true;
                status.cancel.store(true, Ordering::SeqCst);
                false
            } else {
                true
            }
        });
        evicted
    }
}

struct Shared {
    context: Arc<Context>,
    to_main_thread: Mutex<VecDeque<Message>>,
    wake_main_thread: Arc<EventFD>,
}

enum Message {
    Elf {id: BinaryId, elf: Result<Arc<ElfFile>>},
    Symbols {id: BinaryId, symbols: Result<Symbols>},
    Unwind {id: BinaryId, unwind: Result<UnwindInfo>},
}

fn task_load_elf(shared: Arc<Shared>, id: BinaryId, status: Arc<SymbolsLoadingStatus>, elf_contents_maybe: Result<Vec<u8>>, custom_path: Option<String>) {
    let elf;
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("opening elf {}", id.path));
        elf = load_elf(&id, elf_contents_maybe, custom_path);
    }

    {
        let mut lock = shared.to_main_thread.lock().unwrap();
        lock.push_back(Message::Elf {id: id.clone(), elf: elf.clone().into()});
        shared.wake_main_thread.write(1);
    }

    if let Ok(elf) = elf {
        let (shared_clone, id_clone, elf_clone) = (shared.clone(), id.clone(), elf.clone());
        shared.context.executor.add(|| task_load_unwind(shared_clone, id_clone, elf_clone));
        LoadScheduler::start(id, elf, shared, status);
    }
}

fn task_load_unwind(shared: Arc<Shared>, id: BinaryId, elf: Arc<ElfFile>) {
    let unwind;
    {
        let _prof = ProfileScope::with_threshold(0.01, format!("loading unwind {}", id.path));
        unwind = UnwindInfo::load(elf);
    }
    if let Err(e) = &unwind {
        eprintln!("warning: failed to load unwind for {}: {}", id.path, e);
    }

    let mut lock = shared.to_main_thread.lock().unwrap();
    lock.push_back(Message::Unwind {id, unwind});
    shared.wake_main_thread.write(1);
}

fn load_elf(id: &BinaryId, contents_maybe: Result<Vec<u8>>, custom_path: Option<String>) -> Result<Arc<ElfFile>> {
    let elf = match &id.special {
        SpecialSegmentId::None => {
            let path = custom_path.as_ref().unwrap_or(&id.path);
            let file = File::open(path)?;
            let metadata = file.metadata()?;
            if metadata.ino() != id.inode && custom_path.is_none() {
                return err!(Usage, "binary changed");
            }
            let mmap = unsafe { Mmap::map(&file)? };
            ElfFile::from_mmap(id.path.clone(), mmap)?
        }
        SpecialSegmentId::Vdso(_) => {
            ElfFile::from_contents(id.path.clone(), contents_maybe?)?
        }
    };
    Ok(Arc::new(elf))
}

// Drives a SymbolsLoader.
struct LoadScheduler {
    id: BinaryId,
    loader: SyncUnsafeCell<Option<SymbolsLoader>>,
    num_shards: usize,

    shared: Arc<Shared>,
    status: Arc<SymbolsLoadingStatus>,

    failed: AtomicBool,
    stage: AtomicUsize,
    tasks_left: AtomicUsize,
}

impl LoadScheduler {
    fn start(id: BinaryId, elf: Arc<ElfFile>, shared: Arc<Shared>, status: Arc<SymbolsLoadingStatus>) {
        let mut scheduler = LoadScheduler {id: id.clone(), loader: SyncUnsafeCell::new(None), num_shards: 0, shared: shared.clone(), status: status.clone(), failed: AtomicBool::new(false), stage: AtomicUsize::new(0), tasks_left: AtomicUsize::new(1)};
        let max_shards = (shared.context.executor.num_threads - 1).max(1);
        let loader = match SymbolsLoader::new(elf, max_shards, status) {
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
            eprintln!("warning: failed to load symbols for {}: {}", self.id.path, e);
        }
        let mut lock = self.shared.to_main_thread.lock().unwrap();
        lock.push_back(Message::Symbols {id: self.id.clone(), symbols: res});
        self.shared.wake_main_thread.write(1);
    }
}
