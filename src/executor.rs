use std::{sync::{Arc, Mutex, Condvar}, thread::{self, JoinHandle}, collections::VecDeque};

pub struct Executor {
    pub num_threads: usize,
    shared: Arc<Shared>,
    threads: Vec<JoinHandle<()>>,
}

impl Executor {
    pub fn invalid() -> Self { Self {num_threads: 0, shared: Arc::new(Shared {tasks: Mutex::new(VecDeque::new()), wake_workers: Condvar::new()}), threads: Vec::new()} }
    
    pub fn new(num_threads: usize) -> Self {
        assert!(num_threads > 0);
        let mut exec = Self {num_threads, shared: Arc::new(Shared {tasks: Mutex::new(VecDeque::new()), wake_workers: Condvar::new()}), threads: Vec::new()};
        for i in 0..num_threads {
            let shared_clone = exec.shared.clone();
            exec.threads.push(thread::Builder::new().name("debworker".into()).spawn(|| Self::worker_thread(shared_clone)).unwrap());
        }
        exec
    }

    pub fn add_boxed(&self, f: Box<dyn FnOnce() + Send>) {
        self.shared.tasks.lock().unwrap().push_back(Task {f});
        self.shared.wake_workers.notify_one();
    }

    pub fn add<F: FnOnce() + Send + 'static>(&self, f: F) {
        self.add_boxed(Box::new(f));
    }

    fn worker_thread(shared: Arc<Shared>) {
        loop {
            let task;
            {
                let mut lock = shared.tasks.lock().unwrap();
                while lock.is_empty() {
                    lock = shared.wake_workers.wait(lock).unwrap();
                }
                task = lock.pop_front().unwrap();
            }
            (task.f)();
        }
    }
}

impl Drop for Executor {
    fn drop(&mut self) {
        // Currently we only have one Executor, which lives until the end of the program.
        // So we don't join threads, that would just add a few seconds of delay when exiting the program sometimes, for no benefit.
    }
}

struct Task {
    f: Box<dyn FnOnce() + Send>,
}

struct Shared {
    // We do at most hundreds of tasks per second, so not implementing anything fancy here, and not worrying about the cost of having lots of Arc-s and pointer chasing (here and in SymbolsRegistry, etc).
    tasks: Mutex<VecDeque<Task>>,
    wake_workers: Condvar,
}
