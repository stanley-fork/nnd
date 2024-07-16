use std::collections::*;
use std::sync::{Arc, Mutex, RwLock, Weak};
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::time::{Duration, Instant, SystemTime};
use std::ffi::{CString, OsString};
use std::path::PathBuf;
use std::borrow::Cow;
use std::pin::Pin;
use std::marker::PhantomData;
use std::num::NonZeroU8;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64};
use std::hint::black_box;

fn main() {
    // Comments say whether it's implemented.
    
    // Slice-like.
    let slice: &[i32] = &[1, 2, 3];                            // yes
    let mut_slice: &mut [i32] = &mut [1, 2, 3];                // yes
    let v: Vec<i32> = vec![1, 2, 3];                           // yes
    let mut empty_v: Vec<i32> = Vec::new();                    // yes
    let bh: BinaryHeap<i32> = BinaryHeap::from(vec![1, 2, 3]); // good enough

    let vd: VecDeque<i32> = VecDeque::from(vec![1, 2, 3]);     // no

    let string = String::from("hello");                        // yes
    let str_slice: &str = "world";                             // yes
    let cstring = CString::new("hello").unwrap();              // yes
    let osstring = OsString::from("hello");                    // yes
    let pathbuf = PathBuf::from("/path/to/file");              // yes

    let ll: LinkedList<i32> = LinkedList::from_iter(vec![1, 2, 3]); // no

    let bm: BTreeMap<i32, &str> = BTreeMap::from([(1, "one"), (2, "two")]); // no
    let bs: BTreeSet<i32> = BTreeSet::from_iter(vec![1, 2, 3]);             // no

    let mut hm: HashMap<i32, &str> = HashMap::new();                        // no
    hm.insert(1, "one");
    let hs: HashSet<i32> = HashSet::from_iter(vec![1, 2, 3]);               // no

    let rc: Rc<i32> = Rc::new(42);                                          // no
    let arc: Arc<i32> = Arc::new(42);                                       // no
    let weak: Weak<i32> = Arc::downgrade(&arc);                             // no

    // Complicated and not worth it.
    let fn_ptr: fn(i32) -> i32 = |x| x + 1;
    let closure = |x: i32| x + 1;

    // No designated pretty-printers needed below.

    let b: Box<i32> = Box::new(42);
    let raw_ptr: *const i32 = &42;
    let raw_mut_ptr: *mut i32 = &mut 42;
    let cell: Cell<i32> = Cell::new(42);
    let atomic_bool = AtomicBool::new(true);
    let atomic_i32 = AtomicI32::new(42);
    let atomic_u64 = AtomicU64::new(42);
    let pin = Pin::new(&42);
    let phantom: PhantomData<i32> = PhantomData;
    let non_zero = NonZeroU8::new(42).unwrap();

    let refcell: RefCell<i32> = RefCell::new(42);
    let mutex: Mutex<i32> = Mutex::new(42);
    let rwlock: RwLock<i32> = RwLock::new(42);

    let some: Option<i32> = Some(42);
    let none: Option<i32> = None;
    let ok: Result<i32, &str> = Ok(42);
    let err: Result<i32, &str> = Err("error");

    // Iterators
    black_box(&v);
    let iter = v.iter();
    let _ = black_box(iter);
    let into_iter = v.into_iter();
    black_box(&empty_v);
    let iter_mut = empty_v.iter_mut();

    let range = 0..10;
    let range_inclusive = 0..=10;
    let range_to = ..10;
    let range_from = 10..;

    // Moo
    let cow1: Cow<str> = Cow::Borrowed("hello");
    let cow2: Cow<str> = Cow::Owned("world".to_string());

    let duration = Duration::from_secs(42);
    let instant = Instant::now();
    let system_time = SystemTime::now();

    black_box(vd);
    black_box(ll);
    black_box(hm);
    black_box(bm);
    black_box(hs);
    black_box(bs);
    black_box(bh);
    black_box(b);
    black_box(rc);
    black_box(arc);
    black_box(weak);
    black_box(cell);
    black_box(refcell);
    black_box(mutex);
    black_box(rwlock);
    black_box(atomic_bool);
    black_box(atomic_i32);
    black_box(atomic_u64);
    black_box(some);
    black_box(none);
    let _ = black_box(ok);
    let _ = black_box(err);
    black_box(string);
    black_box(str_slice);
    black_box(cstring);
    black_box(osstring);
    black_box(pathbuf);
    let _ = black_box(into_iter);
    let _ = black_box(iter_mut);
    black_box(range);
    black_box(range_inclusive);
    black_box(range_to);
    black_box(range_from);
    black_box(slice);
    black_box(mut_slice);
    black_box(cow1);
    black_box(cow2);
    black_box(pin);
    black_box(phantom);
    black_box(non_zero);
    black_box(duration);
    black_box(instant);
    black_box(system_time);
    black_box(fn_ptr);
    black_box(&closure);
    black_box(raw_ptr);
    black_box(raw_mut_ptr);
}
