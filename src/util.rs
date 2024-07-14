use crate::{*, error::*, log::*};
use libc::{pid_t, c_char, c_void};
use std::{io, io::{Read, BufReader, BufRead, Write}, str::FromStr, ptr, mem, mem::ManuallyDrop, fmt, os::fd::RawFd, ffi::{CStr, OsString}, os::unix::ffi::{OsStringExt, OsStrExt}, arch::asm, cell::UnsafeCell, sync::atomic::{AtomicBool, Ordering}, ops::{Deref, DerefMut, FnOnce}, fs::File, collections::{BinaryHeap, hash_map::DefaultHasher}, hash::{Hash, Hasher}, cmp::Ord, cmp, path::{Path, PathBuf}};

pub fn tgkill(pid: pid_t, tid: pid_t, sig: i32) -> Result<i32> {
    eprintln!("trace: tgkill({}, {}, {})", pid, tid, sig);
    let ret: i32;
    unsafe {
        asm!(
            "syscall",
            in("rax") libc::SYS_tgkill,
            in("rdi") pid,
            in("rsi") tid,
            in("rdx") sig,
            out("rcx") _,
            out("r11") _,
            lateout("rax") ret,
            options(nostack)
        );
    }
    if ret < 0 { return errno_err!("tgkill() failed"); }
    Ok(ret)
}

pub unsafe fn ptrace(request: i32, pid: pid_t, addr: u64, data: u64, prof: &mut ProfileBucket) -> Result<i64> {
    (*libc::__errno_location()) = 0;
    let r = profile_syscall!(prof, {
        libc::ptrace(request, pid, addr, data)
    });
    //eprintln!("trace: ptrace({}, {}, 0x{:x}, 0x{:x}) -> 0x{:x}", ptrace_request_name(request), pid, addr, data, r);
    if r == -1 {
        if (*libc::__errno_location()) != 0 {
            return errno_err!("ptrace({}) failed", request);
        }
        assert!([libc::PTRACE_PEEKDATA, libc::PTRACE_PEEKSIGINFO, libc::PTRACE_PEEKTEXT, libc::PTRACE_PEEKUSER].contains(&request));
    }
    Ok(r)
}


// Epoll fd. Closed in destructor.
pub struct Epoll {
    fd: i32,
}

impl Epoll {
    pub fn new() -> Result<Epoll> {
        unsafe {
            let fd = libc::epoll_create1(0);
            if fd < 0 { return errno_err!("epoll_create1(0) failed"); }
            Ok(Epoll {fd: fd})
        }
    }

    // (This is very basic and not rust-like: no RAII to automatically remove fds, no const/mut distinction. None of these things seem useful for this project, and I care about amout of code more than about conforming to rust's style of doing things.)
    pub fn add(&self, fd: i32, events: i32, data: u64) -> Result<()> {
        unsafe {
            let mut event = libc::epoll_event {events: events as u32, u64: data};
            let r = libc::epoll_ctl(self.fd, libc::EPOLL_CTL_ADD, fd, &mut event);
            if r != 0 { return errno_err!("epoll_ctl(EPOLL_CTL_ADD) failed"); }
        }
        Ok(())
    }

    pub fn modify(&self, fd: i32, events: i32, data: u64) -> Result<()> {
        unsafe {
            let mut event = libc::epoll_event {events: events as u32, u64: data};
            let r = libc::epoll_ctl(self.fd, libc::EPOLL_CTL_MOD, fd, &mut event);
            if r != 0 { return errno_err!("epoll_ctl(EPOLL_CTL_MOD) failed"); }
        }
        Ok(())
        
    }

    pub fn del(&self, fd: i32) -> Result<()> {
        unsafe {
            let r = libc::epoll_ctl(self.fd, libc::EPOLL_CTL_DEL, fd, ptr::null_mut());
            if r != 0 { return errno_err!("epoll_ctl(EPOLL_CTL_DEL) failed"); }
        }
        Ok(())
    }

    // Returns Ok(0) on EINTR.
    pub fn wait(&self, out: &mut [libc::epoll_event]) -> Result<usize> {
        unsafe {
            let r = libc::epoll_wait(self.fd, out.as_mut_ptr(), out.len() as i32, -1);
            if r < 0 {
                if *libc::__errno_location() == libc::EINTR {
                    return Ok(0);
                }
                return errno_err!("epoll_wait() failed");
            }
            Ok(r as usize)
        }
    }
}

impl Drop for Epoll {
    fn drop(&mut self) {
        unsafe {
            let r = libc::close(self.fd);
            if r != 0 { eprintln!("warning: close() failed on epoll fd: {:?}", io::Error::last_os_error()); }
        }
    }
}

pub struct TimerFD {
    pub fd: i32,
}

impl TimerFD {
    pub fn new() -> Self {
        let fd = unsafe {libc::timerfd_create(libc::CLOCK_MONOTONIC, libc::TFD_CLOEXEC | libc::TFD_NONBLOCK)};
        if fd < 0 { panic!("timerfd_create() failed: {:?}", io::Error::last_os_error()); }
        Self {fd}
    }

    pub fn set(&self, value_ns: usize, interval_ns: usize) -> usize {
        const NANO: usize = 1000_000_000;
        let to_timespec = |ns| libc::timespec {tv_sec: (ns / NANO) as i64, tv_nsec: (ns % NANO) as i64};
        let v = libc::itimerspec {it_value: to_timespec(value_ns), it_interval: to_timespec(interval_ns)};
        let mut prev: libc::itimerspec = unsafe {std::mem::zeroed()};
        let r = unsafe {libc::timerfd_settime(self.fd, 0, &v as *const libc::itimerspec, &mut prev as *mut libc::itimerspec)};
        if r != 0 { panic!("timerfd_settime() failed: {:?}", io::Error::last_os_error()); }
        prev.it_value.tv_sec as usize * NANO + prev.it_value.tv_nsec as usize
    }

    pub fn read(&self) -> usize {
        loop {
            let mut res = 0usize;
            let r = unsafe {libc::read(self.fd, &mut res as *mut usize as *mut libc::c_void, 8)};
            if r < 0 && io::Error::last_os_error().kind() == io::ErrorKind::Interrupted {
                continue;
            }
            // Once in a blue moon it fails with errno 11 (WOULDBLOCK). Idk why, guess timerfd just can cause spurious epoll wakeups. Let's ignore it.
            if r < 0 && io::Error::last_os_error().kind() == io::ErrorKind::WouldBlock {
                return 0;
            }
            if r != 8 { panic!("read() from timerfd returned {}: {:?}", r, io::Error::last_os_error()); }
            return res;
        }
    }
}

impl Drop for TimerFD {
    fn drop(&mut self) {
        unsafe {
            let r = libc::close(self.fd);
            if r != 0 { eprintln!("warning: close() failed on timer fd: {:?}", io::Error::last_os_error()); }
        }
    }
}

pub struct EventFD {
    pub fd: i32,
}

impl EventFD {
    pub fn new() -> Self {
        let fd = unsafe {libc::eventfd(0, libc::EFD_CLOEXEC | libc::EFD_NONBLOCK)};
        if fd < 0 { panic!("eventfd() failed: {:?}", io::Error::last_os_error()); }
        Self {fd}
    }

    pub fn read(&self) -> usize {
        loop {
            let mut res = 0usize;
            let r = unsafe {libc::read(self.fd, &mut res as *mut usize as *mut libc::c_void, 8)};
            if r < 0 && io::Error::last_os_error().kind() == io::ErrorKind::Interrupted {
                continue;
            }
            if r != 8 { panic!("read() from eventfd returned {}: {:?}", r, io::Error::last_os_error()); }
            return res;
        }
    }

    pub fn write(&self, v: usize) {
        loop {
            let r = unsafe {libc::write(self.fd, &v as *const usize as *const libc::c_void, 8)};
            if r < 0 && io::Error::last_os_error().kind() == io::ErrorKind::Interrupted {
                continue;
            }
            if r != 8 { panic!("write() to eventfd returned {}: {:?}", r, io::Error::last_os_error()); }
            break;
        }
    }
}

impl Drop for EventFD {
    fn drop(&mut self) {
        unsafe {
            let r = libc::close(self.fd);
            if r != 0 { eprintln!("warning: close() failed on event fd: {:?}", io::Error::last_os_error()); }
        }
    }
}

// Usage: offsetof!(MyStruct, some_field)
// Works with nested structs too: offsetof!(MyStruct, some_field.some_subfield)
#[macro_export]
macro_rules! offsetof {
    ($t:ty, $f:ident $(. $p:ident)*) => (
        // Maybe this is UB, idk. Still seems better than pulling a whole crate for this.
        unsafe {mem::transmute::<_, usize>(&mem::transmute::<_, &$t>(8usize).$f$(.$p)*) - 8}
    )
}

// Uuuugh.
const SIGNAL_NAMES: [&str; 32] = ["[unknown signal number]", "SIGHUP", "SIGINT", "SIGQUIT", "SIGILL", "SIGTRAP", "SIGABRT", "SIGBUS", "SIGFPE", "SIGKILL", "SIGUSR1", "SIGSEGV", "SIGUSR2", "SIGPIPE", "SIGALRM", "SIGTERM", "SIGSTKFLT", "SIGCHLD", "SIGCONT", "SIGSTOP", "SIGTSTP", "SIGTTIN", "SIGTTOU", "SIGURG", "SIGXCPU", "SIGXFSZ", "SIGVTALRM", "SIGPROF", "SIGWINCH", "SIGIO", "SIGPWR", "SIGSYS"];
const ERRNO_NAMES: [&str; 134] = ["[success]", "EPERM", "ENOENT", "ESRCH", "EINTR", "EIO", "ENXIO", "E2BIG", "ENOEXEC", "EBADF", "ECHILD", "EAGAIN", "ENOMEM", "EACCES", "EFAULT", "ENOTBLK", "EBUSY", "EEXIST", "EXDEV", "ENODEV", "ENOTDIR", "EISDIR", "EINVAL", "ENFILE", "EMFILE", "ENOTTY", "ETXTBSY", "EFBIG", "ENOSPC", "ESPIPE", "EROFS", "EMLINK", "EPIPE", "EDOM", "ERANGE", "EDEADLK", "ENAMETOOLONG", "ENOLCK", "ENOSYS", "ENOTEMPTY", "ELOOP", "[unknown errno]", "ENOMSG", "EIDRM", "ECHRNG", "EL2NSYNC", "EL3HLT", "EL3RST", "ELNRNG", "EUNATCH", "ENOCSI", "EL2HLT", "EBADE", "EBADR", "EXFULL", "ENOANO", "EBADRQC", "EBADSLT", "[unknown errno]", "EBFONT", "ENOSTR", "ENODATA", "ETIME", "ENOSR", "ENONET", "ENOPKG", "EREMOTE", "ENOLINK", "EADV", "ESRMNT", "ECOMM", "EPROTO", "EMULTIHOP", "EDOTDOT", "EBADMSG", "EOVERFLOW", "ENOTUNIQ", "EBADFD", "EREMCHG", "ELIBACC", "ELIBBAD", "ELIBSCN", "ELIBMAX", "ELIBEXEC", "EILSEQ", "ERESTART", "ESTRPIPE", "EUSERS", "ENOTSOCK", "EDESTADDRREQ", "EMSGSIZE", "EPROTOTYPE", "ENOPROTOOPT", "EPROTONOSUPPORT", "ESOCKTNOSUPPORT", "EOPNOTSUPP", "EPFNOSUPPORT", "EAFNOSUPPORT", "EADDRINUSE", "EADDRNOTAVAIL", "ENETDOWN", "ENETUNREACH", "ENETRESET", "ECONNABORTED", "ECONNRESET", "ENOBUFS", "EISCONN", "ENOTCONN", "ESHUTDOWN", "ETOOMANYREFS", "ETIMEDOUT", "ECONNREFUSED", "EHOSTDOWN", "EHOSTUNREACH", "EALREADY", "EINPROGRESS", "ESTALE", "EUCLEAN", "ENOTNAM", "ENAVAIL", "EISNAM", "EREMOTEIO", "EDQUOT", "ENOMEDIUM", "EMEDIUMTYPE", "ECANCELED", "ENOKEY", "EKEYEXPIRED", "EKEYREVOKED", "EKEYREJECTED", "EOWNERDEAD", "ENOTRECOVERABLE", "ERFKILL", "EHWPOISON"];

pub fn signal_name(sig: i32) -> &'static str {
    // strsignal() is not thread safe, and sigabbrev_np() is not in rust libc bindings.
    let sig = sig as usize;
    SIGNAL_NAMES[if sig >= SIGNAL_NAMES.len() {0} else {sig}]
}

pub fn errno_name(errno: i32) -> &'static str {
    // There's no errno -> name (not message) function that's consistenly available in C standard library on Linux.
    let errno = errno as usize;
    if errno >= ERRNO_NAMES.len() {"[unknown errno]"} else {ERRNO_NAMES[errno]}
}

pub fn cld_code_name(code: i32) -> &'static str {
    match code {
        libc::CLD_CONTINUED => "CLD_CONTINUED",
        libc::CLD_DUMPED => "CLD_DUMPED",
        libc::CLD_EXITED => "CLD_EXITED",
        libc::CLD_KILLED => "CLD_KILLED",
        libc::CLD_STOPPED => "CLD_STOPPED",
        libc::CLD_TRAPPED => "CLD_TRAPPED",
        0 => "[none]",
        _ => "[unknown code]",
    }
}

pub fn ptrace_request_name(c: i32) -> &'static str {
    match c {
        libc::PTRACE_TRACEME => "PTRACE_TRACEME", libc::PTRACE_PEEKTEXT => "PTRACE_PEEKTEXT", libc::PTRACE_PEEKDATA => "PTRACE_PEEKDATA", libc::PTRACE_PEEKUSER => "PTRACE_PEEKUSER", libc::PTRACE_POKETEXT => "PTRACE_POKETEXT", libc::PTRACE_POKEDATA => "PTRACE_POKEDATA", libc::PTRACE_POKEUSER => "PTRACE_POKEUSER", libc::PTRACE_CONT => "PTRACE_CONT", libc::PTRACE_KILL => "PTRACE_KILL", libc::PTRACE_SINGLESTEP => "PTRACE_SINGLESTEP", libc::PTRACE_GETREGS => "PTRACE_GETREGS", libc::PTRACE_SETREGS => "PTRACE_SETREGS", libc::PTRACE_GETFPREGS => "PTRACE_GETFPREGS", libc::PTRACE_SETFPREGS => "PTRACE_SETFPREGS", libc::PTRACE_ATTACH => "PTRACE_ATTACH", libc::PTRACE_DETACH => "PTRACE_DETACH", libc::PTRACE_GETFPXREGS => "PTRACE_GETFPXREGS", libc::PTRACE_SETFPXREGS => "PTRACE_SETFPXREGS", libc::PTRACE_SYSCALL => "PTRACE_SYSCALL", libc::PTRACE_SYSEMU => "PTRACE_SYSEMU", libc::PTRACE_SYSEMU_SINGLESTEP => "PTRACE_SYSEMU_SINGLESTEP", libc::PTRACE_SETOPTIONS => "PTRACE_SETOPTIONS", libc::PTRACE_GETEVENTMSG => "PTRACE_GETEVENTMSG", libc::PTRACE_GETSIGINFO => "PTRACE_GETSIGINFO", libc::PTRACE_SETSIGINFO => "PTRACE_SETSIGINFO", libc::PTRACE_GETREGSET => "PTRACE_GETREGSET", libc::PTRACE_SETREGSET => "PTRACE_SETREGSET", libc::PTRACE_SEIZE => "PTRACE_SEIZE", libc::PTRACE_INTERRUPT => "PTRACE_INTERRUPT", libc::PTRACE_LISTEN => "PTRACE_LISTEN", libc::PTRACE_PEEKSIGINFO => "PTRACE_PEEKSIGINFO", /*libc::PTRACE_GET_SYSCALL_INFO => "PTRACE_GET_SYSCALL_INFO", libc::PTRACE_GET_RSEQ_CONFIGURATION => "PTRACE_GET_RSEQ_CONFIGURATION",*/
        _ => "[unknown request]",
    }
}

// A thing for limiting the number of warnings printed from each line of source code.
// Usage: if limiter.check(line!()) { eprintln!("warning: ...") }
pub struct Limiter {
    count_by_line: Vec<u16>,
}

impl Limiter {
    pub fn new() -> Self { Self {count_by_line: Vec::new()} }

    // Allow one per line.
    pub fn check(&mut self, line: u32) -> bool {
        self.check_n(line, 1)
    }

    // Allow n per line.
    pub fn check_n(&mut self, line: u32, n: u16) -> bool {
        let idx = line as usize;
        if idx >= self.count_by_line.len() {
            self.count_by_line.resize(idx + 1, 0);
        }
        if self.count_by_line[idx] < n {
            self.count_by_line[idx] += 1;
            true
        } else {
            false
        }        
    }
}



#[repr(align(128))]
pub struct CachePadded<T> {
    pub t: T,
}

impl<T> CachePadded<T> {
    pub fn new(t: T) -> Self { Self {t: t} }
    pub fn into_inner(self) -> T { self.t }
}
unsafe impl<T: Send> Send for CachePadded<T> {}
unsafe impl<T: Sync> Sync for CachePadded<T> {}
impl<T> Deref for CachePadded<T> { type Target = T; fn deref(&self) -> &T { &self.t } }
impl<T> DerefMut for CachePadded<T> { fn deref_mut(&mut self) -> &mut T { &mut self.t } }
impl<T> From<T> for CachePadded<T> { fn from(t: T) -> Self { CachePadded::new(t) } }

pub struct ScopeGuard<F: FnOnce()> { f: ManuallyDrop<F> }
impl<F: FnOnce()> ScopeGuard<F> { pub fn new(f: F) -> Self { Self {f: ManuallyDrop::new(f)} } }
impl<T: FnOnce()> Drop for ScopeGuard<T> { fn drop(&mut self) { (unsafe {ManuallyDrop::take(&mut self.f)})(); } }
#[macro_export]
macro_rules! defer { ($($t:tt)*) => { let _guard = ScopeGuard::new(|| { $($t)* }); }; }

// Prints numbers with a few digits of precision and optional K/M/G suffix, e.g. "42", "12.4K", "123M".
pub struct PrettyCount(pub usize);
impl fmt::Display for PrettyCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (den, name) = match self.0 {
            x if x < 1000 => return write!(f, "{}", x),
            x if x < 1000_000 => (1000, "K"),
            x if x < 1000_000_000 => (1000_000, "M"),
            x if x < 1000_000_000_000usize => (1000_000_000, "G"),  
            x if x < 1000_000_000_000_000usize => (1000_000_000_000, "T"),
            x if x < 1000_000_000_000_000_000usize => (1000_000_000_000_000, "P"),
            _ => (1000_000_000_000_000_000usize, "E"),
        };
        // Use at most 4 digits.
        match self.0 as f64 / den as f64 {
            x if x < 9.995 - 1e-12 => write!(f, "{:.2} {}", x, name), // e.g. 1.23
            x if x < 99.95 - 1e-12 => write!(f, "{:.1} {}", x, name), // e.g. 12.3
            x                      => write!(f, "{:.0} {}", x, name), // e.g. 1000
        }
    }
}
impl PrettyCount {
    pub const MAX_LEN: usize = 6;
}

// Prints byte sizes with a few digits of precision and B/KiB/MiB/GiB/TiB suffix, e.g. "42 B", "12.4 KiB".
// The result is at most PrettySize::MAX_LEN characters long.
pub struct PrettySize(pub usize);
impl fmt::Display for PrettySize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (den, name) = match self.0 {
            x if x < 1usize<<10 => return write!(f, "{} B", x),
            x if x < 1usize<<20 => (1usize<<10, "KiB"),
            x if x < 1usize<<30 => (1usize<<20, "MiB"),
            x if x < 1usize<<40 => (1usize<<30, "GiB"),
            x if x < 1usize<<50 => (1usize<<40, "TiB"),
            x if x < 1usize<<60 => (1usize<<50, "PiB"),
            _              => (1usize<<60, "EiB"),
        };
        // Use at most 4 digits.
        match self.0 as f64 / den as f64 {
            x if x < 9.995 - 1e-12 => write!(f, "{:.2} {}", x, name), // e.g. 1.23
            x if x < 99.95 - 1e-12 => write!(f, "{:.1} {}", x, name), // e.g. 12.3
            x                      => write!(f, "{:.0} {}", x, name), // e.g. 1023
        }
    }
}
impl PrettySize {
    pub const MAX_LEN: usize = 8;
}

pub struct PrettyDuration(pub f64); // seconds
impl fmt::Display for PrettyDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (den, name) = match self.0 {
            x if x < 1e-9 => (1e-12, "ps"),
            x if x < 1e-6 => (1e-9, "ns"),
            x if x < 1e-3 => (1e-6, "us"),
            x if x < 1e0 => (1e-3, "ms"),
            x if x < 3600.0 => (1e0, "s"),
            x if x < 3600.0 * 24.0 => (3600.0, "h"),
            x if x < 3600.0 * 24.0 * 7.0 => (3600.0 * 24.0, "d"),
            x if x < 3600.0 * 24.0 * 7.0 * 9999.0 => (3600.0 * 24.0 * 7.0, "w"),
            _ => return write!(f, ">180 y"),
        };
        // Use at most 4 digits.
        match self.0 as f64 / den as f64 {
            x if x < 9.995 - 1e-12 => write!(f, "{:.2} {}", x, name), // e.g. 1.23
            x if x < 99.95 - 1e-12 => write!(f, "{:.1} {}", x, name), // e.g. 12.3
            x                      => write!(f, "{:.0} {}", x, name), // e.g. 1023
        }
    }
}
impl PrettyDuration {
    pub const MAX_LEN: usize = 7;
}

#[derive(Ord, Eq)]
struct MergeIteratorEntry<K: Ord> {
    key: K,
    src: usize,
}
impl<K: Ord> PartialOrd for MergeIteratorEntry<K> { fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> { Some(self.key.cmp(&other.key)) } }
impl<K: Ord> PartialEq for MergeIteratorEntry<K> { fn eq(&self, other: &Self) -> bool { self.key == other.key } }

pub struct MergeIterator<'a, T, K: Ord, F: FnMut(&T) -> K> {
    sources: Vec<&'a [T]>,
    key_func: F,
    heap: BinaryHeap<MergeIteratorEntry<K>>,
}

impl<'a, T, K: Ord, F: FnMut(&T) -> K> MergeIterator<'a, T, K, F> {
    pub fn new(sources: Vec<&'a [T]>, key_func: F) -> Self {
        let mut r = Self {sources, key_func, heap: BinaryHeap::new()};
        for (i, s) in r.sources.iter().enumerate() {
            if !s.is_empty() {
                r.heap.push(MergeIteratorEntry {key: (r.key_func)(&s[0]), src: i});
            }
        }
        r
    }

    pub fn peek(&self) -> Option<&'a T> {
        match self.heap.peek() {
            None => None,
            Some(e) => Some(&self.sources[e.src][0]) }
    }
}

impl<'a, T, K: Ord, F: FnMut(&T) -> K> Iterator for MergeIterator<'a, T, K, F> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let entry = match self.heap.pop() {
            None => return None,
            Some(e) => e };
        let src = &mut self.sources[entry.src];
        let r = &src[0];
        *src = &src[1..];
        if !src.is_empty() {
            self.heap.push(MergeIteratorEntry {key: (self.key_func)(&src[0]), src: entry.src});
        }
        Some(r)
    }
}

// SyncUnsafeCell copied from the nightly standard library.
#[repr(transparent)]
pub struct SyncUnsafeCell<T> { value: UnsafeCell<T> }
unsafe impl<T: Sync> Sync for SyncUnsafeCell<T> {}
impl<T> SyncUnsafeCell<T> {
    pub fn new(value: T) -> Self { Self { value: UnsafeCell::new(value) } }
    pub fn into_inner(self) -> T { self.value.into_inner() }
    #[inline]
    pub fn get(&self) -> *mut T { self.value.get() }
    #[inline]
    pub fn get_mut(&mut self) -> &mut T { self.value.get_mut() }
}
impl<T> From<T> for SyncUnsafeCell<T> { fn from(t: T) -> SyncUnsafeCell<T> { SyncUnsafeCell::new(t) } }

pub fn hash<T: Hash + ?Sized>(value: &T) -> usize {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish() as usize
}

pub struct HashingBufReader<R: Read> {
    inner: R,
    buf: Vec<u8>,
    filled: usize,
    pos: usize,
    pub hasher: md5::Context,
    pub count: usize,
}
impl<R: Read> HashingBufReader<R> {
    pub fn new(inner: R) -> Self { Self {inner, buf: vec![0u8; 1 << 13], filled: 0, pos: 0, hasher: md5::Context::new(), count: 0} }
}
impl<R: Read> BufRead for HashingBufReader<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        if self.pos >= self.filled {
            self.filled = self.inner.read(&mut self.buf)?;
            self.pos = 0;
        }
        Ok(&self.buf[self.pos..self.filled])
    }
    fn consume(&mut self, amt: usize) {
        self.hasher.consume(&self.buf[self.pos..self.pos + amt]);
        self.count += amt;
        self.pos += amt;
    }
}
impl<R: Read> Read for HashingBufReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let b = self.fill_buf()?;
        let n = b.len().min(buf.len());
        buf[..n].copy_from_slice(&b[..n]);
        Ok(n)
    }
}

pub trait ByteRead : BufRead {
    fn read_u8(&mut self) -> io::Result<u8>;
    fn read_u16(&mut self) -> io::Result<u16>;
    fn read_u32(&mut self) -> io::Result<u32>;
    fn read_u64(&mut self) -> io::Result<u64>;
    fn read_usize(&mut self) -> io::Result<usize>;
    fn read_i8(&mut self) -> io::Result<i8>;
    fn read_i16(&mut self) -> io::Result<i16>;
    fn read_i32(&mut self) -> io::Result<i32>;
    fn read_i64(&mut self) -> io::Result<i64>;
    fn read_isize(&mut self) -> io::Result<isize>;
    fn read_slice(&mut self) -> io::Result<Vec<u8>>;
    fn read_str(&mut self) -> io::Result<String>;
    fn read_path(&mut self) -> io::Result<PathBuf>;
}
impl<R: BufRead> ByteRead for R {
    fn read_u8(&mut self) -> io::Result<u8> { let mut b = [0u8]; self.read_exact(&mut b)?; Ok(b[0]) }
    fn read_u16(&mut self) -> io::Result<u16> { let mut b = [0u8; 2]; self.read_exact(&mut b)?; Ok(u16::from_le_bytes(b)) }
    fn read_u32(&mut self) -> io::Result<u32> { let mut b = [0u8; 4]; self.read_exact(&mut b)?; Ok(u32::from_le_bytes(b)) }
    fn read_u64(&mut self) -> io::Result<u64> { let mut b = [0u8; 8]; self.read_exact(&mut b)?; Ok(u64::from_le_bytes(b)) }
    fn read_usize(&mut self) -> io::Result<usize> { let mut b = [0u8; 8]; self.read_exact(&mut b)?; Ok(usize::from_le_bytes(b)) }
    fn read_i8(&mut self) -> io::Result<i8> { let mut b = [0u8]; self.read_exact(&mut b)?; Ok(b[0] as i8) }
    fn read_i16(&mut self) -> io::Result<i16> { let mut b = [0u8; 2]; self.read_exact(&mut b)?; Ok(i16::from_le_bytes(b)) }
    fn read_i32(&mut self) -> io::Result<i32> { let mut b = [0u8; 4]; self.read_exact(&mut b)?; Ok(i32::from_le_bytes(b)) }
    fn read_i64(&mut self) -> io::Result<i64> { let mut b = [0u8; 8]; self.read_exact(&mut b)?; Ok(i64::from_le_bytes(b)) }
    fn read_isize(&mut self) -> io::Result<isize> { let mut b = [0u8; 8]; self.read_exact(&mut b)?; Ok(isize::from_le_bytes(b)) }
    fn read_slice(&mut self) -> io::Result<Vec<u8>> {
        let n = self.read_usize()?;
        // Don't unconditionally allocate the whole n bytes because it may be huge if the input contains garbage.
        // We could just refuse to read slices longer than e.g. 100 MB, but for no particular reason we're being
        // more permissive and read in chunks until we either complete or reach end of file.
        let block_size = 1usize << 20;
        let mut v: Vec<u8> = Vec::new();
        let mut pos = 0usize;
        while pos < n {
            let add = block_size.min(n - pos);
            v.resize(v.len() + add, 0u8);
            self.read_exact(&mut v[pos..])?;
            pos += add;
        }
        Ok(v)
    }
    fn read_str(&mut self) -> io::Result<String> { let v = self.read_slice()?; String::from_utf8(v).map_err(|e| io::Error::new(io::ErrorKind::Other, e)) }
    fn read_path(&mut self) -> io::Result<PathBuf> { let v = self.read_slice()?; Ok(PathBuf::from(OsString::from_vec(v))) }
}

pub trait ByteWrite : Write {
    fn write_u8(&mut self, x: u8) -> io::Result<()>;
    fn write_u16(&mut self, x: u16) -> io::Result<()>;
    fn write_u32(&mut self, x: u32) -> io::Result<()>;
    fn write_u64(&mut self, x: u64) -> io::Result<()>;
    fn write_usize(&mut self, x: usize) -> io::Result<()>;
    fn write_i8(&mut self, x: i8) -> io::Result<()>;
    fn write_i16(&mut self, x: i16) -> io::Result<()>;
    fn write_i32(&mut self, x: i32) -> io::Result<()>;
    fn write_i64(&mut self, x: i64) -> io::Result<()>;
    fn write_isize(&mut self, x: isize) -> io::Result<()>;
    fn write_slice(&mut self, s: &[u8]) -> io::Result<()>;
    fn write_str(&mut self, s: &str) -> io::Result<()>;
    fn write_path(&mut self, s: &Path) -> io::Result<()>;
}
impl<R: Write> ByteWrite for R {
    fn write_u8(&mut self, x: u8) -> io::Result<()> { self.write_all(&[x]) }
    fn write_u16(&mut self, x: u16) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_u32(&mut self, x: u32) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_u64(&mut self, x: u64) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_usize(&mut self, x: usize) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_i8(&mut self, x: i8) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_i16(&mut self, x: i16) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_i32(&mut self, x: i32) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_i64(&mut self, x: i64) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_isize(&mut self, x: isize) -> io::Result<()> { self.write_all(&x.to_le_bytes()) }
    fn write_slice(&mut self, s: &[u8]) -> io::Result<()> { self.write_usize(s.len())?; self.write_all(s) }
    fn write_str(&mut self, s: &str) -> io::Result<()> { self.write_slice(s.as_bytes()) }
    fn write_path(&mut self, s: &Path) -> io::Result<()> { self.write_slice(s.as_os_str().as_bytes()) }
}

// sysconf(_SC_CLK_TCK), assigned at the start of main().
static mut SYSCONF_SC_CLK_TCK: usize = 0;
static mut SYSCONF_PAGE_SIZE: usize = 0;
static mut MY_PID: pid_t = 0;

#[allow(non_snake_case)]
pub fn sysconf_SC_CLK_TCK() -> usize {
    let r = unsafe {SYSCONF_SC_CLK_TCK};
    debug_assert!(r != 0);
    r
}
#[allow(non_snake_case)]
pub fn sysconf_PAGE_SIZE() -> usize {
    let r = unsafe {SYSCONF_PAGE_SIZE};
    debug_assert!(r != 0);
    r
}

pub fn my_pid() -> pid_t {
    unsafe {MY_PID}
}

pub fn precalc_global_constants() {
    let assert_nonzero = |x: usize| -> usize {
        assert!(x != 0);
        x
    };
    
    unsafe {SYSCONF_SC_CLK_TCK = assert_nonzero(libc::sysconf(libc::_SC_CLK_TCK) as usize)};
    unsafe {SYSCONF_PAGE_SIZE = assert_nonzero(libc::sysconf(libc::_SC_PAGE_SIZE) as usize)};
    unsafe {MY_PID = assert_nonzero(libc::getpid() as usize) as pid_t};
}

#[macro_export]
macro_rules! D {
    () => {
        Default::default()
    };
}

pub trait SetMinMax {
    fn set_min(&mut self, other: Self);
    fn set_max(&mut self, other: Self);
}

impl<T: Ord+Copy> SetMinMax for T {
    fn set_min(&mut self, other: Self) {
        *self = std::cmp::min(*self, other);
    }
    fn set_max(&mut self, other: Self) {
        *self = std::cmp::max(*self, other);
    }
}
