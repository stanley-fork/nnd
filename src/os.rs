use libc::{pid_t};

// Linux api stuff.
//  * Some utils.
//  * Some constants and structs that are not in libc rust crate.
//  * Some constants and structs that are in libc crate, but have pointless minor
//    differences between musl and glibc versions of the "libc" rust crate.
//    E.g. i32 vs u32, or field names prefixed or not prefixed with "__".

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

pub fn precalc_globals_os() {
    let assert_nonzero = |x: usize| -> usize {
        assert!(x != 0);
        x
    };
    
    unsafe {SYSCONF_SC_CLK_TCK = assert_nonzero(libc::sysconf(libc::_SC_CLK_TCK) as usize)};
    unsafe {SYSCONF_PAGE_SIZE = assert_nonzero(libc::sysconf(libc::_SC_PAGE_SIZE) as usize)};
    unsafe {MY_PID = assert_nonzero(libc::getpid() as usize) as pid_t};
}

pub const SHT_PROGBITS: u32 = 0x1;
pub const SHT_SYMTAB: u32 = 0x2;
pub const SHT_NOTE: u32 = 0x7;
pub const SHT_NOBITS: u32 = 0x8; // pronounced as "shit! no bits!"

pub const SHF_TLS: u64 = 1 << 10;
pub const SHF_COMPRESSED: u64 = 1 << 11;
pub const SHF_STRINGS: u64 = 1 << 5;
pub const SHF_EXECINSTR: u64 = 1 << 2;

pub const STT_FUNC: u8 = 2;
pub const STT_OBJECT: u8 = 1;

pub const SHN_UNDEF: u16 = 0;

pub const PT_LOAD: u32 = 1;
pub const PT_DYNAMIC: u32 = 2;
pub const PT_NOTE: u32 = 4;
pub const PT_GNU_EH_FRAME: u32 = 0x60000000 + 0x474e550;

// Segment permissions.
pub const PF_R: u32 = 0x4;
pub const PF_W: u32 = 0x2;
pub const PF_X: u32 = 0x1;

pub const NT_GNU_BUILD_ID: u32 = 3;

// These are used in core dumps.
pub const NT_PRSTATUS: u32 = 1;
pub const NT_PRFPREG: u32 = 2;
pub const NT_PRPSINFO: u32 = 3;
pub const NT_TASKSTRUCT: u32 = 4;
pub const NT_AUXV: u32 = 6;
pub const NT_SIGINFO: u32 = 0x53494749;
pub const NT_FILE: u32 = 0x46494c45;
pub const NT_PRXFPREG: u32 = 0x46e62b7f;
pub const NT_X86_XSTATE: u32 = 0x202;

pub const ELFCOMPRESS_ZLIB: u32 = 1;

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

pub fn trap_si_code_name(c: i32) -> &'static str {
    match c {
        libc::TRAP_BRANCH => "TRAP_BRANCH",
        libc::TRAP_BRKPT => "TRAP_BRKPT",
        libc::TRAP_HWBKPT => "TRAP_HWBKPT",
        libc::TRAP_PERF => "TRAP_PERF",
        libc::TRAP_TRACE => "TRAP_TRACE",
        libc::TRAP_UNK => "TRAP_UNK",
        libc::SI_ASYNCIO => "SI_ASYNCIO",
        libc::SI_ASYNCNL => "SI_ASYNCNL",
        libc::SI_DETHREAD => "SI_DETHREAD",
        libc::SI_KERNEL => "SI_KERNEL",
        libc::SI_MESGQ => "SI_MESGQ",
        libc::SI_QUEUE => "SI_QUEUE",
        libc::SI_SIGIO => "SI_SIGIO",
        libc::SI_TIMER => "SI_TIMER",
        libc::SI_TKILL => "SI_TKILL",
        libc::SI_USER => "SI_USER",
        _ => "[unknown si_code]",
    }
}

// Structs found in core dump notes.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct elf_prstatus {
	pub si_signo: i32, // signal number
    // These two seem to be reversed compared to siginfo_t. They also don't seem to be populated by binfmt_elf.c. The real siginfo is in NT_SIGINFO.
	pub si_code_but_actually_it_is_zero: i32,  // extra code
	pub si_errno_but_actually_it_is_zero: i32, // errno

    pub pr_cursig: i16, // Current signal
    pub pr_sigpend: usize, // Set of pending signals
    pub pr_sighold: usize, // Set of held signals
    pub pr_pid: pid_t,
    pub pr_ppid: pid_t,
    pub pr_pgrp: pid_t,
    pub pr_sid: pid_t,
    pub pr_utime: libc::timeval, // User time
    pub pr_stime: libc::timeval, // System time
    pub pr_cutime: libc::timeval, // Cumulative user time
    pub pr_cstime: libc::timeval, // Cumulative system time
    pub pr_reg: libc::user_regs_struct, // GP registers
    pub pr_fpvalid: i32, // True if math co-processor being used.
}
#[repr(C)]
#[derive(Copy, Clone)]
pub struct elf_prpsinfo {
    pub pr_state: i8, // numeric process state
    pub pr_sname: i8, // char for pr_state
    pub pr_zomb: i8, // zombie
    pub pr_nice: i8, // nice val
    pub pr_flag: u64, // flags
    pub pr_uid: u32,
    pub pr_gid: u32,
    pub pr_pid: pid_t,
    pub pr_ppid: pid_t,
    pub pr_pgrp: pid_t,
    pub pr_sid: pid_t,
    pub pr_fname: [u8; 16], // filename of executable
    pub pr_psargs: [u8; 80], // initial part of arg list
}

pub const PTRACE_TRACEME: i32 = 0;
pub const PTRACE_PEEKTEXT: i32 = 1;
pub const PTRACE_PEEKDATA: i32 = 2;
pub const PTRACE_PEEKUSER: i32 = 3;
pub const PTRACE_POKETEXT: i32 = 4;
pub const PTRACE_POKEDATA: i32 = 5;
pub const PTRACE_POKEUSER: i32 = 6;
pub const PTRACE_CONT: i32 = 7;
pub const PTRACE_KILL: i32 = 8;
pub const PTRACE_SINGLESTEP: i32 = 9;
pub const PTRACE_GETREGS: i32 = 12;
pub const PTRACE_SETREGS: i32 = 13;
pub const PTRACE_GETFPREGS: i32 = 14;
pub const PTRACE_SETFPREGS: i32 = 15;
pub const PTRACE_ATTACH: i32 = 16;
pub const PTRACE_DETACH: i32 = 17;
pub const PTRACE_GETFPXREGS: i32 = 18;
pub const PTRACE_SETFPXREGS: i32 = 19;
pub const PTRACE_SYSCALL: i32 = 24;
pub const PTRACE_GET_THREAD_AREA: i32 = 25;
pub const PTRACE_SET_THREAD_AREA: i32 = 26;
pub const PTRACE_ARCH_PRCTL: i32 = 30;
pub const PTRACE_SYSEMU: i32 = 31;
pub const PTRACE_SYSEMU_SINGLESTEP: i32 = 32;
pub const PTRACE_SINGLEBLOCK: i32 = 33;
pub const PTRACE_SETOPTIONS: i32 = 0x4200;
pub const PTRACE_GETEVENTMSG: i32 = 0x4201;
pub const PTRACE_GETSIGINFO: i32 = 0x4202;
pub const PTRACE_SETSIGINFO: i32 = 0x4203;
pub const PTRACE_GETREGSET: i32 = 0x4204;
pub const PTRACE_SETREGSET: i32 = 0x4205;
pub const PTRACE_SEIZE: i32 = 0x4206;
pub const PTRACE_INTERRUPT: i32 = 0x4207;
pub const PTRACE_LISTEN: i32 = 0x4208;
pub const PTRACE_PEEKSIGINFO: i32 = 0x4209;
pub const PTRACE_GETSIGMASK: i32 = 0x420a;
pub const PTRACE_SETSIGMASK: i32 = 0x420b;
pub const PTRACE_SECCOMP_GET_FILTER: i32 = 0x420c;
pub const PTRACE_SECCOMP_GET_METADATA: i32 = 0x420d;
pub const PTRACE_GET_SYSCALL_INFO: i32 = 0x420e;
pub const PTRACE_GET_RSEQ_CONFIGURATION: i32 = 0x420f;
pub const PTRACE_SET_SYSCALL_USER_DISPATCH_CONFIG: i32 = 0x4210;
pub const PTRACE_GET_SYSCALL_USER_DISPATCH_CONFIG: i32 = 0x4211;

pub fn ptrace_request_name(c: i32) -> &'static str {
    match c {
        PTRACE_TRACEME => "PTRACE_TRACEME", PTRACE_PEEKTEXT => "PTRACE_PEEKTEXT", PTRACE_PEEKDATA => "PTRACE_PEEKDATA", PTRACE_PEEKUSER => "PTRACE_PEEKUSER", PTRACE_POKETEXT => "PTRACE_POKETEXT", PTRACE_POKEDATA => "PTRACE_POKEDATA", PTRACE_POKEUSER => "PTRACE_POKEUSER", PTRACE_CONT => "PTRACE_CONT", PTRACE_KILL => "PTRACE_KILL", PTRACE_SINGLESTEP => "PTRACE_SINGLESTEP", PTRACE_GETREGS => "PTRACE_GETREGS", PTRACE_SETREGS => "PTRACE_SETREGS", PTRACE_GETFPREGS => "PTRACE_GETFPREGS", PTRACE_SETFPREGS => "PTRACE_SETFPREGS", PTRACE_ATTACH => "PTRACE_ATTACH", PTRACE_DETACH => "PTRACE_DETACH", PTRACE_GETFPXREGS => "PTRACE_GETFPXREGS", PTRACE_SETFPXREGS => "PTRACE_SETFPXREGS", PTRACE_SYSCALL => "PTRACE_SYSCALL", PTRACE_GET_THREAD_AREA => "PTRACE_GET_THREAD_AREA", PTRACE_SET_THREAD_AREA => "PTRACE_SET_THREAD_AREA", PTRACE_ARCH_PRCTL => "PTRACE_ARCH_PRCTL", PTRACE_SYSEMU => "PTRACE_SYSEMU", PTRACE_SYSEMU_SINGLESTEP => "PTRACE_SYSEMU_SINGLESTEP", PTRACE_SINGLEBLOCK => "PTRACE_SINGLEBLOCK", PTRACE_SETOPTIONS => "PTRACE_SETOPTIONS", PTRACE_GETEVENTMSG => "PTRACE_GETEVENTMSG", PTRACE_GETSIGINFO => "PTRACE_GETSIGINFO", PTRACE_SETSIGINFO => "PTRACE_SETSIGINFO", PTRACE_GETREGSET => "PTRACE_GETREGSET", PTRACE_SETREGSET => "PTRACE_SETREGSET", PTRACE_SEIZE => "PTRACE_SEIZE", PTRACE_INTERRUPT => "PTRACE_INTERRUPT", PTRACE_LISTEN => "PTRACE_LISTEN", PTRACE_PEEKSIGINFO => "PTRACE_PEEKSIGINFO", PTRACE_GETSIGMASK => "PTRACE_GETSIGMASK", PTRACE_SETSIGMASK => "PTRACE_SETSIGMASK", PTRACE_SECCOMP_GET_FILTER => "PTRACE_SECCOMP_GET_FILTER", PTRACE_SECCOMP_GET_METADATA => "PTRACE_SECCOMP_GET_METADATA", PTRACE_GET_SYSCALL_INFO => "PTRACE_GET_SYSCALL_INFO", PTRACE_GET_RSEQ_CONFIGURATION => "PTRACE_GET_RSEQ_CONFIGURATION", PTRACE_SET_SYSCALL_USER_DISPATCH_CONFIG => "PTRACE_SET_SYSCALL_USER_DISPATCH_CONFIG", PTRACE_GET_SYSCALL_USER_DISPATCH_CONFIG => "PTRACE_GET_SYSCALL_USER_DISPATCH_CONFIG",
        _ => "[unknown request]",
    }
}

pub const PTRACE_EVENT_FORK: i32 = 1;
pub const PTRACE_EVENT_VFORK: i32 = 2;
pub const PTRACE_EVENT_CLONE: i32 = 3;
pub const PTRACE_EVENT_EXEC: i32 = 4;
pub const PTRACE_EVENT_VFORK_DONE: i32 = 5;
pub const PTRACE_EVENT_EXIT: i32 = 6;
pub const PTRACE_EVENT_SECCOMP: i32 = 7;
pub const PTRACE_EVENT_STOP: i32 = 128;

pub const PTRACE_O_TRACESYSGOOD: u64 = 1;
pub const PTRACE_O_TRACEFORK: u64 = 1 << PTRACE_EVENT_FORK;
pub const PTRACE_O_TRACEVFORK: u64 = 1 << PTRACE_EVENT_VFORK;
pub const PTRACE_O_TRACECLONE: u64 = 1 << PTRACE_EVENT_CLONE;
pub const PTRACE_O_TRACEEXEC: u64 = 1 << PTRACE_EVENT_EXEC;
pub const PTRACE_O_TRACEVFORKDONE: u64 = 1 << PTRACE_EVENT_VFORK_DONE;
pub const PTRACE_O_TRACEEXIT: u64 = 1 << PTRACE_EVENT_EXIT;
pub const PTRACE_O_TRACESECCOMP: u64 = 1 << PTRACE_EVENT_SECCOMP;
pub const PTRACE_O_EXITKILL: u64 = 1 << 20;
pub const PTRACE_O_SUSPEND_SECCOMP: u64 = 1 << 21;
