#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use nnd::{*, elf::*, error::*, debugger::*, util::*, ui::*, log::*, process_info::*, symbols::*, symbols_registry::*, procfs::*, unwind::*, range_index::*, settings::*, context::*, executor::*, persistent::*, doc::*, terminal::*, common_ui::*};
use std::{rc::Rc, mem, fs, os::fd::{FromRawFd}, io::Read, io, io::Write, panic, process, thread, thread::ThreadId, cell::UnsafeCell, ptr, pin::Pin, sync::Arc, str::FromStr, path::PathBuf};
use libc::{self, STDIN_FILENO, pid_t};

// Pipes written from corresponding signal handlers, read from main loop. We could use one pipe and write signal number to it, but it would break in the unlikely case when one signal fills up the whole pipe before the main loop drains it - then other signals would get lost. Probably not actually important.
static mut SIGNAL_PIPES_WRITE: [i32; 32] = [-1; 32];

static mut MAIN_THREAD_ID: UnsafeCell<Option<ThreadId>> = UnsafeCell::new(None);
static mut DEBUGGER_TO_DROP_ON_PANIC: UnsafeCell<Option<*mut Debugger>> = UnsafeCell::new(None);

extern "C" fn signal_handler(sig: i32, _: *mut libc::siginfo_t, _: *mut libc::c_void) {
    unsafe {
        libc::write(SIGNAL_PIPES_WRITE[sig as usize], "+".as_ptr() as *const libc::c_void, 1);
    }
}

fn drain_signal_pipe(fd: i32) {
    unsafe {
        let mut temp = [0u8; 128];
        libc::read(fd, temp.as_mut_ptr() as *mut libc::c_void, temp.len());
    }
}

fn parse_arg(args: &mut &[String], long_name: &str, short_name: &str, bool_switch: bool) -> Option<String> {
    assert!(!args.is_empty());
    if args[0].is_empty() {
        return None;
    }
    if !long_name.is_empty() && args[0].starts_with(long_name) && args[0][long_name.len()..].starts_with("=") {
        if bool_switch {
            eprintln!("{} doesn't accept a value", long_name);
            process::exit(1);
        }
        let v = args[0][long_name.len()+1..].to_string();
        *args = &args[1..];
        return Some(v);
    }
    if &args[0][..] == short_name || &args[0][..] == long_name {
        if bool_switch {
            *args = &args[1..];
            return Some(String::new());
        }
        if args.len() == 1 {
            eprintln!("{} requires an argument", long_name);
            process::exit(1);
        }
        let v = args[1].clone();
        *args = &args[2..];
        return Some(v);
    }
    None
}

fn main() {
    precalc_globals_util();
    std::env::set_var("RUST_BACKTRACE", "short");

    let mut settings = Settings::default();
    let mut attach_pid: Option<pid_t> = None;
    let mut command_line: Option<Vec<String>> = None;
    let mut tty_file: Option<String> = None;

    let all_args: Vec<String> = std::env::args().collect();
    let mut args = &all_args[1..];
    while !args.is_empty() && args[0].starts_with("-") {
        if let Some(v) = parse_arg(&mut args, "--pid", "-p", false) {
            attach_pid = match pid_t::from_str(&v) {
                Err(_) => {
                    eprintln!("invalid pid: {}", v);
                    process::exit(1);
                }
                Ok(x) => Some(x),
            };
        } else if let Some(v) = parse_arg(&mut args, "--tty", "-t", false) {
            tty_file = Some(v);
        } else if let Some(v) = parse_arg(&mut args, "--stdin", "", false) {
            settings.stdin_file = Some(v);
        } else if let Some(v) = parse_arg(&mut args, "--stdout", "", false) {
            settings.stdout_file = Some(v);
        } else if let Some(v) = parse_arg(&mut args, "--stderr", "", false) {
            settings.stderr_file = Some(v);
        } else if let Some(_) = parse_arg(&mut args, "--continue", "-c", true) {
            settings.stop_on_initial_exec = false;
        } else if let Some(m) = parse_arg(&mut args, "--mouse", "-m", false) {
            settings.mouse_mode = match &m.to_lowercase()[..] {
                "full" => MouseMode::Full,
                "no-hover" => MouseMode::NoHover,
                "disabled" | "no" | "none" => MouseMode::Disabled,
                _ => {
                    eprintln!("unrecognized --mouse mode: '{}'; expected one of: full, no-hover, disabled", m);
                    process::exit(1);
                }
            };
        } else if let Some(_) = parse_arg(&mut args, "--echo-input", "", true) {
            match run_input_echo_tool() {
                Ok(()) => (),
                Err(e) => eprintln!("error: {}", e),
            }
            return;
        } else if let Some(_) = parse_arg(&mut args, "--fixed-fps", "", true) {
            settings.fixed_fps = true;
        } else if let Some(path) = parse_arg(&mut args, "--dir", "-d", false) {
            settings.code_dirs.push(PathBuf::from(path));
        } else if print_help_chapter(&args[0], &all_args[0]) {
            process::exit(0);
        } else {
            eprintln!("unrecognized argument: '{}' (if it's the command to run, prepend '\\' to escape)", args[0]);
            process::exit(1);
        }
    }
    if settings.code_dirs.is_empty() {
        settings.code_dirs.push(PathBuf::from(""));
    }

    if !args.is_empty() {
        let mut cmd = args[..].to_vec();
        if cmd[0].starts_with("\\") {
            cmd[0] = cmd[0][1..].to_string();
        }
        command_line = Some(cmd);
    }

    if let Some(v) = tty_file {
        settings.stdin_file = Some(v.clone());
        settings.stdout_file = Some(v);
    }

    if attach_pid.is_none() && command_line.is_none() {
        eprintln!("usage: {} (-p pid | path [args..] | --help)", all_args[0]);
        process::exit(1);
    }
    if attach_pid.is_some() && command_line.is_some() {
        eprintln!("can't have both --pid and command line");
        process::exit(1);
    }
    if command_line.is_none() && (settings.stdin_file.is_some() || settings.stdout_file.is_some() || settings.stderr_file.is_some()) {
        eprintln!("can't have both --pid and --stdin/--stdout/--stderr/--tty");
        process::exit(1);
    }

    // This redirects stderr to the log file, so we have to do it early.
    let persistent = PersistentState::init();

    // There are two pieces of best-effort cleanup we need to do on panic:
    //  * Remove breakpoints and detach from the process, if attached with -p.
    //    Currently we only do it if the panic happened in the main thread.
    //  * Restore terminal state.
    // We do these things in panic handler.
    // We avoid unwinding and calling destructors because that can cause additional bogus errors.
    //
    // (Alternatively, we could panic-safe all the code, propagate panics to the main thread, and enable unwinding.
    //  But that sounds worse: (a) more work, (b) hard to have adequate test coverage for all possible panic unwind scenarios.
    //  E.g. maybe some destructor anywhere in the code base inadvertently expects the struct to be in certain clean state -
    //  usually no one will ever notice that until it happens in practice. Or maybe some destructor does something unnecessary and slow,
    //  e.g. wait for background work to complete. In contrast, the panic handler is small and doesn't make many assumptions.)
    let (log_file_path, original_stderr_fd) = (persistent.log_file_path.clone(), persistent.original_stderr_fd);
    unsafe {*MAIN_THREAD_ID.get() = Some(thread::current().id())}; // assign before spawning any threads
    {
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            // Print stack trace.
            default_hook(info);

            // Remove breakpoints and detach. This is sketchy if the panic was thrown from inside a Debugger method,
            // but should be almost always fine, and this is best-effort anyway.
            unsafe {
                if Some(thread::current().id()) == *MAIN_THREAD_ID.get() {
                    if let Some(d) = *DEBUGGER_TO_DROP_ON_PANIC.get() {
                        ptr::drop_in_place(d);
                    }
                }
            }

            // This is not fully reliable because other threads still execute while this hook is running,
            // so the main thread may hide cursor or output a rendered frame after we restored the terminal.
            // To minimize the chance of that, we put this near the end of the hook, importantly after the slow default hook.
            restore_terminal();

            // Print something to the console, otherwise it'll be left confusingly empty, as if the debugger just quit silently.
            if let &Some(fd) = &original_stderr_fd {
                let mut original_stderr = std::mem::ManuallyDrop::new(unsafe {std::fs::File::from_raw_fd(fd)});
                let _ = writeln!(original_stderr, "panic! this usually indicates a bug, please report (to #debugger in slack, or to mk.al13n@gmail.com)");
                if let Some(p) = &log_file_path {
                    let _ = writeln!(original_stderr, "there should be an error message and stack trace at the end of {} - please include them in the report", p.display());
                }
                let _ = original_stderr.flush();
            }

            process::exit(2); // this is redundant with panic='abort', but why not
        }));
    }

    match run(settings, attach_pid, command_line, persistent) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("fatal: {}", e);
            if let &Some(fd) = &original_stderr_fd {
                let mut original_stderr = std::mem::ManuallyDrop::new(unsafe {std::fs::File::from_raw_fd(fd)});
                let _ = writeln!(original_stderr, "error: {}", e);
                let _ = original_stderr.flush();
            }
            process::exit(1);
        }
    }
}

fn run(settings: Settings, attach_pid: Option<pid_t>, command_line: Option<Vec<String>>, persistent: PersistentState) -> Result<()> {
    let num_threads = thread::available_parallelism().map_or(8, |n| n.get()).min(settings.max_threads).max(1);
    let context = Arc::new(Context {settings, executor: Executor::new(num_threads), wake_main_thread: Arc::new(EventFD::new())});

    let epoll = Rc::new(Epoll::new()?);

    // Forward some signals into pipes that wake up the main loop.
    // SIGCHLD is raised when we need to frob ptrace.
    // (I also tried using pidfd, but it turned out to be currenlty broken/annoying in Linux in multiple ways: (1) it's not reported to the tracer if this process is itself ptraced (e.g. if you strace the debugger), (2) the signal needs to be blocked in all threads individually, (3) the fd is inherited by forked processes and needs to be closed manually.)
    let mut signal_pipes_read = [-1i32; 32];
    for sig in [libc::SIGCHLD, libc::SIGWINCH] {
        unsafe {
            // Create a pipe.
            let mut pipe = [0i32; 2];
            let r = libc::pipe2(pipe.as_mut_ptr(), libc::O_NONBLOCK | libc::O_CLOEXEC);
            if r != 0 { return errno_err!("pipe() failed"); }
            SIGNAL_PIPES_WRITE[sig as usize] = pipe[1];
            signal_pipes_read[sig as usize] = pipe[0];

            // Set signal handler.
            let mut action: libc::sigaction = mem::zeroed();
            action.sa_flags = libc::SA_SIGINFO;
            action.sa_sigaction = signal_handler as libc::sighandler_t;
            let mut mask: libc::sigset_t = mem::zeroed();
            libc::sigemptyset(&mut mask);
            action.sa_mask = mask;
            let r = libc::sigaction(sig, &action, std::ptr::null_mut());
            if r != 0 { return errno_err!("sigaction() failed"); }

            epoll.add(pipe[0], libc::EPOLLIN, pipe[0] as u64)?;
        }
    }

    let frame_ns = (1e9 / context.settings.fps) as usize;
    let render_timer = TimerFD::new();

    // Timer for various periodic tasks.
    let periodic_timer = TimerFD::new();
    periodic_timer.set(1, (context.settings.periodic_timer_seconds * 1e9) as usize);

    epoll.add(STDIN_FILENO, libc::EPOLLIN, STDIN_FILENO as u64)?;
    epoll.add(render_timer.fd, libc::EPOLLIN, render_timer.fd as u64)?;
    epoll.add(periodic_timer.fd, libc::EPOLLIN, periodic_timer.fd as u64)?;

    let _restorer = TerminalRestorer;
    configure_terminal(context.settings.mouse_mode)?;

    let mut debugger: Pin<Box<Debugger>>;
    if let &Some(pid) = &attach_pid {
        debugger = Pin::new(Box::new(Debugger::attach(pid, context.clone(), persistent)?));
        unsafe { *DEBUGGER_TO_DROP_ON_PANIC.get() = Some(&mut *debugger); }
    } else {
        debugger = Pin::new(Box::new(Debugger::from_command_line(&command_line.unwrap(), context.clone(), persistent)));
        debugger.start_child()?;
    }
    defer! { unsafe { *DEBUGGER_TO_DROP_ON_PANIC.get() = None; } }

    let mut ui = DebuggerUI::new();

    match PersistentState::load_state(&mut debugger, &mut ui) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("warning: restore failed: {}", e);
            log!(debugger.log, "restore failed: {}", e);
        }
    }

    let symbols_event_fd = debugger.symbols.event_fd();
    epoll.add(symbols_event_fd.fd, libc::EPOLLIN, symbols_event_fd.fd as u64)?;

    let misc_wakeup_fd = debugger.context.wake_main_thread.clone();
    epoll.add(misc_wakeup_fd.fd, libc::EPOLLIN, misc_wakeup_fd.fd as u64)?;

    // Throttle rendering to <= fps. Render only when something changes.
    let mut pending_render = true;
    render_timer.set(1, 0);

    if context.settings.fixed_fps {
        render_timer.set(1, frame_ns);
    }

    // The debugger.process_events() path of this loop should be kept light, it'll likely be the bottleneck for conditional breakpoints (including thread-specific breakpoints, including temporary breakpoints when stepping).
    loop {
        let mut events: [libc::epoll_event; 32] = unsafe { mem::zeroed() };
        let n = epoll.wait(&mut events)?;
        let mut render_now = false;
        let mut schedule_render = false;
        for event in &events[..n] {
            let fd = event.u64 as i32;
            let mut prof = TscScopeExcludingSyscalls::new(&debugger.prof.bucket);
            schedule_render = true;
            if fd == signal_pipes_read[libc::SIGCHLD as usize] {
                drain_signal_pipe(fd);
                let drop_caches = debugger.process_events()?;
                debugger.prof.bucket.debugger_tsc += prof.restart(&debugger.prof.bucket);
                if drop_caches {
                    debugger.drop_caches()?;
                    ui.drop_caches();
                    debugger.prof.bucket.other_tsc += prof.finish(&debugger.prof.bucket);
                }
            } else if fd == signal_pipes_read[libc::SIGWINCH as usize] {
                drain_signal_pipe(fd);
            } else if fd == misc_wakeup_fd.fd {
                misc_wakeup_fd.read();
            } else if fd == STDIN_FILENO {
                let significant = ui.buffer_input(&mut debugger.prof.bucket)?;
                if !significant {
                    schedule_render = false;
                } else if !pending_render && !context.settings.fixed_fps {
                    render_now = true;
                }
            } else if fd == render_timer.fd {
                render_timer.read();
                pending_render = false;
                render_now = true;
            } else if fd == symbols_event_fd.fd {
                let drop_caches = debugger.symbols.process_events();
                if drop_caches {
                    // After symbols are loaded, recalculate things like stack traces.
                    // Currently this is somewhat incomplete: disassembly doesn't automatically scroll, and source window doesn't automatically open the file.
                    // Doing it in a non-janky way seems like a lot of code, and the janky way seems worse than nothing (e.g. if we *always* scroll the disassembly
                    // and source to current instruction when any symbols are loaded, it'll be annoying when some inconsequential small dynamic library was
                    // loaded in the middle of execution while the user is looking at code).
                    debugger.drop_caches()?;
                    ui.drop_caches();
                }
                debugger.prof.bucket.other_tsc += prof.finish(&debugger.prof.bucket);
            } else if fd == periodic_timer.fd {
                periodic_timer.read();
                debugger.refresh_resource_stats();
                PersistentState::try_to_save_state_if_changed(&mut debugger, &mut ui);
                debugger.prof.bucket.other_tsc += prof.finish(&debugger.prof.bucket);
                debugger.prof.advance_bucket();
            } else {
                return err!(Internal, "epoll returned unexpected data: {}", fd);
            }
        }

        if render_now {
            assert!(!pending_render);
            schedule_render = false;

            ui.update_and_render(&mut debugger)?;

            if ui.should_quit {
                // Clean exit. Save state and call destructors. The only important destructor is TerminalRestorer.
                PersistentState::try_to_save_state_if_changed(&mut debugger, &mut ui);
                return Ok(());
            }

            schedule_render |= ui.ui.should_redraw || ui.should_drop_caches;
            if ui.should_drop_caches {
                let prof = TscScopeExcludingSyscalls::new(&debugger.prof.bucket);
                debugger.drop_caches()?;
                ui.drop_caches();
                debugger.prof.bucket.other_tsc += prof.finish(&debugger.prof.bucket);
            }
        }

        if schedule_render && !pending_render && !context.settings.fixed_fps {
            pending_render = true;
            render_timer.set(frame_ns, 0);
        }
    }
}
