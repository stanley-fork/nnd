#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use nnd::{*, elf::*, error::*, debugger::*, util::*, ui::*, log::*, process_info::*, symbols::*, symbols_registry::*, procfs::*, unwind::*, range_index::*, settings::*, context::*, executor::*, persistent::*};
use std::{rc::Rc, mem, fs, os::fd::{FromRawFd}, io::Read, io, io::Write, panic, process, thread, thread::ThreadId, cell::UnsafeCell, ptr, pin::Pin, sync::Arc, str::FromStr};
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

fn main() {
    std::env::set_var("RUST_BACKTRACE", "full"); // (short stack traces don't work with custom panic handlers for some reason - the __rust_begin_short_backtrace/__rust_end_short_backtrace are missing)

    let mut settings = Settings::default();
    let mut attach_pid: Option<pid_t> = None;
    let mut command_line: Option<Vec<String>> = None;

    let args: Vec<String> = std::env::args().collect();
    let mut i = 1usize;
    while i < args.len() && args[i].starts_with("-") {
        match &args[i][..] {
            "-p" => {
                attach_pid = args.get(i+1).map_or(None, |s| pid_t::from_str(s).ok());
                if attach_pid.is_none() {
                    eprintln!("usage: {} -p pid", args[0]);
                    process::exit(1);
                }
                i += 2;
            }
            // Currently there are 2 ways to debug interactive programs (i.e. requiring a tty, e.g. for TUI):
            //  * Attach to it using -p. In this case you can't set up breakpoints in advance, so can't debug things that happen at startup.
            //  * Use --in and --out to attach the debuggee to an existing pty. Like this:
            //     1. Open a terminal window (in xterm or tmux or whatever). Let's call this window A. This is where you'll be able interact with the debuggee.
            //     2. This terminal window is attached to some "pty" "device" in /dev/pts/ . Figure out which one:
            //         $ ls -l /proc/$$/fd | grep /dev/pts/
            //        For example, suppose it output /dev/pts/2 . You can also double-check this with: `echo henlo > /dev/pts/2` - if it appeared in the correct terminal then it's the correct path.
            //     3. Pacify the shell in this terminal window:
            //         $ sleep 1000000000
            //     4. In another terminal window (B) run the debugger:
            //         $ nnd --in /dev/pts/2 --out /dev/pts/2 the_program_to_debug
            //     5. Now you have the debugger running in window B while the debuggee inhabits the terminal in window A
            //        (which will come to life when you resume the debuggee in the debugger, `sleep` notwithstanding).
            "--in" => {
                settings.stdin_file = args.get(i+1).cloned();
                if settings.stdin_file.is_none() {
                    eprintln!("--in requires an argument");
                    process::exit(1);
                }
                i += 2;
            }
            "--out" => {
                settings.stdout_file = args.get(i+1).cloned();
                if settings.stdout_file.is_none() {
                    eprintln!("--out requires an argument");
                    process::exit(1);
                }
                i += 2;
            }
            x => {
                eprintln!("unrecognized argument: '{}' (if it's the command to run, prepend '\\' to escape)", x);
                process::exit(1);
            }
        }
    }
    if i < args.len() {
        command_line = Some(args[i..].to_vec());
    }
    if attach_pid.is_some() == command_line.is_some() || ((settings.stdin_file.is_some() || settings.stdout_file.is_some()) && command_line.is_none()) {
        eprintln!("usage: {} (-p pid | [--in file] [--out file] path [args..])", args[0]);
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
            // To minimize this, we put this at the end of the hook, importantly after the slow default hook.
            restore_terminal_mode();

            // Print something to the console, otherwise it'll be left confusingly empty, like the debugger just quit silently (with nonzero exit code).
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
    let context = Arc::new(Context {settings, executor: Executor::new(num_threads)});

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
    let loading_frame_ns = (1e9 / context.settings.loading_fps) as usize;
    let render_timer = TimerFD::new();
    // for fixed fps, do this here: render_timer.set(1, frame_ns), and remove other render_timer.set(...) calls

    // Periodic timer or saving state.
    let save_timer = TimerFD::new();
    save_timer.set(1, (context.settings.save_period_seconds * 1e9) as usize);

    epoll.add(STDIN_FILENO, libc::EPOLLIN, STDIN_FILENO as u64)?;
    epoll.add(render_timer.fd, libc::EPOLLIN, render_timer.fd as u64)?;
    epoll.add(save_timer.fd, libc::EPOLLIN, save_timer.fd as u64)?;

    set_terminal_to_raw_mode_etc();
    let terminal_restorer = TerminalRestorer;

    let mut debugger: Pin<Box<Debugger>>;
    if let &Some(pid) = &attach_pid {
        debugger = Pin::new(Box::new(Debugger::attach(pid, context.clone(), persistent)?));
        unsafe { *DEBUGGER_TO_DROP_ON_PANIC.get() = Some(&mut *debugger); }
    } else {
        debugger = Pin::new(Box::new(Debugger::from_command_line(&command_line.unwrap(), context.clone(), persistent)));
        debugger.start_child()?;
    }
    defer! { unsafe { *DEBUGGER_TO_DROP_ON_PANIC.get() = None; } }

    let mut ui = UI::new()?;

    match PersistentState::load_state(&mut debugger, &mut ui) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("warning: restore failed: {}", e);
            log!(debugger.log, "restore failed: {}", e);
        }
    }

    let symbols_event_fd = debugger.symbols.event_fd();
    epoll.add(symbols_event_fd.fd, libc::EPOLLIN, symbols_event_fd.fd as u64)?;

    // Throttle rendering to <= fps. Render only when something changes.
    let mut pending_render = true;
    render_timer.set(1, 0);

    // The debugger.process_events() path of this loop should be kept light, it'll likely be the bottleneck for conditional breakpoints (including thread-specific breakpoints, including temporary breakpoints when stepping).
    loop {
        let mut events: [libc::epoll_event; 32] = unsafe { mem::zeroed() };
        let n = epoll.wait(&mut events)?;
        let mut render_now = false;
        let mut schedule_render = false;
        for event in &events[..n] {
            let fd = event.u64 as i32;
            let prof = TscScope::new();
            schedule_render = true;
            if fd == signal_pipes_read[libc::SIGCHLD as usize] {
                drain_signal_pipe(fd);
                let drop_caches = debugger.process_events()?;
                if drop_caches {
                    debugger.drop_caches()?;
                    ui.drop_caches();
                }
                debugger.log.prof.iteration_debugger(prof.finish());
            } else if fd == signal_pipes_read[libc::SIGWINCH as usize] {
                drain_signal_pipe(fd);
            } else if fd == STDIN_FILENO {
                ui.buffer_input()?;
                if !pending_render {
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
                debugger.log.prof.iteration_other(prof.finish());
            } else if fd == save_timer.fd {
                save_timer.read();
                PersistentState::try_to_save_state_if_changed(&mut debugger, &mut ui);
            } else {
                return err!(Internal, "epoll returned unexpected data: {}", fd);
            }
        }

        if render_now {
            assert!(!pending_render);
            let prof = TscScope::new();
            schedule_render = debugger.log.prof.iteration_render_start();

            let res = ui.update_and_render(&mut debugger)?;

            if res.quit {
                // Clean exit. Save state and call destructors. The only important destructor is TerminalRestorer.
                PersistentState::try_to_save_state_if_changed(&mut debugger, &mut ui);
                return Ok(());
            }

            schedule_render |= res.redraw || res.drop_caches;
            if res.drop_caches {
                debugger.drop_caches()?;
                ui.drop_caches();
            }
            if !schedule_render && res.loading {
                render_timer.set(loading_frame_ns, 0);
            }
            debugger.log.prof.iteration_render_end(prof.finish());
        }

        if schedule_render && !pending_render {
            pending_render = true;
            render_timer.set(frame_ns, 0);
        }
    }
}
