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
    std::env::set_var("RUST_BACKTRACE", "full"); // (short stack traces don't work with custom panic handlers for some reason - the __rust_begin_short_backtrace/__rust_end_short_backtrace are missing)

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
        } else if let Some(_) = parse_arg(&mut args, "--help", "-h", true) {
            println!(r"Hi, I'm a debugger.

Please (pretty please!) report all bugs, usability issues, slowness, first impressions, improvement ideas, feature requests, etc
to #debugger in slack (if you're not in clickhouse, and this debugger reached you somehow, send reports to mk.al13n@gmail.com).

Usage:
{0} command [args...]   - run a program under the debugger (just prepend {0} to the command line)
sudo {0} -p pid   - attach to an existing process

You may need to `cd` to the directory with the source code in order for the debugger to find the source code.
(Specifically, this is needed if (a) the debug info don't contain absolute paths, or (b) the source code is at a different absolute path than when the program was built; e.g. it was built on some CI server.)

Additional arguments (not available with -p):
--stdin/--stdout/--stderr path   - redirect stdin/stdout/stderr to file
--tty path   - equivalent to --stdin path --stdout path, see --help-tty
-c   - don't pause on startup, continue the program immediately (similar to pressing 'c' right after startup)
--help   - show this help message; see below for more help pages

Documentation chapters:
--help-overview - general information and first steps, start here
--help-known-problems - list of known bugs and missing features to look out for
--help-watches - watch expression language documentation
--help-state - files in ~/.nnd/ - log file, default stdout/stderr redirects, saved state, customizing colors and key bindings, etc
--help-tty - how to debug interactive programs that require a terminal (e.g. using this debugger to debug itself)
--help-features - list of features (not very readable)",
                     all_args[0]);
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-overview", "", true) {
            println!(r"nnd is a debugger that has a TUI and is meant to be fast and enjoyable to use, and work well on large executables.
('nnd' stands for 'no-nonsense debugger', but it doesn't quite live up to this name at the moment)

Limitations:
 * Linux only
 * x86 only
 * 64-bit only
 * TUI only (no REPL, no GUI)
 * no remote debugging (but works fine over ssh)
 * single process (doesn't follow forks)
 * no rr or other 'big agenda' features

Properties:
 * Not based on gdb or lldb, implemented mostly from scratch.
   (Uses libraries for a few well-isolated things like disassembling, DWARF parsing, and demangling.)
   (Many tricky things are handcrafted, e.g.: stepping (and dealing with inlined functions), breakpoints (and switching between hw/sw breakpoints as needed to make things work),
    debug info handling (data structures, parallel loading), type system (parallel loading and deduplication), watch expression interpreter, specialized UI elements.)
 * Fast.
   Operations that can be instantaneous should be instantaneous. I.e. snappy UI, no random freezes, no long waits.
   (Current known exceptions: (1) function search is temporarily slow and blocking; (2) if the program has >~2k threads things become pretty slow, can be improved; (3) running over ssh unavoidably adds latency.)
   Operations that can't be instantaneous (loading debug info, searching for functions and types) should be reasonably efficient, multi-threaded, asynchronous, cancellable, and have progress bars.
 * Works on large executables (tested mostly on 2.5 GB clickhouse).
 * Reasonably careful error reporting.

# Getting started

When running the debugger for the first time, notice:
 1. The UI consists of windows. There's an 'active' window, indicated with bright bold outline.
    The active window can be selected using digit keys (window numbers are shown in their titles) or using ctrl-wasd.
 2. The 'hints' window in top left lists (almost) all available key combinations.
    Some of them are global, others depend on the active window.
    There's currently no mouse support.

This information should be enough to discover most features by trial and error, which is recommended. Additionally, reading --help-known-problems and --help-watches is recommended.

Tips and caveats:
 * The source code window shows the current line with a green arrow, and the current column with an underlined character.
 * The highlighted characters in the source code window are locations of statements and inlined function calls, as listed in the debug info.
   These are all the places where the control can stop, e.g. if you step repeatedly.
 * 'Step over column' ('m' key by default) runs until the control moves to a different line+column location. Similar to how 'step over line' runs until the control moves to a different line.
   Useful for stepping into a function call when its arguments are nontrivial expressions - you'd do step-over-column to skip over evaluating the arguments,
   then step-into when the current column is at the function call you're interested in.
 * Steps are stack-frame-dependent: step-out steps out of the *currently selected* stack frame (as seen in the threads window on the right), not the innermost stack frame.
   Step-over steps over the calls inside the currently selected stack frame, i.e. it may involve an internal step-out.
   (This may seem like an unnecessary feature, but if you think through how stepping interacts with inlined functions, it's pretty much required, things get very confusing otherwise.)
 * While a step is in progress, breakpoints are automatically disabled for the duration of the step.
 * Step-into-instruction ('S' key by default) works no matter what, even if there's no debug info or if disassembly or stack unwinding fails. Use it when other steps fail.
 * Stepping can be interrupted with the suspend key ('C' by default). Useful e.g. if you try to step-over a function, but the function turns out to be too slow.
 * Breakpoints are preserved across debugger restarts, but they're put into disabled state on startup;
   'B' is the default key to re-enable a breakpoint (in the breakpoints window or the source code window).
 * The function search (in disassembly window, 'o' key by default) does fuzzy search over *mangled* function names, for now (for peformance reasons).
   Omit '::' in the search query. E.g. to search for 'std::foo::bar(int)' try typing 'stdfoobar'.
   The search results display demangled names, i.e. slightly different from what's actually searched.
 * Expect debugger's memory usage around 3x the size of the executable. E.g. ~7 GB for 2.3 GB clickhouse, release build. This is mostly debug information.
   (If you're curious, see ~/.nnd/<number>/log for a breakdown of which parts of the debug info take how much memory and take how long to load.)
 * For clickhouse server, use CLICKHOUSE_WATCHDOG_ENABLE=0. Otherwise it forks on startup, and the debugger doesn't follow forks.

Please (pretty please!) report all bugs, usability issues, slowness, first impressions, improvement ideas, feature requests, etc
to #debugger in slack (if you're not in clickhouse, and this debugger reached you somehow, send reports to mk.al13n@gmail.com).");
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-known-problems", "", true) {
            println!(r"Current limitations:
 * Resizing and rearranging windows is not implemented. You need a reasonably big screen to fit all the UI without cutting off any table columns, sorry.
 * Navigation in the source code window is lacking. There's no search and no go-to-line. You pretty much have to alt-tab into a real text editor,
   find the line you're looking for, alt-tab to the debugger, and scroll to that line using PgUp/PgDown.
   There's also no way to set a breakpoint without navigating to the line in the source code window.
 * Thread filter ('/' in the threads window) is too limited: just a substring match in function name and file name. Need to extend it enough to be able to e.g. filter out idle threads waiting for work or epoll.
 * Stepping is not aware of exceptions. E.g. if you step-over a function call, and the function throws an exception, the step won't notice that the control left the function;
   the program will keep running (the status window will say 'stepping') until you manually suspend it (shift-c by default). Similar for step-out.
 * Almost no pretty-printers.
 * No global variables.
 * In watch expressions, type names (for casts or type info) have to be spelled *exactly* the same way as they appear in the debug info.
   E.g. `std::vector<int>` doesn't work, you have to write `std::__1::vector<int, std::__1::allocator<int> >` (whitespace matters).
   The plan is to add a fuzzy search dialog for type names, similar to file and function search.
   (There is no plan to actually parse the template type names into their component parts; doing it correctly would be crazy complicated like everything else in C++.)
 * Can't assign to the debugged program's variables or registers
 * Can't add breakpoints in the disassembly window.
 * No data breakpoints, whole-file breakpoints, conditional breakpoints, special breakpoints (signals, exceptions/panics, main()).
 * Inside libraries that were dlopen()ed at runtime, breakpoints get disabled on program restart. Manually disable-enable the breakpoint after the dlopen() to reactivate it.
 * The disassembly window can only open 'functions' that appear in .symtab or debug info. Can't disassemble arbitrary memory, e.g. JIT-generated code or code from binaries without .symtab or debug info.
 * The 'locations' window is too cryptic and often cuts off most of the information.
 * Rust unions are not supported well: they show discriminator and all variants as fields, with no indication of which discriminator values correspond to which variants. But it's mostly usable.
 * The debugger gets noticeably slow when the program has > 1K threads, and unusably slow with 20K threads. Part of it is inevitable syscalls
   (to start/stop all n threads we have to do n*const syscalls, then wait for n notifications - that takes a while), but there's a lot of room for improvement anyway
   (reduce the const, do the syscalls in parallel, avoid the remaining O(n^2) work on our side).
 * No customization of colors. Dark theme only.
 * No customization of key bindings.
 * The UI desperately needs line wrapping and/or horizontal scrolling in more places. Useful information gets cut off a lot with no way to see the whole string. In practice:
    - Long function or file names in the stack trace window don't fit.
      Workaround: select the stack frame and look at the top of the disassembly - it shows the function name and file name and has horizontal scrolling.
    - In the threads window, function name doesn't even begin to fit unless you have a big monitor. Needs horizontal scrolling.
    - Watch expressions are usually way too long to fit on one line in the narrow table column.
    - Error messages in the locals/watches window often don't fit.
    - String values in the locals/watches window often don't fit. Workaround: use watches to split into shorter substrings (manually).
    - Type names in the locals/watches window often don't fit. Workaround: use `typeof(<expression>).type.name`, then apply the long string workaround.
 * More UI improvements needed:
    - Scroll bars.
    - Text editing: selection, copy-paste.
 * No remote debugging. The debugger works over ssh, but it's inconvenient for production servers: you have to scp the source code and the unstripped binary to it.
   And the debugger uses lots of RAM, which may be a problem on small servers.
   (I'm not sure what exactly to do about this. Fully separating the debugger-agent from UI+debuginfo would increase the code complexity a lot and make performance worse.
    Maybe I'll instead run the ~whole debugger on the server and have a thin client that just streams the rendered 'image' (text) from the server and sends the source code files on demand.
    This removes the need to scp the source code to the server, but leaves all the other problems.)");
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-watches", "", true) {
            println!(r"In the watches window, you can enter expressions to be evaluated. It uses a custom scripting language, documented here.

Currently the language has no loops or conditionals, just expressions. The syntax is C-like/Rust-like.

(Throughout this document, single quotes '' denote example watch expressions. The quotes themselves are not part of the expression.)

General info:
 * Can access the debugged program's local variables (as seen in the locals window) and registers: 'my_local_var', 'rdi'
 * Has basic C-like things you'd expect: arithmetic and logical operators, literals, array indexing, pointer arithmetic, '&x' to take address, '*x' to dereference.
 * Type cast: 'p as *u8'
 * Field access: 'my_struct.my_field'
 * There's no '->' operator, use '.' instead (it auto-dereferences pointers).
 * Fields can also be accessed by index: 'my_struct.3' (useful when field names are empty or duplicate).
 * 'p.[n]' to turn pointer 'p' to array of length 'n'. E.g. 'my_string.ptr.[my_string.len]'
 * 'begin..end' to turn a pair of pointers to the array [begin, end). E.g. 'my_vector.begin..my_vector.end'

Types:
 * Can access the types from the debugged program's debug info (structs, classes, enums, unions, primitive types, etc) and typedefs.
 * Common primitive types are also always available by the following names: void, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, char8, char32, bool.
 * '* MyType' (without quotes) is pointer to MyType.
 * '[100] MyType' is array of 100 MyType-s.

Inspecting types:
 * 'type(<name>)' to get information about a type, e.g. 'type(DB::Chunk)' - contains things like size and field offsets.
 * Use backticks to escape type names with templates or weird characters: 'type(`std::__1::atomic<int>`)'
 * Type name must be fully-qualified, and must exactly match the debug info.
   E.g. '`std::__1::atomic<int>`' works (on some version of libc++), but '`std::__1::atomic<int32_t>`', '`std::atomic<int>`', and '`atomic<int>`' don't.
 * 'typeof(<expression>)' to get information about result type of an expression.
   (Currently the expression is partially evaluated in some cases, e.g. 'typeof(*(0 as *mystruct))' works, but 'typeof((0 as *mystruct).myfield)' doesn't.)
 * Casting to typeof is not implementing, coming soon: 'x as typeof(y)'

Script variables:
 * 'x=foo.bar.baz' to assign to a new script variable 'x', which can then be used by later watch expressions.
 * Can't assign to the debugged program's variables. Script variables live in the debugger, outside the debugged program.
 * Watch expressions are evaluated in order from top to bottom.
   Watches lower down the list can use script variables assigned above.
 * Values are held by reference when possible. E.g. '&x' works after 'x=foo.bar', but not after 'x=1'.
 * No scopes, all script variables are global.
 * If debuggee has a variable with the same name, it can be accessed with backticks: '`x`', while the script variable can be accessed without backticks: 'x'

Pretty-printers:
Currently there are no pretty-printers for specific types (e.g. std::vector, std::map, std::shared_ptr, etc).
But there are a couple of general pretty-printers:
 * If a struct has exactly one nonempty field, we replace the struct with the value of that field.
   Useful for trivial wrappers (e.g. common in Rust).
   E.g. unwraps std::unique_ptr into a plain pointer, even though it actually has multiple levels of wrappers, inheritance, and empty fields.
 * Automatically downcasts abstract classes to concrete ones. Applies to any class/struct with a vtable pointer.
   Not very reliable, currently it often fails when the concrete type is template or is in anonymous namespace.
   (This can be improved to some extent, but can't be made fully reliable because vtable information is poorly structured in DWARF/symtab.)
 * Fields of base classes are inlined. Otherwise the base class is a field named '#base'.

Value modifiers:
 * 'value.#x' to print in hexadecimal.
 * 'value.#b' to print in binary.
 * 'value.#r' to disable pretty-printers.
   This applies both to how the value is printed and to field access:
   'foo.bar' will access field 'bar' of the transformed 'foo', i.e. after unwrapping single-field structs, downcasting to concrete type, and inlining base class fields.
   'foo.#r.bar' will access field 'bar' of 'foo' verbatim.
 * Modifiers propagate to descendants. E.g. doing 'my_struct.#x' will print all struct's fields as hexadecimal.
 * 'value.#p' is the opposite of '.#r'. Can be useful with field access: 'my_struct.#r.my_field.#p' re-enables pretty-printing after disabling it to access a raw field.");
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-state", "", true) {
            println!(r"The debugger creates directory ~/.nnd/ and stores a few things there, such as log file and saved state (watches, breakpoints, open tabs).
It doesn't create any other files or make any other changes to your system.

Each nnd process uses a subdirectory of ~/.nnd/ . The only one nnd is started, it'll use ~/.nnd/0/ . If a second nnd is started while the first is still running, it'll get ~/.nnd/1/ , etc.
After an nnd process ends, the directory can be reused.
E.g. if you start a few instances of nnd, then quit them all, then start them again in the same order, they'll use the same directories in the same order.

When using `sudo nnd -p`, keep in mind that the ~/.nnd` will be in the home directory of the root user, not the current user.

Files inside ~/.nnd/<number>/:
 * stdout, stderr - redirected stdout and stderr of the debugged program, unless overridden with --stdout/--stderr/--tty.
   (stdin is redirected to /dev/null by default.)
 * state - saved lists of watches, breakpoints, open files, open functions.
 * log - some messages from the debugger itself. Sometimes useful for debugging the debugger. Sometimes there are useful stats about debug info.
   On crash, error message and stack trace goes to this file. Please include this file when reporting bugs, especially crashes.
 * lock - prevents multiple nnd processes from using the same directory simultaneously.");
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-tty", "", true) {
            println!(r"The debugger occupies the whole terminal with its TUI. How to debug a program that also wants to use the terminal in an interactive way?
E.g. using nnd to debug itself.

One way is to just attach using -p

But what if you need to set breakpoints before the program starts, e.g. to debug a crash on startup? Then you can do the following:
 1. Open a terminal window (in xterm or tmux or whatever). Let's call this window A. This is where you'll be able interact with the debuggee.
 2. This terminal window is attached to some 'pty' pseudo-device in /dev/pts/ . Figure out which one:
     $ ls -l /proc/$$/fd | grep /dev/pts/
    For example, suppose it output /dev/pts/2 . You can also double-check this with: `echo henlo > /dev/pts/2` - if the text appears in the correct terminal then it's the correct path.
 3. Pacify the shell in this terminal window:
     $ sleep 1000000000
 4. In another terminal window (B) run the debugger:
     $ nnd --tty /dev/pts/2 the_program_to_debug
 5. Now you have the debugger running in window B while the debuggee inhabits the terminal in window A
    (which will come to life when you resume the debuggee in the debugger, `sleep` notwithstanding).

The latter approach is often more convenient than -p, even when both approaches are viable.

(This can even be chained multiple levels deep: `nnd --tty /dev/pts/1 nnd --tty /dev/pts/2 my_program`. The longest chain I used in practice is 3 nnd-s + 1 clickhouse.");
            process::exit(0);
        } else if let Some(_) = parse_arg(&mut args, "--help-features", "", true) {
            println!(r"Appendix: raw list of features (optional reading)

loading debug info
  progress bar in the binaries window (top right)
  multithreaded, reasonably fast
  debugger is functional while debug info is loading, but there won't be line numbers or function names, etc
  after debug info is loaded, everything is automatically refreshed, so things like function names and source code appear
  supports DWARF 4 and 5, debuglink, compressed ELF sections
threads window (bottom right)
  color tells the stop reason
  search
    not fuzzy
    matches function name and file name
stack trace window (right)
  when switching frame, jumps to the location in disassembly and source code
source code window (bottom)
  makes some attempts to guess the correct path, if it doesn't match between the debug info and the local filesystem
  file search
    fuzzy
    only sees files mentioned in the debug info
  shows statements and inlined calls
  scrolls disassembly window when moving cursor
    can cycle through locations if multiple
    prioritizes locations close to disassembly window cursor
  breakpoints
  shows URL for rust standard library files
stepping
  into-instruction, into-line, over-instruction, over-line, over-column (useful for function arguments), out of inlined function, out of real function
  aware of inlined functions
  frame-dependent (e.g. step-out steps out of the selected stack frame, not the top stack frame; similar for step-over)
    disables breakpoints for the duration of the step
disassembly window (top)
  shows inlined functions
  scrolls source window when moving cursor
    can change inline depth
  function search
    fuzzy
    mangled :(
    shows file:line
  shows breakpoint locations (for breakpoints added in the source code window)
watches, locals, registers (bottom left)
  tree
  automatic downcasting of virtual classes to the most concrete type
  expression language (see --help-watches)
breakpoints (top right, second tab)
  aware of inlined functions
  can be disabled
  jump to location when selecting in the breakpoints window
  shows adjusted line
autosaving/restoring state
  see --help-state
obscure feature: locations window
  tells where each variable lives (register, memory location, expression, etc) (actually it just prints the DWARF expression)
  for selected disassembly line
secret feature: C-p for self-profiler
secret feature: C-l to drop caches and redraw
removing breakpoints on exit
  if the debugger is attached with -p, and some breakpoints are active, it's an important job of the debugger to deactivate all breakpoints when detaching
  otherwise the debuggee will get SIGTRAP and crash as soon as it hits one of the leftover breakpoints
  nnd correctly removes breakpoints when exiting normally, or exiting with an error, or exiting with a panic (aka exception)
  but it doesn't remove breakpoints if the debugger receives a fatal signal (e.g. SIGSEGV or SIGKILL)
");
            process::exit(0);
        } else {
            eprintln!("unrecognized argument: '{}' (if it's the command to run, prepend '\\' to escape)", args[0]);
            process::exit(1);
        }
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
