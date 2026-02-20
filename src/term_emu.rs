use crate::{*, error::*, terminal::*, util::*};
use std::{collections::VecDeque, ffi::OsStr};

pub struct Pty {
    pub master_fd: i32,
    pub slave_fd: i32,
    pub pty_name: PathBuf,

    pub size: [u16; 2],

    pub in_buf: Vec<u8>,
    // What to write to pty. Typically keyboard/mouse input. Should be encoded in a correct way corresponding to the terminal modes.
    // User of Pty can add bytes here directly, then call flush_out_buf_if_needed() later.
    pub out_buf: VecDeque<u8>,
    epollout_enabled: bool,
}
impl Pty {
    pub fn new(size: [u16; 2]) -> Result<Self> {
        let mut pty = Self {master_fd: -1, slave_fd: -1, pty_name: PathBuf::new(), size: size.clone(), in_buf: Vec::new(), out_buf: VecDeque::new(), epollout_enabled: false};
        let mut name_buf = [0u8; 2048];
        let winsize = libc::winsize {ws_row: size[1], ws_col: size[0], ws_xpixel: 0, ws_ypixel: 0};
        let rv = unsafe {libc::openpty(&raw mut pty.master_fd, &raw mut pty.slave_fd, name_buf.as_mut_ptr() as *mut i8, ptr::null(), &raw const winsize)};
        if rv != 0 {
            return errno_err!("openpty() failed");
        }
        assert!(pty.master_fd != -1 && pty.slave_fd != -1);
        let rv = unsafe {libc::fcntl(pty.master_fd, libc::F_SETFL, libc::O_NONBLOCK)};
        if rv != 0 {
            return errno_err!("fcntl(pty, F_SETFL, O_NONBLOCK) failed");
        }
        let name_len = name_buf.iter().copied().position(|c| c == b'\0').unwrap_or(name_buf.len());
        pty.pty_name = PathBuf::from(unsafe {OsStr::from_encoded_bytes_unchecked(&name_buf[..name_len])});
        Ok(pty)
    }

    pub fn add_to_epoll(&self, epoll: &Epoll) -> Result<()> {
        epoll.add(self.master_fd, libc::EPOLLIN, self.master_fd as u64)
    }

    // Call this when epoll says master_fd is readable or writable.
    pub fn do_io(&mut self, epoll: &Epoll) -> Result<()> {
        self.try_flush_out_buf(epoll)?;

        loop {
            let lim: usize = 4096;
            self.in_buf.reserve(lim);
            let r = unsafe {libc::read(self.master_fd, self.in_buf.as_mut_ptr().add(self.in_buf.len()) as *mut libc::c_void, lim)};
            if r < 0 {
                match io::Error::last_os_error().kind() {
                    io::ErrorKind::Interrupted => continue,
                    io::ErrorKind::WouldBlock => break,
                    _ => return errno_err!("read from child tty failed"),
                }
            }
            let n = r as usize;
            assert!(n <= lim);
            unsafe {self.in_buf.set_len(self.in_buf.len() + n)};
        }
        Ok(())
    }

    // Call this after appending to out_buf.
    pub fn flush_out_buf_if_needed(&mut self, epoll: &Epoll) -> Result<()> {
        if !self.out_buf.is_empty() && !self.epollout_enabled {
            self.try_flush_out_buf(epoll)?;
        }
        Ok(())
    }

    pub fn resize(&mut self, size: [u16; 2]) -> Result<()> {
        self.size = size;
        let winsize = libc::winsize {ws_row: size[1], ws_col: size[0], ws_xpixel: 0, ws_ypixel: 0};
        let r = unsafe {libc::ioctl(self.master_fd, libc::TIOCSWINSZ, &raw const winsize)};
        if r == 0 {
            Ok(())
        } else {
            errno_err!("ioctl(TIOCSWINSZ) failed")
        }
    }

    fn try_flush_out_buf(&mut self, epoll: &Epoll) -> Result<()> {
        while !self.out_buf.is_empty() {
            let (front, back) = self.out_buf.as_slices();
            let data = if front.is_empty() {back} else {front};
            assert!(!data.is_empty());
            let r = unsafe {libc::write(self.master_fd, data.as_ptr() as *const libc::c_void, data.len())};
            if r < 0 {
                match io::Error::last_os_error().kind() {
                    io::ErrorKind::Interrupted => continue,
                    io::ErrorKind::WouldBlock => break,
                    _ => return errno_err!("write to child tty failed"),
                }
            }
            let n = r as usize;
            assert!(n <= data.len());
            self.out_buf.drain(..n);
        }

        let want_epollout = !self.out_buf.is_empty();
        if want_epollout != self.epollout_enabled {
            let mut events = libc::EPOLLIN;
            if want_epollout {
                events |= libc::EPOLLOUT;
            }
            epoll.modify(self.master_fd, events, self.master_fd as u64)?;
            self.epollout_enabled = want_epollout;
        }
        Ok(())
    }
}
impl Drop for Pty {
    fn drop(&mut self) {
        if self.slave_fd != -1 {
            unsafe {
                let r = libc::close(self.slave_fd);
                if r != 0 { eprintln!("warning: close() failed on pty slave fd: {:?}", io::Error::last_os_error()); }
            }
        }
        if self.master_fd != -1 {
            unsafe {
                let r = libc::close(self.master_fd);
                if r != 0 { eprintln!("warning: close() failed on pty master fd: {:?}", io::Error::last_os_error()); }
            }
        }
    }
}

pub struct TerminalEmulator {
    // Using a crate. It may make sense to reimplement it instead. Probably still use `vte` crate for parsing the escape sequences, but execute them ourselves, storing contents in ScreenBuffer-s.
    // vt100 has minor issues that we could fix:
    //  * No unicode extended grapheme cluster support, things like '🤦🏼‍♂️' mess up cursor position and screen diffs for the rest of the line. But terminal and application support for it is all over the place anyway; as I'm typing this in emacs, the cursor is one cell off from where it should be.
    //  * No text reflow on window resize.
    //  * Doesn't implement DECRQCRA, so we can't run esctest on it.
    //  * There doesn't seem to be a good way to reset the screen and parser state (on debuggee restart) without losing scrollback and screen contents.
    //  * Unnecessary data conversion between vt100's Screen and our ScreenBuffer.
    pub vt: vt100::Parser,
    pub size: [u16; 2], // [width, height]
    pub scrollback_limit: usize,
}
impl Default for TerminalEmulator { fn default() -> Self { Self {vt: vt100::Parser::new(1, 1, 0), size: [0, 0], scrollback_limit: 0} } }
impl TerminalEmulator {
    pub fn program_restarted(&mut self) {
        // We want to reset parser state and modes/colors/cursor/etc while preserving screen and scrollback contents.
        // vt100 library doesn't seem to have a reasonable way to do it, so we do weird things here that partially rely on vt100 internals and may partially break in future.

        // Create a new Parser to reset the parsing state (useful if the program crashed in the middle of outputting an ANSI escape sequence).
        let mut new_vt = vt100::Parser::new(self.size[1], self.size[0], self.scrollback_limit);

        // Move the screen contents to the new Parser.
        let dummy_vt = vt100::Parser::new(1, 1, 0);
        *new_vt.screen_mut() = mem::replace(self.vt.screen_mut(), dummy_vt.screen().clone());

        // Reset the Screen's state without changing contents.
        // We have to do it by sending ANSI commands because Screen has no public methods to alter state directly.
        // From looking at vt100 source, as of version vt100-0.16.2, the commands below clear all needed fields of Screen except `saved_attrs`.
        // Some fields of Grid would in principle make sense to reset, but it's not important, so we leave them: saved_pos, scroll_top/scroll_bottom (that's ansi stuff, not our ui scrolling), saved_origin_mode.
        let screen = new_vt.screen_mut();
        let blank_screen = dummy_vt.screen();
        new_vt.process(b"\x1b[?47l\x1b[?6l"); // exit alternate screen, clear origin_mode
        new_vt.process(&blank_screen.attributes_formatted());
        new_vt.process(&blank_screen.cursor_state_formatted());
        new_vt.process(&blank_screen.input_mode_formatted());
        new_vt.screen_mut().set_scrollback(0);

        new_vt.process(b"\x1B[10000;0H\n"); // move cursor to the bottom of the screen (vt100 clamps the coordinates), then add a new row below that
        new_vt.process("┄".repeat(self.size[0] as usize).as_bytes()); // draw a horizontal line
        new_vt.process(b"\n\r"); // go to next line

        self.vt = new_vt;
    }

    pub fn set_size_and_scrollback(&mut self, size: [u16; 2], scrollback_limit: usize) {
        if scrollback_limit != self.scrollback_limit {
            self.size = size;
            self.scrollback_limit = scrollback_limit;
            self.vt = vt100::Parser::new(size[1], size[0], scrollback_limit);
        }
        if size != self.size {
            self.size = size;
            self.vt.screen_mut().set_size(size[1], size[0]);
        }
    }

    pub fn process_input(&mut self, in_buf: &mut Vec<u8>) {
        self.vt.process(in_buf);
        in_buf.clear();
    }

    // How many lines are actually in scrollback buffer.
    // (vt100 weirdly doesn't expose this information, so we query it in a roundabout way.)
    pub fn get_scrollback_len(&mut self) -> usize {
        let screen = self.vt.screen_mut();
        let prev_offset = screen.scrollback();
        screen.set_scrollback(usize::MAX); // truncated to the actuall scrollback buffer len
        let res = screen.scrollback();
        screen.set_scrollback(prev_offset);
        res
    }
}
