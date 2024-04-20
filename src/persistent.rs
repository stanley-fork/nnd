use crate::{*, error::*, debugger::*, ui::*, util::*};
use std::{fs, os::fd::{OwnedFd, RawFd, AsRawFd, FromRawFd}, os::unix::ffi::OsStrExt, ffi::CString, io, io::{Read, Write}, path::{Path, PathBuf}, collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

pub struct PersistentState {
    pub path: Result<PathBuf>,
    dir: Option<DirFd>,
    lock: Option<fs::File>,
    
    state_hash: u64,
    save_failures: usize,

    pub log_file_path: Option<PathBuf>,
    pub original_stderr_fd: Option<RawFd>,
}
impl PersistentState {
    // Finds/creates a directory ~/.nnd/0, and flock()s ~/.nnd/0/lock to prevent other debugger processes from using this directory.
    // If ~/.nnd/0 is already locked, tries ~/.nnd/1, etc. The lock is released when debugger exits or dies.
    // If anything fails, fall back to sending everything to /dev/null: debugger's log, debuggee's stdout and stderr.
    pub fn init() -> Self {
        let r = match Self::try_init() {
            Ok(x) => x,
            Err(e) => Self::fallback_init(e),
        };
        r
    }

    pub fn empty() -> Self {
        Self {path: err!(Internal, "state is empty"), dir: None, lock: None, state_hash: 0, save_failures: 0, log_file_path: None, original_stderr_fd: None}
    }

    pub fn open_or_create_file(&self, name: &str) -> fs::File {
        if let Some(d) = &self.dir {
            match d.open_or_create_file(Path::new(name)) {
                Ok(f) => return f,
                Err(e) => eprintln!("error: failed to open {}: {}", name, e),
            }
        }
        open_dev_null().unwrap()
    }

    fn try_init() -> Result<Self> {
        let home = std::env::var("HOME")?;
        let mut parent_path = PathBuf::from(home);
        parent_path.push(".nnd");
        let _ = DirFd::open_or_create(&parent_path)?;
        for i in 0..100 {
            let mut path = parent_path.clone();
            path.push(format!("{}", i));
            let dir = DirFd::open_or_create(&path)?;
            let lock = dir.open_or_create_file(Path::new("lock"))?;
            loop {
                let r = unsafe {libc::flock(lock.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB)};
                if r == 0 {
                    let log_file_name = "log";
                    let log = dir.open_or_create_file(Path::new(log_file_name))?;
                    let original_stderr_fd = redirect_stderr(&log)?;
                    let log_file_path = Some(path.join(log_file_name));
                    return Ok(Self {path: Ok(path), dir: Some(dir), lock: Some(lock), state_hash: 0, save_failures: 0, log_file_path, original_stderr_fd});
                }
                let e = io::Error::last_os_error();
                match e.kind() {
                    io::ErrorKind::WouldBlock => break,
                    io::ErrorKind::Interrupted => continue,
                    _ => return Err(e.into()),
                }
            }
        }
        err!(Sanity, "there appear to be >99 nnd processes running")
    }

    fn fallback_init(err: Error) -> Self {
        let original_stderr_fd = redirect_stderr(&open_dev_null().unwrap()).unwrap();
        Self {path: Err(err), dir: None, lock: None, state_hash: 0, save_failures: 0, log_file_path: None, original_stderr_fd}
    }

    pub fn try_to_save_state_if_changed(debugger: &mut Debugger, ui: &mut UI) {
        match Self::save_state_if_changed(debugger, ui) {
            Ok(()) => (),
            Err(e) => {
                if debugger.persistent.save_failures == 0 {
                    eprintln!("warning: save state failed: {}", e);
                    log!(debugger.log, "save state failed: {}", e);
                }
                debugger.persistent.save_failures += 1;
            }
        }
    }

    fn save_state_if_changed(debugger: &mut Debugger, ui: &mut UI) -> Result<()> {
        if debugger.persistent.dir.is_none() {
            return Ok(());
        }
        let mut buf: Vec<u8> = Vec::new();

        buf.write_usize(STATE_FILE_MAGIC_NUMBER)?;
        debugger.save_state(&mut buf)?;
        ui.save_state(&mut buf)?;

        let self_ = &mut debugger.persistent;

        let mut hasher = DefaultHasher::new();
        buf.hash(&mut hasher);
        let hash = hasher.finish();
        if hash == self_.state_hash {
            return Ok(());
        }

        let dir = self_.dir.as_ref().unwrap();
        {
            let mut temp_file = dir.open_or_create_file(Path::new("state.tmp"))?;
            temp_file.write_all(&buf)?;
            temp_file.sync_all()?;
        }

        dir.rename_file(Path::new("state.tmp"), Path::new("state"))?;

        self_.state_hash = hash;
        Ok(())
    }

    pub fn load_state(debugger: &mut Debugger, ui: &mut UI) -> Result<()> {
        if debugger.persistent.dir.is_none() {
            return Ok(());
        }
        let mut file = match debugger.persistent.dir.as_ref().unwrap().open_file_if_exists(Path::new("state"))? {
            Some(x) => x,
            None => return Ok(()),
        };

        let mut buf: Vec<u8> = Vec::new();
        let len = file.read_to_end(&mut buf)?;
        buf.truncate(len);

        let mut hasher = DefaultHasher::new();
        buf.hash(&mut hasher);
        let hash = hasher.finish();

        let mut inp: &[u8] = &buf;
        let magic = inp.read_usize()?;
        if magic != STATE_FILE_MAGIC_NUMBER {
            return err!(Environment, "bad magic number (spellcast failed)");
        }

        debugger.load_state(&mut inp)?;
        ui.load_state(&mut inp)?;

        if !inp.is_empty() {
            eprintln!("warning: unexpected {} bytes at end of save file", inp.len());
        }

        debugger.persistent.state_hash = hash;
        Ok(())
    }
}

const STATE_FILE_MAGIC_NUMBER: usize = 0xe4b84e6353eb8214;

pub fn open_dev_null() -> Result<fs::File> {
    let fd = unsafe {libc::open("/dev/null\0".as_ptr() as *const i8, libc::O_RDWR, libc::O_CLOEXEC)};
    if fd == -1 {
        return errno_err!("failed to open /dev/null");
    }
    Ok(unsafe {fs::File::from_raw_fd(fd)})
}

fn redirect_stderr(file: &fs::File) -> Result<Option<RawFd>> {
    let original_stderr_fd = match unsafe {libc::dup(2)} {
        -1 => None,
        x => Some(x),
    };
    let r = unsafe {libc::dup2(file.as_raw_fd(), 2)};
    if r < 0 {
        return errno_err!("failed to redirect stderr");
    }
    Ok(original_stderr_fd)
}

struct DirFd {
    fd: OwnedFd,
}
impl DirFd {
    fn open_or_create(path: &Path) -> Result<Self> {
        let c_path = CString::new(path.as_os_str().as_bytes()).unwrap();
        let r = unsafe {libc::mkdir(c_path.as_ptr() as *const i8, 0o777)};
        if r == -1 && io::Error::last_os_error().kind() != io::ErrorKind::AlreadyExists {
            return errno_err!("failed to create {}", path.display());
        }
        let fd = unsafe {libc::open(c_path.as_ptr() as *const i8, libc::O_DIRECTORY | libc::O_CLOEXEC | libc::O_RDONLY)};
        if fd == -1 {
            return errno_err!("failed to open dir {}", path.display());
        }
        Ok(DirFd {fd: unsafe {OwnedFd::from_raw_fd(fd)}})
    }

    fn open_or_create_file(&self, relative_path: &Path) -> Result<fs::File> {
        let c_path = CString::new(relative_path.as_os_str().as_bytes()).unwrap();
        let fd = unsafe {libc::openat(self.fd.as_raw_fd(), c_path.as_bytes().as_ptr() as *const i8, libc::O_CREAT | libc::O_CLOEXEC | libc::O_RDWR | libc::O_TRUNC, 0o666)};
        if fd == -1 {
            return errno_err!("failed to open file {}", relative_path.display());
        }
        Ok(unsafe {fs::File::from_raw_fd(fd)})
    }

    fn open_file_if_exists(&self, relative_path: &Path) -> Result<Option<fs::File>> {
        let c_path = CString::new(relative_path.as_os_str().as_bytes()).unwrap();
        let fd = unsafe {libc::openat(self.fd.as_raw_fd(), c_path.as_bytes().as_ptr() as *const i8, libc::O_CLOEXEC | libc::O_RDWR, 0)};
        match fd {
            -1 if io::Error::last_os_error().kind() == io::ErrorKind::NotFound => Ok(None),
            -1 => errno_err!("failed to open file {}", relative_path.display()),
            fd => Ok(Some(unsafe {fs::File::from_raw_fd(fd)})),
        }
    }

    fn rename_file(&self, old_path: &Path, new_path: &Path) -> Result<()> {
        let c_old_path = CString::new(old_path.as_os_str().as_bytes()).unwrap();
        let c_new_path = CString::new(new_path.as_os_str().as_bytes()).unwrap();
        let r = unsafe {libc::renameat(self.fd.as_raw_fd(), c_old_path.as_bytes().as_ptr() as *const i8, self.fd.as_raw_fd(), c_new_path.as_bytes().as_ptr() as *const i8)};
        if r == -1 {
            return errno_err!("failed to rename file {} to {}", old_path.display(), new_path.display());
        }
        Ok(())
    }
}
