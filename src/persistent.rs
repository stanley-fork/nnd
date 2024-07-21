use crate::{*, error::*, debugger::*, ui::*, util::*, settings::*, log::*};
use std::{fs, os::fd::{OwnedFd, RawFd, AsRawFd, FromRawFd}, os::unix::ffi::OsStrExt, ffi::CString, io, io::{Read, Write}, path::{Path, PathBuf}, collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

pub struct PersistentState {
    pub path: Result<PathBuf>,
    pub configs_path: Option<PathBuf>,
    pub log_file_path: Option<PathBuf>,

    pub config_change_fd: Option<INotifyFD>,
    pub original_stderr_fd: Option<RawFd>,

    dir: Option<DirFd>,
    lock: Option<fs::File>,

    state_hash: u64,
    save_failures: usize,
    keys_config_reload_count: usize,
}
impl Default for PersistentState { fn default() -> Self { Self {path: err!(Internal, "state is empty"), configs_path: None, config_change_fd: None, dir: None, lock: None, state_hash: 0, save_failures: 0, log_file_path: None, original_stderr_fd: None, keys_config_reload_count: 0} } }
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
                    return Ok(Self {path: Ok(path), configs_path: Some(parent_path), dir: Some(dir), lock: Some(lock), log_file_path, original_stderr_fd, ..Default::default()});
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
        Self {path: Err(err), original_stderr_fd, ..Default::default()}
    }

    pub fn try_to_save_state_if_changed(debugger: &mut Debugger, ui: &mut DebuggerUI) {
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

    fn save_state_if_changed(debugger: &mut Debugger, ui: &mut DebuggerUI) -> Result<()> {
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

        dir.rename_file(Path::new("state.tmp"), Path::new("state"), false)?;

        self_.state_hash = hash;
        Ok(())
    }

    pub fn load_state_and_configs(debugger: &mut Debugger, ui: &mut DebuggerUI) -> Option</*config_change_fd*/ i32> {
        if debugger.persistent.dir.is_none() {
            return None;
        }
        if let Err(e) = Self::write_default_configs(debugger.persistent.dir.as_ref().unwrap(), &debugger.persistent.configs_path.as_ref().unwrap()) {
            eprintln!("warning: failed to write default configs: {}", e);
        }
        if let Some(configs_path) = &debugger.persistent.configs_path {
            match Self::subscribe_to_config_changes(configs_path) {
                Ok(x) => debugger.persistent.config_change_fd = Some(x),
                Err(e) => {
                    eprintln!("warning: failed to subscribe to config changes: {}", e);
                    log!(debugger.log, "subscribe failed: {}", e);
                }
            }
            if let Some(binds) = Self::read_keys_config(debugger, ui) {
                ui.ui.key_binds = binds;
            }
        }
        match Self::load_state(debugger, ui) {
            Ok(()) => (),
            Err(e) => {
                eprintln!("warning: restore failed: {}", e);
                log!(debugger.log, "restore failed: {}", e);
            }
        }

        debugger.persistent.config_change_fd.as_ref().map(|f| f.fd)
    }

    pub fn process_events(debugger: &mut Debugger, ui: &mut DebuggerUI) {
        if let Some(fd) = &debugger.persistent.config_change_fd {
            let (mut keys_changed, mut colors_changed) = (false, false);
            for (ev, name) in fd.read() {
                keys_changed |= &name == b"keys";
                colors_changed |= &name == b"colors";
            }
            if keys_changed {
                if let Some(binds) = Self::read_keys_config(debugger, ui) {
                    if binds != ui.ui.key_binds {
                        ui.ui.key_binds = binds;
                        debugger.persistent.keys_config_reload_count += 1;
                        log!(debugger.log, "reloaded keys config ({})", debugger.persistent.keys_config_reload_count);
                    }
                }
            }
            /* uncomment when we actually read colors config here
            if colors_changed {
                ui.drop_caches(); // there are colors in cached disassembly and code
            }*/
        }
    }

    fn subscribe_to_config_changes(configs_path: &Path) -> Result<INotifyFD> {
        let fd = INotifyFD::new()?;
        fd.add_watch(configs_path, libc::IN_CREATE | libc::IN_DELETE | libc::IN_MODIFY | libc::IN_MOVED_FROM | libc::IN_MOVED_TO)?;
        Ok(fd)
    }

    fn load_state(debugger: &mut Debugger, ui: &mut DebuggerUI) -> Result<()> {
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
            return err!(Environment, "bad magic number");
        }

        debugger.load_state(&mut inp)?;
        ui.load_state(&mut inp)?;

        if !inp.is_empty() {
            eprintln!("warning: unexpected {} bytes at end of save file", inp.len());
        }

        debugger.persistent.state_hash = hash;
        Ok(())
    }

    fn write_default_configs(dir: &DirFd, configs_path: &Path) -> Result<()> {
        if cfg!(debug_assertions) {
            // Assert that formatting+parsing round trips.
            let mut temp_str = String::new();
            KeyBinds::write_config_example(&mut temp_str, Path::new(""), Path::new(""), Path::new(""), false).unwrap();
            let mut error_line_number = 0usize;
            let binds = KeyBinds::parse_config(&temp_str, &mut error_line_number).unwrap();
            let default = KeyBinds::default();
            if binds != default {
                for (order, &(first, second)) in [(&binds, &default), (&default, &binds)].iter().enumerate() {
                    for (is_text_input, &(map1, map2)) in [(&first.normal, &second.normal), (&first.text_input, &second.text_input)].iter().enumerate() {
                        for (&key, &action) in &map1.key_to_action {
                            match map2.key_to_action.get(&key) {
                                None => eprintln!("order: {}, is_text_input: {}, key_to_action missing {} {:?}", order, is_text_input, key, action),
                                Some(&a) if a != action => eprintln!("order: {}, is_text_input: {}, key_to_action mismatch {} {:?} {:?}", order, is_text_input, key, action, a),
                                _ => (),
                            }
                        }
                        for (&action, keys) in &map1.action_to_keys {
                            match map2.action_to_keys.get(&action) {
                                None => eprintln!("order: {}, is_text_input: {}, action_to_keys missing {:?} {:?}", order, is_text_input, action, keys),
                                Some(ks) if ks != keys => eprintln!("order: {}, is_text_input: {}, action_to_keys mismatch {:?} {:?} {:?}", order, is_text_input, action, keys, ks),
                                _ => (),
                            }
                        }
                    }
                }
                panic!("default keys config doesn't round trip");
            }
        }

        let mut keys_str = String::new();
        let keys_default = Path::new("keys.default");
        let keys_default_path = configs_path.join(keys_default);
        KeyBinds::write_config_example(&mut keys_str, &configs_path.join("keys"), &keys_default_path, &configs_path.join("keys.error"), true)?;

        Self::write_shared_file(keys_str.as_bytes(), keys_default, configs_path, dir)?;

        Ok(())
    }

    fn write_shared_file(contents: &[u8], file_name: &Path, shared_dir: &Path, my_dir: &DirFd) -> Result<()> {
        let mut temp_file = my_dir.open_or_create_file(file_name)?;
        temp_file.write_all(contents)?;
        // Move from ~/.nnd/0/x to ~/.nnd/x
        my_dir.rename_file(file_name, &shared_dir.join(file_name), true)?;
        Ok(())
    }

    fn read_keys_config(debugger: &mut Debugger, ui: &mut DebuggerUI) -> Option<KeyBinds> {
        let configs_path = debugger.persistent.configs_path.as_ref().unwrap();
        let keys_path = configs_path.join("keys");
        let text = match fs::read_to_string(&keys_path) {
            Ok(x) => x,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Some(KeyBinds::default()),
            Err(e) => {
                eprintln!("warning: failed to read keys config file at {}: {}", keys_path.display(), e);
                log!(debugger.log, "keys config read error: {}", e);
                return None;
            }
        };
        let error_file_name = Path::new("keys.error");
        let mut error_line_number = 0usize;
        match KeyBinds::parse_config(&text, &mut error_line_number) {
            Ok(c) => {
                let _ = fs::remove_file(&debugger.persistent.configs_path.as_ref().unwrap().join(error_file_name));
                Some(c)
            }
            Err(e) => {
                eprintln!("warning: keys config syntax error on line {}: {}", error_line_number, e);
                log!(debugger.log, "keys config error on line {}: {}", error_line_number, e);
                let _ = Self::write_shared_file(format!("line {}: {}\n", error_line_number, e).as_bytes(), error_file_name, configs_path, debugger.persistent.dir.as_ref().unwrap());
                None
            }
        }
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

    fn rename_file(&self, old_path: &Path, new_path: &Path, new_path_is_absolute: bool) -> Result<()> {
        let c_old_path = CString::new(old_path.as_os_str().as_bytes()).unwrap();
        let c_new_path = CString::new(new_path.as_os_str().as_bytes()).unwrap();
        let new_dirfd = if new_path_is_absolute {libc::AT_FDCWD} else {self.fd.as_raw_fd()};
        let r = unsafe {libc::renameat(self.fd.as_raw_fd(), c_old_path.as_bytes().as_ptr() as *const i8, new_dirfd, c_new_path.as_bytes().as_ptr() as *const i8)};
        if r == -1 {
            return errno_err!("failed to rename file {} to {}", old_path.display(), new_path.display());
        }
        Ok(())
    }
}
