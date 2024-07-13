use std::{ffi::{OsStr, OsString}, path::{Path, PathBuf}};

fn main() {
    let a: &'static str = "asd";
    let b: String = "qwe".to_string();
    let c = OsStr::new("zxc");
    let d = OsString::from("123");
    let e = Path::new("rty");
    let f = PathBuf::from("fgh");
    let g = [10usize, 20, 30];
    let h = &g[2..];
    let s = a.len() + b.len() + c.len() + d.len() + e.as_os_str().len() + f.as_os_str().len() + g.len() + h.len();
    std::hint::black_box(s);
}
