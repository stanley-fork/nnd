#[inline(never)]
fn f() {
    panic!("aaaaaa");
}

#[inline(never)]
fn g() {
    f();
}

fn main() {
    if std::panic::catch_unwind(|| g()).is_err() {
        println!("panicked");
    }
    g();
}
