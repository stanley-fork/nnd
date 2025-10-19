use std::env;
use std::hint::black_box;
use std::panic;

fn trigger_panic(method: usize) {
    match method {
        0 => panic!("explicit panic"),
        1 => {
            let a = black_box(u32::MAX);
            let b = black_box(1u32);
            let _c = a + b;
        }
        2 => {
            let arr = black_box([1, 2, 3]);
            let idx = black_box(10usize);
            let _x = arr[idx];
        }
        3 => {
            let x = black_box(false);
            assert!(x, "assertion failed");
        }
        4 => {
            let x = black_box(false);
            debug_assert!(x, "debug assertion failed");
        }
        5 => {
            let a = black_box(10i32);
            let b = black_box(0i32);
            let _c = a / b;
        }
        _ => {}
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let catch = args.get(1)
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1) == 1;

    if let Some(method_str) = args.get(2) {
        if let Ok(method) = method_str.parse::<usize>() {
            if catch {
                let _ = panic::catch_unwind(|| trigger_panic(method));
            } else {
                trigger_panic(method);
            }
        }
    } else {
        for i in 0..6 {
            if catch {
                let _ = panic::catch_unwind(|| trigger_panic(i));
            } else {
                trigger_panic(i);
            }
        }
    }
}
