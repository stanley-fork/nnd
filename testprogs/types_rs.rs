#![allow(dead_code)]
use std::hint::black_box;

enum E {
    X,
    Y(usize),
    Z {a: String, b: usize},
    W,
    A(usize, usize),
}

enum F {
    X,
    Y,
    Z,
    W,
}

fn main() {
    let x = E::X;
    let y = E::Y(42);
    let z = E::Z {a: "hi".to_string(), b: 13};
    let w = E::W;
    let a = E::A(10, 20);
    black_box((x, y, z, w, a));

    let mut a = [E::X, E::W, E::Y(1), E::Y(2), E::Z {a: "meow".to_string(), b: 3}];
    let s = &a[1..3];
    black_box(s);
    let s = &mut a[1..3];
    black_box(s);
    black_box(a);

    let t = (E::X, 1337, "asd", E::Y(1));
    black_box(t);

    let fs = [F::X, F::Y, F::Z, F::W];
    black_box(fs);
}
