long __attribute__((noinline)) g(long i) {
    return i + 1;
}

void __attribute__((noinline)) f() {
    volatile long i = 0;
    while (i < 100000000000l) {
        i = g(i);
    }
}

int main() {
    f();
}
