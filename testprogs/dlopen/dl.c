#include <stdio.h>

int __attribute__((noinline)) g(int x) {
    return x * 10;
}

extern void f(int x) {
    x = g(x);
    printf("%d\n", x);
}
