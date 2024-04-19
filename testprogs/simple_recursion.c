#include <stdio.h>

int __attribute__((noinline)) f(int n) {
    volatile int x = 0;
    x = x + 1;
    if (n > 0)
        x = x + f(n-1);
    x = x + 1;
    return x;
}

int main() {
    printf("%d", f(5));
}
