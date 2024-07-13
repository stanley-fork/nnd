#include <stdio.h>

inline void f(int n) {
    printf("%d", n*2);
}

inline void g(int n) {
    f(n*10);
}

void h(int n) {
    g(n+1);
}

int main() {
    g(1);
}
