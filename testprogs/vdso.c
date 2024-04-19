#include <time.h>
#include <stdio.h>

int main() {
    struct timespec t;
    clock_gettime(0, &t);
    printf("%ld\n", t.tv_sec);
}
