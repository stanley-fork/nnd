#include "p.h"
#include <stdio.h>

int main() {
    struct Pair p;
    fa(&p);
    fb(&p);
    printf("%lu %lu\n", (unsigned long)p.a, (unsigned long)p.b);
}
