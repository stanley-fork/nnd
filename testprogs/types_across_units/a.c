#include "p.h"
#include <stdio.h>

struct A {
    int a;
};

struct A a;

void fa(struct Pair *p) {
    a.a = 42;
    p->a = &a;
    printf("fa %d\n", p->a->a);
}
