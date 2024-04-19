#include "p.h"
#include <stdio.h>
#include <stdlib.h>

struct B {
    double b;
};

void fb(struct Pair *p) {
    p->b = (struct B*)malloc(sizeof(struct B));
    p->b->b = 1.23;
    printf("fb %f\n", p->b->b);
}
