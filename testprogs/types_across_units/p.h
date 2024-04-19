// No compilation unit has both A and B defined. So the debugger can't get a full definition of Pair without relying on names and combining types from different units.

struct A;
struct B;

struct Pair {
    struct A *a;
    struct B *b;
};

void fa(struct Pair *p);
void fb(struct Pair *p);
