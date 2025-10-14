#include <array>

enum class En : unsigned int {
    A = 1,
    B = 0xffffffff,
    C = 0,
};

enum class Neg : int {
    A = -1,
    B = -2,
    C = -100,
    D = 5,
};

enum Flags : int {
    A = 0x1,
    B = 0x2,
    C = 0x4,
    D = 0x8,
    A_AND_C = 0x5,
};

int main() {
    std::array<volatile En, 4> a {En::A, En::B, En::C, (En)42};
    std::array<volatile Neg, 5> b {(Neg)42, Neg::A, Neg::B, Neg::C, Neg::D};
    volatile unsigned s = 0;
    for (auto x : a) s = s + (unsigned)x;
    for (auto x : b) s = s + (unsigned)x;
    (void)s;
    volatile int f = Flags::A | Flags::B | Flags::C;
    (void)f;
}
