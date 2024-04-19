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

int main() {
    std::array<volatile En, 4> a {En::A, En::B, En::C, (En)42};
    std::array<volatile Neg, 5> b {(Neg)42, Neg::A, Neg::B, Neg::C, Neg::D};
    volatile unsigned s = 0;
    for (auto x : a) s = s + (unsigned)x;
    for (auto x : b) s = s + (unsigned)x;
    (void)s;
}
