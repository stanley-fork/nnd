#include <string_view>
#include <cstdio>

int __attribute__((noinline)) f(int x) {
    int v = 1;
    const int C = 10;
    constexpr int CE = 20;
    static int S = 30;
    static const int SC = 40;
    static constexpr int SCE = 50;
    return x + v + C + CE + S + SC + SCE;
}

inline int g(int x) {
    int v = 1;
    const int C = 10;
    constexpr int CE = 20;
    static int S = 30;
    static const int SC = 40;
    static constexpr int SCE = 50;
    return x + v + C + CE + S + SC + SCE;
}

int G = 99;
const int GC = 100;
constexpr int GCE = 200;
static int GS = 300;
static const int GSC = 400;
static constexpr int GSCE = 500;

struct str {
    static int NS;
    static const int NSC = 4000;
    static constexpr int NSCE = 5000;
};
int str::NS = 3000;

constexpr std::string_view GSV = "meow";

template <int X = 42>
struct T {
    int f = X;
    static constexpr int XX = X;
};

int main() {
    volatile T<1337> t;
    printf("%d", f(42) + g(42) + t.f + T<>::XX + G + GC + GCE + GS + GSC + GSCE);
}
