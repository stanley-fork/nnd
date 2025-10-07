#include <vector>
#include <iostream>

int main() {
    volatile struct {
        int foo = 13;
        std::vector<int> bar;
    } x;
    volatile auto y = [&](int z) {
        x.foo += z;
    };
    volatile struct S {
        int a = 42;
    } s;
    std::cout << ((const char*)&x)[sizeof(x)-1] << ((const char*)&y)[sizeof(y)-1] << ((const char*)&s)[sizeof(s)-1];
}
