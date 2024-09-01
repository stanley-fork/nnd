#include <iostream>
#include <string>

void __attribute__((noinline)) f(int x) {
    throw std::string("hi");
}

void __attribute__((noinline)) g(int a, int b) {
    f(a+b);
}

int main() {
    try {
        g(4, 5);
    } catch (std::string & e) {
        std::cout << e << std::endl;
    }

    g(2, 3);
}
