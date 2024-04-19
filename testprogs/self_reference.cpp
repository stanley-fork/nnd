#include <iostream>
using namespace std;

struct A {
    A volatile & a;
    volatile int x = 42;

    A() : a(*this) {}
};

int main() {
    A a;
    cout<<a.x<<endl;
}
