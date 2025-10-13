#include <iostream>
#include <functional>
using namespace std;

long f(int x) {
    return x + 1;
}
long g(int x) {
    return x * 2;
}
typedef long F(int x);

struct S {
    int a=10,b=20,c=30,d=40;

    void foo() volatile {
        cout<<"foo "<<a<<endl;
    }
    void bar() volatile {
        cout<<"bar "<<b<<endl;
    }
};
typedef int S::*SF;

struct fn {
    int this_is_not_a_debugger_type = 1337;
};

int main() {
    long (*volatile p)(int) = f;
    F *volatile q = g;
    cout << (*p)(10) <<  ' ' << (*q)(10) << endl;

    volatile auto f1 = [&] {
        q = p;
    };
    
    volatile std::function<void()> f2 = [&] {
        p = q;
    };
    
    volatile S s;
    int S::*volatile bp = &S::b;
    volatile SF cp = &S::c;
    cout << s.*bp << ' ' << s.*cp << endl;
    s.*bp += 2;
    s.*cp -= 2;
    cout << s.b << ' ' << s.c << endl;

    void (S::*volatile mfp)() volatile = &S::foo;
    (s.*mfp)();
    mfp = &S::bar;
    (s.*mfp)();

    volatile fn ffn;
}
