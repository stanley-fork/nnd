#include <iostream>
using namespace std;

int main(){
    {
        struct A{int f() {return 1;}};
        cout<<A().f()<<endl;
    }
    {
        struct A{int f() {return 2;}};
        cout<<A().f()<<endl;
    }
}
