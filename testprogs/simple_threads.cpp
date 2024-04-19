#include <thread>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
using namespace std;

int main(){
    cout<<"started"<<endl;
    thread t([] {
        for (int i = 0; i < 20; ++i) {
            cout<<"thread i="<<time(0)<<endl;
            sleep(1);
        }
    });
    for (int i = 0; i < 10; ++i) {
        cout<<"main i="<<time(0)<<endl;
        sleep(1);
    }
    t.detach(); cout<<"detached"<<endl;
    //t.join(); cout<<"joined"<<endl;
}
