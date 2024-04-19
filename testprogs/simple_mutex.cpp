#include <mutex>
#include <thread>
#include <iostream>
using namespace std;

int main() {
    mutex m;

    auto f = [&] {
        for (int i = 0; i < 60; ++i) {
            {
                unique_lock<mutex> lock(m);
                cout << i << endl;
                this_thread::sleep_for(chrono::seconds(1));
            }
            this_thread::sleep_for(chrono::milliseconds(10));
        }
    };
    
    thread t1(f);
    thread t2(f);
    t1.join();
    t2.join();
}
