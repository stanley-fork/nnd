#include <thread>
#include <iostream>
#include <vector>

void do_something() {
    volatile long i = 0;
    while (i < 100000) {
        i = i + 1;
        if (i % 5 == 0)
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}

int main() {
    std::vector<std::thread> t;
    for (int i = 0; i < 10000; ++i)
        t.emplace_back([] { do_something(); });
    for (auto & x : t)
        x.join();
}
