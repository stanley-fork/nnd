#include <iostream>
#include <vector>
#include <thread>
#include <mutex>

thread_local int simple_int = 42;
thread_local int simple_array[5] = {10, 20, 30, 40, 50};
thread_local std::vector<int> tls_vector = {1, 2, 3};

std::mutex cout_mutex;

void thread_function(int thread_id) {
    // Modify the thread-local variables to prevent optimization
    simple_int += thread_id;
    simple_array[0] += thread_id;
    tls_vector.push_back(thread_id);
    
    // Print the values to show they are thread-specific
    std::lock_guard<std::mutex> lock(cout_mutex);
    std::cout << "Thread " << thread_id << ":\n";
    std::cout << "  simple_int = " << simple_int << "\n";
    std::cout << "  simple_array[0] = " << simple_array[0] << "\n";
    std::cout << "  tls_vector = {";
    for (size_t i = 0; i < tls_vector.size(); ++i) {
        if (i > 0) std::cout << ", ";
        std::cout << tls_vector[i];
    }
    std::cout << "}\n";
}

int main() {
    std::thread t1(thread_function, 1);
    std::thread t2(thread_function, 2);
    std::thread t3(thread_function, 3);
    
    t1.join();
    t2.join();
    t3.join();
    
    // Access in main thread to prevent optimization
    simple_int += 100;
    simple_array[1] += 100;
    tls_vector.push_back(100);
    
    std::cout << "Main thread:\n";
    std::cout << "  simple_int = " << simple_int << "\n";
    std::cout << "  simple_array[1] = " << simple_array[1] << "\n";
    std::cout << "  tls_vector = {";
    for (size_t i = 0; i < tls_vector.size(); ++i) {
        if (i > 0) std::cout << ", ";
        std::cout << tls_vector[i];
    }
    std::cout << "}\n";
    
    return 0;
}
