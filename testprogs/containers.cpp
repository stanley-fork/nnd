#include <vector>
#include <array>
#include <deque>
#include <list>
#include <forward_list>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <stack>
#include <queue>
#include <string>
#include <string_view>
#include <memory>
#include <functional>
#include <chrono>
#include <thread>
#include <future>
#include <iostream>
#include <sstream>
#include <bitset>
#include <complex>
#include <valarray>
#include <optional>
#include <variant>
#include <any>
#include <span>
#include <ranges>
#include <exception>
#include <system_error>

int main() {
    // Comments say whether pretty-printers work for [libstdc++, libc++].
    
    // Slice-like.
    std::vector<int> vec = {1, 2, 3};    // [yes, yes]
    std::string_view strview = "hello";  // [yes, yes]
    std::valarray<int> va = {1, 2, 3};   // [yes, yes]
    std::span<int> sp(vec);              // [yes, yes]

    // Smart pointers.
    std::shared_ptr<int> sptr = std::make_shared<int>(42);  // [yes, yes]
    std::weak_ptr<int> wptr = sptr;                         // [yes, yes]

    // String.
    std::string short_str = "hello";       // [yes, yes]
    std::string long_str(100, '.');

    // Wide strings.
    std::wstring wstr = L"hello";          // [no, no]
    std::u16string u16str = u"hello";      // [no, no]
    std::u32string u32str = U"hello";      // [no, no]

    // Deque.
    std::deque<int> deq = {1, 2, 3};     // [no, no]
    std::stack<int> stk;                 // [no, no]
    stk.push(1);
    std::queue<int> que;                 // [no, no]
    que.push(1);

    // Linked lists.
    std::list<int> lst = {1, 2, 3};           // [no, no]
    std::forward_list<int> flst = {1, 2, 3};  // [no, no]

    // Trees.
    std::set<int> set = {1, 2, 3};                                                // [no, no]
    std::multiset<int> mset = {1, 1, 2, 3};                                       // [no, no]
    std::map<int, std::string> map = {{1, "one"}, {2, "two"}};                    // [no, no]
    std::multimap<int, std::string> mmap = {{1, "one"}, {1, "uno"}, {2, "two"}};  // [no, no]

    // Hash tables.
    std::unordered_set<int> uset = {1, 2, 3};                                                // [no, no]
    std::unordered_multiset<int> umset = {1, 1, 2, 3};                                       // [no, no]
    std::unordered_map<int, std::string> umap = {{1, "one"}, {2, "two"}};                    // [no, no]
    std::unordered_multimap<int, std::string> ummap = {{1, "one"}, {1, "uno"}, {2, "two"}};  // [no, no]

    // Discriminated unions.
    std::optional<int> opt = 42;           // [no, no]
    std::variant<int, double> var = 3.14;  // [no, no]

    // Complicated and not worth it.
    std::any any_val = 42;                                                          // [no, no]
    std::function<int(int)> func = [](int x) { return x * 2; };                     // [no, no]
    std::future<int> fut = std::async(std::launch::deferred, []() { return 42; });  // [no, no]
    std::promise<int> prom;  // [no, no]
    std::exception_ptr eptr = std::make_exception_ptr(std::runtime_error("test"));  // [no, no]
    std::error_code ec = std::make_error_code(std::errc::invalid_argument);         // [no, no]

    // Things that are ok without designated pretty printers.
    std::array<int, 3> arr = {1, 2, 3};
    int raw_array[5] = {1, 2, 3, 4, 5};
    std::priority_queue<int> pque; // not sorted, but meh
    pque.push(1);
    std::unique_ptr<int> uptr = std::make_unique<int>(42);
    std::pair<int, double> pair = {1, 2.0};
    std::tuple<int, double, char> tuple = {1, 2.0, 'a'};
    auto now = std::chrono::system_clock::now();
    auto duration = std::chrono::hours(1);
    std::thread t([](){});
    std::mutex mtx;
    std::condition_variable cv;
    std::bitset<8> bits(42);
    std::complex<double> c(1.0, 2.0);
    auto range = std::views::iota(1, 10);

    // Prevent optimizations
    volatile int dummy = vec[0] + arr[0] + *uptr + *sptr + short_str[0] + long_str[0] + pair.first + std::get<0>(tuple) + 
        *opt + (int)std::get<double>(var) + std::any_cast<int>(any_val) + func(1) + 
        bits.count() + c.real() + va[0] + sp[0] + *range.begin() + strview[0] + 
        wstr[0] + u16str[0] + u32str[0];
    (void)dummy;

    t.join();

    return 0;
}
