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
    std::wstring wstr = L"hello";          // [good enough, good enough]
    std::u16string u16str = u"hello";      // [good enough, good enough]
    std::u32string u32str = U"hello";      // [good enough, good enough]

    auto pv = std::make_shared<std::vector<int>>(std::vector<int>({10, 20, 30}));

    // Deque.
    std::deque<int> deq = {1, 2, 3};     // [yes, yes]
    std::stack<int> stk;                 // [yes, yes]
    stk.push(1);
    std::queue<int> que;                 // [yes, yes]
    que.push(1);
    for (int i = 0; i < 100; ++i) {
        deq.push_back(i * 10);
        stk.push(i * 100);
        que.push(i * 1000);
    }
    for (int i = 0; i < 50; ++i) {
        deq.pop_front();
        stk.pop();
        que.pop();
    }

    // Linked lists.
    std::list<int> lst = {1, 2, 3};           // [yes, yes]
    std::forward_list<int> flst = {1, 2, 3};  // [yes, yes]

    // Trees.
    std::set<int> set = {1, 2, 3};                                                // [yes, yes]
    std::multiset<int> mset = {1, 1, 2, 3};                                       // [yes, yes]
    std::map<int, std::string> map = {{1, "one"}, {2, "two"}};                    // [yes, yes]
    std::multimap<int, std::string> mmap = {{1, "one"}, {1, "uno"}, {2, "two"}};  // [yes, yes]

    // Hash tables.
    std::unordered_set<int> uset = {1, 2, 3};                                                // [yes, yes]
    std::unordered_multiset<int> umset = {1, 1, 2, 3};                                       // [yes, yes]
    std::unordered_map<int, std::string> umap = {{1, "one"}, {2, "two"}};                    // [yes, yes]
    std::unordered_multimap<int, std::string> ummap = {{1, "one"}, {1, "uno"}, {2, "two"}};  // [yes, yes]

    std::unique_ptr<std::vector<int>> pvec(new std::vector<int>{10, 20, 30, 40});

    // Discriminated unions.
    std::optional<int> opt = 42;           // [yes, yes]
    std::optional<std::string> opt_str = "henlo";
    std::optional<std::optional<std::vector<int>>> opt_nested = std::make_optional(std::make_optional(std::vector<int> {10, 20, 30}));
    std::optional<std::optional<std::vector<int>>> opt_half_null;
    opt_half_null.emplace();
    std::optional<std::string> opt_null;
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
    volatile int64_t dummy = vec[0] + arr[0] + arr[2] + *uptr + *sptr + short_str[0] + long_str[0] + pair.first + std::get<0>(tuple) + 
        (int64_t)&opt + (int64_t)&opt_str + (int64_t)&opt_nested + (int64_t)&opt_half_null + (int64_t)&opt_null +
        (int)std::get<double>(var) + std::any_cast<int>(any_val) + func(1) + 
        bits.count() + c.real() + va[0] + sp[0] + *range.begin() + strview[0] + 
        wstr[0] + u16str[0] + u32str[0] + (*pv)[0];
    (void)dummy;

    t.join();


    // Make everything big.
    std::vector<std::vector<std::complex<double>>> mat;
    for (int i = 0; i < 1000000; ++i) {
        vec.push_back(i * 10);
        long_str.push_back((char)('a'+__builtin_popcount(i)));
        deq.push_back(i * 11);
        stk.push(i * 12);
        lst.push_back(i * 13);
        flst.push_front(i * 14);
        set.insert(i * 15);
        mset.insert(i / 10);
        map.emplace(i * 16, std::to_string(i * 160));
        mmap.emplace(i / 5, std::to_string(i / 5 * 10));
        uset.insert(i * 17);
        umset.insert(i / 6);
        umap.emplace(i * 18, std::to_string(i * 180));
        ummap.emplace(i / 4, std::to_string(i / 4 * 10));

        int row = i / 1000;
        mat.resize(row + 1);
        mat[row].emplace_back((double)i, (double)(i/10));
    }
    auto big_array = std::make_unique<std::array<int, 1000000>>();

    volatile int dummy2 = vec[0] + arr[0] + *uptr + *sptr + short_str[0] + long_str[0] + pair.first + std::get<0>(tuple) + 
        (int)std::get<double>(var) + std::any_cast<int>(any_val) + func(1) + 
        bits.count() + c.real() + va[0] + sp[0] + *range.begin() + strview[0] + 
        wstr[0] + u16str[0] + u32str[0] + (*pv)[0] + mat.size() + mat[10].size() + pvec->size();
    (void)dummy2;

    return 0;
}
