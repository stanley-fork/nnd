#include <coroutine>
#include <iostream>
#include <optional>
#include <utility>

struct Coroutine {
    struct promise_type {
        std::suspend_always initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
        std::suspend_always yield_value(int) { return {}; }

        Coroutine get_return_object() {
            return Coroutine{std::coroutine_handle<promise_type>::from_promise(*this)};
        }
    };

    std::coroutine_handle<promise_type> handle;

    Coroutine(std::coroutine_handle<promise_type> h) : handle(h) {}
    Coroutine(Coroutine && c) : handle(std::exchange(c.handle, nullptr)) {}
    ~Coroutine() { if (handle) handle.destroy(); }
};

Coroutine a_coroutine_function() {
    std::cout << "1" << std::endl;
    co_await std::suspend_always{};
    std::cout << "3" << std::endl;
    co_await std::suspend_always{};
    std::cout << "5" << std::endl;
}

struct ManualExecutor {
    Coroutine coroutine;

    ManualExecutor(Coroutine coro) : coroutine(std::move(coro)) {}

    bool step() {
        if (!coroutine.handle.done()) {
            coroutine.handle.resume();
            return true;
        }
        return false;
    }
};

int main() {
    {
        ManualExecutor exec(a_coroutine_function());
        std::cout << "0" << std::endl;
        if (!exec.step()) std::abort();
        std::cout << "2" << std::endl;
        if (!exec.step()) std::abort();
        std::cout << "4" << std::endl;
        if (!exec.step()) std::abort();
        std::cout << "6" << std::endl;
        if (exec.step()) std::abort();
        std::cout << "7" << std::endl;
    }
    std::cout << "8 - done" << std::endl;
}
