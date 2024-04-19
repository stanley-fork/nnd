#include <string>
#include <cstring>
#include <string_view>
#include <iostream>

int main() {
    volatile const char * a = "asd";
    std::string b = "qwe";
    std::string_view c = b;
    std::string d(1000000, 'x');
    volatile const char * e = d.c_str();
    volatile size_t s = (size_t)std::strlen((const char*)a) + b.size() + c.size() + d.size() + (size_t)strlen((const char*)e);
    std::cout << s;
}
