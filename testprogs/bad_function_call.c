int main() {
    int (*f)(int) = (int (*)(int))42;
    f(10);
}
