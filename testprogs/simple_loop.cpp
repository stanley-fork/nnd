int main() {
    volatile long i = 0;
    while (true) {
        i = i + 1;
        i = i + 2;
    }
}
