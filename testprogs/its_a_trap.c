#include <stdio.h>

int main() {
    printf("one\n");
    __builtin_debugtrap();
    printf("two\n");
    __asm__ volatile("int3");
    printf("three\n");
    __builtin_trap();
}
