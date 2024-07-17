// (written mostly by Claude)

#define _DEFAULT_SOURCE
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Function pointer type for our dynamically created function
typedef int (*MulFunc)(int);

int main() {
    // Machine code for a function that multiplies its argument by 2
    // This is x64 assembly: mov eax, edi; add eax, eax; ret
    unsigned char code[] = {0x89, 0xf8, 0x01, 0xc0, 0xc3};
    size_t code_size = sizeof(code);

    // Allocate executable memory using mmap
    void *mem = mmap(NULL, code_size, PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    
    if (mem == MAP_FAILED) {
        perror("mmap");
        exit(1);
    }

    // Copy our machine code to the allocated memory
    memcpy(mem, code, code_size);

    // Cast the memory to our function pointer type
    MulFunc multiply = (MulFunc)mem;

    // Test our function
    int result = multiply(5);
    printf("5 * 2 = %d\n", result);

    // Clean up
    if (munmap(mem, code_size) == -1) {
        perror("munmap");
        exit(1);
    }

    return 0;
}
