#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <cpuid.h>

int main() {
    unsigned int eax, ebx, ecx, edx;
    unsigned int max_subleaf;
    __cpuid_count(0xD, 0, eax, ebx, ecx, edx);
    max_subleaf = eax;
    for (unsigned int i = 0; i <= max_subleaf; i++) {
        __cpuid_count(0xD, i, eax, ebx, ecx, edx);
        if (eax == 0 && ebx == 0 && ecx == 0 && edx == 0) continue;
        printf("Component %2u: Size=%u Offset=%u Flags=0x%08x\n", i, eax, ebx, ecx);
    }
    return 0;
}
