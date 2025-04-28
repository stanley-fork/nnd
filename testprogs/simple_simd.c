#define _GNU_SOURCE
#include <immintrin.h>
#include <stdio.h>
#include <cpuid.h>
#include <inttypes.h>
#include <time.h>

static void cpuid(int leaf, int subleaf, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx) {
    __cpuid_count(leaf, subleaf, *eax, *ebx, *ecx, *edx);
}

static int has_avx512() {
    unsigned int eax, ebx, ecx, edx;
    cpuid(0, 0, &eax, &ebx, &ecx, &edx);
    if (eax < 7) return 0;
    cpuid(7, 0, &eax, &ebx, &ecx, &edx);
    return (ebx & (1 << 16)) != 0;
}

void fill_avx_registers() {
    uint32_t vals[16];
    for (int i = 0; i < 16; i++) {
        vals[i] = (i + 1) * 0x01010101;
    }
    __asm__ volatile (
        "vbroadcastss 0(%[vals]), %%ymm0\n\t"
        "vbroadcastss 4(%[vals]), %%ymm1\n\t"
        "vbroadcastss 8(%[vals]), %%ymm2\n\t"
        "vbroadcastss 12(%[vals]), %%ymm3\n\t"
        "vbroadcastss 16(%[vals]), %%ymm4\n\t"
        "vbroadcastss 20(%[vals]), %%ymm5\n\t"
        "vbroadcastss 24(%[vals]), %%ymm6\n\t"
        "vbroadcastss 28(%[vals]), %%ymm7\n\t"
        "vbroadcastss 32(%[vals]), %%ymm8\n\t"
        "vbroadcastss 36(%[vals]), %%ymm9\n\t"
        "vbroadcastss 40(%[vals]), %%ymm10\n\t"
        "vbroadcastss 44(%[vals]), %%ymm11\n\t"
        "vbroadcastss 48(%[vals]), %%ymm12\n\t"
        "vbroadcastss 52(%[vals]), %%ymm13\n\t"
        "vbroadcastss 56(%[vals]), %%ymm14\n\t"
        "vbroadcastss 60(%[vals]), %%ymm15\n\t"
        :
        : [vals] "r"(vals)
        : "memory"
    );
}

__attribute__((target("avx512f")))
void fill_avx512_registers() {
    uint32_t vals[32];
    uint8_t kmasks[8];

    for (int i = 0; i < 32; i++) {
        vals[i] = (i + 1) * 0x01010101;
    }
    for (int i = 0; i < 8; i++) {
        kmasks[i] = (1U << i);
    }

    __asm__ volatile (
        "vbroadcastss 0(%[vals]), %%zmm0\n\t"
        "vbroadcastss 4(%[vals]), %%zmm1\n\t"
        "vbroadcastss 8(%[vals]), %%zmm2\n\t"
        "vbroadcastss 12(%[vals]), %%zmm3\n\t"
        "vbroadcastss 16(%[vals]), %%zmm4\n\t"
        "vbroadcastss 20(%[vals]), %%zmm5\n\t"
        "vbroadcastss 24(%[vals]), %%zmm6\n\t"
        "vbroadcastss 28(%[vals]), %%zmm7\n\t"
        "vbroadcastss 32(%[vals]), %%zmm8\n\t"
        "vbroadcastss 36(%[vals]), %%zmm9\n\t"
        "vbroadcastss 40(%[vals]), %%zmm10\n\t"
        "vbroadcastss 44(%[vals]), %%zmm11\n\t"
        "vbroadcastss 48(%[vals]), %%zmm12\n\t"
        "vbroadcastss 52(%[vals]), %%zmm13\n\t"
        "vbroadcastss 56(%[vals]), %%zmm14\n\t"
        "vbroadcastss 60(%[vals]), %%zmm15\n\t"
        "vbroadcastss 64(%[vals]), %%zmm16\n\t"
        "vbroadcastss 68(%[vals]), %%zmm17\n\t"
        "vbroadcastss 72(%[vals]), %%zmm18\n\t"
        "vbroadcastss 76(%[vals]), %%zmm19\n\t"
        "vbroadcastss 80(%[vals]), %%zmm20\n\t"
        "vbroadcastss 84(%[vals]), %%zmm21\n\t"
        "vbroadcastss 88(%[vals]), %%zmm22\n\t"
        "vbroadcastss 92(%[vals]), %%zmm23\n\t"
        "vbroadcastss 96(%[vals]), %%zmm24\n\t"
        "vbroadcastss 100(%[vals]), %%zmm25\n\t"
        "vbroadcastss 104(%[vals]), %%zmm26\n\t"
        "vbroadcastss 108(%[vals]), %%zmm27\n\t"
        "vbroadcastss 112(%[vals]), %%zmm28\n\t"
        "vbroadcastss 116(%[vals]), %%zmm29\n\t"
        "vbroadcastss 120(%[vals]), %%zmm30\n\t"
        "vbroadcastss 124(%[vals]), %%zmm31\n\t"

        "kmovb 0(%[kmasks]), %%k0\n\t"
        "kmovb 1(%[kmasks]), %%k1\n\t"
        "kmovb 2(%[kmasks]), %%k2\n\t"
        "kmovb 3(%[kmasks]), %%k3\n\t"
        "kmovb 4(%[kmasks]), %%k4\n\t"
        "kmovb 5(%[kmasks]), %%k5\n\t"
        "kmovb 6(%[kmasks]), %%k6\n\t"
        "kmovb 7(%[kmasks]), %%k7\n\t"
        :
        : [vals] "r"(vals), [kmasks] "r"(kmasks)
        : "memory"
    );
}

void add_some_numbers_using_avx() {
    int data[32];
    srand(time(NULL));
    for (int i = 0; i < 32; i++) {
        data[i] = rand() % 100;
    }

    __m256i accum = _mm256_setzero_si256();

    for (int i = 0; i < 32; i += 8) {
        __m256i vec = _mm256_loadu_si256((__m256i*)&data[i]);
        accum = _mm256_add_epi32(accum, vec);
    }
    
    int temp[8];
    _mm256_storeu_si256((__m256i*)temp, accum);
    
    int sum = 0;
    for (int i = 0; i < 8; i++) {
        sum += temp[i];
    }
    printf("sum: %d\n", sum);
}

int main() {
    fill_avx_registers();
    if (has_avx512()) {
        fill_avx512_registers();
    }
    add_some_numbers_using_avx();
    return 0;
}
