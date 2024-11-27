// Compile with: gcc -g -O2 -mbmi2 -mbmi a.c

// Benchmark some varint parsing implementations.
// Conclusions:
//  * When all lengths are the same, the simple implementation wins. parse_hybrid() is a tiny bit faster. Anything more clever than that is much slower (4x) on short lengths.
//  * When lengths are random, branchful implementations take a 3x hit, while branchless ones don't care and win by 2x.

#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <x86intrin.h>

#define DATA_SIZE (1024 * 1024)
#define NUM_RUNS 1000
#define NUM_TRIALS 2

inline uint64_t parse_simple(const uint8_t** ptr) {
    uint64_t result = 0;
    unsigned shift = 0;
    uint8_t byte;
    do {
        byte = *(*ptr)++;
        result |= ((uint64_t)(byte & 0x7f) << shift);
        shift += 7;
    } while (byte & 0x80);
    return result;
}

inline uint64_t collect_bits(uint64_t x) {
    // '#' - bit from the source, '.' - zero bit, '-' - garbage bit.
    // Our goal is to go from:
    // -#######-#######-#######-#######-#######-#######-#######-#######
    // to
    // ........########################################################

    // -#######-#######-#######-#######-#######-#######-#######-#######
    x = ((x & 0x7f007f007f007f00) >> 1) | (x & 0x007f007f007f007f);
    // ..##############..##############..##############..##############
    x = ((x & 0xffff0000ffff0000) >> 2) | (x & 0x0000ffff0000ffff);
    // ....############################....############################
    x = ((x & 0xffffffff00000000) >> 4) | (x & 0x00000000ffffffff);
    // ........########################################################
    return x;
}

inline uint64_t collect_bits2(uint64_t x) {
    // '#' - bit from the source, '.' - zero bit, '-' - garbage bit.
    // Our goal is to go from:
    // -#######-#######-#######-#######-#######-#######-#######-#######
    // to
    // ........########################################################

    // -#######-#######-#######-#######-#######-#######-#######-#######
    x = ((x & 0x7f007f007f007f00) >> 1) | (x & 0x007f007f007f007f);
    x = ((x & 0xffff000000000000) >> 6) |
        ((x & 0x0000ffff00000000) >> 4) |
        ((x & 0x00000000ffff0000) >> 2) |
        (x & 0x000000000000ffff);
    return x;
}

inline uint64_t parse_hybrid(const uint8_t** ptr) {
    uint64_t chunk = *(uint64_t*)*ptr;
    if ((chunk & 0x80) == 0) { // 1 byte
        *ptr += 1;
        return chunk & 0x7f;
    } else if ((chunk & 0x0000008080808080) != 0x0000008080808080) { // 2..5 bytes
        uint64_t res = (chunk & 0x7f) | ((chunk & 0x7f00) >> 1);
        *ptr += 2;
        if (chunk & 0x8000) {
            res |= (chunk & 0x7f0000) >> 2;
            *ptr += 1;
        }
        if ((chunk & 0x808000) == 0x808000) {
            res |= (chunk & 0x7f000000) >> 3;
            *ptr += 1;
        }
        if ((chunk & 0x80808000) == 0x80808000) {
            res |= (chunk & 0x7f00000000) >> 4;
            *ptr += 1;
        }
        return res;
    } else if ((chunk & 0x8080808080808080) != 0x8080808080808080) { // 6..8 bytes
        int bytes = _tzcnt_u64((~chunk) & 0x8080808080808080UL) >> 3;
        uint64_t res = collect_bits2(chunk);
        res &= UINT64_MAX >> (64 - 7 - bytes*7);
        *ptr += bytes + 1;
        return res;
    } else { // 9..10 bytes
        uint64_t res = collect_bits2(chunk);
        uint64_t high = (uint64_t)*(uint16_t*)(*ptr + 8);
        res |= high << 56;
        res &= (high << 55) | 0x7fffffffffffffff;
        *ptr += 9 + ((high >> 7) & 1);
        return res;
    }
}

inline uint64_t parse_branchless(const uint8_t** ptr) {
    uint64_t chunk = *(uint64_t*)*ptr;
    int bytes = _tzcnt_u64((~chunk) & 0x8080808080808080UL) >> 3;
    uint64_t res;
    if (bytes < 8) {
        res = _pext_u64(chunk, 0x7f7f7f7f7f7f7f7fUL >> (8 * (7 - bytes)));
    } else {
        res = _pext_u64(chunk, 0x7f7f7f7f7f7f7f7fUL);
        chunk = *(uint64_t*)(*ptr + 8);
        res |= (chunk & 0x7f) << 56;
        uint64_t extra = (chunk >> 7) & 1;
        bytes += extra;
        res |= ((chunk >> 8) & extra) << 63;
    }

    *ptr += bytes + 1;
    return res;
}

inline uint64_t parse_branchless2(const uint8_t** ptr) {
    uint64_t one = *(uint64_t*)*ptr;
    uint64_t two = *(uint64_t*)(*ptr + 8);

    uint64_t bytes1 = _tzcnt_u64((~one) & 0x8080808080808080UL) >> 3;
    uint64_t is_long = bytes1 >> 3;
    uint64_t res = _pext_u64(one, 0x7f7f7f7f7f7f7f7fUL >> (8 * (7 + is_long - bytes1)));

    uint8_t long_mask_byte = -is_long;
    uint64_t bytes2 = (two >> 7) & is_long;
    two &= (two | 0xff) >> 1;
    two &= long_mask_byte;
    res |= two << 56;

    uint64_t bytes = bytes1 + 1 + bytes2;
    *ptr += bytes;
    return res;
}

inline __m128i collect_bits_twice(__m128i x) {
    // Just like collect_bits(), but for two 64-bit numbers simultaneously.
    x = _mm_or_si128(
        _mm_srli_epi64(_mm_and_si128(x, _mm_set1_epi16(0x7f00)), 1),
        _mm_and_si128(x, _mm_set1_epi16(0x007f))
    );
    x = _mm_or_si128(
        _mm_srli_epi64(_mm_and_si128(x, _mm_set1_epi32(0xffff0000)), 2),
        _mm_and_si128(x, _mm_set1_epi32(0x0000ffff))
    );
    x = _mm_or_si128(
        _mm_srli_epi64(_mm_and_si128(x, _mm_set1_epi64x(0xffffffff00000000)), 4),
        _mm_and_si128(x, _mm_set1_epi64x(0x00000000ffffffff))
    );
    return x;
}

inline uint64_t parse_branchless3(const uint8_t** ptr) {
    uint64_t one = *(uint64_t*)*ptr;
    uint64_t two = *(uint64_t*)(*ptr + 8);

    uint64_t bytes1 = _tzcnt_u64((~one) & 0x8080808080808080UL) >> 3;
    uint64_t is_long = bytes1 >> 3;
    uint64_t res = collect_bits(one & (0x7f7f7f7f7f7f7f7fUL >> (8 * (7 + is_long - bytes1))));

    uint8_t long_mask_byte = -is_long;
    uint64_t bytes2 = (two >> 7) & is_long;
    two &= (two | 0xff) >> 1;
    two &= long_mask_byte;
    res |= two << 56;

    uint64_t bytes = bytes1 + 1 + bytes2;
    *ptr += bytes;
    return res;
}

inline uint64_t parse_branchless4(const uint8_t** ptr) {
    uint64_t one = *(uint64_t*)*ptr;
    uint64_t two = *(uint64_t*)(*ptr + 8);

    uint64_t bytes1 = _tzcnt_u64((~one) & 0x8080808080808080UL) >> 3;
    uint64_t is_long = bytes1 >> 3;
    uint64_t res = collect_bits2(one & (0x7f7f7f7f7f7f7f7fUL >> (8 * (7 + is_long - bytes1))));

    uint8_t long_mask_byte = -is_long;
    uint64_t bytes2 = (two >> 7) & is_long;
    two &= (two | 0xff) >> 1;
    two &= long_mask_byte;
    res |= two << 56;

    uint64_t bytes = bytes1 + 1 + bytes2;
    *ptr += bytes;
    return res;
}

inline uint64_t parse_simd(const uint8_t** ptr) {
    __m128i data = _mm_loadu_si128((__m128i*)*ptr);

    __m128i cont = _mm_and_si128(data, _mm_set1_epi8(0x80));
    uint32_t cont_mask = _mm_movemask_epi8(cont);
    int bytes = _tzcnt_u32(~cont_mask) + 1;
    *ptr += bytes;

    __m128i halves = collect_bits_twice(data);
    uint64_t lo = _mm_cvtsi128_si64(halves);
    // Shift by one byte, so the lower byte of upper half turns into the upper byte of the result.
    uint64_t hi = _mm_cvtsi128_si64(_mm_srli_si128(halves, 1));
    uint64_t res = lo | (hi & 0xff00000000000000);
    uint64_t discard_bits = 64 - bytes*7;
    discard_bits &= ~(discard_bits >> 32);
    res &= UINT64_MAX >> (uint8_t)discard_bits;
    return res;
}

// Returns number of bytes written
int encode_uleb128(uint64_t value, uint8_t* ptr) {
    int n = 0;
    do {
        uint8_t byte = value & 0x7f;
        value >>= 7;
        if (value != 0) byte |= 0x80;
        *ptr++ = byte;
        n++;
    } while (value != 0);
    return n;
}

// Generate random n-bit number
uint64_t rand_bits(int n) {
    if (n == 0) return 0;
    uint64_t mask = UINT64_MAX >> (64 - n);
    uint64_t r = 0;
    for (int i = 0; i < 64; i += 16)
        r |= ((uint64_t)rand() & 0xffff) << i;
    return r & mask;
}

void benchmark() {
    printf("RAND_MAX: %d\n", RAND_MAX);
    
    // Generate test data.
    const int NUM_DATASETS = 10;
    uint8_t* data[NUM_DATASETS];
    uint64_t expected_sums[NUM_DATASETS];
    for (int i = 0; i < NUM_DATASETS; ++i) {
        data[i] = aligned_alloc(16, DATA_SIZE + 100);
        expected_sums[i] = 0;
        int pos = 0;
        int count = 0;

        int bits = 7*(i+1);
        if (bits > 64) bits = 64;
        while (pos < DATA_SIZE) {
            if (i == 9) bits = rand() % 64 + 1;
            uint64_t num = rand_bits(bits);
            expected_sums[i] += num;
            pos += encode_uleb128(num, data[i] + pos);
            count += 1;
        }

        printf("dataset %d count: %d (avg %f bytes), sum: %lu\n", i, count, DATA_SIZE * 1. / count, expected_sums[i]);
    }

    // Benchmark
    for (int trial = 0; trial < NUM_TRIALS; trial++) {
        printf("\nTrial %d:\n", trial + 1);

        for (int ds = 0; ds < NUM_DATASETS; ++ds) {
            printf("\n Dataset %d:\n", ds);

            struct timespec start, end;
            uint64_t sum;
            double elapsed;

#define BENCH(f)                                                        \
            clock_gettime(CLOCK_MONOTONIC, &start);                     \
            for (int run = 0; run < NUM_RUNS; run++) {                  \
                sum = 0;                                                \
                const uint8_t* ptr = data[ds];                          \
                const uint8_t* end = data[ds] + DATA_SIZE;              \
                while (ptr < end) {                                     \
                    uint64_t val = f(&ptr);                             \
                    sum += val;                                         \
                }                                                       \
            }                                                           \
            clock_gettime(CLOCK_MONOTONIC, &end);                       \
            elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9; \
            printf(#f ":     %.3f ms%s\n", elapsed * 1000, sum == expected_sums[ds] ? "" : " (sum incorrect!)");

            BENCH(parse_simple);
            BENCH(parse_hybrid);
            BENCH(parse_branchless);
            BENCH(parse_branchless2);
            BENCH(parse_branchless3);
            //BENCH(parse_branchless4);
            BENCH(parse_simd);
        }
    }
}

void test() {
    uint8_t buf[100];
    for (uint64_t i = 0; i < 10000000000; ++i) {
        if (i % 100000000 == 0)
            printf("... i = %#018lx\n", i);

        int bits = rand() % 64 + 1;
        uint64_t num = rand_bits(bits);

        int len = encode_uleb128(num, buf);

        const uint8_t* ptr = buf;
        uint64_t val = parse_hybrid(&ptr);
        
        if (ptr != buf + len || val != num) {
            printf("Failed on i = %lu. Expected: %#018lx (%d bytes)\nFound: %#018lx (%ld bytes)\n", i, num, len, val, ptr - buf);
            break;
        }
    }
}

int main() {
    //srand(time(0));
    //test();
    benchmark();
    return 0;
}
