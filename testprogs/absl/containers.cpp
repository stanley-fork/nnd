#include <iostream>
#include <string>
#include <random>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/container/node_hash_map.h"
#include "absl/container/node_hash_set.h"
#include "absl/container/btree_map.h"
#include "absl/container/btree_set.h"
#include "absl/container/inlined_vector.h"
#include "absl/container/fixed_array.h"

std::string random_string(std::mt19937& gen, int len) {
    static const char alphanum[] =
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    std::uniform_int_distribution<> dis(0, sizeof(alphanum) - 2);
    std::string str(len, ' ');
    for (int i = 0; i < len; ++i) {
        str[i] = alphanum[dis(gen)];
    }
    return str;
}

int main() {
    std::mt19937 gen(42);
    std::uniform_int_distribution<> dis(1, 1000);

    absl::flat_hash_map<int, std::string> fhm;
    for (int i = 0; i < 200; ++i) {
        fhm[i] = random_string(gen, 10);
    }

    absl::flat_hash_set<int> fhs;
    for (int i = 0; i < 200; ++i) {
        fhs.insert(dis(gen));
    }

    absl::node_hash_map<int, std::string> nhm;
    for (int i = 0; i < 200; ++i) {
        nhm[i] = random_string(gen, 10);
    }

    absl::node_hash_set<int> nhs;
    for (int i = 0; i < 200; ++i) {
        nhs.insert(dis(gen));
    }

    absl::btree_map<int, std::string> btm;
    for (int i = 0; i < 200; ++i) {
        btm[i] = random_string(gen, 10);
    }

    absl::btree_set<int> bts;
    for (int i = 0; i < 200; ++i) {
        bts.insert(dis(gen));
    }

    absl::InlinedVector<int, 10> small_iv;
    for (int i = 0; i < 5; ++i) {
        small_iv.push_back(dis(gen));
    }

    absl::InlinedVector<int, 10> large_iv;
    for (int i = 0; i < 20; ++i) {
        large_iv.push_back(dis(gen));
    }

    absl::FixedArray<int> fa(50);
    for (int i = 0; i < 50; ++i) {
        fa[i] = dis(gen);
    }

    volatile int dummy = fhm.size() + fhs.size() + nhm.size() + nhs.size() + 
             btm.size() + bts.size() + small_iv.size() + large_iv.size() + 
             fa.size();
    (void)dummy;

    return 0;
}
