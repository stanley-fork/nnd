cmake_minimum_required(VERSION 3.10)
project(i_dont_want_to_use_cmake)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -Og")

add_subdirectory(lib)

add_executable(containers containers.cpp)

target_link_libraries(containers
    absl::flat_hash_map
    absl::flat_hash_set
    absl::node_hash_map
    absl::node_hash_set
    absl::btree
    absl::inlined_vector
    absl::fixed_array
)
