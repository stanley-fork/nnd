#include <stdio.h>
#include <dlfcn.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <path to dl.so>\n", argv[0]);
        return 1;
    }
    void *dl = dlopen(argv[1], RTLD_LAZY);
    void (*f)(int);
    f = dlsym(dl, "f");
    (*f)(42);
}
