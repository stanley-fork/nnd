#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

int main() {
    char buf[1024];
    int fd = open("/proc/self/exe", O_RDONLY);
    read(fd, buf, 1024);
    printf("%d\n", buf[100]);
    close(fd);
    return 0;
}
