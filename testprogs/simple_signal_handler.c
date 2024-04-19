#include <signal.h>
#include <stdio.h>

void signalHandler(int) {
    printf("received signal\n");
}

int main() {
    if (signal(SIGUSR1, signalHandler) == SIG_ERR) {
        printf("error setting signal handler.\n");
        return 1;
    }

    raise(SIGUSR1);

    printf("bye\n");

    return 0;
}
