#define _DEFAULT_SOURCE
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

void signalHandler(int) {
    volatile int i = 42;
    i = i + 27;
}

void* noOpLoop(void* arg) {
    volatile int i = 0;
    while (1) {
        i = i + 1;
    }
    return NULL;
}

void* signalSender(void* arg) {
    pthread_t target_thread = *((pthread_t*)arg);
    while (1) {
        pthread_kill(target_thread, SIGUSR1);
        usleep(10000);
    }
    return NULL;
}

int main() {
    if (signal(SIGUSR1, signalHandler) == SIG_ERR) {
        printf("error setting signal handler.\n");
        return 1;
    }

    pthread_t thread1, thread2;

    if (pthread_create(&thread1, NULL, noOpLoop, NULL) != 0) {
        printf("error creating thread.\n");
        return 1;
    }

    if (pthread_create(&thread2, NULL, signalSender, &thread1) != 0) {
        printf("error creating thread.\n");
        return 1;
    }

    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);

    printf("???\n");

    return 0;
}
