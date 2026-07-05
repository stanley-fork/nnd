// The main thread exits while other threads keep running. The main thread (thread-group leader) becomes a
// "zombie leader": ptrace requests on its tid fail with ESRCH, but waitpid doesn't report its exit until all
// other threads exit too. The debugger must handle this in-between state.
// Repro for debugger bugs: run this under the debugger, wait for "main thread exiting" to be printed, then add
// a breakpoint (e.g. on the sleep(1) line) and see what breaks.
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

void* sleepALot(void* arg) {
    while (1) {
        sleep(1);
    }
    return NULL;
}

int main() {
    for (int i = 0; i < 2; i++) {
        pthread_t t;
        pthread_create(&t, NULL, sleepALot, NULL);
    }
    printf("main thread exiting; worker threads keep running\n");
    pthread_exit(NULL);
}
