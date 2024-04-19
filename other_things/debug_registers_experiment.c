#include <sys/ptrace.h>
#include <linux/futex.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <stddef.h>

#define CALL(x) if (x < 0) { perror(#x); exit(1); }

int main() {
    int pid = fork();
    if (pid < 0) { perror("fork"); exit(1); }
    if (pid == 0) {
        printf("hi\n");
        __asm__("mov $0, %rax\n"
                "loop:\n"
                "inc %rax\n"
                "jmp loop\n");
        printf("bye?\n");
        return 1;
    }

    CALL(ptrace(PTRACE_SEIZE, pid, 0, PTRACE_O_EXITKILL | PTRACE_O_TRACECLONE | PTRACE_O_TRACEEXEC | PTRACE_O_TRACEFORK | PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACEVFORK | PTRACE_O_TRACEVFORKDONE));

    sleep(1);

    CALL(ptrace(PTRACE_INTERRUPT, pid, 0, 0));

    int assigned = 0;

    for (int i = 0; i < 30; ++i) {
        int w;
        CALL(wait(&w));

        if (!WIFSTOPPED(w)) { fprintf(stderr, "unexpected wait result: %d", w); exit(1); }
        int sig = WSTOPSIG(w);
        if (sig != SIGTRAP) { fprintf(stderr, "unexpected signal: %d", sig); exit(1); }

        struct user_regs_struct regs;
        CALL(ptrace(PTRACE_GETREGS, pid, 0, &regs));

        printf("rip: %llu, rax: %llu\n", regs.rip, regs.rax);

        //if (!assigned) {
            CALL(ptrace(PTRACE_POKEUSER, pid, offsetof(struct user, u_debugreg[0]), regs.rip));
            CALL(ptrace(PTRACE_POKEUSER, pid, offsetof(struct user, u_debugreg[7]), 1 << 0));
        //}

        //CALL(ptrace(PTRACE_SINGLESTEP, pid, 0, 0));
        CALL(ptrace(PTRACE_CONT, pid, 0, 0));
    }
}
