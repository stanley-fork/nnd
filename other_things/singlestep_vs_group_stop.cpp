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
#include <iostream>
#include <thread>
#include <sys/syscall.h>
using namespace std;

#define CHECK(x) if (!(x)) { cerr << "check failed on line " << __LINE__ << ": " << #x << endl; exit(1); }
#define CHECK_EQ(a, b) { auto hyg_a = (a); auto hyg_b = (b); if (hyg_a != hyg_b) { cerr << "check failed on line " << __LINE__ << ": " #a " == " #b " (" << hyg_a << " != " << hyg_b << ")" << endl; exit(1); } }
#define ERRNO(x) if ((x) < 0) { perror(#x); exit(1); }

// This reproduces a somewhat unexpected behavior of PTRACE_SINGLESTEP. Scenario:
//  1. PTRACE_SINGLESTEP for a thread that's blocked on syscall.
//  2. The thread gets stopped because of some signal (unrelated to the SINGLESTEP).
//  3. wait() reports a SIGTRAP instead of the signal that stopped the thread - a little unexpected, but
//     understandable (the signal delivery interrupted the syscall, which can be considered a step).
//  4. PTRACE_CONT.
//  5. The thread immediately stops again and reports the signal.
//
// A similar but more weird behavior that I saw in practice:
//  1. PTRACE_SINGLESTEP for a thread that's blocked on syscall.
//  2. The thread gets stopped by a group-stop.
//  3. wait() reports the group-stop.
//  4. PTRACE_CONT.
//  5. wait() reports SIGTRAP. This seems unexpected and difficult to handle correctly!
//     I guess what happens is that both the group-stop event and SIGTRAP event get enqueued for delivery at once,
//     then get delivered in arbitrary order, so we may get either this scenario or the scenario above arbitrarily.

void __attribute__((noinline)) child_run() {
    struct timespec req, rem;
    req.tv_sec = 5;
    req.tv_nsec = 0;
    int pid = syscall(SYS_getpid);

    syscall(SYS_tgkill, pid, pid, SIGSTOP);
    syscall(SYS_nanosleep, &req, &rem);
    syscall(SYS_exit, 42);
}

int main(int argc, char**argv) {
    if (argc == 2) {
        cout<<"running child function only"<<endl;
        child_run();
        return 0;
    }

    cout<<"forking"<<endl;
    int pid = fork();
    ERRNO(pid);
    if (pid == 0) {
        child_run();
        return 1;
    }
    
    cout<<"attaching to "<<pid<<endl;
    ERRNO(ptrace(PTRACE_SEIZE, pid, 0, PTRACE_O_EXITKILL | PTRACE_O_TRACECLONE | PTRACE_O_TRACEEXEC | PTRACE_O_TRACEFORK | PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACEVFORK | PTRACE_O_TRACEVFORKDONE));

    int w, r;
    struct user_regs_struct regs;

    cout<<"waiting for initial SIGSTOP"<<endl;
    r = wait(&w); ERRNO(r); CHECK_EQ(r, pid); CHECK(WIFSTOPPED(w)); CHECK_EQ(WSTOPSIG(w), SIGSTOP); CHECK_EQ(w>>16, 0);

    cout<<"continuing"<<endl;
    ERRNO(ptrace(PTRACE_CONT, pid, 0, 0));

    cout<<"sleeping"<<endl;
    sleep(1);
    r = waitpid(-1, &w, WNOHANG);
    if (r != 0) { cerr << "unexpected event: " << w << endl; exit(1); }

    cout<<"stopping"<<endl;
    ERRNO(tgkill(pid, pid, SIGSTOP));
    r = wait(&w); ERRNO(r); CHECK_EQ(r, pid); CHECK(WIFSTOPPED(w)); CHECK_EQ(WSTOPSIG(w), SIGSTOP); CHECK_EQ(w>>16, 0);
    ERRNO(ptrace(PTRACE_GETREGS, pid, 0, &regs));
    cout<<"stopped at 0x"<<hex<<regs.rip<<endl;

    cout<<"triggering a single-step"<<endl;
    ERRNO(ptrace(PTRACE_SINGLESTEP, pid, 0, 0));

    cout<<"sleeping"<<endl;
    sleep(1);
    r = waitpid(-1, &w, WNOHANG);
    if (r != 0) { cerr << "unexpected event: " << w << endl; exit(1); }

    cout<<"stopping"<<endl;
    ERRNO(tgkill(pid, pid, SIGSTOP));
    r = wait(&w); ERRNO(r); CHECK_EQ(r, pid); CHECK(WIFSTOPPED(w)); CHECK_EQ(w>>16, 0);
    ERRNO(ptrace(PTRACE_GETREGS, pid, 0, &regs));
    cout<<"stopped at 0x"<<hex<<regs.rip<<endl;
    if (WSTOPSIG(w) == SIGTRAP) {
        cout<<"!! UNEXPECTED SIGTRAP !!"<<endl;
        cout<<"continuing"<<endl;
        ERRNO(ptrace(PTRACE_CONT, pid, 0, 0));
        cout<<"waiting for SIGSTOP"<<endl;
        r = wait(&w); ERRNO(r); CHECK_EQ(r, pid); CHECK(WIFSTOPPED(w)); CHECK_EQ(w>>16, 0); CHECK_EQ(WSTOPSIG(w), SIGSTOP);
    } else {
        CHECK_EQ(WSTOPSIG(w), SIGSTOP);
    }

    cout<<"sleeping"<<endl;
    sleep(1);
    r = waitpid(-1, &w, WNOHANG);
    if (r != 0) { cerr << "unexpected event: " << w << endl; exit(1); }

    cout<<"continuing"<<endl;
    ERRNO(ptrace(PTRACE_CONT, pid, 0, 0));

    cout<<"waiting"<<endl;
    r = wait(&w); ERRNO(r); CHECK_EQ(r, pid); CHECK(WIFEXITED(w)); CHECK_EQ(WEXITSTATUS(w), 42); CHECK_EQ(w>>16, 0);

    cout<<"done"<<endl;
}
