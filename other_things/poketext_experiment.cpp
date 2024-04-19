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
using namespace std;

#define CHECK(x) if (!(x)) { cerr << "check failed on line " << __LINE__ << ": " << #x << endl; exit(1); }
#define CHECK_EQ(a, b) { auto hyg_a = (a); auto hyg_b = (b); if (hyg_a != hyg_b) { cerr << "check failed on line " << __LINE__ << ": " #a " == " #b " (" << hyg_a << " != " << hyg_b << ")" << endl; exit(1); } }
#define CALL(x) if ((x) < 0) { perror(#x); exit(1); }

int main() {
    //cout<<offsetof(user, u_debugreg)<<endl; return 0;
    
    int pid = fork();
    if (pid < 0) { perror("fork"); exit(1); }
    if (pid == 0) {
        CALL(raise(SIGSTOP));
        std::thread t([] { volatile long i=0; while (true) i+=1; });
        volatile long i=0; while (true) i+=1;
        return 1;
    }
    cout<<"spawned "<<pid<<endl;

    CALL(ptrace(PTRACE_SEIZE, pid, 0, PTRACE_O_EXITKILL | PTRACE_O_TRACECLONE | PTRACE_O_TRACEEXEC | PTRACE_O_TRACEFORK | PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACEVFORK | PTRACE_O_TRACEVFORKDONE));
    cout<<"attached"<<endl;

    int w;
    int r = wait(&w);
    CALL(r);
    CHECK_EQ(r, pid);
    CHECK(WIFSTOPPED(w)); CHECK_EQ(WSTOPSIG(w), SIGSTOP);
    CALL(ptrace(PTRACE_CONT, pid, 0, 0));

    r=wait(&w);
    CALL(r);
    CHECK_EQ(r, pid); CHECK(WIFSTOPPED(w)); CHECK_EQ(WSTOPSIG(w), SIGTRAP); CHECK_EQ(w >> 16, PTRACE_EVENT_CLONE);
    pid_t tid;
    CALL(ptrace(PTRACE_GETEVENTMSG, pid, 0, &tid));
    cout<<"thread "<<tid<<endl;

    CALL(ptrace(PTRACE_CONT, pid, 0, 0)); cout<<"continued "<<pid<<endl; int running_tid = pid, stopped_tid = tid;

    r=wait(&w);
    CALL(r);
    CHECK_EQ(r, tid); CHECK(WIFSTOPPED(w));
    CHECK_EQ(w>>16, PTRACE_EVENT_STOP);
    // We get either SIGTRAP or SIGSTOP, seemingly at random, different from run to run.
    CHECK(WSTOPSIG(w) == SIGTRAP || WSTOPSIG(w) == SIGSTOP);

    //CALL(ptrace(PTRACE_CONT, tid, 0, 0)); cout<<"continued "<<tid<<endl; int running_tid = tid, stopped_tid = pid;
    //int running_tid = tid, stopped_tid = pid;

    sleep(1);

    r=waitpid(-1, &w, WNOHANG);
    CALL(r);
    if(r) cout<<"unexpected event for "<<r<<": "<<w<<endl;
    CHECK_EQ(r, 0);

    struct user_regs_struct regs;
    r=ptrace(PTRACE_GETREGS, running_tid, 0, &regs);
    CHECK_EQ(r, -1);
    perror("GETREGS on running process (expected to fail)");

    CALL(ptrace(PTRACE_GETREGS, stopped_tid, 0, &regs));
    printf("rip: %llu, rax: %llu\n", regs.rip, regs.rax);

    r=ptrace(PTRACE_POKETEXT, running_tid, regs.rip&~7ul, 0xccccccccccccccccul);
    CHECK_EQ(r, -1);
    perror("POKETEXT on running process (expected to fail)");

    CALL(ptrace(PTRACE_POKETEXT, stopped_tid, regs.rip&~7ul, 0xccccccccccccccccul));
    cout<<"poked"<<endl;

    CALL(ptrace(PTRACE_CONT, stopped_tid, 0, 0));
    cout<<"resumed"<<endl;

    // This is supposed to hit one of the 0xcc-s and stop, but doesn't. Idk why.
    r=wait(&w);
    CALL(r);
    CHECK_EQ(r, stopped_tid);
    CALL(wait(&w));
    CHECK(WIFSTOPPED(w)); CHECK_EQ(WSTOPSIG(w), SIGTRAP); CHECK_EQ(w>>16, 0);

    cout<<"all done"<<endl;
}
