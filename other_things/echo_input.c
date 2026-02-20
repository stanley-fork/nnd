#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>

int main(int argc, char **argv) {
    struct termios old, raw;
    tcgetattr(STDIN_FILENO, &old);
    raw = old;
    raw.c_lflag &= ~(ICANON | ECHO);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSANOW, &raw);

    /* enable requested modes */
    for (int i = 1; i < argc; i++)
        printf("\x1b[?%sh", argv[i]);
    printf("\x1b[H");
    fflush(stdout);

    for (;;) {
        char buf[256];
        int n = read(STDIN_FILENO, buf, sizeof(buf));
        if (n <= 0) break;
        if (n == 1 && buf[0] == 'q') break;

        /* print each byte, escaping non-printables */
        for (int i = 0; i < n; i++) {
            unsigned char c = buf[i];
            if (c == 0x1b)
                printf("\\e");
            else if (c < 0x20 || c >= 0x7f)
                printf("\\x%02x", c);
            else
                putchar(c);
        }
        printf("  [%d bytes]\n", n);
        fflush(stdout);
    }

    /* disable requested modes */
    for (int i = 1; i < argc; i++)
        printf("\x1b[?%sl", argv[i]);
    fflush(stdout);

    tcsetattr(STDIN_FILENO, TCSANOW, &old);
    return 0;
}
