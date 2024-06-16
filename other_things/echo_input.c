#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <poll.h>
#include <ctype.h>
#include <string.h>
#include <sys/ioctl.h>

// Save original termios settings
struct termios orig_termios;

// Function to restore the original terminal settings
void restore_terminal() {
    tcsetattr(STDIN_FILENO, TCSANOW, &orig_termios);
    printf("\033[?1049l"); // Switch back to main screen
    printf("\033[?25h"); // Show cursor
    printf("\x1b[?9l\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1006l\x1b[?1007l\x1b[?1015l\x1b[?1016l\x1b[?1004l");
    fflush(stdout);
}

// Function to switch to alternate screen and raw mode
void setup_terminal() {
    tcgetattr(STDIN_FILENO, &orig_termios);
    struct termios raw = orig_termios;
    cfmakeraw(&raw);
    tcsetattr(STDIN_FILENO, TCSANOW, &raw);
    printf("\033[?1049h"); // Switch to alternate screen
    printf("\033[H\033[J"); // Clear screen
    printf("\033[?25l"); // Hide cursor
    // https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking
    printf("\x1b[?1003h\x1b[?1007h\x1b[?1006h\x1b[?1004h");
    fflush(stdout);
}

// Function to get the terminal height
int get_terminal_height() {
    struct winsize ws;
    ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
    return ws.ws_row;
}

// Function to print the buffer with escaped non-alphanumeric characters
void print_buffer(const char *buffer, ssize_t len) {
    for (ssize_t i = 0; i < len; ++i) {
        if (buffer[i] == '\e') {
            printf("\\e");
        } else if (buffer[i] >= 33 && buffer[i] <= 126) {
            putchar(buffer[i]);
        } else {
            printf("\\x%02x", (unsigned char)buffer[i]);
        }
    }
    putchar('\n');
    fflush(stdout);
}

int main() {
    setup_terminal();
    atexit(restore_terminal); // Ensure terminal settings are restored on exit

    struct pollfd fds = {
        .fd = STDIN_FILENO,
        .events = POLLIN
    };

    char buffer[128];
    int row = 0;
    int max_row = get_terminal_height();

    while (1) {
        if (poll(&fds, 1, -1) > 0) {
            if (fds.revents & POLLIN) {
                ssize_t n = read(STDIN_FILENO, buffer, sizeof(buffer) - 1);
                if (n > 0) {
                    buffer[n] = '\0';
                    if (buffer[0] == 'q') {
                        break;
                    }
                    printf("\033[%d;1H", row + 1); // Move cursor to the start of the next line
                    print_buffer(buffer, n);
                    row++;
                    if (row >= max_row) {
                        printf("\033[2J\033[H"); // Clear screen and move cursor to top
                        fflush(stdout);
                        row = 0;
                    }
                }
            }
        }
    }

    return 0;
}
