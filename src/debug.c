#include <errno.h>
#include <execinfo.h>
#if INTERFACE
#include <stdbool.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* #include "log.h" */
/* #if EXPORT */
/* #include "utarray.h" */
/* #endif */

#include "debug.h"

bool debug = false;
bool trace = false;

/* #if defined(DEBUG) */
/* https://stackoverflow.com/questions/6934659/how-to-make-backtrace-backtrace-symbols-print-the-function-names */
static void full_write(int fd, const char *buf, size_t len)
{
        while (len > 0) {
                ssize_t ret = write(fd, buf, len);

                if ((ret == -1) && (errno != EINTR))
                        break;

                buf += (size_t) ret;
                len -= (size_t) ret;
        }
}

void print_c_backtrace(void)
{
    static const char start[] = "C BACKTRACE ------------\n";
    static const char end[] = "----------------------\n";

    void *bt[1024];
    int bt_size;
    char **bt_syms;
    int i;

    bt_size = backtrace(bt, 1024);
    bt_syms = backtrace_symbols(bt, bt_size);
    full_write(STDERR_FILENO, start, strlen(start));
    for (i = 1; i < bt_size; i++) {
        size_t len = strlen(bt_syms[i]);
        full_write(STDERR_FILENO, bt_syms[i], len);
        full_write(STDERR_FILENO, "\n", 1);
    }
    full_write(STDERR_FILENO, end, strlen(end));
    free(bt_syms);
}
/* #endif */
