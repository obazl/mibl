#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <fts.h>
#include <glob.h>
#include <libgen.h>
#ifdef __linux__
#include <linux/limits.h>
#include <sys/wait.h>           /* waitpid */
#else
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

/* #include "ini.h" */
#include "log.h"
/* #if EXPORT_INTERFACE */
/* #include "utarray.h" */
#include "utstring.h"
/* #endif */

#include "cmd_runner.h"

extern bool mibl_debug_deps;

#if INTERFACE
#define BUFSZ 4096 * 4
#endif

char buffer[BUFSZ];

EXPORT char * run_cmd(char *executable, char **argv)
{
#if defined(DEBUG_TRACE)
    char **ptr = argv;
    UT_string *tmp;
    utstring_new(tmp);
    while (*ptr) {
        utstring_printf(tmp, "%s ", *ptr);
        ptr++;
    }
#if defined(DEBUG_TRACE)
    if (mibl_debug_deps) {
        log_debug("run cmd: %s", utstring_body(tmp));
        char **p = argv;
        while(*p != NULL) {
            log_debug("arg: %s", *p);
            p++;
        }
    }
#endif
    utstring_free(tmp);
#endif

    pid_t pid;
    int rc;

    extern char **environ;

    /* FIXME: write stderr to log instead of dev/null? */
    /* int DEVNULL_FILENO = open("/dev/null", O_WRONLY); */

    int cout_pipe[2];
    int cerr_pipe[2];

    if(pipe(cout_pipe) || pipe(cerr_pipe)) {
        log_error("pipe returned an error.");
        exit(EXIT_FAILURE);
    }

    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);

    /* child inherits open FDs, so: */
    /* close read end of pipes on child */
    posix_spawn_file_actions_addclose(&action, cout_pipe[0]);
    posix_spawn_file_actions_addclose(&action, cerr_pipe[0]);

    /* dup write-ends on child-side, connect stdout/stderr */
    posix_spawn_file_actions_adddup2(&action, cout_pipe[1],
                                     STDOUT_FILENO);
    posix_spawn_file_actions_adddup2(&action, cerr_pipe[1],
                                     STDERR_FILENO);

    /* close write end on child side */
    posix_spawn_file_actions_addclose(&action, cout_pipe[1]);
    posix_spawn_file_actions_addclose(&action, cerr_pipe[1]);

    /* now child will not inherit open pipes (fds?), but its
       stdout/stderr FDs will be connected to the write ends of our
       pipes.
     */
    /* posix_spawn_file_actions_addopen(&action, */
    /*                                  STDOUT_FILENO, */
    /*                                  codept_deps_file, */
    /*                                   O_WRONLY | O_CREAT | O_TRUNC, */
    /*                                   S_IRUSR | S_IWUSR | S_IRGRP ); */

    /* if ((rc = posix_spawn_file_actions_adddup2(&action, */
    /*                                            DEVNULL_FILENO, */
    /*                                            STDERR_FILENO))) { */
    /*     perror("posix_spawn_file_actions_adddup2"); */
    /*     posix_spawn_file_actions_destroy(&action); */
    /*     exit(rc); */
    /* } */

    // FIXME: get absolute path of codept
    // FIXME: restrict environ

    /* log_debug("spawning %s", executable); */
    errno = 0;
    rc = posix_spawnp(&pid, executable, &action, NULL, argv, environ);

    if (rc != 0) {
        /* does not set errno */
        log_fatal("run_command posix_spawn error rc: %d, %s",
                  rc, strerror(rc));
        //exit(EXIT_FAILURE);
        return NULL;
    }

    int bytes_read;

    int status;
    int waitrc = waitpid(pid, &status, 0);
    if (waitrc == -1) {
        perror("spawn_cmd waitpid error");
        log_error("spawn_cmd");
        posix_spawn_file_actions_destroy(&action);
        //FIXME: close pipes?
        return NULL;
    }
#if defined(DEBUG_TRACE)
    if (mibl_debug_deps)
        log_trace("waitpid for pid %d returned %d", pid, waitrc);
#endif
    /* if (waitrc == 0) { */
    // child exit OK
    if ( WIFEXITED(status) ) {
        // terminated normally by a call to _exit(2) or exit(3).
#if defined(DEBUG_TRACE)
        if (mibl_debug_deps) {
            log_trace("status: %d", status);
            log_trace("WIFEXITED(status): %d", WIFEXITED(status));
            log_trace("WEXITSTATUS(status): %d", WEXITSTATUS(status));
        }
#endif
        /* now close the write end on parent side */
        close(cout_pipe[1]);
        close(cerr_pipe[1]);

        if (status) {
            /* process exited normally but returned non-zero rc */
            log_debug("reading cerr_pipe");
            bytes_read = read(cerr_pipe[0], &buffer[0], BUFSZ);
            log_debug("readed %d bytes from cerr_pipe", bytes_read);
            if (bytes_read > 0) {
                fprintf(stdout, "Read message: %s", buffer);
            }
            close(cout_pipe[1]);
            close(cerr_pipe[1]);
            posix_spawn_file_actions_destroy(&action);
            return NULL; //exit(EXIT_FAILURE);
        }

        /* process exited normally, returning rc 0 */
        /* read the stdout pipe, ignore stderr pipe */

        /* https://github.com/pixley/InvestigativeProgramming/blob/114b698339fb0243f50cf5bfbe5d5a701733a125/test_spawn_pipe.cpp */

        /* make sure pipe is ready */
        struct timespec timeout = {5, 0};

        fd_set read_set;
        memset(&read_set, 0, sizeof(read_set));
        FD_SET(cout_pipe[0], &read_set);
        FD_SET(cerr_pipe[0], &read_set);

        int larger_fd = (cout_pipe[0] > cerr_pipe[0])
            ? cout_pipe[0]
            : cerr_pipe[0];

        rc = pselect(larger_fd + 1, &read_set, NULL, NULL, &timeout, NULL);
        //thread blocks until either packet is received or the timeout goes through
        if (rc == 0) {
            fprintf(stderr, "pselect timed out.\n");
            /* return 1; */
            close(cout_pipe[1]);
            close(cerr_pipe[1]);
            posix_spawn_file_actions_destroy(&action);
            return NULL; //exit(EXIT_FAILURE);
        }

        /* log_debug("reading cout_pipe"); */
        bytes_read = read(cout_pipe[0], &buffer[0], BUFSZ);
#if defined(DEBUG_TRACE)
        if (mibl_debug_deps)
            log_debug("outpipe bytes_read: %d", bytes_read);
#endif
        if (bytes_read > 0){
            /* drop trailing newline */
            if (buffer[bytes_read - 1] == '\n')
                buffer[bytes_read - 1] = '\0';
            bytes_read = read(cout_pipe[0], &buffer[0], BUFSZ);
            if (bytes_read > 0) {
                log_error("cmd runner buffer too small! please file an issue.");
                close(cout_pipe[1]);
                close(cerr_pipe[1]);
                posix_spawn_file_actions_destroy(&action);
                return NULL;
            }
        }

        close(cout_pipe[1]);
        close(cerr_pipe[1]);
        posix_spawn_file_actions_destroy(&action);
        return buffer;
    }
    else if (WIFSIGNALED(rc)) {
        // terminated due to receipt of a signal
        log_error("WIFSIGNALED(rc)");
        log_error("WTERMSIG: %d", WTERMSIG(rc));
#ifdef WCOREDUMP
        log_error("WCOREDUMP?: %d", WCOREDUMP(rc));
#endif
        close(cout_pipe[1]);
        close(cerr_pipe[1]);
        posix_spawn_file_actions_destroy(&action);
        return NULL;
    } else if (WIFSTOPPED(rc)) {
        /* process has not terminated, but has stopped and can
           be restarted. This macro can be true only if the
           wait call specified the WUNTRACED option or if the
           child process is being traced (see ptrace(2)). */
        log_error("WIFSTOPPED(rc)");
        log_error("WSTOPSIG: %d", WSTOPSIG(rc));
        close(cout_pipe[1]);
        close(cerr_pipe[1]);
        posix_spawn_file_actions_destroy(&action);
        return NULL;
    }
    /* fall-through should not happen */
    log_error("%s:%d Fall-through - should not happen!",
              __FILE__, __LINE__);
    return NULL;
}
