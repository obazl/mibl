#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <libgen.h>
#ifdef __linux__
#include <linux/limits.h>
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

#include "ini.h"
#include "log.h"
#if EXPORT_INTERFACE
#include "s7.h"
#include "utarray.h"
#include "utstring.h"
#endif

#include "utils.h"

int strsort(const void *_a, const void *_b)
{
    const char *a = *(const char* const *)_a;
    const char *b = *(const char* const *)_b;
    return strcmp(a,b);
}

EXPORT char * run_cmd(char *executable, char **argv)
{
    log_debug("run_cmd %s", argv[0]);

    pid_t pid;
    /* char *argv[] = { */
    /*     "codept", */
    /*     "-args", codept_args_file, */
    /*     NULL}; */
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

    /* now child will not inherit open pipes, but its stdout/stderr
       FDs will be connected to the write ends of the pipe.
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
    rc = posix_spawnp(&pid, executable, &action, NULL, argv, environ);

    if (rc != 0) {
        /* does not set errno */
        log_fatal("run_command posix_spawn error rc: %d, %s",
                  rc, strerror(rc));
        exit(EXIT_FAILURE);
    }

    /* now close the write end on parent side */
    close(cout_pipe[1]);
    close(cerr_pipe[1]);

    /* https://github.com/pixley/InvestigativeProgramming/blob/114b698339fb0243f50cf5bfbe5d5a701733a125/test_spawn_pipe.cpp */

    // Read from pipes
    static char buffer[1024] = "";
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
        exit(EXIT_FAILURE);
    }

    int bytes_read = read(cerr_pipe[0], &buffer[0], 1024);
    if (bytes_read > 0) {
        /* fprintf(stdout, "Read message: %s", buffer); */
    }

    bytes_read = read(cout_pipe[0], &buffer[0], 1024);
    if (bytes_read > 0){
        //std::cout << "Read in " << bytes_read << " bytes from cout_pipe." << std::endl;
        buffer[bytes_read] = '\0';
        fprintf(stdout, "%s\n",
                //bytes_read,
                buffer);
    }

    waitpid(pid, &rc, 0);
    if (rc) {
        log_error("run_command rc: %d", rc);
        posix_spawn_file_actions_destroy(&action);
        exit(EXIT_FAILURE);
    }

    /* fprintf(stdout,  "exit code: %d\n", rc); */

    posix_spawn_file_actions_destroy(&action);
    return buffer;
}
