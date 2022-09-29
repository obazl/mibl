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

#include "config_opam.h"

/* const char *errmsg = NULL; */

int rc;

/* char *bazel_script_dir = NULL; */

LOCAL UT_string *opam_switch;
LOCAL UT_string *opam_bin;
LOCAL UT_string *opam_lib;

void config_opam(char *_opam_switch)
{
    /*
      1. discover switch
         a. check env var OPAMSWITCH
         b. use -s option
         c. run 'opam var switch'
      2. discover lib dir: 'opam var lib'
     */

    utstring_new(opam_switch);
    utstring_new(opam_bin);
    utstring_new(opam_lib);

    /* FIXME: handle switch arg */
    /* FIXME: argv */
    char *exe = NULL, *result = NULL;
    if (_opam_switch == NULL) {
        /* log_info("opam: using current switch"); */

        exe = "opam";
        char *argv[] = {"opam", "var", "switch",NULL};

        result = run_cmd(exe, argv);
        if (result == NULL) {
            fprintf(stderr, "FAIL: run_cmd 'opam var switch'\n");
        } else {
            utstring_printf(opam_switch, "%s", result);
        }
    } // else??

    /* cmd = "opam var bin"; */
    char *argv1[] = {"opam", "var", "bin", NULL};
    result = NULL;
    result = run_cmd(exe, argv1);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var bin'\n");
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_bin, "%s", result);

    /* cmd = "opam var lib"; */
    char *argv2[] = {"opam", "var", "lib", NULL};
    result = NULL;
    result = run_cmd(exe, argv2);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var lib'\n");
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_lib, "%s", result);

}
