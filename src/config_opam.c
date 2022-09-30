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
#include "utarray.h"
#include "utstring.h"
#endif

#include "config_opam.h"

/* const char *errmsg = NULL; */

int rc;

/* char *bazel_script_dir = NULL; */

UT_string *opam_switch;
UT_string *opam_bin;
UT_string *opam_lib = NULL;

EXPORT char *opam_configure(char *_opam_switch)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_trace("opam_configure: '%s'", _opam_switch);
    log_trace("cwd: %s\n", getcwd(NULL, 0));
#endif

    /*
      if _opam_switch emtpy, discover current switch:
          - check for local switch ('_opam' subdir of root dir)
         - check env var OPAMSWITCH
         - else run 'opam var switch'
      2. discover lib dir: 'opam var lib'
     */

    utstring_new(opam_switch);
    utstring_new(opam_bin);
    utstring_new(opam_lib);

    /* FIXME: handle switch arg */
    /* FIXME: argv */
    char *exe = NULL, *result = NULL;
    if (strlen(_opam_switch) == 0) {
        log_info("opam: using current switch");

        exe = "opam";
        /* char *argv[] = {"opam", "var", "switch",NULL}; */
        char *argv[] = {"opam", "var", "ocaml:version", NULL};

        result = run_cmd(exe, argv);
        if (result == NULL) {
            fprintf(stderr, "FAIL: run_cmd 'opam var switch'\n");
        } else {
            utstring_printf(opam_switch, "%s", result);
#if defined(DEBUG_TRACE)
            log_debug("cmd result: '%s'", utstring_body(opam_switch));
#endif
        }
    } // else??

    /* cmd = "opam var bin"; */
    char *argv1[] = {"opam", "var", "bin", NULL};
    result = NULL;
    result = run_cmd(exe, argv1);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var bin'\n");
        exit(EXIT_FAILURE);
    } else {
        utstring_printf(opam_bin, "%s", result);
#if defined(DEBUG_TRACE)
        log_debug("cmd result: '%s'", utstring_body(opam_bin));
#endif
    }
    /* cmd = "opam var lib"; */
    char *argv2[] = {"opam", "var", "lib", NULL};
    result = NULL;
    result = run_cmd(exe, argv2);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd 'opam var lib'\n");
        exit(EXIT_FAILURE);
    } else {
        utstring_printf(opam_lib, "%s", result);
#if defined(DEBUG_TRACE)
        log_debug("cmd result: '%s'", utstring_body(opam_lib));
#endif
    }
    return utstring_body(opam_lib);
}

