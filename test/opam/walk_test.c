#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "log.h"
/* #include "utarray.h" */
/* #include "utstring.h" */

/* #if INTERFACE */
/* #include "utstring.h" */
/* #endif */

/* #include "opam_lexer.h" */
/* #include "opam_lex.h" */

/* #include "mibl.h" */
#include "libtreewalker.h"
#include "walk_test.h"

extern bool debug = false;
extern bool debug_findlib = false;
extern bool trace = false;
extern bool verbose = false;

char *pkg_path = NULL;

int main(int argc, char *argv[])
{
    int opt;

    char *opts = "p:hdmtv";

    char *opam_switch = NULL;

    while ((opt = getopt(argc, argv, opts)) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            /* utstring_printf(opam_file, "%s", optarg); */
            break;
        case 'd':
            debug = true;
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        case 'm':
            debug_findlib = true;
            break;
        case 'p':
            printf("PKG: %s\n", optarg);
            pkg_path = strdup(optarg);
            /* remove trailing '/' */
            int len = strlen(pkg_path);
            if (pkg_path[len-1] == '/') {
                pkg_path[len-1] = '\0';
            }
            /* validate - no abs paths, may start with '//" */
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        default:
            ;
            /* log_error("Usage: %s [-f] [opamfile]", argv[0]); */
            /* exit(EXIT_FAILURE); */
        }
    }

    bazel_configure();

    // bazel_configure does chdir to ws root

    // opam_configure must be run from root ws to account for local switches
    char *opam_lib;
    if (opam_switch)
        opam_lib = opam_configure(opam_switch);
    else
        opam_lib = opam_configure("");

    mibl_configure();

    /* char *wd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* if (wd) { */
    /*     /\* we launched from bazel workspace, cd to launch dir *\/ */
    /*     chdir(wd); */
    /* } */

    walk_tree(opam_lib, pkg_path);

    /* UT_array *result = opam_lex_file(utstring_body(opam_file)); */

    /* UT_array *result = sealark_lex_string("'hello'\n#cmt1\n"); */

    log_debug("main RESULT dump:");
    /* dump_nodes(result); */
}
