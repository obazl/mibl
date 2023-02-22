//FIXME: support -j (--jsoo-enable) flag

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

/* #include "log.h" */
/* #include "utarray.h" */
/* #include "utstring.h" */

/* #if INTERFACE */
/* #include "utstring.h" */
/* #endif */

/* #include "opam_lexer.h" */
/* #include "opam_lex.h" */

/* #include "mibl.h" */
/* #include "libtreewalker.h" */

#include "coswitch.h"


#if defined(DEBUG_TRACE)
extern bool debug;
extern bool debug_findlib;
extern bool trace;
#endif
extern bool verbose;

char *pkg_path = NULL;

UT_array *opam_include_pkgs;
UT_array *opam_exclude_pkgs;

int main(int argc, char *argv[])
{
    int opt;

    extern bool enable_jsoo;

    char *opts = "jp:hdmtvx:";

    char *opam_switch = NULL;

    utarray_new(opam_include_pkgs,&ut_str_icd);
    utarray_new(opam_exclude_pkgs,&ut_str_icd);

    while ((opt = getopt(argc, argv, opts)) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            /* utstring_printf(opam_file, "%s", optarg); */
            break;
        case 'd':
#if defined(DEBUG_TRACE)
            debug = true;
#endif
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        case 'j':
            enable_jsoo = true;
            break;
        case 'm':
#if defined(DEBUG_TRACE)
            debug_findlib = true;
#endif
            break;
        case 'p':               /* pkg name, not path */
            printf("PKG: %s\n", optarg);
            utarray_push_back(opam_include_pkgs, &optarg);
            /* pkg_path = strdup(optarg); */
            /* /\* remove trailing '/' *\/ */
            /* int len = strlen(pkg_path); */
            /* if (pkg_path[len-1] == '/') { */
            /*     pkg_path[len-1] = '\0'; */
            /* } */
            /* validate - no abs paths, may start with '//" */
            break;
        case 't':
#if defined(DEBUG_TRACE)
            trace = true;
#endif
            break;
        case 'v':
            verbose = true;
            break;
        case 'x':
            printf("EXCL %s\n", optarg);
            utarray_push_back(opam_exclude_pkgs, &optarg);
            break;
        default:
            ;
            log_error("Usage: %s [-f] [opamfile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    bazel_configure();

    chdir(bws_root);            /* always run from base ws root */

    // opam_configure must be run from root ws to account for local switches
    // sets global opam_switch_* vars
    if (opam_switch)
        opam_configure(opam_switch);
    else
        opam_configure("");

    mibl_configure();

    mibl_s7_init();

    /* char *wd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* if (wd) { */
    /*     /\* we launched from bazel workspace, cd to launch dir *\/ */
    /*     chdir(wd); */
    /* } */

    /* walk_tree(opam_lib, pkg_path); */

    convert_findlib_pkgs(opam_include_pkgs, opam_exclude_pkgs);

    /* UT_array *result = opam_lex_file(utstring_body(opam_file)); */

    /* UT_array *result = sealark_lex_string("'hello'\n#cmt1\n"); */

    /* FIXME: free opam_include_pkgs, opam_exclude_pkgs */

#if defined(DEBUG_TRACE)
    log_debug("exiting coswitch");
#endif
    /* dump_nodes(result); */
}
