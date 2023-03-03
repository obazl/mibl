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
#include "utarray.h"
#include "utstring.h"

/* #if INTERFACE */
/* #include "utstring.h" */
/* #endif */

/* #include "opam_lexer.h" */
/* #include "opam_lex.h" */

/* #include "mibl.h" */
/* #include "libtreewalker.h" */

#include "s7.h"

#include "convert.h"

#if defined(DEBUG_TRACE)
extern bool debug;
extern bool debug_findlib;
extern bool trace;
#endif

extern bool verbose;

char *pkg_path = NULL;

UT_array *pkg_paths;

int main(int argc, char *argv[])
{
    int opt;

    char *opts = "p:hdmtv";

    char *opam_switch = NULL;

    utarray_new(pkg_paths,&ut_str_icd);

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
        case 'm':
#if defined(DEBUG_TRACE)
            debug_findlib = true;
#endif
            break;
        case 'p':               /* pkg name, not path */
            printf("PKG: %s\n", optarg);
            utarray_push_back(pkg_paths, &optarg);
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
        default:
            ;
            /* log_error("Usage: %s [-f] [opamfile]", argv[0]); */
            /* exit(EXIT_FAILURE); */
        }
    }

    /* config in this order: first bazel, then mibl, then s7 */

    bazel_configure();

    /* if (opam_switch) */
    /*     opam_configure(opam_switch); */
    /* else */
    /*     opam_configure(""); */

    mibl_configure();

    s7_scheme *s7 = s7_configure();

    /* if (exit_on_error) {        /\* FIXME *\/ */
    /*     s7_define_variable(s7, "*exit-on-error*", s7_t(s7)); */
    /* } else { */
    /*     s7_define_variable(s7, "*exit-on-error*", s7_f(s7)); */
    /* } */

    /* printf("*exit-on-error*? %d\n", */
    /*        (s7_t(s7) == s7_name_to_value(s7, "*exit-on-error*"))); */

    s7_load(s7, "dune.scm");

    convert_dune_project(pkg_paths);

    /* convert_findlib_pkgs(opam_pkgs); */

    /* UT_array *result = opam_lex_file(utstring_body(opam_file)); */

    /* UT_array *result = sealark_lex_string("'hello'\n#cmt1\n"); */

    log_debug("exiting mibl convert");
    /* dump_nodes(result); */
}
