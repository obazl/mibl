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

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

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

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,
    OPT_EMIT,
    OPT_EMIT_EM,            /* = OPT_EMIT_MIBL */
    OPT_EMIT_MIBL,
    OPT_NO_EMIT_MIBL,
    OPT_EMIT_EB,            /* = OPT_EMIT_BAZEL */
    OPT_EMIT_BAZEL,
    OPT_NO_EMIT_BAZEL,
    OPT_NOEMIT,
    OPT_MENHIR,
    OPT_DUMP_EXPORTS,
    OPT_DUMP_MIBL,
    OPT_DUMP_PARSETREE,
    OPT_DUMP_STARLARK,
    OPT_HELP,
    OPT_DEBUG,
    OPT_DEBUG_DE,
    OPT_DEBUG_EMIT,
    OPT_DEBUG_DX,
    OPT_DEBUG_EXECUTABLES,
    OPT_DEBUG_DM,
    OPT_DEBUG_MIBL,
    OPT_DEBUG_DPPX,
    OPT_DEBUG_PPX,
    OPT_TRACE,
    OPT_VERBOSE,
    OPT_LAST
};

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");
    printf("Flags (note that some flags require double-hyphenation):\n");
    printf("\t-d  | --debug\t\t\tEnable all debugging flags.\n");
    printf("\t--dx | --debug-executables\tDebug handling of Dune executable and executables stanzas.\n");
    printf("\t--de | --debug-emit\t\tDebug emit logic.\n");
    printf("\t--dm | --debug-mibl\t\tDebug mibl elaboration.\n");
    printf("\t--dppx | --debug-ppx\t\tDebug ppx stuff.\n");
    printf("\t--dump-exports\t\tDebug exported syms table.\n");
    printf("\t--em | --emit-mibl\t\tEmit BUILD.mibl files.\n");
    printf("\t--no-emit\t\t\tDisable emitting.\n");
    printf("\t-t  | --trace\t\t\tEnable trace flags.\n");
    printf("\t-v  | --verbose\t\t\tEnable verbosity. Repeatable.\n");
    printf("\t--menhir\t\t\tEmit 'menhir' targets for .mly files, instead of ocamlyacc.\n");

    printf("Options:\n");
    printf("\t-D | -dump <arg>\t\tDump <arg> (parsetree, mibl, or starlark}) to stdout.\n");
    printf("\t-e | --emit <arg>\t\tEmit BUILD.<arg> files, where <arg> = mibl | bazel. BUILD.bazel always emitted unless --no-emit passed.\n");

    printf("\t-p | --pkg | --package <arg>"
           "\tRestrict dump ouput to <arg> (relative pkg path).\n");

}

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
