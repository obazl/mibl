#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "libmibl.h"

#include "unity.h"


#if defined(DEBUG_TRACE)
extern bool debug;
extern int  debug_level;
extern bool debug_miblrc;
extern bool debug_mibl_crawl;
extern bool trace;
#endif
extern bool verbose;

s7_scheme *s7;

static const char *_mpp(s7_scheme *sc, s7_pointer obj) /* (pp obj) */
{
  return(s7_string(
          s7_eval_c_string_with_environment(sc,
            "(catch #t                         \
               (lambda ()                      \
                 (unless (defined? 'mibl-pp)        \
                   (load \"mibl_pp.scm\"))       \
                 (mibl-pp obj))                     \
               (lambda (type info)             \
                 (apply format #f info)))",
	   s7_inlet(sc, s7_list(sc, 1, s7_cons(sc, s7_make_symbol(sc, "obj"), obj))))));
}

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

UT_string *setter;
/* char *parsetree_expected = */
/* (hash-table "mibl/test/library/multiple/case010" ((:ws-path "/private/var/tmp/_bazel_gar/6675c906ec3a331e206e11211d4c18ac/sandbox/darwin-sandbox/494/execroot/mibl/bazel-out/darwin-fastbuild/bin/mibl/test/library/multiple/case010_test.runfiles/mibl") (:pkg-path "mibl/test/library/multiple/case010") (:realpath "/private/var/tmp/_bazel_gar/6675c906ec3a331e206e11211d4c18ac/sandbox/darwin-sandbox/494/execroot/mibl/bazel-out/darwin-fastbuild/bin/mibl/test/library/multiple/case010_test.runfiles/mibl/mibl/test/library/multiple/case010") (dune (library (name orderbook) (modules orderbook) (public_name ocxmr.orderbook) (libraries ocxmr.make_orderbook ocxmr.quote_include) (preprocess (pps ppx_jane ppx_sexp_conv))) (library (name make_orderbook) (modules make_orderbook orderbook_intf) (public_name ocxmr.make_orderbook) (libraries core ocxmr.price_comparator ocxmr.market_event) (preprocess (pps ppx_jane))) (library (name orderbook_test) (modules orderbook_test) (libraries ocxmr.orderbook) (inline_tests) (preprocess (pps ppx_jane)))) (:structures (:static (Make_orderbook . make_orderbook.ml) (Orderbook_intf . orderbook_intf.ml) (Orderbook_test . orderbook_test.ml))) (:modules (Orderbook (:ml . orderbook.ml) (:mli . orderbook.mli))))) */

void test_a(void) {
    log_info("test_a");
    char *rootdir;
    char *pathdir;

    /* rootdir = "mibl/test/library/multiple"; */
    /* pathdir = "mibl/test/library/multiple"; */

    /* s7_pointer pkg_tbl = load_project(getcwd(NULL,0), //rootdir, */
    /*                                   pathdir); */
    /* log_debug("%s", TO_STR(pkg_tbl)); // s7_object_to_c_string(s7, pkg_tbl)); */

    /* s7_pointer pp = s7_name_to_value(s7, "mibl-pretty-print"); */
    /* if (pp == s7_undefined(s7)) { */
    /*     log_error("unbound symbol: mibl-pretty-print"); */
    /*     log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    /* } */
    /* s7_pointer mpp = s7_name_to_value(s7, "mibl-pp"); */
    /* if (mpp == s7_undefined(s7)) { */
    /*     log_error("unbound symbol: mibl-pp"); */
    /*     log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    /* } */

    /* log_debug("printing"); */
    /* printf("aaaa\n"); */
    /* s7_pointer tbl_str = s7_call(s7, mpp, */
    /*                              pkg_tbl */
    /*                              /\* s7_list(s7, 1, pkg_tbl) *\/ */
    /*                              ); */
    /* printf("%s\n", TO_STR(tbl_str)); */
    /* printf("%s\n", _mpp(s7, pkg_tbl)); */
    /* fflush(stdout); */

    /* utstring_new(setter); */

    /* (pkg (hash-table-ref pkgs arg)) */
    /* s7_pointer ht_ref = s7_name_to_value(s7, "hash-table-ref"); */
    /* if (ht_ref == s7_undefined(s7)) { */
    /*     printf("unbound symbol: hash-table-ref"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer pkg_key = s7_make_string(s7, "dune/stanzas/library/deps/select"); */
    /* s7_pointer pkg = s7_call(s7, ht_ref, s7_list(s7, 2, pkg_tbl, pkg_key)); */
    /* printf(BGRN "pkg:" CRESET " %s\n", TO_STR(pkg)); */
    TEST_ASSERT_TRUE(true);
}

void test_b(void) {
    /* log_info("test_b"); */
    TEST_ASSERT_TRUE(true);
}

void _print_usage(void) {
    printf("Usage:\t$ bazel test <tgt> [flags, options]\n");
    printf("Flags\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t--debug-config\t\tEnable all config debugging flags.\n");
    printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");

    printf("Options:\n");
    /* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); */

    printf("\t-r, --root <arg>"
           "\tStart traversal at <arg> (path relative to cwd).\n");
    printf("\t-p, --pkg <arg>"
           "\t\tProcess only <arg> (relative root path).\n");
}

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,

    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_MIBLRC,
    FLAG_DEBUG_MIBL_CRAWL,
    FLAG_DEBUG_SCM,
    FLAG_DEBUG_SCM_LOADS,

    FLAG_EMIT_PARSETREE,        /* config load_project to emit PARSETREE.mibl */

    FLAG_TRACE,
    FLAG_VERBOSE,

    LAST
};

static struct option options[] = {
    /* 0 */
    [OPT_ROOT] = {.long_name="root",.short_name='r',
                  .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_PKG] = {.long_name="pkg",.short_name='p',
                 .flags=GOPT_ARGUMENT_REQUIRED
    },
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBLRC] = {.long_name="debug-miblrc",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBL_CRAWL] = {.long_name="debug-mibl-crawl",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

#if defined(DEBUG_TRACE)
extern bool debug;
extern bool debug_bazel;
extern bool debug_mibl;
extern bool debug_mibl_crawl;
extern bool debug_scm;
extern bool debug_loads;
extern bool debug_s7_config;
extern bool trace;
extern bool trace_bazel;
extern bool trace_mibl;
#endif

extern bool emit_parsetree;


int main(int argc, char **argv)
{
    fflush(NULL);
    argc = gopt (argv, options);
    gopt_errors (argv[0], options);

    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        log_info("verbose ct: %d", options[FLAG_VERBOSE].count);
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("debug ct: %d", options[FLAG_DEBUG].count);
        debug = true;
        debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_DEBUG_CONFIG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
           log_info("debug_config ct: %d", options[FLAG_DEBUG_CONFIG].count);
        debug_bazel = true;
        debug_mibl = true;
        debug_s7_config = true;
#else
        log_error("--debug-config only valid with -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_MIBLRC].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("debug_miblrc ct: %d", options[FLAG_DEBUG_MIBLRC].count);
        debug_miblrc = true;
#endif
    }

    if (options[FLAG_DEBUG_MIBL_CRAWL].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("debug_mibl_crawl ct: %d", options[FLAG_DEBUG_MIBL_CRAWL].count);
        debug_mibl_crawl = true;
#else
        log_error(RED "ERROR: " CRESET
                  "--debug-mibl-crawl requires -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_SCM].count) {
#if defined(DEBUG_TRACE)
        debug_scm = true;
#else
        log_warn("--debug-scm only takes effect for debug builds (-c dbg)");
#endif
    }
    if (options[FLAG_DEBUG_SCM_LOADS].count) {
#if defined(DEBUG_TRACE)
        debug_loads = true;
#else
        log_warn("--debug-scm-loads only takes effect for debug builds (-c dbg)");
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("trace ct: %d", options[FLAG_TRACE].count);
        trace = true;
        trace_bazel = true;
        trace_mibl = true;
#endif
    }

    if (options[FLAG_EMIT_PARSETREE].count) {
        if (getenv("BAZEL_TEST")) {
            /* log_warn("BAZEL_TEST: %s", getenv("BAZEL_TEST")); */
            if ( !getenv("BUILD_WORKSPACE_DIRECTORY") ) {
                fprintf(stderr,
                        RED "ERROR: " CRESET
                        "--emit-parsetree not supported under bazel test. Try 'bazel run'.\n");
                exit(EXIT_FAILURE);
            }
        }
        emit_parsetree = true;
    }

    bazel_configure();

    mibl_configure();

    s7 = s7_configure();

    s7_load(s7, "dune.scm");

    initialize_mibl_data_model(s7);

    char *rootdir;
    char *pathdir;
    rootdir = "mibl/test/library/multiple";
    pathdir = "mibl/test/library/multiple";

    s7_pointer pkg_tbl = load_project(getcwd(NULL,0), //rootdir,
                                      pathdir);
                                      /* getcwd(NULL,0)); */

    UNITY_BEGIN();
    RUN_TEST(test_a);
    RUN_TEST(test_b);
    return UNITY_END();
}
