/* e.g. (modules (:standard \ foo)) etc. */

#include <libgen.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>

#include "gopt.h"
#include "liblogc.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "mibl.h"

#include "unity.h"


#if defined(TRACING)
/* extern bool debug; */
extern bool mibl_debug_bazel;
extern int  mibl_debug_level;
extern bool mibl_debug_mibl;
extern bool mibl_debug_miblrc;
extern bool mibl_debug_scm;
extern bool mibl_debug_s7_config;
extern bool mibl_debug_traversal;
extern bool mibl_trace;
extern bool mibl_trace_bazel;
/* extern bool trace_mibl; */
#endif

extern UT_string *mibl_runfiles_root;
extern bool verbose;

s7_scheme *s7;

s7_pointer mibl_project;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

UT_string *setter;

static __attribute__((unused)) void x_pp_mibl_project(void)
{// the hard way to do it:
    s7_pointer mpp = s7_name_to_value(s7, "mibl-pretty-print");
    if (mpp == s7_undefined(s7)) {
        log_error("unbound symbol: mibl-pretty-print");
        /* log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    }
    s7_pointer mproj = s7_name_to_value(s7, "*mibl-project*");
    if (mproj == s7_undefined(s7)) {
        log_error("unbound symbol: *mibl-project*");
        /* log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    }

    /* log_debug("printing"); */
    /* printf("before\n"); */
    s7_pointer tbl_str = s7_apply_function(s7, mpp,
                                           s7_list(s7, 1, mproj)
                                           );
    (void)tbl_str;

    s7_newline(s7, s7_current_output_port(s7));
    /* printf("after\n"); */
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* printf("%s\n", TO_STR(tbl_str)); */
    /* printf("%s\n", _mpp(s7, pkg_tbl)); */
    fflush(stdout);
}

static __attribute__((unused)) void _pp_mibl_project(void)
{// the easy way:

    s7_pointer res = s7_eval_c_string(s7,
                                      "(mibl-pretty-print *mibl-project*)");
    (void)res;
    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* printf("%s\n", TO_STR(tbl_str)); */
    /* printf("%s\n", _mpp(s7, pkg_tbl)); */
    fflush(stdout);
}

static __attribute__((unused)) void _pp_mibl_expected(void)
{
    s7_pointer res = s7_eval_c_string(s7,
                                      "(mibl-pretty-print *mibl-expected*)");
    (void)res;
    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));
    fflush(stdout);
}

void test_a(void) {
    log_info("test_a");

    s7_pointer expected = s7_eval_c_string(s7,
        "'(:opts (:standard) (:flags \"-nopervasives\" -nostdlib))"
                                      );

    char *sexp =
        "(normalize-stanza-fld-flags "
        " '(flags (:standard -nopervasives -nostdlib)) :common)"
        ;

    s7_pointer res = s7_eval_c_string(s7, sexp);
    /* char *s = s7_object_to_c_string(s7, res); */
    /* log_debug("SUM: %s", s); */
    /* free(s); */
    /* (void)res; */
    /* s7_newline(s7, s7_current_output_port(s7)); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /* fflush(stdout); */

    TEST_ASSERT_TRUE(s7_is_equal(s7, res, expected));
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

extern bool emit_parsetree;

/* init_mibl_proj - read initial and expected data files */
void _init_mibl_proj(char *test_root)
{
    log_debug("CWD: %s", getcwd(NULL, 0));
    log_debug("TEST ROOT: %s", test_root);
    log_debug("BAZEL_TEST: %s", getenv("BAZEL_TEST"));
    log_debug("TEST_TARGET: %s", getenv("TEST_TARGET"));
    log_debug("TEST_SRCDIR: %s", getenv("TEST_SRCDIR"));

    /* NB: this pgm is designed as a cc_binary target that is run by a
       sh_test target. Therefore BAZEL_TEST, TEST_TARGET, etc. should
       be defined. We derive the relative path for runfiles
       PARSETREE.s7 and PROJECT.s7 from TEST_TARGET.*/
    /* TEST_TARGET is defined by bazel for test targets. */
    char *test_target = strndup(getenv("TEST_TARGET"), 1024);
    /* test_target alway has form '//foo/bar:baz' ??? */
    char *colon = strrchr(test_target, ':');
    *colon = '\0';

    log_debug("TEST_PKG: %s", test_target);

    UT_string *pkg_path;
    utstring_new(pkg_path);
    utstring_printf(pkg_path, "%s", test_target + 2); // drop leading //
    free(test_target);

    log_debug("PKG_PATH: %s", utstring_body(pkg_path));

    UT_string *pgm;
    utstring_new(pgm);

    utstring_printf(pgm,
                    "(define *mibl-project* "
                    "  (call-with-input-file \"%s/PARSETREE.s7\" "
                    "    (lambda (p) "
                    "      (let* ((x (read p)) "
                    "             (y (eval (read (open-input-string x))))) "
                    "          y))))",
                    utstring_body(pkg_path));

    s7_pointer res = s7_eval_c_string(s7, utstring_body(pgm));
    (void)res;
    /* char *s = TO_STR(res); */
    /* log_debug("initial: %s", s); */
    /* free(s); */

    /* .mibl/EXPECTED.s7 generated by */
    /* bazel run @mibl//script:convert -- -w <pkgpath> --emit-result --emit-s7  */
    utstring_renew(pgm);
    utstring_printf(pgm,
                    "(define *mibl-expected* "
                    "  (call-with-input-file \"%s/PROJECT.s7\" "
                    "    (lambda (p) "
                    "      (let* ((x (read p)) "
                    "             (y (eval (read (open-input-string x))))) "
                    "          y))))",
                    utstring_body(pkg_path));

    res = s7_eval_c_string(s7, utstring_body(pgm));
    /* s = TO_STR(res); */
    /* log_debug("DBG: expected: %s", s); */
    /* free(s); */
}

void _set_options(struct option options[])
{
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
#if defined(TRACING)
        if (verbose)
            log_info("debug ct: %d", options[FLAG_DEBUG].count);
        /* debug = true; */
        mibl_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_DEBUG_CONFIG].count) {
#if defined(TRACING)
        if (verbose)
           log_info("debug_config ct: %d", options[FLAG_DEBUG_CONFIG].count);
        mibl_debug_bazel = true;
        /* debug_mibl = true; */
        mibl_debug_s7_config = true;
#else
        log_error("--debug-config only valid with -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_MIBLRC].count) {
#if defined(TRACING)
        if (verbose) log_info("debug_miblrc ct: %d", options[FLAG_DEBUG_MIBLRC].count);
        mibl_debug_miblrc = true;
#endif
    }

    if (options[FLAG_DEBUG_MIBL_CRAWL].count) {
#if defined(TRACING)
        if (verbose) log_info("mibl_debug_traversal ct: %d", options[FLAG_DEBUG_MIBL_CRAWL].count);
        mibl_debug_traversal = true;
#else
        log_error(RED "ERROR: " CRESET
                  "--debug-mibl-crawl requires -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_SCM].count) {
#if defined(TRACING)
        mibl_debug_scm = true;
#else
        log_warn("--debug-scm only takes effect for debug builds (-c dbg)");
#endif
    }
    if (options[FLAG_DEBUG_SCM_LOADS].count) {
        mibl_s7_set_flag("*mibl-debug-s7-loads*", true);
    }

    if (options[FLAG_TRACE].count) {
#if defined(TRACING)
        if (verbose)
            log_info("trace ct: %d", options[FLAG_TRACE].count);
        mibl_trace = true;
        mibl_trace_bazel = true;
        /* trace_mibl = true; */
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
}

int main(int argc, char **argv)
{
    log_debug("ARGV[0]: %s", argv[0]);
    log_debug("CWD: %s", getcwd(NULL, 0));
    log_debug("BWSD: %s", getenv("BUILD_WORKSPACE_DIRECTORY"));
    log_debug("BWD: %s", getenv("BUILD_WORKING_DIRECTORY"));
    log_debug("TESTTGT: %s", getenv("TEST_TARGET"));
    log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR"));

    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    _set_options(options);
    /* **************************************************************** */
    /* In test env:
       BAZEL_TEST 1
       TEST_TARGET - label of this target, relative to BUILD_WS_DIRECTORY
       BUILD_WORKSPACE_DIRECTORY - NULL
       BUILD_WORKING_DIRECTORY - NULL
       RUNFILES_DIR - set

       task: config s7 with runfiles, then chdir to
               BUILD_WORKSPACE_DIRECTORY/TEST_TARGET pkg

       alternative: put the project tree in runfiles. the drawback is
       that the paths would be relative to the launch dir, not the dir
       containing the test target. but we want that dir to be like a ws.

       alternative: argv[0] is the test executable in the test case
       dir. take its dirname. or use TEST_SRCDIR which is dirname(argv[0])

       But according the the Test Encyclopedia, we should not change
       directory, but use the runfiles (TEST_SRCDIR, TEST_WORKSPACE,
       etc.)

       https://bazel.build/reference/test-encyclopedia#initial-conditions:

       "The test runner must invoke each test with the path to the
       test executable in argv[0]. This path must be relative and
       beneath the test's current directory (which is in the runfiles
       tree, see below). The test runner should not pass any other
       arguments to a test unless the user explicitly requests it."

       "The initial working directory shall be
       $TEST_SRCDIR/$TEST_WORKSPACE."

       "File descriptors 1 (stdout) and 2 (stderr) shall be open for
       writing, but what they are attached to is unspecified. It could
       be a terminal, a pipe, a regular file, or anything else to
       which characters can be written."

       "Tests must not assume that any constant path is available for
       their exclusive use."

       "Tests should create files only within the directories
       specified by $TEST_TMPDIR and $TEST_UNDECLARED_OUTPUTS_DIR (if
       set). These directories will be initially empty. Tests must not
       attempt to remove, chmod, or otherwise alter these directories."

       "Tests must access inputs through the runfiles mechanism, or
       other parts of the execution environment which are specifically
       intended to make input files available."

       "Tests must not access other outputs of the build system at
       paths inferred from the location of their own executable."

       "Tests should avoid using paths containing .. components within
       the runfiles tree."

       "No directory, file, or symlink within the runfiles tree
       (including paths which traverse symlinks) should be writable.
       (It follows that the initial working directory should not be
       writable.) Tests must not assume that any part of the runfiles
       is writable, or owned by the current user (for example, chmod
       and chgrp may fail)."

       "The runfiles tree (including paths which traverse symlinks)
       must not change during test execution. Parent directories and
       filesystem mounts must not change in any way which affects the
       result of resolving a path within the runfiles tree."

     */

    utstring_new(mibl_runfiles_root);
    char *rfr = getenv("RUNFILES_DIR");
    log_debug("RFR: %s", rfr);
    if (rfr)
        utstring_printf(mibl_runfiles_root, "%s", rfr);
    else
        utstring_printf(mibl_runfiles_root, "%s", getcwd(NULL, 0));

    mibl_s7_init();

    mibl_s7_init2(NULL, // options[OPT_MAIN].argument,
                  NULL); //options[OPT_WS].argument);

    /* struct mibl_config_s *mibl_config = mibl_s7_init(NULL, /\* script dir *\/ */
    /*                                                  NULL); /\* ws *\/ */
    /* (void)mibl_config; */
    /* we do not call mibl_s7_run - that's for running a -main script.
       here we run our script fragments in the test cases. iow we
       drive the processing from this c program instead of from a
       script with -main.
     */

    /* log_info("cwd: %s", getcwd(NULL, 0)); */
    /* show_bazel_config(); */
    /* show_mibl_config(); */
    /* show_s7_config(); */
    /* log_info("arg0: %s", argv[0]); */

    char *test_pgm = strdup(argv[0]); // free
    errno = 0;
    char *test_root = dirname(test_pgm);
    if (test_root == NULL) {
        perror(test_pgm);
    }

    /* read in PARSETREE.s7 and EXPECTED.s7 */
    /* _init_mibl_proj(test_root); */

    UNITY_BEGIN();
    fprintf(stderr, "xxxxxxxxxxxxxxxx\n");
    RUN_TEST(test_a);
    /* RUN_TEST(test_b); */
    return UNITY_END();
}
