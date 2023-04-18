#include <unistd.h>

#include "gopt.h"
#include "ini.h"
#include "log.h"
#include "s7.h"

/* libmibl.a public header */
#include "libmibl.h"

/* #include "xen_repl.h" */

#include "mibl.h"

#if defined(DEBUG_TRACE)
extern bool mibl_debug;
extern bool mibl_debug_deps;
extern bool mibl_debug_traversal;
extern bool mibl_trace;
#endif

extern bool mibl_show_deps;
extern bool mibl_show_raw_deps;
extern bool mibl_show_traversal;
extern bool verbose;

extern UT_string *mibl_runfiles_root;

/* extern char *ews_root; */
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

extern struct mibl_config_s mibl_config;

#define DEV_MODE

enum OPTS {
    OPT_HALT_AFTER,
    OPT_MAIN,
    OPT_WS,
    OPT_FLAGS, /* ad-hoc flags; if not passed, (if *mibl-foo*...) fails */

    FLAG_HELP,
    FLAG_MENHIR,                /* treat .mly files as menhir */
    FLAG_SHOW_DEPS,
    FLAG_SHOW_RAW_DEPS,
    FLAG_SHOW_CONFIG,
    FLAG_SHOW_PARSETREE,
    FLAG_SHOW_MIBL,
    FLAG_SHOW_PKG,
    FLAG_SHOW_TRAVERSAL,
    FLAG_DEBUG,
    FLAG_REPORT_PARSETREE,
    FLAG_DEBUG_S7_LOADS,
#if defined(DEV_MODE)
    FLAG_DEBUG_DEPS,
    FLAG_DEBUG_EMIT,
    FLAG_DEBUG_PPX,
    FLAG_DEBUG_S7,
    FLAG_DEBUG_S7_ALL,
    FLAG_DEBUG_TRAVERSAL,
#endif

    FLAG_DEV_MODE,

    FLAG_CLEAN,
    FLAG_CLEAN_MIBL,
    FLAG_CLEAN_S7,

    FLAG_EMIT_PKGS,
    FLAG_EMIT_WSS,
    FLAG_EMIT_PARSETREE,
    FLAG_EMIT_PROJECT,
    // formats:
    FLAG_EMIT_MIBL,
    FLAG_EMIT_S7,

    FLAG_NO_MIBLRC,

    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_VERSION,
    LAST
};

int _update_mibl_config(struct option options[])
                        /* struct mibl_config_s *mibl_config) */
{
    /* log_debug("_update_mibl_config"); */

    if (options[FLAG_NO_MIBLRC].count > 0) {
        if (verbose && verbosity > 1)
            log_info("miblrc processing disabled");
        mibl_config.load_miblrc = false;
    }

    if (options[FLAG_EMIT_PARSETREE].count)
        mibl_config.emit_parsetree = true;

    if (options[OPT_HALT_AFTER].count) {
        if ((strlen(options[OPT_HALT_AFTER].argument) == 9)
            && (strncmp("parsetree", options[OPT_HALT_AFTER].argument, 9) == 0))
            mibl_config.halt_after_parsetree = true;
        else {
            log_error("Unrecognized halt-after: %s", options[OPT_HALT_AFTER].argument);
            //FIXME: cleanup?
            exit(EXIT_FAILURE);
        }
    }

    if (options[FLAG_REPORT_PARSETREE].count) {
        mibl_config.emit_parsetree = true;
        /* mibl_s7_set_flag("*mibl-report-parsetree*", true); */
    }

    return 0;                   /* success */
}

void _update_s7_globals(struct option options[])
{
    if (options[FLAG_QUIET].count)
        mibl_s7_set_flag("*mibl-quiet*", true);

    if (options[FLAG_VERBOSE].count)
        mibl_s7_set_flag("*mibl-verbose*", true);

    if (options[FLAG_MENHIR].count)
        mibl_s7_set_flag("*mibl-menhir*", true);
    else
        mibl_s7_set_flag("*mibl-menhir*", false);

    if (options[FLAG_DEBUG_PPX].count)
        mibl_s7_set_flag("*mibl-debug-ppx*", true);

    if (options[FLAG_DEBUG_EMIT].count)
        mibl_s7_set_flag("*mibl-debug-emit*", true);

    if (options[FLAG_DEBUG_S7].count)
        mibl_s7_set_flag("*mibl-debug-s7*", true);

    if (options[FLAG_DEBUG_S7_ALL].count)
        mibl_s7_set_flag("*mibl-debug-all*", true);

    if (options[FLAG_DEBUG_S7_LOADS].count)
        mibl_s7_set_flag("*mibl-debug-s7-loads*", true);

    if (options[FLAG_DEV_MODE].count)
        mibl_s7_set_flag("*mibl-dev-mode*", true);

    if (options[FLAG_SHOW_CONFIG].count)
        mibl_s7_set_flag("*mibl-show-config*", true);

    if (options[FLAG_SHOW_MIBL].count)
        mibl_s7_set_flag("*mibl-show-mibl*", true);

    if (options[FLAG_SHOW_PKG].count) {
        //FIXME: pkg is relative to ws; this prints pkg in root ws
        s7_define_variable(s7, "*mibl-show-pkg*",
                       s7_make_string(s7, options[FLAG_SHOW_PKG].argument));
    } else
        mibl_s7_set_flag("*mibl-show-pkg*", false);

    if (options[FLAG_SHOW_PARSETREE].count)
        mibl_s7_set_flag("*mibl-show-parsetree*", true);
    else if (mibl_config.show_parsetree)
        mibl_s7_set_flag("*mibl-show-parsetree*", true);

    if (options[FLAG_EMIT_MIBL].count)
        mibl_s7_set_flag("*mibl-emit-mibl*", true);

    if (options[FLAG_EMIT_S7].count)
        mibl_s7_set_flag("*mibl-emit-s7*", true);

    if (options[FLAG_EMIT_WSS].count)
        mibl_s7_set_flag("*mibl-emit-wss*", true);

    if (options[FLAG_EMIT_PROJECT].count)
        mibl_s7_set_flag("*mibl-emit-project*", true);

    if (options[FLAG_EMIT_PKGS].count)
        mibl_s7_set_flag("*mibl-emit-pkgs*", true);

    if (options[FLAG_CLEAN].count)
        mibl_s7_set_flag("*mibl-clean-all*", true);
    if (options[FLAG_CLEAN_MIBL].count)
        mibl_s7_set_flag("*mibl-clean-mibl*", true);
    if (options[FLAG_CLEAN_S7].count)
        mibl_s7_set_flag("*mibl-clean-s7*", true);

    /* --flags sets vars defined above, may be used to define ad-hoc
       flags (where the scm src will not permanently refer to the
       flag, e.g. (if *mibl-foo*)
     */
    if (options[OPT_FLAGS].count > 0) {
        UT_string *flag;
        utstring_new(flag);

        char *token, *sep = ",";
        log_debug("--flags arg: %s", options[OPT_FLAGS].argument);
        token = strtok((char*)options[OPT_FLAGS].argument, sep);
        log_debug("--flags tok 1: %s", token);
        while( token != NULL ) {
            log_debug("--flags token: %s", token);
            utstring_renew(flag);
            utstring_printf(flag, "*mibl-%s*", token);
            mibl_s7_set_flag(utstring_body(flag), true);
            token = strtok(NULL, sep);
        }
        utstring_free(flag);
    }
}

void _print_version(void) {
    printf("FIXME: version id\n");
}

void _print_usage(void) {
    printf("Usage:\t$ bazel run @mibl//mibl [flags, options]\n");
    printf("Crawls the tree rooted at -w:\n");
    printf("Options:\n");
    printf("\t-w, --workspace <path>"
           "\tRelative path to workspace root directory to be used as traversal root. (OPTIONAL)\n");
    printf("\t-m, --main <entry-pt>"
           "\tPath to script containing -main routine. (OPTIONAL)\n");
    printf("\n");
    printf("Flags:\n");
    printf("  Emit flags control file writing.\n");
    printf("\t--emit-parsetree\tWrite files PARSETREE.{mibl,s7}.\n");
    printf("\t--emit-wss\t\tSet var *mibl-emit-wss*; default script writes WS files.\n");
    printf("\t--emit-pkgs\t\tSet var *mibl-emit-pkgs; default script writes PKG files.\n");

    printf("\n");
    printf("  Emit formats: mibl = human-readable; s7=scheme-readable\n");
    printf("\t--emit-mibl\t\tSet var *mibl-emit-mibl*; default script writes *.mibl files.\n");
    printf("\t--emit-s7\t\tSet var *mibl-emit-s7*; default script writes *.s7 files.\n");

    /* printf("\t--clean\t\t\tSet var *mibl-clean*; default script removes *.mibl and *.s7 files.\n"); */
    /* printf("\t--clean-mibl\t\tSet var *mibl-clean-mibl*; default script removes *.mibl files.\n"); */
    /* printf("\t--clean-s7\t\tSet var *mibl-clean-s7*; default script removes *.s7 files.\n"); */

    printf("  Report flags control file writing and halt.\n");
    printf("\t--report-parsetree\tWrite files PARSETREE.{mibl,s7} and halt.\n");

    printf("\n");
    printf("  Show flags control output to stdout.\n");
    printf("\t--show-config\t\tPrint configuration to stdout and exit.\n");
    printf("\t--show-mibl\t\tPrint mibl to stdout.\n");
    printf("\t--show-parsetree\tPrint parsetree to stdout and exit.\n");
    printf("\t--show-traversal\tPrint statistics on traversal.\n");

    printf("\n");
    /* printf("\t-d, --debug\t\tEnable all debugging flags.\n"); */
    /* printf("\t-t, --trace\t\tEnable trace flags.\n"); */
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t-q, --quiet\t\tSuppress stdout/stderr.\n");
    printf("\t--version\t\tShow version Id.\n");
    printf("\n");
    printf("INI file: $XDG_CONFIG_HOME/miblrc\n");

    printf("\n");
}

void _print_debug_usage(void) {
    _print_usage();
    printf("Debug flags:\n");
    printf("\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t-q, --quiet\t\tSuppress stdout/stderr.\n");
    printf("\t--version\t\tShow version Id.\n");
    printf("\n");
 }

static struct option options[] = {
    /* 0 */
    [OPT_MAIN] = {.long_name="main",.short_name='m',
                  .flags=GOPT_ARGUMENT_REQUIRED | GOPT_REPEATABLE},
    [OPT_WS] = {.long_name="workspace",.short_name='w',
                .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_HALT_AFTER] = {.long_name="halt-after",
                   .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_FLAGS] = {.long_name="flags",
                   .flags=GOPT_ARGUMENT_REQUIRED},

    [FLAG_MENHIR] = {.long_name="menhir",
                     .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_DEPS] = {.long_name="show-deps",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_RAW_DEPS] = {.long_name="show-raw-deps",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_MIBL] = {.long_name="show-mibl",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_PKG] = {.long_name="show-pkg",
                       .flags=GOPT_ARGUMENT_REQUIRED},
    [FLAG_SHOW_PARSETREE] = {.long_name="show-parsetree",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_TRAVERSAL] = {.long_name="show-traversal",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_DEPS] = {.long_name="debug-deps",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_EMIT] = {.long_name="debug-emit",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_PPX] = {.long_name="debug-ppx",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_REPORT_PARSETREE] = {.long_name="report-parsetree",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_S7] = {.long_name="debug-s7",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_S7_ALL] = {.long_name="debug-all",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_S7_LOADS] = {.long_name="debug-s7-loads",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_TRAVERSAL] = {.long_name="debug-traversal",
                                 .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_DEV_MODE] = {.long_name="dev",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_CLEAN] = {.long_name="clean",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_CLEAN_MIBL] = {.long_name="clean-mibl",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_CLEAN_S7] = {.long_name="clean-s7",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_EMIT_MIBL] = {.long_name="emit-mibl",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_S7] = {.long_name="emit-s7",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_WSS] = {.long_name="emit-wss",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PKGS] = {.long_name="emit-pkgs",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PROJECT] = {.long_name="emit-project",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_NO_MIBLRC] = {.long_name="no-miblrc",
                        .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERSION] = {.long_name="version",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        if (options[FLAG_HELP].count > 1)
            _print_debug_usage();
        else
            _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERSION].count) {
        _print_version();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        /* printf("verbose ct: %d\n", options[FLAG_VERBOSE].count); */
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        mibl_debug = true;
#endif
    }

    if (options[FLAG_DEBUG_DEPS].count) {
#if defined(DEBUG_TRACE)
        mibl_debug_deps = true;
#else
        log_error("--debug-deps requires debug build, -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_TRAVERSAL].count) {
#if defined(DEBUG_TRACE)
        mibl_debug_traversal = true;
#else
        log_error("--debug-traversal requires debug build, -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_TRACE].count) {
        /* printf("trace ct: %d\n", options[FLAG_TRACE].count); */
#if defined(DEBUG_TRACE)
        mibl_trace = true;
#endif
    }

    if (options[FLAG_SHOW_DEPS].count) {
        mibl_show_deps = true;
    }

    if (options[FLAG_SHOW_RAW_DEPS].count) {
        mibl_show_raw_deps = true;
    }

    if (options[FLAG_SHOW_TRAVERSAL].count) {
        mibl_show_traversal = true;
    }
}

int main(int argc, char **argv, char **envp)
{
    int gopt_argc = gopt(argv, options);
    (void)gopt_argc;

    gopt_errors(argv[0], options);

    _set_options(options);

    mibl_check_tools();

    utstring_new(mibl_runfiles_root);
    utstring_printf(mibl_runfiles_root, "%s", getcwd(NULL, 0));

    mibl_s7_init();

    _update_mibl_config(options);

    mibl_s7_init2(NULL, // options[OPT_MAIN].argument,
                 options[OPT_WS].argument);

    _update_s7_globals(options);

    /*  if (options[FLAG_SHOW_CONFIG].count) { */
    /*     show_bazel_config(); */
    /*     show_mibl_config(); */
    /*     show_s7_config(); */

    /*     /\* dump env vars: *\/ */
    /*     /\* for (char **env = envp; *env != 0; env++) { *\/ */
    /*     /\*     char *thisEnv = *env; *\/ */
    /*     /\*     printf("%s\n", thisEnv); *\/ */
    /*     /\* } *\/ */
    /*     exit(EXIT_SUCCESS); */
    /* } */

    if (options[OPT_MAIN].count) {
        mibl_s7_run(options[OPT_MAIN].argument, options[OPT_WS].argument);
    } else {
        mibl_s7_run("mibl_main.scm", NULL);
        /* xen_repl(argc, argv); */

        char *script = "repl.scm";
        if (!s7_load(s7, script)) {
            log_error("failed: load %s", script);
            log_info("cwd: %s", getcwd(NULL,0));
        }
        s7_eval_c_string(s7, "((*repl* 'run))");
    }

    if (verbose)
        log_info("script exit...");
    return 0;

    /* if (exit_on_error) { */
    /*     s7_define_variable(s7, "*exit-on-error*", s7_t(s7)); */
    /* } else { */
    /*     s7_define_variable(s7, "*exit-on-error*", false); */
    /* } */
    /* printf("*exit-on-error*? %d\n", */
    /*        (s7_t(s7) == s7_name_to_value(s7, "*exit-on-error*"))); */

}
