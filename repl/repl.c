#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"

#include "linenoise.h"
#include "s7.h"

#include "utarray.h"
#include "utstring.h"

#include "libmibl.h"
#include "repl.h"

extern s7_scheme *s7;

char *history = ".ocamlark.history.txt";

extern bool debug;
extern bool trace;
extern bool verbose;
extern bool ini_error;
/* extern UT_string *obazl_ini_path; */
/* extern struct configuration_s bazel_config; */

/* UT_array *opam_dirs;             /\* string list *\/ */
/* extern UT_string *codept_args_file; */

void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}

char *hints(const char *buf, int *color, int *bold) {
    if (!strcasecmp(buf,"hello")) {
        *color = 35;
        *bold = 0;
        return " World";
    }
    return NULL;
}

void std_repl()
{
    char *line;
    char response[1024];        /* result of evaluating input */

    /* const char *errmsg = NULL; */

    /* list opam dirs, to parameterize codept */
    /* opam_dirs = inventory_opam(); */
    /* log_debug("OPAM dir ct: %d", utarray_len(opam_dirs)); */
    /* char **dir = NULL; */
    /* while ( (dir=(char**)utarray_next(opam_dirs,dir))) { */
    /*     log_debug("%s",*dir); */
    /* } */

    /* dir = NULL; */
    /* while ( (dir=(char**)utarray_next(bazel_config.src_dirs,dir))) { */
    /*     log_debug("src dir: %s",*dir); */
    /* } */

    /* log_debug("linenoise config"); */
    linenoiseSetMultiLine(1);   /* always support multiline */

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    linenoiseSetCompletionCallback(completion);
    linenoiseSetHintsCallback(hints);

    //FIXME: put history in ~/.obazl.d
    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad(history); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */

    while((line = linenoise("s7> ")) != NULL) {

        if (line[0] != '\0' && line[0] != '/') {
            snprintf(response, 1024, "(write %s)", line);
            s7_eval_c_string(s7, response);
            printf("%s", "\n");

            linenoiseHistoryAdd(line); /* Add to the history. */
            linenoiseHistorySave(history); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            linenoiseHistorySetMaxLen(len);
        } else if (!strncmp(line, "/mask", 5)) {
            linenoiseMaskModeEnable();
        } else if (!strncmp(line, "/unmask", 7)) {
            linenoiseMaskModeDisable();
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    }
}

void _print_version(void) {
    printf("mibl repl v 0.1\n");
}

void _print_usage(void)
{
    printf("Usage: mibl [-m | -k | -v | -h ]\n");
}

enum OPTS {
    FLAG_MULTILINE,
    FLAG_KEYCODES,
    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_VERSION,
    LAST
};

static struct option options[] = {
    /* 0 */
    [FLAG_MULTILINE] = {.long_name="multiline",.short_name='m',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_KEYCODES] = {.long_name="keycodes",.short_name='k',
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERSION] = {.long_name="version",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

int main(int argc, char **argv)
{
    /* Parse options, with --multiline we enable multi line editing. */

    argc = gopt(argv, options);
    gopt_errors(argv[0], options);

    if (options[FLAG_HELP].count) {
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

    if (options[FLAG_MULTILINE].count) {
        linenoiseSetMultiLine(1);
        if (verbose) printf("Multi-line mode enabled.\n");
    }

    if (options[FLAG_KEYCODES].count) {
        linenoisePrintKeyCodes();
        exit(0);                /* ??? */
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        debug = true;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        trace = true;
#endif
    }

   /* initialize in this order: bazel then mibl then s7 */
    bazel_configure(NULL);

    mibl_configure();

    s7_configure(NULL, NULL);

    s7_load(s7, "dune.scm");

    initialize_mibl_data_model(s7);

    /* chdir(launch_dir); */
    /* if (mibl_debug) */
    /*     log_debug("Set CWD to launch dir: %s", launch_dir); */

    /* s7_repl(s7); */
    xen_repl(argc, argv);

    return 0;
}
