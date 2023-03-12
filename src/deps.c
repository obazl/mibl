#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "deps.h"

void analyze_deps(const char *rootdir, UT_array *ocaml_src_dirs)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("analyze_deps");
        log_trace("cwd: %s", getcwd(NULL,0));
        log_debug("traversal root: '%s'", rootdir);
        char **p;
        p = NULL;
        log_debug("ocaml src dirs:");
        while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) {
            log_info("\t%s",*p);
        }
    }
#endif

    /* char *argv[] = { */
    char **p;

    int n = utarray_len(ocaml_src_dirs);

    char **argv = calloc(6 + n, sizeof(char*));
    argv[0] = "codept";
    argv[1] = "-verbosity";
    argv[2] = "info";
    argv[3] = "-sexp";
    argv[4] = "-k";
    /* argv[5] = "runtime-lib"; */
    /* argv[6] = "src"; */
    int i = 5;
    p = NULL;
    while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) {
        argv[i] = *p;
        i++;
    }
    argv[i] = NULL;
    /* log_debug("I: %d", i); */

    char *exe = NULL, *result = NULL;
    exe = "codept";

    /* FIXME: write to tmp dir instead of buffer */
    /* or write to DEPS.mibl? */
    result = run_cmd(exe, argv);
    if (result == NULL) {
        fprintf(stderr,
                "%s:%d "
                RED "ERROR: " CRESET
                " run_cmd 'codept ...'\n",
                __FILE__, __LINE__);
    } else {
        log_debug("codept result: '%s'", result);
    }

}
