#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "s7.h"
#include "deps.h"

LOCAL int _select_ocaml_srcs(const struct dirent *de) {
    /* log_debug("selecting on %s", de->d_name); */
    char *ext = strrchr(de->d_name, '.');
    if (ext == NULL) {
        return 0;
    } else if (strncmp(ext, ".mli", 4) == 0) {
        return 1;
    }
    else if (strncmp(ext, ".ml", 3) == 0) {
        return 1;
    } else
        return 0;
}

LOCAL int _compare(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

/* traverse tree once to get list of dirs containing ocaml srcs */
UT_array *_ws_src_dirs(char *const *rootdir)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace("_ws_src_dirs");
#endif
    UT_array  *_ocaml_src_dirs;
    utarray_new(_ocaml_src_dirs, &ut_str_icd);

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    int scan_ct;
    struct dirent **namelist;

    errno = 0;
    tree = fts_open(rootdir,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &_compare
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return NULL;
    }
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            switch (ftsentry->fts_info) {
            case FTS_D : // dir visited in pre-order
                scan_ct = scandir(ftsentry->fts_path, &namelist,
                                  _select_ocaml_srcs,
                                  alphasort);
                /* log_debug("scan_ct: %d", scan_ct); */
                if (scan_ct > 0) {
                    char *s = strndup(ftsentry->fts_name,
                                      strlen(ftsentry->fts_name));
                    utarray_push_back(_ocaml_src_dirs, &s);
                    free(s);
                }
                break;
            default:
                /* log_error(RED "Unhandled FTS type %d\n", */
                /*           ftsentry->fts_info); */
                /* exit(EXIT_FAILURE); */
                break;
            }
        }
    }
    return _ocaml_src_dirs;
}

s7_pointer analyze_deps(char *const *rootdir) //, UT_array *ocaml_src_dirs)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("analyze_deps");
        log_trace("cwd: %s", getcwd(NULL,0));
        log_debug("traversal root: '%s'", *rootdir);
        /* char **p; */
        /* p = NULL; */
        /* log_debug("ocaml src dirs:"); */
        /* while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) { */
        /*     log_info("\t%s",*p); */
        /* } */
    }
#endif


    /* NB: the utarray and its elts must be freed */
    UT_array *_ocaml_src_dirs = _ws_src_dirs(rootdir);
    char **p = NULL;
    while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) {
        log_debug("src dir: %s", *p);
    }

    int n = utarray_len(_ocaml_src_dirs);

    char **argv = calloc(6 + n, sizeof(char*));
    argv[0] = "codept";
    argv[1] = "-verbosity";
    argv[2] = "info";
    argv[3] = "-sexp";
    argv[4] = "-k";
    /* now add ocaml src dirs collected by load_project */
    int i = 5;
    p = NULL;
    while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) {
        argv[i] = *p;
        i++;
    }
    argv[i] = NULL;
    /* log_debug("I: %d", i); */

    char *exe = NULL;
    const char *result = NULL;
    exe = "codept";

    /* FIXME: write to tmp dir instead of buffer */
    /* or write to DEPS.mibl? */
    result = run_cmd(exe, argv);
    if (result == NULL) {
        log_error(" run_cmd 'codept ...'\n");
        fprintf(stderr,
                "%s:%d "
                RED "ERROR: " CRESET
                " run_cmd 'codept ...'\n",
                __FILE__, __LINE__);
        s7_flush_output_port(s7, s7_current_output_port(s7));
        s7_flush_output_port(s7, s7_current_error_port(s7));
        fflush(NULL);
        return s7_nil(s7);
    }

    s7_pointer depgraph_port = s7_open_input_string(s7, result);
    s7_pointer depgraph = s7_read(s7, depgraph_port);

    return depgraph;
}
