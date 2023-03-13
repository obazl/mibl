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

    log_debug("cwd: %s", getcwd(NULL,0));
    log_debug("traversal root: %s", *rootdir);

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
            case FTS_D : // dir visited in post-order
                if (traverse_dir(tree, ftsentry)) {
                    log_debug("Scanning dir: %s", ftsentry->fts_path);
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
                }
                else log_debug("NOT scanning dir: %s", ftsentry->fts_path);
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

/* analyze_deps - run codept and ingest resulting sexp */
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
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "depgraph"),
                                              depgraph)));
    char *sexp =
        "(let ((deps-list (assoc-val 'dependencies depgraph))) "
        "  (car deps-list)) "
        ;

    s7_pointer deps_list = s7_eval_c_string_with_environment(s7, sexp, env);
    /* (void)deps_list; */
    char *tostr = TO_STR(deps_list);
    log_debug("DEPS-LIST: %s", tostr);
    free(tostr);
    s7_flush_output_port(s7, s7_current_output_port(s7));

    return deps_list;
}

s7_pointer get_deps(char *_pkg, char *tgt, s7_pointer deps_list)
{
    log_trace("get_deps: %s : %s", _pkg, tgt);

    /* char *tostr = TO_STR(deps_list); */
    /* log_debug("DEPS-LIST: %s", tostr); */
    /* free(tostr); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */

    /* NB: _pkg has leading "./", e.g. "./src/foo" */
    char *pkg;
    if (_pkg[0] == '.' && _pkg[1] == '/')
        pkg = _pkg + 2;
    else
        pkg = _pkg;
    (void)pkg;

    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 3,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "deps-list"),
                                              deps_list),
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "pkg"),
                                              s7_make_string(s7, pkg)),
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "tgt"),
                                              s7_make_string(s7, tgt))));

    char *sexp =
        "(let* ((path (format #f \"~A/~A\" pkg tgt)) "
        "       (key `(file ,(symbol path)))) "
        "  (format #t \"KEY: ~A~%\" key) "
        "  (if-let ((needle (find-if (lambda (x) (equal? key (car x))) deps-list))) "
        "      (if-let ((deps (cdr needle))) "
        "        (if (truthy? deps) "
        "          (let* ((dlist (cadar deps)) "
        "                 (fixed (map (lambda (lst) "
        "                                  (if (> (length lst) 1) "
        "                                      (symbol (string-join (map symbol->string lst) \".\")) "
        "                                       lst)) "
        "                             dlist))) "
        "            (format #t \"deps: ~A~%\" (flatten fixed)) "
        "            (flatten fixed)) "
        "            '()) "
        "         '()) "
        "       '()))"
        ;

    s7_pointer deps = s7_eval_c_string_with_environment(s7, sexp, env);
    /* (void)deps; */
    /* char *tostr = TO_STR(deps); */
    /* log_debug("DEPS-LIST: %s", tostr); */
    /* free(tostr); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */

    return deps;  // s7_list(s7, 1, s7_make_symbol(s7, "Foobar"));
}