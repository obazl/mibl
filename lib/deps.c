#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <inttypes.h>
#include <unistd.h>

#include "liblogc.h"
#include "utarray.h"
#include "utstring.h"
#include "s7.h"
#include "deps.h"

#if defined(PROFILE_fastbuild)
#define TRACE_FLAG mibl_trace
extern bool    TRACE_FLAG;
#define DEBUG_LEVEL mibl_debug
extern int     DEBUG_LEVEL;
#define S7_DEBUG_LEVEL libs7_debug
extern int libs7_debug;
extern int     s7plugin_debug;

bool mibl_debug_deps = false;
#endif

char *tostr1;
char *tostr2;

bool mibl_show_raw_deps = false;

LOCAL int _select_ocaml_srcs(const struct dirent *de) {
    log_debug("selecting on %s", de->d_name);
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
    TRACE_ENTRY;
    UT_array  *_ocaml_src_dirs;
    utarray_new(_ocaml_src_dirs, &ut_str_icd);

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    int scan_ct;
    struct dirent **namelist;

#if defined(TRACING)
    if (mibl_debug_deps) {
        log_debug("cwd: %s", getcwd(NULL,0));
        log_debug("traversal root: %s", *rootdir);
    }
#endif
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
#if defined(TRACING)
                    if (mibl_debug_deps)
                        log_debug("Scanning dir: %s", ftsentry->fts_path);
#endif
                    scan_ct = scandir(ftsentry->fts_path, &namelist,
                                      _select_ocaml_srcs,
                                      alphasort);
                    log_debug("scan_ct: %d", scan_ct);
                    log_debug("fts_path: %s", ftsentry->fts_path);
                    log_debug("fts_name: %s", ftsentry->fts_name);
                    if (scan_ct > 0) {
                        char *s = strndup(ftsentry->fts_path+2, // chop './' pfx
                                          strlen(ftsentry->fts_path) - 1);
                        /* utarray_push_back makes a copy */
                        utarray_push_back(_ocaml_src_dirs, &s);
                        free(s);
                    }
                }
#if defined(TRACING)
                else {
                    if (mibl_debug_deps)
                        log_debug("NOT scanning dir: %s", ftsentry->fts_path);
                }
#endif
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

s7_pointer _codept_deps(s7_scheme *s7, UT_array *_ocaml_src_dirs)
{
    int n = utarray_len(_ocaml_src_dirs);

    // WARNING: we can only run codept on one dir at a time, due to
    // the flat namespace. E.g. sexplib has src/std.ml and
    // num/lib/std.ml If we codept both of those at once only one will
    // be returned since the key is the module name, and they're both
    // Std.

    // Maybe a bug in codept. It should return an entry in
    // 'dependencies for both, since those are keyed by file path.
    // Also the 'local list, being keyed by module name, should
    // include multiple entries for modules with multiple impl files.

    char **argv = calloc(6 + n, sizeof(char*));
    argv[0] = "codept";
    argv[1] = "-verbosity";
    argv[2] = "info";
    argv[3] = "-sexp";
    argv[4] = "-k";
    /* now add ocaml src dirs collected by load_project */
    int i = 5;
    char **p = NULL;
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

    log_debug("CODEPT: %s", result);
    exit(0);
    s7_pointer depgraph_port = s7_open_input_string(s7, result);
    s7_pointer depgraph = s7_read(s7, depgraph_port);
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "depgraph"),
                                              depgraph)));

    if (mibl_show_raw_deps) {
        log_info("DEPS:");
        char *sexp = "(mibl-pretty-print depgraph) ";
        s7_pointer r = s7_eval_c_string_with_environment(s7, sexp, env);
        (void)r;
        s7_newline(s7, s7_current_output_port(s7));
        /* LOG_S7_DEBUG("DEPS-LIST", tostr1); */
        s7_flush_output_port(s7, s7_current_output_port(s7));
    }

    /* FIXME: this will fail if depgraph is empty, */
    char *sexp =
        "(let ((deps-list (assoc-val 'dependencies depgraph))) "
        "  (car deps-list)) "
        ;

    s7_pointer deps_list = s7_eval_c_string_with_environment(s7, sexp, env);
    (void)deps_list;
    /* LOG_S7_DEBUG("DEPS_LIST", deps_list); */

   return deps_list;
}

s7_pointer _codept_deps_pkg(s7_scheme *s7, char *pkgdir)
{
    TRACE_ENTRY;
#if defined(TRACING)
        log_trace("_codept_deps_pkg: %s", pkgdir);
#endif

    char **argv = calloc(7, sizeof(char*));
    argv[0] = "codept";
    argv[1] = "-verbosity";
    argv[2] = "info";
    argv[3] = "-sexp";
    argv[4] = "-k";
    argv[5] = (char*)pkgdir;
    /* /\* now add ocaml src dirs collected by load_project *\/ */
    /* int i = 5; */
    /* char **p = NULL; */
    /* p = NULL; */
    /* while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) { */
    /*     argv[i] = *p; */
    /*     i++; */
    /* } */
    argv[6] = NULL;
    /* log_debug("I: %d", i); */

    char *exe = NULL;
    const char *result = NULL;
    exe = "codept";

    /* FIXME: write to tmp dir instead of buffer */
    /* or write to DEPS.mibl? */
#if defined(TRACING)
    log_debug("running codept cmd");
#endif
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

    log_debug("XCODEPT: %s", result);
    exit(0);
    s7_pointer depgraph_port = s7_open_input_string(s7, result);
    s7_pointer depgraph = s7_read(s7, depgraph_port);
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "depgraph"),
                                              depgraph)));

    if (mibl_show_raw_deps) {
        log_info("DEPS:");
        char *sexp = "(mibl-pretty-print depgraph) ";
        s7_pointer r = s7_eval_c_string_with_environment(s7, sexp, env);
        (void)r;
        s7_newline(s7, s7_current_output_port(s7));
        /* LOG_S7_DEBUG("DEPS-LIST", r); */
        s7_flush_output_port(s7, s7_current_output_port(s7));
    }

    /* FIXME: this will fail if depgraph is empty, */
    char *sexp =
        "(let ((deps-list (assoc-val 'dependencies depgraph))) "
        "  (car deps-list)) "
        ;

    s7_pointer deps_list = s7_eval_c_string_with_environment(s7, sexp, env);
    (void)deps_list;
    /* log_debug("DEPS_LIST: %s", s); */

   return deps_list;
}

s7_int gc_module_deps;          /* gc protects deps returned by get_deps */
s7_int gc_depgraph_port, gc_depgraph, gc_env, gc_deps_list;

/* s7_pointer analyze_deps_file(FTSENT *ftsentry) */
/* returns gc_protected deps-list */
s7_pointer analyze_deps_file(s7_scheme *s7, char *pkg, char *tgt)
{
    TRACE_ENTRY;
#if defined(PROFILE_fastbuild)
    if (mibl_debug) {
        log_trace(RED "analyze_deps_file" CRESET);
        log_debug("pkg: '%s'", pkg); //ftsentry->fts_path);
        log_debug("tgt: '%s'", tgt); //ftsentry->fts_name);
        log_trace("cwd: %s", getcwd(NULL,0));
        /* char **p; */
        /* p = NULL; */
        /* log_debug("ocaml src dirs:"); */
        /* while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) { */
        /*     log_info("\t%s",*p); */
        /* } */
    }
    log_info(RED "analyze traversal ct:" CRESET " %d", tct);
#endif

    UT_string *fname;
    utstring_new(fname);
    utstring_printf(fname, "%s/%s", pkg, tgt);

    char **argv = calloc(7, sizeof(char*));
    argv[0] = "codept";
    argv[1] = "-verbosity";
    argv[2] = "info";
    argv[3] = "-sexp";
    argv[4] = "-k";
    argv[5] = utstring_body(fname);  // ftsentry->fts_path + 2; // drop leading ./
    argv[6] = NULL;
    /* log_debug("I: %d", i); */

    char *exe = NULL;
    const char *result = NULL;
    exe = "codept";

    /* FIXME: write to tmp dir instead of buffer */
    /* or write to DEPS.mibl? */

    /* log_debug("running codept cmd"); */
    result = run_cmd(exe, argv);
    /* log_debug("returned: %s", result); */
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

#if defined(PROFILE_fastbuild)
    log_debug("CODEPT result string: %s", result);
#endif

    s7_pointer depgraph_port = s7_open_input_string(s7, result);
    s7_gc_protect_via_stack(s7, depgraph_port);
#if defined(PROFILE_fastbuild)
    if (mibl_config.debug_deps)
        LOG_S7_DEBUG("depgraph_port", depgraph_port);
#endif
    s7_pointer depgraph = s7_read(s7, depgraph_port);

    s7_gc_protect_via_stack(s7, depgraph);
    s7_gc_unprotect_via_stack(s7, depgraph_port);
    s7_close_input_port(s7, depgraph_port);

#if defined(PROFILE_fastbuild)
    LOG_S7_DEBUG("depgraph", depgraph);
#endif

    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "depgraph"),
                                              depgraph)));
    s7_gc_protect_via_stack(s7, env);
#if defined(PROFILE_fastbuild)
    LOG_S7_DEBUG("env", env);
#endif
    /* gc_env = s7_gc_protect(s7, env); */

    if (mibl_show_raw_deps) {

        log_info("RAWDEPS:");
        char *sexpx = "(mibl-pretty-print depgraph) ";

        /* s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7)); */
        /* s7_int gc_x = -1; */
        /* if (old_port != s7_nil(s7)) */
	/*     gc_x = s7_gc_protect(s7, old_port); */
        s7_pointer r = s7_eval_c_string_with_environment(s7, sexpx, env);
        LOG_S7_DEBUG("raw", r);
        (void)r; // no need to gc protect
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        /* if we got something, wrap it in "[]" */
        if ((errmsg) && (*errmsg))
	    log_error("[%s]", errmsg);

        /* s7_close_output_port(s7, s7_current_error_port(s7)); */
	  /* s7_set_current_error_port(s7, old_port); */
	  /* if (gc_x != -1) */
	  /*   s7_gc_unprotect_at(s7, gc_x); */

        s7_newline(s7, s7_current_output_port(s7));
        s7_flush_output_port(s7, s7_current_output_port(s7));
    }

    /* FIXME: this will fail if depgraph is empty, */
    char *sexp =
        "(let ((deps-list (assoc-val 'dependencies depgraph))) "
        "  (car deps-list)) "
        ;

    s7_pointer deps_list = s7_eval_c_string_with_environment(s7, sexp, env);
    gc_deps_list = s7_gc_protect(s7, deps_list);
#if defined(PROFILE_fastbuild)
    LOG_S7_DEBUG("DEPS_list", deps_list);
#endif

    /* (void)deps_list; */
#if defined(PROFILE_fastbuild)
    LOG_S7_DEBUG("DEPS_LIST", deps_list);
#endif

    s7_gc_unprotect_via_stack(s7, depgraph);
    s7_gc_unprotect_via_stack(s7, env);

    return deps_list;
}

/* s7_pointer _ocamldep_deps(UT_array *_ocaml_src_dirs) */
/* { */
/*     int n = utarray_len(_ocaml_src_dirs); */

/*     char **argv = calloc(3 + n, sizeof(char*)); */
/*     argv[0] = "ocamldep"; */
/*     argv[1] = "-modules"; */
/*     /\* now add ocaml src dirs collected by load_project *\/ */
/*     int i = 2; */
/*     char **p = NULL; */
/*     p = NULL; */
/*     while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) { */
/*         argv[i] = *p; */
/*         i++; */
/*     } */
/*     argv[i] = NULL; */
/*     /\* log_debug("I: %d", i); *\/ */

/*     char *exe = NULL; */
/*     const char *result = NULL; */
/*     exe = "ocamldep"; */

/*     /\* FIXME: write to tmp dir instead of buffer *\/ */
/*     /\* or write to DEPS.mibl? *\/ */
/*     result = run_cmd(exe, argv); */
/*     if (result == NULL) { */
/*         log_error(" run_cmd 'ocamldep ...'\n"); */
/*         fprintf(stderr, */
/*                 "%s:%d " */
/*                 RED "ERROR: " CRESET */
/*                 " run_cmd 'ocamldep ...'\n", */
/*                 __FILE__, __LINE__); */
/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*         s7_flush_output_port(s7, s7_current_error_port(s7)); */
/*         fflush(NULL); */
/*         return s7_nil(s7); */
/*     } */

/*     s7_pointer depgraph_port = s7_open_input_string(s7, result); */
/*     s7_pointer depgraph = s7_read(s7, depgraph_port); */
/*     LOG_S7_DEBUG("DEPGRAPH", depgraph); */

/*     s7_pointer env = s7_inlet(s7, */
/*                               s7_list(s7, 1, */
/*                                       s7_cons(s7, */
/*                                               s7_make_symbol(s7, "depgraph"), */
/*                                               depgraph))); */

/*     if (mibl_show_raw_deps) { */
/*         log_info("DEPS:"); */
/*         char *sexp = "(mibl-pretty-print depgraph) "; */
/*         s7_pointer r = s7_eval_c_string_with_environment(s7, sexp, env); */
/*         (void)r; */
/*         s7_newline(s7, s7_current_output_port(s7)); */
/*         /\* LOG_S7_DEBUG("DEPS-LIST:", r); *\/ */
/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*     } */

/*     /\* FIXME: this will fail if depgraph is empty, *\/ */
/*     char *sexp = */
/*         "(let ((deps-list (assoc-val 'dependencies depgraph))) " */
/*         "  (car deps-list)) " */
/*         ; */

/*     s7_pointer deps_list = s7_eval_c_string_with_environment(s7, sexp, env); */
/*     (void)deps_list; */
/*     /\* LOG_S7_DEBUG("DEPS_LIST", deps_list); *\/ */
/*     /\* free(s); *\/ */

/*    return deps_list; */
/* } */

/* analyze_deps - run codept and ingest resulting sexp */
/* s7_pointer analyze_deps_wsroot(char *const *rootdir) //, UT_array *ocaml_src_dirs) */
/* { */
/* #if defined(TRACING) */
/*     if (mibl_trace) { */
/*         log_trace("analyze_deps"); */
/*         log_trace("cwd: %s", getcwd(NULL,0)); */
/*         log_debug("traversal root: '%s'", *rootdir); */
/*         /\* char **p; *\/ */
/*         /\* p = NULL; *\/ */
/*         /\* log_debug("ocaml src dirs:"); *\/ */
/*         /\* while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) { *\/ */
/*         /\*     log_info("\t%s",*p); *\/ */
/*         /\* } *\/ */
/*     } */
/* #endif */

/*     /\* _check_tools(); *\/ */

/*     /\* NB: the utarray must be freed *\/ */
/*     UT_array *_ocaml_src_dirs = _ws_src_dirs(rootdir); */

/* #if defined(TRACING) */
/*     /\* if (mibl_debug_deps) { *\/ */
/*         log_debug("ocaml_src_dirs for codept:"); */
/*         char **p = NULL; */
/*         p = NULL; */
/*         while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) { */
/*             log_debug("src dir: %s", *p); */
/*         } */
/*     /\* } *\/ */
/* #endif */

/*     s7_pointer depslist; */
/*     /\* depslist = _ocamldep_deps(_ocaml_src_dirs); *\/ */
/*     /\* depslist = _codept_deps(_ocaml_src_dirs); *\/ */

/*     if (system("which codept > /dev/null 2>&1")) { */
/*         /\* codept not found, try ocamldep *\/ */
/*         /\* depslist _ocamldep_deps(_ocaml_src_dirs); *\/ */
/*         log_error("Required tool: codept not found."); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/*         depslist = _codept_deps(_ocaml_src_dirs); */
/*     } */

/*     utarray_free(_ocaml_src_dirs); */

/*     return depslist; */
/* } */

/* s7_pointer analyze_deps_pkg(char *pkgdir) */
/* { */
/* #if defined(TRACING) */
/*     if (mibl_trace) { */
/*         log_trace("cwd: %s", getcwd(NULL,0)); */
/*         log_debug("pkgdir: '%s'", pkgdir); */
/*         /\* char **p; *\/ */
/*         /\* p = NULL; *\/ */
/*         /\* log_debug("ocaml src dirs:"); *\/ */
/*         /\* while ( (p=(char**)utarray_next(ocaml_src_dirs,p))) { *\/ */
/*         /\*     log_info("\t%s",*p); *\/ */
/*         /\* } *\/ */
/*     } */
/* #endif */

/*     /\* _check_tools(); *\/ */

/*     /\* NB: the utarray must be freed *\/ */
/* /\*     UT_array *_ocaml_src_dirs = _ws_src_dirs(rootdir); *\/ */

/* /\* #if defined(TRACING) *\/ */
/* /\*     /\\* if (mibl_debug_deps) { *\\/ *\/ */
/* /\*         log_debug("ocaml_src_dirs for codept:"); *\/ */
/* /\*         char **p = NULL; *\/ */
/* /\*         p = NULL; *\/ */
/* /\*         while ( (p=(char**)utarray_next(_ocaml_src_dirs,p))) { *\/ */
/* /\*             log_debug("src dir: %s", *p); *\/ */
/* /\*         } *\/ */
/* /\*     /\\* } *\\/ *\/ */
/* /\* #endif *\/ */

/*     s7_pointer depslist; */
/*     /\* depslist = _ocamldep_deps(_ocaml_src_dirs); *\/ */
/*     /\* depslist = _codept_deps(_ocaml_src_dirs); *\/ */

/*     if (system("which codept > /dev/null 2>&1")) { */
/*         /\* codept not found, try ocamldep *\/ */
/*         /\* depslist _ocamldep_deps(_ocaml_src_dirs); *\/ */
/*         log_error("Required tool: codept not found."); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/*         depslist = _codept_deps_pkg(pkgdir); */
/*     } */

/*     /\* utarray_free(_ocaml_src_dirs); *\/ */

/*     return depslist; */
/* } */

/* returns gc_protected deps list */
s7_pointer get_deps(s7_scheme *s7, char *_pkg, char *tgt)
// , s7_pointer deps_list)
{
#if defined(TRACING)
    /* if (mibl_debug_deps) */
        log_trace(RED "get_deps:" CRESET " %s : %s", _pkg, tgt);
        log_trace("traversal ct: %d", tct);
#endif

    /* return s7_nil(s7); */

/* #if defined(TRACING) */
/*     LOG_S7_DEBUG("DEPS-LIST", deps_list); */
/* #endif */

    /* NB: _pkg has leading "./", e.g. "./src/foo" */
    char *pkg;
    if (_pkg[0] == '.' && _pkg[1] == '/')
        pkg = _pkg + 2;
    else
        pkg = _pkg;
    (void)pkg;

    s7_pointer deps_list = analyze_deps_file(s7, pkg, tgt);
    /* deps_list is gc_protected */

    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 3,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "deps-list"),
                                              // s7_nil(s7)),
                                              deps_list),
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "pkg"),
                                              s7_make_symbol(s7, pkg)),
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "tgt"),
                                              s7_make_symbol(s7, tgt))));
    s7_gc_protect_via_stack(s7, env);
    /* deps_list is referenced by env inlet, so we can unprotect it */
    s7_gc_unprotect_at(s7, gc_deps_list);

#if defined(TRACING)
    LOG_S7_DEBUG("ENV", env);
#endif

    /* char *sexp = */
    /*     "(begin " */
    /*     "(format #t \"XXXX depslist: ~A\n\" deps-list)" */
    /*     ")" */
    /*     ; */

    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* char *sexp = */
    /*     "(let ((p (format #f \"~A/~A\" pkg tgt))) '())" */
    /*     ; */
    /* "(let ((p (string-append (symbol->string pkg) \"/\" (symbol->string tgt)))) (format () \"~A~%\" p) '())" */

    /* (let ((path (format #f \"~A/~A\" pkg tgt))) \ */


    char *sexp =
        "(begin "
        "  (let* ((path (format #f \"~A/~A\" pkg tgt)) "
        "         (key `(file ,(symbol path)))) "
        "  (if-let ((needle (find-if (lambda (x) (equal? key (car x))) deps-list))) "
        "      (if-let ((deps (cdr needle))) "
        "        (if (truthy? deps) "
        "          (let* ((dlist (cadar deps)) "
        "                 (fixed (map (lambda (lst) "
        "                                  (if (> (length lst) 1) "
        "                                      (list->vector lst) lst)) "
        "                             dlist))) "
        "            (flatten fixed)) "
        "            '()) "
        "         '()) "
        "       '())))"
        ;
    /* (void)sexp; */

    /* log_debug("sexp: %s", sexp); */

    /* errno = 0; */
    /* char *x = malloc(10024); */
    /* if (errno) { */
    /*     log_error("MALLOC FAIL"); */
    /*     exit(1); */
    /* } else { */
    /*     free(x); */
    /*     log_info("malloc ok"); */
    /* } */

    /* s7_pointer deps = s7_list(s7, 1, s7_make_symbol(s7, "testdep")); */

    s7_pointer deps = s7_eval_c_string_with_environment(s7, sexp, env);
    gc_module_deps = s7_gc_protect(s7, deps);

    /* we're done with env and sexp, so unprotect */
    s7_gc_unprotect_via_stack(s7, env);
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */

#if defined(TRACING)
    LOG_S7_DEBUG("DEPS", deps);
#endif
    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));
    // }

    return deps;  // s7_list(s7, 1, s7_make_symbol(s7, "Foobar"));
}
