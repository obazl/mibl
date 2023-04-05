#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#if INTERFACE
#include <inttypes.h>
#endif
#include <libgen.h>
#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"

/* #if EXPORT_INTERFACE */
#include "utarray.h"
#include "utstring.h"
/* #endif */

/* #if INTERFACE */
#include "s7.h"
/* #endif */

#include "config_s7.h"

#if defined(DEBUG_TRACE)
extern bool mibl_debug;
extern bool mibl_trace;
#endif

bool mibl_debug_scm   = false;

bool emit_parsetree = false;

extern bool bzl_mode;

/* char *callback_script_file = "dune.scm"; // passed in 'data' attrib */
char *callback = "camlark_handler"; /* fn in callback_script_file  */

#if INTERFACE
#define print_s7_int PRId64
#endif

s7_scheme *s7;                  /* GLOBAL s7 */

s7_int gc_wss;
s7_int gc_mibl_project;

s7_pointer dune_project_sym;
s7_pointer dune_stanzas_kw;
s7_pointer dune_stanzas_sym;
s7_pointer ws_path_kw;
s7_pointer pkg_path_kw;
s7_pointer realpath_kw;

s7_pointer modules_kw;
s7_pointer deps_kw;
s7_pointer sigs_kw;
s7_pointer structs_kw;
s7_pointer mll_kw;
s7_pointer mly_kw;
s7_pointer mllib_kw;
s7_pointer cppo_kw;
s7_pointer cc_kw;
s7_pointer cc_srcs_kw;
s7_pointer cc_hdrs_kw;
s7_pointer files_kw;
s7_pointer scripts_kw;
s7_pointer static_kw;
s7_pointer dynamic_kw;

s7_pointer opam_kw;

s7_pointer _s7_result;          /* for use with s7_call */
s7_pointer assoc;
s7_pointer assoc_in;
s7_pointer assoc_val;
s7_pointer sort_bang;
s7_pointer string_lt;
s7_pointer _s7_acons = NULL;
s7_pointer _s7_append = NULL;
s7_pointer _s7_list_set = NULL;
s7_pointer _s7_quote = NULL;
s7_pointer _s7_set_car = NULL;
s7_pointer _s7_set_cdr = NULL;

extern int rc;

#if INTERFACE
#define LOAD_DUNE_HELP "(mibl-load-project rootdir pathdir) rootdir is relative to $HOME; pathdir is relative to rootdir.  Change dir to rootdir and load pathdir, creating pkg-tbl"

/* NB: we need to escape #\" in C... */
#define LOAD_DUNE_FORMAL_PARAMS "s"

#endif

static char *mibl_s7_flags[] = {
    "*mibl-build-dyads*",
    "*mibl-clean-all*",
    "*mibl-clean-mibl*",
    "*mibl-clean-s7*",
    "*mibl-debug-action-deps*",
    "*mibl-debug-action-directives*",
    "*mibl-debug-alias*",
    "*mibl-debug-cleanup*",
    "*mibl-debug-cmd-runner*",
    "*mibl-debug-emit*",
    "*mibl-debug-executables*",
    "*mibl-debug-expanders*",
    "*mibl-debug-flags*",
    "*mibl-debug-file-exports*",
    "*mibl-debug-genrules*",
    "*mibl-debug-lexyacc*",
    "*mibl-debug-mibl*",
    "*mibl-debug-modules*",
    "*mibl-debug-prologues*",
    "*mibl-debug-ppx*",
    "*mibl-debug-rule-stanzas*",
    "*mibl-debug-s7*",
    "*mibl-debug-s7-loads*",
    "*mibl-debug-shared*",
    "*mibl-debug-show-pkgs*",
    "*mibl-debug-tests*",
    "*mibl-debug-updaters*",
    "*mibl-debug-s7*",
    "*mibl-dev-mode*",
    "*mibl-dune-prologue-includes-main*",
    "*mibl-dunefile-count*",
    "*mibl-emit-bazel-pkg*",
    "*mibl-emit-mibl*",
    /* "*mibl-emit-parsetree*", */
    "*mibl-emit-pkgs*",
    "*mibl-emit-project*",
    /* "*mibl-emit-s7*", */
    "*mibl-emit-starlark*",
    "*mibl-emit-wss*",
    "*mibl-js-emit-rules-closure*",
    "*mibl-js-emit-rules-js*",
    "*mibl-js-emit-rules-jsoo*",
    "*mibl-js-emit-rules-swc*",
    "*mibl-local-ppx-driver*",
    "*mibl-menhir*",
    "*mibl-namespace-executables*",
    "*mibl-ns-topdown*",
    "*mibl-ppxlib-ppx-driver*",
    "*mibl-quiet*",
    "*mibl-report-parsetree*",
    "*mibl-shared-deps*",
    "*mibl-shared-opts*",
    /* "*mibl-shared-ppx-pkg*", */
    "*mibl-show-config*",
    "*mibl-show-exports*",
    "*mibl-show-mibl*",
    "*mibl-show-parsetree*",
    "*mibl-show-project*",
    "*mibl-show-starlark*",
    "*mibl-unwrapped-libs-to-archives*",
    "*mibl-wrapped-libs-to-ns-archives*",
    "*mibl-verbose*",

    NULL /* do not remove */
};
char **mibl_s7_flag;

/* EXPORT s7_pointer g_effective_ws_root(s7_scheme *s7,  s7_pointer args) */
/* { */
/*     char *dir = NULL; */
/*     if ( s7_is_null(s7, args) ) { */
/*         dir = getcwd(NULL, 0); */
/*     } else { */
/*         s7_int args_ct = s7_list_length(s7, args); */
/*         if (args_ct == 1) { */
/*             s7_pointer arg = s7_car(args); */
/*             if (s7_is_string(arg)) { */
/*                 dir = strdup((char*)s7_string(arg)); */
/*             } */
/*         } else { */
/*             // throw exception */
/*         } */
/*     } */
/*     ews_root = effective_ws_root(dir); */
/*     free(dir); // effective_ws_root allocates its own */
/*     return s7_make_string(s7, ews_root); */
/* } */

/* called by load-project, repl. needed for project scripting. */
EXPORT void initialize_mibl_data_model(s7_scheme *s7)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_mibl)
        log_trace("initialize_mibl_data_model");
#endif
    /*
     * data model:
     * wss: alist, keys are ws names with @, values are alists
     * ws item alist:
     *   ws name
     *   ws path (realpath)
     *   exports: hash_table keyedy by target, vals: pkg paths
     *   filegroups:  derived from glob_file expressions in dunefile
     *   pkgs: hash_table keyed by pkg path

     * exports and filegroups are temporary, used to support multiple
     * passes.
     */

#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_debug("_initialize_mibl_data_model");
#endif

    /* _s7_acons = _load_acons(s7); */
    /* _s7_list_set = _load_list_set(s7); */
    /* printf("_s7_list_set: %s\n", TO_STR(_s7_list_set)); */

    /* s7_pointer key, datum; */
    /* s7_pointer q = s7_name_to_value(s7, "quote"); */

    /* s7_pointer root_ws = s7_call(s7, q, */
    /*                              s7_list(s7, 1, */
    /*                                      s7_list(s7, 1, */
    /*                                              s7_make_symbol(s7, "@")))); */

    /* _s7_append = _load_append(s7); */

    UT_string *init_sexp;
    utstring_new(init_sexp);
    utstring_printf(init_sexp, "(define *mibl-project* "
                    "`((:@ (:name . \"@\") (:path . %s) "
                    "(:exports ,(make-hash-table)) "
                    "(:opam ,(make-hash-table)) "
                    "(:shared-ppx ,(make-hash-table)) "
                    "(:filegroups ,(make-hash-table)) "
                    "(:pkgs ,(make-hash-table)))))",
                    rootws);

    s7_pointer wss = s7_eval_c_string(s7, utstring_body(init_sexp));
    gc_wss = s7_gc_protect(s7, wss);
    /* (void)gc_wss; */
    /* (void)wss; */

    s7_pointer mp = s7_name_to_value(s7, "*mibl-project*");
    gc_mibl_project = s7_gc_protect(s7, mp);

    /* char *s = TO_STR(wss); */
    /* log_debug(RED "INITIAL *mibl-project*: %s" CRESET, s); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /* fflush(NULL); */
    /* free(s); */

    /* s7_pointer x = s7_name_to_value(s7, "*mibl-project*"); */
    /* if (x== s7_undefined(s7)) { */
    /*     log_error("unbound symbol: *mibl-project*"); */
    /* } */
    /* char *xs = s7_object_to_c_string(s7, x); */
    /* /\* fflush(stdout); *\/ */
    /* fprintf(stdout, "WWWW: %s\n", xs); */

    if (verbose && verbosity > 1) {
        /* printf("XXXX %s\n", NM_TO_STR("*mibl-project*")); */
        /* fflush(stdout); */
    }
    /* printf("2YYYYYYYYYYYYYYYY\n"); */
    /* log_info("YYYYYYYYYYYYYYYY"); */

    /* /\* s7_pointer base_entry = s7_make_list(s7, 4, s7_f(s7)); *\/ */
    /* key = s7_make_symbol(s7, "name"); */
    /* datum = s7_make_symbol(s7, "@"); */
    /* s7_pointer root_ws = s7_call(s7, _s7_append, */
    /*                              s7_list(s7, 2, root_ws, */
    /*                                      s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (mibl_debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* key   = s7_make_symbol(s7, "path"); */
    /* datum = s7_make_string(s7, rootws); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (mibl_debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* /\* table of "exports" - libs etc. possibly referenced as deps *\/ */
    /* key   = s7_make_symbol(s7, "exports"); */
    /* datum = s7_make_hash_table(s7, 64); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (mibl_debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* key   = s7_make_symbol(s7, "pkgs"); */
    /* datum = s7_make_hash_table(s7, 32); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (mibl_debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* root_ws = s7_list(s7, 1, root_ws); */

    /* if (mibl_debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* s7_define_variable(s7, "*mibl-project*", root_ws); */

    /* return root_ws; */
}

s7_pointer _init_scheme_fns(s7_scheme *s7)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_s7_config) log_debug("_init_scheme_fns");
#endif

    if (_s7_set_cdr == NULL) {
        _s7_set_cdr = s7_name_to_value(s7, "set-cdr!");
        if (_s7_set_cdr == s7_undefined(s7)) {
            log_error("unbound symbol: set-cdr!");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path*", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "set-cdr!")));
        }
    }

    if (_s7_quote == NULL) {
        _s7_quote = s7_name_to_value(s7, "quote");
        if (_s7_quote == s7_undefined(s7)) {
            log_error("unbound symbol: quote");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 2", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "quote")));
        }
    }

    _load_acons(s7);
    _load_assoc();
    assoc_in = _load_assoc_in();
    /* assoc_val = _load_assoc_val(); */
    _load_append();
    _load_list_set(s7);
    _load_sort();
    _load_string_lt();

    return _s7_set_cdr;
}

s7_pointer _load_acons(s7_scheme *s7)
{
    if (_s7_acons == NULL) {
        _s7_acons = s7_name_to_value(s7, "acons");
        if (_s7_acons == s7_undefined(s7)) {
            log_error("unbound symbol: acons");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 3", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "acons")));
        }
    /* } else { */
    /*     printf("already loaded\n"); */
    }
    return _s7_acons;
}

s7_pointer _load_assoc(void)
{
    if (assoc == NULL) {
        assoc = s7_name_to_value(s7, "assoc");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: assoc");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 4", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "assoc")));
        }
    }
    return assoc;
}

s7_pointer _load_assoc_in(void)
{
    if (assoc_in == NULL) {
        assoc_in = s7_name_to_value(s7, "assoc-in");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: assoc-in");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 5", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "assoc-in")));
        }
    }
    return assoc_in;
}

s7_pointer _load_assoc_val(void)
{
    if (assoc_val == NULL) {
        assoc_val = s7_name_to_value(s7, "assoc-in");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: assoc-in");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 6", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "assoc-in")));
        }
    }
    return assoc_val;
}

s7_pointer _load_append(void)
{
    if (_s7_append == NULL) {
        _s7_append = s7_name_to_value(s7, "append");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: append");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 7", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "append")));
        }
    }
    return _s7_append;
}

s7_pointer _load_list_set(s7_scheme *s7)
{
    if (_s7_list_set == NULL) {
        _s7_list_set = s7_name_to_value(s7, "list-set!");
        if (_s7_list_set == s7_undefined(s7)) {
            log_error("unbound symbol: list-set!");
#if defined(DEBUG_TRACE)
            s7_pointer lp = s7_load_path(s7);
            LOG_S7_DEBUG("*load-path* 8", lp);
#endif
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "list-set!")));
        }
    }
    return _s7_list_set;
}

s7_pointer _load_sort(void)
{
    sort_bang = s7_name_to_value(s7, "sort!");
    if (assoc == s7_undefined(s7)) {
        log_error("unbound symbol: sort!");
#if defined(DEBUG_TRACE)
        s7_pointer lp = s7_load_path(s7);
        LOG_S7_DEBUG("*load-path* 9", lp);
#endif
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "sort!")));
    }
    return sort_bang;
}

s7_pointer _load_string_lt(void)
{
    string_lt = s7_name_to_value(s7, "string<?");
    if (assoc == s7_undefined(s7)) {
        log_error("unbound symbol: string<?");
#if defined(DEBUG_TRACE)
        s7_pointer lp = s7_load_path(s7);
        LOG_S7_DEBUG("*load-path* 10", lp);
#endif
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "string<?")));
    }
    return string_lt;
}

void libc_s7_init(s7_scheme *sc);

/* FIXME: call into libs7 for this */
LOCAL __attribute__((unused)) void s7_config_repl(s7_scheme *sc)
{
    printf("mibl: s7_repl\n");
#if (!WITH_C_LOADER)
  /* dumb_repl(sc); */
#else
#if WITH_NOTCURSES
  s7_load(sc, "nrepl.scm");
#else
  log_debug("XXXXXXXXXXXXXXXX");
  s7_pointer old_e, e, val;
  s7_int gc_loc;
  bool repl_loaded = false;
  /* try to get lib_s7.so from the repl's directory, and set *libc*.
   *   otherwise repl.scm will try to load libc.scm which will try to build libc_s7.so locally, but that requires s7.h
   */
  e = s7_inlet(sc,
               s7_list(sc, 2,
                       s7_make_symbol(sc, "init_func"),
                      s7_make_symbol(sc, "libc_s7_init")));
               /* list_2(sc, s7_make_symbol(sc, "init_func"), */
               /*        s7_make_symbol(sc, "libc_s7_init"))); */
  gc_loc = s7_gc_protect(sc, e);
  old_e = s7_set_curlet(sc, e);   /* e is now (curlet) so loaded names from libc will be placed there, not in (rootlet) */

  /* printf("loading %s/%s\n", TOSTRING(OBAZL_RUNFILES_DIR), "/libc_s7.o"); */
  printf("loading libc_s7.o\n");
  printf("cwd: %s\n", getcwd(NULL, 0));

  val = s7_load_with_environment(sc, "libc_s7.so", e);
  if (val)
    {
      /* s7_pointer libs; */
      /* uint64_t hash; */
      /* hash = raw_string_hash((const uint8_t *)"*libc*", 6);  /\* hack around an idiotic gcc 10.2.1 warning *\/ */
      /* s7_define(sc, sc->nil, new_symbol(sc, "*libc*", 6, hash, hash % SYMBOL_TABLE_SIZE), e); */
      /* libs = global_slot(sc->libraries_symbol); */
      /* slot_set_value(libs, cons(sc, cons(sc, make_permanent_string("libc.scm"), e), slot_value(libs))); */
    }
  /* else */
  /*   { */
  /*       printf("mibl: load libc_s7.so failed\n"); */
  /*     val = s7_load(sc, "repl.scm"); */
  /*     if (val) repl_loaded = true; */
  /*   } */
  s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
  s7_gc_unprotect_at(sc, gc_loc);

  if (!val) /* s7_load was unable to find/load libc_s7.so or repl.scm */
      {
          log_error("Unable to load libc_s7.so");
          exit(EXIT_FAILURE);
    /* dumb_repl(sc); */
      }
  else
    {
      s7_provide(sc, "libc.scm");

      printf("repl_loaded? %d\n", repl_loaded); /* OBAZL */
      /* if (!repl_loaded) { */
      /*     printf("Loading repl.scm\n"); /\* OBAZL *\/ */
      /*     s7_load(sc, "s7/repl.scm"); */
      /*             /\* TOSTRING(OBAZL_RUNFILES_DIR) *\/ */
      /*             /\* "/repl.scm"); /\\* OBAZL *\\/ *\/ */
      /* } */
      /* s7_eval_c_string(sc, "((*repl* 'run))"); */
    }
#endif
#endif
}

EXPORT void s7_shutdown(s7_scheme *s7)
{
    close_error_config();
    s7_quit(s7);
}

#if defined(__APPLE__)
#define DSO_EXT ".dylib"
#else
#define DSO_EXT ".so"
#endif

void _s7_init(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace("_s7_init");
#endif

    s7 = s7_init();             /* @libs7//src:s7.c */
    /* s7_gc_on(s7, s7_f(s7)); */

#if defined(DEBUG_TRACE)
    s7_pointer lp = s7_load_path(s7);
    LOG_S7_DEBUG("*load-path* 11", lp);
    if (mibl_debug) {
        log_debug("mibl_runfiles_root: %s", utstring_body(mibl_runfiles_root));
    }
#endif
    build_ws_dir= getenv("BUILD_WORKSPACE_DIRECTORY");
    if (build_ws_dir)
        bzl_mode = true;

    UT_string *libc_s7;
    utstring_new(libc_s7);
    char *dso_dir;
    if (bzl_mode) {
        /* running under bazel run or test */

        /* add @libs7//scm to *load-path* */
        char *libs7_scmdir = realpath("../libs7/scm", NULL);
        /* log_debug("libs7_scmdir: %s", libs7_scmdir); */
        s7_add_to_load_path(s7, libs7_scmdir);
        free(libs7_scmdir);

        /* load libc_s7 */
        dso_dir = utstring_body(mibl_runfiles_root);
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_debug("bzl mode: %s", dso_dir);
#endif
        utstring_printf(libc_s7, "%s/%s",
                        dso_dir,
                        "external/libs7/src/libc_s7" DSO_EXT);

    } else {
        /* running standalone, outside of bazel */
        /* FIXME: add /usr/share/lib/libs7/scm */
        dso_dir = utstring_body(xdg_data_home);
        utstring_printf(libc_s7, "%s/%s",
                        dso_dir,
                        "mibl/libc_s7" DSO_EXT);
#if defined(DEBUG_TRACE)
        log_debug("not bzl mode: %s", dso_dir);
#endif
    }

    s7_config_libc_s7(s7, utstring_body(libc_s7)); /* @libs7//src:s7.c*/
    utstring_free(libc_s7);

    /* libc stuff is in *libc*, which is an environment
     * (i.e. (let? *libc*) => #t)
     * import the stuff we're likely to use into the root env:
     * (varlet (rootlet 'regcomp (*libc* 'regcomp) ...)
     */

    /* trap error messages */
    /* close_error_config(); */
    error_config();
    /* log_debug("running init_error_handlers"); */
    init_error_handlers_dune();

    /* tmp dir */
    char tplt[] = "/tmp/obazl.XXXXXXXXXX";
    char *tmpdir = mkdtemp(tplt);
#if defined(DEBUG_TRACE)
    if (mibl_debug)
        log_debug("tmpdir: %s", tmpdir);
#endif
    s7_define_variable(s7, "*mibl-tmp-dir*", s7_make_string(s7, tmpdir));
}

/* s7 kws used by tree-crawlers to create parsetree mibl */
void _define_mibl_s7_keywords(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace(BLU "_define_mibl_s7_keywords" CRESET);
#endif

    /* initialize s7 stuff */
    dune_project_sym = s7_make_symbol(s7, "dune-project"),
    dune_stanzas_kw = s7_make_keyword(s7, "dune-stanzas");
    dune_stanzas_sym = s7_make_symbol(s7, "dune");
    ws_path_kw = s7_make_keyword(s7, "ws-path");
    pkg_path_kw = s7_make_keyword(s7, "pkg-path");
    realpath_kw = s7_make_keyword(s7, "realpath");

    modules_kw = s7_make_keyword(s7, "modules");
    deps_kw = s7_make_keyword(s7, "deps");
    sigs_kw = s7_make_keyword(s7, "signatures");
    structs_kw = s7_make_keyword(s7, "structures");
    mll_kw = s7_make_keyword(s7, "lex");
    mly_kw = s7_make_keyword(s7, "yacc");
    mllib_kw = s7_make_keyword(s7, "mllib");
    cppo_kw = s7_make_keyword(s7, "cppo");
    files_kw   = s7_make_keyword(s7, "files");
    static_kw  = s7_make_keyword(s7, "static");
    dynamic_kw = s7_make_keyword(s7, "dynamic");

    opam_kw  = s7_make_keyword(s7, "opam");

    scripts_kw = s7_make_keyword(s7, "scripts");
    cc_kw = s7_make_keyword(s7, "cc");
    cc_srcs_kw = s7_make_keyword(s7, "srcs");
    cc_hdrs_kw = s7_make_keyword(s7, "hdrs");
}

/* policy: global vars are earmuffed */
/* Client can only override these. May be set by .miblrc or --flags,
   but if neither they must still be defined so scm code does not
   break with undefined var. */
void _define_mibl_s7_flags(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_debug(BLU "_define_mibl_s7_flags" CRESET);
#endif

    /* define global flags */
    mibl_s7_flag = mibl_s7_flags;
    while (*mibl_s7_flag != NULL) {
        /* log_info("setting flag %s", *mibl_s7_flag); */
        s7_define_variable(s7, *mibl_s7_flag, s7_f(s7));
        mibl_s7_flag++;
    }
    /* log_info("done setting mibl_s7_flags"); */

    s7_eval_c_string(s7, "(set! *mibl-build-dyads* #t)");
    s7_eval_c_string(s7, "(set! *mibl-namespace-executables* #t)");
    s7_eval_c_string(s7, "(set! *mibl-shared-deps* #t)");
    s7_eval_c_string(s7, "(set! *mibl-shared-opts* #t)");
}

void _define_mibl_s7_vars(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_s7_config)
        log_debug(BLU "_define_mibl_s7_vars" CRESET);
#endif

    s7_define_variable(s7, "*mibl-shared-ppx-pkg*",
                       s7_make_string(s7, "bzl"));
}

/* FIXME: if var does not exist, create it.
   That way users can use globals to pass args to -main.
 */
EXPORT void mibl_s7_set_flag(char *flag, bool val) {
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace(BLU "mibl_s7_set_flag:" CRESET
                  " %s: %d", flag, val);
#endif
    s7_pointer fld = s7_name_to_value(s7, flag);
    if (fld == s7_undefined(s7)) {
        if (verbose && verbosity > 1)
            log_info("Flag %s undefined, defining as %d", flag, val);
        s7_define_variable(s7, flag, val? s7_t(s7) : s7_f(s7));
        return;
    }
    utstring_renew(setter);
    utstring_printf(setter, "(set! %s %s)",
                    flag, val? "#t": "#f");
#if defined(DEBUG_TRACE)
    if (mibl_debug) {
        log_debug("Setting s7 global var: %s", utstring_body(setter));
    }
#endif
    s7_eval_c_string(s7, utstring_body(setter));
}

EXPORT void show_s7_config(void)
{
    log_info(GRN "s7 configuration summary:" CRESET);
    log_info("*features*: %s", NM_TO_STR("*features*"));
    log_info("*autoload*: %s", NM_TO_STR("*autoload*"));
    log_info("*libraries*: %s", NM_TO_STR("*libraries*"));

    log_info("mibl global flags:");

    char *exec_sexp =
        "  (let ((mibls (filter (lambda (kv) "
        "                         (string-prefix? \"*mibl-\" "
        "                            (format #f \"~A\" (car kv)))) "
        "                       (let->list (rootlet))))) "
        "    (for-each (lambda (kv) "
        "                (format #t \"~A~%\" kv)) "
        "              (sort! mibls (lambda (a b) "
        "                             (sym<? (car a) (car b))))))) "
        ;
    s7_eval_c_string(s7, exec_sexp);
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* s7_pointer lp = s7_load_path(s7); */
    /* LOG_S7_DEBUG("*load-path*" lp); */
    fflush(NULL);
    /* log_info("mibl_runfiles_root: %s", utstring_body(mibl_runfiles_root)); */

    exec_sexp =
        "(for-each (lambda (path)"
        "            (format #t \"~A~%\" path))"
        "          *load-path*)"
        ;

    s7_eval_c_string(s7, exec_sexp);

    s7_flush_output_port(s7, s7_current_output_port(s7));
    log_info(GRN "End s7 configuration summary." CRESET);
    fflush(NULL);
}

/* called by all apps */
EXPORT void mibl_s7_init(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_debug(UBLU "mibl_s7_init" CRESET);
    }
#endif
    /* _mibl_s7_init(); */
    _s7_init();

    _define_mibl_s7_keywords();

    _define_mibl_s7_flags();

    _define_mibl_s7_vars();

#if defined(DEBUG_TRACE)
    s7_pointer lp = s7_load_path(s7);
    LOG_S7_DEBUG("mibl_s7_init *load-path*", lp);
#endif

    /* FIXME: this should be a var, not a fn */
    /* s7_define_safe_function(s7, "effective-ws-root", */
    /*                         g_effective_ws_root, */
    /*                         0, 1, 0, NULL); */

}
