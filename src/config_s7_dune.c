#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
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

#include "config_s7_dune.h"

/* #if defined(DEBUG_TRACE) */
/* extern bool debug; */
/* extern bool debug_miblrc; */
/* #endif */

extern bool bzl_mode;

/* char *callback_script_file = "dune.scm"; // passed in 'data' attrib */
char *callback = "camlark_handler"; /* fn in callback_script_file  */

#if EXPORT_INTERFACE
#define TO_STR(x) s7_object_to_c_string(s7, x)
#endif

s7_scheme *s7;                  /* GLOBAL s7 */

s7_pointer dune_project_sym;
s7_pointer dune_stanzas_kw;
s7_pointer dune_stanzas_sym;
s7_pointer ws_path_kw;
s7_pointer pkg_path_kw;
s7_pointer realpath_kw;

s7_pointer modules_kw;
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
s7_pointer sort_bang;
s7_pointer string_lt;
s7_pointer _s7_acons = NULL;
s7_pointer _s7_append = NULL;
s7_pointer _s7_list_set = NULL;
s7_pointer _s7_quote = NULL;
s7_pointer _s7_set_car = NULL;
s7_pointer _s7_set_cdr = NULL;

extern int rc;

#define MIBL    "mibl"
#define MIBL_S7 MIBL "/s7"
/* #define OIBL   "mibl" */

/* XDG
   https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
 */

/*
  "The base directory defined by $XDG_DATA_HOME is considered more
  important than any of the base directories defined by
  $XDG_DATA_DIRS. The base directory defined by $XDG_CONFIG_HOME is
  considered more important than any of the base directories defined
  by $XDG_CONFIG_DIRS.
*/

/* "If $XDG_DATA_HOME is either not set or empty, a default equal to
   $HOME/.local/share should be used." */

#define XDG_DATA_HOME_SFX ".local/share"
extern UT_string *xdg_data_home;

#define XDG_CONFIG_HOME_SFX ".config"

/* state home for history files, current state to be reused on restart */
#define XDG_STATE_HOME_SFX ".local/state"

#define XDG_BIN_HOME_SFX   ".local/bin"

#define XDG_CACHE_HOME_SFX ".cache"

/* sockets, named pipes, etc. */
/* #define XDG_RUNTIME_DIR */

/* in addition we define a project-local directory for scm scripts */
#define PROJ_MIBL ".config"

/* preference-ordered base directories in addition to XDG_DATA_HOME */
#define XDG_DATA_DIRS   "/usr/local/share/:/usr/share/"
#define XDG_CONFIG_DIRS "/etc/xdg"


/* load-path script directories: sys, user, proj, in order
   sys scripts:
       run under `bazel run`: dir in runfiles containing callback script
           @camlark//scm/s7, @camlark//scm
       run directly: XDG_DATA_DIRS default: /usr/local/share/:/usr/share/
           XDG_DATA_DIRS/mibl
           XDG_DATA_DIRS/mibl/s7
   user scripts:
       ($HOME)/.config/mibl
       $XDG_DATA_HOME default: $HOME/.local/share
           XDG_DATA_HOME/mibl
           XDG_DATA_HOME/mibl/s7
   proj scripts:
       .mibl

 */

#if INTERFACE
#define LOAD_DUNE_HELP "(load-dune rootdir pathdir) rootdir is relative to $HOME; pathdir is relative to rootdir.  Change dir to rootdir and load pathdir, creating pkg-tbl"

/* NB: we need to escape #\" in C... */
#define LOAD_DUNE_FORMAL_PARAMS "s"

#endif

EXPORT s7_pointer g_effective_ws_root(s7_scheme *s7,  s7_pointer args)
{
    char *dir = NULL;
    if ( s7_is_null(s7, args) ) {
        dir = getcwd(NULL, 0);
    } else {
        s7_int args_ct = s7_list_length(s7, args);
        if (args_ct == 1) {
            s7_pointer arg = s7_car(args);
            if (s7_is_string(arg)) {
                dir = strdup((char*)s7_string(arg));
            }
        } else {
            // throw exception
        }
    }
    ews_root = effective_ws_root(dir);
    free(dir); // effective_ws_root allocates its own
    return s7_make_string(s7, ews_root);
}

EXPORT void initialize_mibl_data_model(s7_scheme *s7)
{
#if defined(DEBUG_TRACE)
    if (debug_mibl)
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
    if (trace)
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
                    "`((:@ (:name \"@\") (:path %s) "
                    "(:exports ,(make-hash-table)) "
                    "(:opam ,(make-hash-table)) "
                    "(:shared-ppx ,(make-hash-table)) "
                    "(:filegroups ,(make-hash-table)) "
                    "(:pkgs ,(make-hash-table)))))",
                    bws_root);

    s7_pointer wss = s7_eval_c_string(s7, utstring_body(init_sexp));
    (void)wss;

#if defined(DEBUG_TRACE)
    if (debug) // & verbosity > 1)
        log_debug("wss: %s\n", TO_STR(wss));
#endif
    /* /\* s7_pointer base_entry = s7_make_list(s7, 4, s7_f(s7)); *\/ */
    /* key = s7_make_symbol(s7, "name"); */
    /* datum = s7_make_symbol(s7, "@"); */
    /* s7_pointer root_ws = s7_call(s7, _s7_append, */
    /*                              s7_list(s7, 2, root_ws, */
    /*                                      s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* key   = s7_make_symbol(s7, "path"); */
    /* datum = s7_make_string(s7, bws_root); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* /\* table of "exports" - libs etc. possibly referenced as deps *\/ */
    /* key   = s7_make_symbol(s7, "exports"); */
    /* datum = s7_make_hash_table(s7, 64); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* key   = s7_make_symbol(s7, "pkgs"); */
    /* datum = s7_make_hash_table(s7, 32); */
    /* root_ws = s7_call(s7, _s7_append, */
    /*                   s7_list(s7, 2, root_ws, */
    /*                           s7_list(s7, 1, */
    /*                                   s7_list(s7, 2, key, datum)))); */
    /* if (debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* root_ws = s7_list(s7, 1, root_ws); */

    /* if (debug) */
    /*     log_debug("root_ws: %s\n", TO_STR(root_ws)); */

    /* s7_define_variable(s7, "*mibl-project*", root_ws); */

    /* return root_ws; */
}

s7_pointer _init_scheme_fns(s7_scheme *s7)
{
    /* log_debug("_init_scheme_fns\n"); */

    if (_s7_set_cdr == NULL) {
        _s7_set_cdr = s7_name_to_value(s7, "set-cdr!");
        if (_s7_set_cdr == s7_undefined(s7)) {
            log_error("unbound symbol: set-cdr!");
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "set-cdr!")));
        }
    }

    if (_s7_quote == NULL) {
        _s7_quote = s7_name_to_value(s7, "quote");
        if (_s7_quote == s7_undefined(s7)) {
            log_error("unbound symbol: quote");
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "quote")));
        }
    }

    _load_acons(s7);
    _load_assoc();
    assoc_in = _load_assoc_in();
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
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "acons")));
        }
    /* } else { */
    /*     printf("already loaded\n"); */
    }
    return _s7_acons;
}

s7_pointer _load_assoc()
{
    if (assoc == NULL) {
        assoc = s7_name_to_value(s7, "assoc");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: assoc");
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "assoc")));
        }
    }
    return assoc;
}

s7_pointer _load_assoc_in()
{
    if (assoc_in == NULL) {
        assoc_in = s7_name_to_value(s7, "assoc-in");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: assoc-in");
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "assoc-in")));
        }
    }
    return assoc_in;
}

s7_pointer _load_append()
{
    if (_s7_append == NULL) {
        _s7_append = s7_name_to_value(s7, "append");
        if (assoc == s7_undefined(s7)) {
            log_error("unbound symbol: append");
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
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
            log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
            s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                     s7_list(s7, 1, s7_make_string(s7, "list-set!")));
        }
    }
    return _s7_list_set;
}

s7_pointer _load_sort()
{
    sort_bang = s7_name_to_value(s7, "sort!");
    if (assoc == s7_undefined(s7)) {
        log_error("unbound symbol: sort!");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "sort!")));
    }
    return sort_bang;
}

s7_pointer _load_string_lt()
{
    string_lt = s7_name_to_value(s7, "string<?");
    if (assoc == s7_undefined(s7)) {
        log_error("unbound symbol: string<?");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "string<?")));
    }
    return string_lt;
}

LOCAL void _config_s7_load_path_bazel_runfiles(char *manifest)
{
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    /* put project-local .mibl on load-path if it exits. do
       not create it. */

    /* utstring_new(codept_args_file); */
    /* utstring_printf(codept_args_file, "%s/%s", utstring_body(config_mibl), codept_args_filename); */

    /* utstring_new(codept_deps_file); */
    /* utstring_printf(codept_deps_file, "%s/%s", utstring_body(config_mibl), codept_deps_filename); */

    /*
      build scripts list their scm srcs in the 'data' attrib, which
      puts them in the runfiles area. they are listed in MANIFEST, so
      we need to parse it to find out which dirs we need to add to the
      s7 load path.
     */

    /* bazel (sys) script dir */
    fp = fopen(manifest, "r");
    if (fp == NULL) {
        log_error("fopen failure %s", manifest);
        /* exit(EXIT_FAILURE); */
    }

#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("Reading MANIFEST");
#endif

    /* char *mibl_mibl = NULL; */

    s7_pointer load_dirs = s7_make_hash_table(s7, 5);
    s7_pointer sdir;

    /*
      We we need to get these paths from the MANIFEST since it has
      absolute paths, and we could be running from any dir (when used
      as a tool lib).

      We build the path list by following the MANIFEST order for
      libs7, but not mibl since the MANIFEST seems to put that
      last. So we save the latter when we find it, then when done with
      the MANIFEST we put the mibl/scm on top of the stack. Finally
      we append our list to *load-path*, which puts "." on top.
    */

    s7_pointer tmp_load_path = s7_list(s7, 0);

    while ((read = getline(&line, &len, fp)) != -1) {
        /* log_debug("Retrieved line of length %zu:", read); */
        /* log_debug("%s", line); */

        line[strcspn(line, "\n")] = '\0';    /* trim trailing newline */

        /* two tokens per line, first is path relative to exec dir,
           second is corresponding absolute path */
        char *token, *sep = " ";
        token = strtok((char*)line, sep);
        if (token != NULL) {
            token = strtok(NULL, sep);
        } else {
            /* log_debug("skipping entry"); */
            continue;
        }

        if ( (strncmp(basename(token),
                      "libc_s7.so", 10) == 0) ) {
#if defined(DEBUG_TRACE)
            if (trace)
                log_info("FOUND LIBC_S7.SO: %s", token);
#endif

            tmp_load_path =
                s7_append(s7, tmp_load_path,
                          s7_list(s7, 1,
                                  s7_make_string(s7,
                                                 dirname(token))));

            continue;
        }

        char *ext = strrchr(token, '.');
        if (ext != NULL) {
            if (strncmp(ext, ".so", 3) == 0) {
                log_info("SHARED: %s", token);
            }

            if ( (strncmp(ext, ".scm", 4) == 0) && strlen(ext) == 4) {
                char *scriptdir = dirname(token);
                /* log_info("SCRIPTDIR: %s", scriptdir); */

                /* char *substr = strstr(scriptdir, "mibl/scm"); */
                /* if (substr != NULL) { */
                /*     /\* log_debug("FOUND mibl path: %s, %s", *\/ */
                /*     /\*           line, scriptdir); *\/ */
                /*     if (mibl_mibl == NULL) { */
                /*         int len = strlen(scriptdir) + 1; */
                /*         mibl_mibl = calloc(len, 1); */
                /*         strlcpy(mibl_mibl, scriptdir, len); */
                /*     } */
                /*     continue; */
                /* } */

                sdir = s7_make_string(s7, scriptdir);
                s7_pointer r = s7_hash_table_ref(s7, load_dirs, sdir);
                if (r == s7_f(s7)) {
                    // add to hash to ensure uniqueness
                    /* log_debug("adding to hash"); */
                    s7_hash_table_set(s7, load_dirs,
                                      sdir, s7_t(s7));

                    tmp_load_path = s7_append(s7, tmp_load_path,
                                    s7_list(s7, 1,
                                            s7_make_string(s7, scriptdir)));
                }
            }
        }
    }
    fclose(fp);

    /* log_debug("tmp_load_path: %s", TO_STR(tmp_load_path)); */
    /* tmp_load_path = s7_cons(s7, */
    /*                         s7_make_string(s7, mibl_mibl), */
    /*                         tmp_load_path); */
    /* log_debug("2 tmp_load_path: %s", TO_STR(tmp_load_path)); */

    /* now put default "." on top of tmp stack */
    /* s7_pointer loadp = s7_load_path(s7); */
    /* tmp_load_path = s7_append(s7, loadp, tmp_load_path); */
    /* log_debug("lp: %s", s7_object_to_c_string(s7, tmp_load_path)); */

    /* replace *load-path* with our shiny new stack */
    s7_define_variable(s7, "*load-path*", tmp_load_path);
}

LOCAL void _config_s7_load_path_bws_root(void)
{
    /* char *project_script_dir = PROJ_MIBL; */

    UT_string *proj_script_dir;
    utstring_new(proj_script_dir);
    utstring_printf(proj_script_dir, "%s/%s",
                    ews_root, PROJ_MIBL);
    rc = access(utstring_body(proj_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_warn("Not found: project script dir %s not found",
                     utstring_body(proj_script_dir));
    } else {
        /* if (verbose) */
        /*     log_debug("adding to *load-path*: %s", */
        /*              utstring_body(proj_script_dir)); */
        s7_add_to_load_path(s7, utstring_body(proj_script_dir));
    }
}

LOCAL void _config_user_load_path(void)
{
    char *_user_script_dir = HOME_MIBL;
    UT_string *user_script_dir;

    utstring_new(user_script_dir);
    utstring_printf(user_script_dir, "%s/%s",
                    homedir, _user_script_dir);

    rc = access(utstring_body(user_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: user script dir: %s",
                     utstring_body(user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(user_script_dir));
        s7_add_to_load_path(s7, utstring_body(user_script_dir));
    }
}

LOCAL void _config_s7_load_path_xdg_home(void)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_trace("_config_s7_load_path_xdg_home");
#endif

    UT_string *xdg_script_dir;

    utstring_new(xdg_script_dir);
    // note the leading _
    char *_xdg_data_home = getenv("XDG_DATA_HOME");

    utstring_new(xdg_data_home);
    if (_xdg_data_home == NULL) {
        _xdg_data_home = getenv("HOME");
        utstring_printf(xdg_data_home, "%s/%s",
                        _xdg_data_home, XDG_DATA_HOME_SFX);
    } else {
        utstring_printf(xdg_data_home, "%s", _xdg_data_home);
    }
    if (verbose)
        log_trace("xdg_data_home: %s", utstring_body(xdg_data_home));

    /* start with lib/libc_s7.so, mibl dlopens it */

    // instead of adding another dir with only one file to the load-path,
    // we store libc_s7.o in .local/share/mibl, which is already on the path

    /* utstring_printf(xdg_script_dir, */
    /*                 "%s/.local/share/mibl/lib", */
    /*                 utstring_body(xdg_data_home)); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                   utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    utstring_renew(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/mibl/s7",
                    utstring_body(xdg_data_home));
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: %s.", utstring_body(xdg_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(xdg_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    }

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/dune"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/meta"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/opam"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    utstring_renew(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/mibl",
                    utstring_body(xdg_data_home));
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: %s.", utstring_body(xdg_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                      utstring_body(xdg_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    }
}

LOCAL __attribute__((unused)) void _config_s7_load_path_xdg_sys(void)
{
    UT_string *xdg_script_dir;

    /* system mibl script dirs:
       $XDG_DATA_DIRS/mibl
    */
    char *xdg_data_dirs = getenv("XDG_DATA_DIRS");
    if (xdg_data_dirs == NULL) {
        xdg_data_dirs = XDG_DATA_DIRS;
    }

    utstring_new(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/%s",
                    xdg_data_dirs, "mibl");
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: %s.", utstring_body(xdg_script_dir));
    } else {
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    }

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 xdg_data_dirs, "s7"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: obazl s7 system script dir at: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

}

EXPORT void set_load_path(void) // char *scriptfile)
{
    /* char *_wd = getcwd(NULL, 0); */

#if defined(DEBUG_TRACE)
    if (debug) {
        s7_pointer lp = s7_load_path(s7);
        log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp));
    }
#endif

    /* FIXME: reliable way to detect if we're run by bazel */

    /* https://docs.bazel.build/versions/main/user-manual.html#run
bazel run is similar, but not identical, to directly invoking the binary built by Bazel and its behavior is different depending on whether the binary to be invoked is a test or not. When the binary is not a test, the current working directory will be the runfiles tree of the binary. When the binary is a test, the current working directory will be the exec root and a good-faith attempt is made to replicate the environment tests are usually run in.
  */

    /* so if we find MANIFEST, we're running a test? */
    /* UT_string *manifest; */
    /* utstring_new(manifest); */
    /* utstring_printf(manifest, "../MANIFEST"); */
    /* if (debug) */
    /*     log_debug("MANIFEST: %s", utstring_body(manifest)); */

    char *manifest = "../MANIFEST";
    rc = access(manifest, R_OK);

    if (rc) {
        if (verbose) log_info("Configuring for non-bazel env.");
        _config_s7_load_path_xdg_home();
        /* _config_s7_load_path_xdg_sys(); */
        _config_user_load_path();
    } else {
        if (verbose) log_info("Configuring for bazel env.");
        /* Running under bazel: do NOT put XDG dirs in load-path. This
           ensures a pristine runtime env. The user can always add
           directories to load-path. The only exception is the
           project-local script directory in <projroot>/.mibl . */
/*         s7_pointer lp = s7_load_path(s7); */
/* #if defined(DEBUG_TRACE) */
/*         if (debug) { */
/*             log_debug("1 *LOAD-PATH*: %s", TO_STR(lp)); */
/*         } */
/* #endif */
        _config_s7_load_path_bazel_runfiles(manifest);
        s7_pointer lp = s7_load_path(s7);
        (void)lp;
#if defined(DEBUG_TRACE)
        if (debug) {
            log_debug("2 *LOAD-PATH*: %s", TO_STR(lp));
        }
#endif
    }
    _config_s7_load_path_bws_root();

    s7_add_to_load_path(s7, ".");

    /* s7_pointer lp = s7_load_path(s7); */
    /* s7_pointer new_lp = s7_call(s7, del, */
    /*                             s7_list(s7, 2, pred, lp)); */

    /* log_debug("new lp: %s", s7_object_to_c_string(s7, new_lp)); */


    /* if (debug) { */
    /*     s7_pointer lp = s7_load_path(s7); */
    /*     log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp)); */
    /* } */
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

/* defined in s7.c, we need the prototype */
void s7_config_libc_s7(s7_scheme *sc, char *libc_s7_path);

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

void _mibl_s7_init(void)
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("_mibl_s7_init");
#endif

    s7 = s7_init();             /* @libs7//src:s7.c */

#if defined(DEBUG_TRACE)
    log_debug("runfiles_root: %s", utstring_body(runfiles_root));
#endif

    UT_string *libc_s7;
    utstring_new(libc_s7);
    char *dso_dir;
    if (bzl_mode) {
        dso_dir = utstring_body(runfiles_root);
#if defined(DEBUG_TRACE)
        log_debug("bzl mode: %s", dso_dir);
#endif
        utstring_printf(libc_s7, "%s/%s",
                        dso_dir,
                        "external/libs7/src/libc_s7" DSO_EXT);
    } else {
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

    /* exit(1); */

    /* trap error messages */
    /* close_error_config(); */
    error_config();
    init_error_handlers();

    /* tmp dir */
    char tplt[] = "/tmp/obazl.XXXXXXXXXX";
    char *tmpdir = mkdtemp(tplt);
#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("tmpdir: %s", tmpdir);
#endif
    s7_define_variable(s7, "*tmp-dir*", s7_make_string(s7, tmpdir));
}

/* FIXME: rename s7_configure_for_dune */
EXPORT s7_scheme *s7_configure(void)
{
#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("s7_configure");
#endif

    _mibl_s7_init();

    s7_define_variable(s7, "*debugging*", s7_f(s7));
    s7_define_variable(s7, "*debug-alias*", s7_f(s7));
    s7_define_variable(s7, "*debug-emit*", s7_f(s7));
    s7_define_variable(s7, "*debug-executables*", s7_f(s7));
    s7_define_variable(s7, "*debug-genrules*", s7_f(s7));
    s7_define_variable(s7, "*debug-mibl*", s7_f(s7));
    s7_define_variable(s7, "*debug-ppx*", s7_f(s7));

    s7_define_variable(s7, "*dunefile-count*", s7_make_integer(s7, 0));

    /* logging */
    s7_define_variable(s7, "*log-exports*", s7_f(s7));
    s7_define_variable(s7, "*log-mibl*", s7_f(s7));
    s7_define_variable(s7, "*log-parsetree*", s7_f(s7));
    s7_define_variable(s7, "*log-starlark*", s7_f(s7));

    /* emit to files (e.g. BUILD.bazel) */
    s7_define_variable(s7, "*emit-mibl*", s7_f(s7));
    s7_define_variable(s7, "*emit-parsetree*", s7_f(s7));
    s7_define_variable(s7, "*emit-starlark*", s7_f(s7));

    if (bws_root) {
        s7_define_variable(s7, "ws-root", s7_make_string(s7, bws_root));
    } else {
        /* should have been set by bazel_configure */
        log_error("bws_root not set\n");
        exit(EXIT_FAILURE);
    }

    s7_define_safe_function(s7, "effective-ws-root",
                            g_effective_ws_root,
                            0, 1, 0, NULL);

    s7_define_function(s7, "load-dune", g_load_project,
                       0, 2, 0,
                       /* LOAD_DUNE_FORMAL_PARAMS, */
                       LOAD_DUNE_HELP);

    /* generate obazl code for top-down namespacing */
    s7_define_variable(s7, "*ns-topdown*", s7_t(s7));

    /* map dune library (wrapped) to :ns-archive or :ns-library */
    s7_define_variable(s7, "*wrapped-libs-to-ns-archives*", s7_t(s7));

    /* true: map dune library (unwrapped) to :archive
       false: map to :library */
    s7_define_variable(s7, "*unwrapped-libs-to-archives*", s7_t(s7));

    /* emit menhir target instead of ocamlyacc */
    s7_define_variable(s7, "*menhir*", s7_f(s7));

    /* emit ocaml_signature for every sigfile target */
    s7_define_variable(s7, "*build-dyads*", s7_t(s7));

    /* put ppx driver in same pkg as the ppx_executable */
    s7_define_variable(s7, "*local-ppx-driver*", s7_t(s7));

    /* use ":" for locally defined ppxes */
    s7_define_variable(s7, "*shared-ppx-pkg*", s7_make_string(s7, "bzl/ppx"));

    /* list of pkgs whose stanzas share deps */
    /* FIXME: by default we always shared deps across stanzas, per pkg */
    s7_define_variable(s7, "*shared-deps*", s7_list(s7, 0));

    /* only emit bazel code for pkg listed here (string); nil means no
       exclusion. set by cli -p option */
    s7_define_variable(s7, "*emit-bazel-pkg*", s7_f(s7));

    /* executable 'main' attr may be a module or an 'execlib'.
       if this flag is true, use execlib; otherwise use module.
    */
    s7_define_variable(s7, "*dune-execlib-includes-main*", s7_f(s7));

    s7_define_variable(s7, "*emit-rules-jsoo*", s7_t(s7));
    s7_define_variable(s7, "*emit-rules-js*", s7_t(s7));
    s7_define_variable(s7, "*emit-rules-swc*", s7_f(s7));
    s7_define_variable(s7, "*emit-rules-closure*", s7_f(s7));

    /* populate pkgs list, so scheme code can use it */
    char **p = NULL;
    s7_pointer _s7_pkgs = s7_nil(s7);
    while ( (p=(char**)utarray_next(mibl_config.pkgs, p))) {
#if defined(DEBUG_TRACE)
        if (debug) printf("Adding to pkgs list: %s\n", *p);
#endif
        _s7_pkgs = s7_cons(s7, s7_make_string(s7, *p), _s7_pkgs);
    }
    s7_define_variable(s7, "*dump-pkgs*", _s7_pkgs);

    /* populate exclusions list, so scheme code can use it */
    p = NULL;
    s7_pointer _s7_exclusions = s7_nil(s7);
    while ( (p=(char**)utarray_next(mibl_config.exclude_dirs, p))) {
#if defined(DEBUG_TRACE)
        if (debug_miblrc)
            log_debug("Adding to exlusions list: %s",*p);
#endif
        _s7_exclusions = s7_cons(s7, s7_make_string(s7, *p), _s7_exclusions);
    }
#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("exclusions list: %s", TO_STR(_s7_exclusions));
#endif
    s7_define_variable(s7, "*scan-exclusions*", _s7_exclusions);
    /* p = utarray_find(mibl_config.exclude_dirs, */
    /*                  &ptr, */
    /*                  /\* &ftsentry->fts_path, *\/ */
    /*                  strsort); */

    /* initialize s7 stuff */
    //FIXME: do this in config, no need to rerun for each load_project
    dune_project_sym = s7_make_symbol(s7, "dune-project"),
    dune_stanzas_kw = s7_make_keyword(s7, "dune-stanzas");
    dune_stanzas_sym = s7_make_symbol(s7, "dune");
    ws_path_kw = s7_make_keyword(s7, "ws-path");
    pkg_path_kw = s7_make_keyword(s7, "pkg-path");
    realpath_kw = s7_make_keyword(s7, "realpath");

    modules_kw = s7_make_keyword(s7, "modules");
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
    cc_srcs_kw = s7_make_keyword(s7, "cc-srcs");
    cc_hdrs_kw = s7_make_keyword(s7, "cc-hdrs");

    set_load_path(); //callback_script_file);

    /* init_glob(s7); */

    /* s7_config_repl(s7); */
    /* s7_repl(s7); */

    s7_load(s7, "dune.scm");

    _init_scheme_fns(s7);       /* call _after_ loading dune.scm */

    chdir(bws_root);            /* always run from base ws root */

    return s7;
}
