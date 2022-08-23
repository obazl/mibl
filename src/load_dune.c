#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <fcntl.h>              /* open() */
#if INTERFACE
#include <fts.h>
#endif
#include <libgen.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#if INTERFACE
#include "utarray.h"
#include "utstring.h"
#include "s7.h"
#endif

#include "log.h"
#include "load_dune.h"

UT_array  *segs;
UT_string *group_tag;

int dunefile_ct = 0;
int file_ct = 0;
int dir_ct  = 0;

void _indent(int i)
{
    /* printf("_indent: %d\n", i); */
    /* for (; i > 0; i--) */
    /*     printf("    "); */
}

UT_string *dunefile_name;

s7_int dune_gc_loc = -1;

LOCAL s7_pointer _read_dunefile(char *path) //, char *fname)
{
    if (debug)
        log_debug("_read_dunefile %s", path); //, fname);

    /* read dunefile */
    utstring_renew(dunefile_name);
    utstring_printf(dunefile_name, "%s", path); //, fname);
    /* log_debug("reading dunefile: %s", utstring_body(dunefile_name)); */

    s7_pointer port = s7_open_input_file(s7,
                                         utstring_body(dunefile_name),
                                         "r");
    if (!s7_is_input_port(s7, port)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            s7_shutdown(s7);
            exit(EXIT_FAILURE);
        }
    } else {
        if (trace)
            log_trace("opened input port for %s",
                      utstring_body(dunefile_name));
    }

    dune_gc_loc = s7_gc_protect(s7, port);

    s7_pointer stanzas = s7_list(s7, 0); // s7_nil(s7));

    close_error_config();
    init_error_handling();
    error_config();

    if (trace)
        log_trace("reading stanzas of %s", utstring_body(dunefile_name));
    /* repeat until all objects read */
    while(true) {
        /* printf("reading stanza\n"); */

        s7_pointer stanza = s7_read(s7, port);
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));

        if ((errmsg) && (*errmsg)) {
            if (debug)
                log_error("[%s\n]", errmsg);
            s7_gc_unprotect_at(s7, dune_gc_loc);
            s7_close_input_port(s7, port);
            //if ".)", read file into buffer, convert to "\.)", then
            // read with the scheme reader
            if (strstr(errmsg, "BADDOT") != NULL) {
                log_info("fixing baddot in %s", utstring_body(dunefile_name));
                s7_gc_unprotect_at(s7, dune_gc_loc);
                s7_close_input_port(s7, port);
                /* clear out old error */
                close_error_config();
                init_error_handling();
                error_config();

                // FIXME: test case: 'include' after baddot
                s7_pointer fixed = fix_baddot(dunefile_name);
                if (debug) log_debug(RED "FIXED:" CRESET " %s",
                                     TO_STR(fixed));
                if (s7_is_null(s7,stanzas)) {
                    // fixed is a list of stanzas
                    stanzas = fixed;
                } else{
                    stanzas = s7_append(s7, stanzas, fixed);
                }
            }
            close_error_config();
            init_error_handling();
            error_config();
            /* s7_quit(s7); */
            /* exit(EXIT_FAILURE); */
            break;
        }
        /* printf("readed stanza\n"); */

        if (stanza == s7_eof_object(s7)) {
            if (trace) log_trace("readed eof");
            break;
        }

        /* log_debug("SEXP: %s", TO_STR(stanza)); */
        /* if (debug) */
        /*     log_debug("stanza: %s", TO_STR(s7_car(stanza))); */

        if (s7_is_pair(stanza)) {
            if (s7_is_equal(s7, s7_car(stanza),
                            s7_make_symbol(s7, "include"))) {
                log_debug("FOUND (include ...)");
                /* we can't insert a comment, e.g. ;;(include ...)
                   instead we would have to put the included file in an
                   alist and add a :comment entry. but we needn't bother,
                   we're not going for roundtrippability.
                */

                s7_pointer inc_file = s7_cadr(stanza);
                log_debug("    including %s", TO_STR(inc_file));
                UT_string *dunepath;
                utstring_new(dunepath);
                utstring_printf(dunepath, "%s/%s",
                                //FIXME: dirname may mutate its arg
                                dirname(path), TO_STR(inc_file));
                s7_pointer nested = _read_dunefile(utstring_body(dunepath));
                /* s7_pointer nested = _read_dunefile(path, TO_STR(inc_file)); */
                log_debug("nested:", TO_STR(nested));
                stanzas = s7_append(s7,stanzas, nested);
                /* alt: (:include "(include dune.inc)" (included ...)) */
            } else {
                if (s7_is_null(s7,stanzas)) {
                    stanzas = s7_cons(s7, stanza, stanzas);
                } else{
                    stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
                }
            }
        } else {
            /* stanza not a pair - automatically means corrupt dunefile? */
            log_error("corrupt dune file? %s\n", utstring_body(dunefile_name));
            /* if exit-on-error */
            exit(EXIT_FAILURE);
        }
    }
    s7_gc_unprotect_at(s7, dune_gc_loc);
    s7_close_input_port(s7, port);

    return stanzas;
    /* s7_close_input_port(s7, port); */
    /* s7_gc_unprotect_at(s7, gc_loc); */
}

bool _is_ws_root(FTSENT *ftsentry)
{
    if (trace)
        log_trace("_is_ws_root: %s", ftsentry->fts_path);

    UT_string *pathdir;
    utstring_new(pathdir);
    utstring_printf(pathdir, "%s", ftsentry->fts_path);
    utstring_printf(pathdir, "%s", "/WORKSPACE.bazel");
    /* log_trace("accessing %s", utstring_body(pathdir)); */
    int rc = access(utstring_body(pathdir), R_OK);
    /* log_debug("RC: %d", rc); */
    if (!rc) {
        if (trace) log_trace("true");
        return true;
    } else {
        utstring_new(pathdir);
        utstring_printf(pathdir, "%s", ftsentry->fts_path);
        utstring_printf(pathdir, "%s", "/WORKSPACE");
        rc = access(utstring_body(pathdir), R_OK);
        if (!rc) {
            if (trace) log_trace("true");
            return true;
        }
    }
    if (trace) log_trace("false");
    return false;
}

LOCAL bool _this_is_hidden(FTSENT *ftsentry)
{
    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            if (trace)
                log_trace(RED "Excluding" CRESET " hidden dir: %s/%s\n",
                          ftsentry->fts_path, ftsentry->fts_name);
            return true;
            /* } else { */
            /*     printf("ROOT DOT dir\n"); */
        }
    }
    return false;
}

/*
  create a pkg-tbl entry if dir contains a dune file or at least one
  OCaml source file.
 */
LOCAL void _handle_dir(s7_pointer pkg_tbl, FTS* tree, FTSENT *ftsentry)
{
    if (debug) {
        log_debug(BLU "_handle_dir:" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
        log_info("%-20s%s", "base ws:", bws_root);
        log_info("%-20s%s", "effective ws:",ews_root);
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }

    UT_string *dune_test;
    utstring_new(dune_test);
    utstring_printf(dune_test, "%s/%s", ftsentry->fts_path, "dune");
    int rc = access(utstring_body(dune_test), F_OK);
    if (rc) {
        if (debug)
            log_warn(RED "dunefile not found in: " CRESET "%s",
                      ftsentry->fts_path);
        /* return; */
    }

    if (strncmp(ews_root, ftsentry->fts_path,
                 strlen(ftsentry->fts_path)) != 0) {
        /* is this a ws root (other than base ws)? */
        if (_is_ws_root(ftsentry)) {
            /* log_debug("SKIPPING ws root: %s", ftsentry->fts_path); */
            /* do not process embedded subrepos yet */
            /* fts_set(tree, ftsentry, FTS_SKIP); */
            /* return; */
        }
    }

    /* stdout */
    _indent(ftsentry->fts_level);
    /* printf("%d. %s", */
    /*        ftsentry->fts_level, */
    /*        /\* ftsentry->fts_name, *\/ */
    /*        ftsentry->fts_path); */
    /* printf("\n"); */

    /* create pkg table entry for this dir */

    /* // fts_path is relative to traversal root */
    /* printf("DIR path    %s\n", ftsentry->fts_path); */

    /* // fts_accpath is relative to current dir */
    /* printf("DIR accpath %s\n", ftsentry->fts_accpath); */

    s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
    /* if (trace) */
    /*     log_trace(RED "pkg-path: %s" CRESET, TO_STR(key)); */
    /* s7_pointer test_assoc = s7_list(s7, 2, */
    /*                                 s7_make_keyword(s7, "test"), */
    /*                                 s7_make_symbol(s7, "dummy")); */
    /* s7_pointer result = s7_hash_table_set(s7, pkg_tbl, key, */
    /*                                       s7_list(s7, 1, test_assoc)); */

    /* MB: result of realpath must be freed */
    char *rpath = realpath(ftsentry->fts_path, NULL);
    /* FIXME: check result */
    /* s7_pointer result = */

    /* printf("PKG TBL: %s\n", TO_STR(pkg_tbl)); */

    s7_hash_table_set(s7, pkg_tbl, key,
                      s7_list(s7, 3,
                              //FIXME: use a ws-alist instead of
                              //annotating each pkg with :ws-path
                              s7_list(s7, 2, ws_path_kw,
                                      s7_make_string(s7, ews_root)),
                              s7_list(s7, 2, pkg_path_kw,
                                      key),
                              s7_list(s7, 2, realpath_kw,
                                      s7_make_string(s7, rpath))));
}

static char principal[256];

LOCAL char *_module_name(FTSENT *ftsentry, char *ext)
{
    strlcpy(principal, ftsentry->fts_name, 256);
    principal[ext - ftsentry->fts_name] = '\0';
    principal[0] = toupper(principal[0]);
    return (char *)principal;
}

LOCAL char *_principal_name(FTSENT *ftsentry, char *ext)
{
    strlcpy(principal, ftsentry->fts_name, 256);
    principal[ext - ftsentry->fts_name] = '\0';
    /* principal[0] = toupper(principal[0]); */
    return (char *)principal;
}

static s7_pointer alist_updt_in_fn(s7_scheme *s, s7_pointer args)
{
    log_debug("running updater");
    return(s7_make_string(s7, "HELLO"));
}

LOCAL bool _exclusions(FTSENT *ftsentry, char *ext)
{
    if (ext == NULL) {
    } else {
        if (strncmp(ext, ".gitignore", 10) == 0)
            return true;
    }
    return false;
}

#define TAG_MLI 0
#define TAG_MLI_DYN 1
#define TAG_ML  2
#define TAG_ML_DYN  3
#define TAG_MLL 4
#define TAG_MLY 5

/* char *_get_extension(char *filename) */
/* { */
/*     char *pt = strrchr(filename, '.'); */
/*     return pt; */
/* } */

LOCAL void _update_pkg_files(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    if (debug)
        log_debug("_update_pkg_files: %s, ext: %s",
                  ftsentry->fts_name, ext);

    if (_exclusions(ftsentry, ext)) {
        if (debug)
            log_warn("excluding %s", ftsentry->fts_name);
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (debug) */
    /*     log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) */
    /*     log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    char *file_ext =  strrchr(ftsentry->fts_name, '.');
    file_ext++; // exclude dot

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
        if (debug)
            log_debug("no entry for this pkg: %s", pkg_name);
    } else {
        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, files_kw, static_kw);
        s7_pointer files_assoc = s7_call(s7, assoc_in,
                                         s7_list(s7, 2,
                                                 keypath,
                                                 pkg_alist));
        if (debug) log_debug("files_assoc %s", TO_STR(files_assoc));

        if (files_assoc == s7_f(s7)) {
            if (debug)
                log_debug("initializing (:files (:static (:type . fname))) list");

            /* s7_pointer file_pair = */
            /*     s7_list(s7, 1, */
            /*             /\* s7_make_keyword(s7, file_ext), *\/ */
            /*             s7_make_string(s7, ftsentry->fts_name)); */

            s7_pointer static_assoc = s7_list(s7, 2,
                                              static_kw,
                                              s7_make_string(s7, ftsentry->fts_name));
                                              /* file_pair); */

            s7_pointer file_list =
                s7_list(s7, 1, s7_list(s7, 2,
                                       files_kw,
                                       static_assoc
                                       ));
            if (debug)
                log_debug("file_list: %s", TO_STR(file_list));

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            if (debug)
                log_debug("new pkg: %s", TO_STR(new_pkg));

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);

        } else {
            /* assoc-in '(:files :static) returns assoc (:static ...),
               but we need the alist */
            s7_pointer files_alist = s7_cdr(files_assoc);

            /* if (debug) { */
            /*     log_debug("updating :files"); */
            /*     log_debug("files_alist: %s", TO_STR(files_alist)); */
            /* } */

            /* s7_pointer file_pair = */
            /*     s7_list(s7, 2, */
            /*             s7_make_keyword(s7, file_ext), */
            /*             s7_make_string(s7, ftsentry->fts_name)); */
            /* log_debug("new file_pair: %s", TO_STR(file_pair)); */

            s7_pointer new_files_alist =
                s7_append(s7,
                /* s7_list(s7, 2, */
                /* s7_cons(s7, */
                          files_alist,
                          s7_list(s7, 1,
                                  s7_make_string(s7, ftsentry->fts_name))
                        /* file_pair, */
                          );
            /* log_debug("new files_alist: %s", */
            /*            TO_STR(new_files_alist)); */

            /* s7_pointer sort      = _load_sort(); */
            /* s7_pointer string_lt = _load_string_lt(); */
            /* s7_pointer sorted */
            /*     = s7_call(s7, sort, s7_list(s7, 2, */
            /*                                 new_files_alist, */
            /*                                 string_lt)); */

            /* log_debug("new files_alist sorted: %s", */
            /*           TO_STR(sorted)); */

            s7_set_cdr(files_assoc, new_files_alist);
            /* log_debug("files_assoc: %s", */
            /*           TO_STR(files_assoc)); */

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */

        }
    }
}

LOCAL void _update_pkg_script_files(s7_pointer pkg_tbl,
                                    FTSENT *ftsentry, char *ext)
{
    if (debug)
        log_debug("_update_pkg_script_files: %s, ext: %s",
                  ftsentry->fts_name, ext);

    if (_exclusions(ftsentry, ext)) {
        if (debug)
            log_warn("excluding %s", ftsentry->fts_name);
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (debug) */
    /*     log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) */
    /*     log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
        if (debug)
            log_debug("no entry for this pkg");
    } else {
        s7_pointer assoc = _load_assoc();

        s7_pointer scripts_list
            = s7_call(s7, assoc, s7_list(s7, 2, scripts_kw, pkg_alist));
        if (debug)
            log_debug("scripts_list %s",
                      TO_STR(scripts_list));

        if (scripts_list == s7_f(s7)) {
            if (debug)
                log_debug("adding :scripts");

            s7_pointer file_list =
                s7_list(s7, 1,
                        s7_list(s7, 2,
                                scripts_kw,
                                s7_make_string(s7, ftsentry->fts_name)));
            if (debug)
                log_debug("file_list: %s", TO_STR(file_list));

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            if (debug)
                log_debug("new pkg: %s", TO_STR(new_pkg));

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);

        } else {
            if (debug) {
                log_debug("updating :scripts");
                /* log_debug("srcs_alist: %s", */
                /*        TO_STR(srcs_alist)); */
                log_debug("scripts_list: %s",
                       TO_STR(scripts_list));
            }
            s7_pointer new_scripts_list =
                s7_cons(s7,
                        s7_make_string(s7, ftsentry->fts_name),
                        s7_cdr(scripts_list));
            log_debug("new scripts_list: %s",
                       TO_STR(new_scripts_list));

            s7_pointer sort = s7_name_to_value(s7, "sort!");
            s7_pointer lt = s7_name_to_value(s7, "string<?");
            s7_pointer sorted
                = s7_call(s7, sort, s7_list(s7, 2,
                                            new_scripts_list,
                                            lt));

            /* log_debug("new scripts_list sorted: %s", */
            /*           TO_STR(sorted)); */

            s7_set_cdr(scripts_list, sorted);
            log_debug("scripts_list: %s",
                      TO_STR(scripts_list));

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */
        }
    }
}

LOCAL void _update_pkg_deps(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    char *pkg_name = dirname(ftsentry->fts_path);
    char *fname = ftsentry->fts_name;
    char *mname = _module_name(ftsentry, ext);
    if (verbose) {
        log_info(BLU "_update_pkg_deps:" CRESET " %s; ", mname);
        log_info("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }

    char *ml_name = strdup(ftsentry->fts_name);

    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug) log_debug("pkg_key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist));

    /* if (pkg_alist == s7_f(s7)) { */
    /*     if (debug) { */
    /*         log_debug("no dunefile in this directory"); */
    /*         /\* FIXME?? *\/ */
    /*     } */
    /* } else { */
    /*     s7_pointer mname_sym   = s7_make_symbol(s7, mname); */

    /*     assoc_in = _load_assoc_in(); */
    /*     s7_pointer keypath = s7_list(s7, 1, modules_kw); */
    /*     s7_pointer modules_alist = s7_call(s7, assoc_in, */
    /*                                        s7_list(s7, 2, */
    /*                                                keypath, */
    /*                                                pkg_alist)); */

    /*     s7_pointer ml_assoc = s7_cons(s7, */
    /*                                   s7_make_keyword(s7, */
    /*                                                   ftype == TAG_ML */
    /*                                                   ?"ml" */
    /*                                                   :ftype == TAG_MLI */
    /*                                                   ?"mli" */
    /*                                                   :"UNKNOWN" */
    /*                                                   ), */
    /*                                   s7_make_symbol(s7, fname)); */
    /*     if (debug) */
    /*         log_debug("ml_assoc: %s", TO_STR(ml_assoc)); */

    /*     /\* if (srcs_alist == s7_f(s7)) { *\/ */
    /*     if (modules_alist == s7_f(s7)) { */
    /*         if (debug) */
    /*             log_debug("INITIALIZING :modules field"); */
    /*         /\* (acons :srcs *\/ */
    /*         /\*  ((:modules ((:Foo ((:ml foo.ml) (:mli foo.mli)) *\/ */
    /*         /\*              (:Bar ((:ml bar.ml) (:mli bar.mli))))))) *\/ */
    /*         /\*  pkg_alist) *\/ */
    /*         /\* s7_pointer msrcs_alist = s7_list(s7, 1, ml_assoc); *\/ */
    /*         /\* log_debug("msrcs_alist: %s", TO_STR(msrcs_alist)); *\/ */

    /*         s7_pointer module_assoc = s7_list(s7, 2, */
    /*                                           mname_sym, */
    /*                                           ml_assoc); */
    /*         /\* msrcs_alist); *\/ */
    /*         if (debug) */
    /*             log_debug("module_assoc: %s", TO_STR(module_assoc)); */

    /*         /\* s7_pointer modules_alist = s7_list(s7, 1, module_assoc); *\/ */
    /*         /\* log_debug("modules_alist: %s", *\/ */
    /*         /\*        TO_STR(modules_alist)); *\/ */

    /*         /\* s7_pointer statics_assoc = *\/ */
    /*         /\*     s7_list(s7, 2, *\/ */
    /*         /\*             static_kw, *\/ */
    /*         /\*             module_assoc); *\/ */


    /*         s7_pointer modules_assoc = s7_list(s7, 2, */
    /*                                            modules_kw, */
    /*                                            // statics_assoc */
    /*                                            module_assoc); */
    /*         if (debug) log_debug("modules_assoc: %s", TO_STR(modules_assoc)); */

    /*         s7_pointer new_pkg_alist = s7_append(s7, pkg_alist, */
    /*                                              s7_list(s7, 1, */
    /*                                                      modules_assoc)); */
    /*         /\* if (debug) *\/ */
    /*         /\*     log_debug("pkg_alist: %s", *\/ */
    /*         /\*            TO_STR(new_pkg_alist)); *\/ */

    /*         s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist); */
    /*     } else { */
    /*         if (debug) { */
    /*             log_debug("UPDATING :modules"); */
    /*             log_debug("modules_alist: %s", TO_STR(modules_alist)); */
    /*             log_debug("mname_sym: %s", TO_STR(mname_sym)); */
    /*         } */

    /*         /\* s7_pointer assoc_in = _load_assoc_in(); *\/ */

    /*         s7_pointer keypath = s7_list(s7, 2, //  3, */
    /*                                      /\* srcs_kw, *\/ */
    /*                                      modules_kw, */
    /*                                      /\* static_kw, *\/ */
    /*                                      mname_sym); */
    /*         if (debug) { */
    /*             log_debug("assoc-in: %s", TO_STR(assoc_in)); */
    /*             log_debug("keypath: %s", TO_STR(keypath)); */
    /*             /\* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); *\/ */
    /*         } */

    /*         s7_pointer module_alist = s7_call(s7, assoc_in, */
    /*                                           s7_list(s7, 2, */
    /*                                                   keypath, */
    /*                                                   pkg_alist)); */

    /*         if (debug) { */
    /*             log_debug("module_alist: %s", TO_STR(module_alist)); */
    /*         } */
    /*         if (module_alist == s7_f(s7)) { */
    /*             /\* new *\/ */
    /*             if (debug) */
    /*                 log_debug(RED "ADDING" CRESET " module %s to %s", */
    /*                           TO_STR(mname_sym), TO_STR(modules_alist)); */
    /*             /\* s7_pointer keypath = s7_list(s7, 1, modules_kw); *\/ */
    /*             /\* if (debug) log_debug("trying keypath: %s", *\/ */
    /*             /\*                      TO_STR(keypath)); *\/ */
    /*             if (debug) log_debug("modules_alist: %s", */
    /*                                  s7_object_to_c_string(s7, */
    /*                                                        modules_alist)); */

    /*             s7_pointer modules_alist_cdr = s7_cdr(modules_alist); */
    /*             if (debug) */
    /*                 log_debug("modules_alist_cdr: %s", */
    /*                    TO_STR(modules_alist_cdr)); */

    /*             s7_pointer module_assoc = */
    /*                 s7_list(s7, 1, s7_list(s7, 2, mname_sym, ml_assoc)); */
    /*             if (debug) log_debug("new module_assoc: %s", */
    /*                                  TO_STR(module_assoc)); */

    /*             s7_pointer new_modules_alist_cdr = */
    /*                 s7_append(s7, modules_alist_cdr, module_assoc); */
    /*                 /\* s7_cons(s7, module_assoc, modules_alist_cdr); *\/ */
    /*             if (debug) { */
    /*                 log_debug("new_modules_alist_cdr: %s", */
    /*                    TO_STR(new_modules_alist_cdr)); */
    /*             } */

    /*             s7_pointer new_modules_alist */
    /*                 = s7_set_cdr(modules_alist, new_modules_alist_cdr); */

    /*             if (debug) { */
    /*                 log_debug("new_modules_alist: %s", */
    /*                           TO_STR(new_modules_alist)); */
    /*             } */
    /*         } else { */
    /*             /\* update *\/ */
    /*             if (debug) log_debug(RED "UPDATING" CRESET " module_alist: %s", */
    /*                                  TO_STR(module_alist)); */

    /*             s7_pointer modules_alist_cdr = s7_cdr(module_alist); */
    /*             if (debug) */
    /*                 log_debug("modules_alist_cdr: %s", */
    /*                    TO_STR(modules_alist_cdr)); */

    /*             /\* s7_pointer msrcs = s7_cons(s7, ml_assoc, modules_alist_cdr); *\/ */
    /*             s7_pointer msrcs = s7_append(s7, */
    /*                                          modules_alist_cdr, */
    /*                                          s7_list(s7, 1, ml_assoc)); */

    /*             /\* if (debug) { *\/ */
    /*             /\*     log_debug("setting cdr to: %s", TO_STR(msrcs)); *\/ */
    /*             /\* } *\/ */

    /*             s7_pointer new_modules_alist */
    /*                 = s7_set_cdr(module_alist, msrcs); */

    /*            if (debug) { */
    /*                 log_debug("new_modules_alist: %s", */
    /*                           TO_STR(new_modules_alist)); */
    /*                 log_debug("new pkgs: %s", TO_STR(pkg_alist)); */
    /*             } */
    /*         } */
    /*     } */
    /* } */
}

LOCAL void _update_pkg_modules(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
    if (trace) {
        log_trace(RED "_update_pkg_modules" CRESET);

    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        log_debug("fname: %s", fname);
        log_debug("tag: %d", ftype);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug)
        log_debug("pkg_key: %s", TO_STR(pkg_key));
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist));

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 1, modules_kw); //, static_kw);
        s7_pointer modules_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
        if (debug) log_debug("modules_alist %s", TO_STR(modules_alist));

        /* s7_pointer srcs_alist = s7_call(s7, assoc, */
        /*                                 s7_list(s7, 2, srcs_kw, pkg_alist)); */
        /* log_debug(":srcs_alist %s", */
        /*        TO_STR(srcs_alist)); */
        /* log_debug(":srcs null?: %d", s7_is_null(s7, srcs_alist)); */
        /* log_debug(":srcs false?: %d", s7_f(s7) ==  srcs_alist); */

        /* s7_pointer ml_assoc = s7_list(s7, 2, */
        s7_pointer ml_assoc = s7_cons(s7,
                                      s7_make_keyword(s7,
                                                      (ftype == TAG_ML)
                                                      ?"ml"
                                                      :(ftype == TAG_ML_DYN)
                                                      ?"ml_"
                                                      :(ftype == TAG_MLI)
                                                      ?"mli"
                                                      :(ftype == TAG_MLI_DYN)
                                                      ?"mli_"
                                                      :(ftype == TAG_MLL)
                                                      ?"mll"
                                                      :(ftype == TAG_MLY)
                                                      ?"mly"
                                                      :"UNKNOWN"
                                                      ),
                                      s7_make_symbol(s7, fname));
        if (debug)
            log_debug("ml_assoc: %s", TO_STR(ml_assoc));

        /* if (srcs_alist == s7_f(s7)) { */
        if (modules_alist == s7_f(s7)) {
            if (debug)
                log_debug("INITIALIZING :modules field");
            /* (acons :srcs */
            /*  ((:modules ((:Foo ((:ml foo.ml) (:mli foo.mli)) */
            /*              (:Bar ((:ml bar.ml) (:mli bar.mli))))))) */
            /*  pkg_alist) */
            /* s7_pointer msrcs_alist = s7_list(s7, 1, ml_assoc); */
            /* log_debug("msrcs_alist: %s", TO_STR(msrcs_alist)); */

            s7_pointer module_assoc = s7_list(s7, 2,
                                              mname_sym,
                                              ml_assoc);
            /* msrcs_alist); */
            if (debug)
                log_debug("module_assoc: %s", TO_STR(module_assoc));

            /* s7_pointer modules_alist = s7_list(s7, 1, module_assoc); */
            /* log_debug("modules_alist: %s", */
            /*        TO_STR(modules_alist)); */

            /* s7_pointer statics_assoc = */
            /*     s7_list(s7, 2, */
            /*             static_kw, */
            /*             module_assoc); */


            s7_pointer modules_assoc = s7_list(s7, 2,
                                               modules_kw,
                                               // statics_assoc
                                               module_assoc);
            if (debug) log_debug("modules_assoc: %s", TO_STR(modules_assoc));

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         modules_assoc));
            /* if (debug) */
            /*     log_debug("pkg_alist: %s", */
            /*            TO_STR(new_pkg_alist)); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
            if (debug) {
                log_debug("UPDATING :modules");
                log_debug("modules_alist: %s", TO_STR(modules_alist));
                log_debug("mname_sym: %s", TO_STR(mname_sym));
            }

            /* s7_pointer assoc_in = _load_assoc_in(); */

            s7_pointer keypath = s7_list(s7, 2, //  3,
                                         /* srcs_kw, */
                                         modules_kw,
                                         /* static_kw, */
                                         mname_sym);
            if (debug) {
                /* log_debug("assoc-in: %s", TO_STR(assoc_in)); */
                log_debug("keypath: %s", TO_STR(keypath));
                /* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */
            }

            s7_pointer module_alist = s7_call(s7, assoc_in,
                                              s7_list(s7, 2,
                                                      keypath,
                                                      pkg_alist));

            if (debug) {
                log_debug("module_alist: %s", TO_STR(module_alist));
            }
            if (module_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug(RED "ADDING" CRESET " module %s to %s",
                              TO_STR(mname_sym), TO_STR(modules_alist));
                /* s7_pointer keypath = s7_list(s7, 1, modules_kw); */
                /* if (debug) log_debug("trying keypath: %s", */
                /*                      TO_STR(keypath)); */
                if (debug) log_debug("modules_alist: %s",
                                     s7_object_to_c_string(s7,
                                                           modules_alist));

                s7_pointer modules_alist_cdr = s7_cdr(modules_alist);
                if (debug)
                    log_debug("modules_alist_cdr: %s",
                       TO_STR(modules_alist_cdr));

                s7_pointer module_assoc =
                    s7_list(s7, 1, s7_list(s7, 2, mname_sym, ml_assoc));
                if (debug) log_debug("new module_assoc: %s",
                                     TO_STR(module_assoc));

                s7_pointer new_modules_alist_cdr =
                    s7_append(s7, modules_alist_cdr, module_assoc);
                    /* s7_cons(s7, module_assoc, modules_alist_cdr); */
                if (debug) {
                    log_debug("new_modules_alist_cdr: %s",
                       TO_STR(new_modules_alist_cdr));
                }

                s7_pointer new_modules_alist
                    = s7_set_cdr(modules_alist, new_modules_alist_cdr);

                if (debug) {
                    log_debug("new_modules_alist: %s",
                              TO_STR(new_modules_alist));
                }
            } else {
                /* update */
                if (debug) log_debug(RED "UPDATING" CRESET " module_alist: %s",
                                     TO_STR(module_alist));

                s7_pointer modules_alist_cdr = s7_cdr(module_alist);
                if (debug)
                    log_debug("modules_alist_cdr: %s",
                       TO_STR(modules_alist_cdr));

                /* s7_pointer msrcs = s7_cons(s7, ml_assoc, modules_alist_cdr); */
                s7_pointer msrcs = s7_append(s7,
                                             modules_alist_cdr,
                                             s7_list(s7, 1, ml_assoc));

                /* if (debug) { */
                /*     log_debug("setting cdr to: %s", TO_STR(msrcs)); */
                /* } */

                s7_pointer new_modules_alist
                    = s7_set_cdr(module_alist, msrcs);

               if (debug) {
                    log_debug("new_modules_alist: %s",
                              TO_STR(new_modules_alist));
                    log_debug("new pkgs: %s", TO_STR(pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_pkg_sigs(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
    if (trace) {
        log_trace(RED "_update_pkg_sigs" CRESET);
    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug) log_debug("pkg_key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, sigs_kw, static_kw);
        s7_pointer sigs_alist = s7_call(s7, assoc_in,
                                        s7_list(s7, 2,
                                                keypath,
                                                pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
        if (debug) log_debug("sigs_alist %s", TO_STR(sigs_alist));

        s7_pointer mli_file = s7_make_symbol(s7, fname);
        /* s7_pointer mli_assoc = s7_list(s7, 2, */
        /*                               s7_make_keyword(s7, "mli"), */
        /*                               s7_make_string(s7, fname)); */
        /* if (debug) */
        /*     log_debug("mli_assoc: %s", TO_STR(mli_assoc)); */

        /* s7_pointer sig_assoc = s7_list(s7, 2, mname_sym, mli_file); */
        s7_pointer sig_assoc = s7_cons(s7, mname_sym, mli_file);
        if (debug) log_debug("sig_assoc: %s", TO_STR(sig_assoc));

        if (sigs_alist == s7_f(s7)) {
            if (debug)
                log_debug("INITIALIZING :signatures field");
            /* (:signatures ((:Foo (:mli foo.mli)) */
            /*               (:Bar (:mli bar.mli)))) */

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, sig_assoc);

            s7_pointer sigs_assoc = s7_list(s7, 2,
                                            sigs_kw, statics_assoc);
            if (debug) log_debug("sigs_assoc: %s", TO_STR(sigs_assoc));

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         sigs_assoc));
            /* if (debug) */
            /*     log_debug("pkg_alist: %s", */
            /*            TO_STR(new_pkg_alist)); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
            if (debug) {
                log_debug("UPDATING :signatures");
                log_debug("sigs_alist: %s", TO_STR(sigs_alist));
                log_debug("mname_sym: %s", TO_STR(mname_sym));
            }

            s7_pointer keypath = s7_list(s7, 3,
                                         sigs_kw,
                                         static_kw,
                                         mname_sym);
            if (debug) {
                /* log_debug("assoc-in: %s", TO_STR(assoc_in)); */
                log_debug("keypath: %s", TO_STR(keypath));
                /* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */
            }

            s7_pointer sig_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

            if (debug) {
                log_debug("sig_alist: %s", TO_STR(sig_alist));
            }
            if (sig_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug(RED "ADDING" CRESET " sig %s to %s",
                              TO_STR(mname_sym), TO_STR(sigs_alist));
                if (debug) log_debug("sigs_alist: %s",
                                     s7_object_to_c_string(s7,
                                                           sigs_alist));

                s7_pointer sigs_alist_cdr = s7_cdr(sigs_alist);
                if (debug) {
                    log_debug("sigs_alist_cdr: %s",
                       TO_STR(sigs_alist_cdr));
                }

                /* s7_pointer sig_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mli_file)); //mli_assoc)); */
                /* if (debug) log_debug("new sig_assoc: %s", */
                /*                      TO_STR(sig_assoc)); */

                s7_pointer new_sigs_alist_cdr =
                    s7_append(s7, sigs_alist_cdr,
                              s7_list(s7, 1, sig_assoc));

                if (debug)
                    log_debug("new_sigs_alist_cdr: %s",
                       TO_STR(new_sigs_alist_cdr));

                s7_pointer new_sigs_alist
                    = s7_set_cdr(sigs_alist, new_sigs_alist_cdr);
                if (debug) {
                    log_debug("new_sigs_alist: %s",
                              TO_STR(new_sigs_alist));
                }
            } else {
                /* update */
                if (debug) log_debug(RED "UPDATING" CRESET " sig_alist: %s",
                                     TO_STR(sig_alist));

                s7_pointer sigs_alist_cdr = s7_cdr(sig_alist);
                if (debug)
                    log_debug("sigs_alist_cdr: %s",
                       TO_STR(sigs_alist_cdr));

                s7_pointer msrcs = s7_append(s7,
                                             sigs_alist_cdr,
                                             s7_list(s7, 1, mli_file)); //mli_assoc));

                s7_pointer new_sigs_alist
                    = s7_set_cdr(sig_alist, msrcs);
                if (debug) {
                    log_debug("new_sigs_alist: %s",
                              TO_STR(new_sigs_alist));
                    log_debug("new pkgs: %s", TO_STR(pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_pkg_structs(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
    if (trace) {
        log_trace(BLU "_update_pkg_structs" CRESET);
    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug) log_debug("pkg_key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, structs_kw,
                                     (ftype == TAG_ML)
                                     ?static_kw
                                     :dynamic_kw);

        s7_pointer structs_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
        if (debug) log_debug("structs_alist keypath %s", TO_STR(keypath));
        if (debug) log_debug("structs_alist %s", TO_STR(structs_alist));

        /* s7_pointer ml_assoc = s7_list(s7, 2, */
        /*                               s7_make_keyword(s7, "ml"), */
        /*                               s7_make_string(s7, fname)); */
        /* if (debug) */
        /*     log_debug("ml_assoc: %s", TO_STR(ml_assoc)); */

        s7_pointer struct_assoc =
            s7_cons(s7, mname_sym, s7_make_symbol(s7, fname));
        /* s7_list(s7, 2, mname_sym, s7_make_symbol(s7, fname)); */
        if (debug) log_debug("struct_assoc: %s", TO_STR(struct_assoc));

        if (structs_alist == s7_f(s7)) {
            if (debug)
                log_debug("INITIALIZING %s field", TO_STR(keypath));
            /* (:structures (:static (:Foo (:ml foo.ml)) */
            /*               (:dynamic (:ml bar.ml)))) */

            s7_pointer structures_alist = s7_call(s7, assoc,
                                                  s7_list(s7, 2,
                                                          structs_kw,
                                                          pkg_alist));
            log_debug("STRUCTURES alist: %s", TO_STR(structures_alist));
            if (structures_alist == s7_f(s7)) {
                log_debug("NEW");
                s7_pointer statics_assoc =
                    s7_list(s7, 2,
                            // static_kw,
                            (ftype == TAG_ML) ?static_kw :dynamic_kw,
                            struct_assoc);

                s7_pointer structs_assoc = s7_list(s7, 2,
                                                   structs_kw, statics_assoc);
                if (debug) log_debug("structs_assoc: %s", TO_STR(structs_assoc));

                s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                     s7_list(s7, 1,
                                                             structs_assoc));
                if (debug)
                    log_debug("updated pkg_alist: %s",
                              TO_STR(new_pkg_alist));

                s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
            } else {
                log_debug("OLD");

                s7_pointer structures_alist_cdr = s7_cdr(structures_alist);
                if (debug) {
                    log_debug("structures_alist_cdr: %s",
                       TO_STR(structures_alist_cdr));
                }

                s7_pointer new_struct_assoc = s7_list(s7, 2,
                                                      // static_kw,
                                                      (ftype == TAG_ML) ?static_kw :dynamic_kw,
                                                      struct_assoc);

                s7_pointer new_structures_alist_cdr =
                    s7_append(s7, structures_alist_cdr,
                              s7_list(s7, 1, new_struct_assoc));
                if (debug)
                    log_debug("new_structures_alist_cdr: %s",
                       TO_STR(new_structures_alist_cdr));

                s7_pointer new_structures_alist
                    = s7_set_cdr(structures_alist, new_structures_alist_cdr);
            }
        } else {
            if (debug) {
                log_debug("UPDATING :structures");
                log_debug("structs_alist: %s", TO_STR(structs_alist));
                log_debug("mname_sym: %s", TO_STR(mname_sym));
            }

            s7_pointer keypath = s7_list(s7, 3,
                                         structs_kw,
                                         static_kw,
                                         mname_sym);
            if (debug) {
                /* log_debug("assoc-in: %s", TO_STR(assoc_in)); */
                log_debug("keypath: %s", TO_STR(keypath));
                /* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */
            }

            s7_pointer struct_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

            if (debug) {
                log_debug("struct_alist: %s", TO_STR(struct_alist));
            }
            if (struct_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug(RED "ADDING" CRESET " struct %s to %s",
                              TO_STR(mname_sym), TO_STR(structs_alist));
                if (debug) log_debug("structs_alist: %s",
                                     s7_object_to_c_string(s7,
                                                           structs_alist));

                s7_pointer structs_alist_cdr = s7_cdr(structs_alist);
                if (debug) {
                    log_debug("structs_alist_cdr: %s",
                       TO_STR(structs_alist_cdr));
                }

                /* s7_pointer struct_assoc = */
                /*     s7_list(s7, 1, */
                /*             s7_list(s7, 2, */
                /*                     mname_sym, s7_make_symbol(s7, fname))); */
                /* if (debug) log_debug("new struct_assoc: %s", */
                /*                      TO_STR(struct_assoc)); */

                s7_pointer new_structs_alist_cdr =
                    s7_append(s7, structs_alist_cdr,
                              s7_list(s7, 1, struct_assoc));
                if (debug)
                    log_debug("new_structs_alist_cdr: %s",
                       TO_STR(new_structs_alist_cdr));

                s7_pointer new_structs_alist
                    = s7_set_cdr(structs_alist, new_structs_alist_cdr);
                if (debug) {
                    log_debug("new_structs_alist: %s",
                              TO_STR(new_structs_alist));
                }
            } else {
                /* update */
                if (debug) log_debug(RED "UPDATING" CRESET " struct_alist: %s",
                                     TO_STR(struct_alist));

                s7_pointer structs_alist_cdr = s7_cdr(struct_alist);
                if (debug)
                    log_debug("structs_alist_cdr: %s",
                       TO_STR(structs_alist_cdr));

                s7_pointer msrcs =
                    s7_append(s7, structs_alist_cdr,
                              s7_list(s7, 1, s7_make_symbol(s7, fname))); //ml_assoc));

                s7_pointer new_structs_alist
                    = s7_set_cdr(struct_alist, msrcs);
                if (debug) {
                    log_debug("new_structs_alist: %s",
                              TO_STR(new_structs_alist));
                    log_debug("new pkgs: %s", TO_STR(pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_pkg_mll_files(s7_pointer pkg_tbl,
                                 char *pkg_name, char *mname,
                                 char *fname, int ftype)
{
    if (trace) {
        log_trace(RED "_update_pkg_mll_files" CRESET);
    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug) log_debug("pkg_key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, mll_kw, static_kw);
        s7_pointer ocamllex_alist = s7_call(s7, assoc_in,
                                        s7_list(s7, 2,
                                                keypath,
                                                pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
        if (debug) log_debug("ocamllex_alist %s", TO_STR(ocamllex_alist));

        s7_pointer mll_file = s7_make_symbol(s7, fname);

        /* s7_pointer mll_assoc = s7_list(s7, 2, mname_sym, mll_file); */
        s7_pointer mll_assoc = s7_cons(s7, mname_sym, mll_file);
        if (debug) log_debug("mll_assoc: %s", TO_STR(mll_assoc));

        if (ocamllex_alist == s7_f(s7)) {
            if (debug)
                log_debug("INITIALIZING :ocamllex field");

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, mll_assoc);

            s7_pointer ocamllex_assoc = s7_list(s7, 2,
                                            mll_kw, statics_assoc);
            if (debug) log_debug("ocamllex_assoc: %s", TO_STR(ocamllex_assoc));

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         ocamllex_assoc));
            /* if (debug) */
            /*     log_debug("pkg_alist: %s", */
            /*            TO_STR(new_pkg_alist)); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
            if (debug) {
                log_debug("UPDATING :signatures");
                log_debug("ocamllex_alist: %s", TO_STR(ocamllex_alist));
                log_debug("mname_sym: %s", TO_STR(mname_sym));
            }

            s7_pointer keypath = s7_list(s7, 3,
                                         mll_kw,
                                         static_kw,
                                         mname_sym);
            if (debug) {
                /* log_debug("assoc-in: %s", TO_STR(assoc_in)); */
                log_debug("keypath: %s", TO_STR(keypath));
                /* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */
            }

            s7_pointer mll_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

            if (debug) {
                log_debug("mll_alist: %s", TO_STR(mll_alist));
            }
            if (mll_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug(RED "ADDING" CRESET " sig %s to %s",
                              TO_STR(mname_sym), TO_STR(ocamllex_alist));
                if (debug) log_debug("ocamllex_alist: %s",
                                     s7_object_to_c_string(s7,
                                                           ocamllex_alist));

                s7_pointer ocamllex_alist_cdr = s7_cdr(ocamllex_alist);
                if (debug) {
                    log_debug("ocamllex_alist_cdr: %s",
                       TO_STR(ocamllex_alist_cdr));
                }

                /* s7_pointer mll_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mll_file)); //mll_assoc)); */
                /* if (debug) log_debug("new mll_assoc: %s", */
                /*                      TO_STR(mll_assoc)); */

                s7_pointer new_ocamllex_alist_cdr =
                    s7_append(s7, ocamllex_alist_cdr,
                              s7_list(s7, 1, mll_assoc));

                if (debug)
                    log_debug("new_ocamllex_alist_cdr: %s",
                       TO_STR(new_ocamllex_alist_cdr));

                s7_pointer new_ocamllex_alist
                    = s7_set_cdr(ocamllex_alist, new_ocamllex_alist_cdr);
                if (debug) {
                    log_debug("new_ocamllex_alist: %s",
                              TO_STR(new_ocamllex_alist));
                }
            } else {
                /* update */
                if (debug) log_debug(RED "UPDATING" CRESET " mll_alist: %s",
                                     TO_STR(mll_alist));

                s7_pointer ocamllex_alist_cdr = s7_cdr(mll_alist);
                if (debug)
                    log_debug("ocamllex_alist_cdr: %s",
                       TO_STR(ocamllex_alist_cdr));

                s7_pointer msrcs = s7_append(s7,
                                             ocamllex_alist_cdr,
                                             s7_list(s7, 1, mll_file));

                s7_pointer new_ocamllex_alist
                    = s7_set_cdr(mll_alist, msrcs);
                if (debug) {
                    log_debug("new_ocamllex_alist: %s",
                              TO_STR(new_ocamllex_alist));
                    log_debug("new pkgs: %s", TO_STR(pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_mll(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    if (trace) {
        log_trace(RED "_update_mll", CRESET);
    }
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    if (trace) {
        log_trace("module name: %s ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }

    /* ocamllex emits .ml - if static .mli found, update :modules
       else update :structures */

    char *ml_name = strdup(ftsentry->fts_name);
    ml_name[strlen(ftsentry->fts_name) - 1] = '\0';

    char *mli_name = strdup(ftsentry->fts_name);
    mli_name[strlen(ftsentry->fts_name) - 1] = 'i';

    /* dirname may mutate its arg, use a copy */
    char *dname = strdup(ftsentry->fts_path);
    UT_string *mli_test;
    utstring_new(mli_test);
    /* add terminal 'i' with printf */
    utstring_printf(mli_test, "%s/%s", dirname(dname), mli_name);
    if (trace) {
        log_debug("Checking for companion .mli: %s",
                  utstring_body(mli_test));
    }
    int rc = access(utstring_body(mli_test), F_OK);
    if (rc) {
        /* companion mli file not found */
        _update_pkg_structs(pkg_tbl, pkg_name, mname,
                            ml_name, // ftsentry->fts_name,
                            TAG_ML_DYN);
    } else {
        /* _update_pkg_modules(pkg_tbl, pkg_name, mname, */
        /*                     ftsentry->fts_name, TAG_ML); */
        _update_pkg_modules(pkg_tbl, pkg_name, mname,
                            ftsentry->fts_name,
                            TAG_MLL);
        _update_pkg_modules(pkg_tbl, pkg_name, mname,
                            mli_name, // ftsentry->fts_name,
                            TAG_MLI);
        _update_pkg_modules(pkg_tbl, pkg_name, mname,
                            ml_name, // ftsentry->fts_name,
                            TAG_ML_DYN);
    }

    /* update pkg fld :ocamllex */
    _update_pkg_mll_files(pkg_tbl, pkg_name, mname,
                          ftsentry->fts_name, TAG_MLL);

    free(mli_name);
    free(ml_name);
    free(dname);
    utstring_free(mli_test);
    /* exit(0); */
}

LOCAL void _update_pkg_mly_files(s7_pointer pkg_tbl,
                                 char *pkg_name, char *mname,
                                 char *fname, int ftype)
{
    // mly entry structure: (Foo (:mly foo.mly) (:ml_ foo.ml) (:ml-deps ...) (:mli_ foo.mli) (:mli-deps ...))
    if (trace) {
        log_trace(RED "_UPDATE_PKG_MLY_FILES" CRESET);
    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug) log_debug("pkg_key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
        /* return ??? */
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, mly_kw, static_kw);
        s7_pointer ocamlyacc_alist = s7_call(s7, assoc_in,
                                        s7_list(s7, 2,
                                                keypath,
                                                pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
        if (debug) log_debug("ocamlyacc_alist %s", TO_STR(ocamlyacc_alist));

        s7_pointer mly_file = s7_make_symbol(s7, fname);

        /* s7_pointer mly_assoc = s7_list(s7, 2, mname_sym, mly_file); */
        s7_pointer mly_assoc = s7_cons(s7, mname_sym, mly_file);
        if (debug) log_debug("mly_assoc: %s", TO_STR(mly_assoc));

        if (ocamlyacc_alist == s7_f(s7)) {
            if (debug)
                log_debug("INITIALIZING :ocamlyacc field");

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, mly_assoc);

            s7_pointer ocamlyacc_assoc = s7_list(s7, 2,
                                            mly_kw, statics_assoc);
            if (debug) log_debug("ocamlyacc_assoc: %s", TO_STR(ocamlyacc_assoc));

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         ocamlyacc_assoc));
            /* if (debug) */
            /*     log_debug("pkg_alist: %s", */
            /*            TO_STR(new_pkg_alist)); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
            if (debug) {
                log_debug("UPDATING :signatures");
                log_debug("ocamlyacc_alist: %s", TO_STR(ocamlyacc_alist));
                log_debug("mname_sym: %s", TO_STR(mname_sym));
            }

            s7_pointer keypath = s7_list(s7, 3,
                                         mly_kw,
                                         static_kw,
                                         mname_sym);
            if (debug) {
                /* log_debug("assoc-in: %s", TO_STR(assoc_in)); */
                log_debug("keypath: %s", TO_STR(keypath));
                /* log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */
            }

            s7_pointer mly_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

            if (debug) {
                log_debug("mly_alist: %s", TO_STR(mly_alist));
            }
            if (mly_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug(RED "ADDING" CRESET " sig %s to %s",
                              TO_STR(mname_sym), TO_STR(ocamlyacc_alist));
                if (debug) log_debug("ocamlyacc_alist: %s",
                                     s7_object_to_c_string(s7,
                                                           ocamlyacc_alist));

                s7_pointer ocamlyacc_alist_cdr = s7_cdr(ocamlyacc_alist);
                if (debug) {
                    log_debug("ocamlyacc_alist_cdr: %s",
                       TO_STR(ocamlyacc_alist_cdr));
                }

                /* s7_pointer mly_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mly_file)); //mly_assoc)); */
                /* if (debug) log_debug("new mly_assoc: %s", */
                /*                      TO_STR(mly_assoc)); */

                s7_pointer new_ocamlyacc_alist_cdr =
                    s7_append(s7, ocamlyacc_alist_cdr,
                              s7_list(s7, 1, mly_assoc));

                if (debug)
                    log_debug("new_ocamlyacc_alist_cdr: %s",
                       TO_STR(new_ocamlyacc_alist_cdr));

                s7_pointer new_ocamlyacc_alist
                    = s7_set_cdr(ocamlyacc_alist, new_ocamlyacc_alist_cdr);
                if (debug) {
                    log_debug("new_ocamlyacc_alist: %s",
                              TO_STR(new_ocamlyacc_alist));
                }
            } else {
                /* update */
                if (debug) log_debug(RED "UPDATING" CRESET " mly_alist: %s",
                                     TO_STR(mly_alist));

                s7_pointer ocamlyacc_alist_cdr = s7_cdr(mly_alist);
                if (debug)
                    log_debug("ocamlyacc_alist_cdr: %s",
                       TO_STR(ocamlyacc_alist_cdr));

                s7_pointer msrcs = s7_append(s7,
                                             ocamlyacc_alist_cdr,
                                             s7_list(s7, 1, mly_file));

                s7_pointer new_ocamlyacc_alist
                    = s7_set_cdr(mly_alist, msrcs);
                if (debug) {
                    log_debug("new_ocamlyacc_alist: %s",
                              TO_STR(new_ocamlyacc_alist));
                    log_debug("new pkgs: %s", TO_STR(pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_mly(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    if (trace) {
        log_trace(RED "_update_mly" CRESET);
    }
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    if (trace) {
        log_trace("_update_mly: %s; ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }

    char *mli_name = strdup(ftsentry->fts_name);
    mli_name[strlen(ftsentry->fts_name) - 1] = 'i';
    log_trace(RED "mli_name: %s" CRESET, mli_name);

    char *ml_name = strdup(ftsentry->fts_name);
    ml_name[strlen(ftsentry->fts_name) - 1] = '\0';
    log_trace(RED "ml_name: %s" CRESET, ml_name);

   _update_pkg_modules(pkg_tbl, pkg_name, mname,
                        ftsentry->fts_name,
                        TAG_MLY);
    _update_pkg_modules(pkg_tbl, pkg_name, mname,
                        mli_name, // ftsentry->fts_name,
                        TAG_MLI_DYN);
    _update_pkg_modules(pkg_tbl, pkg_name, mname,
                        ml_name, // ftsentry->fts_name,
                        TAG_ML_DYN);

    /* update pkg fld :ocamlyacc */
    _update_pkg_mly_files(pkg_tbl, pkg_name, mname,
                          ftsentry->fts_name, TAG_MLY);

    free(mli_name);
    free(ml_name);
}

LOCAL void _update_mli(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* printf("_update_mli: "); */
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    if (trace) {
        log_trace("_update_mli: %s; ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }
    char *ml_name = strdup(ftsentry->fts_name);
    ml_name[strlen(ftsentry->fts_name) - 1] = '\0';

    /* dirname may mutate its arg, use a copy */
    char *dname = strdup(ftsentry->fts_path);
    UT_string *ml_test;
    utstring_new(ml_test);
    utstring_printf(ml_test, "%s/%s", dirname(dname), ml_name);
    if (trace) {
        log_debug(RED "Checking for companion .ml:" CRESET  " %s",
                  utstring_body(ml_test));
    }
    int rc = access(utstring_body(ml_test), F_OK);
    if (rc) {
        /* companion ml file not found */
        _update_pkg_sigs(pkg_tbl, pkg_name, mname,
                         ftsentry->fts_name, TAG_MLI);
    } else {
        _update_pkg_modules(pkg_tbl, pkg_name, mname,
                            ftsentry->fts_name, TAG_MLI);
    }
}

LOCAL void _update_ml(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* printf("_update_ml: "); */
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    if (verbose) {
        log_info(BLU "_update_ml:" CRESET " %s; ", mname);
        log_info("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }

    char *ml_name = strdup(ftsentry->fts_name);

    /* dirname may mutate its arg, use a copy */
    char *dname = strdup(ftsentry->fts_path);
    UT_string *mli_test;
    utstring_new(mli_test);
    /* add terminal 'i' with printf */
    utstring_printf(mli_test, "%s/%si", dirname(dname), ml_name);
    if (trace) {
        log_debug("Checking for companion .mli: %s",
                  utstring_body(mli_test));
    }
    int rc = access(utstring_body(mli_test), F_OK);
    if (rc) {
        /* companion mli file not found */
        _update_pkg_structs(pkg_tbl, pkg_name, mname,
                            ftsentry->fts_name, TAG_ML);
    } else {
        _update_pkg_modules(pkg_tbl, pkg_name, mname,
                            ftsentry->fts_name, TAG_ML);
    }
}

/*
  if no entry in pkg-tbl for ctx dir, add one
 */
LOCAL void _handle_ml_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* log_debug("_handle_ml_file %s, %s\n", */
    /*           ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    /* _indent(ftsentry->fts_level); */
    /* printf("%d. " CRESET, ftsentry->fts_level); */

    if ((strncmp(ext, ".ml", 3) == 0)
        && (strlen(ext) == 3)) {
        /* printf(":%-6s", "ml"); */
        _update_ml(pkg_tbl, ftsentry, ext);
        /* _update_pkg_deps(pkg_tbl, ftsentry, ext); */
    }
    else if ((strncmp(ext, ".mli", 4) == 0)
        && (strlen(ext) == 4)) {
        /* printf("%s", "mli"); */
        _update_mli(pkg_tbl, ftsentry, ext);
    }
    /*  */
    else if ((strncmp(ext, ".mlt", 4) == 0)
        /* .mlt - ocamlformat file, not a module */
        && (strlen(ext) == 4)) {
        /* log_warn("UNHANDLED: :%-6s %s", "mlt", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mll", 4) == 0)
        && (strlen(ext) == 4)) {
        _update_mll(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mly", 4) == 0)
        && (strlen(ext) == 4)) {
        _update_mly(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mlh", 4) == 0)
        && (strlen(ext) == 4)) {
        /* log_warn("UNHANDLED: :%-6s %s", "mlh", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mllib", 6) == 0)
        && (strlen(ext) == 6)) {
        /* log_warn("UNHANDLED: :%-6s %s", "mllib", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mligo", 6) == 0)
        && (strlen(ext) == 6)) {
        /* tezos */
        /* log_warn("UNHANDLED: :%-6s %s", "mligo", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mldylib", 8) == 0)
        && (strlen(ext) == 8)) {
        /* mina */
        /* log_warn("UNHANDLED: :%-6s %s", "mldylib", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".md", 3) == 0)
        && (strlen(ext) == 3)) {
        /* log_warn("UNHANDLED: :%-6s %s", "md", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else {
        log_warn(RED "UNKNOWN ml ext: :%-6s\n" CRESET, ext);
        _update_pkg_files(pkg_tbl, ftsentry, ext);
        /* exit(EXIT_FAILURE); */
    }
    /* printf("%s" CRESET " ", ftsentry->fts_name); */
    /* printf("\n"); */
}

LOCAL void _handle_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* if (debug) */
    /*     log_debug("_handle_file %s, %s\n", ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* _indent(ftsentry->fts_level); */
    /* printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name); */

    if (ftsentry->fts_name[0] == '.') {
        // do not process hidden files
        if (trace) {
            log_trace(RED "Excluding" CRESET " hidden file: %s/%s\n",
                      ftsentry->fts_path, ftsentry->fts_name);
        }
        return;
    } else {
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
}

LOCAL void _handle_dune_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
    if (debug)
        log_debug("_handle_dune_file: %s", ftsentry->fts_path);

    /* && (ftsentry->fts_namelen = 4) == 0)) { */
    _indent(ftsentry->fts_level);
    /* printf("%d. " RED "%s ", */
    /*        ftsentry->fts_level, */
    /*        ftsentry->fts_name); */
    /* printf(CRESET "\n"); */

    static char buf[256];
    strlcpy(buf, ftsentry->fts_path, 256);
    /* strlcat(buf, "/dune", 256); */
    /* printf("checking %s\n", buf); */

    /* char *s1; */
    /* avoid tuareg dunefiles! */
    FILE *fileStream;
    char fileText [100];
    utstring_renew(dunefile_name);
    utstring_printf(dunefile_name, "%s", ftsentry->fts_path);
    fileStream = fopen(utstring_body(dunefile_name), "r");
    if (fileStream == NULL) {
        log_error("FAIL: fopen(%s)", utstring_body(dunefile_name));
        //FIXME: cleanup
        exit(EXIT_FAILURE);
    }
    fgets(fileText, 100, fileStream);
    char *r = strstr(fileText, "tuareg");
    if (r != NULL) {
        log_debug("TUAREG! %s/dune", ftsentry->fts_path);
        fclose(fileStream);
        /* s7_pointer pkgs = s7_name_to_value(s7, "pkg-tbl"); */
        s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
        /* s7_pointer test_assoc = s7_list(s7, 2, */
        /*                                 s7_make_keyword(s7, "test"), */
        /*                                 s7_make_symbol(s7, "dummy")); */
        char *rpath = realpath(ftsentry->fts_path, NULL);
        /* FIXME: check result */
        /* s7_pointer result = */
        s7_hash_table_set(s7, pkg_tbl, key,
                          s7_list(s7, 2,
                                  s7_list(s7, 2, pkg_path_kw,
                                          key),
                                  s7_list(s7, 2, realpath_kw,
                                          s7_make_string(s7, rpath))));

        return;
    }
    fclose(fileStream);     /* end tuareg check */

    dunefile_ct++;

    s7_pointer stanzas = _read_dunefile(ftsentry->fts_path); //, "dune");
    if (trace) log_debug("readed stanzas: %s", TO_STR(stanzas));

    s7_pointer pkg_key = s7_make_string(s7, dirname(ftsentry->fts_path));

    if (debug) log_debug("pkg tbl: %s", TO_STR(pkg_tbl));
    if (debug) log_debug("pkg key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    if (debug)
        log_debug("pkg_alist: %s", TO_STR(pkg_alist));

    s7_pointer assoc = _load_assoc();

    s7_pointer stanzas_alist =
        s7_call(s7, assoc, s7_list(s7, 2,
                                   dune_stanzas_sym,
                                   pkg_alist));

    if (stanzas_alist == s7_f(s7)) {
        s7_pointer stanzas_assoc = s7_cons(s7, dune_stanzas_sym, stanzas);
        /* log_debug("appending new stanzas_assoc: %s", TO_STR(stanzas_assoc)); */
        /* FIXME: check result */
        /* s7_pointer result = */
        s7_hash_table_set(s7, pkg_tbl, pkg_key,
                          s7_append(s7, pkg_alist,
                                    s7_list(s7, 1, stanzas_assoc)));
    } else {
        if (debug) log_debug("setting cdr of ");
        s7_set_cdr(stanzas_alist, stanzas);
    }

    /* if (debug) */
    /*     log_debug("updated pkg-tbl: %s", TO_STR(pkg_tbl)); */
}

LOCAL void _handle_dune_project_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
    if (debug)
        log_debug("_handle_dune_project_file: %s", ftsentry->fts_path);

    _indent(ftsentry->fts_level);
    /* printf("%d. " RED "%s ", */
    /*        ftsentry->fts_level, */
    /*        ftsentry->fts_name); */
    /* printf(CRESET "\n"); */

    static char buf[256];
    strlcpy(buf, ftsentry->fts_path, 256);

    /* char *s1; */

    s7_pointer stanzas = _read_dunefile(ftsentry->fts_path);

    s7_pointer dune_project_assoc = s7_cons(s7,
                                            dune_project_sym,
                                            stanzas);

    s7_pointer key = s7_make_string(s7, dirname(ftsentry->fts_path));
    if (debug)
        log_debug("pkg key: %s", TO_STR(key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, key);
    if (debug)
        log_debug("pkg_alist: %s", TO_STR(pkg_alist));

    if (pkg_alist == s7_f(s7)) {
        // we hit a dune-project file w/o a dune file
        s7_hash_table_set(s7, pkg_tbl, key,
                          s7_list(s7, 1, dune_project_assoc));
    } else {

        /* FIXME: check result */
        /* s7_pointer result = */
        s7_hash_table_set(s7, pkg_tbl, key,
                          /* s7_list(s7, 2, */
                          s7_append(s7, pkg_alist,
                                    s7_list(s7, 1,
                                            dune_project_assoc)));
        /* if (debug) */
        /*     log_debug("updated pkg-tbl: %s", TO_STR(pkg_tbl)); */

        /* return pkg_tbl; */
    }
}

LOCAL void _handle_opam_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
    /* printf("_handle_opam_file %s\n", ftsentry->fts_name); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    /* _indent(ftsentry->fts_level); */

    /* printf("%d. " MAG  ":%-6s" CRESET " %s\n", */
    /*        ftsentry->fts_level, */
    /*        "opam", */
    /*        ftsentry->fts_name); */

    /* _update_ml(ftsentry, ext); */
}

LOCAL void _handle_ocamlformat_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
    _indent(ftsentry->fts_level);

    /* printf("%d. " BHMAG ":%-6s" CRESET " %s\n", */
    /*        ftsentry->fts_level, */
    /*        "ocamlformat", */
    /*        ftsentry->fts_name); */

    /* _update_ml(ftsentry, ext); */
}

LOCAL void _handle_script_file(s7_pointer pkg_tbl,
                               FTSENT *ftsentry, char *ext)
{
    /* if (debug) */
    /*     log_debug("_handle_script_file %s, %s\n", ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    _indent(ftsentry->fts_level);
    /* printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name); */

    _update_pkg_script_files(pkg_tbl, ftsentry, ext);
}

LOCAL void _handle_symlink(s7_pointer pkg_tbl, FTS *tree, FTSENT *ftsentry)
{
    if (strncmp(ftsentry->fts_name, "bazel-", 6) == 0) {
        /* skip Bazel dirs, e.g. bazel-bin */
        if (verbose)
            log_info(RED "Excluding" CRESET " Bazel dir: %s", ftsentry->fts_name);
        fts_set(tree, ftsentry, FTS_SKIP);
        return;
    }

    if (strncmp(ftsentry->fts_name, "dune", 4) == 0) {
        log_warn("SYMLINKED dunefile: %s", ftsentry->fts_name);
        return;
    }

#define BUFSZ 4095
    char linkbuf[BUFSZ];
    int  linklen;

    _indent(ftsentry->fts_level);

    /* printf("%d. ", ftsentry->fts_level); */

    if (strncmp(ftsentry->fts_name, "dune", 4) == 0) {
        printf(RED);
    }

    /* printf("%s ", ftsentry->fts_name); */
    /* printf(YEL); */
    linklen = 0;
    linklen = readlink(ftsentry->fts_path,
                       linkbuf, BUFSZ);
    if (linklen < 0) {
        printf(RED "ERROR on readlink: %s \n",
               strerror(errno));
    /* } else { */
    /*     printf("%.*s" CRESET "\n", linklen, linkbuf); */
    }
}

/* control traversal order */
int _compare(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

LOCAL const char *_get_path_dir(s7_pointer arg)
{
    if (trace)
        log_trace("_get_path_dir: %s", TO_STR(arg));

    char *pathdir = s7_string(arg);

    if (pathdir[0] == '/') {
        log_error("Path arg must be relative");
        return NULL;
    }

    /* always remove trailing '/' */
    if (pathdir[strlen(pathdir) - 1] == '/') {
        pathdir[strlen(pathdir) - 1] = '\0';
    }

    errno = 0;
    int rc = access(pathdir, R_OK);
    if (rc) {
        /* log_error("%s: %s", strerror(errno), pathdir); */
        s7_pointer s7s = s7_list(s7, 3,
                                 s7_make_string(s7, "~A: ~A\n"),
                                 s7_make_string(s7, strerror(errno)),
                                 s7_make_string(s7, pathdir));
        s7_error(s7, s7_make_symbol(s7, "access-error"), s7s);
        return NULL;
    }
    struct stat path_stat;
    stat(pathdir, &path_stat);
    if ( !S_ISDIR(path_stat.st_mode) ) {
        log_error("Path arg must be a directory: %s", pathdir);
        return NULL;
    }
    return pathdir;
}

s7_pointer _merge_pkg_tbls(s7_scheme *s7, s7_pointer ht1, s7_pointer ht2)
{
    log_debug("merging: %s", TO_STR(ht2));
    log_debug(" into: %s", TO_STR(ht1));

    s7_pointer _iter = s7_make_iterator(s7, ht2);
    s7_int _gc = s7_gc_protect(s7, _iter);

    s7_pointer _item = s7_iterate(s7, _iter);

    while ( ! s7_is_eq(s7_eof_object(s7), _item) ) {
        if (debug) {
            /* log_info("item: %s\n", TO_STR(_item)); */
            log_info("merging key: %s", TO_STR(s7_car(_item)));
            log_info("    val: %s", TO_STR(s7_cdr(_item)));
        }
        s7_hash_table_set(s7, ht1,
                          s7_car(_item),
                          s7_cdr(_item));
        _item = s7_iterate(s7, _iter);
    }

    s7_gc_unprotect_at(s7, _gc);
    return ht1;
}

EXPORT s7_pointer g_load_dune(s7_scheme *s7,  s7_pointer args)
{
    if (debug) {
        log_debug(RED "g_load_dune" CRESET ", args: %s", TO_STR(args));
        log_debug("build_wd: %s (=BUILD_WORKING_DIRECTORY)", build_wd);
        log_debug("launch_dir: %s", launch_dir);
        log_debug("base ws root: %s", bws_root);
        log_debug("effective ws root: %s", ews_root);
        log_debug("cwd: %s", getcwd(NULL, 0));
    }

    /* s7_pointer wss =  */
    ///s7_pointer root_ws =
    initialize_mibl_data_model(s7);

    /* FIXME: s7_pointer _pkg_tbl = */
    /* s7_eval_c_string(s7, "(cadr (assoc-in '(:@ :pkgs) -mibl-ws-table))"); */
    /* printf("pkg_tbl: %s\n", TO_STR(_pkg_tbl)); */

    const char *rootdir, *pathdir;

    if ( s7_is_null(s7, args) ) {
        rootdir = getcwd(NULL, 0);
        pathdir = ".";
        /* s7_pointer _pkg_tbl = */
            load_dune(rootdir, pathdir);
        /* if (trace) { */
        log_trace("LOADED DUNE NOARG");
        /* log_trace(RED "-mibl-ws-table:" CRESET " %s\n", */
        /*           TO_STR(s7_name_to_value(s7, "-mibl-ws-table"))); */
        /* } */
        return s7_name_to_value(s7, "-mibl-ws-table");
    } else {
        s7_int args_ct = s7_list_length(s7, args);
        if (debug) log_debug("args ct: %d", args_ct);

        /* s7_pointer rootarg; */

        /* if (args_ct == 2) { */
        /*     rootdir = s7_string(s7_car(args)); */
        /*     pathdir = s7_string(s7_cadr(args)); */
        /* } */

        if (args_ct == 1) {
            s7_pointer arg = s7_car(args);
            if (s7_is_list(s7, arg)) {
                if (trace)
                    log_info("Arg is list: %s", TO_STR(arg));

                rootdir = getcwd(NULL,0);

                /* s7_int gc1; */
                /* s7_pointer _pkgs, _iter; */

                s7_pointer arglist = arg;
                while ( !s7_is_null(s7, arglist)) {
                    s7_pointer item = s7_car(arglist);
                    if (trace)
                        log_info("item: %s", TO_STR(item));
                    pathdir = _get_path_dir(item);
                    if (pathdir) {
                        /* FIXME: s7_pointer _pkgs = */
                        load_dune(rootdir, pathdir);
                        if (trace)
                            printf(RED "LOADED DUNE 1" CRESET "\n");

                        //FIXME: is this needed?
                        /* if (s7_is_hash_table(_pkgs)) { */
                        /*     _pkg_tbl = _merge_pkg_tbls(s7, _pkg_tbl, _pkgs); */
                        /*     log_debug("merged result: %s", TO_STR(_pkg_tbl)); */
                        /* } else { */
                        /*     log_error("load_dune returned %s", TO_STR(_pkgs)); */
                        /*     return s7_nil(s7); */
                        /* } */
                    } else {
                        log_error("cwd: %s", getcwd(NULL,0));
                    }
                    arglist = s7_cdr(arglist);
                }
                /* return _pkg_tbl; */
                //  (set-cdr! (assoc-in '(:@ :pkgs) ws-table) )
                /* s7_eval(s7, s7_list(s7, 3, */
                /*                     _s7_set_cdr, */
                /*                     s7_list(s7, 3, */
                /*                             assoc_in, */
                /*                             s7_list(s7, 2, */
                /*                                     s7_make_keyword(s7, "@"), */
                /*                                     s7_make_keyword(s7, "pkgs")), */
                /*                             s7_name_to_value(s7, "-mibl-ws-table")), */
                /*                     _pkg_tbl), */
                /*         s7_rootlet(s7)); */

                /* printf("root_ws 1: %s\n", TO_STR(s7_name_to_value(s7, "-mibl-ws-table"))); */

                return s7_name_to_value(s7, "-mibl-ws-table");
            }
            else if (s7_is_string(arg)) {
                /* one string arg == path relative to current wd */
                rootdir = getcwd(NULL,0);
                if (trace)
                    log_trace("Rootdir: %s", rootdir);
                pathdir = _get_path_dir(arg);
                /* s7_pointer q = s7_name_to_value(s7, "quote"); */
                if (pathdir) {
                    /* FIXME: s7_pointer _pkg_tbl = */
                    load_dune(rootdir, pathdir);
                    if (trace)
                        printf(RED "LOADED DUNE 2" CRESET "\n");

                    //FIXME: is this needed?

                    //TODO: use s7_eval?
                    /* s7_eval_c_string_with_environment(s7, */
                    /*                                   "(set-cdr! (assoc-in '(:@ :pkgs) -mibl-ws-table) (list _pkg_tbl))", */
                    /*                                   s7_inlet(s7, s7_list(s7, 1, */
                    /*                                                        s7_cons(s7, s7_make_symbol(s7, "_pkg_tbl"), _pkg_tbl)))); */
                    /* /\* printf("root_ws 2: %s\n", TO_STR(s7_name_to_value(s7, "-mibl-ws-table"))); *\/ */

                    return s7_name_to_value(s7, "-mibl-ws-table");

                } else {
                    log_error("cwd: %s", getcwd(NULL,0));
                    return s7_nil(s7);
                }
            } else {
                log_error("Arg must be string or list of strings");
                return s7_nil(s7);
            }
        }
        else if (args_ct == 0) {
            rootdir = getcwd(NULL,0);
            pathdir = ".";
        }
        else {
            log_error("Too many args");
            fprintf(stderr,
                    RED "ERROR: unexpected arg count %d for load-dune\n",
                    (int)args_ct);
            exit(EXIT_FAILURE);
        }
        /* rootdir = (char *)TO_STR(rootarg); */
        /* rootdir = (char *)s7_string(rootarg); */
        /* printf("s7_car(args): %s\n", rootdir); */
        /* strlcpy(rootdir, s, 256); */
        /* rootdir = "test"; */
    }

    /* char *sexp = "(set-cdr! " */
    /*     "(assoc-in '(:@ :pkgs) -mibl-ws-table) " */
    /*     "_pkg_tbl)"; */

    /* s7_eval_c_string(s7, sexp); */
    /* s7_eval_c_string_with_environment(s7, sexp, */
    /*                                   s7_inlet(s7, s7_list(s7, 1, */
    /*                                                        s7_cons(s7, s7_make_symbol(s7, "_pkg_tbl"), _pkg_tbl)))); */

    /* s7_eval(s7, s7_list(s7, 3, */
    /*                     _s7_set_cdr, */
    /*                     s7_list(s7, 3, */
    /*                             assoc_in, */
    /*                             /\* FIXME: figure out how to quote this *\/ */
    /*                             s7_list(s7, 2, */
    /*                                     s7_make_keyword(s7, "@"), */
    /*                                     s7_make_keyword(s7, "pkgs")), */
    /*                             s7_name_to_value(s7, "-mibl-ws-table")), */
    /*                     _pkg_tbl), */
    /*         s7_rootlet(s7)); */

    /* printf("root_ws 3: %s\n", TO_STR(s7_name_to_value(s7, "-mibl-ws-table"))); */
    return s7_name_to_value(s7, "-mibl-ws-table");
}

bool _include_this(FTSENT *ftsentry)
{
    if (trace)
        log_trace(MAG "_include_this?" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);

    if (debug) {
        dump_mibl_config();
    }

    if (ftsentry->fts_name[0] == '.') {
        if (ftsentry->fts_path[0] == '.') {
            if (strlen(ftsentry->fts_path) == 1) {
                return true;
            }
        }
    }
    /* exclusions override inclusiongs */
    /* if exclude return false */
    /* otherwise, if include return true else false */

    /* for exclusions we want an exact match */
    char *ptr = NULL;
    if (ftsentry->fts_path[0] == '.' & ftsentry->fts_path[1] == '/')
        ptr = ftsentry->fts_path+2;
    else
        ptr = ftsentry->fts_path;

    if (debug) log_debug("srch ptr: %s", ptr);
    char **p;
    p = NULL;
    p = utarray_find(mibl_config.exclude_dirs,
                     &ptr,
                     /* &ftsentry->fts_path, */
                     strsort);
    if  (p != NULL) {
        if (verbose) { // & (verbosity > 2)) {
            log_info(RED "Excluding:" CRESET " '%s'", ftsentry->fts_path);
        }
        return false;
    }

    /* for inclusions:
       if include_dirs is empty, default to ./ - include everything
       otherwise, iterate over include_dirs
       include if tbl contains prefix of fts_path
    */

    if (utarray_len(mibl_config.include_dirs) > 0) {
        p = NULL;
        while ( (p=(char**)utarray_next(mibl_config.include_dirs, p))) {
            log_debug("inclusion test pfx: '%s', path: '%s'",
                      *p, ftsentry->fts_path);
            log_debug("result: %d\n",
                      strncmp(*p, ftsentry->fts_path, strlen(*p)));
            if (strncmp(*p, ftsentry->fts_path, strlen(*p)) < 1) {
                if (verbose) { // & verbosity > 2) {
                    log_info("Include! '%s'", ftsentry->fts_path);
                }
                return true;
            };
        }
        if (verbose) { // & verbosity > 2) {
            log_debug("Include? '%s': %d", ftsentry->fts_path, false);
        }
        return false;
    } else {
        return true;
    }
}

EXPORT s7_pointer load_dune(const char *home_sfx, const char *traversal_root)
{
    if (debug) {
        log_debug(BLU "load_dune" CRESET);
        log_debug("%-16s%s", "launch_dir:", launch_dir);
        log_debug("%-16s%s", "base ws:", bws_root);
        log_debug("%-16s%s", "effective ws:", ews_root);
        log_debug("%-16s%s", "home_sfx:", home_sfx);
        log_debug("%-16s%s", "traversal_root:", traversal_root);
    }
    if (verbose) {
        printf(YEL "%-16s%s\n" CRESET, "current dir:", getcwd(NULL, 0));
        printf(YEL "%-16s%s\n" CRESET, "traversal_root:", traversal_root);
    }


    /*
      FIXME: traversal root(s) to be determined by miblrc.srcs.include
      default is cwd, but if miblrc designates 'include' dirs, then
      cwd must be excluded, and each 'include' dir traversed.
     */

    UT_string *abs_troot;
    utstring_new(abs_troot);
    if (debug) log_debug("build_wd: %s", build_wd);
    utstring_printf(abs_troot, "%s/%s",
                    //getcwd(NULL,0),
                    //build_wd,
                    ews_root,
                    traversal_root);
    char *abstr = strdup(utstring_body(abs_troot)); //FIXME: free after use
    char *_ews = effective_ws_root(abstr);
    if (debug) log_debug("ews: %s", _ews);
    ews_root = _ews;
    // put ews_root into the scheme env. so users can use it
    /* s7_define_variable(s7, */
    /*                    "effective-ws-root", */
    /*                    s7_make_string(s7, ews_root)); */

    if (debug) {
        log_debug("haystack (troot): %s", utstring_body(abs_troot));
        log_debug("needle (ews): %s", ews_root);
    }

    char *resolved_troot = strnstr(utstring_body(abs_troot),
                                   ews_root, strlen(ews_root));
    if (resolved_troot) {
        if (strlen(utstring_body(abs_troot)) == strlen(ews_root)) {
            /* resolved_troot = realpath(".",NULL); */
            /* log_debug("match: %s", resolved_troot); */
        } else {
            resolved_troot = utstring_body(abs_troot) + strlen(ews_root) + 1; // + for '/'
            /* log_debug("resolved_troot: %s", resolved_troot); */
        }
    } else {
        /* log_error("no resolved_troot"); */
        resolved_troot = realpath(".", NULL);
    }
    if (debug) {
        log_debug("resolved resolved_troot: %s", resolved_troot);
        log_debug("cwd: %s", getcwd(NULL, 0));
    }

    errno = 0;

    /*
      always cd to effective ws root, since the resolved traversal
      root is relative to it. that way ftsentry->fts_path will be a
      proper workspace-relative pkg-path.

      restore cwd after traversal.
    */
    char *old_cwd = getcwd(NULL, 0);
    if (strncmp(old_cwd, ews_root, strlen(ews_root)) != 0) {
        if (debug) {
            log_debug("chdir: %s => %s\n", old_cwd, ews_root);
        }
        rc = chdir(ews_root);
        if (rc != 0) {
            log_error("FAIL on chdir: %s => %s\n", old_cwd, ews_root);
            fprintf(stderr, RED "FAIL on chdir: %s => %s: %s\n",
                    old_cwd, ews_root, strerror(errno));
            exit(EXIT_FAILURE);
        }
        if (debug) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));
    }

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _traversal_root[] = {
        /* [0] = resolved_troot, // traversal_root; */
        [0] = (char *const)traversal_root,
        NULL
    };
    if (debug) log_debug("_traversal_root: %s", _traversal_root[0]);
    if (debug) log_debug("real _traversal_root: %s",
                         realpath(_traversal_root[0], NULL));

    errno = 0;
    tree = fts_open(_traversal_root,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &_compare
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return s7_error(s7, s7_make_symbol(s7, "fts_open"),
                        s7_list(s7, 2,
                                s7_make_string(s7, strerror(errno)),
                                s7_make_string(s7, _traversal_root[0])));
    }

    /* s7_pointer pkg_tbl = s7_make_hash_table(s7, PKG_CT); */
    /* s7_define_variable(s7, "pkg-tbl", pkg_tbl); */

    s7_pointer pkg_tbl =
        s7_eval_c_string(s7, "(cadr (assoc-in '(:@ :pkgs) -mibl-ws-table))");
    if (debug)
        log_debug("building pkg_tbl: %s", TO_STR(pkg_tbl));

    char *ext;

    if (verbose) {
        log_info(GRN "Beginning traversal" CRESET " at %s",
                 _traversal_root[0]);
                 // resolved_troot);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }

    /* TRAVERSAL STARTS HERE */
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            if (debug) {
                if (ftsentry->fts_info != FTS_DP) {
                    log_debug(CYN "ftsentry:" CRESET " %s (%s), type: %d",
                              ftsentry->fts_name,
                              ftsentry->fts_path,
                              ftsentry->fts_info);
                }
            }
            switch (ftsentry->fts_info)
                {
                case FTS_D : // dir visited in pre-order
                    if (trace)
                        log_trace("visiting dir: %s (%s) :: (%s)",
                                  ftsentry->fts_name,
                                  ftsentry->fts_path,
                                  ftsentry->fts_accpath);
                    if (_this_is_hidden(ftsentry)) {
                        fts_set(tree, ftsentry, FTS_SKIP);
                        break;
                    }
                    if (_include_this(ftsentry)) {
                        if (trace) log_info("INCLUDING %s",
                                            ftsentry->fts_path);
                        if (strncmp(ftsentry->fts_name, "_build", 6) == 0) {
                            /* skip _build (dune) */
                            fts_set(tree, ftsentry, FTS_SKIP);
                            break;
                        }
                        dir_ct++;
                        _handle_dir(pkg_tbl, tree, ftsentry);
                        /* printf("pkg tbl: %s\n", TO_STR(pkg_tbl)); */
                    } else {
                        fts_set(tree, ftsentry, FTS_SKIP);
                    }
                    break;
                case FTS_DP:
                    /* postorder directory */
                    break;
                case FTS_F : // regular file
                    file_ct++;

                    if (strncmp(ftsentry->fts_name,"BUILD.bazel", 11)==0){
                            /* skip BUILD.bazel files */
                            break;
                    }
                    /* TODO: skip *.bzl files */
                    /* TODO: skip standard files: READMEs, LICENSE, etc. */
                    /* _handle_regular_file(ftsentry); */
                    if (strncmp(ftsentry->fts_name, "dune-project", 12)
                        == 0) {
                        _handle_dune_project_file(pkg_tbl, ftsentry);
                        break;
                    }
                    if ((strncmp(ftsentry->fts_name, "dune", 4) == 0)
                        /* don't read dune.foo */
                        && (strlen(ftsentry->fts_name) == 4)) {
                        _handle_dune_file(pkg_tbl, ftsentry);
                        break;
                    }

                    ext = strrchr(ftsentry->fts_name, '.');

                    if (ext) {
                        if ((strncmp(ext, ".ml", 3) == 0)) {
                            _handle_ml_file(pkg_tbl, ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".md", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_ml_file(pkg_tbl, ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".sh", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_file(pkg_tbl, ftsentry, ext);
                            /*_handle_script_file(pkg_tbl, ftsentry, ext);*/
                        }
                        else if ((strncmp(ext, ".py", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_file(pkg_tbl, ftsentry, ext);
                            /*_handle_script_file(pkg_tbl, ftsentry, ext);*/
                        }
                        else if ((strncmp(ext, ".opam", 5) == 0)
                                 && (strlen(ext) == 5)) {
                            _handle_opam_file(pkg_tbl, ftsentry);
                        }
                        else if (strncmp(ext, ".ocamlformat", 12) == 0) {
                            _handle_ocamlformat_file(pkg_tbl, ftsentry);
                        }
                        else {
                            _handle_file(pkg_tbl, ftsentry, ext);
                        }
                    }
                    else {
                        /* no extension */
                        if (strstr(ftsentry->fts_name, "opam")) {
                            _handle_opam_file(pkg_tbl, ftsentry);
                        }
                        else {
                            _handle_file(pkg_tbl, ftsentry, ext);
                        }
                    }
                    break;
                case FTS_SL: // symlink
                    file_ct++;
                    _handle_symlink(pkg_tbl, tree, ftsentry);
                    break;
                case FTS_SLNONE:
                    /* symlink to non-existent target */
                    log_warn("FTS_SLNONE: %s", ftsentry->fts_path);
                    break;
                case FTS_ERR:
                    log_error("FTS_ERR: %s", ftsentry->fts_path);
                    log_error("  error: %d: %s", ftsentry->fts_errno,
                              strerror(ftsentry->fts_errno));
                    break;
                case FTS_DC:
                    /* dir causing a cycle dir */
                    log_warn("FTS_DC: %s", ftsentry->fts_path);
                    break;
                case FTS_DNR:
                    /* unreadable dir */
                    log_warn("FTS_DNR: %s", ftsentry->fts_path);
                    break;
                case FTS_NS:
                    /* no stat info, error */
                    log_error("FTS_NS: %s", ftsentry->fts_path);
                    log_error("  error: %d: %s", ftsentry->fts_errno,
                              strerror(ftsentry->fts_errno));
                    break;
                case FTS_NSOK:
                    /* no stat info, not an error */
                    log_warn("FTS_NSOK: %s", ftsentry->fts_path);
                    break;
                case FTS_DEFAULT:
                    log_warn("FTS_DEFAULT: %s", ftsentry->fts_path);
                    break;
                /* case FTS_DOT : // not specified to fts_open */
                /*     // do not process children of hidden dirs */
                /*     /\* fts_set(tree, ftsentry, FTS_SKIP); *\/ */
                /*     break; */
                default:
                    log_error(RED "Unhandled FTS type %d\n",
                              ftsentry->fts_info);
                    exit(EXIT_FAILURE);
                    break;
                }
        }
        chdir(old_cwd);
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    }
    /* s7_pointer pkg_tbl = */
    /*     s7_eval_c_string(s7, "(set-cdr! (assoc-in '(:@ :pkgs) -mibl-ws-table))"); */

    if (verbose) {
        log_info("cwd: %s", getcwd(NULL, 0));
        log_info("bws: %s", bws_root);
        log_info("ews: %s", ews_root);
        log_info("dir count: %d", dir_ct);
        log_info("file count: %d", file_ct);
        log_info("dunefile count: %d", dunefile_ct);
        log_info("exiting load_dune");
    }

    /* we were called by g_load_dune, which expects pkg tbl: */
    return pkg_tbl;
}
