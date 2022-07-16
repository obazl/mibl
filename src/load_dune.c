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
    }

    s7_pointer stanzas = s7_list(s7, 0);

    close_error_config();
    error_config();
    /* repeat until all objects read */
    while(true) {
        s7_pointer stanza = s7_read(s7, port);
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            if (debug)
                log_error("[%s\n]", errmsg);
            s7_close_input_port(s7, port);
            //if ".)", read file into buffer, convert to "\.)", then
            // read with the scheme reader
            if (strstr(errmsg, "\"unexpected close paren:")
                != NULL) {
                s7_close_input_port(s7, port);
                /* clear out old error */
                close_error_config();
                s7_pointer fixed = fix_baddot(dunefile_name);
                log_debug("FIXED: %s", TO_STR(fixed));
                if (s7_is_null(s7,stanzas)) {
                    /* stanzas = s7_list(s7, 1, fixed); */
                    /* fixed; */
                } else{
                    stanzas = s7_append(s7, stanzas, fixed);
                                        /* s7_list(s7, 1, fixed)); */
                }
            }
            /* s7_quit(s7); */
            /* exit(EXIT_FAILURE); */
            break;
        }
        if (stanza == s7_eof_object(s7)) break;
        /* log_debug("SEXP: %s", TO_STR(stanza)); */
        if (debug)
            log_debug("stanza: %s",
                  TO_STR(s7_car(stanza)));
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
            utstring_printf(dunepath, "%s/%s", path, TO_STR(inc_file));
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
        /* if (debug) */
        /*     log_debug("stanzas: %s", TO_STR(stanzas)); */
    }
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

/*
  create a pkg-tbl entry if dir contains a dune file or at least one
  OCaml source file.
 */
LOCAL void _handle_dir(s7_pointer pkg_tbl, FTS* tree, FTSENT *ftsentry)
{
    if (debug) {
        log_debug("_handle_dir: %s", ftsentry->fts_name);
        log_info("%-20s%s", "base ws:", bws_root);
        log_info("%-20s%s", "effective ws:",ews_root);
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }

    if (strncmp(ews_root, ftsentry->fts_path,
                 strlen(ftsentry->fts_path)) != 0) {
        /* is this a ws root? */
        if (_is_ws_root(ftsentry)) {
            log_debug("SKIPPING ws root: %s", ftsentry->fts_path);
            log_debug("SKIPPING ws root: %s", ftsentry->fts_path);
            /* do not process */
            fts_set(tree, ftsentry, FTS_SKIP);
            return;
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

    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            printf("Skipping dotted dir: %s/%s\n",
                   ftsentry->fts_path, ftsentry->fts_name);
            fts_set(tree, ftsentry, FTS_SKIP);
            return;
        } else
            printf("ROOT DOT dir\n");
    }


    /* // fts_path is relative to traversal root */
    /* printf("DIR path    %s\n", ftsentry->fts_path); */

    /* // fts_accpath is relative to current dir */
    /* printf("DIR accpath %s\n", ftsentry->fts_accpath); */

    s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
    printf(RED "pkg-path: %s" CRESET "\n", TO_STR(key));
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
#define TAG_ML  1

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
            log_debug("no entry for this pkg");
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

            if (debug) {
                log_debug("updating :files");
                log_debug("files_alist: %s", TO_STR(files_alist));
            }

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
                s7_list(s7, 2,
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

LOCAL void _update_pkg_modules(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
    if (trace) {
        log_trace("_UPDATE_PKG_MODULES");
    }
    if (debug) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl: %s", TO_STR(pkg_tbl)); */
    }
    s7_pointer pkg_key = s7_make_string(s7, pkg_name);
    if (debug)
        log_debug("pkg_key: %s", TO_STR(pkg_key));
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) */
    /*     log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, modules_kw, static_kw);
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
                                                      ftype == TAG_ML
                                                      ?"ml"
                                                      :ftype == TAG_MLI
                                                      ?"mli"
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

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, module_assoc);

            s7_pointer modules_assoc = s7_list(s7, 2,
                                               modules_kw, statics_assoc);
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

            s7_pointer keypath = s7_list(s7, 3,
                                         /* srcs_kw, */
                                         modules_kw,
                                         static_kw,
                                         mname_sym);
            if (debug) {
                log_debug("assoc-in: %s", TO_STR(assoc_in));
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
        log_trace(RED "_UPDATE_PKG_SIGS" CRESET);
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
        log_trace(RED "_UPDATE_PKG_STRUCTS" CRESET);
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
        s7_pointer keypath = s7_list(s7, 2, structs_kw, static_kw);
        s7_pointer structs_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
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
                log_debug("INITIALIZING :structures field");
            /* (:structures ((:Foo (:ml foo.ml)) */
            /*               (:Bar (:ml bar.ml)))) */

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, struct_assoc);

            s7_pointer structs_assoc = s7_list(s7, 2,
                                            structs_kw, statics_assoc);
            if (debug) log_debug("structs_assoc: %s", TO_STR(structs_assoc));

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         structs_assoc));
            /* if (debug) */
            /*     log_debug("pkg_alist: %s", */
            /*            TO_STR(new_pkg_alist)); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
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
                log_debug("assoc-in: %s", TO_STR(assoc_in));
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

LOCAL void _update_mli(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* printf("_update_mli: "); */
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    if (verbose) {
        printf(BLU ":module" CRESET " %s; ", mname);
        printf("pkg name: %s; fname: %s\n", pkg_name, ftsentry->fts_name);
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
        printf(BLU ":module" CRESET " %s; ", mname);
        printf("pkg name: %s; fname: %s\n", pkg_name, ftsentry->fts_name);
    }

    char *ml_name = strdup(ftsentry->fts_name);

    /* dirname may mutate its arg, use a copy */
    char *dname = strdup(ftsentry->fts_path);
    UT_string *mli_test;
    utstring_new(mli_test);
    /* add terminal 'i' with printf */
    utstring_printf(mli_test, "%s/%si", dirname(dname), ml_name);
    if (trace) {
        log_debug(RED "Checking for companion .mli:" CRESET  " %s",
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
    }
    else if ((strncmp(ext, ".mli", 4) == 0)
        && (strlen(ext) == 4)) {
        /* printf("%s", "mli"); */
        _update_mli(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mlt", 4) == 0)
        /* .mlt - ocamlformat file, not a module */
        && (strlen(ext) == 4)) {
        printf(":%-6s %s\n" CRESET, "mlt", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mll", 4) == 0)
        && (strlen(ext) == 4)) {
        printf(BLU ":%-6s" CRESET " %s\n", "mll", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mly", 4) == 0)
        && (strlen(ext) == 4)) {
        printf(BLU ":%-6s" CRESET " %s\n", "mly", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mlh", 4) == 0)
        && (strlen(ext) == 4)) {
        printf(RED ":%-6s %s\n" CRESET, "mlh", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mllib", 6) == 0)
        && (strlen(ext) == 6)) {
        printf(RED ":%-6s %s\n" CRESET, "mllib", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mligo", 6) == 0)
        && (strlen(ext) == 6)) {
        /* tezos */
        printf(BLU ":%-6s" CRESET " %s\n", "mligo", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".mldylib", 8) == 0)
        && (strlen(ext) == 8)) {
        /* mina */
        printf(RED ":%-6s %s\n" CRESET, "mldylib", ftsentry->fts_name);
    }
    else if ((strncmp(ext, ".md", 3) == 0)
        && (strlen(ext) == 3)) {
        printf(BLU ":%-6s" CRESET " %s\n",
               "md", ftsentry->fts_name);
    }
    else {
        printf(RED "UNKNOWN ml ext: :%-6s\n" CRESET, ext);
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

    _update_pkg_files(pkg_tbl, ftsentry, ext);
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
    /* log_debug("stanzas: %s", TO_STR(stanzas)); */

    s7_pointer pkg_key = s7_make_string(s7, dirname(ftsentry->fts_path));

    if (debug)
        log_debug("pkg key: %s", TO_STR(pkg_key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (debug) */
    /*     log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    s7_pointer assoc = _load_assoc();
    if (assoc == s7_undefined(s7)) {
        log_error("unbound symbol: assoc");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "assoc")));
    }
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

    s7_pointer key = s7_make_string(s7, dirname(ftsentry->fts_path));
    if (debug)
        log_debug("pkg key: %s", TO_STR(key));

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, key);
    /* if (debug) */
    /*     log_debug("pkg_alist: %s", TO_STR(pkg_alist)); */

    s7_pointer dune_project_assoc = s7_cons(s7, dune_project_sym,
                                            stanzas);

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
        printf("Skipping Bazel dir: %s\n", ftsentry->fts_name);
        fts_set(tree, ftsentry, FTS_SKIP);
        return;
    }

    if (strncmp(ftsentry->fts_name, "dune", 4) == 0) {
        log_warn("SYMLINKED dunefile: %s\n", ftsentry->fts_name);
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
    printf("_get_path_dir: %s\n", TO_STR(arg));
    const char *pathdir = s7_string(arg);

    if (pathdir[0] == '/') {
        log_error("Path arg must be relative");
        return NULL;
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
        log_debug("g_load_dune, args: %s", TO_STR(args));
        log_debug("build_wd: %s (=BUILD_WORKING_DIRECTORY)", build_wd);
        log_debug("launch_dir: %s", launch_dir);
        log_debug("base ws root: %s", bws_root);
        log_debug("effective ws root: %s", ews_root);
        log_debug("cwd: %s", getcwd(NULL, 0));
    }

    /* s7_pointer wss =  */
    ///s7_pointer root_ws =
    initialize_mibl_data_model(s7);

    s7_pointer _pkg_tbl =
        s7_eval_c_string(s7, "(cadr (assoc-in '(:@ :pkgs) -mibl-ws-table))");
    /* printf("pkg_tbl: %s\n", TO_STR(_pkg_tbl)); */

    const char *rootdir, *pathdir;

    if ( s7_is_null(s7, args) ) {
        rootdir = getcwd(NULL, 0);
        pathdir = ".";
        _pkg_tbl = load_dune(rootdir, pathdir);
        printf(RED "LOADED DUNE NOARG" CRESET "\n");
        return s7_name_to_value(s7, "-mibl-ws-table");
    } else {
        s7_int args_ct = s7_list_length(s7, args);
        if (debug)
            log_debug("args ct: %d\n", args_ct);

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
                        s7_pointer _pkgs = load_dune(rootdir, pathdir);
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
                printf("Rootdir: %s\n", rootdir);
                pathdir = _get_path_dir(arg);
                /* s7_pointer q = s7_name_to_value(s7, "quote"); */
                if (pathdir) {
                    s7_pointer _pkg_tbl = load_dune(rootdir, pathdir);
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
}

/* FIXME: use same logic for rootdir as */
EXPORT s7_pointer load_dune(const char *home_sfx, const char *traversal_root)
{
    printf("load_dune\n");
    if (debug) {
        log_debug("load_dune");
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
        [0] = resolved_troot, // traversal_root;
        NULL
    };
    if (debug) log_debug("_traversal_root: %s\n", _traversal_root[0]);
    if (debug) log_debug("real _traversal_root: %s\n",
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
    log_debug("building pkg_tbl: %s\n", TO_STR(pkg_tbl));

    char *ext;

    /* TRAVERSAL STARTS HERE */
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            if (debug) {
                log_debug("ftsentry: %2/%s, type: %d",
                          ftsentry->fts_path,
                          ftsentry->fts_name,
                          ftsentry->fts_info);
            }
            switch (ftsentry->fts_info)
                {
                case FTS_DOT : // not specified to fts_open
                    // do not process children of hidden dirs
                    /* fts_set(tree, ftsentry, FTS_SKIP); */
                    break;
                case FTS_D : // dir visited in pre-order
                    if (strncmp(ftsentry->fts_name, "_build", 6) == 0) {
                        /* skip _build (dune) */
                        fts_set(tree, ftsentry, FTS_SKIP);
                        break;
                    }

                    dir_ct++;
                    _handle_dir(pkg_tbl, tree, ftsentry);
                    /* printf("pkg tbl: %s\n", TO_STR(pkg_tbl)); */
                    break;
                case FTS_DP:
                    /* postorder directory */
                    break;
                case FTS_F : // regular file
                    file_ct++;
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
                            _handle_script_file(pkg_tbl, ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".py", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_script_file(pkg_tbl, ftsentry, ext);
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
    if (trace) {
        log_debug("cwd: %s", getcwd(NULL, 0));
        log_debug("FTS_D: %d", FTS_D);
        log_debug("FTS_DP: %d", FTS_DP);
        log_debug("FTS_F: %d", FTS_F);
        log_debug("exiting load_dune");
    }
    /* s7_pointer pkg_tbl = */
    /*     s7_eval_c_string(s7, "(set-cdr! (assoc-in '(:@ :pkgs) -mibl-ws-table))"); */

    /* we were called by g_load_dune, which expects pkg tbl: */
    return pkg_tbl;
}
