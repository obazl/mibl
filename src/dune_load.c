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
#include <sys/types.h>
#include <unistd.h>

#if INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "log.h"
#include "dune_load.h"

UT_array  *segs;
UT_string *group_tag;

int dunefile_ct = 0;
int file_ct = 0;
int dir_ct  = 0;

void _indent(int i)
{
    /* printf("_indent: %d\n", i); */
    for (; i > 0; i--)
        printf("    ");
}

UT_string *dunefile_name;

LOCAL s7_pointer _read_dunefile(char *path, char *fname)
{
    if (debug)
        log_debug("_read_dunefile %s/%s", path, fname);

    /* read dunefile */
    utstring_renew(dunefile_name);
    utstring_printf(dunefile_name, "%s/%s", path, fname);
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
            if (strstr(errmsg, ";read-error (\"unexpected close paren:")
                != NULL) {
                s7_close_input_port(s7, port);
                /* clear out old error */
                close_error_config();
                s7_pointer fixed = fix_baddot(dunefile_name);
                log_debug("FIXED: %s", TO_STR(fixed));
                if (s7_is_null(s7,stanzas)) {
                    log_debug("xxxxxxxxxxxxxxxx");
                    /* stanzas = s7_list(s7, 1, fixed); */
                    fixed;
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
        /* log_debug("SEXP: %s", s7_object_to_c_string(s7, stanza)); */
        if (debug)
            log_debug("stanza: %s",
                  s7_object_to_c_string(s7, s7_car(stanza)));
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
            s7_pointer nested = _read_dunefile(path, TO_STR(inc_file));
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
        if (debug)
            log_debug("stanzas: %s", TO_STR(stanzas));
    }
    s7_close_input_port(s7, port);

    return stanzas;
    /* s7_close_input_port(s7, port); */
    /* s7_gc_unprotect_at(s7, gc_loc); */
}

/*
  create a pkg-tbl entry if dir contains a dune file or at least one
  OCaml source file.
 */
LOCAL void _handle_dir(FTS* tree, FTSENT *ftsentry)
{
    /* printf("_handle_dir %s\n", ftsentry->fts_name); */

    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            printf("Skipping dotted dir: %s\n", ftsentry->fts_name);
            fts_set(tree, ftsentry, FTS_SKIP);
            return;
        } else
            printf("ROOT DOT dir\n");
    }

    /* // fts_path is relative to traversal root */
    /* printf("DIR path    %s\n", ftsentry->fts_path); */

    /* // fts_accpath is relative to current dir */
    /* printf("DIR accpath %s\n", ftsentry->fts_accpath); */

    static char buf[256];
    strlcpy(buf, ftsentry->fts_path, 256);
    strlcat(buf, "/dune", 256);
    /* printf("checking %s\n", buf); */

    char *s1;
    if (access(buf, F_OK) == 0) {
        /* should only happen once per directory */
        /* in case 'dune' is a directory, access fails */

        /* avoid tuareg dunefiles! */
        FILE *fileStream;
        char fileText [100];
        utstring_renew(dunefile_name);
        utstring_printf(dunefile_name, "%s/dune", ftsentry->fts_path);
        fileStream = fopen(utstring_body(dunefile_name), "r");
        fgets(fileText, 100, fileStream);
        char *r = strstr(fileText, "tuareg");
        if (r != NULL) {
            log_debug("TUAREG! %s/dune", ftsentry->fts_path);
            fclose(fileStream);
            s7_pointer pkgs = s7_name_to_value(s7, "pkg-tbl");
            s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
            /* s7_pointer test_assoc = s7_list(s7, 2, */
            /*                                 s7_make_keyword(s7, "test"), */
            /*                                 s7_make_symbol(s7, "dummy")); */
            s7_pointer result =
                s7_hash_table_set(s7, pkgs, key,
                                  s7_list(s7, 2,
                                          s7_make_keyword(s7, "pkg-path"),
                                          key));
            goto rest;
        }
        fclose(fileStream);

        dunefile_ct++;
        s7_pointer stanzas = _read_dunefile(ftsentry->fts_path, "dune");

        /* add entry to pkg-tbl */
        /* printf("DUNE PKG at %s\n", ftsentry->fts_path); */
        s7_pointer pkgs = s7_name_to_value(s7, "pkg-tbl");
        s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
        s7_pointer dune_assoc = s7_cons(s7,
                                        s7_make_keyword(s7, "stanzas"),
                                        stanzas);
        s7_pointer result =
            s7_hash_table_set(s7, pkgs, key,
                              /* s7_list(s7, 2, */
                              s7_append(s7,
                                      s7_list(s7, 1,
                                              s7_list(s7, 2,
                                              s7_make_keyword(s7, "pkg-path"),
                                                      key)
                                              ),
                                        s7_list(s7, 1,
                                                dune_assoc)));

        /* printf("hts res: %s\n", s7_object_to_c_string(s7, result)); */
    } else {
        /* no dunefile in dir, make empty :stanzas alist */
        s7_pointer pkgs = s7_name_to_value(s7, "pkg-tbl");
        s7_pointer key = s7_make_string(s7, ftsentry->fts_path);
        /* s7_pointer test_assoc = s7_list(s7, 2, */
        /*                                 s7_make_keyword(s7, "test"), */
        /*                                 s7_make_symbol(s7, "dummy")); */
        /* s7_pointer result = s7_hash_table_set(s7, pkgs, key, */
        /*                                       s7_list(s7, 1, test_assoc)); */
        s7_pointer result =
            s7_hash_table_set(s7, pkgs, key,
                              s7_list(s7, 2,
                                      s7_list(s7, 2,
                                              s7_make_keyword(s7, "pkg-path"),
                                              key),
                                      s7_list(s7, 2,
                                              s7_make_keyword(s7, "stanzas"),
                                              s7_nil(s7))));
    }
 rest:
    _indent(ftsentry->fts_level);
    printf("%d. %s",
           ftsentry->fts_level,
           /* ftsentry->fts_name, */
           ftsentry->fts_path);
    /* FTSENT *p = ftsentry->fts_parent; */
    /* while (p) { */
    /*     if (strlen(p->fts_name) > 0) */
    /*         printf("\\%s", p->fts_name); */
    /*     p = p->fts_parent; */
    /* } */
    printf("\n");
}

static char principal[256];

LOCAL char *_module_name(FTSENT *ftsentry, char *ext)
{
    strlcpy(principal, ftsentry->fts_name, 256);
    principal[ext - ftsentry->fts_name] = '\0';
    principal[0] = toupper(principal[0]);
    return (char *)principal;
}

static s7_pointer alist_updt_in_fn(s7_scheme *s, s7_pointer args)
{
    log_debug("running updater");
    return(s7_string("HELLO"));
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

LOCAL void _update_pkg_files(FTSENT *ftsentry, char *ext)
{
    if (debug)
        log_debug("_update_pkg_files: %s, ext: %s",
                  ftsentry->fts_name, ext);

    if (_exclusions(ftsentry, ext)) {
        if (debug)
            log_warn("excluding %s", ftsentry->fts_name);
        return;
    }

    s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl");
    if (debug)
        log_debug("pkg_tbl: %s", s7_object_to_c_string(s7, pkg_tbl));

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_kw = s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_kw);
    if (debug)
        log_debug("pkg_alist: %s", s7_object_to_c_string(s7, pkg_alist));

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
        if (debug)
            log_debug("no entry for this pkg");
    } else {
        s7_pointer files_kw = s7_make_keyword(s7, "files");

        s7_pointer assoc = s7_name_to_value(s7, "assoc");

        s7_pointer files_list
            = s7_call(s7, assoc, s7_list(s7, 2, files_kw, pkg_alist));
        if (debug)
            log_debug("files_list %s",
                      s7_object_to_c_string(s7, files_list));

        if (files_list == s7_f(s7)) {
            if (debug)
                log_debug("adding :files");

            s7_pointer file_list =
                s7_list(s7, 1,
                        s7_list(s7, 2,
                                files_kw,
                                s7_make_string(s7, ftsentry->fts_name)));
            if (debug)
                log_debug("file_list: %s", TO_STR(file_list));

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            if (debug)
                log_debug("new pkg: %s", TO_STR(new_pkg));

            s7_hash_table_set(s7, pkg_tbl, pkg_kw, new_pkg);

        } else {
            if (debug) {
                log_debug("updating :files");
                /* log_debug("srcs_alist: %s", */
                /*        s7_object_to_c_string(s7, srcs_alist)); */
                log_debug("files_list: %s",
                       s7_object_to_c_string(s7, files_list));
            }
            s7_pointer new_files_list =
                s7_cons(s7,
                        s7_make_string(s7, ftsentry->fts_name),
                        s7_cdr(files_list));
            log_debug("new files_list: %s",
                       s7_object_to_c_string(s7, new_files_list));

            s7_pointer sort = s7_name_to_value(s7, "sort!");
            s7_pointer lt = s7_name_to_value(s7, "string<?");
            s7_pointer sorted
                = s7_call(s7, sort, s7_list(s7, 2,
                                            new_files_list,
                                            lt));

            /* log_debug("new files_list sorted: %s", */
            /*           s7_object_to_c_string(s7, sorted)); */

            s7_set_cdr(files_list, sorted);
            log_debug("files_list: %s",
                      s7_object_to_c_string(s7, files_list));

            /* s7_hash_table_set(s7, pkg_tbl, pkg_kw, new_pkg); */

        }
    }
}

LOCAL void _update_pkg_script_files(FTSENT *ftsentry, char *ext)
{
    if (debug)
        log_debug("_update_pkg_script_files: %s, ext: %s",
                  ftsentry->fts_name, ext);

    if (_exclusions(ftsentry, ext)) {
        if (debug)
            log_warn("excluding %s", ftsentry->fts_name);
        return;
    }

    s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl");
    if (debug)
        log_debug("pkg_tbl: %s", s7_object_to_c_string(s7, pkg_tbl));

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_kw = s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_kw);
    if (debug)
        log_debug("pkg_alist: %s", s7_object_to_c_string(s7, pkg_alist));

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
        if (debug)
            log_debug("no entry for this pkg");
    } else {
        s7_pointer scripts_kw = s7_make_keyword(s7, "scripts");

        s7_pointer assoc = s7_name_to_value(s7, "assoc");

        s7_pointer scripts_list
            = s7_call(s7, assoc, s7_list(s7, 2, scripts_kw, pkg_alist));
        if (debug)
            log_debug("scripts_list %s",
                      s7_object_to_c_string(s7, scripts_list));

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

            s7_hash_table_set(s7, pkg_tbl, pkg_kw, new_pkg);

        } else {
            if (debug) {
                log_debug("updating :scripts");
                /* log_debug("srcs_alist: %s", */
                /*        s7_object_to_c_string(s7, srcs_alist)); */
                log_debug("scripts_list: %s",
                       s7_object_to_c_string(s7, scripts_list));
            }
            s7_pointer new_scripts_list =
                s7_cons(s7,
                        s7_make_string(s7, ftsentry->fts_name),
                        s7_cdr(scripts_list));
            log_debug("new scripts_list: %s",
                       s7_object_to_c_string(s7, new_scripts_list));

            s7_pointer sort = s7_name_to_value(s7, "sort!");
            s7_pointer lt = s7_name_to_value(s7, "string<?");
            s7_pointer sorted
                = s7_call(s7, sort, s7_list(s7, 2,
                                            new_scripts_list,
                                            lt));

            /* log_debug("new scripts_list sorted: %s", */
            /*           s7_object_to_c_string(s7, sorted)); */

            s7_set_cdr(scripts_list, sorted);
            log_debug("scripts_list: %s",
                      s7_object_to_c_string(s7, scripts_list));

            /* s7_hash_table_set(s7, pkg_tbl, pkg_kw, new_pkg); */
        }
    }
}

LOCAL void _update_pkg_modules(char *pkg_name, char *mname,
                               char *fname, int ftype)
{
    s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl");
    if (debug)
        log_debug("pkg_tbl: %s", s7_object_to_c_string(s7, pkg_tbl));

    s7_pointer pkg_kw = s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_kw);
    if (debug)
        log_debug("pkg_alist: %s", s7_object_to_c_string(s7, pkg_alist));

    if (pkg_alist == s7_f(s7)) {
        if (debug)
            log_debug("no dunefile in this directory");
    } else {

        s7_pointer mname_sym   = s7_make_symbol(s7, mname);
        s7_pointer modules_kw = s7_make_keyword(s7, "modules");
        s7_pointer srcs_kw    = s7_make_keyword(s7, "srcs");

        s7_pointer assoc = s7_name_to_value(s7, "assoc");
        s7_pointer modules_alist
            = s7_call(s7, assoc, s7_list(s7, 2, modules_kw, pkg_alist));
        if (debug)
            log_debug("modules_alist %s",
                      s7_object_to_c_string(s7, modules_alist));

        /* s7_pointer srcs_alist = s7_call(s7, assoc, */
        /*                                 s7_list(s7, 2, srcs_kw, pkg_alist)); */
        /* log_debug(":srcs_alist %s", */
        /*        s7_object_to_c_string(s7, srcs_alist)); */
        /* log_debug(":srcs null?: %d", s7_is_null(s7, srcs_alist)); */
        /* log_debug(":srcs false?: %d", s7_f(s7) ==  srcs_alist); */

        s7_pointer ml_assoc = s7_list(s7, 2,
                                      s7_make_keyword(s7,
                                                      ftype == TAG_ML
                                                      ?"ml"
                                                      :ftype == TAG_MLI
                                                      ?"mli"
                                                      :"UNKNOWN"
                                                      ),
                                      s7_make_string(s7, fname));
        if (debug)
            log_debug("ml_assoc: %s", s7_object_to_c_string(s7, ml_assoc));

        /* if (srcs_alist == s7_f(s7)) { */
        if (modules_alist == s7_f(s7)) {
            if (debug)
                log_debug("adding :modules");
            /* (acons :srcs */
            /*  ((:modules ((:Foo ((:ml foo.ml) (:mli foo.mli)) */
            /*              (:Bar ((:ml bar.ml) (:mli bar.mli))))))) */
            /*  pkg_alist) */
            /* s7_pointer msrcs_alist = s7_list(s7, 1, ml_assoc); */
            /* log_debug("msrcs_alist: %s", s7_object_to_c_string(s7, msrcs_alist)); */

            s7_pointer module_assoc = s7_list(s7, 2,
                                              mname_sym,
                                              ml_assoc);
            /* msrcs_alist); */
            if (debug)
                log_debug("module_assoc: %s", s7_object_to_c_string(s7, module_assoc));

            /* s7_pointer modules_alist = s7_list(s7, 1, module_assoc); */
            /* log_debug("modules_alist: %s", */
            /*        s7_object_to_c_string(s7, modules_alist)); */

            s7_pointer modules_assoc = s7_list(s7, 2,
                                               modules_kw,
                                               module_assoc);
            if (debug)
                log_debug("modules_assoc: %s",
                   s7_object_to_c_string(s7, modules_assoc));

            /* s7_pointer srcs_alist = s7_list(s7, 1, modules_assoc); */
            /* log_debug("srcs_alist: %s", */
            /*        s7_object_to_c_string(s7, srcs_alist)); */

            /* s7_pointer srcs_assoc = s7_list(s7, 2, */
            /*                                 srcs_kw, */
            /*                                 modules_assoc); */
            /* /\* srcs_alist); *\/ */
            /* if (debug) */
            /*     log_debug("srcs_assoc: %s", s7_object_to_c_string(s7, srcs_assoc)); */

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         /* srcs_assoc */
                                                         modules_assoc));
            if (debug)
                log_debug("pkg_alist: %s",
                       s7_object_to_c_string(s7, new_pkg_alist));

            s7_hash_table_set(s7, pkg_tbl, pkg_kw, new_pkg_alist);
        } else {
            if (debug) {
                log_debug("updating :modules");
                /* log_debug("srcs_alist: %s", */
                /*        s7_object_to_c_string(s7, srcs_alist)); */
                log_debug("modules_alist: %s",
                       s7_object_to_c_string(s7, modules_alist));
                log_debug("mname_sym: %s", s7_object_to_c_string(s7, mname_sym));
            }

            s7_pointer assoc_in = s7_name_to_value(s7, "assoc-in");
            s7_pointer keypath = s7_list(s7, 2,
                                         /* srcs_kw, */
                                         modules_kw,
                                         mname_sym);
            if (debug)
                log_debug("keypath: %s",
                   s7_object_to_c_string(s7, keypath));
            s7_pointer module_alist = s7_call(s7, assoc_in,
                                              s7_list(s7, 2,
                                                      keypath,
                                                      pkg_alist));
            if (module_alist == s7_f(s7)) {
                /* new */
                if (debug)
                    log_debug("New module at %s",
                              s7_object_to_c_string(s7, keypath));
                keypath = s7_list(s7, 1,
                                  /* srcs_kw, */
                                  modules_kw);
                if (debug)
                    log_debug("trying keypath: %s",
                              s7_object_to_c_string(s7, keypath));
                s7_pointer modules_alist =
                    s7_call(s7, assoc_in, s7_list(s7, 2, keypath, pkg_alist));
                if (debug)
                    log_debug("modules_alist: %s",
                           s7_object_to_c_string(s7, modules_alist));

                s7_pointer malist_cdr = s7_cdr(modules_alist);
                if (debug)
                    log_debug("malist_cdr: %s",
                       s7_object_to_c_string(s7, malist_cdr));

                s7_pointer module_assoc = s7_list(s7, 2,
                                                  mname_sym,
                                                  ml_assoc);
                if (debug)
                    log_debug("new module_assoc: %s",
                       s7_object_to_c_string(s7, module_assoc));

                s7_pointer new_malist_cdr =
                    s7_cons(s7, module_assoc, malist_cdr);
                if (debug)
                    log_debug("new_malist_cdr: %s",
                       s7_object_to_c_string(s7, new_malist_cdr));

                s7_pointer new_malist = s7_set_cdr(modules_alist,
                                                   new_malist_cdr);
                if (debug)
                    log_debug("new_malist: %s",
                       s7_object_to_c_string(s7, modules_alist));
            } else {
                /* update */
                if (debug)
                    log_debug("module_alist: %s",
                       s7_object_to_c_string(s7, module_alist));

                s7_pointer malist_cdr = s7_cdr(module_alist);
                if (debug)
                    log_debug("malist_cdr: %s",
                       s7_object_to_c_string(s7, malist_cdr));

                s7_pointer msrcs = s7_cons(s7, ml_assoc, malist_cdr);
                if (debug)
                    log_debug("msrcs: %s", s7_object_to_c_string(s7, msrcs));

                s7_pointer new_malist = s7_set_cdr(module_alist, msrcs);
                if (debug) {
                    log_debug("new_malist: %s", s7_object_to_c_string(s7, module_alist));

                    log_debug("new pkgs: %s",
                       s7_object_to_c_string(s7, pkg_alist));
                }
            }
        }
    }
}

LOCAL void _update_mli(FTSENT *ftsentry, char *ext)
{
    /* printf("_update_mli: "); */
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    printf(BLU ":module" CRESET " %s; ", mname);
    printf("pkg name: %s; fname: %s\n", pkg_name, ftsentry->fts_name);
    _update_pkg_modules(pkg_name, mname,
                                ftsentry->fts_name, TAG_MLI);
}

LOCAL void _update_ml(FTSENT *ftsentry, char *ext)
{
    /* printf("_update_ml: "); */
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
    printf(BLU ":module" CRESET " %s; ", mname);
    printf("pkg name: %s; fname: %s\n", pkg_name, ftsentry->fts_name);
    _update_pkg_modules(pkg_name, mname,
                                ftsentry->fts_name, TAG_ML);
}

/*
  if no entry in pkg-tbl for ctx dir, add one
 */
LOCAL void _handle_ml_file(FTSENT *ftsentry, char *ext)
{
    /* log_debug("_handle_ml_file %s, %s\n", */
    /*           ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    _indent(ftsentry->fts_level);

    printf("%d. " CRESET, ftsentry->fts_level);

    if ((strncmp(ext, ".ml", 3) == 0)
        && (strlen(ext) == 3)) {
        /* printf(":%-6s", "ml"); */
        _update_ml(ftsentry, ext);
    }
    else if ((strncmp(ext, ".mli", 4) == 0)
        && (strlen(ext) == 4)) {
        /* printf("%s", "mli"); */
        _update_mli(ftsentry, ext);
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
        exit(EXIT_FAILURE);
    }
    /* printf("%s" CRESET " ", ftsentry->fts_name); */
    /* printf("\n"); */
}

LOCAL void _handle_file(FTSENT *ftsentry, char *ext)
{
    /* if (debug) */
    /*     log_debug("_handle_file %s, %s\n", ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    _indent(ftsentry->fts_level);
    printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name);

    /* if ((strncmp(ext, ".ml", 3) == 0) */
    /*     && (strlen(ext) == 3)) { */
        /* printf(":%-6s", "ml"); */
        _update_pkg_files(ftsentry, ext);
    /* } */
}

LOCAL void _handle_dune_file(FTSENT *ftsentry)
{
    /* && (ftsentry->fts_namelen = 4) == 0)) { */
    _indent(ftsentry->fts_level);
    printf("%d. " RED "%s ",
           ftsentry->fts_level,
           ftsentry->fts_name);
    printf(CRESET "\n");
}

LOCAL void _handle_opam_file(FTSENT *ftsentry)
{
    /* printf("_handle_opam_file %s\n", ftsentry->fts_name); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    char *ext = strrchr(ftsentry->fts_name, '.');
    _indent(ftsentry->fts_level);

    printf("%d. " MAG  ":%-6s" CRESET " %s\n",
           ftsentry->fts_level,
           "opam",
           ftsentry->fts_name);

    /* _update_ml(ftsentry, ext); */
}

LOCAL void _handle_ocamlformat_file(FTSENT *ftsentry)
{
    _indent(ftsentry->fts_level);

    printf("%d. " BHMAG ":%-6s" CRESET " %s\n",
           ftsentry->fts_level,
           "ocamlformat",
           ftsentry->fts_name);

    /* _update_ml(ftsentry, ext); */
}

LOCAL void _handle_script_file(FTSENT *ftsentry, char *ext)
{
    /* if (debug) */
    /*     log_debug("_handle_script_file %s, %s\n", ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    _indent(ftsentry->fts_level);
    printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name);

    _update_pkg_script_files(ftsentry, ext);
}

LOCAL void _handle_symlink(FTS *tree, FTSENT *ftsentry)
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

    printf("%d. ", ftsentry->fts_level);

    if (strncmp(ftsentry->fts_name, "dune", 4) == 0) {
        printf(RED);
    }

    printf("%s ", ftsentry->fts_name);
    printf(YEL);
    linklen = 0;
    linklen = readlink(ftsentry->fts_path,
                       linkbuf, BUFSZ);
    if (linklen < 0) {
        printf(RED "ERROR on readlink: %s \n",
               strerror(errno));
    } else
        printf("%.*s" CRESET "\n", linklen, linkbuf);
}

/* control traversal order */
int _compare(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

EXPORT s7_pointer g_dune_load(s7_scheme *s7, s7_pointer args)
{
    char *rootdir, *pathdir;

    printf("args: %s\n", s7_object_to_c_string(s7, args));
    if ( s7_is_null(s7, args) ) {
        rootdir = getcwd(NULL, 0);
        pathdir = ".";
    } else {
        s7_int args_ct = s7_list_length(s7, args);
        printf("args ct: %d\n", args_ct);

        s7_pointer rootarg;

        if (args_ct == 2) {
            rootdir = s7_string(s7_car(args));
            pathdir = s7_string(s7_cadr(args));
        }
        else if (args_ct == 1) {
            rootdir = getcwd(NULL,0);
            pathdir = s7_string(s7_car(args));
        }
        else if (args_ct == 0) {
            rootdir = getcwd(NULL,0);
            pathdir = ".";
        }
        else {
            fprintf(stderr,
                    RED "ERROR: unexpected arg count %d for dune-load\n",
                    args_ct);
            exit(EXIT_FAILURE);
        }
        /* rootdir = (char *)s7_object_to_c_string(s7, rootarg); */
        /* rootdir = (char *)s7_string(rootarg); */
        /* printf("s7_car(args): %s\n", rootdir); */
        /* strlcpy(rootdir, s, 256); */
        /* rootdir = "test"; */
    }
    dune_load(rootdir, pathdir);
    printf("g_dune_load done\n");
    printf("cwd: %s\n", getcwd(NULL, 0));
    /* free(rootdir); */
    return s7_name_to_value(s7, "pkg-tbl");
}

EXPORT void dune_load(char *root, char *path)
{
    printf("dune_load root: %s, path: %s\n", root, path);

    errno = 0;
    int rc = chdir(getenv("HOME"));
    if (rc != 0) {
        fprintf(stderr, RED "ERROR chdir(%s): %s", root, strerror(errno));
        exit(EXIT_FAILURE);
    }
    rc = chdir(root);

    printf("cwd: %s\n", getcwd(NULL, 0));

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;
    char *const dir = path;

    /* WARING: fts_open will segfault on macos if the access
       specifiers are not right. first (path) arg is char *const *
    */
    /* FTS * fts_open(char * const *path_argv, int options, int (*compar)(const FTSENT **, const FTSENT **)); */

    errno = 0;
    tree = fts_open(&dir,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &_compare
                    );
    if (errno != 0)
        printf("fts_open %s RC %d: %s\n",
               dir,
               errno, strerror(errno));

    printf("loading...\n");

    char *ext;

    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            if (debug)
                log_debug("ftsentry: %s", ftsentry->fts_name);
            switch (ftsentry->fts_info)
                {
                case FTS_DOT : // not specified to fts_open
                    // do not process children of hidden dirs
                    /* fts_set(tree, ftsentry, FTS_SKIP); */
                    break;
                case FTS_D : // dir visited in pre-order
                    dir_ct++;
                    _handle_dir(tree, ftsentry);
                    break;
                case FTS_DP:
                    /* postorder directory */
                    break;
                case FTS_F : // regular file
                    file_ct++;
                    /* _handle_regular_file(ftsentry); */
                    if (strncmp(ftsentry->fts_name, "dune", 4) == 0) {
                        _handle_dune_file(ftsentry);
                        break;
                    }

                    ext = strrchr(ftsentry->fts_name, '.');

                    if (ext) {
                        if ((strncmp(ext, ".ml", 3) == 0)) {
                            _handle_ml_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".md", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_ml_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".sh", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_script_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".py", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            _handle_script_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".opam", 5) == 0)
                                 && (strlen(ext) == 5)) {
                            _handle_opam_file(ftsentry);
                        }
                        else if (strncmp(ext, ".ocamlformat", 12) == 0) {
                            _handle_ocamlformat_file(ftsentry);
                        }
                        else {
                            _handle_file(ftsentry, ext);
                            /* FTSENT *p = ftsentry->fts_parent; */
                            /* while (p) { */
                            /*     if (strlen(p->fts_name) > 0) */
                            /*         printf("\\%s", p->fts_name); */
                            /*     p = p->fts_parent; */
                            /* } */
                            /* printf("\n"); */
                        }
                    }
                    else {
                        /* no extension */
                        if (strstr(ftsentry->fts_name, "opam")) {
                            _handle_opam_file(ftsentry);
                        }
                        else {
                            _handle_file(ftsentry, ext);
                            /* _indent(ftsentry->fts_level); */
                            /* printf("%d. " CYN "%s" CRESET "\n", */
                            /*        ftsentry->fts_level, */
                            /*        ftsentry->fts_name); */
                        }
                    }
                    break;
                case FTS_SL: // symlink
                    file_ct++;
                    _handle_symlink(tree, ftsentry);
                    break;
                default:
                    log_error(RED "Unhandled FTS type %d\n",
                              ftsentry->fts_info);
                    exit(EXIT_FAILURE);
                    break;
                }
        }
    }
    printf("done\n");
    return;
}
