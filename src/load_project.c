#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fnmatch.h>
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

#include "log.h"
/* #if INTERFACE */
#include "utarray.h"
#include "utstring.h"
#include "s7.h"
/* #endif */


#include "load_project.h"

s7_int gc_deps = -1;
s7_pointer deps_list;

int tct = 0;

char *tostr;

#if defined(DEBUG_TRACE)
bool mibl_debug_traversal = false;
bool mibl_debug_traversal_opam = false;
extern bool mibl_debug_deps;

/* extern char *tostr1; */
/* extern char *tostr2; */
#endif

bool mibl_show_deps = false;
bool mibl_show_traversal = false;


/* bool mibl_emit_parsetree = false; */

UT_array  *segs;
UT_string *group_tag;

extern int dunefile_ct;
extern int file_ct;
extern int dir_ct;

void _indent(int i)
{
    /* printf("_indent: %d\n", i); */
    /* for (; i > 0; i--) */
    /*     printf("    "); */
}

UT_string *dunefile_name;

s7_int dune_gc_loc = -1;

s7_pointer g_dunefile_port;

/* extern s7_pointer s7_read_thunk_catcher; */

void s7_show_stack(s7_scheme *sc);

/* /\* TODO: validate dunefile_port *\/ */
/* /\* s7 defined in s7_config.c *\/ */
/* LOCAL s7_pointer dune_read_thunk(s7_scheme *s7, s7_pointer args) { */
/*     printf("dune_read_thunk: %s\n", */
/*            s7_port_filename(s7, g_dunefile_port)); */
/*     return s7_read(s7, g_dunefile_port); */
/* } */

/* s7_pointer s7_read_thunk; */

/* used by scandir */
/* struct dirent **namelist; */
/* int scan_ct; */
/* LOCAL int _select_ocaml_srcs(const struct dirent *de) { */
/*     /\* log_debug("selecting on %s", de->d_name); *\/ */
/*     char *ext = strrchr(de->d_name, '.'); */
/*     if (ext == NULL) { */
/*         return 0; */
/*     } else if (strncmp(ext, ".mli", 4) == 0) { */
/*         return 1; */
/*     } */
/*     else if (strncmp(ext, ".ml", 3) == 0) { */
/*         return 1; */
/*     } else */
/*         return 0; */
/* } */

bool _is_ws_root(FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace("_is_ws_root: %s", ftsentry->fts_path);
#endif

    UT_string *pathdir;
    utstring_new(pathdir);
    utstring_printf(pathdir, "%s", ftsentry->fts_path);
    utstring_printf(pathdir, "%s", "/WORKSPACE.bazel");
    /* log_trace("accessing %s", utstring_body(pathdir)); */
    int rc = access(utstring_body(pathdir), R_OK);
    /* log_debug("RC: %d", rc); */
    if (!rc) {
#if defined(DEBUG_TRACE)
        if (mibl_trace) log_trace("true");
#endif
        return true;
    } else {
        utstring_new(pathdir);
        utstring_printf(pathdir, "%s", ftsentry->fts_path);
        utstring_printf(pathdir, "%s", "/WORKSPACE");
        rc = access(utstring_body(pathdir), R_OK);
        if (!rc) {
#if defined(DEBUG_TRACE)
            if (mibl_trace) log_trace("true");
#endif
            return true;
        }
    }
#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace("false");
#endif
    return false;
}

LOCAL bool _this_is_hidden(FTSENT *ftsentry)
{
    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            /* if (mibl_trace) */
            /*     log_trace(RED "Excluding" CRESET " hidden dir: %s\n", */
            /*               ftsentry->fts_path); //, ftsentry->fts_name); */
            return true;
            /* } else { */
            /*     printf("ROOT DOT dir\n"); */
        }
    }
    return false;
}

LOCAL s7_pointer make_pkg_key(char *path)
{
    if (path[0] == '.' && path[1] == '\0') {
        /* return s7_make_keyword(s7, ":wsroot"); */
        return s7_make_string(s7, "./");
    } else {
        if (path[0] == '.' && path[1] == '/') {
            /* return s7_make_keyword(s7, ":wsroot"); */
            return s7_make_string(s7, path+2);
        } else {
            return s7_make_string(s7, path);
        }
    }
}

/*
  create a pkg-tbl entry if dir contains a dune file or at least one
  OCaml source file.
 */
LOCAL void _handle_dir(s7_pointer pkg_tbl, FTS* tree, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug("");
        log_debug(BLUB "_handle_dir:" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
        log_info("%-20s%s", "base ws:", rootws);
        log_info("%-20s%s", "effective ws:",ews_root);
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }
#endif
    UT_string *dune_test;
    utstring_new(dune_test);
    utstring_printf(dune_test, "%s/%s", ftsentry->fts_path, "dune");
    /* log_info("DUNEFILE: %s", utstring_body(dune_test)); */
    /* fflush(stdout); */
    /* fflush(stderr); */

    int rc = access(utstring_body(dune_test), F_OK);
    if (rc) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_warn(RED "dunefile not found in: " CRESET "%s",
                      ftsentry->fts_path);
#endif
        /* return; */
    }

    char *fts_realpath = realpath(ftsentry->fts_path, NULL);
    if (strncmp(ews_root, fts_realpath, strlen(ews_root)) != 0) {
    /* if (strncmp(ews_root, ftsentry->fts_path, */
    /*              strlen(ftsentry->fts_path)) != 0) { */
        /* is this a ws root (other than base ws)? */
        if (_is_ws_root(ftsentry)) {
            log_warn("SKIPPING ws root: %s", ftsentry->fts_path);
            log_warn("ews root: %s", ews_root);
            /* we do not process embedded subrepos yet */
            fts_set(tree, ftsentry, FTS_SKIP);
            //FIXME: when we encounter an embedded ws, we need to
            //traverse it independently.
            return;
        }
    }
    free(fts_realpath);

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

    /* drop leading "./" */
    s7_pointer pkg_key = make_pkg_key(ftsentry->fts_path);

/* #if defined(DEBUG_TRACE) */
/*         log_trace(RED "pkg_key" CRESET, pkg_key); */
/*     } */
/* #endif */

    /* s7_pointer test_assoc = s7_list(s7, 2, */
    /*                                 s7_make_keyword(s7, "test"), */
    /*                                 s7_make_symbol(s7, "dummy")); */
    /* s7_pointer result = s7_hash_table_set(s7, pkg_tbl, pkg_key, */
    /*                                       s7_list(s7, 1, test_assoc)); */

    /* MB: result of realpath must be freed */
    char *rpath = realpath(ftsentry->fts_path, NULL);
    /* log_trace(RED "realpath %s" CRESET, rpath); */
    /* FIXME: check result */
    /* s7_pointer result = */

    s7_hash_table_set(s7, pkg_tbl, pkg_key,
                      s7_list(s7, 3,
                              //FIXME: use a ws-alist instead of
                              //annotating each pkg with :ws-path
                              s7_cons(s7, ws_path_kw,
                                      s7_make_string(s7, ews_root)),
                              s7_cons(s7, pkg_path_kw,
                                      pkg_key),
                              s7_cons(s7, realpath_kw,
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

LOCAL __attribute__((unused)) char *_principal_name(FTSENT *ftsentry, char *ext)
{
    strlcpy(principal, ftsentry->fts_name, 256);
    principal[ext - ftsentry->fts_name] = '\0';
    /* principal[0] = toupper(principal[0]); */
    return (char *)principal;
}

/* static s7_pointer alist_updt_in_fn(s7_scheme *s, s7_pointer args) */
/* { */
/*     log_debug("running updater"); */
/*     return(s7_make_string(s7, "HELLO")); */
/* } */

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
#define TAG_CPPO 6
#define TAG_MLLIB 7

/* char *_get_extension(char *filename) */
/* { */
/*     char *pt = strrchr(filename, '.'); */
/*     return pt; */
/* } */

LOCAL void _update_pkg_files(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_update_pkg_files: %s, ext: %s",
                  ftsentry->fts_name, ext);
#endif

    if (ftsentry->fts_name[0] == '#') {
        // skip e.g. #BUILD.bazel#
        return;
    }

    if (ftsentry->fts_name[strlen(ftsentry->fts_name) - 1] == '~') {
        // skip backup files
        return;
    }


    if (_exclusions(ftsentry, ext)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_warn("excluding %s", ftsentry->fts_name);
#endif
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */

    char *pkg_name = dirname(ftsentry->fts_path);

    /* s7_pointer pkg_key = s7_make_string(s7, pkg_name); */
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no entry for this pkg: %s", pkg_name);
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {
        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, files_kw, static_kw);
        s7_pointer files_assoc = s7_call(s7, assoc_in,
                                         s7_list(s7, 2,
                                                 keypath,
                                                 pkg_alist));
/* #if defined(DEBUG_TRACE) */
/*         if (mibl_debug_traversal) */
/*             LOG_S7_DEBUG("files_assoc", files_assoc); */
/* #endif */

        if (files_assoc == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("initializing (:files (:static (:type . fname))) list");
#endif

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
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("file_list", file_list);
            }
#endif

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            s7_gc_unprotect_via_stack(s7, pkg_alist);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("new pkg", file_list);
            }
#endif

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);
            s7_gc_unprotect_via_stack(s7, pkg_key);

        } else {
            /* assoc-in '(:files :static) returns assoc (:static ...),
               but we need the alist */
            s7_pointer files_alist = s7_cdr(files_assoc);

/* #if defined(DEBUG_TRACE) */
/*             if (mibl_debug_traversal) { */
/*                 log_debug("updating :files"); */
/*                 LOG_S7_DEBUG("files_alist", files_alist); */
/*                 log_debug("files_alist is pair?: %d", */
/*                           (s7_is_pair(files_alist)? 1 : 0)); */
/*                 log_debug("files_alist is list?: %d", */
/*                           (s7_is_list(s7, files_alist)? 1 : 0)); */
/*                 log_debug("files_alist length: %d", */
/*                           s7_list_length(s7, files_alist)); */
/*             } */
/* #endif */

            /* s7_pointer file_pair = */
            /*     s7_list(s7, 2, */
            /*             s7_make_keyword(s7, file_ext), */
            /*             s7_make_string(s7, ftsentry->fts_name)); */
            /* LOG_S7_DEBUG("new file_pair", file_pair); */

            s7_pointer addition = s7_list(s7, 1,
                                          s7_make_string(s7, ftsentry->fts_name));
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("addition", addition);
            }
#endif
            s7_pointer new_files_alist =
                s7_append(s7,
                /* s7_list(s7, 2, */
                /* s7_cons(s7, */
                          files_alist,
                          addition
                        /* file_pair, */
                          );
/* #if defined(DEBUG_TRACE) */
/*             if (mibl_debug_traversal) */
/*                 LOG_S7_DEBUG("new files_alist", new_files_alist); */
/* #endif */

            /* s7_pointer sort      = _load_sort(); */
            /* s7_pointer string_lt = _load_string_lt(); */
            /* s7_pointer sorted */
            /*     = s7_call(s7, sort, s7_list(s7, 2, */
            /*                                 new_files_alist, */
            /*                                 string_lt)); */

            /* LOG_S7_DEBUG("new files_alist sorted", sorted); */

            s7_set_cdr(files_assoc, new_files_alist);
            /* LOG_S7_DEBUG("files_assoc", files_assoc); */

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */

        }
    }
}

LOCAL void _update_cc_src_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_update_cc_src_file: %s, ext: %s",
                  ftsentry->fts_name, ext);
#endif

    if (_exclusions(ftsentry, ext)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_warn("excluding %s", ftsentry->fts_name);
#endif
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */

    char *pkg_name = dirname(ftsentry->fts_path);

    /* s7_pointer pkg_key = s7_make_string(s7, pkg_name); */
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no entry for this pkg: %s", pkg_name);
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {
        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, cc_kw, cc_srcs_kw); // , static_kw);
        s7_pointer cc_assoc = s7_call(s7, assoc_in,
                                         s7_list(s7, 2,
                                                 keypath,
                                                 pkg_alist));
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("cc_assoc", cc_assoc);
        }
#endif

        if (cc_assoc == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("initializing (:cc (:srcs fname))) list");
#endif

            /* s7_pointer cc_pair = */
            /*     s7_list(s7, 1, */
            /*             /\* s7_make_keyword(s7, cc_ext), *\/ */
            /*             s7_make_string(s7, ftsentry->fts_name)); */

            s7_pointer static_assoc = s7_list(s7, 2,
                                              cc_srcs_kw, // static_kw,
                                              s7_make_string(s7, ftsentry->fts_name));
                                              /* cc_pair); */

            s7_pointer cc_list =
                s7_list(s7, 1, s7_list(s7, 2,
                                       cc_kw, //cc_srcs_kw,
                                       static_assoc
                                       ));
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("cc_list", cc_list);
            }
#endif

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           cc_list);
            s7_gc_unprotect_via_stack(s7, pkg_alist);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("new pkg", new_pkg);
            }
#endif

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);
            s7_gc_unprotect_via_stack(s7, pkg_key);

        } else {
            /* assoc-in '(:cc :static) returns assoc (:static ...),
               but we need the alist */
            s7_pointer cc_alist = s7_cdr(cc_assoc);

            /* if (mibl_debug_traversal) { */
            /*     log_debug("updating :cc"); */
            /*     LOG_S7_DEBUG("cc_alist", cc_alist); */
            /* } */

            /* s7_pointer cc_pair = */
            /*     s7_list(s7, 2, */
            /*             s7_make_keyword(s7, cc_ext), */
            /*             s7_make_string(s7, ftsentry->fts_name)); */
            /* LOG_S7_DEBUG("new cc_pair", cc_pair); */

            s7_pointer new_cc_alist =
                s7_append(s7,
                /* s7_list(s7, 2, */
                /* s7_cons(s7, */
                          cc_alist,
                          s7_list(s7, 1,
                                  s7_make_string(s7, ftsentry->fts_name))
                        /* cc_pair, */
                          );
            /* LOG_S7_DEBUG("new cc_alist", new_cc_alist); */

            /* s7_pointer sort      = _load_sort(); */
            /* s7_pointer string_lt = _load_string_lt(); */
            /* s7_pointer sorted */
            /*     = s7_call(s7, sort, s7_list(s7, 2, */
            /*                                 new_cc_alist, */
            /*                                 string_lt)); */

            /* LOG_S7_DEBUG("new cc_alist sorted", sorted); */

            s7_set_cdr(cc_assoc, new_cc_alist);
            /* LOG_S7_DEBUG("cc_assoc", cc_assoc); */

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */

        }
    }
}

LOCAL void _update_cc_hdr_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_update_cc_hdr_file: %s, ext: %s",
                  ftsentry->fts_name, ext);
#endif

    if (_exclusions(ftsentry, ext)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_warn("excluding %s", ftsentry->fts_name);
#endif
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    /* char *cc_ext =  strrchr(ftsentry->fts_name, '.'); */
    /* cc_ext++; // exclude dot */

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no entry for this pkg: %s", pkg_name);
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {
        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, cc_kw, cc_hdrs_kw); //, static_kw);
        s7_pointer cc_assoc = s7_call(s7, assoc_in,
                                         s7_list(s7, 2,
                                                 keypath,
                                                 pkg_alist));
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("cc_assoc (hdrs)", cc_assoc);
        }
#endif

        if (cc_assoc == s7_f(s7)) {
            /* (:cc (:hdrs )) not found */

            s7_pointer cc_hdrs_assoc = s7_list(s7, 2,
                                               cc_hdrs_kw,
                                               s7_make_string(s7, ftsentry->fts_name));
                /* cc_pair); */

/* #if defined(DEBUG_TRACE) */
/*             if (mibl_debug_traversal) */
/*                 log_debug("initializing (:cc (:hdrs fname)) list"); */
/* #endif */
            /* no (:cc (:hdrs )); get (:cc ) */
            keypath = s7_list(s7, 1, cc_kw); //, cc_hdrs_kw); //, static_kw);
            cc_assoc = s7_call(s7, assoc_in,
                               s7_list(s7, 2,
                                       keypath,
                                       pkg_alist));
            if (cc_assoc == s7_f(s7)) {
                /* (:cc ) not found */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    log_debug("initializing (:cc )) list");
#endif

                s7_pointer cc_list =
                    s7_list(s7, 1, s7_list(s7, 2,
                                           cc_kw, // cc_hdrs_kw,
                                           cc_hdrs_assoc
                                           ));
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("cc_list", cc_list);
                    LOG_S7_DEBUG("new cc_hdrs_assoc", cc_hdrs_assoc);
                }
#endif

                s7_pointer new_pkg = s7_append(s7,
                                               pkg_alist,
                                               cc_list);
                s7_gc_unprotect_via_stack(s7, pkg_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new pkg", new_pkg);
                }
#endif

                s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);
                s7_gc_unprotect_via_stack(s7, pkg_key);

            } else {
                /* (:cc ) found w/o :hdrs; add (:hdrs ) */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("found cc_assoc", cc_assoc);
                    LOG_S7_DEBUG("adding cc_hdrs_assoc", cc_hdrs_assoc);
                }
#endif
                s7_pointer cc_cdr = s7_cdr(cc_assoc);
                (void)cc_cdr;
                s7_pointer new_cc_cdr =
                    s7_append(s7,
                              cc_cdr,
                              s7_list(s7, 1, cc_hdrs_assoc)
                              );
#if defined(DEBUG_TRACE)
                LOG_S7_DEBUG("new_cc_cdr", new_cc_cdr);
#endif
                s7_set_cdr(cc_assoc, new_cc_cdr);
            }
        } else {
            /* found (:cc (:hdrs ...)); augment it */
            /* assoc-in '(:cc :static) returns assoc (:static ...),
               but we need the alist */
            s7_pointer cc_alist = s7_cdr(cc_assoc);

            /* if (mibl_debug_traversal) { */
            /*     log_debug("updating :cc"); */
            /*     LOG_S7_DEBUG("cc_alist", cc_alist); */
            /* } */

            /* s7_pointer cc_pair = */
            /*     s7_list(s7, 2, */
            /*             s7_make_keyword(s7, cc_ext), */
            /*             s7_make_string(s7, ftsentry->fts_name)); */
            /* LOG_S7_DEBUG("new cc_pair", cc_pair); */

            s7_pointer new_cc_alist =
                s7_append(s7,
                /* s7_list(s7, 2, */
                /* s7_cons(s7, */
                          cc_alist,
                          s7_list(s7, 1,
                                  s7_make_string(s7, ftsentry->fts_name))
                        /* cc_pair, */
                          );
            /* LOG_S7_DEBUG("new cc_alist", new_cc_alist); */

            /* s7_pointer sort      = _load_sort(); */
            /* s7_pointer string_lt = _load_string_lt(); */
            /* s7_pointer sorted */
            /*     = s7_call(s7, sort, s7_list(s7, 2, */
            /*                                 new_cc_alist, */
            /*                                 string_lt)); */

            /* LOG_S7_DEBUG("new cc_alist sorted", sorted); */

            s7_set_cdr(cc_assoc, new_cc_alist);
            /* LOG_S7_DEBUG("cc_assoc", cc_assoc); */

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */

        }
    }
}

LOCAL void _update_pkg_script_files(s7_pointer pkg_tbl,
                                    FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_update_pkg_script_files: %s, ext: %s",
                  ftsentry->fts_name, ext);
#endif

    if (_exclusions(ftsentry, ext)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_warn("excluding %s", ftsentry->fts_name);
#endif
        return;
    }

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);

    //s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_unprotect_via_stack(s7, pkg_alist);

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no entry for this pkg");
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {
        s7_pointer assoc = _load_assoc();

        s7_pointer scripts_list
            = s7_call(s7, assoc, s7_list(s7, 2, scripts_kw, pkg_alist));
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("scripts_list", scripts_list);
        }
#endif

        if (scripts_list == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("adding :scripts");
#endif

            s7_pointer file_list =
                s7_list(s7, 1,
                        s7_list(s7, 2,
                                scripts_kw,
                                s7_make_string(s7, ftsentry->fts_name)));
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("file_list", file_list);
            }
#endif
            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            s7_gc_unprotect_via_stack(s7, pkg_alist);

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("new pkg", new_pkg);
            }
#endif
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);
            s7_gc_unprotect_via_stack(s7, pkg_key);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("updating :scripts");
                /* LOG_S7_DEBUG("srcs_alist", srcs_alist); */
                LOG_S7_DEBUG("scripts_list", scripts_list);
            }
#endif
            s7_pointer new_scripts_list =
                s7_cons(s7,
                        s7_make_string(s7, ftsentry->fts_name),
                        s7_cdr(scripts_list));
#if defined(DEBUG_TRACE)
            LOG_S7_DEBUG("new scripts_list", new_scripts_list);
#endif

            s7_pointer sort = s7_name_to_value(s7, "sort!");
            s7_pointer lt = s7_name_to_value(s7, "string<?");
            s7_pointer sorted
                = s7_call(s7, sort, s7_list(s7, 2,
                                            new_scripts_list,
                                            lt));

            /* LOG_S7_DEBUG("new scripts_list sorted", sorted); */

            s7_set_cdr(scripts_list, sorted);
#if defined(DEBUG_TRACE)
            LOG_S7_DEBUG("scripts_list", scripts_list);
#endif

            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */
        }
    }
}

/* LOCAL __attribute__((unused)) void _update_pkg_deps(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext) */
/* { */
/*     char *pkg_name = dirname(ftsentry->fts_path); */
/*     /\* char *fname = ftsentry->fts_name; *\/ */
/*     char *mname = _module_name(ftsentry, ext); */
/*     if (verbose) { */
/*         log_info(BLU "_update_pkg_deps:" CRESET " %s; ", mname); */
/*         log_info("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name); */
/*     } */

/*     /\* char *ml_name = strdup(ftsentry->fts_name); *\/ */

/*     s7_pointer pkg_key = make_pkg_key(pkg_name); */
/*     s7_gc_protect_via_stack(s7, pkg_key); */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_debug_traversal) { */
/*         log_debug("pkg_key", pkg_key); */
/*     } */
/* #endif */

/*     s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key); */
/*     s7_gc_protect_via_stack(s7, pkg_alist); */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_debug_traversal) { */
/*         log_debug("pkg_alist", pkg_alist); */
/*     } */
/* #endif */

/* } */

LOCAL void _update_pkg_modules(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("_update_pkg_modules");
    }
#endif
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        log_debug("fname: %s", fname);
        log_debug("tag: %d", ftype);
        /* log_debug("pkg_tbl", pkg_tbl); */
    }
#endif

    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);
    //s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        LOG_S7_DEBUG("pkg_key", pkg_key);
    }
#endif
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        LOG_S7_DEBUG("pkg_alist", pkg_alist);
    }
#endif

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
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
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("modules_alist", modules_alist);
        }
#endif

        /* get_deps returns mdeps protected by gc_module_deps */
        s7_pointer mdeps;
        if (ftype == TAG_ML)
            mdeps = get_deps(pkg_name, fname); // , deps_list);
        else if (ftype == TAG_MLI)
            mdeps = get_deps(pkg_name, fname); // , deps_list);
        else {
            mdeps = s7_nil(s7);
            gc_module_deps = s7_gc_protect(s7, mdeps);
        }
#if defined(DEBUG_TRACE)
        if (mibl_debug_deps) {
            log_debug("fname: %s", fname);
            LOG_S7_DEBUG("MDEPS", mdeps);
        }
#endif
        /* fprintf(stderr, "s7_gc_unprotect_at gc_deps_list: %ld\n", (long)gc_deps_list); */
        /* s7_gc_unprotect_at(s7, gc_deps_list); */

        s7_pointer ml_assoc;
        if (ftype == TAG_MLY) {
            ml_assoc = s7_cons(s7,
                               s7_make_keyword(s7, "mly"),
                               s7_make_symbol(s7, fname));
        }
        else if (ftype == TAG_MLL) {
            ml_assoc = s7_cons(s7,
                               s7_make_keyword(s7, "mll"),
                               s7_make_symbol(s7, fname));
        } else {
            if (mdeps == s7_nil(s7)) {
                ml_assoc = s7_cons(s7,
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
                                                   :"UNKNOWN"
                                                   ),
                                   s7_make_symbol(s7, fname));
            } else {
                ml_assoc = s7_cons(s7,
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
                                                   :"UNKNOWN"
                                                   ),
                                   s7_cons(s7,
                                           s7_make_symbol(s7, fname),
                                           mdeps));
            }
        }
        /* mdeps is now protected by inclusion in ml_assoc, so unprotect it*/
        s7_gc_unprotect_at(s7, gc_module_deps);

        /* s7_pointer ml_deps_assoc = s7_cons(s7, deps_kw, */
        /*                                    s7_make_symbol(s7, "Foo")); */

#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("ml_assoc", ml_assoc);
            /* LOG_S7_DEBUG("ml_deps_assoc", ml_deps_assoc); */
        }
#endif

        /* if (srcs_alist == s7_f(s7)) { */
        if (modules_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug(GRNB "INITIALIZING " CRESET ":modules field");
#endif
            /* (acons :srcs */
            /*  ((:modules ((:Foo ((:ml foo.ml) (:mli foo.mli)) */
            /*              (:Bar ((:ml bar.ml) (:mli bar.mli))))))) */
            /*  pkg_alist) */
            /* s7_pointer msrcs_alist = s7_list(s7, 1, ml_assoc); */
            /* LOG_S7_DEBUG("msrcs_alist", msrcs_alist); */

            s7_pointer module_assoc = s7_list(s7, 2,
                                              mname_sym,
                                              ml_assoc);
                                              /* ml_deps_assoc); */
            /* msrcs_alist); */
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("module_assoc", module_assoc);
            }
#endif
            /* s7_pointer modules_alist = s7_list(s7, 1, module_assoc); */
            /* log_debug("modules_alist", modules_alist); */

            /* s7_pointer statics_assoc = */
            /*     s7_list(s7, 2, */
            /*             static_kw, */
            /*             module_assoc); */


            s7_pointer modules_assoc = s7_list(s7, 2,
                                               modules_kw,
                                               // statics_assoc
                                               module_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("modules_assoc", modules_assoc);
            }
#endif
            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         modules_assoc));
            s7_gc_unprotect_via_stack(s7, pkg_alist);

            /* if (mibl_debug_traversal) */
            /*     LOG_S7_DEBUG("pkg_alist", new_pkg_alist); */

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
            s7_gc_unprotect_via_stack(s7, pkg_key);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :modules");
                LOG_S7_DEBUG("modules_alist", modules_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            /* s7_pointer assoc_in = _load_assoc_in(); */

            s7_pointer keypath = s7_list(s7, 2, //  3,
                                         /* srcs_kw, */
                                         modules_kw,
                                         /* static_kw, */
                                         mname_sym);
            s7_gc_protect_via_stack(s7, keypath);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                LOG_S7_DEBUG("pkg_alist", pkg_alist);
            }
#endif

            s7_pointer module_alist = s7_call(s7, assoc_in,
                                              s7_list(s7, 2,
                                                      keypath,
                                                      pkg_alist));
            s7_gc_protect_via_stack(s7, module_alist);
            s7_gc_unprotect_via_stack(s7, keypath);

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("module_alist", module_alist);
            }
#endif
            if (module_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("module_alist", modules_alist);
                }
#endif
                /* s7_pointer keypath = s7_list(s7, 1, modules_kw); */
                /* if (mibl_debug_traversal) LOG_S7_DEBUG("trying keypath", keypath); */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("modules_alist", modules_alist);
#endif

                s7_pointer modules_alist_cdr = s7_cdr(modules_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("modules_alist_cdr", modules_alist_cdr);
                }
#endif

                s7_pointer module_assoc =
                    s7_list(s7, 1, s7_list(s7, 2, mname_sym, ml_assoc));
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new module_assoc", module_assoc);
                }
#endif
                s7_pointer new_modules_alist_cdr =
                    s7_append(s7, modules_alist_cdr, module_assoc);
                    /* s7_cons(s7, module_assoc, modules_alist_cdr); */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_modules_alist_cdr", new_modules_alist_cdr);
                }
#endif

                s7_pointer new_modules_alist
                    = s7_set_cdr(modules_alist, new_modules_alist_cdr);
                (void)new_modules_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_modules_alist", new_modules_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(GRNB "UPDATING" CRESET);
                    LOG_S7_DEBUG("module_alist", module_alist);
                }
#endif
                s7_pointer modules_alist_cdr = s7_cdr(module_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("modules_alist_cdr", modules_alist_cdr);
                }
#endif

                /* s7_pointer msrcs = s7_cons(s7, ml_assoc, modules_alist_cdr); */
                s7_pointer msrcs = s7_append(s7,
                                             modules_alist_cdr,
                                             s7_list(s7, 1, ml_assoc));//, ml_deps_assoc));

                /* if (mibl_debug_traversal) { */
                /*     LOG_S7_DEBUG("setting cdr to", msrcs); */
                /* } */

                s7_pointer new_modules_alist
                    = s7_set_cdr(module_alist, msrcs);
                (void)new_modules_alist;
#if defined(DEBUG_TRACE)
               if (mibl_debug_traversal) {
                   LOG_S7_DEBUG("new_modules_alist", new_modules_alist);
                   LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_pkg_sigs(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_pkg_sigs" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        /* LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */
    }
#endif

    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        LOG_S7_DEBUG("pkg_key", pkg_key);
    }
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);
        s7_gc_protect_via_stack(s7, mname_sym);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, sigs_kw, static_kw);
        /* s7_gc_protect_via_stack(s7, pkg_keypath); */

        s7_pointer sigs_alist = s7_call(s7, assoc_in,
                                        s7_list(s7, 2,
                                                keypath,
                                                pkg_alist));
        s7_gc_protect_via_stack(s7, sigs_alist);

        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("sigs_alist", sigs_alist);
        }
#endif

        s7_pointer mli_file = s7_make_symbol(s7, fname);
        /* s7_pointer mli_assoc = s7_list(s7, 2, */
        /*                               s7_make_keyword(s7, "mli"), */
        /*                               s7_make_string(s7, fname)); */
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("mli_assoc", mli_assoc); */

        /* s7_pointer sig_assoc = s7_list(s7, 2, mname_sym, mli_file); */

        s7_pointer mdeps; /* gc protected by gc_module_deps */
        mdeps = get_deps(pkg_name, fname); // , deps_list);

        s7_pointer sig_assoc;
        if (mdeps == s7_nil(s7))
            sig_assoc = s7_cons(s7, mname_sym, mli_file);
        else
            sig_assoc = s7_cons(s7, mname_sym,
                                           s7_cons(s7,
                                                   mli_file,
                                                   mdeps));

        s7_gc_protect_via_stack(s7, sig_assoc);
        s7_gc_unprotect_via_stack(s7, mname_sym);

        /* mdeps is now protected by inclusion in sig_assoc, so unprotect it*/
        s7_gc_unprotect_at(s7, gc_module_deps);

#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("sig_assoc", sig_assoc);
        }
#endif

        if (sigs_alist == s7_f(s7)) {
            s7_gc_unprotect_via_stack(s7, sigs_alist);

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("INITIALIZING :signatures field");
#endif
            /* (:signatures ((:Foo (:mli foo.mli)) */
            /*               (:Bar (:mli bar.mli)))) */

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, sig_assoc);
            s7_gc_unprotect_via_stack(s7, sig_assoc);

            s7_pointer sigs_assoc = s7_list(s7, 2,
                                            sigs_kw, statics_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("sigs_assoc", sigs_assoc);
            }
#endif

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         sigs_assoc));
            s7_gc_unprotect_via_stack(s7, pkg_alist);

            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
            s7_gc_unprotect_via_stack(s7, pkg_key);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :signatures");
                LOG_S7_DEBUG("sigs_alist", sigs_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         sigs_kw,
                                         static_kw,
                                         mname_sym);
            s7_gc_protect_via_stack(s7, keypath);
            s7_gc_unprotect_via_stack(s7, mname_sym);

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                /* LOG_S7_DEBUG("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer sig_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));
            s7_gc_protect_via_stack(s7, sig_alist);
            s7_gc_unprotect_via_stack(s7, keypath);

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("sig_alist", sig_alist);
            }
#endif
            if (sig_alist == s7_f(s7)) {
                s7_gc_unprotect_via_stack(s7, sig_alist);

                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("sigs_alist", sigs_alist);
                }
#endif

                s7_pointer sigs_alist_cdr = s7_cdr(sigs_alist);

#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("sigs_alist_cdr", sigs_alist_cdr);
                }
#endif

                /* s7_pointer sig_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mli_file)); //mli_assoc)); */
                /* if (mibl_debug_traversal) LOG_S7_DEBUG("new sig_assoc", sig_assoc); */

                s7_pointer new_sigs_alist_cdr =
                    s7_append(s7, sigs_alist_cdr,
                              s7_list(s7, 1, sig_assoc));
                s7_gc_unprotect_via_stack(s7, sig_assoc);

#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("new_sigs_alist_cdr", new_sigs_alist_cdr);
#endif

                s7_pointer new_sigs_alist
                    = s7_set_cdr(sigs_alist, new_sigs_alist_cdr);
                s7_gc_unprotect_via_stack(s7, sigs_alist);
                (void)new_sigs_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_sigs_alist", new_sigs_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG(RED "UPDATING" CRESET " sig_alist", sig_alist);
#endif

                s7_pointer sigs_alist_cdr = s7_cdr(sig_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("sigs_alist_cdr", sigs_alist_cdr);
#endif

                s7_pointer msrcs = s7_append(s7,
                                             sigs_alist_cdr,
                                             s7_list(s7, 1, mli_file)); //mli_assoc));
                s7_pointer new_sigs_alist
                    = s7_set_cdr(sig_alist, msrcs);
                s7_gc_unprotect_via_stack(s7, sig_alist);

                (void)new_sigs_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_sigs_alist", new_sigs_alist);
                    LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_pkg_structs(s7_pointer pkg_tbl,
                               char *pkg_name, char *mname,
                               char *fname, int ftype)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(BLU "_update_pkg_structs" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name", pkg_name);
        /* LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */
    }
#endif
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);
    // s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
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
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            LOG_S7_DEBUG("structs_alist keypath", keypath);
            LOG_S7_DEBUG("structs_alist", structs_alist);
        }
#endif

        /* s7_pointer ml_assoc = s7_list(s7, 2, */
        /*                               s7_make_keyword(s7, "ml"), */
        /*                               s7_make_string(s7, fname)); */
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("ml_assoc", ml_assoc); */

    /* LOG_S7_DEBUG("pkg_key 2", pkg_key); */
        s7_pointer mdeps;
        mdeps = get_deps(pkg_name, fname); // , deps_list);
        /* gc_mdeps = gc_protect(s7, mdeps); */
        /* fprintf(stderr, "s7_gc_unprotect_at gc_deps_list 3: %ld\n", (long)gc_deps_list); */
        /* s7_gc_unprotect_at(s7, gc_deps_list); */
    /* LOG_S7_DEBUG("pkg_key 3", pkg_key); */

        s7_pointer struct_assoc;
        if (mdeps == s7_nil(s7)) {
            struct_assoc = s7_cons(s7, mname_sym, s7_make_symbol(s7, fname));
        } else {
            struct_assoc = s7_cons(s7, mname_sym,
                                   s7_cons(s7,
                                           s7_make_symbol(s7, fname),
                                           mdeps));
        }
        s7_gc_protect_via_stack(s7, struct_assoc);

                    /* s7_make_symbol(s7, fname)); */
        /* mdeps is now protected by inclusion in ml_assoc, so unprotect it*/
        s7_gc_unprotect_at(s7, gc_module_deps);

        /* s7_list(s7, 2, mname_sym, s7_make_symbol(s7, fname)); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("struct_assoc", struct_assoc);
#endif

        if (structs_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("pkg_key", pkg_key);
                LOG_S7_DEBUG("INITIALIZING keypath", keypath);
            }
#endif
            /* (:structures (:static (:Foo (:ml foo.ml)) */
            /*               (:dynamic (:ml bar.ml)))) */

            s7_pointer structures_alist = s7_call(s7, assoc,
                                                  s7_list(s7, 2,
                                                          structs_kw,
                                                          pkg_alist));
            /* LOG_S7_DEBUG("STRUCTURES alist", structures_alist); */
            if (structures_alist == s7_f(s7)) {
                /* log_debug("NEW :structures list"); */
                s7_pointer statics_assoc =
                    s7_list(s7, 2,
                            // static_kw,
                            (ftype == TAG_ML) ?static_kw :dynamic_kw,
                            struct_assoc);
                s7_gc_unprotect_via_stack(s7, struct_assoc);

                s7_pointer structs_assoc = s7_list(s7, 2,
                                                   structs_kw, statics_assoc);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) LOG_S7_DEBUG("structs_assoc", structs_assoc);
#endif

                s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                     s7_list(s7, 1,
                                                             structs_assoc));
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("pkg_key", pkg_key);
                    LOG_S7_DEBUG("updated pkg_alist", new_pkg_alist);
                    LOG_S7_DEBUG("pkg_key", pkg_key);
                }
#endif
                s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
                s7_gc_unprotect_via_stack(s7, pkg_key);
            } else {
                log_debug("OLD");

                s7_pointer structures_alist_cdr = s7_cdr(structures_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("structures_alist_cdr", structures_alist_cdr);
                }
#endif

                s7_pointer new_struct_assoc = s7_list(s7, 2,
                                                      // static_kw,
                                                      (ftype == TAG_ML) ?static_kw :dynamic_kw,
                                                      struct_assoc);

                s7_pointer new_structures_alist_cdr =
                    s7_append(s7, structures_alist_cdr,
                              s7_list(s7, 1, new_struct_assoc));
                (void)new_structures_alist_cdr;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("new_structures_alist_cdr", new_structures_alist_cdr);
#endif

                s7_pointer new_structures_alist
                    = s7_set_cdr(structures_alist, new_structures_alist_cdr);
                (void)new_structures_alist;
            }
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :structures");
                LOG_S7_DEBUG("structs_alist", structs_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         structs_kw,
                                         static_kw,
                                         mname_sym);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                /* LOG_S7_DEBUG("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer struct_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("struct_alist", struct_alist);
            }
#endif
            if (struct_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("structs_alist", structs_alist);
                }
#endif

                s7_pointer structs_alist_cdr = s7_cdr(structs_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("structs_alist_cdr", structs_alist_cdr);
                }
#endif

                /* s7_pointer struct_assoc = */
                /*     s7_list(s7, 1, */
                /*             s7_list(s7, 2, */
                /*                     mname_sym, s7_make_symbol(s7, fname))); */
                /* if (mibl_debug_traversal) LOG_S7_DEBUG("new struct_assoc", struct_assoc); */

                s7_pointer new_structs_alist_cdr =
                    s7_append(s7, structs_alist_cdr,
                              s7_list(s7, 1, struct_assoc));
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("new_structs_alist_cdr", new_structs_alist_cdr);
#endif

                s7_pointer new_structs_alist
                    = s7_set_cdr(structs_alist, new_structs_alist_cdr);
                (void)new_structs_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_structs_alist", new_structs_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "UPDATING" CRESET);
                    LOG_S7_DEBUG(" struct_alist", struct_alist);
                }
#endif

                s7_pointer structs_alist_cdr = s7_cdr(struct_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("structs_alist_cdr", structs_alist_cdr);
#endif

                s7_pointer msrcs =
                    s7_append(s7, structs_alist_cdr,
                              s7_list(s7, 1, s7_make_symbol(s7, fname))); //ml_assoc));
                s7_pointer new_structs_alist
                    = s7_set_cdr(struct_alist, msrcs);
                (void)new_structs_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_structs_alist", new_structs_alist);
                    LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_pkg_mll_files(s7_pointer pkg_tbl,
                                 char *pkg_name, char *mname,
                                 char *fname, int ftype)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_pkg_mll_files" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        /* LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */
    }
#endif
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    //s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
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
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("ocamllex_alist", ocamllex_alist);
#endif

        s7_pointer mll_file = s7_make_symbol(s7, fname);

        /* s7_pointer mll_assoc = s7_list(s7, 2, mname_sym, mll_file); */
        s7_pointer mll_assoc = s7_cons(s7, mname_sym, mll_file);
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("mll_assoc", mll_assoc);
#endif

        if (ocamllex_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("INITIALIZING :lex field");
#endif

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, mll_assoc);

            s7_pointer ocamllex_assoc = s7_list(s7, 2,
                                            mll_kw, statics_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) LOG_S7_DEBUG("ocamllex_assoc", ocamllex_assoc);
#endif

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         ocamllex_assoc));
            /* if (mibl_debug_traversal) */
            /*     LOG_S7_DEBUG("pkg_alist", new_pkg_alist); */

            /* tostr = TO_STR(pkg_key); */
            /* log_debug("set ht key: %s", tostr); */
            /* free(tostr); */
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :signatures");
                LOG_S7_DEBUG("ocamllex_alist", ocamllex_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         mll_kw,
                                         static_kw,
                                         mname_sym);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                /* LOG_S7_DEBUG("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer mll_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("mll_alist", mll_alist);
            }
#endif
            if (mll_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("ocamllex_alist", ocamllex_alist);
                }
#endif

                s7_pointer ocamllex_alist_cdr = s7_cdr(ocamllex_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("ocamllex_alist_cdr", ocamllex_alist_cdr);
                }
#endif

                /* s7_pointer mll_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mll_file)); //mll_assoc)); */
                /* if (mibl_debug_traversal) LOG_S7_DEBUG("new mll_assoc", mll_assoc); */

                s7_pointer new_ocamllex_alist_cdr =
                    s7_append(s7, ocamllex_alist_cdr,
                              s7_list(s7, 1, mll_assoc));

#if defined(DEBUG_TRACE)

                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("new_ocamllex_alist_cdr", new_ocamllex_alist_cdr);
#endif

                s7_pointer new_ocamllex_alist
                    = s7_set_cdr(ocamllex_alist, new_ocamllex_alist_cdr);
                (void)new_ocamllex_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_ocamllex_alist", new_ocamllex_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "UPDATING" CRESET);
                    LOG_S7_DEBUG("mll_alist", mll_alist);
                }
#endif

                s7_pointer ocamllex_alist_cdr = s7_cdr(mll_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("ocamllex_alist_cdr", ocamllex_alist_cdr);
#endif

                s7_pointer msrcs = s7_append(s7,
                                             ocamllex_alist_cdr,
                                             s7_list(s7, 1, mll_file));

                s7_pointer new_ocamllex_alist
                    = s7_set_cdr(mll_alist, msrcs);
                (void)new_ocamllex_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_ocamllex_alist", new_ocamllex_alist);
                    LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_mll(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_mll", CRESET);
    }
#endif
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("module name: %s ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }
#endif

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

#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_debug("Checking for companion .mli: %s",
                  utstring_body(mli_test));
    }
#endif

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

LOCAL void _update_pkg_mllib_files(s7_pointer pkg_tbl,
                                   char *pkg_name, char *mname,
                                   char *fname, int ftype)
{
    /* TODO:  read the mllib file and populate :submodules list */
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_pkg_mllib_files" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        /* LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */
    }
#endif
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    //s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_alist", pkg_alist);
#endif
    if (pkg_alist == s7_f(s7)) {
        log_error("no pkg alist found for %s, %s", pkg_name, mname);
        exit(1);
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, mllib_kw, static_kw);
        s7_pointer mllibs_alist = s7_call(s7, assoc_in,
                                          s7_list(s7, 2,
                                                  keypath,
                                                  pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("mllibs_alist", mllibs_alist);
#endif

        s7_pointer mllib_file = s7_make_symbol(s7, fname);

        /* s7_pointer mllib_assoc = s7_list(s7, 2, mname_sym, mllib_file); */
        s7_pointer mllib_assoc = s7_cons(s7, mname_sym, mllib_file);
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("mllib_assoc", mllib_assoc);
#endif

        if (mllibs_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("INITIALIZING :lex field");
#endif

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, mllib_assoc);

            s7_pointer mllib_assoc = s7_list(s7, 2,
                                            mllib_kw, statics_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) LOG_S7_DEBUG("mllib_assoc", mllib_assoc);
#endif

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         mllib_assoc));
            /* if (mibl_debug_traversal) */
            /*     LOG_S7_DEBUG("pkg_alist", new_pkg_alist); */

            tostr = TO_STR(pkg_key);
            log_debug("set ht key: %s", tostr);
            free(tostr);
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :signatures");
                LOG_S7_DEBUG("mllibs_alist", mllibs_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         mllib_kw,
                                         static_kw,
                                         mname_sym);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                /* LOG_S7_DEBUG("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer mllib_alist = s7_call(s7, assoc_in,
                                             s7_list(s7, 2,
                                                     keypath,
                                                     pkg_alist));

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("mllib_alist", mllib_alist);
            }
#endif
            if (mllib_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("mllibs_alist", mllibs_alist);
                }
#endif

                s7_pointer mllibs_alist_cdr = s7_cdr(mllibs_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("mllibs_alist_cdr", mllibs_alist_cdr);
                }
#endif

                /* s7_pointer mllib_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mllib_file)); //mllib_assoc)); */
                /* if (mibl_debug_traversal) LOG_S7_DEBUG("new mllib_assoc", mllib_assoc); */

                s7_pointer new_mllibs_alist_cdr =
                    s7_append(s7, mllibs_alist_cdr,
                              s7_list(s7, 1, mllib_assoc));

#if defined(DEBUG_TRACE)

                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("new_mllibs_alist_cdr", new_mllibs_alist_cdr);
#endif

                s7_pointer new_mllibs_alist
                    = s7_set_cdr(mllibs_alist, new_mllibs_alist_cdr);
                (void)new_mllibs_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_mllibs_alist", new_mllibs_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "UPDATING" CRESET);
                    LOG_S7_DEBUG("mllib_alist", mllib_alist);
                }
#endif

                s7_pointer mllib_alist_cdr = s7_cdr(mllib_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("mllib_alist_cdr", mllib_alist_cdr);
#endif

                s7_pointer msrcs = s7_append(s7,
                                             mllib_alist_cdr,
                                             s7_list(s7, 1, mllib_file));

                s7_pointer new_mllib_alist
                    = s7_set_cdr(mllib_alist, msrcs);
                (void)new_mllib_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_mllib_alist", new_mllib_alist);
                    LOG_S7_DEBUG("new pkg_alist", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_mllib(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_mllib", CRESET);
    }
#endif
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("module name: %s ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }
#endif

    _update_pkg_mllib_files(pkg_tbl, pkg_name, mname,
                            ftsentry->fts_name, TAG_MLLIB);
}

LOCAL void _update_pkg_mly_files(s7_pointer pkg_tbl,
                                 char *pkg_name, char *mname,
                                 char *fname, int ftype)
{
    // mly entry structure: (Foo (:mly . foo.mly) (:ml_ foo.ml Dep1 Dep2)  (:mli_ foo.mli Dep1 Dep2))
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_UPDATE_PKG_MLY_FILES" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl", pkg_tbl); */
    }
#endif
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    //s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) log_debug("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (mibl_debug_traversal) log_debug("pkg_alist", pkg_alist); */

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
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
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) log_debug("ocamlyacc_alist", ocamlyacc_alist);
#endif

        s7_pointer mly_file = s7_make_symbol(s7, fname);

        /* s7_pointer mly_assoc = s7_list(s7, 2, mname_sym, mly_file); */
        s7_pointer mly_assoc = s7_cons(s7, mname_sym, mly_file);
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) log_debug("mly_assoc", mly_assoc);
#endif

        if (ocamlyacc_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("INITIALIZING :yacc field");
#endif

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, mly_assoc);

            s7_pointer ocamlyacc_assoc = s7_list(s7, 2,
                                                 mly_kw, statics_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) log_debug("ocamlyacc_assoc", ocamlyacc_assoc);
#endif

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         ocamlyacc_assoc));
            /* if (mibl_debug_traversal) */
            /*     log_debug("pkg_alist", new_pkg_alist); */

            /* tostr = TO_STR(pkg_key); */
            /* log_debug("set ht key: %s", tostr); */
            /* free(tostr); */
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :signatures");
                log_debug("ocamlyacc_alist", ocamlyacc_alist);
                log_debug("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         mly_kw,
                                         static_kw,
                                         mname_sym);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* log_debug("assoc-in", assoc_in); */
                log_debug("keypath", keypath);
                /* log_debug("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer mly_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("mly_alist", mly_alist);
            }
#endif
            if (mly_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("ocamlyacc_alist", ocamlyacc_alist);
                }
#endif

                s7_pointer ocamlyacc_alist_cdr = s7_cdr(ocamlyacc_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug("ocamlyacc_alist_cdr", ocamlyacc_alist_cdr);
                }
#endif

                /* s7_pointer mly_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, mly_file)); //mly_assoc)); */
                /* if (mibl_debug_traversal) log_debug("new mly_assoc", mly_assoc); */

                s7_pointer new_ocamlyacc_alist_cdr =
                    s7_append(s7, ocamlyacc_alist_cdr,
                              s7_list(s7, 1, mly_assoc));

#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    log_debug("new_ocamlyacc_alist_cdr", new_ocamlyacc_alist_cdr);
#endif

                s7_pointer new_ocamlyacc_alist
                    = s7_set_cdr(ocamlyacc_alist, new_ocamlyacc_alist_cdr);
                (void)new_ocamlyacc_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug("new_ocamlyacc_alist", new_ocamlyacc_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "UPDATING" CRESET);
                    LOG_S7_DEBUG("mly_alist", mly_alist);
                }
#endif

                s7_pointer ocamlyacc_alist_cdr = s7_cdr(mly_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    LOG_S7_DEBUG("ocamlyacc_alist_cdr", ocamlyacc_alist_cdr);
#endif

                s7_pointer msrcs = s7_append(s7,
                                             ocamlyacc_alist_cdr,
                                             s7_list(s7, 1, mly_file));

                s7_pointer new_ocamlyacc_alist
                    = s7_set_cdr(mly_alist, msrcs);
                (void)new_ocamlyacc_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_ocamlyacc_alist", new_ocamlyacc_alist);
                    LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_mly(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(RED "_update_mly" CRESET);
    }
#endif
    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("_update_mly: %s; ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }
#endif

    char *mli_name = strdup(ftsentry->fts_name);
    mli_name[strlen(ftsentry->fts_name) - 1] = 'i';
    /* log_trace(RED "mli_name: %s" CRESET, mli_name); */

    char *ml_name = strdup(ftsentry->fts_name);
    ml_name[strlen(ftsentry->fts_name) - 1] = '\0';
    /* log_trace(RED "ml_name: %s" CRESET, ml_name); */

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

LOCAL void _update_pkg_cppo_files(s7_pointer pkg_tbl,
                                 char *pkg_name, char *mname,
                                 char *fname, int ftype)
{
#if defined(DEBUG_TRACE)
    if (verbose) {
        log_trace(RED "_update_pkg_cppo_files" CRESET);
    }
    if (mibl_debug_traversal) {
        log_debug("pkg_name: %s", pkg_name);
        /* log_debug("pkg_tbl", pkg_tbl); */
    }
#endif
    s7_pointer pkg_key = make_pkg_key(pkg_name);
    //s7_make_string(s7, pkg_name);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    if (pkg_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no dunefile in this directory");
#endif
    } else {
        s7_pointer mname_sym   = s7_make_symbol(s7, mname);

        s7_pointer assoc_in = _load_assoc_in();
        s7_pointer keypath = s7_list(s7, 2, cppo_kw, static_kw);
        s7_pointer ocppo_alist = s7_call(s7, assoc_in,
                                        s7_list(s7, 2,
                                                keypath,
                                                pkg_alist));
        /* = s7_call(s7, assoc_in, */
        /*           s7_list(s7, 2, modules_kw, pkg_alist)); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("ocppo_alist", ocppo_alist);
#endif

        s7_pointer cppo_file = s7_make_symbol(s7, fname);

        s7_pointer cppo_assoc = s7_list(s7, 2, mname_sym, cppo_file);
        /* s7_pointer cppo_assoc = s7_cons(s7, mname_sym, cppo_file); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("cppo_assoc", cppo_assoc);
#endif

        if (ocppo_alist == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_debug("INITIALIZING :ocppo field");
#endif

            s7_pointer statics_assoc =
                s7_list(s7, 2, static_kw, cppo_assoc);

            s7_pointer ocppo_assoc = s7_list(s7, 2,
                                            cppo_kw, statics_assoc);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) LOG_S7_DEBUG("ocppo_assoc", ocppo_assoc);
#endif

            s7_pointer new_pkg_alist = s7_append(s7, pkg_alist,
                                                 s7_list(s7, 1,
                                                         ocppo_assoc));
            /* if (mibl_debug_traversal) */
            /*     LOG_S7_DEBUG("pkg_alist", new_pkg_alist); */

            tostr = TO_STR(pkg_key);
            log_debug("set ht key: %s", tostr);
            free(tostr);
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg_alist);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug("UPDATING :signatures");
                LOG_S7_DEBUG("ocppo_alist", ocppo_alist);
                LOG_S7_DEBUG("mname_sym", mname_sym);
            }
#endif

            s7_pointer keypath = s7_list(s7, 3,
                                         cppo_kw,
                                         static_kw,
                                         mname_sym);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                /* LOG_S7_DEBUG("assoc-in", assoc_in); */
                LOG_S7_DEBUG("keypath", keypath);
                /* LOG_S7_DEBUG("pkg_alist", pkg_alist); */
            }
#endif

            s7_pointer cppo_alist = s7_call(s7, assoc_in,
                                           s7_list(s7, 2,
                                                   keypath,
                                                   pkg_alist));

#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                LOG_S7_DEBUG("cppo_alist", cppo_alist);
            }
#endif
            if (cppo_alist == s7_f(s7)) {
                /* new */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "ADDING" CRESET);
                    LOG_S7_DEBUG("mname_sym", mname_sym);
                    LOG_S7_DEBUG("ocppo_alist", ocppo_alist);
                }
#endif

                s7_pointer ocppo_alist_cdr = s7_cdr(ocppo_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug("ocppo_alist_cdr", ocppo_alist_cdr);
                }
#endif

                /* s7_pointer cppo_assoc = */
                /*     s7_list(s7, 1, s7_list(s7, 2, mname_sym, cppo_file)); //cppo_assoc)); */
                /* if (mibl_debug_traversal) log_debug("new cppo_assoc", cppo_assoc); */

                s7_pointer new_ocppo_alist_cdr =
                    s7_append(s7, ocppo_alist_cdr,
                              s7_list(s7, 1, cppo_assoc));

#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    log_debug("new_ocppo_alist_cdr", new_ocppo_alist_cdr);
#endif

                s7_pointer new_ocppo_alist
                    = s7_set_cdr(ocppo_alist, new_ocppo_alist_cdr);
                (void)new_ocppo_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug("new_ocppo_alist", new_ocppo_alist);
                }
#endif
            } else {
                /* update */
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    log_debug(RED "UPDATING" CRESET);
                    LOG_S7_DEBUG("cppo_alist", cppo_alist);
                }
#endif

                s7_pointer ocppo_alist_cdr = s7_cdr(cppo_alist);
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("ocppo_alist_cdr", ocppo_alist_cdr);
                    LOG_S7_DEBUG("cppo_file", cppo_file);
                }
#endif

                s7_pointer msrcs = s7_append(s7,
                                             ocppo_alist_cdr,
                                             s7_list(s7, 1, cppo_file));
                LOG_S7_DEBUG("msrcs", msrcs);

                s7_pointer new_ocppo_alist
                    = s7_set_cdr(cppo_alist, msrcs);
                (void)new_ocppo_alist;
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal) {
                    LOG_S7_DEBUG("new_ocppo_alist", new_ocppo_alist);
                    LOG_S7_DEBUG("new pkgs", pkg_alist);
                }
#endif
            }
        }
    }
}

LOCAL void _update_cppo(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (verbose) {
        log_info(BLU "_update_cppo:" CRESET " %s; ", ftsentry->fts_name);
    }
#endif

    char *pkg_name = dirname(ftsentry->fts_path);
    char *mname = _module_name(ftsentry, ext);
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("module name: %s ", mname);
        log_trace("pkg name: %s; fname: %s", pkg_name, ftsentry->fts_name);
    }
#endif

    /* cppo emits .ml (or .mli) - if static cppol.mli found, update :modules
       else update :structures */

/*     char *ml_name = strdup(ftsentry->fts_name); */
/*     ml_name[strlen(ftsentry->fts_name) - 1] = '\0'; */

/*     char *mli_name = strdup(ftsentry->fts_name); */
/*     mli_name[strlen(ftsentry->fts_name) - 1] = 'i'; */

/*     /\* dirname may mutate its arg, use a copy *\/ */
/*     char *dname = strdup(ftsentry->fts_path); */
/*     UT_string *mli_test; */
/*     utstring_new(mli_test); */
/*     /\* add terminal 'i' with printf *\/ */
/*     utstring_printf(mli_test, "%s/%s", dirname(dname), mli_name); */

/* #if defined(DEBUG_TRACE) */
/*     if (mibl_trace) { */
/*         log_debug("Checking for companion .mli: %s", */
/*                   utstring_body(mli_test)); */
/*     } */
/* #endif */

/*     int rc = access(utstring_body(mli_test), F_OK); */
/*     if (rc) { */
/*         /\* companion mli file not found *\/ */
/*         _update_pkg_structs(pkg_tbl, pkg_name, mname, */
/*                             ml_name, // ftsentry->fts_name, */
/*                             TAG_ML_DYN); */
/*     } else { */
/*         /\* _update_pkg_modules(pkg_tbl, pkg_name, mname, *\/ */
/*         /\*                     ftsentry->fts_name, TAG_ML); *\/ */
/*         _update_pkg_modules(pkg_tbl, pkg_name, mname, */
/*                             ftsentry->fts_name, */
/*                             TAG_CPPO); */
/*         _update_pkg_modules(pkg_tbl, pkg_name, mname, */
/*                             mli_name, // ftsentry->fts_name, */
/*                             TAG_MLI); */
/*         _update_pkg_modules(pkg_tbl, pkg_name, mname, */
/*                             ml_name, // ftsentry->fts_name, */
/*                             TAG_ML_DYN); */
/*     } */

    /* update pkg fld :cppo */
    _update_pkg_cppo_files(pkg_tbl, pkg_name, mname,
                          ftsentry->fts_name, TAG_CPPO);

    /* free(mli_name); */
    /* free(ml_name); */
    /* free(dname); */
    /* utstring_free(mli_test); */
    /* exit(0); */
}

LOCAL void _update_mli(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* printf("_update_mli: "); */
    char *dname = strdup(ftsentry->fts_path);
    char *pkg_path = dirname(dname);
    char *mname = _module_name(ftsentry, ext);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_info(BLU "_update_mli" CRESET);
        log_info("\tpkg path: %s", pkg_path);
        log_info("\tfts_name: %s", ftsentry->fts_name);
        log_info("\tfts_path: %s", ftsentry->fts_path);
        log_info("\tfts_accpath: %s", ftsentry->fts_accpath);
    }
#endif
    /* truncate mli name to get ml name */
    char *ml_name = strdup(ftsentry->fts_name);
    ml_name[strlen(ftsentry->fts_name) - 1] = '\0';
    UT_string *ml_test;
    utstring_new(ml_test);
    utstring_printf(ml_test, "%s/%s", realpath(pkg_path, NULL), ml_name);

#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_debug("Checking for companion .ml: %s",
                  utstring_body(ml_test));
    }
#endif
    int rc = access(utstring_body(ml_test), F_OK);
    if (rc) {
        /* companion ml file not found */
        _update_pkg_sigs(pkg_tbl, pkg_path, mname,
                         ftsentry->fts_name, TAG_MLI);
    } else {
        _update_pkg_modules(pkg_tbl, pkg_path, mname,
                            ftsentry->fts_name, TAG_MLI);
    }
}

LOCAL void _update_ml(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
    /* printf("_update_ml: "); */
    /* dirname may mutate its arg, use a copy */
    char *dname = strdup(ftsentry->fts_path);
    char *pkg_path = dirname(dname);
    char *mname = _module_name(ftsentry, ext);

#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace(BLU "_update_ml" CRESET);
        log_trace("\tpkg path: %s", pkg_path);
        log_trace("\tfts_name: %s", ftsentry->fts_name);
        log_trace("\tfts_path: %s", ftsentry->fts_path);
        log_trace("\tfts_accpath: %s", ftsentry->fts_accpath);
    }
#endif
    char *ml_name = strdup(ftsentry->fts_name);
    UT_string *mli_test;
    utstring_new(mli_test);
    /* add terminal 'i' with printf */
    utstring_printf(mli_test, "%s/%si", realpath(pkg_path, NULL), ml_name);

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug("Checking for companion .mli: %s",
                  utstring_body(mli_test));
    }
#endif
    int rc = access(utstring_body(mli_test), F_OK);
    if (rc) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            { log_debug("Not found: %s", utstring_body(mli_test)); }
#endif
        _update_pkg_structs(pkg_tbl, pkg_path, mname,
                            ftsentry->fts_name, TAG_ML);
    } else {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            { log_debug("Found: %s", utstring_body(mli_test)); }
#endif
        _update_pkg_modules(pkg_tbl, pkg_path, mname,
                            ftsentry->fts_name, TAG_ML);
    }
}

/*
  if no entry in pkg-tbl for ctx dir, add one
  .ml and .mli files only
 */
LOCAL void _handle_ml_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug("_handle_ml_file");
        log_debug("\tfts_name: %s", ftsentry->fts_name, ext);
        log_debug("\tfts_path: %s", ftsentry->fts_path, ext);
    }
#endif
    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    /* _indent(ftsentry->fts_level); */
    /* printf("%d. " CRESET, ftsentry->fts_level); */

    if ((strncmp(ext, ".ml", 3) == 0)
        && (strlen(ext) == 3)) {

        fflush(NULL);
        /* deps_list = analyze_deps_file(ftsentry); */
        /* /\* deps_list is gc_protected *\/ */
        /* if ( !s7_is_list(s7, deps_list) ) { */
        /*     log_error("analyze_deps failed"); */
        /*     exit(EXIT_FAILURE); */
        /* } */

        int ln = strlen(ftsentry->fts_name);
        if (ln > 7) {
            if (strncmp((ftsentry->fts_name) + (ln-7), "cppo", 4) == 0) {
                _update_cppo(pkg_tbl, ftsentry, ext);
            } else {
                _update_ml(pkg_tbl, ftsentry, ext);
            }
        } else {
            _update_ml(pkg_tbl, ftsentry, ext);
        }
    }
    else if ((strncmp(ext, ".mli", 4) == 0)
        && (strlen(ext) == 4)) {

        /* fflush(NULL); */
        /* deps_list = analyze_deps_file(ftsentry); */
        /* /\* deps_list is gc_protected *\/ */
        /* if ( !s7_is_list(s7, deps_list) ) { */
        /*     log_error("analyze_deps failed"); */
        /*     exit(EXIT_FAILURE); */
        /* } */

        int ln = strlen(ftsentry->fts_name);
        if (ln > 8) {
            if (strncmp((ftsentry->fts_name) + (ln-8), "cppo", 4) == 0) {
                _update_cppo(pkg_tbl, ftsentry, ext);
            } else {
                _update_mli(pkg_tbl, ftsentry, ext);
            }
        } else {
            _update_mli(pkg_tbl, ftsentry, ext);
        }
        /* _update_mli(pkg_tbl, ftsentry, ext); */
    }
    /*  */
    else if ((strncmp(ext, ".mlt", 4) == 0)
        /* .mlt - toplevel? ocamlformat? file, not a module */
        && (strlen(ext) == 4)) {
        /* log_warn("UNHANDLED: :%-6s %s", "mlt", ftsentry->fts_name); */
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
    else if ((strncmp(ext, ".mllib", 6) == 0)
        && (strlen(ext) == 6)) {
        _update_mllib(pkg_tbl, ftsentry, ext);
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
    else if ((strncmp(ext, ".mld", 4) == 0)
        && (strlen(ext) == 4)) {
        log_warn("UNHANDLED: :%-6s %s", "mld", ftsentry->fts_name);
        /* _update_pkg_files(pkg_tbl, ftsentry, ext); */
    }
    else if (ext[strlen(ext) - 1] == '~') {
        return;                 /* skip backup files */
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

LOCAL void _handle_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
    file_ct++;

    if (strncmp(ftsentry->fts_name,"BUILD.bazel", 11)==0){
        /* skip BUILD.bazel files */
        return;
    }

    if (strncmp(ftsentry->fts_name,"DUNEFILE.mibl", 13)==0){
        /* skip BUILD.bazel files */
        return;
    }
    /* TODO: skip *.bzl files */
    /* TODO: skip standard files: READMEs, LICENSE, etc. */
    /* _handle_regular_file(ftsentry); */
    if (strncmp(ftsentry->fts_name, "dune-project", 12)
        == 0) {
        _handle_dune_project_file(pkg_tbl, ftsentry);
        return; // break;
    }
    if ((strncmp(ftsentry->fts_name, "dune", 4) == 0)
        /* don't read dune.foo */
        && (strlen(ftsentry->fts_name) == 4)) {
        _handle_dune_file(pkg_tbl, ftsentry);
        /* break; */
        return; // continue;
    }

    char *ext = strrchr(ftsentry->fts_name, '.');

    if (ext) {
        if ((strncmp(ext, ".cm", 3) == 0)) {
            log_debug("skipping .cm? file : %s", ftsentry->fts_name);
        }
        else if (strncmp(ext, ".ml", 3) == 0) {
            /* handle_ml_file will analyze the full extension */
            _handle_ml_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".md", 3) == 0)
                 && (strlen(ext) == 3)) {
            //FIXME: handle_md, not _ml?
            _handle_ml_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".sh", 3) == 0)
                 && (strlen(ext) == 3)) {
            _handle_generic_file(pkg_tbl, ftsentry, ext);
            /*_handle_script_file(pkg_tbl, ftsentry, ext);*/
        }
        else if ((strncmp(ext, ".py", 3) == 0)
                 && (strlen(ext) == 3)) {
            _handle_generic_file(pkg_tbl, ftsentry, ext);
            /*_handle_script_file(pkg_tbl, ftsentry, ext);*/
        }
        else if ((strncmp(ext, ".opam", 5) == 0)
                 && (strlen(ext) == 5)) {
            _handle_opam_file(pkg_tbl, ftsentry);
        }
        else if (fnmatch("*.opam.template",
                         ftsentry->fts_name, 0) == 0) {
            _handle_opam_template_file(pkg_tbl, ftsentry);
        }
        else if (strncmp(ext, ".ocamlformat", 12) == 0) {
            _handle_ocamlformat_file(pkg_tbl, ftsentry);
        }
        else if ((strncmp(ext, ".c", 2) == 0)
                 && (strlen(ext) == 2)) {
            _update_cc_src_file(pkg_tbl, ftsentry, ext);
            /* _handle_cc_file(pkg_tbl, ftsentry, ext); */
        }
        else if ((strncmp(ext, ".h", 2) == 0)
                 && (strlen(ext) == 2)) {
            _update_cc_hdr_file(pkg_tbl, ftsentry, ext);
            /* _handle_cc_file(pkg_tbl, ftsentry, ext); */
        }
        else if ((strncmp(ext, ".cc", 3) == 0)
                 && (strlen(ext) == 3)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".hh", 3) == 0)
                 && (strlen(ext) == 3)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".cpp", 4) == 0)
                 && (strlen(ext) == 4)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".hpp", 4) == 0)
                 && (strlen(ext) == 4)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".cxx", 4) == 0)
                 && (strlen(ext) == 4)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else if ((strncmp(ext, ".hxx", 4) == 0)
                 && (strlen(ext) == 4)) {
            _handle_cc_file(pkg_tbl, ftsentry, ext);
        }
        else {
            _handle_generic_file(pkg_tbl, ftsentry, ext);
        }
    } else {
        /* no extension */
        if (strstr(ftsentry->fts_name, "opam")) {
            _handle_opam_file(pkg_tbl, ftsentry);
        }
        else {
            _handle_generic_file(pkg_tbl, ftsentry, ext);
        }
    }
}

LOCAL void _handle_generic_file(s7_pointer pkg_tbl, FTSENT *ftsentry, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_handle_generic_file %s, %s", ftsentry->fts_name, ext);
#endif
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* _indent(ftsentry->fts_level); */
    /* printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name); */

    if (ftsentry->fts_name[0] == '.') {
        // do not process hidden files
#if defined(DEBUG_TRACE)
        if (mibl_trace) {
            log_trace(RED "Excluding" CRESET " hidden file: %s",
                      ftsentry->fts_path); //, ftsentry->fts_name);
        }
#endif
        return;
    } else {
        _update_pkg_files(pkg_tbl, ftsentry, ext);
    }
}

LOCAL void _handle_dune_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_handle_dune_file: %s", ftsentry->fts_path);
#endif

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
#if defined(DEBUG_TRACE)
        log_debug(RED "TUAREG!" CRESET " %s", ftsentry->fts_path);
#endif
        fclose(fileStream);

        s7_pointer stanzas = s7_eval_c_string(s7, "(list (list 'tuareg))");

        s7_pointer pkg_key = make_pkg_key(dirname(ftsentry->fts_path));

        /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg tbl", pkg_tbl); */
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) LOG_S7_DEBUG("pkg key", pkg_key);
#endif

        s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            LOG_S7_DEBUG("pkg_alist", pkg_alist);
#endif

        s7_pointer assoc = _load_assoc();

        s7_pointer stanzas_alist =
            s7_call(s7, assoc, s7_list(s7, 2,
                                       dune_stanzas_sym,
                                       pkg_alist));

        if (stanzas_alist == s7_f(s7)) {
            s7_pointer stanzas_assoc = s7_cons(s7, dune_stanzas_sym, stanzas);
#if defined(DEBUG_TRACE)
            LOG_S7_DEBUG("appending new stanzas_assoc", stanzas_assoc);
#endif
            /* FIXME: check result */
            /* s7_pointer result = */
            tostr = TO_STR(pkg_key);
            log_debug("set ht key: %s", tostr);
            free(tostr);
            s7_hash_table_set(s7, pkg_tbl, pkg_key,
                              s7_append(s7, pkg_alist,
                                        s7_list(s7, 1, stanzas_assoc)));
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) log_debug("setting cdr of ");
#endif
            s7_set_cdr(stanzas_alist, stanzas);
        }

        /* /\* s7_pointer pkgs = s7_name_to_value(s7, "pkg-tbl"); *\/ */
        /* s7_pointer key = s7_make_string(s7, ftsentry->fts_path); */
        /* /\* s7_pointer test_assoc = s7_list(s7, 2, *\/ */
        /* /\*                                 s7_make_keyword(s7, "test"), *\/ */
        /* /\*                                 s7_make_symbol(s7, "dummy")); *\/ */
        /* char *rpath = realpath(ftsentry->fts_path, NULL); */
        /* /\* FIXME: check result *\/ */
        /* /\* s7_pointer result = *\/ */
        /* s7_hash_table_set(s7, pkg_tbl, key, */
        /*                   s7_list(s7, 3, */
        /*                           s7_list(s7, 2, ws_path_kw, */
        /*                               s7_make_string(s7, ews_root)), */
        /*                           s7_list(s7, 2, pkg_path_kw, */
        /*                                   key), */
        /*                           s7_list(s7, 2, realpath_kw, */
        /*                                   s7_make_string(s7, rpath)))); */

        return;
    }
    fclose(fileStream);     /* end tuareg check */

    dunefile_ct++;

    s7_pointer stanzas = read_dunefile(ftsentry->fts_path); //, "dune");
#if defined(DEBUG_TRACE)
    if (mibl_trace) LOG_S7_DEBUG("READED stanzas", stanzas);
#endif
    /* s7_pointer pkg_key = s7_make_string(s7, dirname(ftsentry->fts_path)); */
    s7_pointer pkg_key = make_pkg_key(dirname(ftsentry->fts_path));
    s7_gc_protect_via_stack(s7, pkg_key);

    /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg tbl", pkg_tbl); */
    /* if (mibl_debug_traversal) LOG_S7_DEBUG("pkg key", pkg_key); */

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    s7_pointer assoc = _load_assoc();

    s7_pointer stanzas_alist =
        s7_call(s7, assoc, s7_list(s7, 2,
                                   dune_stanzas_sym,
                                   pkg_alist));
    s7_gc_protect_via_stack(s7, pkg_alist);

    if (stanzas_alist == s7_f(s7)) {
        s7_pointer stanzas_assoc = s7_cons(s7, dune_stanzas_sym, stanzas);
        /* LOG_S7_DEBUG("appending new stanzas_assoc", stanzas_assoc); */
        /* FIXME: check result */
        /* s7_pointer result = */
        s7_hash_table_set(s7, pkg_tbl, pkg_key,
                          s7_append(s7, pkg_alist,
                                    s7_list(s7, 1, stanzas_assoc)));
        s7_gc_unprotect_via_stack(s7, pkg_key);
        s7_gc_unprotect_via_stack(s7, pkg_alist);

    } else {
        /* if (mibl_debug_traversal) log_debug("setting cdr of "); */
        s7_set_cdr(stanzas_alist, stanzas);
    }

    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("updated pkg-tbl", pkg_tbl); */
}

LOCAL void _handle_dune_project_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_handle_dune_project_file: %s", ftsentry->fts_path);
#endif

    _indent(ftsentry->fts_level);
    /* printf("%d. " RED "%s ", */
    /*        ftsentry->fts_level, */
    /*        ftsentry->fts_name); */
    /* printf(CRESET "\n"); */

    static char buf[256];
    strlcpy(buf, ftsentry->fts_path, 256);

    /* char *s1; */

    s7_pointer stanzas = read_dunefile(ftsentry->fts_path);
    /* log_debug("READed stanzas"); */
    /* LOG_S7_DEBUG("stanzas", stanzas); */

    s7_pointer dune_project_assoc = s7_cons(s7,
                                            dune_project_sym,
                                            stanzas);

    s7_pointer pkg_key = make_pkg_key(dirname(ftsentry->fts_path));
    s7_gc_protect_via_stack(s7, pkg_key);

    // s7_make_string(s7, dirname(ftsentry->fts_path));
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        LOG_S7_DEBUG("pkg_key", pkg_key);
#endif

    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_alist", pkg_alist); */

    if (pkg_alist == s7_f(s7)) {
        // we hit a dune-project file w/o a dune file
        s7_hash_table_set(s7, pkg_tbl, pkg_key,
                          s7_list(s7, 1, dune_project_assoc));
        s7_gc_unprotect_via_stack(s7, pkg_key);

    } else {

        /* FIXME: check result */
        /* s7_pointer result = */
        s7_hash_table_set(s7, pkg_tbl, pkg_key,
                          /* s7_list(s7, 2, */
                          s7_append(s7, pkg_alist,
                                    s7_list(s7, 1,
                                            dune_project_assoc)));
        s7_gc_unprotect_via_stack(s7, pkg_key);
        s7_gc_unprotect_via_stack(s7, pkg_alist);
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("updated pkg-tbl", pkg_tbl); */

        /* return pkg_tbl; */
    }
}

LOCAL void _update_opam_files(s7_pointer pkg_tbl, FTSENT *ftsentry) //, char *ext)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        log_debug("_update_opam_files: %s", //, ext: %s",
                  ftsentry->fts_name); //, ext);
#endif

    /* if (_exclusions(ftsentry, ext)) { */
    /*     if (mibl_debug_traversal) */
    /*         log_warn("excluding %s", ftsentry->fts_name); */
    /*     return; */
    /* } */

    /* s7_pointer pkg_tbl = s7_name_to_value(s7, "pkg-tbl"); */
    /* if (mibl_debug_traversal) */
    /*     LOG_S7_DEBUG("pkg_tbl", pkg_tbl); */

    char *pkg_name = dirname(ftsentry->fts_path);

    s7_pointer pkg_key = make_pkg_key(pkg_name);
    s7_gc_protect_via_stack(s7, pkg_key);

    //s7_make_string(s7, pkg_name);
    s7_pointer pkg_alist  = s7_hash_table_ref(s7, pkg_tbl, pkg_key);
    s7_gc_protect_via_stack(s7, pkg_alist);

    if (pkg_alist == s7_f(s7)) {
        // FIXME: should not happen, we always add a pkg entry first
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            log_debug("no entry for this pkg");
#endif
        s7_gc_unprotect_via_stack(s7, pkg_alist);
    } else {

        s7_pointer assoc = _load_assoc();
        s7_pointer opam_list
            = s7_call(s7, assoc, s7_list(s7, 2, opam_kw, pkg_alist));
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal_opam) LOG_S7_DEBUG("opam_list", opam_list);
#endif

        if (opam_list == s7_f(s7)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam)
                log_debug("adding :opam list to pkg");
#endif

            s7_pointer file_list =
                s7_list(s7, 1,
                        s7_list(s7, 2,
                                opam_kw,
                                s7_make_string(s7, ftsentry->fts_name)));
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam)
                LOG_S7_DEBUG("file_list", file_list);
#endif

            s7_pointer new_pkg = s7_append(s7,
                                           pkg_alist,
                                           file_list);
            s7_gc_unprotect_via_stack(s7, pkg_alist);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam)
                LOG_S7_DEBUG("new pkg", new_pkg);
#endif
            s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg);
            s7_gc_unprotect_via_stack(s7, pkg_key);
        } else {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam) {
                log_debug("updating :opam");
                /* LOG_S7_DEBUG("srcs_alist", srcs_alist); */
                LOG_S7_DEBUG("opam_list", opam_list);
            }
#endif
            s7_pointer new_opam_list =
                s7_cons(s7,
                        s7_make_string(s7, ftsentry->fts_name),
                        s7_cdr(opam_list));
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam) {
                LOG_S7_DEBUG("new opam_list", new_opam_list);
            }
#endif

            s7_pointer sort = s7_name_to_value(s7, "sort!");
            s7_pointer lt = s7_name_to_value(s7, "string<?");
            s7_pointer sorted
                = s7_call(s7, sort, s7_list(s7, 2,
                                            new_opam_list,
                                            lt));

            /* LOG_S7_DEBUG("new opam_list sorted", sorted); */

            s7_set_cdr(opam_list, sorted);
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal_opam) {
                LOG_S7_DEBUG("opam_list", opam_list);
            }
#endif
            /* s7_hash_table_set(s7, pkg_tbl, pkg_key, new_pkg); */
        }
    }
}

LOCAL void _handle_opam_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal_opam) {
        log_trace("_handle_opam_file %s", ftsentry->fts_name);
        /* log_trace(RED "Ignoring opam files for now" CRESET "\n"); */
    }
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    /* _indent(ftsentry->fts_level); */

    /* printf("%d. " MAG  ":%-6s" CRESET " %s\n", */
    /*        ftsentry->fts_level, */
    /*        "opam", */
    /*        ftsentry->fts_name); */
#endif

    _update_opam_files(pkg_tbl, ftsentry);
}

LOCAL void _handle_opam_template_file(s7_pointer pkg_tbl, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_trace("_handle_opam_template_file %s", ftsentry->fts_name);
        log_trace(RED "Ignoring opam template files for now" CRESET "\n");
    }
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    /* char *ext = strrchr(ftsentry->fts_name, '.'); */
    /* _indent(ftsentry->fts_level); */

    /* printf("%d. " MAG  ":%-6s" CRESET " %s\n", */
    /*        ftsentry->fts_level, */
    /*        "opam", */
    /*        ftsentry->fts_name); */
#endif

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

LOCAL __attribute__((unused)) void _handle_script_file(s7_pointer pkg_tbl,
                               FTSENT *ftsentry, char *ext)
{
    /* if (mibl_debug_traversal) */
    /*     log_debug("_handle_script_file %s, %s\n", ftsentry->fts_name, ext); */
    /* printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */

    _indent(ftsentry->fts_level);
    /* printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name); */

    _update_pkg_script_files(pkg_tbl, ftsentry, ext);
}

LOCAL void _handle_cc_file(s7_pointer pkg_tbl,
                               FTSENT *ftsentry, char *ext)
{
    /* if (mibl_debug_traversal) { */
    /*     log_debug("_handle_cc_file %s, %s\n", ftsentry->fts_name, ext); */
    /*     printf("    pkg: %s\n", dirname(ftsentry->fts_path)); */
    /* } */

    /* _indent(ftsentry->fts_level); */
    /* printf("%d. %s\n", ftsentry->fts_level, ftsentry->fts_name); */

    _update_cc_src_file(pkg_tbl, ftsentry, ext);
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
        /* return; */
    }

    /* for now, treat symlinked files as ordinary files */
    _handle_file(pkg_tbl, ftsentry);
    return;


#define LINK_BUFSZ 4095
    char linkbuf[LINK_BUFSZ];
    int  linklen;

    _indent(ftsentry->fts_level);

    /* printf("%d. ", ftsentry->fts_level); */

    /* if (strncmp(ftsentry->fts_name, "dune", 4) == 0) { */
    /*     printf(RED); */
    /* } */

    /* printf("%s ", ftsentry->fts_name); */
    /* printf(YEL); */
    linklen = 0;
    linklen = readlink(ftsentry->fts_path,
                       linkbuf, LINK_BUFSZ);
    if (linklen < 0) {
        printf(RED "ERROR: " CRESET " on readlink: %s \n",
               strerror(errno));
    } else {
        printf("SYMLINK %.*s\n", linklen, linkbuf);
    }
    fflush(stdout);
}

/* control traversal order */
LOCAL int _compare(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

LOCAL __attribute__((unused)) const char *_get_path_dir(s7_pointer arg)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace("_get_path_dir", arg);
#endif

    char *pathdir = (char*)s7_string(arg);

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
#if defined(DEBUG_TRACE)
    LOG_S7_DEBUG("merging", ht2);
    LOG_S7_DEBUG(" into", ht1);
#endif

    s7_pointer _iter = s7_make_iterator(s7, ht2);
    s7_int _gc = s7_gc_protect(s7, _iter);

    s7_pointer _item = s7_iterate(s7, _iter);

    while ( ! s7_is_eq(s7_eof_object(s7), _item) ) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            /* LOG_S7_DEBUG("item", _item); */
            LOG_S7_DEBUG("merging key", s7_car(_item));
            LOG_S7_DEBUG("val", s7_cdr(_item));
        }
#endif
        s7_hash_table_set(s7, ht1, /* OBAZL */
                          s7_car(_item),
                          s7_cdr(_item));
        _item = s7_iterate(s7, _iter);
    }

    s7_gc_unprotect_at(s7, _gc);
    return ht1;
}

EXPORT s7_pointer g_load_project(s7_scheme *s7,  s7_pointer args)
{
    /* arg0: rootpath to traverse */
    /* arg1: restrict bazel emit to this pkg */

    /* s7_pointer _s7_rootpath = s7_car(args); */

    /* trace = true; */

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug(RED "g_load_project" CRESET);
        LOG_S7_DEBUG("args", args);
        log_debug("cwd: %s", getcwd(NULL, 0));
        log_debug("build_wd: %s (=BUILD_WORKING_DIRECTORY)", build_wd);
        log_debug("launch_dir: %s", launch_dir);
        log_debug("base ws root: %s", rootws);
        log_debug("effective ws root: %s", ews_root);
        log_debug("cwd: %s", getcwd(NULL, 0));
    }
#endif

    /* s7_pointer wss =  */
    ///s7_pointer root_ws =
    initialize_mibl_data_model(s7);

    /* FIXME: s7_pointer _pkg_tbl = */
    /* s7_eval_c_string(s7, "(cadr (assoc-in '(:@ :pkgs) *mibl-project*))"); */
    /* printf("pkg_tbl: %s\n", TO_STR(_pkg_tbl)); */

    s7_int args_ct = s7_list_length(s7, args);
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) log_debug("args ct: %d", args_ct);
#endif

    /* s7_define_variable(s7, "*mibl-dune-execlib-includes-main*", s7_t(s7)); */

    const char *rootdir, *pathdir;

    if ( s7_is_null(s7, args)
         || (args_ct == 0) ) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) log_debug("args null or args ct 0");
#endif
        rootdir = getcwd(NULL,0);
        pathdir = ".";
        /* s7_pointer _pkg_tbl = */
        load_project(rootdir, pathdir);
        /* if (mibl_trace) { */
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_trace("LOADED DUNE NOARG");
#endif
        /* log_trace(RED "*mibl-project*:" CRESET " %s\n", */
        /*           TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */
        /* } */
        return s7_name_to_value(s7, "*mibl-project*");

    } else if (args_ct < 3) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) log_debug("args ct < 3");
#endif

        /* rootpath is always arg 0; ignore arg 1, it is used to set var mibl-emit-bazel-pkg* */
        s7_pointer arg = s7_car(args);
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal)
            LOG_S7_DEBUG("arg 0", arg);
#endif

        if (s7_is_list(s7, arg)) {
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal)
                log_info("Arg 0 is list", arg);
#endif

            if (arg == s7_nil(s7)) {
#if defined(DEBUG_TRACE)
                if (mibl_debug_traversal)
                    log_trace("Arg 0 is null list", arg);
#endif
                rootdir = getcwd(NULL, 0);
                pathdir = ".";
                /* s7_pointer _pkg_tbl = */
                load_project(rootdir, pathdir);
                /* if (mibl_trace) { */
#if defined(DEBUG_TRACE)
                log_trace("LOADED DUNE NOARG");
#endif
                /* log_trace(RED "*mibl-project*:" CRESET " %s\n", */
                /*           TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */
                /* } */
                return s7_name_to_value(s7, "*mibl-project*");
            }

            rootdir = getcwd(NULL,0);

            /* s7_int gc1; */
            /* s7_pointer _pkgs, _iter; */

            /* FIXME: only allow a single rootpath, always arg 0 */

            s7_pointer arglist = arg;
            while ( !s7_is_null(s7, arglist)) {
                s7_pointer item = s7_car(arglist);
                (void)item;
#if defined(DEBUG_TRACE)
                if (mibl_trace)
                    LOG_S7_DEBUG("item", item);
#endif
                /* pathdir = _get_path_dir(item); */
                if (true) { //(pathdir) {
                    /* FIXME: s7_pointer _pkgs = */
                    load_project(rootdir, "."); //pathdir);
#if defined(DEBUG_TRACE)
                    if (mibl_trace)
                        log_trace(RED "LOADED DUNE 1" CRESET);
#endif

                    //FIXME: is this needed?
                    /* if (s7_is_hash_table(_pkgs)) { */
                    /*     _pkg_tbl = _merge_pkg_tbls(s7, _pkg_tbl, _pkgs); */
                    /*     LOG_S7_DEBUG("merged result", _pkg_tbl); */
                    /* } else { */
                    /*     LOG_S7_DEBUG("load_project returned", _pkgs); */
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
            /*                             s7_name_to_value(s7, "*mibl-project*")), */
            /*                     _pkg_tbl), */
            /*         s7_rootlet(s7)); */

            /* printf("root_ws 1: %s\n", TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */

            return s7_name_to_value(s7, "*mibl-project*");
        }
        else if (s7_is_string(arg)) {
            /* one string arg == path relative to current wd */
            rootdir = getcwd(NULL,0);
#if defined(DEBUG_TRACE)
            if (mibl_trace)
                log_trace("Rootdir: %s", rootdir);
#endif
            /* pathdir = _get_path_dir(arg); */
            /* s7_pointer q = s7_name_to_value(s7, "quote"); */
            if (true) { // (pathdir) {
                s7_pointer _pkg_tbl =
                    load_project(rootdir, "."); // pathdir);
                (void)_pkg_tbl;
#if defined(DEBUG_TRACE)
                if (mibl_trace)
                    log_trace(RED "LOADED DUNE 2" CRESET);
#endif

                //FIXME: is this needed?

                //TODO: use s7_eval?
                /* s7_eval_c_string_with_environment(s7, */
                /*    "(set-cdr! (assoc-in '(:@ :pkgs) *mibl-project*) (list _pkg_tbl))", */
                /*    s7_inlet(s7, s7_list(s7, 1, */
                /*    s7_cons(s7, s7_make_symbol(s7, "_pkg_tbl"), _pkg_tbl)))); */

                /* printf("root_ws 2: %s\n", TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */
                /* print_backtrace(s7); */
                /* return s7_name_to_value(s7, "*mibl-project*"); */
                return s7_t(s7);
                /* return s7_make_string(s7, "FOOBAR"); */

            } else {
                log_error("cwd: %s", getcwd(NULL,0));
                return s7_nil(s7);
            }
        } else {
            log_error("Arg must be string or list of strings");
            return s7_nil(s7);
        }
    } else {
        log_error("Too many args");
        fprintf(stderr,
                RED "ERROR: unexpected arg count %d for mibl-load-project\n",
                (int)args_ct);
        exit(EXIT_FAILURE);
    }

    /* exit(0); */

    /* printf("root_ws 3: %s\n", TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */
    return s7_name_to_value(s7, "*mibl-project*");
}

bool traverse_dir(FTS* tree, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace(MAG "traverse_dir?" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
#endif

    if (_this_is_hidden(ftsentry)) {
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_trace(RED "Excluding" CRESET " hidden dir: %s",
                      ftsentry->fts_path);
#endif
        fts_set(tree, ftsentry, FTS_SKIP);
        return false;
    }
    else if (strncmp(ftsentry->fts_name, "_build", 6) == 0) {
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_trace(RED "Excluding: " CRESET "%s",
                      ftsentry->fts_path);
#endif
        fts_set(tree, ftsentry, FTS_SKIP);
        return false;
    }
    else if (fnmatch("_opam",
                     ftsentry->fts_name, 0) == 0) {
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_trace(RED "Excluding: " CRESET "%s",
                      ftsentry->fts_path);
#endif
        fts_set(tree, ftsentry, FTS_SKIP);
        return false;
    }
    else if (fnmatch("*.opam-bundle",
                     ftsentry->fts_name, 0) == 0) {
#if defined(DEBUG_TRACE)
        if (mibl_trace)
            log_trace(RED "Excluding: " CRESET "%s",
                      ftsentry->fts_path);
#endif
        fts_set(tree, ftsentry, FTS_SKIP);
        return false;
    }
    else if (ftsentry->fts_name[0] == '.') {
        if (ftsentry->fts_path[0] == '.') {
            if (strlen(ftsentry->fts_path) == 1) {
                // root dir
                return true;
            }
        }
    }
    /* exclusions override inclusions */
    /* if exclude return false */
    /* otherwise, if include return true else false */

    /* for exclusions we want an exact match */

    /* discard leading "./" */
    char *ptr = NULL;
    if ((ftsentry->fts_path[0] == '.') && (ftsentry->fts_path[1] == '/'))
        ptr = ftsentry->fts_path+2;
    else
        ptr = ftsentry->fts_path;

#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace("srch ptr: %s", ptr);
#endif
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
        fts_set(tree, ftsentry, FTS_SKIP);
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
#if defined(DEBUG_TRACE)
            log_debug("inclusion test pfx: '%s', path: '%s'",
                      *p, ftsentry->fts_path);
            log_debug("result: %d",
                      strncmp(*p, ftsentry->fts_path, strlen(*p)));
#endif
            if (strncmp(*p, ftsentry->fts_path, strlen(*p)) < 1) {
                if (verbose) { // & verbosity > 2) {
                    log_info("Include! '%s'", ftsentry->fts_path);
                }
                return true;
            };
        }
        if (verbose) { // & verbosity > 2) {
#if defined(DEBUG_TRACE)
            log_debug("Include? '%s': %d", ftsentry->fts_path, false);
#endif
        }
        return false;
    } else {
        return true;
    }
}

/* deprecated - do this in scheme code */
/* LOCAL __attribute__((unused)) void _emit_pkg_parsetrees(s7_pointer pkg_tbl) */
/* { */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_trace) log_trace("_emit_pkg_parsetrees"); */
/* #endif */
/*     /\* log_info("\tpkg_tbl: %s", TO_STR(pkg_tbl)); *\/ */

/*     char *ws_root = getenv("BUILD_WORKSPACE_DIRECTORY"); */

/*     s7_pointer env = s7_inlet(s7, */
/*                               s7_list(s7, 2, */
/*                                       s7_cons(s7, */
/*                                               s7_make_symbol(s7, "pkg-tbl"), */
/*                                               pkg_tbl), */
/*                                       s7_cons(s7, */
/*                                               s7_make_symbol(s7, "ws-root"), */
/*                                               s7_make_string(s7, ws_root)) */
/*                                       )); */
/*     /\* LOG_S7_DEBUG("env: %s", TO_STR(env)); *\/ */
/*     char * exec_sexp = */
/*         "(for-each (lambda (k)" */
/*         "            (let* ((pkg (hash-table-ref pkg-tbl k))" */
/*         "                  (pkg-path (car (assoc-val :pkg-path pkg)))" */
/*         "                  (outpath (string-append ws-root \"/\" pkg-path \"/DUNEFILE.mibl\")))" */
/*         "              (call-with-output-file outpath" */
/*         "                 (lambda (p)" */
/*         "                    (mibl-pretty-print pkg p)))))" */
/*         /\* flush-output-port? *\/ */
/*         "          (hash-table-keys pkg-tbl))" */
/*         ; */
/*         /\* "              (format #t \"OUTPATH: ~A~%\" outpath)))" *\/ */

/*     s7_pointer x = s7_eval_c_string_with_environment(s7, exec_sexp, env); */

/*     (void)x; */
/*     fflush(stdout); */
/* } */

EXPORT s7_pointer load_project(const char *home_sfx, const char *traversal_root)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug(BLU "load_project" CRESET);
        log_debug("%-16s%s", "cwd:", getcwd(NULL, 0));
        log_debug("%-16s%s", "launch_dir:", launch_dir);
        log_debug("%-16s%s", "base ws:", rootws);
        log_debug("%-16s%s", "effective ws:", ews_root);
        log_debug("%-16s%s", "home_sfx:", home_sfx);
        log_debug("%-16s%s", "traversal_root:", traversal_root);
    }
#endif
    /*
      FIXME: traversal root(s) to be determined by miblrc.srcs.include
      default is cwd, but if miblrc designates 'include' dirs, then
      cwd must be excluded, and each 'include' dir traversed.
     */

    /* populated by postorder dir handler below: */
    /* UT_array  *_ocaml_src_dirs; */
    /* utarray_new(_ocaml_src_dirs, &ut_str_icd); */

    if (traversal_root) {
        /* log_debug("Atr cwd: %s", getcwd(NULL, 0)); */
        char *tr = realpath(traversal_root, NULL);
        /* log_debug("reapath tr %s", tr); */
        chdir(tr);  // (traversal_root);
        /* log_debug("Btr cwd: %s", getcwd(NULL, 0)); */
        traversal_root = ".";
    }

    /* UT_string *abs_troot; */
    /* utstring_new(abs_troot); */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_debug_traversal) log_debug("build_wd: %s", build_wd); */
/* #endif */
    /* utstring_printf(abs_troot, "%s/%s", */
    /*                 //getcwd(NULL,0), */
    /*                 //build_wd, */
    /*                 ews_root, */
    /*                 traversal_root); */

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        /* log_debug("abs_troot: %s", utstring_body(abs_troot)); */
        log_debug("ews_root: %s", ews_root);
        log_debug("traversal_root: %s", traversal_root);
    }
#endif
/*     char *abstr = strdup(utstring_body(abs_troot)); //FIXME: free after use */
/*     (void)abstr; */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_debug_traversal) log_debug("abstr: %s", abstr); */
/* #endif */

    /* char *_ews = effective_ws_root(abstr); */
/* #if defined(DEBUG_TRACE) */
/*     if (mibl_debug_traversal) log_debug("ews: %s", _ews); */
/* #endif */
/*     ews_root = _ews; */
    // put ews_root into the scheme env. so users can use it
    /* s7_define_variable(s7, */
    /*                    "effective-ws-root", */
    /*                    s7_make_string(s7, ews_root)); */

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        /* log_debug("haystack (troot): %s", utstring_body(abs_troot)); */
        log_debug("needle (ews): %s", ews_root);
    }
#endif

    /* char *resolved_troot = strnstr(utstring_body(abs_troot), */
    /*                                ews_root, strlen(ews_root)); */
    /* if (resolved_troot) { */
    /*     if (strlen(utstring_body(abs_troot)) == strlen(ews_root)) { */
    /*         /\* resolved_troot = realpath(".",NULL); *\/ */
    /*         /\* log_debug("match: %s", resolved_troot); *\/ */
    /*     } else { */
    /*         resolved_troot = utstring_body(abs_troot) + strlen(ews_root) + 1; // + for '/' */
    /*         /\* log_debug("resolved_troot: %s", resolved_troot); *\/ */
    /*     } */
    /* } else { */
    /*     /\* log_error("no resolved_troot"); *\/ */
    /*     resolved_troot = realpath(".", NULL); */
    /* } */
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        /* log_debug("resolved resolved_troot: %s", resolved_troot); */
        log_debug("cwd: %s", getcwd(NULL, 0));
    }
#endif

    /* utstring_free(abs_troot); */

    errno = 0;

    /*
      always cd to effective ws root, since the resolved traversal
      root is relative to it. that way ftsentry->fts_path will be a
      proper workspace-relative pkg-path.

      restore cwd after traversal.
    */
    char *old_cwd = getcwd(NULL, 0);
    if (strncmp(old_cwd, ews_root, strlen(ews_root)) != 0) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) {
            log_debug("chdir: %s => %s\n", old_cwd, ews_root);
        }
#endif
        rc = chdir(ews_root);
        if (rc != 0) {
            log_error("FAIL on chdir: %s => %s\n", old_cwd, ews_root);
            fprintf(stderr, RED "FAIL on chdir: %s => %s: %s\n",
                    old_cwd, ews_root, strerror(errno));
            exit(EXIT_FAILURE);
        }
#if defined(DEBUG_TRACE)
        if (mibl_debug_traversal) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));
#endif
    }

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _traversal_root[] = {
        /* [0] = resolved_troot, // traversal_root; */
        [0] = (char *const)traversal_root,
        NULL
    };
#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal) {
        log_debug("cwd: %s", getcwd(NULL, 0));
        log_debug("_traversal_root: %s", _traversal_root[0]);
        log_debug("real _traversal_root: %s",
                         realpath(_traversal_root[0], NULL));
    }
#endif

    /* deps_list = analyze_deps_wsroot(_traversal_root); */
    /* if ( !s7_is_list(s7, deps_list) ) { */
    /*     log_error("analyze_deps failed"); */
    /*     exit(EXIT_FAILURE); */
    /* /\* } else { *\/ */
    /* /\*     /\\* case: dune file but no ocaml srcs *\\/ *\/ */
    /* /\*     if (deps_list == s7_nil(s7)) *\/ */
    /* /\*         return deps_list;             /\\* empty list *\\/ *\/ */
    /* } */

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
        s7_eval_c_string(s7, "(cadr (assoc-in '(:@ :pkgs) *mibl-project*))");
    s7_int gc_pkg_tbl = s7_gc_protect(s7, pkg_tbl);
    if (pkg_tbl != s7_gc_protected_at(s7, gc_pkg_tbl)) {
        log_error("%d: %s is not gc protected at %" print_s7_int ": %s?",
                  __LINE__,
                  tostr1 = TO_STR(pkg_tbl), gc_pkg_tbl,
                  tostr2 = TO_STR(s7_gc_protected_at(s7, gc_pkg_tbl)));
        free(tostr1); free(tostr2);
        exit(1);
    }
    (void)gc_pkg_tbl;

#if defined(DEBUG_TRACE)
    if (mibl_debug_traversal)
        LOG_S7_DEBUG("building pkg_tbl", pkg_tbl);
#endif
    /* s7_int pkg_tbl_gc_loc = s7_gc_protect(s7, pkg_tbl); */

    /* if (verbose) { */
    /*     log_info(GRN "Traversal root:" CRESET " %s", */
    /*              _traversal_root[0]); */
    /*              // resolved_troot); */
    /*     fflush(NULL); */
    /*     /\* log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0)); *\/ */
    /* } */

    /* s7_gc_on(s7, s7_f(s7)); */

    /* TRAVERSAL STARTS HERE */
    /* fprintf(stderr, "/"); */
    bool swap = false;
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            tct++;
            /* log_debug("traversal ct: %d", tct); */
            if (swap) {
                fprintf(stderr, "/\b");
                swap = !swap;
            } else {
                fprintf(stderr, "\\\b");
                swap = !swap;
            }

            s7_flush_output_port(s7, s7_current_output_port(s7));
            s7_flush_output_port(s7, s7_current_error_port(s7));

            /* log_debug("fts iteration"); */
            /* if (ftsentry->fts_info == FTS_DP) { */
            /*     continue; // do not process post-order visits */
            /* } */
#if defined(DEBUG_TRACE)
            if (mibl_debug_traversal) {
                log_debug(CYN "iter ftsentry->fts_name: " CRESET "%s",
                          ftsentry->fts_name);
                log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path);
                log_debug("iter ftsentry->fts_info: %d", ftsentry->fts_info);
            }
#endif
            /* if (mibl_debug_traversal) { */
            /*     if (ftsentry->fts_info != FTS_DP) { */
            /*         log_debug(CYN "ftsentry:" CRESET " %s (%s), type: %d", */
            /*                   ftsentry->fts_name, */
            /*                   ftsentry->fts_path, */
            /*                   ftsentry->fts_info); */
            /*     } */
            /* } */
            switch (ftsentry->fts_info)
                {
                case FTS_D : // dir visited in pre-order
#if defined(DEBUG_TRACE)
                    if (mibl_trace)
                        log_trace("pre-order visit dir: %s (%s) :: (%s)",
                                  ftsentry->fts_name,
                                  ftsentry->fts_path,
                                  ftsentry->fts_accpath);
#endif
                    if (traverse_dir(tree, ftsentry)) {
#if defined(DEBUG_TRACE)
                        if (mibl_trace)
                            log_info(RED "traversing" CRESET " %s",
                                     ftsentry->fts_path);
#endif

                        // codept analysis of pkg will not work because it recurs on subdirs.
                        // that will break if the same module name occurs in multiple subdirs.
                        /* if (strlen(ftsentry->fts_path) > 1) { */
                        /*     deps_list = analyze_deps_pkg(ftsentry->fts_path+2); // skip ./ pfx */
                        /*     if ( !s7_is_list(s7, deps_list) ) { */
                        /*         log_error("analyze_deps failed"); */
                        /*         exit(EXIT_FAILURE); */
                        /*         /\* } else { *\/ */
                        /*         /\*     /\\* case: dune file but no ocaml srcs *\\/ *\/ */
                        /*         /\*     if (deps_list == s7_nil(s7)) *\/ */
                        /*         /\*         return deps_list;             /\\* empty list *\\/ *\/ */
                        /*     } */
                        /* } */

                        dir_ct++;
                        _handle_dir(pkg_tbl, tree, ftsentry);
                        /* printf("pkg tbl: %s\n", TO_STR(pkg_tbl)); */
                    }
                    break;
                case FTS_DP:
                    /* postorder directory */

                    /* use scandir's select filter to see if dir
                       contains ocaml src files. assumption: this is
                       more efficient than keeping a lookup table and
                       checking/updating it on every src file we
                       encounter during traversal. */

                    /* FIXME: skip the same dirs as in preorder */

                    /* if (skip_dir(ftsentry)) */
                    /*     break; */

#if defined(DEBUG_TRACE)
                    if (mibl_trace)
                        log_trace(RED "post-order " CRESET
                                  "visit dir: %s (%s) :: (%s)",
                                  ftsentry->fts_name,
                                  ftsentry->fts_path,
                                  ftsentry->fts_accpath);
#endif
                    /* scan_ct = scandir(ftsentry->fts_path, &namelist, */
                    /*                   _select_ocaml_srcs, */
                    /*                   alphasort); */
                    /* /\* log_debug("scan_ct: %d", scan_ct); *\/ */
                    /* if (scan_ct == 0) { */
                    /*     /\* log_debug("no ocaml srcs in: '%s'", ftsentry->fts_name); *\/ */
                    /* } */
                    /* else if (scan_ct < 0) { */
                    /*     perror("scandir"); */
                    /*     exit(EXIT_FAILURE); */
                    /* } else { */
                    /*     /\* log_debug("ocaml srcs in: '%s'", ftsentry->fts_name); *\/ */
                    /*     /\* char *s = strndup(ftsentry->fts_name, strlen(ftsentry->fts_name)); *\/ */
                    /*     /\* utarray_push_back(_ocaml_src_dirs, &s); *\/ */
                    /*     /\* free(s); *\/ */
                    /* } */
                    break;
                case FTS_F : // regular file
                    _handle_file(pkg_tbl, ftsentry);
                    break;
                case FTS_SL: // symlink
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
        /* log_info("end while: (ftsentry = fts_read(tree)) != NULL)"); */
        chdir(old_cwd);
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    } else {
        log_error("TREE == NULL");
    }
    fprintf(stderr, "\b");
    /* s7_gc_on(s7, s7_t(s7)); */

    /* s7_pointer pkg_tbl = */
    /*     s7_eval_c_string(s7, "(set-cdr! (assoc-in '(:@ :pkgs) *mibl-project*))"); */

    fflush(NULL);

    if (mibl_show_traversal) {
        fflush(NULL);
        log_info(GRN "Traversal summary:" CRESET);
        log_info("\troot ws:\t\t%s", getenv("BUILD_WORKSPACE_DIRECTORY"));
        /* log_info("\troot ws:\t\t%s", rootws); */
        log_info("\teffective ws (cwd):\t%s", getcwd(NULL, 0));
        log_info("\ttraversal root:\t\t%s", _traversal_root[0]);
        log_info("\tdir count:\t\t%d", dir_ct);
        log_info("\tfile count:\t\t%d", file_ct);
        log_info("\tdunefile count:\t\t%d", dunefile_ct);

        /* FIXME: do we need gc_protect here? */
        s7_int gc_loc;
	gc_loc = s7_gc_protect(s7, s7_current_error_port(s7));

        /* log_info("pkg_tbl:"); */
        /* fprintf(stderr, "%s\n", TO_STR(pkg_tbl)); */
        /* log_info("*mibl-project:"); */
        /* fprintf(stdout, "%s\n", NM_TO_STR("*mibl-project*")); */
        s7_flush_output_port(s7, s7_current_output_port(s7));
        s7_flush_output_port(s7, s7_current_error_port(s7));
        /* fprintf(stderr, "s7_gc_unprotect_at gc_loc: %ld\n", (long)gc_loc); */
        s7_gc_unprotect_at(s7, gc_loc);
        fflush(NULL);

        /* s7_pointer wss = s7_eval_c_string(s7, "*mibl-project*"); */
        /* log_info("*mibl-project*: %s\n", TO_STR(wss)); */

        /* print_backtrace(s7); */
        fflush(NULL);
        /* log_info("exiting load_project"); */
    }
    /* s7_gc_unprotect_at(s7, pkg_tbl_gc_loc); */

    UT_string *setter;
    utstring_new(setter);
    utstring_printf(setter, "(set! *mibl-dunefile-count* %d)", dunefile_ct);
    s7_eval_c_string(s7, utstring_body(setter));

    /* if (emit_parsetree || mibl_config.emit_parsetree) { */
    /*     _emit_ws_parsetree(pkg_tbl); */
    /* } */

    /* printf("*mibl-project*: %s\n", */
    /*        TO_STR(s7_name_to_value(s7, "*mibl-project*"))); */

    /* if (mibl_show_deps) { */
    /*     log_info("DEPS LIST:"); */
    /*     s7_pointer env */
    /*         = s7_inlet(s7, s7_list(s7, 1, */
    /*                                s7_cons(s7, */
    /*                                        s7_make_symbol(s7, "deps-list"), */
    /*                                        deps_list))); */
    /*     char *sexp = "(mibl-pretty-print deps-list) "; */
    /*     s7_pointer r = s7_eval_c_string_with_environment(s7, sexp, env); */
    /*     (void)r; */
    /*     s7_newline(s7, s7_current_output_port(s7)); */
    /*     s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /* } */

    return pkg_tbl;
}
