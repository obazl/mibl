#include <assert.h>
#include <ctype.h>
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

/* #if INTERFACE */
/* #include "utarray.h" */
/* #include "utstring.h" */
/* #endif */

/* #include "log.h" */

/* #include "libfindlib.h" */
#include "handlers_opam.h"

UT_array  *segs;
UT_string *group_tag;

int opam_dune_package_ct = 0;
int opam_opam_file_ct = 0;
int opam_meta_ct = 0;

/* void _indent(int i) */
/* { */
/*     /\* printf("_indent: %d\n", i); *\/ */
/*     /\* for (; i > 0; i--) *\/ */
/*     /\*     printf("    "); *\/ */
/* } */

char *ext;

UT_string *dunefile_name;

/* s7_int dune_gc_loc = -1; */

/* s7_pointer g_dunefile_port; */

/* bool _is_ws_root(FTSENT *ftsentry) */
/* { */
/* #if defined(DEBUG_TRACE) */
/*     if (trace) */
/*         log_trace("_is_ws_root: %s", ftsentry->fts_path); */
/* #endif */

/*     UT_string *pathdir; */
/*     utstring_new(pathdir); */
/*     utstring_printf(pathdir, "%s", ftsentry->fts_path); */
/*     utstring_printf(pathdir, "%s", "/WORKSPACE.bazel"); */
/*     /\* log_trace("accessing %s", utstring_body(pathdir)); *\/ */
/*     int rc = access(utstring_body(pathdir), R_OK); */
/*     /\* log_debug("RC: %d", rc); *\/ */
/*     if (!rc) { */
/* #if defined(DEBUG_TRACE) */
/*         if (trace) log_trace("true"); */
/* #endif */
/*         return true; */
/*     } else { */
/*         utstring_new(pathdir); */
/*         utstring_printf(pathdir, "%s", ftsentry->fts_path); */
/*         utstring_printf(pathdir, "%s", "/WORKSPACE"); */
/*         rc = access(utstring_body(pathdir), R_OK); */
/*         if (!rc) { */
/* #if defined(DEBUG_TRACE) */
/*             if (trace) log_trace("true"); */
/* #endif */
/*             return true; */
/*         } */
/*     } */
/* #if defined(DEBUG_TRACE) */
/*     if (trace) log_trace("false"); */
/* #endif */
/*     return false; */
/* } */

LOCAL bool _this_is_hidden(FTSENT *ftsentry)
{
    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            /* if (trace) */
            /*     log_trace(RED "Excluding" CRESET " hidden dir: %s\n", */
            /*               ftsentry->fts_path); //, ftsentry->fts_name); */
            return true;
            /* } else { */
            /*     printf("ROOT DOT dir\n"); */
        }
    }
    return false;
}

/* LOCAL s7_pointer make_pkg_key(char *path) */
/* { */
/*     if (path[0] == '.' && path[1] == '\0') { */
/*         return s7_make_string(s7, path); */
/*     } else { */
/*         if (path[0] == '.' */
/*             && path[1] == '/') { */
/*             return s7_make_string(s7, path+2); */
/*         } else { */
/*             return s7_make_string(s7, path); */
/*         } */
/*     } */
/* } */

/*
  create a pkg-tbl entry if dir contains a dune file or at least one
  OCaml source file.
 */
void handle_dir(FTS* tree, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("");
        log_debug(BLU "handle_dir:" CRESET);
        /* log_info("%-20s%s", "base ws:", bws_root); */
        /* log_info("%-20s%s", "effective ws:",ews_root); */
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }
#endif

    /* UT_string *dune_test; */
    /* utstring_new(dune_test); */
    /* utstring_printf(dune_test, "%s/%s", ftsentry->fts_path, "dune"); */
    /* int rc = access(utstring_body(dune_test), F_OK); */
    /* if (rc) { */
    /*     /\* if (debug) *\/ */
    /*         /\* log_warn(RED "dunefile not found in: " CRESET "%s", *\/ */
    /*         /\*           ftsentry->fts_path); *\/ */
    /*     /\* return; *\/ */
    /* } */

    /* if (strncmp(ews_root, ftsentry->fts_path, */
    /*              strlen(ftsentry->fts_path)) != 0) { */
    /*     /\* is this a ws root (other than base ws)? *\/ */
    /*     if (_is_ws_root(ftsentry)) { */
    /*         /\* log_debug("SKIPPING ws root: %s", ftsentry->fts_path); *\/ */
    /*         /\* do not process embedded subrepos yet *\/ */
    /*         /\* fts_set(tree, ftsentry, FTS_SKIP); *\/ */
    /*         /\* return; *\/ */
    /*     } */
    /* } */

    /* stdout */
    /* _indent(ftsentry->fts_level); */
}

/* static char principal[256]; */

/* LOCAL char *_module_name(FTSENT *ftsentry, char *ext) */
/* { */
/*     strlcpy(principal, ftsentry->fts_name, 256); */
/*     principal[ext - ftsentry->fts_name] = '\0'; */
/*     principal[0] = toupper(principal[0]); */
/*     return (char *)principal; */
/* } */

/* LOCAL char *_principal_name(FTSENT *ftsentry, char *ext) */
/* { */
/*     strlcpy(principal, ftsentry->fts_name, 256); */
/*     principal[ext - ftsentry->fts_name] = '\0'; */
/*     /\* principal[0] = toupper(principal[0]); *\/ */
/*     return (char *)principal; */
/* } */

/* LOCAL bool _exclusions(FTSENT *ftsentry, char *ext) */
/* { */
/*     if (ext == NULL) { */
/*     } else { */
/*         if (strncmp(ext, ".gitignore", 10) == 0) */
/*             return true; */
/*     } */
/*     return false; */
/* } */

void handle_ml_file(FTSENT *ftsentry, char *ext)
{
}

void handle_file(FTSENT *ftsentry, char *ext)
{
}

void handle_dune_file(FTSENT *ftsentry)
{
}

void handle_dune_package_file(FTSENT *ftsentry)
{
    opam_dune_package_ct++;
}

void handle_meta_file(FTSENT *ftsentry)
{
    opam_meta_ct++;
#if defined(DEBUG_TRACE)
    if (debug_findlib) {
        log_debug("");
        log_debug(BLU "handle_meta_file:" CRESET);
        /* log_info("%-20s%s", "base ws:", bws_root); */
        /* log_info("%-20s%s", "effective ws:",ews_root); */
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }
#endif

    handle_findlib_meta(ftsentry);
}

void handle_opam_file(FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("");
        log_debug(BLU "handle_opam_file:" CRESET);
        /* log_info("%-20s%s", "base ws:", bws_root); */
        /* log_info("%-20s%s", "effective ws:",ews_root); */
        log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name);
        log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path);
        log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath);
    }
#endif

    opam_opam_file_ct++;
}

void handle_opam_template_file(FTSENT *ftsentry)
{
}

void handle_ocamlformat_file(FTSENT *ftsentry)
{
}

void handle_script_file(FTSENT *ftsentry, char *ext)
{
}

void handle_cc_file(FTSENT *ftsentry, char *ext)
{
}

void opam_handle_symlink(FTS *tree, FTSENT *ftsentry)
{
}

/* **************************************************************** */
void opam_handle_fts_d(FTS *tree, FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_trace("pre-order visit dir: %s (%s) :: (%s)",
                  ftsentry->fts_name,
                  ftsentry->fts_path,
                  ftsentry->fts_accpath);
#endif
    if (_this_is_hidden(ftsentry)) {
#if defined(DEBUG_TRACE)
        if (trace)
            log_trace(RED "Excluding" CRESET " hidden dir: %s",
                      ftsentry->fts_path);
#endif
        fts_set(tree, ftsentry, FTS_SKIP);
        /* break; */
    }
    else if (fnmatch("*.opam-bundle",
                     ftsentry->fts_name, 0) == 0) {
        fts_set(tree, ftsentry, FTS_SKIP);
        /* break; */
    } else {
        handle_dir(tree, ftsentry);

        /*                         if (_include_this(ftsentry)) { */
        /* #if defined(DEBUG_TRACE) */
        /*                             if (trace) log_info(RED "Including" CRESET " %s", */
        /*                                                 ftsentry->fts_path); */
        /* #endif */
        /*                             if (strncmp(ftsentry->fts_name, "_build", 6) == 0) { */
        /*                                 /\* skip _build (dune) *\/ */
        /*                                 fts_set(tree, ftsentry, FTS_SKIP); */
        /*                                 break; */
        /*                             } */
        /*                             dir_ct++; */
        /*                             handle_dir(tree, ftsentry); */
        /*                             /\* printf("pkg tbl: %s\n", TO_STR(pkg_tbl)); *\/ */
        /*                         } else { */
        /*                             fts_set(tree, ftsentry, FTS_SKIP); */
        /*                         } */
    }
}

void opam_handle_fts_f(FTS *tree, FTSENT *ftsentry)
{
    if (strncmp(ftsentry->fts_name,"BUILD.bazel", 11)==0){
        /* skip BUILD.bazel files - we'll need this when we re-run conversion */
        return;
    }
    /* TODO: skip *.bzl files */

    if ((strstr(ftsentry->fts_name, "opam"))
        && (strlen(ftsentry->fts_name) == 4)) {
        return handle_opam_file(ftsentry);
    }
    else if ((strstr(ftsentry->fts_name, "META"))
             && (strlen(ftsentry->fts_name) == 4)) {
        return handle_meta_file(ftsentry);
    }
    else if (strncmp(ftsentry->fts_name, "dune-package", 12) == 0) {
        return handle_dune_package_file(ftsentry);
    }
    else if ((strncmp(ftsentry->fts_name, "dune", 4) == 0)
             && (strlen(ftsentry->fts_name) == 4)) {
        return handle_dune_file(ftsentry);
    }

    ext = strrchr(ftsentry->fts_name, '.');

    if (ext) {
        if ((strncmp(ext, ".ml", 3) == 0)) {
            handle_ml_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".md", 3) == 0)
                 && (strlen(ext) == 3)) {
            handle_ml_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".sh", 3) == 0)
                 && (strlen(ext) == 3)) {
            handle_file(ftsentry, ext);
            /*_handle_script_file(ftsentry, ext);*/
        }
        else if ((strncmp(ext, ".py", 3) == 0)
                 && (strlen(ext) == 3)) {
            handle_file(ftsentry, ext);
            /*_handle_script_file(ftsentry, ext);*/
        }
        else if ((strncmp(ext, ".opam", 5) == 0)
                 && (strlen(ext) == 5)) {
            handle_opam_file(ftsentry);
        }
        else if (fnmatch("*.opam.template",
                         ftsentry->fts_name, 0) == 0) {
            handle_opam_template_file(ftsentry);
        }
        else if (strncmp(ext, ".ocamlformat", 12) == 0) {
            handle_ocamlformat_file(ftsentry);
        }
        else if ((strncmp(ext, ".c", 2) == 0)
                 && (strlen(ext) == 2)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".h", 2) == 0)
                 && (strlen(ext) == 2)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".cc", 3) == 0)
                 && (strlen(ext) == 3)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".hh", 3) == 0)
                 && (strlen(ext) == 3)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".cpp", 4) == 0)
                 && (strlen(ext) == 4)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".hpp", 4) == 0)
                 && (strlen(ext) == 4)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".cxx", 4) == 0)
                 && (strlen(ext) == 4)) {
            handle_cc_file(ftsentry, ext);
        }
        else if ((strncmp(ext, ".hxx", 4) == 0)
                 && (strlen(ext) == 4)) {
            handle_cc_file(ftsentry, ext);
        }
        else {
            handle_file(ftsentry, ext);
        }
    }
    else {
        /* no extension */
        /* if ((strstr(ftsentry->fts_name, "opam")) */
        /*     && (strlen(ftsentry->fts_name) == 4)) { */
        /*     handle_opam_file(ftsentry); */
        /* } */
        /* else if ((strstr(ftsentry->fts_name, "META")) */
        /*          && (strlen(ftsentry->fts_name) == 4)) { */
        /*     handle_meta_file(ftsentry); */
        /* } */
        /* else { */
            handle_file(ftsentry, ext);
        /* } */
    }
}
