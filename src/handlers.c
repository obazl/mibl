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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* #if INTERFACE */
#include "utarray.h"
#include "utstring.h"
/* #endif */

#include "log.h"

#include "handlers.h"

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

/* s7_int dune_gc_loc = -1; */

/* s7_pointer g_dunefile_port; */

bool include_this(FTSENT *ftsentry)
{
#if defined(TRACING)
    if (mibl_trace)
        log_trace(MAG "_include_this?" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
#endif

    /* if (mibl_debug) { */
    /*     dump_mibl_config(); */
    /* } */

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

    /* discard leading "./" */
    char *ptr = NULL;
    if ((ftsentry->fts_path[0] == '.') && (ftsentry->fts_path[1] == '/'))
        ptr = ftsentry->fts_path+2;
    else
        ptr = ftsentry->fts_path;

#if defined(TRACING)
    if (mibl_debug) log_debug("srch ptr: %s", ptr);
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
#if defined(TRACING)
            if (mibl_debug) {
                log_debug("inclusion test pfx: '%s', path: '%s'",
                          *p, ftsentry->fts_path);
                log_debug("result: %d\n",
                          strncmp(*p, ftsentry->fts_path, strlen(*p)));
            }
#endif
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

/* control traversal order */
EXPORT int compare_fts(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

bool _is_ws_root(FTSENT *ftsentry)
{
#if defined(TRACING)
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
#if defined(TRACING)
        if (mibl_trace) log_trace("true");
#endif
        return true;
    } else {
        utstring_new(pathdir);
        utstring_printf(pathdir, "%s", ftsentry->fts_path);
        utstring_printf(pathdir, "%s", "/WORKSPACE");
        rc = access(utstring_body(pathdir), R_OK);
        if (!rc) {
#if defined(TRACING)
            if (mibl_trace) log_trace("true");
#endif
            return true;
        }
    }
#if defined(TRACING)
    if (mibl_trace) log_trace("false");
#endif
    return false;
}

/* LOCAL bool _this_is_hidden(FTSENT *ftsentry) */
/* { */
/*     if (ftsentry->fts_name[0] == '.') { */
/*         /\* process the "." passed to fts_open, skip any others *\/ */
/*         if (ftsentry->fts_pathlen > 1) { */
/*             // do not process children of hidden dirs */
/*             /\* if (mibl_trace) *\/ */
/*             /\*     log_trace(RED "Excluding" CRESET " hidden dir: %s\n", *\/ */
/*             /\*               ftsentry->fts_path); //, ftsentry->fts_name); *\/ */
/*             return true; */
/*             /\* } else { *\/ */
/*             /\*     printf("ROOT DOT dir\n"); *\/ */
/*         } */
/*     } */
/*     return false; */
/* } */

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
/* void handle_dir(FTS* tree, FTSENT *ftsentry) */
/* { */
/* #if defined(TRACING) */
/*     if (mibl_debug) { */
/*         log_debug(""); */
/*         /\* log_debug(BLUE "_handle_dir:" CRESET " %s (%s)", *\/ */
/*         /\*           ftsentry->fts_name, ftsentry->fts_path); *\/ */
/*         /\* log_info("%-20s%s", "base ws:", rootws); *\/ */
/*         /\* log_info("%-20s%s", "effective ws:",ews_root); *\/ */
/*         log_info("%-20s%s", "ftsentry->name:", ftsentry->fts_name); */
/*         log_info("%-20s%s", "ftsentry->path:", ftsentry->fts_path); */
/*         log_info("%-20s%s", "ftsentry->accpath:", ftsentry->fts_accpath); */
/*     } */
/* #endif */

/*     UT_string *dune_test; */
/*     utstring_new(dune_test); */
/*     utstring_printf(dune_test, "%s/%s", ftsentry->fts_path, "dune"); */
/*     int rc = access(utstring_body(dune_test), F_OK); */
/*     if (rc) { */
/*         /\* if (mibl_debug) *\/ */
/*             /\* log_warn(RED "dunefile not found in: " CRESET "%s", *\/ */
/*             /\*           ftsentry->fts_path); *\/ */
/*         /\* return; *\/ */
/*     } */

/*     /\* if (strncmp(ews_root, ftsentry->fts_path, *\/ */
/*     /\*              strlen(ftsentry->fts_path)) != 0) { *\/ */
/*     /\*     /\\* is this a ws root (other than base ws)? *\\/ *\/ */
/*     /\*     if (_is_ws_root(ftsentry)) { *\/ */
/*     /\*         /\\* log_debug("SKIPPING ws root: %s", ftsentry->fts_path); *\\/ *\/ */
/*     /\*         /\\* do not process embedded subrepos yet *\\/ *\/ */
/*     /\*         /\\* fts_set(tree, ftsentry, FTS_SKIP); *\\/ *\/ */
/*     /\*         /\\* return; *\\/ *\/ */
/*     /\*     } *\/ */
/*     /\* } *\/ */

/*     /\* stdout *\/ */
/*     _indent(ftsentry->fts_level); */
/* } */

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

/* void handle_ml_file(FTSENT *ftsentry, char *ext) */
/* { */
/* } */

/* void handle_file(FTSENT *ftsentry, char *ext) */
/* { */
/* } */

/* void handle_dune_file(FTSENT *ftsentry) */
/* { */
/* } */

/* void handle_dune_project_file(FTSENT *ftsentry) */
/* { */
/* } */

/* void handle_opam_file(FTSENT *ftsentry) */
/* { */
/* } */

/* void handle_opam_template_file(FTSENT *ftsentry) */
/* { */
/* } */

/* void handle_ocamlformat_file(FTSENT *ftsentry) */
/* { */
/* } */

/* void handle_script_file(FTSENT *ftsentry, char *ext) */
/* { */
/* } */

/* void handle_cc_file(FTSENT *ftsentry, char *ext) */
/* { */
/* } */

/* void handle_symlink(FTS *tree, FTSENT *ftsentry) */
/* { */
/* } */

