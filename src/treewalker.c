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

#if INTERFACE
#include "utarray.h"
#include "utstring.h"
/* #include "s7.h" */
#endif

#include "log.h"
#include "treewalker.h"

UT_array  *segs;
UT_string *group_tag;

UT_string *dunefile_name;

/* s7_int dune_gc_loc = -1; */

/* s7_pointer g_dunefile_port; */

/* void s7_show_stack(s7_scheme *sc); */

UT_string *meta_path;           /* = opam_lib + pkg_name */

extern UT_string *opam_lib;

int fts_d_ct  = 0;
int fts_dp_ct  = 0;
int fts_dot_ct  = 0;
int fts_f_ct = 0;
int fts_sl_ct = 0;

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

LOCAL bool _exclusions(FTSENT *ftsentry, char *ext)
{
    if (strncmp(ext, ".gitignore", 10) == 0)
        return true;
    else
        return false;
}

/* control traversal order */
int _compare(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

bool _include_this(FTSENT *ftsentry)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_trace(MAG "_include_this?" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
#endif

    /* if (debug) { */
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
    if (ftsentry->fts_path[0] == '.' & ftsentry->fts_path[1] == '/')
        ptr = ftsentry->fts_path+2;
    else
        ptr = ftsentry->fts_path;

#if defined(DEBUG_TRACE)
    if (debug) log_debug("srch ptr: %s", ptr);
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
#if defined(DEBUG_TRACE)
            if (debug) {
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

LOCAL void _walk_opam(char *opam_switch)
{
    log_trace("walk_opam: %s", opam_switch);
    log_trace("global opam_lib: %s", utstring_body(opam_lib));

    char *old_cwd = getcwd(NULL, 0);

    int rc = chdir(opam_switch);

#if defined(DEBUG_TRACE)
    if (debug) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));
#endif
    log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _pkg_path[] = {
        [0] = ".", // (char *const)opam_switch,
        NULL
    };

    errno = 0;
    tree = fts_open(_pkg_path,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    /* | FTS_SEEDOT, // handle foo/. and foo/.. */
                    // NULL
                    &_compare
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return;
        /* return s7_error(s7, s7_make_symbol(s7, "fts_open"), */
        /*                 s7_list(s7, 2, */
        /*                         s7_make_string(s7, strerror(errno)), */
        /*                         s7_make_string(s7, _pkg_path[0]))); */
    }
    log_debug("fts_open ok\n");
    if (verbose) {
        log_info(GRN "Beginning traversal" CRESET " at %s",
                 _pkg_path[0]);
                 // resolved_troot);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }

    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            /* if (ftsentry->fts_info == FTS_DP) { continue; } // do not process post-order visits */

#if defined(DEBUG_TRACE)
            if (debug) {
                printf("\n");
                log_debug(CYN "iter ftsentry->fts_name: " CRESET "%s",
                          ftsentry->fts_name);
                log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path);
                log_debug("iter ftsentry->fts_info: %d (%d)", ftsentry->fts_info, FTS_D);
            }
#endif
            switch (ftsentry->fts_info)
                {
                case FTS_D : // dir visited in pre-order
                    fts_d_ct++;
                    opam_handle_fts_d(tree, ftsentry);
                    break;
                case FTS_DP:
                    fts_dp_ct++;
                    /* postorder directory - skip */
                    break;
                case FTS_DOT:
                    /* log_debug("FTS_DOT ftsentry->fts_path: %s", ftsentry->fts_path); */
                    /* log_debug("FTS_DOT ftsentry->fts_info: %d (%d)", ftsentry->fts_info, FTS_D); */
                    fts_dot_ct++;
                    break;
                case FTS_F : // regular file
                    fts_f_ct++;
                    opam_handle_fts_f(tree, ftsentry);
                    break;
                case FTS_SL: // symlink
                    fts_sl_ct++;
                    opam_handle_symlink(tree, ftsentry);
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
        } /* end while */
        /* chdir(old_cwd); */
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    }


    fts_close(tree);
    rc = chdir(old_cwd);


#if defined(DEBUG_TRACE)
    log_debug("fts_d_ct: %d", fts_d_ct);
    log_debug("fts_dp_ct: %d", fts_dp_ct);
    log_debug("fts_dot_ct: %d", fts_dot_ct);
    log_debug("fts_f_ct: %d", fts_f_ct);
    log_debug("fts_sl_ct: %d", fts_sl_ct);
    log_debug("opam_opam_file_ct: %d", opam_opam_file_ct);
    log_debug("opam_meta_ct: %d", opam_meta_ct);
    log_debug("opam_dune_package_ct: %d", opam_dune_package_ct);
    log_debug("traversal root: %s", opam_switch);
    log_debug("cwd: %s", getcwd(NULL, 0));
#endif

}

LOCAL void _walk_findlib_all(char *opam_switch)
{
    log_trace(RED "walk_findlib_all:" CRESET " %s", opam_switch);
    log_trace("global opam_lib: %s", utstring_body(opam_lib));

    char *old_cwd = getcwd(NULL, 0);

    int rc = chdir(opam_switch);

#if defined(DEBUG_TRACE)
    if (debug) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));
#endif
    log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _pkg_path[] = {
        [0] = ".", // (char *const)opam_switch,
        NULL
    };

    errno = 0;
    tree = fts_open(_pkg_path,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    /* | FTS_SEEDOT, // handle foo/. and foo/.. */
                    // NULL
                    &_compare
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return;
        /* return s7_error(s7, s7_make_symbol(s7, "fts_open"), */
        /*                 s7_list(s7, 2, */
        /*                         s7_make_string(s7, strerror(errno)), */
        /*                         s7_make_string(s7, _pkg_path[0]))); */
    }
    log_debug("fts_open ok\n");
    if (verbose) {
        log_info(GRN "Beginning traversal" CRESET " at %s",
                 _pkg_path[0]);
                 // resolved_troot);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }

    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            /* if (ftsentry->fts_info == FTS_DP) { continue; } // do not process post-order visits */

#if defined(DEBUG_TRACE)
            if (debug) {
                printf("\n");
                log_debug(CYN "iter ftsentry->fts_name: " CRESET "%s",
                          ftsentry->fts_name);
                log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path);
                log_debug("iter ftsentry->fts_info: %d (%d)", ftsentry->fts_info, FTS_D);
            }
#endif
            switch (ftsentry->fts_info)
                {
                case FTS_D : // dir visited in pre-order
                    fts_d_ct++;
                    opam_handle_fts_d(tree, ftsentry);
                    break;
                case FTS_DP:
                    fts_dp_ct++;
                    /* postorder directory - skip */
                    break;
                case FTS_DOT:
                    /* log_debug("FTS_DOT ftsentry->fts_path: %s", ftsentry->fts_path); */
                    /* log_debug("FTS_DOT ftsentry->fts_info: %d (%d)", ftsentry->fts_info, FTS_D); */
                    fts_dot_ct++;
                    break;
                case FTS_F : // regular file
                    fts_f_ct++;
                    opam_handle_fts_f(tree, ftsentry);
                    break;
                case FTS_SL: // symlink
                    fts_sl_ct++;
                    opam_handle_symlink(tree, ftsentry);
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
        } /* end while */
        /* chdir(old_cwd); */
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    }


    fts_close(tree);
    rc = chdir(old_cwd);


#if defined(DEBUG_TRACE)
    log_debug("fts_d_ct: %d", fts_d_ct);
    log_debug("fts_dp_ct: %d", fts_dp_ct);
    log_debug("fts_dot_ct: %d", fts_dot_ct);
    log_debug("fts_f_ct: %d", fts_f_ct);
    log_debug("fts_sl_ct: %d", fts_sl_ct);
    log_debug("opam_opam_file_ct: %d", opam_opam_file_ct);
    log_debug("opam_meta_ct: %d", opam_meta_ct);
    log_debug("opam_dune_package_ct: %d", opam_dune_package_ct);
    log_debug("traversal root: %s", opam_switch);
    log_debug("cwd: %s", getcwd(NULL, 0));
#endif

}

/* switch: NULL = no opam; empty = default switch
   pkg: for conversions, relative to root dir
*/
EXPORT void walk_tree(char *opam_switch, char* pkg_name)
{
/* #if defined(DEBUG_TRACE) */
        log_debug(BLU "walk_tree" CRESET);
        log_debug("%-16s%s", "opam switch:", opam_switch);
        log_debug("%-16s%s", "pkg name:", pkg_name);
        log_debug("%-16s%s", "launch_dir:", launch_dir);
        log_debug("%-16s%s", "base ws:", bws_root);
        log_debug("%-16s%s", "effective ws:", ews_root);
/* #endif */
    if (verbose) {
        log_debug("current dir: %s", getcwd(NULL, 0));
        /* printf(YEL "%-16s%s\n" CRESET, "pkg_name:", pkg_name); */
    }

    utstring_new(meta_path);

    if (opam_switch == NULL) {
        /* _walk_project(pkg_name); */
    } else {
        if (pkg_name == NULL) {
            _walk_findlib_all(opam_switch);
        } else {
            utstring_new(workspace_file);
            handle_findlib_pkg(opam_switch, pkg_name);
        }
    }

    utstring_free(meta_path);
    return;
}
