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
#if EXPORT_INTERFACE
#include <stdbool.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

/* #include "ini.h" */
#include "log.h"

/* #if EXPORT_INTERFACE */
#include "utarray.h"
#include "utstring.h"
/* #endif */

#include "config_bazel.h"

/* bool debug; */
/* bool verbose; */

extern int rc;

extern bool bzl_mode;   /* t: we launched from mibl under bazel */

char *build_wd; /* BUILD_WORKING_DIRECTORY else NULL */
char *launch_dir; /* real launch dir */

/* path args passed to mibl relative to build_wd */

/* UT_string *ws_root; */
char *bws_root = NULL;     /* base ws root */
char *ews_root = NULL;     /* effective ws root */

char *traversal_root;           /* maybe not same as ws root */

/* UT_string *runfiles_root;       /\* bazel only *\/ */
/* UT_string *config_obazl; // obazl_d; */

/* #define XDG_LOCAL_SHARE ".local/share" */

UT_string *runtime_data_dir;

UT_string *runfiles_root;

bool ini_error; // = false;
UT_string *obazl_ini_path; // .config


#define OBAZL_VERSION "0.1.0"

/* UT_array *src_files;            /\* FIXME: put this in configuration_s? *\/ */
char *homedir;

/*
  FIXME: also deal with dune workspace roots
 */
char *_effective_ws_root(char *dir)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_debug("effective_ws_root: %s", dir);
#endif

   if (strncmp(homedir, dir, strlen(dir)) == 0) {
       log_debug("No Bazel workspace file found.");
       return NULL;
   }

   UT_string *_ws_path;
   utstring_new(_ws_path);
   utstring_printf(_ws_path, "%s/%s", dir, "WORKSPACE.bazel");
   /* log_debug("Testing %s", utstring_body(_ws_path)); */
   int rc = access(utstring_body(_ws_path), R_OK);
    if (rc == 0) {
        /* log_debug("found %s", utstring_body(_ws_path)); */
        return dir;
    } else {
        utstring_new(_ws_path);
        utstring_printf(_ws_path, "%s/%s", dir, "WORKSPACE");
        int rc = access(utstring_body(_ws_path), R_OK);
        if (rc == 0) {
            /* log_debug("found %s", utstring_body(_ws_path)); */
            return dir;
        } else {
            return effective_ws_root(dirname(dir));
        }
    }
}

char *effective_ws_root(char *_dir)
{
    /* log_debug("effective_ws_root: %s", dir); */
    /* use realpath to remove cwd dot, e.g. /home/uid/foo/bar.  */
    char *d = realpath(_dir, NULL); /* FIXME: d must be freed */
    char *dir = strdup(d);
    free(d);
    return _effective_ws_root(dir);
}

/*
 */
void _set_base_ws_root(void)
{
    /* if (trace) { */
    /*     printf(RED "_set_base_ws_root" CRESET "\n"); */
    /* } */
    char *_bws_root = getenv("BUILD_WORKSPACE_DIRECTORY");
#if defined(DEBUG_TRACE)
    if (debug) log_debug("Bazel BUILD_WORKSPACE_DIRECTORY: %s", bws_root);
#endif

    if (_bws_root == NULL) {
        /* we're not in Bazel rte, but we may be in a Bazel WS. So
           look for nearest WORKSPACE.bazel (or WORKSPACE) file
           ancestor. */
        /* effective_ws_root makes a copy */
        bws_root = effective_ws_root(getcwd(NULL,0));
#if defined(DEBUG_TRACE)
        if (debug)
            log_debug("Found WS file at %s", bws_root);
#endif
    } else {
        bws_root = strdup(_bws_root);
    }

    ews_root = strdup(bws_root);  /* by default, effective ws == base ws */
#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("base ws root: %s", bws_root);
#endif

    /* utstring_new(ws_root); */
    /* if (bws_root == NULL) */
    /*     utstring_printf(ws_root, "%s", getcwd(NULL, 0)); */
    /* else */
    /*     utstring_printf(ws_root, "%s", bws_root); */
}

/* should always be called first, so launch dir gets set to cwd */
EXPORT void bazel_configure(void) // char *_exec_root)
{
#if defined(DEBUG_TRACE)
    if (trace) {
        log_debug("bazel_configure");
    }
#endif
    launch_dir = getcwd(NULL, 0);

    /* NB: the Bazel cc toolchain _may_ set
       -DBAZEL_CURRENT_REPOSITORY, not sure when.
     */
#if defined(DEBUG_TRACE)
#ifdef BAZEL_CURRENT_REPOSITORY
        log_debug("BAZEL_CURRENT_REPOSITORY: %s", BAZEL_CURRENT_REPOSITORY);
#endif
#endif
    build_wd = getenv("BUILD_WORKING_DIRECTORY");

    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
#if defined(DEBUG_TRACE)
    log_debug("runfiles_root: %s", utstring_body(runfiles_root));
#endif

    /* RUNFILES_MANIFEST_FILE and RUNFILES_DIR are only set for 'bazel
       test' runs.
    char *runfiles_manifest = getenv("RUNFILES_MANIFEST_FILE");
    log_debug("RUNFILES_MANIFEST_FILE: %s", runfiles_manifest);

    char *runfiles_dir = getenv("RUNFILES_DIR");
    log_debug("RUNFILES_DIR: %s", runfiles_dir);
    */

#if defined(DEBUG_TRACE)
    if (debug) log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd);
#endif

    config_xdg_dirs();

    if (build_wd == NULL) {
        /* running outside of bazel */
#if defined(DEBUG_TRACE)
        if (debug) log_debug("Running outside of Bazel");
#endif
        build_wd = launch_dir;
        bzl_mode = false;
#if defined(DEBUG_TRACE)
    } else {
        bzl_mode = true;
        if (debug) log_debug("Running in bzl mode");
#endif
    }

#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("build_wd: %s", build_wd);
        log_debug("launch_dir: %s", launch_dir);
        log_debug("xdg_data_home: %s", utstring_body(xdg_data_home));
    }
#endif

    homedir = getenv("HOME");
    /* log_debug("HOME: %s", homedir); */

    /* utstring_new(runfiles_root); */
    /* utstring_printf(runfiles_root, "%s", getcwd(NULL, 0)); */
    /* if (debug) */
    /*     log_debug("runfiles_root: %s", utstring_body(runfiles_root)); */

    _set_base_ws_root();

    /* mibl_config(); */
    /* utarray_new(src_files,&ut_str_icd); */
}
