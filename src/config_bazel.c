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

#if EXPORT_INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "config_bazel.h"

extern int rc;

extern bool bzl_mode;   /* t: we launched from mibl under bazel */

char *build_ws_dir; /* BUILD_WORKSPACE_DIRECTORY else NULL */
char *build_wd; /* BUILD_WORKING_DIRECTORY else NULL */
char *launch_dir; /* real launch dir */

/* path args passed to mibl relative to build_wd */

/* UT_string *ws_root; */
char *rootws = NULL;     /* proj root workspace */
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

/*
  FIXME: also deal with dune workspace roots
 */
char *_effective_ws_root(char *dir)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace("_effective_ws_root: %s", dir);
#endif

   /* if (strncmp(homedir, dir, strlen(dir)) == 0) { */
   /*     log_warn("No Bazel workspace file found."); */
   /*     return NULL; */
   /* } */

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
#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace("effective_ws_root for: %s", _dir);
#endif

    if (getenv("BAZEL_TEST"))
        return getcwd(NULL,0);

    /* log_debug("effective_ws_root: %s", dir); */
    /* use realpath to remove cwd dot, e.g. /home/uid/foo/bar.  */
    errno = 0;
    char *d = realpath(_dir, NULL); /* FIXME: d must be freed */
    if (errno != 0) {
        perror(NULL);
    }
    char *dir = strdup(d);
    free(d);
    return _effective_ws_root(dir);
}

//FIXME: this sets runfiles dir, not root ws!
void _set_rootws(char *ws_root)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) log_trace(RED "_set_rootws: " CRESET
                              "%s", ws_root);
#endif

    UT_string *_root_ws;
    utstring_new(_root_ws);

    if (getenv("BAZEL_TEST")) {
        /* rootws = strdup(utstring_body(runfiles_root)); */
        utstring_printf(_root_ws, "%s", utstring_body(runfiles_root));
        if (ws_root)
            utstring_printf(_root_ws, "/%s", ws_root);
#if defined(DEBUG_TRACE)
        if (mibl_debug_bazel)
            log_debug("Running under bazel test; setting bws to runfiles root%s", utstring_body(runfiles_root));
#endif
    }
    else {
        /* outside of bazel test env, the bazel run env will set these
           two BUILD_* env vars: */
        //char *_rootws
        build_ws_dir= getenv("BUILD_WORKSPACE_DIRECTORY");
        build_wd    = getenv("BUILD_WORKING_DIRECTORY");

        if (verbose && verbosity > 1) {
            log_info("BUILD_WORKING_DIRECTORY: %s", build_wd);
            log_info("BUILD_WORKSPACE_DIRECTORY: %s", build_ws_dir);
        }

        if (build_ws_dir == NULL) { /* _rootws */
            /* we're not running under bazel run or test, but we may
               be in a Bazel repo/project. So look for nearest
               WORKSPACE.bazel (or WORKSPACE) file ancestor. */

            /* FIXME: always treat cwd as ws root? abort if no ws file
               found? or offer to write one? */

            /* For now we don't care because we only run under bazel. */

            fprintf(stdout, RED "ERROR: " CRESET
                    "This program must be run under Bazel, using 'bazel run' or 'bazel test'.\n");
            exit(EXIT_FAILURE);

            /* effective_ws_root makes a copy */
            rootws = effective_ws_root(getcwd(NULL,0));
#if defined(DEBUG_TRACE)
            if (mibl_debug_bazel)
                log_debug("Found WS file at %s", rootws);
#endif
        } else {
            /* rootws = strdup(build_ws_dir); // _rootws); */
            utstring_printf(_root_ws, "%s", build_ws_dir);
            if (ws_root)
                utstring_printf(_root_ws, "/%s", ws_root);
        }
    }

    rootws = strdup(utstring_body(_root_ws));
    ews_root = strdup(rootws);  /* by default, effective ws == base ws */
#if defined(DEBUG_TRACE)
    if (mibl_debug_bazel)
        log_debug("base ws root: %s", rootws);
#endif

    /* utstring_new(ws_root); */
    /* if (rootws == NULL) */
    /*     utstring_printf(ws_root, "%s", getcwd(NULL, 0)); */
    /* else */
    /*     utstring_printf(ws_root, "%s", rootws); */
}

EXPORT void show_bazel_config(void)
{
    log_info(GRN "Bazel configuration summary:" CRESET);
    if (getenv("BAZEL_TEST")) {
        if (getenv("BUILD_WORKSPACE_DIRECTORY")) {
            log_info("Configured for 'bazel run', TEST_TARGET: %s",
                     getenv("TEST_TARGET"));
        } else {
            log_info("Configured for 'bazel test', TEST_TARGET %s",
                     getenv("TEST_TARGET"));
        }
    }
    else if (getenv("BUILD_WORKSPACE_DIRECTORY")) {
        log_info("Configured for 'bazel run' env");
    }
    else {
        //FIXME: support standalone runs
        fprintf(stderr, RED "ERROR: " CRESET
                "Non-bazel environment. This tool must be run from a Bazel workspace root using 'bazel run'.\n");
        exit(EXIT_FAILURE);
    }
    log_info("\tBAZEL_TEST?: %s", getenv("BAZEL_TEST"));
    log_info("\trootws: %s", rootws);
    log_info("\tHOME: %s", getenv("HOME"));
    log_info("\tPWD: %s", getenv("PWD"));
    log_info("\tBUILD_WORKSPACE_DIRECTORY: %s", getenv("BUILD_WORKSPACE_DIRECTORY"));
    log_info("\tBUILD_WORKING_DIRECTORY: %s", getenv("BUILD_WORKING_DIRECTORY"));

    log_info("\tTEST_TMPDIR: %s", getenv("TEST_TMPDIR"));
    log_info("\tRUNFILES_MANIFEST_FILE: %s", getenv("RUNFILES_MANIFEST_FILE"));
    log_info("\tRUNFILES_MANIFEST_ONLY: %s", getenv("RUNFILES_MANIFEST_ONLY"));
    log_info("\tRUNFILES_DIR: %s", getenv("RUNFILES_DIR"));
    log_info(GRN "End Bazel configuration summary." CRESET);
    fflush(NULL);
}
/* bazel_configure

   Should always be called first.

   IMPORTANT: configuration depends on runtime env. Its not the same
   for 'bazel run' 'bazel test', and running outside of bazel!.

   In particular searching for ws root (WORKSPACE.bazel) by backing up
   the tree does not work under bazel test. In the test env info about
   the workspace is not passed? We can get the workspace name and
   that's about it. That means we cannot chdir to the ws root, as we
   might under 'bazel run', e.g. in order to write files into the
   source tree.
 */
EXPORT void bazel_configure(char *ws_root) // char *_exec_root)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace || mibl_debug_bazel) {
        log_trace("bazel_configure");
    }
#endif

    /* RUNTIME ENVIRONMENT:

       cmd           tgt rule   BAZEL_TEST     BUILD_WORKSPACE_DIRECTORY
       ---           --------   ----------     -------------------------
       'bazel test'   test      defined        undefined
       'bazel run',   test      defined        defined
       'bazel run',  non-test   undefined      defined

       standalone     n/a       undefined      undefined

     */

    launch_dir = getcwd(NULL, 0);

    /* NB: the Bazel cc toolchain _may_ set
       -DBAZEL_CURRENT_REPOSITORY, not sure when.
     */
#if defined(DEBUG_TRACE)
#ifdef BAZEL_CURRENT_REPOSITORY
    /* defined for 'bazel run' UNLESS target is a test rule */
    if (mibl_debug_bazel)
        log_debug("BAZEL_CURRENT_REPOSITORY: '%s'", BAZEL_CURRENT_REPOSITORY);
#endif
#endif
    build_wd = getenv("BUILD_WORKING_DIRECTORY");

    //FIXME: is runfiles_root always === cwd?
    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
#if defined(DEBUG_TRACE)
    if (verbose)
        log_info("runfiles_root: %s", utstring_body(runfiles_root));
#endif

    if (getenv("BAZEL_TEST")) {
        bzl_mode = true;
        if (verbose) log_info("Test rule target: %s",
                              getenv("TEST_TARGET"));
    }
    else if (build_wd == NULL) {
        /* running standalone - outside of bazel */
#if defined(DEBUG_TRACE)
        if (verbose) log_info("Running outside of Bazel");
#endif
        build_wd = launch_dir;
        bzl_mode = false;
        config_xdg_dirs();
#if defined(DEBUG_TRACE)
        log_debug("xdg_data_home: %s", utstring_body(xdg_data_home));
#endif
/* #if defined(DEBUG_TRACE) */
    } else {
        /* running under 'bazel run' */
        bzl_mode = true;
        if (verbose) log_info("Running under 'bazel run'");
/* #endif */
    }

#if defined(DEBUG_TRACE)
    if (mibl_debug_bazel) {
        log_debug("build_wd: %s", build_wd);
        log_debug("launch_dir: %s", launch_dir);
    }
#endif

    /* utstring_new(runfiles_root); */
    /* utstring_printf(runfiles_root, "%s", getcwd(NULL, 0)); */
    /* if (mibl_debug_bazel) */
    /*     log_debug("runfiles_root: %s", utstring_body(runfiles_root)); */

    /* if ( !getenv("BAZEL_TEST") ) */
    _set_rootws(ws_root);
    /* log_debug("rootws: %s", rootws); */
    /* mibl_config(); */
    /* utarray_new(src_files,&ut_str_icd); */

    if (verbose && verbosity > 1) {
        show_bazel_config();
    }
}
