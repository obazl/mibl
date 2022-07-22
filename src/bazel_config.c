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

#include "bazel_config.h"

/* bool debug; */
/* bool verbose; */

int rc;

char *build_wd; /* BUILD_WORKING_DIRECTORY else NULL */
char *launch_dir; /* real launch dir */

/* path args passed to mibl relative to build_wd */

/* UT_string *ws_root; */
const char *bws_root;     /* base ws root */
char *ews_root;                 /* effective ws root */
char *traversal_root;           /* maybe not same as ws root */

/* UT_string *runfiles_root;       /\* bazel only *\/ */
/* UT_string *config_obazl; // obazl_d; */

#define XDG_LOCAL_SHARE ".local/share"

UT_string *runtime_data_dir;

bool ini_error; // = false;
UT_string *obazl_ini_path; // .config


#define OBAZL_VERSION "0.1.0"

/* UT_array *src_files;            /\* FIXME: put this in configuration_s? *\/ */
char *homedir;

/*
  FIXME: also deal with dune workspace roots
 */
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

char *_effective_ws_root(char *dir)
{
    if (trace)
        log_debug("effective_ws_root: %s", dir);

   if (strncmp(homedir, dir, strlen(dir)) == 0) {
       log_debug("xxxx");
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
    if (debug) log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bws_root);

    if (_bws_root == NULL) {
        /* we're not in Bazel rte, but we may be in a Bazel WS. So
           look for nearest WORKSPACE.bazel (or WORKSPACE) file
           ancestor. */
        /* effective_ws_root makes a copy */
        bws_root = effective_ws_root(getcwd(NULL,0));
        if (debug)
            log_debug("Found WS file at %s", bws_root);
    } else {
        bws_root = strdup(_bws_root);
    }

    ews_root = strdup(bws_root);  /* by default, effective ws == base ws */
    if (debug)
        log_debug("base ws root: %s", bws_root);

    /* utstring_new(ws_root); */
    /* if (bws_root == NULL) */
    /*     utstring_printf(ws_root, "%s", getcwd(NULL, 0)); */
    /* else */
    /*     utstring_printf(ws_root, "%s", bws_root); */
}

/* should always be called first, so launch dir gets set to cwd */
EXPORT void bazel_configure(void) // char *_exec_root)
{
    if (trace) {
        log_debug("bazel_configure");
    }
    launch_dir = getcwd(NULL, 0);

    build_wd = getenv("BUILD_WORKING_DIRECTORY");
    if (debug) log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd);

    if (build_wd == NULL) {
        /* running outside of bazel */
        if (debug) log_debug("BUILD_WORKING_DIRECTORY: null");
        build_wd = launch_dir;
    }

    if (debug) {
        log_debug("build_wd: %s (=BUILD_WORKING_DIRECTORY)", build_wd);
        log_debug("launch_dir: %s", launch_dir);
    }

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
