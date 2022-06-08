#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"
#if EXPORT_INTERFACE
#include "s7.h"
#include "utarray.h"
#include "utstring.h"
#endif

#include "s7_config.h"

char *callback_script_file = "dune.scm"; // passed in 'data' attrib
char *callback = "camlark_handler"; /* fn in callback_script_file  */

#if INTERFACE
#define TO_STR(x) s7_object_to_c_string(s7, x)
#endif

bool debug;
bool verbose;

s7_scheme *s7;                  /* GLOBAL s7 */

int rc;

#define LIBS7    "libs7"
#define LIBS7_S7 LIBS7 "/s7"
#define OIBL   "oibl"
#define XDG_LOCAL_SHARE ".local/share"

/* load-path script directories: sys, user, proj, in order
   obazl (sys) scripts:
       run under `bazel run`: dir in runfiles containing callback script
           @camlark//scm/s7, @camlark//scm
       run directly: XDG_DATA_DIRS default: /usr/local/share
           XDG_DATA_DIRS/libs7
           XDG_DATA_DIRS/libs7/s7
   user scripts:
       ($HOME)/.obazl.d/scm
       $XDG_DATA_HOME default: $HOME/.local/share
           XDG_DATA_HOME/s7
           XDG_DATA_HOME/obazl/scm
   proj scripts:
       .obazl.d

 */

s7_pointer s7_error_handler(s7_scheme *sc, s7_pointer args)
{
    log_error("error: %s\n", s7_string(s7_car(args)));
    fprintf(stdout, "error: %s\n", s7_string(s7_car(args)));
    return(s7_f(sc));
}

#if INTERFACE
#define DUNE_LOAD_HELP "(dune-load rootdir pathdir) rootdir is relative to $HOME; pathdir is relative to rootdir.  Change dir to rootdir and load pathdir, creating pkg-tbl"

/* NB: we need to escape #\" in C... */
#define DUNE_LOAD_FORMAL_PARAMS "s"
#endif

/* s7_pointer pkg_tbl; */

#define PKG_CT 50

/* EXPORT UT_array *inventory_opam(void) */
/* { */
/*     config_opam(NULL); */
/*     log_debug("opam switch: %s", utstring_body(opam_switch)); */
/*     log_debug("opam bin: %s", utstring_body(opam_bin)); */
/*     log_debug("opam lib: %s", utstring_body(opam_lib)); */

/*     // FIXME: make re-entrant */
/*     UT_array *opam_dirs;             /\* string list *\/ */
/*     utarray_new(opam_dirs, &ut_str_icd); */

/*     // FIXME: add support for exclusions list */
/*     //int rc = */
/*     dirseq(utstring_body(opam_lib), opam_dirs); */

/*     return opam_dirs; */
/* } */

/*
  sets bazel_script_dir to dir containing scriptfile, which must be
  passed in 'data' attrib of cc_binary rule
 */
LOCAL void _config_bazel_load_path(char *scriptfile, UT_string *manifest)
{
    if (verbose)
        log_info("Configuring for `bazel run`");
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    /* if running under bazel .obazl.d must exist, for codept output */
    utstring_new(obazl_d);
    utstring_printf(obazl_d, "%s/%s", utstring_body(ws_root), ".obazl.d");
    rc = access(utstring_body(obazl_d), R_OK);
    if (rc) {
        if (verbose)
            log_info("Creating project obazl workdir: %s", utstring_body(obazl_ini_path));
        rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(utstring_body(obazl_d));
                log_error("mkdir error");
            }
        }
    }

    /* utstring_new(codept_args_file); */
    /* utstring_printf(codept_args_file, "%s/%s", utstring_body(obazl_d), codept_args_filename); */

    /* utstring_new(codept_deps_file); */
    /* utstring_printf(codept_deps_file, "%s/%s", utstring_body(obazl_d), codept_deps_filename); */

    /*
      build scripts list their scm srcs in the 'data' attrib, which
      puts them in the runfiles area. they are listed in MANIFEST, so
      we need to parse it to find out which dirs we need to add to the
      s7 load path.
     */

    /* bazel (sys) script dir */
    fp = fopen(utstring_body(manifest), "r");
    if (fp == NULL) {
        log_error("fopen failure %s", utstring_body(manifest));
        /* exit(EXIT_FAILURE); */
    }

    if (debug)
        log_debug("Reading MANIFEST");

    char *dune_ed_scm = NULL;

    s7_pointer load_dirs = s7_make_hash_table(s7, 5);
    s7_pointer sdir;

    /*
      WARNING: we have to jump thru some hoops to get the order right:
      "." > dune_ed/scm > libs7/scm > libs7/scm/s7

      We we need to get these paths from the MANIFEST since it has
      absolute paths, and we could be running from any dir (when used
      as a tool lib).

      We build the path list by following the MANIFEST order for
      libs7, but not dune_ed since the MANIFEST seems to put that
      last. So we save the latter when we find it, then when done with
      the MANIFEST we put the dune_ed/scm on top of the stack. Finally
      we append our list to *load-path*, which puts "." on top.
    */

    s7_pointer tmp_load_path = s7_list(s7, 0);

    while ((read = getline(&line, &len, fp)) != -1) {
        /* log_debug("Retrieved line of length %zu:", read); */
        /* log_debug("%s", line); */

        line[strcspn(line, "\n")] = '\0';    /* trim trailing newline */

        /* two tokens per line, first is path relative to exec dir,
           second is corresponding absolute path */
        char *token, *sep = " ";
        token = strtok((char*)line, sep);
        if (token != NULL) {
            token = strtok(NULL, sep);
        } else {
            /* log_debug("skipping entry"); */
            continue;
        }

        char *ext = strrchr(token, '.');
        if (ext != NULL) {
            if ( (strncmp(ext, ".scm", 4) == 0) && strlen(ext) == 4) {
                char *scriptdir = dirname(token);
                /* log_info("SCRIPTDIR: %s", scriptdir); */

                char *substr = strstr(scriptdir, "dune_ed/scm");
                if (substr != NULL) {
                    /* log_debug("FOUND dune_ed path: %s, %s", */
                    /*           line, scriptdir); */
                    if (dune_ed_scm == NULL) {
                        int len = strlen(scriptdir) + 1;
                        dune_ed_scm = calloc(len, 1);
                        strlcpy(dune_ed_scm, scriptdir, len);
                    }
                    continue;
                }

                sdir = s7_make_string(s7, scriptdir);
                s7_pointer r = s7_hash_table_ref(s7, load_dirs, sdir);
                if (r == s7_f(s7)) {
                    // add to hash to ensure uniqueness
                    /* log_debug("adding to hash"); */
                    s7_hash_table_set(s7, load_dirs,
                                      sdir, s7_t(s7));

                    tmp_load_path = s7_append(s7, tmp_load_path,
                                    s7_list(s7, 1,
                                            s7_make_string(s7, scriptdir)));
                }
            }
        }
    }
    fclose(fp);

    /* put dune_ed/scm on top of tmp stack */
    tmp_load_path = s7_cons(s7,
                            s7_make_string(s7, dune_ed_scm),
                            tmp_load_path);

    /* now put default "." on top of tmp stack */
    s7_pointer loadp = s7_load_path(s7);
    tmp_load_path = s7_append(s7, loadp, tmp_load_path);
    /* log_debug("lp: %s", s7_object_to_c_string(s7, tmp_load_path)); */

    /* replace *load-path* with our shiny new stack */
    s7_define_variable(s7, "*load-path*", tmp_load_path);
}

LOCAL void _config_project_load_path(void)
{
    char *project_script_dir = ".obazl.d/scm";

    UT_string *proj_script_dir;
    utstring_new(proj_script_dir);
    utstring_printf(proj_script_dir, "%s/%s",
                    utstring_body(ws_root), project_script_dir);
    rc = access(utstring_body(proj_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("project script dir %s not found",
                     utstring_body(proj_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(proj_script_dir));
        s7_add_to_load_path(s7, utstring_body(proj_script_dir));
    }

    /* private project script dir */
    UT_string *private_script_dir;
    utstring_new(private_script_dir);
    utstring_printf(private_script_dir, "%s/.private/scm",
                    utstring_body(ws_root));
    rc = access(utstring_body(private_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("private script dir %s not found",
                     utstring_body(private_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(private_script_dir));
        s7_add_to_load_path(s7, utstring_body(private_script_dir));
    }
}

LOCAL void _config_user_load_path(void)
{
    char *_home_dir = getenv("HOME");
    char *_user_script_dir = ".obazl.d/scm";
    UT_string *user_script_dir;

    utstring_new(user_script_dir);
    utstring_printf(user_script_dir, "%s/%s",
                    _home_dir, _user_script_dir);

    rc = access(utstring_body(user_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: user script dir: %s.",
                     utstring_body(user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(user_script_dir));
        s7_add_to_load_path(s7, utstring_body(user_script_dir));
    }
}

LOCAL void _config_xdg_load_path(void)
{
    UT_string *xdg_script_dir;

    /* system libs7 script dirs:
       $XDG_DATA_DIRS/libs7
    */
    char *xdg_data_dirs = getenv("XDG_DATA_DIRS");
    if (xdg_data_dirs == NULL) {
        xdg_data_dirs = "/usr/local/share";
    }
    utstring_new(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/%s",
                    xdg_data_dirs, "libs7");
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: %s.", utstring_body(xdg_script_dir));
    } else {
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    }

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 xdg_data_dirs, "s7"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: obazl s7 system script dir at: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* user obazl script dir: $XDG_DATA_HOME/obazl/scm */
    struct passwd *pw = getpwuid(getuid());
    const char *homedir = pw->pw_dir;
    /* log_debug("HOME DIR: %s", homedir); */

    utstring_new(xdg_script_dir);
    char *xdg_data_home = getenv("XDG_DATA_HOME");
    /* s7 first */
    if (xdg_data_home == NULL) {
        xdg_data_home = homedir;
    }
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 homedir, ".local/share/s7"); */
    /* } else { */
    /* utstring_printf(xdg_script_dir, "%s/%s", xdg_data_home, "libs7"); */
    utstring_printf(xdg_script_dir,
                    "%s/.local/share/libs7",
                    xdg_data_home);
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: %s.", utstring_body(xdg_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                      utstring_body(xdg_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    }

    /* utstring_renew(xdg_script_dir); */
    /* if (xdg_data_home == NULL) { */
    /*     utstring_printf(xdg_script_dir, "%s/%s", */
    /*                     homedir, ".local/share/obazl/scm"); */
    /* } else { */
    /*     utstring_printf(xdg_script_dir, "%s/%s", */
    /*                     xdg_data_home, "obazl/scm"); */
    /* } */
    /* /\* log_debug("s7 xdg_script_dir: %s", *\/ */
    /* /\*           utstring_body(xdg_script_dir)); *\/ */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */

    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: user xdg obazl script dir: %s.", */
    /*                  utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */
}

/* LOCAL void _config_oibl_load_path(void) */
/* { */
/*     char *_home_dir = getenv("HOME"); */
/*     char *_user_script_dir = ".obazl.d/scm"; */
/*     UT_string *user_script_dir; */

/*     utstring_new(user_script_dir); */
/*     utstring_printf(user_script_dir, "%s/%s", */
/*                     _home_dir, _user_script_dir); */

/*     rc = access(utstring_body(user_script_dir), R_OK); */
/*     if (rc) { */
/*         if (verbose || debug) */
/*             log_info("Not found: user script dir: %s.", */
/*                      utstring_body(user_script_dir)); */
/*     } else { */
/*         if (verbose) */
/*             log_debug("adding to *load-path*: %s", */
/*                      utstring_body(user_script_dir)); */
/*         s7_add_to_load_path(s7, utstring_body(user_script_dir)); */
/*     } */
/* } */

EXPORT void set_load_path(char *scriptfile)
{
    /* char *_wd = getcwd(NULL, 0); */

    if (debug) {
        s7_pointer lp = s7_load_path(s7);
        log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp));
    }

    /* FIXME: reliable way to detect if we're run by bazel */

    /* https://docs.bazel.build/versions/main/user-manual.html#run
bazel run is similar, but not identical, to directly invoking the binary built by Bazel and its behavior is different depending on whether the binary to be invoked is a test or not. When the binary is not a test, the current working directory will be the runfiles tree of the binary. When the binary is a test, the current working directory will be the exec root and a good-faith attempt is made to replicate the environment tests are usually run in.
  */

    /* so if we find MANIFEST, we're running a test? */
    char *mdir = dirname(utstring_body(exec_root)); // _wd);
    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    if (debug)
        log_debug("MANIFEST: %s", utstring_body(manifest));

    rc = access(utstring_body(manifest), R_OK);

    if (rc) {
        if (verbose)
            log_info("Configuring for non-bazel run");
        _config_xdg_load_path();
    } else {
        _config_bazel_load_path(scriptfile, manifest);
    }
    _config_user_load_path();
    _config_project_load_path();

    s7_add_to_load_path(s7, ".");

    // MANIFEST puts dune_ed/scm last, we need to reorder
    /* s7_load(s7, "utils.scm"); */
    /* s7_pointer del = s7_name_to_value(s7, "remove-ifx"); */
    /* s7_pointer pred = s7_eval_c_string(s7, */
    /*                                    "(lambda (x) (or (equal? x \"/Users/gar/obazl/dune_ed/scm\") (equal? x \".\")))"); */

    /* s7_pointer lp = s7_load_path(s7); */
    /* s7_pointer new_lp = s7_call(s7, del, */
    /*                             s7_list(s7, 2, pred, lp)); */

    /* log_debug("new lp: %s", s7_object_to_c_string(s7, new_lp)); */


    if (debug) {
        s7_pointer lp = s7_load_path(s7);
        log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp));
    }

}

EXPORT void s7_initialize(void)
{
    s7 = s7_init();

    /* trap error messages */
    error_config();

    s7_define_safe_function(s7, "dune-load", g_dune_load,
                            0, 2, 0,
                                 /* DUNE_LOAD_FORMAL_PARAMS, */
                            DUNE_LOAD_HELP);

    set_load_path(callback_script_file);

    s7_repl(s7);

    s7_pointer lf;
    /* log_info("loading default script: %s", callback_script_file); */
    lf =  s7_load(s7, callback_script_file);

    lf =  s7_load(s7, "alist.scm");

    // pkg-tbl
    s7_pointer pkg_tbl = s7_make_hash_table(s7, PKG_CT);
    s7_define_variable(s7, "pkg-tbl", pkg_tbl);
}

EXPORT void s7_shutdown(s7_scheme *s7)
{
    close_error_config();
    s7_quit(s7);
}
