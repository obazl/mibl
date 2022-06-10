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

/* char *callback_script_file = "dune.scm"; // passed in 'data' attrib */
char *callback = "camlark_handler"; /* fn in callback_script_file  */

#if INTERFACE
#define TO_STR(x) s7_object_to_c_string(s7, x)
#endif

s7_scheme *s7;                  /* GLOBAL s7 */

#define HOME_LIBS7 ".libs7"
#if INTERFACE
#define LIBS7_INI_FILE ".config/libs7rc"
#endif

UT_string *config_libs7;        /* work string */

int rc;

#define LIBS7    "libs7"
#define LIBS7_S7 LIBS7 "/s7"
#define OIBL   "oibl"

/* XDG
   https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
 */

/*
  "The base directory defined by $XDG_DATA_HOME is considered more
  important than any of the base directories defined by
  $XDG_DATA_DIRS. The base directory defined by $XDG_CONFIG_HOME is
  considered more important than any of the base directories defined
  by $XDG_CONFIG_DIRS.
*/

/* "If $XDG_DATA_HOME is either not set or empty, a default equal to
   $HOME/.local/share should be used." */

#define XDG_DATA_HOME_SFX ".local/share"
UT_string *xdg_data_home;

#define XDG_CONFIG_HOME_SFX ".config"

/* state home for history files, current state to be reused on restart */
#define XDG_STATE_HOME_SFX ".local/state"

#define XDG_BIN_HOME_SFX   ".local/bin"

#define XDG_CACHE_HOME_SFX ".cache"

/* sockets, named pipes, etc. */
/* #define XDG_RUNTIME_DIR */

/* in addition we define a project-local directory for scm scripts */
#define PROJ_LIBS7 ".libs7"

/* preference-ordered base directories in addition to XDG_DATA_HOME */
#define XDG_DATA_DIRS   "/usr/local/share/:/usr/share/"
#define XDG_CONFIG_DIRS "/etc/xdg"


/* load-path script directories: sys, user, proj, in order
   sys scripts:
       run under `bazel run`: dir in runfiles containing callback script
           @camlark//scm/s7, @camlark//scm
       run directly: XDG_DATA_DIRS default: /usr/local/share/:/usr/share/
           XDG_DATA_DIRS/libs7
           XDG_DATA_DIRS/libs7/s7
   user scripts:
       ($HOME)/.config/libs7
       $XDG_DATA_HOME default: $HOME/.local/share
           XDG_DATA_HOME/libs7
           XDG_DATA_HOME/libs7/s7
   proj scripts:
       .config/libs7

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

LOCAL void _config_s7_load_path_bazel_runfiles(UT_string *manifest)
{
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    /* put project-local .config/libs7 on load-path if it exits. do
       not create it. */

    /* utstring_new(codept_args_file); */
    /* utstring_printf(codept_args_file, "%s/%s", utstring_body(config_libs7), codept_args_filename); */

    /* utstring_new(codept_deps_file); */
    /* utstring_printf(codept_deps_file, "%s/%s", utstring_body(config_libs7), codept_deps_filename); */

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

    /* char *oibl_libs7 = NULL; */

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

        if ( (strncmp(basename(token),
                      "libc_s7.so", 10) == 0) ) {
            if (trace)
                log_info("FOUND LIBC_S7.SO: %s", token);

            tmp_load_path =
                s7_append(s7, tmp_load_path,
                          s7_list(s7, 1,
                                  s7_make_string(s7,
                                                 dirname(token))));

            continue;
        }

        char *ext = strrchr(token, '.');
        if (ext != NULL) {
            if (strncmp(ext, ".so", 3) == 0) {
                log_info("SHARED: %s", token);
            }

            if ( (strncmp(ext, ".scm", 4) == 0) && strlen(ext) == 4) {
                char *scriptdir = dirname(token);
                /* log_info("SCRIPTDIR: %s", scriptdir); */

                /* char *substr = strstr(scriptdir, "dune_ed/scm"); */
                /* if (substr != NULL) { */
                /*     /\* log_debug("FOUND dune_ed path: %s, %s", *\/ */
                /*     /\*           line, scriptdir); *\/ */
                /*     if (oibl_libs7 == NULL) { */
                /*         int len = strlen(scriptdir) + 1; */
                /*         oibl_libs7 = calloc(len, 1); */
                /*         strlcpy(oibl_libs7, scriptdir, len); */
                /*     } */
                /*     continue; */
                /* } */

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

    /* log_debug("tmp_load_path: %s", TO_STR(tmp_load_path)); */
    /* tmp_load_path = s7_cons(s7, */
    /*                         s7_make_string(s7, oibl_libs7), */
    /*                         tmp_load_path); */
    /* log_debug("2 tmp_load_path: %s", TO_STR(tmp_load_path)); */

    /* now put default "." on top of tmp stack */
    /* s7_pointer loadp = s7_load_path(s7); */
    /* tmp_load_path = s7_append(s7, loadp, tmp_load_path); */
    /* log_debug("lp: %s", s7_object_to_c_string(s7, tmp_load_path)); */

    /* replace *load-path* with our shiny new stack */
    s7_define_variable(s7, "*load-path*", tmp_load_path);
}

LOCAL void _config_s7_load_path_proj(void)
{
    char *project_script_dir = PROJ_LIBS7;

    UT_string *proj_script_dir;
    utstring_new(proj_script_dir);
    utstring_printf(proj_script_dir, "%s/%s",
                    utstring_body(ws_root),
                    PROJ_LIBS7);
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
}

LOCAL void _config_user_load_path(void)
{
    char *_user_script_dir = HOME_LIBS7;
    UT_string *user_script_dir;

    utstring_new(user_script_dir);
    utstring_printf(user_script_dir, "%s/%s",
                    homedir, _user_script_dir);

    rc = access(utstring_body(user_script_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Not found: user script dir: %s",
                     utstring_body(user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(user_script_dir));
        s7_add_to_load_path(s7, utstring_body(user_script_dir));
    }
}

LOCAL void _config_s7_load_path_xdg_home(void)
{
    if (trace)
        log_trace("_config_s7_load_path_xdg_home");

    UT_string *xdg_script_dir;

    utstring_new(xdg_script_dir);
    // note the leading _
    char *_xdg_data_home = getenv("XDG_DATA_HOME");

    utstring_new(xdg_data_home);
    if (_xdg_data_home == NULL) {
        _xdg_data_home = getenv("HOME");
        utstring_printf(xdg_data_home, "%s/%s",
                        _xdg_data_home, XDG_DATA_HOME_SFX);
    } else {
        utstring_printf(xdg_data_home, "%s", _xdg_data_home);
    }
    log_trace("xdg_data_home %s", utstring_body(xdg_data_home));

    /* start with lib/libc_s7.o, oibl dlopens it */

    // instead of adding another dir with only one file to the load-path,
    // we store libc_s7.o in .local/share/libs7, which is already on the path

    /* utstring_printf(xdg_script_dir, */
    /*                 "%s/.local/share/libs7/lib", */
    /*                 utstring_body(xdg_data_home)); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                   utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    utstring_renew(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/libs7/s7",
                    utstring_body(xdg_data_home));
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
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/libs7/dune"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/libs7/meta"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/libs7/opam"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    utstring_renew(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/libs7",
                    utstring_body(xdg_data_home));
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
}

LOCAL void _config_s7_load_path_xdg_sys(void)
{
    UT_string *xdg_script_dir;

    /* system libs7 script dirs:
       $XDG_DATA_DIRS/libs7
    */
    char *xdg_data_dirs = getenv("XDG_DATA_DIRS");
    if (xdg_data_dirs == NULL) {
        xdg_data_dirs = XDG_DATA_DIRS;
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

}

EXPORT void set_load_path(void) // char *scriptfile)
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
        if (verbose) log_info("Configuring for non-bazel env.");
        _config_s7_load_path_xdg_home();
        /* _config_s7_load_path_xdg_sys(); */
        _config_user_load_path();
    } else {
        if (verbose) log_info("Configuring for bazel env.");
        /* Running under bazel: do NOT put XDG dirs in load-path. This
           ensures a pristine runtime env. The user can always add
           directories to load-path. The only exception is the
           project-local script directory in <projroot>/.oibl . */
        s7_pointer lp = s7_load_path(s7);
        log_debug("1 *LOAD-PATH*: %s", TO_STR(lp));
        _config_s7_load_path_bazel_runfiles(manifest);
        lp = s7_load_path(s7);
        log_debug("2 *LOAD-PATH*: %s", TO_STR(lp));
    }
    _config_s7_load_path_proj();

    s7_add_to_load_path(s7, ".");

    /* s7_pointer lp = s7_load_path(s7); */
    /* s7_pointer new_lp = s7_call(s7, del, */
    /*                             s7_list(s7, 2, pred, lp)); */

    /* log_debug("new lp: %s", s7_object_to_c_string(s7, new_lp)); */


    if (debug) {
        s7_pointer lp = s7_load_path(s7);
        log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp));
    }
}

/* void libc_s7_init(s7_scheme *sc); */

EXPORT void s7_configure(void)
{
    s7 = s7_init();

    /* trap error messages */
    error_config();

    s7_define_safe_function(s7, "dune-load", g_dune_load,
                            0, 2, 0,
                                 /* DUNE_LOAD_FORMAL_PARAMS, */
                            DUNE_LOAD_HELP);

    set_load_path(); //callback_script_file);
    /* s7_load(s7, "dune.scm"); */
    /* s7_load(s7, "alist.scm"); */
}

EXPORT void s7_shutdown(s7_scheme *s7)
{
    close_error_config();
    s7_quit(s7);
}
