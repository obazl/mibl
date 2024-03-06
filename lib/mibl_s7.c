#include <libgen.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <unistd.h>

/* #include "ini.h" */
#include "liblogc.h"
#include "utstring.h"
#include "utarray.h"
#include "xdgc.h"

#include "s7.h"
#include "mibl_s7.h"

extern const UT_icd ut_str_icd;

extern bool bzl_mode;
extern int  verbosity;

#if defined(PROFILE_fastbuild)
#define TRACE_FLAG mibl_trace
extern bool    TRACE_FLAG;
#define DEBUG_LEVEL mibl_debug
extern int     DEBUG_LEVEL;
#define S7_DEBUG_LEVEL libs7_debug
extern int libs7_debug;
extern int s7plugin_debug;

extern bool mibl_debug_runfiles;
#endif

/* s7_scheme *s7; */

UT_string *setter;

#define MIBL    "mibl"
#define MIBL_S7 MIBL "/s7"
/* #define OIBL   "mibl" */

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

/* #define XDG_DATA_HOME_SFX ".local/share" */
/* extern UT_string *xdg_data_home; */

/* #define XDG_CONFIG_HOME_SFX ".config" */

/* /\* state home for history files, current state to be reused on restart *\/ */
/* #define XDG_STATE_HOME_SFX ".local/state" */

/* #define XDG_BIN_HOME_SFX   ".local/bin" */

/* #define XDG_CACHE_HOME_SFX ".cache" */

/* /\* sockets, named pipes, etc. *\/ */
/* /\* #define XDG_RUNTIME_DIR *\/ */

/* in addition we define a project-local directory for scm scripts */
#define PROJ_MIBL ".mibl"

/* preference-ordered base directories in addition to XDG_DATA_HOME */
/* #define XDG_DATA_DIRS   "/usr/local/share/:/usr/share/" */
/* #define XDG_CONFIG_DIRS "/etc/xdg" */


/* load-path script directories: sys, user, proj, in order
   sys scripts:
       run under `bazel run`: dir in runfiles containing callback script
           @camlark//scm/s7, @camlark//scm
       run directly: XDG_DATA_DIRS default: /usr/local/share/:/usr/share/
           XDG_DATA_DIRS/mibl
           XDG_DATA_DIRS/mibl/s7
   user scripts:
       ($HOME)/.config/mibl
       $XDG_DATA_HOME default: $HOME/.local/share
           XDG_DATA_HOME/mibl/user
   proj scripts:
       .mibl

 */

/* mibl load path

   bazel env: distro srcs already in runfiles

   standalone mibl: distro scm srcs in /usr/local/share/mibl

   user mibl srcs: always in XDG_DATA_HOME/obazl/mibl

 */

/* setting *load-path* in bazel env: two strategies
   1. generic: iterate over runfiles, add each dir containing .scm files
   2. specific: we know where the scm dirs are, just hardcode them

   this is not a general purpose routine for use with any runfiles, so
   we use strategy 2, listing runfiles dirs that should be available
   when run under bazel run or bazel test.

   the problem with this is layering. mibl uses one known set of scm
   dirs, which is fine to hardcode here, since this is mibl. but
   tools_obazl/convert adds its own, and other apps will have their
   own. which are always listed in the runfiles. so we must deal with
   runfiles like it or not.

*/
/*
 */
char *scm_runfiles_dirs[] = {
    /* this seems to work when pgm is run from mibl repo or as external */
    "../mibl/scm/findlib",
    "../mibl/scm/opam",
    "../mibl/scm/dune",
    "../mibl/scm/mibl",
    "../mibl/scm",

    "../libs7/repl",
    // libs7/scm already in runfiles, since libmibl depends on libs7
    NULL /* do not remove terminating null */
};
char **scm_dir;

LOCAL void _config_s7_load_path_bazel_env(s7_scheme *s7)
{
    TRACE_ENTRY;
    /* s7_pointer tmp_load_path = s7_list(s7, 0); */
#ifdef LOCAL_REPO
    LOG_DEBUG(0, "bazel local repo: '%s'", LOCAL_REPO);
#endif
    scm_dir = scm_runfiles_dirs;
    char *scmdir;
    while (*scm_dir) {
#if defined(PROFILE_fastbuild)
        if (mibl_debug_runfiles)
            log_debug("runfile: %s", *scm_dir);
#endif
        scmdir = realpath(*scm_dir, NULL);
        /* if (scmdir == NULL) { */
        /*    log_error("realpath fail: runfile not found: %s", *scm_dir); */
        /*     exit(EXIT_FAILURE); */
        /* } */
#if defined(PROFILE_fastbuild)
        if (mibl_debug_runfiles)
            log_debug("runfile realpath: %s", scmdir);
#endif
        s7_add_to_load_path(s7, scmdir);
        free(scmdir);
        (void)*scm_dir++;
    }
    char *mibl_runfiles;
    /* log_debug("mibl BAZEL_CURRENT_REPOSITORY: '%s'", BAZEL_CURRENT_REPOSITORY); */
    /* log_debug("PWD: '%s'", getcwd(NULL,0)); */
    if (strlen(LOCAL_REPO) == 0) {
        mibl_runfiles = realpath("scm", NULL);
    } else {
        mibl_runfiles = realpath("external/" LOCAL_REPO "/scm", NULL);
    }
    LOG_DEBUG(0, "MIBL runfiles: %s", mibl_runfiles);
    s7_add_to_load_path(s7, mibl_runfiles);

    s7_pointer lp = s7_load_path(s7);
    char *s = s7_object_to_c_string(s7, lp);
    LOG_DEBUG(0, "load-path: %s", s);
    free(s);

    //FIXME: uas s7_add_to_load_path!!!
    /* s7_define_variable(s7, "*load-path*", tmp_load_path); */
}

/* we must read the manifest, because we do not know what clients may
   add as scm runtime deps. */
/* LOCAL __attribute__((unused)) void _config_s7_load_path_bazel_run_env(char *manifest) */
/* { */
/*     FILE * fp; */
/*     char * line = NULL; */
/*     size_t len = 0; */
/*     ssize_t read; */

/*     /\* put project-local .mibl on load-path if it exits. do */
/*        not create it. *\/ */

/*     /\* utstring_new(codept_args_file); *\/ */
/*     /\* utstring_printf(codept_args_file, "%s/%s", utstring_body(config_mibl), codept_args_filename); *\/ */

/*     /\* utstring_new(codept_deps_file); *\/ */
/*     /\* utstring_printf(codept_deps_file, "%s/%s", utstring_body(config_mibl), codept_deps_filename); *\/ */

/*     /\* */
/*       build scripts list their scm srcs in the 'data' attrib, which */
/*       puts them in the runfiles area. they are listed in MANIFEST, so */
/*       we need to parse it to find out which dirs we need to add to the */
/*       s7 load path. */
/*      *\/ */

/*     /\* bazel (sys) script dir *\/ */
/*     fp = fopen(manifest, "r"); */
/*     if (fp == NULL) { */
/*         log_error("fopen failure %s", manifest); */
/*         /\* exit(EXIT_FAILURE); *\/ */
/*     } */

/* #if defined(PROFILE_fastbuild) */
/*     if (mibl_debug) */
/*         log_debug("Reading MANIFEST"); */
/* #endif */

/*     /\* char *mibl_mibl = NULL; *\/ */

/*     s7_pointer load_dirs = s7_make_hash_table(s7, 5); */
/*     s7_pointer sdir; */

/*     /\* */
/*       We we need to get these paths from the MANIFEST since it has */
/*       absolute paths, and we could be running from any dir (when used */
/*       as a tool lib). */

/*       We build the path list by following the MANIFEST order for */
/*       libs7, but not mibl since the MANIFEST seems to put that */
/*       last. So we save the latter when we find it, then when done with */
/*       the MANIFEST we put the mibl/scm on top of the stack. Finally */
/*       we append our list to *load-path*, which puts "." on top. */
/*     *\/ */

/*     s7_pointer tmp_load_path = s7_list(s7, 0); */

/*     while ((read = getline(&line, &len, fp)) != -1) { */
/*         /\* log_debug("Retrieved line of length %zu:", read); *\/ */
/*         /\* log_debug("%s", line); *\/ */

/*         line[strcspn(line, "\n")] = '\0';    /\* trim trailing newline *\/ */

/*         /\* two tokens per line, first is path relative to exec dir, */
/*            second is corresponding absolute path *\/ */
/*         char *token, *sep = " "; */
/*         token = strtok((char*)line, sep); */
/*         if (token != NULL) { */
/*             token = strtok(NULL, sep); */
/*         } else { */
/*             /\* log_debug("skipping entry"); *\/ */
/*             continue; */
/*         } */

/*         if ( (strncmp(basename(token), */
/*                       "libc_s7.so", 10) == 0) ) { */
/* #if defined(PROFILE_fastbuild) */
/*             if (mibl_trace) */
/*                 log_info("FOUND LIBC_S7.SO: %s", token); */
/* #endif */

/*             tmp_load_path = */
/*                 s7_append(s7, tmp_load_path, */
/*                           s7_list(s7, 1, */
/*                                   s7_make_string(s7, */
/*                                                  dirname(token)))); */

/*             continue; */
/*         } */

/*         char *ext = strrchr(token, '.'); */
/*         if (ext != NULL) { */
/*             if (strncmp(ext, ".so", 3) == 0) { */
/*                 log_info("SHARED: %s", token); */
/*             } */

/*             if ( (strncmp(ext, ".scm", 4) == 0) && strlen(ext) == 4) { */
/*                 char *scriptdir = dirname(token); */
/*                 /\* log_info("SCRIPTDIR: %s", scriptdir); *\/ */

/*                 /\* char *substr = strstr(scriptdir, "mibl/scm"); *\/ */
/*                 /\* if (substr != NULL) { *\/ */
/*                 /\*     /\\* log_debug("FOUND mibl path: %s, %s", *\\/ *\/ */
/*                 /\*     /\\*           line, scriptdir); *\\/ *\/ */
/*                 /\*     if (mibl_mibl == NULL) { *\/ */
/*                 /\*         int len = strlen(scriptdir) + 1; *\/ */
/*                 /\*         mibl_mibl = calloc(len, 1); *\/ */
/*                 /\*         strlcpy(mibl_mibl, scriptdir, len); *\/ */
/*                 /\*     } *\/ */
/*                 /\*     continue; *\/ */
/*                 /\* } *\/ */

/*                 sdir = s7_make_string(s7, scriptdir); */
/*                 s7_pointer r = s7_hash_table_ref(s7, load_dirs, sdir); */
/*                 if (r == s7_f(s7)) { */
/*                     // add to hash to ensure uniqueness */
/*                     /\* log_debug("adding to hash"); *\/ */
/*                     s7_hash_table_set(s7, load_dirs, */
/*                                       sdir, s7_t(s7)); */

/*                     tmp_load_path = s7_append(s7, tmp_load_path, */
/*                                     s7_list(s7, 1, */
/*                                             s7_make_string(s7, scriptdir))); */
/*                 } */
/*             } */
/*         } */
/*     } */
/*     fclose(fp); */

/*     /\* log_debug("tmp_load_path: %s", TO_STR(tmp_load_path)); *\/ */
/*     /\* tmp_load_path = s7_cons(s7, *\/ */
/*     /\*                         s7_make_string(s7, mibl_mibl), *\/ */
/*     /\*                         tmp_load_path); *\/ */
/*     /\* log_debug("2 tmp_load_path: %s", TO_STR(tmp_load_path)); *\/ */

/*     /\* now put default "." on top of tmp stack *\/ */
/*     /\* s7_pointer loadp = s7_load_path(s7); *\/ */
/*     /\* tmp_load_path = s7_append(s7, loadp, tmp_load_path); *\/ */
/*     /\* log_debug("lp: %s", s7_object_to_c_string(s7, tmp_load_path)); *\/ */

/*     /\* replace *load-path* with our shiny new stack *\/ */
/*     s7_define_variable(s7, "*load-path*", tmp_load_path); */
/* } */

LOCAL void _config_s7_load_path_rootws(s7_scheme *s7)
{
    /* char *project_script_dir = PROJ_MIBL; */

    UT_string *proj_script_dir;
    utstring_new(proj_script_dir);
    utstring_printf(proj_script_dir, "%s/%s",
                    /* FIXME: relative to base wsroot, not ews */
                    ews_root, PROJ_MIBL);
    rc = access(utstring_body(proj_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_warn("Not found: project script dir %s not found",
                     utstring_body(proj_script_dir));
    } else {
        /* if (verbose) */
        /*     log_debug("adding to *load-path*: %s", */
        /*              utstring_body(proj_script_dir)); */
        s7_add_to_load_path(s7, utstring_body(proj_script_dir));
    }
}

LOCAL void _config_user_load_path(s7_scheme *s7)
{
    char *_user_script_dir = HOME_MIBL; //FIXME: XDG_DATA_HOME/obazl/mibl
    UT_string *user_script_dir;
    char *homedir = getenv("HOME");

    utstring_new(user_script_dir);
    utstring_printf(user_script_dir, "%s/%s",
                    homedir, _user_script_dir);

    rc = access(utstring_body(user_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: user script dir: %s",
                     utstring_body(user_script_dir));
    } else {
        if (verbose)
            log_debug("adding to *load-path*: %s",
                     utstring_body(user_script_dir));
        s7_add_to_load_path(s7, utstring_body(user_script_dir));
    }
}

LOCAL void _config_s7_load_path_xdg_home(s7_scheme *s7)
{
    TRACE_ENTRY;

    UT_string *xdg_script_dir;

    utstring_new(xdg_script_dir);
    // note the leading _
    /* char *_xdg_data_home = xdg_data_home(); */
        /* getenv("XDG_DATA_HOME"); */
    /* log_debug("XDG_DATA_HOME: %s", _xdg_data_home); */

    /* utstring_new(xdg_data_home); */
    /* utstring_printf(xdg_data_home, "%s", xdg_data_home()); */
    /* if (_xdg_data_home == NULL) { */
    /*     // WARNING: in bazel test env HOME is unset */
    /*     // "Paths starting with /home may not be available. Tests should not access any such paths." */
    /*     _xdg_data_home = getenv("HOME"); */
    /*     /\* log_debug("BAZEL_TEST: %d", getenv("BAZEL_TEST")); *\/ */
    /*     /\* log_debug("HOME XDG_DATA_HOME: %s", _xdg_data_home); *\/ */
    /*     utstring_printf(xdg_data_home, "%s/%s", */
    /*                     _xdg_data_home, XDG_DATA_HOME_SFX); */
    /* } else { */
    /*     log_debug("BBBB"); */
    /*     utstring_printf(xdg_data_home, "%s", _xdg_data_home); */
    /* } */
    if (verbose)
        log_trace("xdg_data_home: %s", xdg_data_home());

    /* start with lib/libc_s7.so, mibl dlopens it */

    // instead of adding another dir with only one file to the load-path,
    // we store libc_s7.o in .local/share/mibl, which is already on the path

    /* utstring_printf(xdg_script_dir, */
    /*                 "%s/.local/share/mibl/lib", */
    /*                 utstring_body(xdg_data_home)); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                   utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/mibl/s7", */
    /*                 utstring_body(xdg_data_home)); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/dune"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/meta"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    /* utstring_renew(xdg_script_dir); */
    /* utstring_printf(xdg_script_dir, "%s/%s", */
    /*                 utstring_body(xdg_data_home), "/mibl/opam"); */
    /* rc = access(utstring_body(xdg_script_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose) */
    /*         log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("adding to *load-path*: %s", */
    /*                  utstring_body(xdg_script_dir)); */
    /*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
    /* } */

    utstring_renew(xdg_script_dir);
    utstring_printf(xdg_script_dir, "%s/mibl/user",
                    xdg_data_home());
    rc = access(utstring_body(xdg_script_dir), R_OK);
    if (rc) {
        if (verbose)
            log_info("Not found: %s; creating.", utstring_body(xdg_script_dir));
        mkdir(utstring_body(xdg_script_dir), S_IRWXU | S_IRWXG | S_IRGRP );
    }
    /* else { */
        if (verbose)
            log_debug("adding to *load-path*: %s",
                      utstring_body(xdg_script_dir));
        s7_add_to_load_path(s7, utstring_body(xdg_script_dir));
    /* } */
}

/* LOCAL __attribute__((unused)) void _config_s7_load_path_xdg_sys(s7_scheme *s7) */
/* { */
/*     UT_string *xdg_script_dir; */

/*     /\* system mibl script dirs: */
/*        $XDG_DATA_DIRS/mibl */
/*     *\/ */
/*     char *xdg_data_dirs = getenv("XDG_DATA_DIRS"); */
/*     if (xdg_data_dirs == NULL) { */
/*         xdg_data_dirs = XDG_DATA_DIRS; */
/*     } */

/*     utstring_new(xdg_script_dir); */
/*     utstring_printf(xdg_script_dir, "%s/%s", */
/*                     xdg_data_dirs, "mibl"); */
/*     rc = access(utstring_body(xdg_script_dir), R_OK); */
/*     if (rc) { */
/*         if (verbose) */
/*             log_info("Not found: %s.", utstring_body(xdg_script_dir)); */
/*     } else { */
/*         s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); */
/*     } */

/*     /\* utstring_renew(xdg_script_dir); *\/ */
/*     /\* utstring_printf(xdg_script_dir, "%s/%s", *\/ */
/*     /\*                 xdg_data_dirs, "s7"); *\/ */
/*     /\* rc = access(utstring_body(xdg_script_dir), R_OK); *\/ */
/*     /\* if (rc) { *\/ */
/*     /\*     if (verbose) *\/ */
/*     /\*         log_info("Not found: obazl s7 system script dir at: %s", *\/ */
/*     /\*                  utstring_body(xdg_script_dir)); *\/ */
/*     /\* } else { *\/ */
/*     /\*     if (verbose) *\/ */
/*     /\*         log_debug("adding to *load-path*: %s", *\/ */
/*     /\*                  utstring_body(xdg_script_dir)); *\/ */
/*     /\*     s7_add_to_load_path(s7, utstring_body(xdg_script_dir)); *\/ */
/*     /\* } *\/ */

/* } */

LOCAL void _emit_mibl_file(s7_scheme *s7, char *stem)
{
    TRACE_ENTRY;

    UT_string *sexp;
    utstring_new(sexp);
        /* "    ((*libc* 'mkdir) (format #f \"~A/.mibl\" ws-path) " */
        /* "           (logior (*libc* 'S_IRWXU) " */
        /* "                   (*libc* 'S_IRGRP) " */
        /*             " (*libc* 'S_IXGRP) (*libc* 'S_IROTH)))" */

    utstring_printf(sexp,
        "(let* ((@ws (assoc-val :@ *mibl-project*)) "
        "       (ws-path (assoc-val :path (cdr @ws))) "
        "       (mibl-file (format #f \"~A/.mibl/%s.mibl\" ws-path)) "
        "       (s7-file (format #f \"~A/.mibl/%s.s7\" ws-path))) "
        "    (libc:mkdir (format #f \"~A/.mibl\" ws-path) "
        "           (logior libc:S_IRWXU "
        "                   libc:S_IRGRP "
        "                   libc:S_IXGRP "
        "                   libc:S_IROTH))"
        "    (let ((outp "
        "            (catch #t "
        "               (lambda () "
        "                 (open-output-file mibl-file)) "
        "               (lambda args "
        "                 (error 'OPEN_ERROR_EMIT "
        "                   (format #f \"OPEN ERROR: ~A\n\" mibl-file)))))) "
        "        (if (not *mibl-quiet*) (format #t \"~A: Emitting ~A\n\" (green \"INFO\") mibl-file)) "
        "        (mibl-pretty-print *mibl-project* outp) "
        "        (close-output-port outp)) "
        "    (let ((outp "
        "            (catch #t "
        "               (lambda () "
        "                 (open-output-file s7-file)) "
        "               (lambda args "
        "                 (error 'OPEN_ERROR_EMIT "
        "                   (format #f \"OPEN ERROR: ~A\n\" s7-file)))))) "
        "        (if (not *mibl-quiet*) (format #t \"~A: Emitting ~A\n\" (green \"INFO\") s7-file)) "
        "        (write (object->string *mibl-project* :readable) outp) "
        "        (close-output-port outp))) "
                    , stem, stem);

    /* log_debug("SEXP: %s", utstring_body(sexp)); */

    s7_pointer ws_path = s7_eval_c_string(s7, utstring_body(sexp));
    (void)ws_path;

    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));

    /* char *tostr; */
    /* tostr = TO_STR(ws_path); */
    /* log_debug("s: %s", tostr); */
    /* free(tostr); */

    return;

}

LOCAL void _mibl_s7_configure_x(s7_scheme *s7)
{
    TRACE_ENTRY;

    /* miblrc:  *mibl-dump-pkgs*, *mibl-scan-exclusions* */
    /* populate pkgs list, so scheme code can use it */
    char **p = NULL;
    s7_pointer _s7_pkgs = s7_nil(s7);
    while ( (p=(char**)utarray_next(mibl_config.pkgs, p))) {
#if defined(PROFILE_fastbuild)
        if (mibl_debug) printf("Adding to pkgs list: %s\n", *p);
#endif
        _s7_pkgs = s7_cons(s7, s7_make_string(s7, *p), _s7_pkgs);
    }
    s7_define_variable(s7, "*mibl-dump-pkgs*", _s7_pkgs);

    /* populate exclusions list, so scheme code can use it */
    p = NULL;
    s7_pointer _s7_exclusions = s7_nil(s7);
    while ( (p=(char**)utarray_next(mibl_config.exclude_dirs, p))) {
#if defined(PROFILE_fastbuild)
        if (mibl_debug_miblrc)
            log_debug("Adding to exlusions list: %s",*p);
#endif
        _s7_exclusions = s7_cons(s7, s7_make_string(s7, *p), _s7_exclusions);
    }
#if defined(PROFILE_fastbuild)
    if (mibl_debug)
        LOG_S7_DEBUG(0, "exclusions list", _s7_exclusions);
#endif
    s7_define_variable(s7, "*mibl-scan-exclusions*", _s7_exclusions);
}

/*
  FIXME: this is not mibl-specific?
 */
EXPORT void set_load_path(s7_scheme *s7) // char *scriptfile)
{
    /* char *_wd = getcwd(NULL, 0); */

#if defined(PROFILE_fastbuild)
    if (mibl_debug) {
        s7_pointer lp = s7_load_path(s7);
        LOG_S7_DEBUG(0, "*load-path*", lp);
    }
#endif

    /* FIXME: reliable way to detect if we're run by bazel */

    /* https://docs.bazel.build/versions/main/user-manual.html#run */

    /* bazel run is similar, but not identical, to directly invoking
       the binary built by Bazel and its behavior is different
       depending on whether the binary to be invoked is a test or not.
       When the binary is not a test, the current working directory
       will be the runfiles tree of the binary. When the binary is a
       test, the current working directory will be the exec root and a
       good-faith attempt is made to replicate the environment tests
       are usually run in.
  */
    /* BAZEL_TEST will be in the env when the target is a test rule,
       whether run under bazel test or bazel run. */

    char *manifest = "../MANIFEST";

    if (getenv("BAZEL_TEST")) {
#if defined(TRACING)
        if (verbose) log_info("Configuring s7 for bazel test env.");
#endif
        //FIXME: only way find dirs for the s7 *load-path* is to crawl
        //the runfiles dir looking for scm files.
        /* _config_s7_load_path_bazel_run_env(manifest); */
        _config_s7_load_path_bazel_env(s7);

#if defined(PROFILE_fastbuild)
        s7_pointer lp = s7_load_path(s7);
        (void)lp;
        if (mibl_debug) {
            LOG_S7_DEBUG(0, "2 *LOAD-PATH*", lp);
        }
#endif
    }
    else {
        rc = access(manifest, R_OK);

        if (rc) {
            if (verbose) log_info("Configuring for non-bazel env.");
            _config_s7_load_path_xdg_home(s7);
            /* _config_s7_load_path_xdg_sys(s7); */
            _config_user_load_path(s7);
        } else {
            if (verbose) log_info("Configuring for bazel env.");
            /* Running under bazel: do NOT put XDG dirs in load-path? This
               ensures a pristine runtime env. The user can always add
               directories to load-path. The only exception is the
               project-local script directory in <projroot>/.mibl . */
            #if defined(PROFILE_fastbuild)
                    if (mibl_debug) {
                        s7_pointer lp = s7_load_path(s7);
                        LOG_S7_DEBUG(0, "1 *LOAD-PATH*", lp);
                    }
            #endif

            /* _config_s7_load_path_bazel_run_env(manifest); */
            _config_s7_load_path_bazel_env(s7);
#if defined(PROFILE_fastbuild)
            if (mibl_debug) {
                s7_pointer lp = s7_load_path(s7);
                log_debug("3 *LOAD-PATH*", lp);
            }
#endif
        }
    }

    /* s7_pointer lp = s7_load_path(s7); */
    /* s7_pointer new_lp = s7_call(s7, del, */
    /*                             s7_list(s7, 2, pred, lp)); */

    /* log_debug("new lp: %s", s7_object_to_c_string(s7, new_lp)); */


    /* if (mibl_debug) { */
    /*     s7_pointer lp = s7_load_path(s7); */
    /*     log_debug("*load-path*: %s", s7_object_to_c_string(s7, lp)); */
    /* } */
}


/* **************************************************************** */
// to configure we need scm dirs for *load-path*, ws_root for traversal
// to run we need main_script
// q: script runs load-project; doesn't that handle ws root?
/* called by scripters, not coswitch */
void _mibl_s7_configure_paths(s7_scheme *s7,
                              char *scmdir, /*char *main_script,*/
                              char *ws_root)
{
    (void)ws_root;
    set_load_path(s7); //callback_script_file);

    // we need to do this before we chdir to repo root
    /* if (main_script) { */
    /*     UT_string *tmp; */
    /*     utstring_new(tmp); */
    /*     utstring_printf(tmp, "../%s", main_script); */
    /*     char *realmain = realpath(utstring_body(tmp), NULL); */
    /*     /\* log_debug("realmain %s", realmain); *\/ */

    /*     s7_pointer result = s7_add_to_load_path(s7, realmain); */
    /*     (void)result; */
    /*     /\* log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); *\/ */
    /* } */
    /* init_glob(s7); */

    /* s7_config_repl(s7); */
    /* s7_repl(s7); */

    if (scmdir) {
        char *tmpdir = realpath(scmdir, NULL);
        /* log_debug("tmpscm: %s", tmpdir); */
        s7_add_to_load_path(s7, tmpdir);
    }

    if ( !getenv("BAZEL_TEST") )
        _config_s7_load_path_xdg_home(s7);
    _config_s7_load_path_rootws(s7); /* always penultimate */
    /* s7_add_to_load_path(s7, "."); /\* always last *\/ */

    //TODO: what should be loaded by default and what left to user?
    if (!s7_load(s7, "libmibl.scm")) {
        log_error("Can't load libmibl.scm");
        /* exit(EXIT_FAILURE); */
    /* } else { */
    /*     log_info("loaded libmibl.scm"); */
    }
    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));

    /* if (!s7_load(s7, "mibl_pp.scm")) { */
    /*     log_error("Can't load mibl_pp.scm"); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    /* LOG_DEBUG(0, "loading alist.scm", ""); */
    /* if (!s7_load(s7, "alist.scm")) { */
    /*     log_error("Can't load alist.scm"); */
    /* } */
    init_scheme_fns(s7);       /* call _after_ loading dune.scm */

    /* chdir(rootws);            /\* always run from base ws root *\/ */

    /* if (verbose && verbosity > 1) { */
    /*     show_s7_config(); */
    /* } */

    s7_define_function(s7, "mibl-load-project", g_load_project,
                       0, 2, 0,
                       /* LOAD_DUNE_FORMAL_PARAMS, */
                       LOAD_DUNE_HELP);

    /* return s7; */
}

/* called by apps needing to run a script */
/* i.e. by convert but not coswitch */
EXPORT struct mibl_config_s *mibl_s7_init2(s7_scheme *s7,
                                           char *scm_dir,
                                           char *ws_root)
{
    TRACE_ENTRY;
    // scm_dir augments default *load-path*, which contains the mibl/scm dirs
#if defined(TRACING)
    /* if (mibl_trace) { */
    log_debug("mibl_s7_init2: scm dir: %s, wsroot: %s", scm_dir, ws_root);
#endif

    /* s7 = s7_configure(scm_dir, ws_root); //FIXME: ws_root not used by s7_configure? */

    // globals in config_bazel.c:
    // bzl_mode
    // mibl_runfiles_root, runtime_data_dir
    // obazl_ini_path
    // rootws, ews_root
    // traversal_root
    // build_ws_dir, build_wd, launch_dir
    // may call config_xdg_dirs()
    bazel_configure(ws_root);

    opam_configure(NULL);

    /* reads miblrc, sets struct mibl_config, may set s7 flags */
    mibl_configure();  // read miblrc; may call s7_set

    _mibl_s7_configure_x(s7);        /* just some s7_define_variable */

    _mibl_s7_configure_paths(s7, scm_dir, ws_root);

    /* always run from base ws root, set by bazel_configure */
    /* for test targets base == runfiles dir */
    /* for std targets, base == ws root (possibly set by -w (--workspace) */
    chdir(rootws);

    utstring_new(setter);

    if (verbose) {
        log_info("pwd: %s", getcwd(NULL,0));
#if defined(PROFILE_fastbuild)
        s7_pointer lp =s7_load_path(s7);
        LOG_S7_DEBUG(0, "*load-path*", lp);
#endif
    }

    //FIXME: move to mibl_s7.c?
    s7_define_function(s7, "mibl-load-project", g_load_project,
                       0, 2, 0,
                       /* LOAD_DUNE_FORMAL_PARAMS, */
                       LOAD_DUNE_HELP);

    return &mibl_config;
}

/* run a script (which may or may not run load-project) */
/* EXPORT void mibl_s7_run(s7_scheme *s7, char *main_script, char *ws) */

/* run mibl_expand.scm -main */
EXPORT void mibl_s7_expand(s7_scheme *s7, char *ws)
{
    TRACE_ENTRY;
#if defined(TRACING)
    /* log_trace("main script: %s, ws: %s", main_script, ws); */
    log_trace("cwd: %s", getcwd(NULL, 0));
#endif

    char *main_script = "mibl_expand.scm";

    /* if (verbose) { */
    /*     log_debug("mibl_run: %s, %s", main_script, ws); */
    /*     log_debug("mibl_run cwd: %s", getcwd(NULL, 0)); */
    /* } */

    /* mibl_s7_ingest(s7, ws); */

    /* **************************************************************** */
    /* parsetree processing: one of */
    /* a. mustache:render */
    /* b. mibl:expand */
    /* c. other scm sript? */

    /* or: pass arbitrary script and run -main. script decides which
       of a-c to run. */

    /* legacy: */
    /* now run the s7 script on the parsetree */
    /* s7_pointer lp = s7_load_path(s7); */
    /* LOG_S7_DEBUG(0, "*load-path*", lp); */
    /* if (main_script) { */
    /*     if (!s7_load(s7, main_script)) { */
    /*         log_error(RED "Could not load main_script '%s'; exiting\n", */
    /*                   main_script); */
    /*         fflush(NULL); */
    /*         exit(EXIT_FAILURE); */
    /*     } else { */
    /*         if (verbose && verbosity > 1) */
    /*             log_debug("loaded main_script: %s", main_script); */
    /*     } */
    /* } else { */
    LOG_INFO(2, GRN "INFO: " CRESET "main_script is NULL, defaulting to %s", main_script);
    if (!s7_load(s7, main_script)) {
        log_error(RED "Could not load main_script '%s'; exiting\n",
                  main_script);
        fflush(NULL);
        exit(EXIT_FAILURE);
    } else {
        if (verbose && verbosity > 1)
            log_debug("loaded main_script: %s", main_script);
    }
        /* exit(EXIT_FAILURE); */
        /* return; */
    /* } */

    s7_pointer _main = s7_name_to_value(s7, "-main");

    if (_main == s7_undefined(s7)) {
        log_error(RED "Could not find procedure -main; exiting\n");
        exit(EXIT_FAILURE);
    }

    s7_pointer _s7_ws;
    if (ws) {
        /* _s7_ws = s7_make_string(s7, ws); */
        _s7_ws = s7_nil(s7);
    } else {
        _s7_ws = s7_nil(s7);
    }

    s7_pointer _s7_args;

    /* if (rootpath) { */
    /*     _s7_args = s7_list(s7, 2, */
    /*                        s7_make_string(s7, rootpath), */
    /*                        _s7_ws); */
    /* } else { */
    _s7_args = s7_list(s7, 2,
                       s7_nil(s7),
                       _s7_ws);
    /* } */

#if defined(PROFILE_fastbuild)
    if (mibl_debug) LOG_S7_DEBUG(0, "s7 args", _s7_args);
#endif

    /* s7_gc_on(s7, s7_f(s7)); */


    /* s7_int main_gc_loc = s7_gc_protect(s7, _main); */

    if (verbose && verbosity > 2) {
        LOG_S7_DEBUG(0, "calling s7", _main);
    }
    if (verbose)
        log_info("workspace root: %s", ws);

    /* **************************************************************** */

    /* trap error messages */
    s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_port);

    /* log_debug("running -main from %s", getcwd(NULL, 0)); */
    /* this does the actual conversion: */
    s7_pointer result = s7_call(s7, _main, _s7_args);
    /* log_debug("ran -main"); */
    if (result == s7_undefined(s7)) {
        log_error("running -main failed");
        exit(EXIT_FAILURE); //FIXME: graceful exit
    }
    /* look for error messages */
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));

    /* if we got something, wrap it in "[]" */
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);

    if ((errmsg) && (*errmsg)) {
        fprintf(stdout, "[%s]\n", errmsg);
        return;
    }

    /* **************************************************************** */

    /* log_info("RESULT: %s\n", TO_STR(result)); */
    /* fprintf(stderr, "s7_gc_unprotect_at _main: %ld\n", (long)_main); */
    s7_gc_unprotect_at(s7, (s7_int)_main);

    errmsg = (char*)s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }

    /* now *mibl-project* contains the transformed parsetree */
    if (s7_name_to_value(s7, "*mibl-quiet*") == s7_f(s7)) {
        log_info("Parsetree (expanded):");
        mibl_show_parsetree(s7);
    }

    if (s7_name_to_value(s7, "*mibl-emit-mibl*") == s7_t(s7)) {
            /* log_debug("EMITTING PROJECT"); */
        _emit_mibl_file(s7, "PROJECT");
    }
}

EXPORT void mibl_s7_ingest(s7_scheme *s7, char *ws)
{
    TRACE_ENTRY;
#if defined(TRACING)
        log_trace("cwd: %s", getcwd(NULL, 0));
#endif

    /* if (verbose) { */
    /*     log_debug("mibl_run cwd: %s", getcwd(NULL, 0)); */
    /* } */

    if (s7_name_to_value(s7, "*mibl-dev-mode*") == s7_t(s7)) {
        if (s7_name_to_value(s7, "*mibl-report-parsetree*") == s7_t(s7)) {
            log_error("debug:report is incompatible with --dev");
            return;
        }
        if (s7_name_to_value(s7, "*mibl-emit-parsetree*") == s7_t(s7)) {
            log_error("--emit-parsetree is incompatible with --dev");
            return;
        }
    }

    if (s7_name_to_value(s7, "*mibl-report-parsetree*") == s7_t(s7)) {
        mibl_config.emit_parsetree = true;
        mibl_s7_set_flag(s7, "*mibl-emit-mibl*", true);
        mibl_s7_set_flag(s7, "*mibl-emit-s7*", true);
        /* mibl_s7_set_flag("*mibl-emit-result*", true); */
        log_debug("DEBUG REPORT");
    }

    if (s7_name_to_value(s7, "*mibl-show-config*") == s7_t(s7)) {
        show_bazel_config();
        show_mibl_config();
        show_s7_config(s7);
        return;
    }

    /* 1. if dev-mode, load PARSETREE.s7, else run mibl-load-project, producing parsetree */
    /* 2. IF user provided -main, run it */

    UT_string *sexp;
    utstring_new(sexp);
    if (ws) {
        if (s7_name_to_value(s7, "*mibl-dev-mode*") == s7_t(s7)) {
            log_debug(GRN "INFO: " CRESET
                      "dev mode, loading .mibl/PARSETREE.s7");
            utstring_printf(sexp, "%s",
                    "(define *mibl-project* "
                    "  (call-with-input-file \".mibl/PARSETREE.s7\" "
                    "    (lambda (p) "
                    "      (let* ((x (read p)) "
                    "             (y (eval (read (open-input-string x))))) "
                    "          y))))"
                    );
        } else {
            utstring_printf(sexp, "(mibl-load-project \"%s\")", ws);
        }
    } else {
        if (s7_name_to_value(s7, "*mibl-dev-mode*") == s7_t(s7)) {
            char *root = getcwd(NULL, 0);
            log_debug(GRN "INFO: " CRESET
                      "dev mode, loading %s/.mibl/PARSETREE.s7",
                      root);
            utstring_printf(sexp,
                    "(define *mibl-project* "
                    "  (call-with-input-file \".mibl/PARSETREE.s7\" "
                    "    (lambda (p) "
                    "      (let* ((x (read p)) "
                    "             (y (eval (read (open-input-string x))))) "
                    "          y))))"
                    );
        } else {
            utstring_printf(sexp, "(mibl-load-project)");
        }
    }
    /* if (true) { */
    /*     log_debug("CWD: %s", getcwd(NULL, 0)); */
    /*     log_debug("load sexp: %s", utstring_body(sexp)); */
    /* } */

    s7_pointer result;
    int gc_loc = -1;
    const char *errmsg = NULL;
    /* trap error messages */
    s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_port);

    result = s7_eval_c_string(s7, utstring_body(sexp));
    if (result == s7_undefined(s7)) {
        log_error("s7_eval_c_string failure");
        //FIXME: graceful exit
        exit(EXIT_FAILURE);
    }
    utstring_free(sexp);

    /* look for error messages */
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));

    /* if we got something, wrap it in "[]" */
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);

    if ((errmsg) && (*errmsg)) {
        fprintf(stdout, "[%s]\n", errmsg);
        return;
    }

    /* char *s = TO_STR(ptree); */
    /* log_debug("PTREE: '%s'", ptree); */
    /* free(s); */
    /* (void)ptree; */

    /* now we have the parsetree in *mibl-project* */

    /* if (s7_name_to_value(s7, "*mibl-mustachios*") == s7_t(s7)) { */
    /*     log_debug("MUSTACHIOS PARSETREE"); */
    /*     UT_string *sexp; */
    /*     utstring_new(sexp); */
    /*     utstring_printf(sexp, "(mustache:render template *mibl-project* 0)"); */
    /*     s7_pointer x = s7_eval_c_string(s7, utstring_body(sexp)); */
    /*     (void)x; */
    /*     s7_newline(s7,  s7_current_output_port(s7)); */
    /*     s7_flush_output_port(s7, s7_current_output_port(s7)); */
    /*     /\* char *s = TO_STR(ptree); *\/ */
    /*     /\* log_debug("%s", s); *\/ */
    /*     /\* free(s); *\/ */
    /*     return; */
    /* } */

    if (s7_name_to_value(s7, "*mibl-show-parsetree*") == s7_t(s7)) {
        log_info("Showing parsetree (raw):");
        mibl_show_parsetree(s7);
    }

    if (mibl_config.emit_parsetree) {
        if (s7_name_to_value(s7, "*mibl-dev-mode*") == s7_f(s7)) {
            log_debug("EMITTING PARSETREE");
            _emit_mibl_file(s7, "PARSETREE");
            if (mibl_config.halt_after_parsetree) {
                //FIXME: cleanup
                /* exit(EXIT_SUCCESS); */
                return;
            }
        } else {
            log_error("--emit-parsetree incompatible with --dev");
            return;
        }
    }
    if (s7_name_to_value(s7, "*mibl-quiet*") == s7_f(s7)) {
        log_info("Parsetree (raw):");
        mibl_show_parsetree(s7);
    }
    // TEST
    /* opam_fts("fmt"); */
}

void mibl_show_parsetree(s7_scheme *s7)
{
    TRACE_ENTRY;
    UT_string *sexp;
    utstring_new(sexp);
    utstring_printf(sexp, "(mibl-pretty-print *mibl-project*)");
    s7_pointer x = s7_eval_c_string(s7, utstring_body(sexp));
    (void)x;
    s7_newline(s7,  s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));
    /* char *s = TO_STR(ptree); */
    /* log_debug("%s", s); */
    /* free(s); */
    return;
}