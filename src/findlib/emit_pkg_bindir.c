/* #include <assert.h> */
#include <errno.h>
/* #include <dirent.h> */
/* #include <fnmatch.h> */
/* #include <libgen.h> */
/* #include <regex.h> */

/* #if EXPORT_INTERFACE */
/* #include <stdio.h> */
/* #endif */

/* #ifdef __linux__ */
/* #include <linux/limits.h> */
/* #else */
/* #include <limits.h> */
/* #endif */
/* #include <string.h> */
/* #include <sys/stat.h> */
#include <unistd.h>             /* access */

/* #if EXPORT_INTERFACE */
/* #include "s7.h" */
/* #endif */

/* #include "utarray.h" */
/* #include "utstring.h" */

/* #include "log.h" */

/* #include "libdune.h" */
#include "emit_pkg_bindir.h"

extern UT_string *opam_switch_lib;

/* **************************************************************** */
/* s7_scheme *s7;                  /\* GLOBAL s7 *\/ */
/* /\* const char *errmsg = NULL; *\/ */
/* #define TO_STR(x) s7_object_to_c_string(s7, x) */

/* static int level = 0; */
extern int spfactor;
extern char *sp;

#if defined(DEBUG_TRACE)
extern int indent;
extern int delta;
#endif

bool stdlib_root = false;

/* char *buildfile_prefix = "@//" HERE_OBAZL_ROOT "/buildfiles"; */
/* was: "@//.opam.d/buildfiles"; */

long *KPM_TABLE;

FILE *opam_resolver;

extern UT_string *repo_name;

/* FIXME: same in mibl/error_handler.c */
void emit_opam_pkg_bindir(const char *pkg) // UT_string *dune_pkg_file)
                          /* char *switch_lib, */
                          /* char *pkg, */
                          /* char *obazl, */
                          /* bool emitted_bootstrapper) */
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("emit_opam_pkg_bindir");
#endif

    /* read dune-package file. if it exports executables:
       1. write bin/BUILD.bazel with a rule for each
       2. symlink from opam switch
     */

    utstring_renew(dune_pkg_file);
    utstring_printf(dune_pkg_file, "%s/%s/dune-package",
                    utstring_body(opam_switch_lib), /* global */
                    pkg);

    void *stanzas = read_dune_package(dune_pkg_file);

    /* s7_pointer executables = get_pkg_executables(dune_pkg_file); */
    UT_array *executables = get_pkg_executables(stanzas); // dune_pkg_file);

    if (utarray_len(executables) == 0) goto stublibs;

    /* if executables not null:
       1. create 'bin' subdir of pkg
       2. add WORKSPACE.bazel to <pkg>/bin
       3. symlink executables to <pkg>/bin
       4. add BUILD.bazel with exports_files for linked executables
     */

    UT_string *outpath;
    utstring_new(outpath);
    UT_string *opam_bin;
    utstring_new(opam_bin);
    /* s7_pointer iter, binfile; */

    /* for most pkgs, WORKSPACE.bazel is already written,
       but for some containing only executables (e.g. menhir),
       we need to write it now
    */
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/WORKSPACE.bazel",
                    utstring_body(opam_coswitch_lib),
                    pkg);
#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("checking ws: %s", utstring_body(outpath));
#endif
    if (access(utstring_body(outpath), F_OK) != 0) {
#if defined(DEBUG_TRACE)
        log_debug("creating %s\n", utstring_body(outpath));
#endif
        /* if obazl/pkg not exist, create it with WORKSPACE */
        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s", */
        /*                 utstring_body(opam_coswitch_lib), */
        /*                 pkg); */
        /* errno = 0; */
        /* if (access(utstring_body(outpath), F_OK) != 0) { */
        /*     utstring_printf(outpath, "/%s", "WORKSPACE.bazel"); */
        emit_workspace_file(outpath, pkg);
    }
    errno = 0;
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/bin",
                    utstring_body(opam_coswitch_lib),
                    //bws_root, /* obazl, */
                    pkg);
    mkdir_r(utstring_body(outpath));

    /* create <pkg>/bin/BUILD.bazel */
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/bin/BUILD.bazel",
                    utstring_body(opam_coswitch_lib),
                    pkg);
    /* rc = access(utstring_body(build_bazel_file), F_OK); */
#if defined(DEBUG_TRACE)
    log_debug("fopening: %s", utstring_body(outpath));
#endif

    FILE *ostream;
    ostream = fopen(utstring_body(outpath), "w");
    if (ostream == NULL) {
        printf(RED "ERROR" CRESET "fopen failure for %s", utstring_body(outpath));
        log_error("fopen failure for %s", utstring_body(outpath));
        perror(utstring_body(outpath));
        log_error("Value of errno: %d", errnum);
        log_error("fopen error %s", strerror( errnum ));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n");

    fprintf(ostream, "exports_files([");

    /* For each executable, create symlink and exports_files entry */
    char **p = NULL;
    while ( (p=(char**)utarray_next(executables,p))) {
#if defined(DEBUG_TRACE)
        log_debug("bin: %s",*p);
#endif
        fprintf(ostream, "\"%s\",", *p);
        /* symlink */
        utstring_renew(opam_bin);
        utstring_printf(opam_bin, "%s/%s",
                        utstring_body(opam_switch_bin),
                        *p);
#if defined(DEBUG_TRACE)
        log_debug("SYMLINK SRC: %s", utstring_body(opam_bin));
#endif
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/bin/%s",
                        utstring_body(opam_coswitch_lib),
                        pkg,
                        *p); // TO_STR(binfile));
#if defined(DEBUG_TRACE)
        log_debug("SYMLINK DST: %s", utstring_body(outpath));
#endif
        rc = symlink(utstring_body(opam_bin),
                     utstring_body(outpath));
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(NULL);
                fprintf(stderr, "exiting\n");
                exit(EXIT_FAILURE);
            }
        }
    }
    fprintf(ostream, "])\n");
    fclose(ostream);

    if (verbose) {
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/bin",
                        utstring_body(opam_coswitch_lib),
                        pkg);

        log_info("Created %s containing symlinked pkg executables",
                 utstring_body(outpath));
    }


 stublibs: ;
    /* opam dumps all stublibs ('dllfoo.so') in lib/stublibs; they are
       not found in the pkg's lib subdir. But the package's
       dune-package file lists them, so we read that and then symlink
       them from lib/stublibs to lib/<pkg>/stublibs.
     */

    UT_array *stublibs = get_pkg_stublibs((char*)pkg, stanzas);
    if (utarray_len(stublibs) == 0) goto exit;

    UT_string *opam_stublib;
    utstring_new(opam_stublib);

    /* s7_pointer iter, binfile; */

    /* for most pkgs, WORKSPACE.bazel is already written,
       but for some containing only stublibs (e.g. menhir),
       we need to write it now
    */
    utstring_new(outpath);
    utstring_printf(outpath, "%s/%s/WORKSPACE.bazel",
                    utstring_body(opam_coswitch_lib),
                    pkg);

#if defined(DEBUG_TRACE)
    if (debug)
        log_debug("checking ws: %s", utstring_body(outpath));
#endif
    if (access(utstring_body(outpath), F_OK) != 0) {
#if defined(DEBUG_TRACE)
        log_debug("creating %s\n", utstring_body(outpath));
#endif
        /* if obazl/pkg not exist, create it with WORKSPACE */
        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s", */
        /*                 utstring_body(opam_coswitch_lib), */
        /*                 pkg); */
        /* errno = 0; */
        /* if (access(utstring_body(outpath), F_OK) != 0) { */
        /*     utstring_printf(outpath, "/%s", "WORKSPACE.bazel"); */
        emit_workspace_file(outpath, pkg);
    }
    errno = 0;
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/stublibs",
                    utstring_body(opam_coswitch_lib),
                    //bws_root, /* obazl, */
                    pkg);
    mkdir_r(utstring_body(outpath));

    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/stublibs/BUILD.bazel",
                    utstring_body(opam_coswitch_lib),
                    pkg);
    /* rc = access(utstring_body(build_bazel_file), F_OK); */
#if defined(DEBUG_TRACE)
    log_debug("fopening: %s", utstring_body(outpath));
#endif

    /* FILE *ostream; */
    ostream = fopen(utstring_body(outpath), "w");
    if (ostream == NULL) {
        printf(RED "ERROR" CRESET "fopen failure for %s", utstring_body(outpath));
        log_error("fopen failure for %s", utstring_body(outpath));
        perror(utstring_body(outpath));
        log_error("Value of errno: %d", errnum);
        log_error("fopen error %s", strerror( errnum ));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n");

    fprintf(ostream, "exports_files([");

    /* For each stublib, create symlink and exports_files entry */
    p = NULL;
    while ( (p=(char**)utarray_next(stublibs,p))) {
#if defined(DEBUG_TRACE)
        log_debug("stublib: %s",*p);
#endif
        fprintf(ostream, "\"%s\",", *p);
        /* symlink */
        utstring_renew(opam_stublib);
        utstring_printf(opam_stublib, "%s/stublibs/%s",
                        utstring_body(opam_switch_lib),
                        *p);
#if defined(DEBUG_TRACE)
        log_debug("SYMLINK SRC: %s", utstring_body(opam_stublib));
#endif
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/stublibs/%s",
                        utstring_body(opam_coswitch_lib),
                        pkg,
                        *p); // TO_STR(binfile));
#if defined(DEBUG_TRACE)
        log_debug("SYMLINK DST: %s", utstring_body(outpath));
#endif
        rc = symlink(utstring_body(opam_stublib),
                     utstring_body(outpath));
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(NULL);
                fprintf(stderr, "exiting\n");
                exit(EXIT_FAILURE);
            }
        }
    }
    fprintf(ostream, "])\n");
    fclose(ostream);

    if (verbose) {
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/stublibs",
                        utstring_body(opam_coswitch_lib),
                        pkg);

        log_info("Created %s containing symlinked stublibs",
                 utstring_body(outpath));
    }

 exit: ;
    /* utstring_free(outpath); */
#if defined(DEBUG_TRACE)
    printf("exiting\n");
#endif
}

UT_string *dune_pkg_file;

/* pkg always relative to (global) opam_switch_lib */
EXPORT void emit_pkg_bindir(const char *pkg)
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("emit_pkg_bindir");
#endif

    utstring_renew(dune_pkg_file);
    utstring_printf(dune_pkg_file, "%s/%s/dune-package",
                    utstring_body(opam_switch_lib), /* global */
                    pkg);

#if defined(DEBUG_TRACE)
    log_debug("CHECKING DUNE-PACKAGE: %s\n", utstring_body(dune_pkg_file));
#endif
    if (access(utstring_body(dune_pkg_file), F_OK) == 0) {
        emit_opam_pkg_bindir(pkg); // dune_pkg_file);
                             /* switch_lib, */
                             /* pkgdir, */
                             /* obazl_opam_root, */
                             /* emitted_bootstrapper); */
    }
}
