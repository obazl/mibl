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

static int level = 0;
static int spfactor = 4;
static char *sp = " ";

#if defined(DEBUG_TRACE)
static int indent = 2;
static int delta = 2;
#endif

bool stdlib_root = false;

/* char *buildfile_prefix = "@//" HERE_OBAZL_ROOT "/buildfiles"; */
/* was: "@//.opam.d/buildfiles"; */

long *KPM_TABLE;

FILE *opam_resolver;

extern UT_string *repo_name;

/* FIXME: same in mibl/error_handler.c */
void emit_opam_pkg_bindir(char *pkg) // UT_string *dune_pkg_file)
                          /* char *switch_lib, */
                          /* char *pkg, */
                          /* char *obazl, */
                          /* bool emitted_bootstrapper) */
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("emit_opam_pkg_bindir");
#endif

    UT_string *outpath;
    utstring_new(outpath);
    UT_string *opam_bin;
    utstring_new(opam_bin);
    /* s7_pointer iter, binfile; */

    /* read dune-package file. if it exports executables:
       1. write bin/BUILD.bazel with a rule for each
       2. symlink from opam switch
     */

    utstring_renew(dune_pkg_file);
    utstring_printf(dune_pkg_file, "%s/%s/dune-package",
                    utstring_body(opam_switch_lib), /* global */
                    pkg);

    /* s7_pointer executables = get_pkg_executables(dune_pkg_file); */
    UT_array *executables = get_pkg_executables(dune_pkg_file);

    if (utarray_len(executables) == 0) return;

    /* if executables not null:
       1. create 'bin' subdir of pkg
       2. add WORKSPACE.bazel to <pkg>/bin
       3. symlink executables to <pkg>/bin
       4. add BUILD.bazel with exports_files for linked executables
     */

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
    log_debug("fopening: %s", utstring_body(outpath));

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
    fprintf(ostream, "## Z generated file - DO NOT EDIT\n");

    fprintf(ostream, "exports_files([");

    /* For each executable, create symlink and exports_files entry */
    char **p = NULL;
    while ( (p=(char**)utarray_next(executables,p))) {
        log_debug("bin: %s",*p);
        fprintf(ostream, "\"%s\",", *p);
        /* symlink */
        utstring_renew(opam_bin);
        utstring_printf(opam_bin, "%s/%s",
                        utstring_body(opam_switch_bin),
                        *p);
        log_debug("SYMLINK SRC: %s", utstring_body(opam_bin));
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/bin/%s",
                        utstring_body(opam_coswitch_lib),
                        pkg,
                        *p); // TO_STR(binfile));
        log_debug("SYMLINK DST: %s", utstring_body(outpath));
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

        /*     iter = s7_make_iterator(s7, executables); */
    /*     //gc_loc = s7_gc_protect(s7, iter); */

    /*     if (!s7_is_iterator(iter)) */
    /*         fprintf(stderr, "%d: %s is not an iterator\n", */
    /*                 __LINE__, TO_STR(iter)); */
    /*     if (s7_iterator_is_at_end(s7, iter)) */
    /*         fprintf(stderr, "%d: %s is prematurely done\n", */
    /*                 __LINE__, TO_STR(iter)); */
    /*     while (true) { */
    /*         binfile = s7_iterate(s7, iter); */
    /*         if (s7_iterator_is_at_end(s7, iter)) break; */
    /*         printf("\tbin: %s\n", TO_STR(binfile)); */
    /*         utstring_renew(opam_bin); */
    /*         utstring_printf(opam_bin, "%s/%s", */
    /*                         utstring_body(opam_switch_bin), */
    /*                         TO_STR(binfile)); */

    /*         utstring_renew(outpath); */
    /*         utstring_printf(outpath, "%s/%s/bin/%s", */
    /*                         obazl, pkg, TO_STR(binfile)); */
    /*         rc = symlink(utstring_body(opam_bin), */
    /*                      utstring_body(outpath)); */
    /*         if (rc != 0) { */
    /*             if (errno != EEXIST) { */
    /*                 perror(NULL); */
    /*                 fprintf(stderr, "exiting\n"); */
    /*                 exit(EXIT_FAILURE); */
    /*             } */
    /*         } */
    /*         if (!emitted_bootstrapper) */
    /*             emit_local_repo_decl(bootstrap_FILE, pkg); */

    /*         fprintf(ostream, "exports_files([\"%s\"])\n", TO_STR(binfile)); */
    /*         fprintf(ostream, "## src: %s\n", utstring_body(opam_bin)); */
    /*         fprintf(ostream, "## dst: %s\n", utstring_body(outpath)); */

    /*     } */
    /*     /\* s7_gc_unprotect_at(s7, gc_loc); *\/ */

    fclose(ostream);
}

UT_string *dune_pkg_file;

/* pkg always relative to (global) opam_switch_lib */
EXPORT void emit_pkg_bindir(char *pkg)
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
