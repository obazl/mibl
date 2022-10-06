#include <assert.h>
#include <errno.h>
#include <dirent.h>
#include <fnmatch.h>
#include <libgen.h>
/* #include <regex.h> */

#if EXPORT_INTERFACE
#include <stdio.h>
#endif

#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* #if EXPORT_INTERFACE */
#include "s7.h"
/* #endif */

#include "log.h"
#if EXPORT_INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "emit_build_bazel.h"

/* **************************************************************** */
s7_scheme *s7;                  /* GLOBAL s7 */
/* const char *errmsg = NULL; */
#define TO_STR(x) s7_object_to_c_string(s7, x)

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

UT_string *repo_name = NULL;

s7_pointer _get_executables(s7_pointer stanzas)
{
    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * sexp =
        "(let ((files (assoc 'files stanzas)))"
        "  (if files"
        "      (let ((bin (assoc 'bin (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer files = s7_eval_c_string_with_environment(s7, sexp, e);
    return files;
}

/* FIXME: same in mibl/error_handler.c */
s7_pointer read_dune_package(UT_string *dunefile_name)
{
    //FIXME: this duplicates the code in load_dune:_read_dunefile
    if (trace) printf("read_dune_package\n");

    char *dunestring = dunefile_to_string(dunefile_name);
    printf("readed str: %s\n", dunestring);

    /* stanza accumulator */
    s7_pointer stanzas = s7_list(s7, 0);

    s7_pointer sport = s7_open_input_string(s7, dunestring);

    if (!s7_is_input_port(s7, sport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            printf(RED "ERROR" CRESET "s7_open_input_string failed\n");
            log_error("[%s\n]", errmsg);
            s7_quit(s7);
            exit(EXIT_FAILURE);
        }
    }

    printf("s7 reading stanzas\n");

    /* read all stanzas in dunefile */
    while(true) {
        printf("iter\n");
        s7_pointer stanza = s7_read(s7, sport);
        /* FIXME: error checks */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* if ((errmsg) && (*errmsg)) { */
        /*     if (debug) log_error("[%s\n]", errmsg); */
        /*     s7_close_input_port(s7, sport); */
        /*     s7_quit(s7); */
        /*     exit(EXIT_FAILURE); */
        /*     break; */
        /* } */
        if (stanza == s7_eof_object(s7)) break;
        if (s7_is_null(s7,stanzas)) {
            stanzas = s7_list(s7, 1, stanza);
        } else{
            stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
        }
    }
    s7_close_input_port(s7, sport);
    /* s7_gc_unprotect_at(s7, baddot_gc_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    /* free(dunestring); */
    return stanzas;
}

EXPORT void emit_opam_pkg_bindir(UT_string *dune_pkg_file,
                                 char *switch_lib, char *pkg,
                                 char *obazl,
                                 bool emitted_bootstrapper)
{
    if (trace) {
        log_info("");
        printf("EMIT_OPAM_PKG_BINDIR %s/%s\n", obazl, pkg);
    }

    UT_string *outpath;
    utstring_new(outpath);
    UT_string *opam_bin;
    utstring_new(opam_bin);
    s7_pointer iter, binfile;

    /* read dune-package file. if it exports executables:
       1. write bin/BUILD.bazel with a rule for each
       2. symlink from opam switch
     */

    s7_pointer stanzas = read_dune_package(dune_pkg_file);

    s7_pointer executables = _get_executables(stanzas);
    if (s7_is_list(s7, executables)) {
        if (verbose) {
            printf(GRN "EXECUTABLES:" CRESET
                   " for %s: %s\n",
                   utstring_body(dune_pkg_file),
                   TO_STR(executables));
        }
            utstring_new(outpath);
            utstring_printf(outpath, "%s/%s/bin", obazl, pkg);
            /* printf("checking outdir: %s\n", utstring_body(outpath)); */

            if (access(utstring_body(outpath), F_OK) != 0) {
                printf("creating %s\n", utstring_body(outpath));
                /* if obazl/pkg not exist, create it with WORKSPACE */
                utstring_renew(outpath);
                utstring_printf(outpath, "%s/%s", obazl, pkg);
                errno = 0;
                if (access(utstring_body(outpath), F_OK) != 0) {
                    mkdir_r(utstring_body(outpath));
                    utstring_printf(outpath, "/%s", "WORKSPACE.bazel");
                    emit_workspace_file(outpath, pkg);
                }
                errno = 0;
                utstring_renew(outpath);
                utstring_printf(outpath, "%s/%s/bin", obazl, pkg);
                mkdir_r(utstring_body(outpath));
            }

            utstring_renew(outpath);
            utstring_printf(outpath, "%s/%s/bin/BUILD.bazel", obazl, pkg);
            /* rc = access(utstring_body(build_bazel_file), F_OK); */
            log_debug("fopening: %s\n", utstring_body(outpath));

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

            iter = s7_make_iterator(s7, executables);
            //gc_loc = s7_gc_protect(s7, iter);

            if (!s7_is_iterator(iter))
                fprintf(stderr, "%d: %s is not an iterator\n",
                        __LINE__, TO_STR(iter));
            if (s7_iterator_is_at_end(s7, iter))
                fprintf(stderr, "%d: %s is prematurely done\n",
                        __LINE__, TO_STR(iter));
            while (true) {
                binfile = s7_iterate(s7, iter);
                if (s7_iterator_is_at_end(s7, iter)) break;
                printf("\tbin: %s\n", TO_STR(binfile));
                utstring_renew(opam_bin);
                utstring_printf(opam_bin, "%s/%s",
                                utstring_body(opam_switch_bin),
                                TO_STR(binfile));

                utstring_renew(outpath);
                utstring_printf(outpath, "%s/%s/bin/%s",
                                obazl, pkg, TO_STR(binfile));
                rc = symlink(utstring_body(opam_bin),
                             utstring_body(outpath));
                if (rc != 0) {
                    if (errno != EEXIST) {
                        perror(NULL);
                        fprintf(stderr, "exiting\n");
                        exit(EXIT_FAILURE);
                    }
                }
                if (!emitted_bootstrapper)
                    emit_local_repo_decl(bootstrap_FILE, pkg);

                fprintf(ostream, "exports_files([\"%s\"])\n", TO_STR(binfile));
                fprintf(ostream, "## src: %s\n", utstring_body(opam_bin));
                fprintf(ostream, "## dst: %s\n", utstring_body(outpath));

            }
            /* s7_gc_unprotect_at(s7, gc_loc); */

            fclose(ostream);
    }
}
