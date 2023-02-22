#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
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

/* #if INTERFACE */
/* #include "utarray.h" */
/* #include "utstring.h" */
/* /\* #include "s7.h" *\/ */
/* #endif */

/* #include "log.h" */

/* #include "libdune.h" */

/* #include "libfindlib.h" */

#include "opam_switch_converter.h"

UT_array  *segs;
UT_string *group_tag;

UT_string *dunefile_name;

/* s7_int dune_gc_loc = -1; */

/* s7_pointer g_dunefile_port; */

/* void s7_show_stack(s7_scheme *sc); */

UT_string *meta_path;           /* = opam_switch_lib + pkg_name */

extern UT_string *opam_switch_lib;

/* config_opam.c */
extern UT_string *opam_switch_lib;

int fts_d_ct  = 0;
int fts_dp_ct  = 0;
int fts_dot_ct  = 0;
int fts_f_ct = 0;
int fts_sl_ct = 0;

char *toolchains[] = {
    "@ocaml//toolchain/selectors/local:vmvm",
    "@ocaml//toolchain/selectors/local:vmsys",
    "@ocaml//toolchain/selectors/local:sysvm",
    "@ocaml//toolchain/selectors/local:syssys",
    "@ocaml//toolchain/selectors/local:_vm", /* *>vm */
    "@ocaml//toolchain/selectors/local:__",  /* *>* (default) */
    /* profiles - order matters */
    "@ocaml//toolchain/profiles:sys-dev",
    "@ocaml//toolchain/profiles:sys-dbg",
    "@ocaml//toolchain/profiles:sys-opt",
    "@ocaml//toolchain/profiles:vm-dev",
    "@ocaml//toolchain/profiles:vm-dbg",
    "@ocaml//toolchain/profiles:vm-opt",
    /* default must come last, empty target_compatible_with */
    "@ocaml//toolchain/profiles:default-dev",
    "@ocaml//toolchain/profiles:default-dbg",
    "@ocaml//toolchain/profiles:default-opt",

    /* later, for cross-compilers: */
    /* "@ocaml//toolchain/selectors/macos/x86_64:vm", */
    /* "@ocaml//toolchain/selectors/macos/x86_64:macos_x86_64", */
    /* "@ocaml//toolchain/selectors/macos/x86_64:linux_x86_64", */

    /* "@ocaml//toolchain/selectors/linux/x86_64:vm", */
    /* "@ocaml//toolchain/selectors/linux/x86_64:linux_x86_64", */
    /* "@ocaml//toolchain/selectors/linux/x86_64:macos_x86_64", */

    "" /* do not remove terminating null */
};
char **tc;

EXPORT void convert_findlib_pkgs(UT_array *opam_pending_deps,
                                 UT_array *opam_exclude_pkgs)
{
/* #if defined(DEBUG_TRACE) */
        log_debug(BLU "convert_findlib_pkgs" CRESET);
        log_debug("%-16s%s", "opam switch:", utstring_body(opam_switch_lib));
        log_debug("%-16s%s", "launch_dir:", launch_dir);
        log_debug("%-16s%s", "base ws:", bws_root);
        log_debug("%-16s%s", "effective ws:", ews_root);
        log_debug("pendings ct: %d", utarray_len(opam_pending_deps));

/* #endif */
    if (verbose) {
        log_debug("current dir: %s", getcwd(NULL, 0));
        /* printf(YEL "%-16s%s\n" CRESET, "pkg_name:", pkg_name); */
    }

    /* first: construct list of dirs to convert */

    const char **p = NULL;

    if (utarray_len(opam_pending_deps) < 1) {
        /* default: all pkgs in switch */
        log_info("reading opam pkgs in %s", utstring_body(opam_switch_lib));
        errno = 0;
        DIR *switch_dir = opendir(utstring_body(opam_switch_lib));
        if (switch_dir == NULL) {
            log_error("ERROR: bad opendir: %s\n", strerror(errno));
            fprintf(stderr, "ERROR: bad opendir: %s\n", strerror(errno));
            return;
        }
        struct dirent *direntry;
        char *s;
        while ((direntry = readdir(switch_dir)) != NULL) {
            /* log_debug("readed %s", direntry->d_name); */
            if ('.' == direntry->d_name[0]) continue;
            if (direntry->d_type != DT_DIR) continue;
            s = strdup(direntry->d_name);
            utarray_push_back(opam_pending_deps, &s);
        }
        closedir(switch_dir);

        log_debug("pendings ct: %d", utarray_len(opam_pending_deps));

        /* p = NULL; */
        /* while ( (p=(char**)utarray_next(opam_pending_deps, p))) { */
        /*     log_info("read:  %s", *p); */
        /* } */

        /* log_error("EXITING"); */
        /* exit(0); */
    }

    /* **************************************************************** */
    /* **************************************************************** */

    /* now the conversion */

    utstring_new(workspace_file);
    bazel_ws_root = utstring_body(opam_switch_lib); /* dst dir == src dir */

    /* Do requested opam pkgs FIRST */
    UT_array *opam_completed_deps;
    utarray_new(opam_completed_deps, &ut_str_icd);

    char **this;
    char *next;
    while ( utarray_len(opam_pending_deps) > 0 ) {
        this = utarray_eltptr(opam_pending_deps, 0);
        next = strdup(*this);
#if defined(DEBUG_TRACE)
        log_info("next pkg: %s", next);
#endif
        utarray_erase(opam_pending_deps, 0, 1);
        /* handle_findlib_pkg will check completed_deps before adding
           new pkg to pending_deps */
        /* FIXME: make this a subroutine */
        p = NULL;
        p = (const char**)utarray_find(opam_completed_deps, &next, strsort);
        if (p == NULL)
            utarray_push_back(opam_completed_deps, &next);

        handle_findlib_pkg(next, opam_pending_deps, opam_completed_deps);

        free(next);
    }

    /* **************************************************************** */
    /* Do @ocaml last, since it creates dirs */
    emit_ocaml_workspace(bazel_ws_root);

#if defined(DEBUG_TRACE)
    log_debug("done");
#endif

    UT_string *dune_pkg_file;    /* FIXME: free */
    utstring_new(dune_pkg_file); /* global, in emit_pkg_bindir.c */
    while ( (p=(const char**)utarray_next(opam_completed_deps, p))) {
#if defined(DEBUG_TRACE)
        log_info("  %s", *p);
#endif
        emit_pkg_bindir(*p);
    }
#if defined(DEBUG_TRACE)
    log_debug("pending:");
    while ( (p=(const char**)utarray_next(opam_pending_deps, p))) {
        log_info("  %s", *p);
    }
#endif

    /* **************************************************************** */
    /* finally write WORKSPACE.opam.bzl to import deps repos */
    UT_string *opam_bzl_file;
    utstring_new(opam_bzl_file);
    utstring_printf(opam_bzl_file, "%s/WORKSPACE.opam.bzl", bws_root);
    log_debug("writing %s", utstring_body(opam_bzl_file));

    FILE *ostream = fopen(utstring_body(opam_bzl_file), "w");
    if (ostream == NULL) {
        perror(utstring_body(opam_bzl_file));
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(opam_bzl_file));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n\n");

    fprintf(ostream, "def bootstrap():\n");

    /* fprintf(ostream, "    native.local_repository(\n"); */
    /* fprintf(ostream, "        name       = \"ocaml\",\n"); */

    /* fprintf(ostream, "        path       = "); */
    /* fprintf(ostream, "\"%s/%s\",\n", bazel_ws_root, "ocaml"); */
    /* fprintf(ostream, "    )\n\n"); */

    /* /\* we also always have lib/stublibs, which is not an OPAM pkg *\/ */
    /* fprintf(ostream, "    native.local_repository(\n"); */
    /* fprintf(ostream, "        name       = \"stublibs\",\n"); */

    /* fprintf(ostream, "        path       = "); */
    /* fprintf(ostream, "\"%s/%s\",\n", bazel_ws_root, "stublibs"); */
    /* fprintf(ostream, "    )\n\n"); */

    utarray_sort(opam_exclude_pkgs, strsort);
    char **q = NULL;
    p = NULL;
    while ( (p=(const char**)utarray_next(opam_completed_deps, p))) {

        q = (const char**)utarray_find(opam_exclude_pkgs, p, strsort);

        if (q == NULL) {
            log_info("Importing opam pkg: %s", *p);
            fprintf(ostream, "    native.local_repository(\n");
            fprintf(ostream, "        name       = \"%s\",\n", *p);
            fprintf(ostream, "        path       = ");
            fprintf(ostream, "\"%s/%s\",\n", bazel_ws_root, *p);
            fprintf(ostream, "    )\n\n");
        } else {
            log_info(RED "Excluding opam pkg: %s" CRESET, *p);
        }
    }

    log_debug("pending ct: %d",  utarray_len(opam_pending_deps));
    log_debug("completed ct: %d",  utarray_len(opam_completed_deps));
    /* FIXME: free opam_completed_deps, opam_pending_deps */
    /* while ( (p=(char**)utarray_next(opam_completed_deps, p))) { */
    /* free(p); */
    /* } */


    /* toolchains */
    tc = toolchains;
    while (strlen(*tc) != 0) { // *tc != "") {
        fprintf(ostream,
                "    native.register_toolchains(\"%s\")\n",
                *tc++);
    }

    fclose(ostream);
    utstring_free(opam_bzl_file);


    /* if (opam_switch_lib == NULL) { */
    /*     /\* _walk_project(pkg_name); *\/ */
    /* } else { */
    /*     if (pkg_name == NULL) { */
    /*         _walk_findlib_all(opam_switch_lib); */
    /*     } else { */
    /*         handle_findlib_pkg(opam_switch_lib, pkg_name); */
    /*     } */
    /* } */

    return;
}
