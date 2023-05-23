#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
/* #include <fnmatch.h> */
#include <fcntl.h>              /* open() */
#include <fts.h>
#include <libgen.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "log.h"
#include "utstring.h"

#include "coswitch_cleaner.h"

extern UT_string *opam_switch_lib;

int rm_buildfile_ct = 0;
int rm_ws_ct = 0;
int rmdir_ct  = 0;
int unlink_ct = 0;

int _compare_fts(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

int _select(const struct dirent *de) {
    if ((strncmp(".", de->d_name, 1) == 0)
        && strnlen(de->d_name, 2) == 1) {
        return 0;
    }
    return strncmp("..", de->d_name, 2);
    return 1;
}


EXPORT void clean_coswitch(void)
{
#if defined(TRACING)
    if (mibl_debug) {
        log_debug(BLU "clean_coswitch" CRESET);
        log_debug("%-16s%s", "opam switch:", utstring_body(opam_switch_lib));
        log_debug("current dir: %s", getcwd(NULL, 0));
    }
#endif

    char *switch_root = utstring_body(opam_switch_lib);

    char *const _pkg_path[] = {
        [0] = (char *const)switch_root,
        NULL
    };
    FTS* tree        = NULL;
    FTSENT *ftsentry = NULL;
    errno = 0;
    tree = fts_open(_pkg_path,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &_compare_fts
                    );
    if (errno != 0) {
        log_error("fts_open error: %s:  '%s'",strerror(errno), *_pkg_path);
        return;
    }

    struct dirent **namelist;
    int n;

#if defined(TRACING)
    if (mibl_debug) {
        log_info(GRN "Beginning traversal" CRESET " at %s", _pkg_path[0]);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }
#endif
    /* TRAVERSAL STARTS HERE */
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            switch (ftsentry->fts_info) {
            case FTS_DP : // dir visited in post-order
                /* if it contains BUILD.bazel, remove it. if it is
                   then empty, remove the dir. */
                /* log_debug("postorder: %s", ftsentry->fts_path); */
                n = scandir(ftsentry->fts_path, &namelist, _select, alphasort);
                if (n == 0) {
#if defined(TRACING)
                    if (mibl_debug)
                        log_info("removing EMPTY DIR: %s", ftsentry->fts_path);
#endif
                    rmdir(ftsentry->fts_path);
                    rmdir_ct++;
                }
                else if (n < 0) perror("scandir");
                else {
                    while (n--) { free(namelist[n]); }
                    free(namelist);
                }
                break;
            case FTS_D : // dir visited in pre-order
                /* log_debug("iter ftsentry->fts_info: %d", ftsentry->fts_info); */
                /* log_debug("iter ftsentry->fts_name: %s", */
                /*           ftsentry->fts_name); */
                /* log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path); */
                /* if dir contains BUILD.bazel,  */
                break;
            case FTS_F : // regular file
                if (strncmp("BUILD.bazel", ftsentry->fts_name, 11) == 0) {
#if defined(TRACING)
                    if (mibl_debug)
                        log_info("Removing %s", ftsentry->fts_path);
#endif
                    remove(ftsentry->fts_path);
                    rm_buildfile_ct++;
                    break;
                }
                if (strncmp("WORKSPACE.bazel", ftsentry->fts_name, 15) == 0) {
#if defined(TRACING)
                    if (mibl_debug)
                        log_info("Removing %s", ftsentry->fts_path);
#endif
                    remove(ftsentry->fts_path);
                    rm_ws_ct++;
                    break;
                }
                break;
            case FTS_SL: // symlink
#if defined(TRACING)
                if (mibl_debug)
                    log_info("unlinking %s", ftsentry->fts_path);
#endif
                unlink(ftsentry->fts_path);
                unlink_ct++;
                break;
            }
        }
    }
    fts_close(tree);

    /* reset WORKSPACE.opam.bzl */
    //FIXME: this assumes cwd is proj root
    UT_string *opam_bzl_file;
    utstring_new(opam_bzl_file);
    utstring_printf(opam_bzl_file, "%s/WORKSPACE.opam.bzl", rootws);

    FILE *ostream = fopen(utstring_body(opam_bzl_file), "w");
    if (ostream == NULL) {
        perror(utstring_body(opam_bzl_file));
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(opam_bzl_file));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n\n");

    fprintf(ostream, "def bootstrap():\n");
    fprintf(ostream, "    pass\n");

    fclose(ostream);

    if (verbose) {
        log_info("Removed %d WORKSPACE.bazel files", rm_ws_ct);
        log_info("Removed %d BUILD.bazel files", rm_buildfile_ct);
        log_info("Removed %d symlinks", unlink_ct);
        log_info("Removed %d directories", rmdir_ct);
        log_info("Wrote %s", utstring_body(opam_bzl_file));
    }

    utstring_free(opam_bzl_file);

    return;
}
