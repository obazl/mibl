#include <fnmatch.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "treewalker_project.h"

/* FIXME: common to both walkers */
LOCAL bool _this_is_hidden(FTSENT *ftsentry)
{
    if (ftsentry->fts_name[0] == '.') {
        /* process the "." passed to fts_open, skip any others */
        if (ftsentry->fts_pathlen > 1) {
            // do not process children of hidden dirs
            /* if (trace) */
            /*     log_trace(RED "Excluding" CRESET " hidden dir: %s\n", */
            /*               ftsentry->fts_path); //, ftsentry->fts_name); */
            return true;
            /* } else { */
            /*     printf("ROOT DOT dir\n"); */
        }
    }
    return false;
}

EXPORT void convert_dune_project(UT_array *opam_pending_deps)
{
#if defined(DEBUG_TRACE)
    log_debug(BLU "convert_dune_project" CRESET);
    log_debug("%-16s%s", "opam switch:", utstring_body(opam_switch_lib));
    log_debug("%-16s%s", "launch_dir:", launch_dir);
    log_debug("%-16s%s", "base ws:", rootws);
    log_debug("%-16s%s", "effective ws:", ews_root);
#endif
    return;
    /*
      FIXME: traversal root(s) to be determined by miblrc.srcs.include
      default is cwd, but if miblrc designates 'include' dirs, then
      cwd must be excluded, and each 'include' dir traversed.
     */

    /*
      always cd to effective ws root, since the resolved traversal
      root is relative to it. that way ftsentry->fts_path will be a
      proper workspace-relative pkg-path.

      restore cwd after traversal.
    */
    char *old_cwd = getcwd(NULL, 0);
    if (strncmp(old_cwd, ews_root, strlen(ews_root)) != 0) {
#if defined(DEBUG_TRACE)
        if (debug) {
            log_debug("chdir: %s => %s\n", old_cwd, ews_root);
        }
#endif
        rc = chdir(ews_root);
        if (rc != 0) {
            log_error("FAIL on chdir: %s => %s\n", old_cwd, ews_root);
            fprintf(stderr, RED "FAIL on chdir: %s => %s: %s\n",
                    old_cwd, ews_root, strerror(errno));
            exit(EXIT_FAILURE);
        }
#if defined(DEBUG_TRACE)
        if (debug) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0));
#endif
    }

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _pkg_path[] = {
        /* [0] = resolved_troot, // pkg_path; */
        [0] = (char *const)pkg_path,
        NULL
    };
#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("_pkg_path: %s", _pkg_path[0]);
        log_debug("real _pkg_path: %s",
                  realpath(_pkg_path[0], NULL));
    }
#endif

    errno = 0;
    tree = fts_open(_pkg_path,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &compare_fts
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return;
        /* return s7_error(s7, s7_make_symbol(s7, "fts_open"), */
        /*                 s7_list(s7, 2, */
        /*                         s7_make_string(s7, strerror(errno)), */
        /*                         s7_make_string(s7, _pkg_path[0]))); */
    }

    char *ext;

    if (verbose) {
        log_info(GRN "Beginning traversal" CRESET " at %s",
                 _pkg_path[0]);
                 // resolved_troot);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }

    /* TRAVERSAL STARTS HERE */
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            if (ftsentry->fts_info == FTS_DP) {
                continue; // do not process post-order visits
            }
#if defined(DEBUG_TRACE)
            if (debug) {
                printf("\n");
                log_debug(CYN "iter ftsentry->fts_name: " CRESET "%s",
                          ftsentry->fts_name);
                log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path);
                log_debug("iter ftsentry->fts_info: %d", ftsentry->fts_info);
            }
#endif
            /* if (debug) { */
            /*     if (ftsentry->fts_info != FTS_DP) { */
            /*         log_debug(CYN "ftsentry:" CRESET " %s (%s), type: %d", */
            /*                   ftsentry->fts_name, */
            /*                   ftsentry->fts_path, */
            /*                   ftsentry->fts_info); */
            /*     } */
            /* } */
            switch (ftsentry->fts_info)
                {
                case FTS_D : // dir visited in pre-order
#if defined(DEBUG_TRACE)
                    if (trace)
                        log_trace("pre-order visit dir: %s (%s) :: (%s)",
                                  ftsentry->fts_name,
                                  ftsentry->fts_path,
                                  ftsentry->fts_accpath);
#endif
                    if (_this_is_hidden(ftsentry)) {
#if defined(DEBUG_TRACE)
                        if (trace)
                            log_trace(RED "Excluding" CRESET " hidden dir: %s",
                                      ftsentry->fts_path);
#endif
                        fts_set(tree, ftsentry, FTS_SKIP);
                        /* break; */
                    }
                    else if (fnmatch("*.opam-bundle",
                                     ftsentry->fts_name, 0) == 0) {
                        fts_set(tree, ftsentry, FTS_SKIP);
                        /* break; */
                    } else {
                        if (include_this(ftsentry)) {
#if defined(DEBUG_TRACE)
                            if (trace) log_info(RED "Including" CRESET " %s",
                                                ftsentry->fts_path);
#endif
                            if (strncmp(ftsentry->fts_name, "_build", 6) == 0) {
                                /* skip _build (dune) */
                                fts_set(tree, ftsentry, FTS_SKIP);
                                break;
                            }

                            handle_dir(tree, ftsentry);
                            /* printf("pkg tbl: %s\n", TO_STR(pkg_tbl)); */
                        } else {
                            fts_set(tree, ftsentry, FTS_SKIP);
                        }
                    }
                    break;
                case FTS_DP: /* postorder directory */
#if defined(DEBUG_TRACE)
                    if (trace)
                        log_trace("post-order visit dir: %s (%s) :: (%s)",
                                  ftsentry->fts_name,
                                  ftsentry->fts_path,
                                  ftsentry->fts_accpath);
#endif
                    break;
                case FTS_F : // regular file
                    file_ct++;

                    if (strncmp(ftsentry->fts_name,"BUILD.bazel", 11)==0){
                            /* skip BUILD.bazel files */
                            break;
                    }
                    /* TODO: skip *.bzl files */
                    /* TODO: skip standard files: READMEs, LICENSE, etc. */
                    /* handle_regular_file(ftsentry); */
                    if (strncmp(ftsentry->fts_name, "dune-project", 12)
                        == 0) {
                        handle_dune_project_file(ftsentry);
                        break;
                    }
                    if ((strncmp(ftsentry->fts_name, "dune", 4) == 0)
                        /* don't read dune.foo */
                        && (strlen(ftsentry->fts_name) == 4)) {
                        handle_dune_file(ftsentry);
                        /* break; */
                        continue;
                    }

                    ext = strrchr(ftsentry->fts_name, '.');

                    if (ext) {
                        if ((strncmp(ext, ".ml", 3) == 0)) {
                            handle_ml_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".md", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            handle_ml_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".sh", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            handle_file(ftsentry, ext);
                            /*_handle_script_file(ftsentry, ext);*/
                        }
                        else if ((strncmp(ext, ".py", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            handle_file(ftsentry, ext);
                            /*_handle_script_file(ftsentry, ext);*/
                        }
                        else if ((strncmp(ext, ".opam", 5) == 0)
                                 && (strlen(ext) == 5)) {
                            handle_opam_file(ftsentry);
                        }
                        else if (fnmatch("*.opam.template",
                                         ftsentry->fts_name, 0) == 0) {
                            handle_opam_template_file(ftsentry);
                        }
                        else if (strncmp(ext, ".ocamlformat", 12) == 0) {
                            handle_ocamlformat_file(ftsentry);
                        }
                        else if ((strncmp(ext, ".c", 2) == 0)
                                 && (strlen(ext) == 2)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".h", 2) == 0)
                                 && (strlen(ext) == 2)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".cc", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".hh", 3) == 0)
                                 && (strlen(ext) == 3)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".cpp", 4) == 0)
                                 && (strlen(ext) == 4)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".hpp", 4) == 0)
                                 && (strlen(ext) == 4)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".cxx", 4) == 0)
                                 && (strlen(ext) == 4)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else if ((strncmp(ext, ".hxx", 4) == 0)
                                 && (strlen(ext) == 4)) {
                            handle_cc_file(ftsentry, ext);
                        }
                        else {
                            handle_file(ftsentry, ext);
                        }
                    }
                    else {
                        /* no extension */
                        if ((strstr(ftsentry->fts_name, "opam"))
                            && (strlen(ext) == 4)) {
                            handle_opam_file(ftsentry);
                        }
                        else if ((strstr(ftsentry->fts_name, "META"))
                                 && (strlen(ext) == 4)) {
                            handle_meta_file(ftsentry);
                        }
                        else {
                            handle_file(ftsentry, ext);
                        }
                    }
                    break;
                case FTS_SL: // symlink
                    file_ct++;
                    handle_symlink(tree, ftsentry);
                    break;
                case FTS_SLNONE:
                    /* symlink to non-existent target */
                    log_warn("FTS_SLNONE: %s", ftsentry->fts_path);
                    break;
                case FTS_ERR:
                    log_error("FTS_ERR: %s", ftsentry->fts_path);
                    log_error("  error: %d: %s", ftsentry->fts_errno,
                              strerror(ftsentry->fts_errno));
                    break;
                case FTS_DC:
                    /* dir causing a cycle dir */
                    log_warn("FTS_DC: %s", ftsentry->fts_path);
                    break;
                case FTS_DNR:
                    /* unreadable dir */
                    log_warn("FTS_DNR: %s", ftsentry->fts_path);
                    break;
                case FTS_NS:
                    /* no stat info, error */
                    log_error("FTS_NS: %s", ftsentry->fts_path);
                    log_error("  error: %d: %s", ftsentry->fts_errno,
                              strerror(ftsentry->fts_errno));
                    break;
                case FTS_NSOK:
                    /* no stat info, not an error */
                    log_warn("FTS_NSOK: %s", ftsentry->fts_path);
                    break;
                case FTS_DEFAULT:
                    log_warn("FTS_DEFAULT: %s", ftsentry->fts_path);
                    break;
                /* case FTS_DOT : // not specified to fts_open */
                /*     // do not process children of hidden dirs */
                /*     /\* fts_set(tree, ftsentry, FTS_SKIP); *\/ */
                /*     break; */
                default:
                    log_error(RED "Unhandled FTS type %d\n",
                              ftsentry->fts_info);
                    exit(EXIT_FAILURE);
                    break;
                }
        }
        chdir(old_cwd);
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    }

    if (verbose) {
        log_info("cwd: %s", getcwd(NULL, 0));
        log_info("bws: %s", rootws);
        log_info("ews: %s", ews_root);
        log_info("dir count: %d", dir_ct);
        log_info("file count: %d", file_ct);
        log_info("dunefile count: %d", dunefile_ct);
        /* log_info("pkg_tbl: %s", TO_STR(pkg_tbl)); */

        log_info("exiting load_project");
    }

    fts_close(tree);
    /* s7_gc_unprotect_at(s7, pkg_tbl_gc_loc); */

    return;
}

