#include <ctype.h>
#include <fnmatch.h>
#include <glob.h>
#include <libgen.h>             /* dirname */
#include <stdlib.h>
#include <sys/errno.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

#include "cwalk.h"
#include "findlibc.h"
#include "liblogc.h"
#include "utarray.h"
#include "utstring.h"

#include "opam_fts.h"

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

int opam_dir_ct  = 0;
int opam_file_ct = 0;
int opam_symlink_ct = 0;
int opam_pkg_ct  = 0;

char cwk_buffer[FILENAME_MAX];

bool include_this(FTSENT *ftsentry)
{
#if defined(TRACING)
    if (mibl_trace)
        log_trace(MAG "_include_this?" CRESET " %s (%s)",
                  ftsentry->fts_name, ftsentry->fts_path);
#endif

    /* if (mibl_debug) { */
    /*     dump_mibl_config(); */
    /* } */

    if (ftsentry->fts_name[0] == '.') {
        if (ftsentry->fts_path[0] == '.') {
            if (strlen(ftsentry->fts_path) == 1) {
                return true;
            }
        }
    }
    /* exclusions override inclusiongs */
    /* if exclude return false */
    /* otherwise, if include return true else false */

    /* for exclusions we want an exact match */

    /* discard leading "./" */
    char *ptr = NULL;
    if ((ftsentry->fts_path[0] == '.') && (ftsentry->fts_path[1] == '/'))
        ptr = ftsentry->fts_path+2;
    else
        ptr = ftsentry->fts_path;

#if defined(TRACING)
    if (mibl_debug) log_debug("srch ptr: %s", ptr);
#endif
    char **p;
    p = NULL;
    p = utarray_find(mibl_config.exclude_dirs,
                     &ptr,
                     /* &ftsentry->fts_path, */
                     strsort);
    if  (p != NULL) {
        if (verbose) { // & (verbosity > 2)) {
            log_info(RED "Excluding:" CRESET " '%s'", ftsentry->fts_path);
        }
        return false;
    }

    /* for inclusions:
       if include_dirs is empty, default to ./ - include everything
       otherwise, iterate over include_dirs
       include if tbl contains prefix of fts_path
    */

    if (utarray_len(mibl_config.include_dirs) > 0) {
        p = NULL;
        while ( (p=(char**)utarray_next(mibl_config.include_dirs, p))) {
#if defined(TRACING)
            if (mibl_debug) {
                log_debug("inclusion test pfx: '%s', path: '%s'",
                          *p, ftsentry->fts_path);
                log_debug("result: %d\n",
                          strncmp(*p, ftsentry->fts_path, strlen(*p)));
            }
#endif
            if (strncmp(*p, ftsentry->fts_path, strlen(*p)) < 1) {
                if (verbose) { // & verbosity > 2) {
                    log_info("Include! '%s'", ftsentry->fts_path);
                }
                return true;
            };
        }
        if (verbose) { // & verbosity > 2) {
            log_debug("Include? '%s': %d", ftsentry->fts_path, false);
        }
        return false;
    } else {
        return true;
    }
}

/* control traversal order */
int compare_fts(const FTSENT** one, const FTSENT** two)
{
    return (strcmp((*one)->fts_name, (*two)->fts_name));
}

/* LOCAL bool _this_is_hidden(FTSENT *ftsentry) */
/* { */
/*     if (ftsentry->fts_name[0] == '.') { */
/*         /\* process the "." passed to fts_open, skip any others *\/ */
/*         if (ftsentry->fts_pathlen > 1) { */
/*             // do not process children of hidden dirs */
/*             /\* if (mibl_trace) *\/ */
/*             /\*     log_trace(RED "Excluding" CRESET " hidden dir: %s\n", *\/ */
/*             /\*               ftsentry->fts_path); //, ftsentry->fts_name); *\/ */
/*             return true; */
/*             /\* } else { *\/ */
/*             /\*     printf("ROOT DOT dir\n"); *\/ */
/*         } */
/*     } */
/*     return false; */
/* } */

s7_pointer g_opam_fts(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY;
    s7_pointer p, arg;
    char* module;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg)) {
        module = (char*)s7_string(arg);
    } else {
        if (s7_is_symbol(arg)) {
            module = (char*)s7_symbol_name(arg);
        }
        else return(s7_wrong_type_arg_error(s7, __func__, 0, arg, "string"));
    }
    char *pkg = opam_fts(module);
    if (pkg == NULL) {
        return(s7_nil(s7));
    } else {
        s7_pointer s = s7_make_symbol(s7, pkg);
        free(pkg);
        return(s);
    }
}

char *_get_pkg_name(FTSENT *ftsentry, char *module)
{
    TRACE_ENTRY;
    size_t len;
    cwk_path_get_dirname(ftsentry->fts_path, &len);

    // module is normalized OCaml module name
    // convert Foo to foo.o
    UT_string *ofile;
    utstring_new(ofile);
    utstring_printf(ofile, "%c", tolower(module[0]));
    utstring_printf(ofile, "%s.o", &module[1]);

    log_debug("getting pkg name for %s in %.*s",
              utstring_body(ofile), len, ftsentry->fts_path);

    UT_string *meta_path;
    utstring_new(meta_path);
    struct obzl_meta_package *opam_pkg;
    bool empty_pkg = false;
    (void)empty_pkg;            /* FIXME? */

    UT_string *dir;
    utstring_new(dir);
    utstring_printf(dir, "%.*s*.a",
                    (int)len, ftsentry->fts_path);
    log_debug("globbing: %s", utstring_body(dir));

    /* 1. get all .a files in dir */
    glob_t globlist;
    int rc = glob(utstring_body(dir), /* pattern */
                  0,       /* flags */
                  NULL,              /* error-handler addr */
                  &globlist);
    if (rc != 0) {
        if (rc == GLOB_NOMATCH) {
            log_warn("no .a files found");
        } else {
            log_error("GLOB error");
        }
        return NULL;
    }
    /* success, output found filenames */
    UT_string *ar_cmd;
    utstring_new(ar_cmd);

    UT_string *archive;         /* .a filename */
    utstring_new(archive);
    UT_string *cmxa;            /* .cmxa filename */
    utstring_new(cmxa);

    log_debug("Found %zu filename matches", globlist.gl_pathc);
    char **found = globlist.gl_pathv;
    char *exe = "ar";
    char *result = NULL;

    while(*found)
    {
        utstring_renew(archive);
        utstring_printf(archive, "%s",
                        *found);
        LOG_DEBUG(0, "archive: %s", utstring_body(archive));

        /* rc = system(utstring_body(ar_cmd)); */
        char *argv[] = {"ar", "-t",
                        utstring_body(archive),
                        utstring_body(ofile),
                        NULL};
        LOG_DEBUG(2, "calling run_cmd", "");
        result = run_cmd(exe, argv);
        if (result == NULL) {
            log_warn("FAIL: run_cmd 'ar t'\n");
            found++;
            continue;
        } else {
            LOG_DEBUG(2, "cmd result: '%s'", utstring_body(opam_switch_id));
        }

        /* log_debug("ar t result: %d", rc); */
        if (rc == 0) {
            // found ofile in archive
            // now map archive name to pkg name
            // first read the META file
            utstring_renew(meta_path);
            utstring_printf(meta_path, "%.*s/META",
                            (int)len, ftsentry->fts_path);
            /* LOG_INFO(0, "meta_path: %s", utstring_body(meta_path)); */

            errno = 0;
            if ( access(utstring_body(meta_path), F_OK) != 0 ) {
                // no META happens for e.g. <switch>/lib/stublibs
                log_warn("%s: %s",
                         strerror(errno), utstring_body(meta_path));
                /* FIXME: prior to 5.0.0, lib/ocaml did not contain META files */
                /* so for e.g. compiler-libs targets like Toploop we need to hardcode */
            } else {
                /* exists */
                LOG_INFO(1, "accessible: %s", utstring_body(meta_path));
                empty_pkg = false;
                errno = 0;
                // FIXME: opam_pkg must be freed...
                opam_pkg = obzl_meta_parse_file(utstring_body(meta_path));

                if (opam_pkg == NULL) {
                    if ((errno == -1)
                        || (errno == -2)) {
                        empty_pkg = true;
                        LOG_WARN(1, "Empty META file: %s", utstring_body(meta_path));
                        /* create null obzl_meta_package for empty META */
                        opam_pkg = (struct obzl_meta_package*)calloc(sizeof(struct obzl_meta_package), 1);
                        char *fname = strdup(utstring_body(meta_path));
                        opam_pkg->name      = package_name_from_file_name(fname);
                        /* opam_pkg->name      = opam_pkg_dir; */
                        char *x = strdup(opam_pkg->name);
                        char *p;
                        // module names may not contain uppercase
                        for (p = x; *p; ++p) *p = tolower(*p);
                        opam_pkg->module_name = strdup(x);
                        free(x);
                        x = strdup(fname);
                        opam_pkg->path      = strdup(dirname(x));
                        free(x);
                        opam_pkg->directory = opam_pkg->name; // dirname(fname);
                        opam_pkg->metafile  = utstring_body(meta_path);
                        opam_pkg->entries = NULL;
                        /* return; */
                        /* } */
                        /* else if (errno == -2) { */
                        /*     log_warn("META file contains only whitespace: %s", utstring_body(meta_path)); */
                        /*     return; */
                    } else {
                        log_error("Error parsing %s", utstring_body(meta_path));
                        return NULL;
                    }
                    /* emitted_bootstrapper = false; */
                } else {
                    /* found and parsed META file for archive */
                    if (verbose > 0) {
                        log_info("Parsed: %s", utstring_body(meta_path));
                        log_info("pkg name: %s", opam_pkg->name);
                        bool x = obzl_meta_package_has_archives(opam_pkg);
                        log_info("pkg has archvies?: %d",
                                 x);
                        x = obzl_meta_package_has_subpackages(opam_pkg);
                        log_info("pkg has subpkgs?: %d",
                                 x);
                        findlib_version_t *v = findlib_pkg_version(opam_pkg);
                        log_info("pkg version: %d.%d.%d",
                                 v->major, v->minor, v->patch);
                    }
                    /* archive contains abs path to .a file */
                    const char *basename;
                    size_t length;
                    cwk_path_get_basename(utstring_body(archive), &basename, &length);
                    log_debug("archive basename is: '%.*s'", (int)length, basename);

                    memset(cwk_buffer, 0, sizeof(cwk_buffer));
                    cwk_path_change_extension(basename, "cmxa", cwk_buffer,
                                              sizeof(cwk_buffer));
                    log_debug("cmxa basename is: '%s'", cwk_buffer);

                    /* utstring_renew(cmxa); */
                    /* utstring_printf(cmxa, "%.*s", */
                    /*         (int)len, ftsentry->fts_path); */

                    char *flpkg = findlib_pkg_for_cmxa(opam_pkg, NULL, cwk_buffer);
                    log_info("pkg for cmxa: %s", flpkg);
                    if (flpkg != NULL) {
                        return flpkg;
                    }
                }
            }
        }
        found++;
    }
    /* while (globlist.gl_pathv[i]) { */
    /*     printf("%s\n", globlist.gl_pathv[i]); */
    /*     i++; */
    /* } */

    return NULL;
}

/* returns name of opam pkg containing module */
/* returned string must be freed */
EXPORT char *opam_fts(char *module)
{
    TRACE_ENTRY;
    /* char *old_cwd = getcwd(NULL, 0); */
    /* char *opam_switch = utstring_body(opam_switch_id); */
    char *opam_libdir = utstring_body(opam_switch_lib);
    LOG_DEBUG(0, "opam_libdir: %s", opam_libdir);

    UT_string *key;
    utstring_new(key);
    utstring_printf(key, "%c", tolower(module[0]));
    utstring_printf(key, "%s.cmi", &module[1]);
    char *_module = utstring_body(key);
    LOG_DEBUG(0, "opam_fts search key: %s", _module);

    LOG_DEBUG(0, "opam_fts root: %s", utstring_body(opam_switch_prefix));

/*     rc = chdir(opam_switch); */
/*     if (rc != 0) { */
/*         log_error("FAIL on chdir: %s => %s\n", old_cwd, opam_switch); */
/*         fprintf(stderr, RED "FAIL on chdir: %s => %s: %s\n", */
/*                 old_cwd, opam_switch, strerror(errno)); */
/*         exit(EXIT_FAILURE); */
/*         } */
/* #if defined(TRACING) */
/*         if (mibl_debug) log_debug("%-16s%s", "cwd:",  getcwd(NULL, 0)); */
/* #endif */

    FTS* tree = NULL;
    FTSENT *ftsentry     = NULL;

    errno = 0;

    char *const _opam_libdir[] = {
        /* [0] = resolved_troot, // opam_libdir; */
        [0] = (char *const)opam_libdir,
        NULL
    };
#if defined(TRACING)
    if (mibl_debug) {
        log_debug("_opam_libdir: %s", _opam_libdir[0]);
        log_debug("real _opam_libdir: %s",
                  realpath(_opam_libdir[0], NULL));
    }
#endif

    errno = 0;
    tree = fts_open(_opam_libdir,
                    FTS_COMFOLLOW
                    | FTS_NOCHDIR
                    | FTS_PHYSICAL,
                    // NULL
                    &compare_fts
                    );
    if (errno != 0) {
        log_error("fts_open error: %s", strerror(errno));
        return NULL;
        /* return s7_error(s7, s7_make_symbol(s7, "fts_open"), */
        /*                 s7_list(s7, 2, */
        /*                         s7_make_string(s7, strerror(errno)), */
        /*                         s7_make_string(s7, _opam_libdir[0]))); */
    }

    /* char *ext; */

    if (verbose) {
        log_info(GRN "Beginning traversal" CRESET " at %s",
                 _opam_libdir[0]);
                 // resolved_troot);
        log_info(GRN " with cwd:" CRESET " at %s", getcwd(NULL, 0));
    }

    /* TRAVERSAL STARTS HERE */
    if (NULL != tree) {
        while( (ftsentry = fts_read(tree)) != NULL) {
            if (ftsentry->fts_info == FTS_DP) {
                /* log_debug("FTS_DP: %s", ftsentry->fts_path); */
                continue; // do not process post-order visits
            }
#if defined(TRACING)
            if (mibl_debug) {
                printf("\n");
                log_debug(CYN "iter ftsentry->fts_name: " CRESET "%s",
                          ftsentry->fts_name);
                log_debug("iter ftsentry->fts_path: %s", ftsentry->fts_path);
                log_debug("iter ftsentry->fts_info: %d", ftsentry->fts_info);
            }
#endif
            /* if (mibl_debug) { */
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
                    opam_dir_ct++;
                    /* dir_ct++; */
                    /* log_debug("FTS_D: %s", ftsentry->fts_name); */
                    /* log_debug("FTS_D: %s", ftsentry->fts_path); */
                    /* log_debug("FTS_D: %s", ftsentry->fts_accpath); */

                    //FIXME: counting pkgs just for statistics
                    // omit?
                    if (strncmp(ftsentry->fts_path,
                                opam_libdir,
                                strlen(opam_libdir))==0){
                        if (strlen(ftsentry->fts_path)
                            == strlen(opam_libdir)) {
                            // skip traversal root (switch lib)
                            break;
                        }
                    }
                    memset(cwk_buffer, 0, sizeof(cwk_buffer));
                    cwk_path_get_relative(opam_libdir,
                                          ftsentry->fts_path,
                                          cwk_buffer,
                                          sizeof(cwk_buffer));
                    /* log_debug("The relative path is: %s", cwk_buffer); */
                    char *loc = strchr(cwk_buffer, '/');
                    if (loc == NULL) {
                        /* log_debug("pkg++"); */
                        opam_pkg_ct++;
                    }
                    break;
                case FTS_F : // regular file
                    opam_file_ct++;
                    /* log_debug("FTS_F: %s", ftsentry->fts_path); */
                    if ((strncmp(ftsentry->fts_name,
                                _module, strlen(_module))==0)
                        /* FIXME: verify length of fts_name? */
                        ) {
                        LOG_DEBUG(0, RED "FOUND file for module: %s" CRESET, module);
                        LOG_DEBUG(1, "FTS_D: %s", ftsentry->fts_name);
                        LOG_DEBUG(1, "FTS_D: %s", ftsentry->fts_path);
                        LOG_DEBUG(1, "FTS_D: %s", ftsentry->fts_accpath);

                        // Now: get the findlib pkg name
                        char *pkg = _get_pkg_name(ftsentry, module);
                        // Then: add it to cache, in case of
                        // multiple matches
                        // But for now: return first match

                        fts_close(tree);
                        return (pkg);
                    }
                    break;
                case FTS_SL: // symlink
                    opam_symlink_ct++;
                    log_debug("FTS_SL: %s", ftsentry->fts_name);
                    /* handle_symlink(tree, ftsentry); */
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
                    log_error(RED "Unhandled FTS type %d\n" CRESET,
                              ftsentry->fts_info);
                    exit(EXIT_FAILURE);
                    break;
                }
        }
        LOG_DEBUG(0, "traversal finished", "");
        /* chdir(old_cwd); */
        /* printf(RED "Restored cwd: %s\n" CRESET, getcwd(NULL, 0)); */
    }
    LOG_DEBUG(0, "NOT FOUND: opam pkg for %s", module);

    if (verbose) {
        log_info("cwd: %s", getcwd(NULL, 0));
        log_info("traversal root:  %s", _opam_libdir[0]);
        /* log_info("bws: %s", rootws); */
        /* log_info("ews: %s", ews_root); */
        log_info("opam pkg count:  %d", opam_pkg_ct);
        log_info("opam dir count:  %d", opam_dir_ct);
        log_info("opam file count: %d", opam_file_ct);
        log_info("opam symlink ct: %d", opam_symlink_ct);
        log_info("exiting opam_fts");
    }

    fts_close(tree);
    /* s7_gc_unprotect_at(s7, pkg_tbl_gc_loc); */

    return NULL;
}

