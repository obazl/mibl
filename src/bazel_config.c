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
#if EXPORT_INTERFACE
#include <stdbool.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"

#if EXPORT_INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "bazel_config.h"

/* bool debug; */
/* bool verbose; */

int rc;

UT_string *exec_root;
UT_string *runfiles_root;
UT_string *ws_root;
/* UT_string *config_obazl; // obazl_d; */

#define LIBS7    "libs7"
#define LIBS7_S7 LIBS7 "/s7"
#define OIBL   "oibl"
#define XDG_LOCAL_SHARE ".local/share"

UT_string *runtime_data_dir;

bool ini_error; // = false;
UT_string *obazl_ini_path; // .config

#if EXPORT_INTERFACE
struct configuration_s {
    char *obazl_version;
    int libct;
    UT_array *src_dirs;         /* string list; used by fileseq to get src_files */
    UT_array *watch_dirs;       /* string list */
    /* struct lib_s *ocamllibs[10]; /\* is 10 enough? *\/ */
    /* struct lib_s *coqlibs[10]; /\* is 10 enough? *\/ */
};
#endif

#define OBAZL_VERSION "0.1.0"

struct configuration_s bazel_config = {.obazl_version = OBAZL_VERSION, .libct = 0};

UT_array *src_files;            /* FIXME: put this in configuration_s? */

EXPORT void bazel_configure(char *_exec_root)
{
    /* log_debug("bazel_configure"); */
    utstring_new(exec_root);
    utstring_printf(exec_root, "%s", _exec_root);
    if (debug)
        log_debug("EXEC ROOT (Bazel): %s", utstring_body(exec_root));

    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
    if (debug)
        log_debug("runfiles_root: %s", utstring_body(runfiles_root));

    char *_ws_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_ws_root == NULL) {
        if (debug)
            log_debug("BUILD_WORKSPACE_DIRECTORY: null");
    } else {
        if (debug)
            log_debug("BUILD_WORKSPACE_DIRECTORY: %s", _ws_root);
    }

    utstring_new(ws_root);
    if (_ws_root == NULL)
        utstring_printf(ws_root, "%s", getcwd(NULL, 0));
    else
        utstring_printf(ws_root, "%s", _ws_root);

    /* **************** */
    char *_wd = getenv("BUILD_WORKING_DIRECTORY");
    if (_wd == NULL) {
        if (debug)
            log_debug("BUILD_WORKING_DIRECTORY: null");
    } else {
        if (debug)
            log_debug("BUILD_WORKING_DIRECTORY: %s", _wd);
    }
    if (debug)
        log_debug("LAUNCH DIR: %s", _wd);

    /* .config/libs7 config file */
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s",
                    utstring_body(ws_root), LIBS7_INI_FILE);

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("NOT FOUND: libs7rc config file %s",
                     utstring_body(obazl_ini_path));
    } else {
        ini_error = false;
        if (verbose || debug)
            log_warn("FOUND: libs7rc config file %s",
                     utstring_body(obazl_ini_path));

        utarray_new(bazel_config.src_dirs, &ut_str_icd);
        utarray_new(bazel_config.watch_dirs, &ut_str_icd);
        rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &bazel_config);
        if (rc < 0) {
            //FIXME: deal with missing .obazl
            perror("ini_parse");
            log_fatal("Can't load/parse ini file: %s",
                      utstring_body(obazl_ini_path));
            exit(EXIT_FAILURE);
        }
        if (ini_error) {
            log_error("Error parsing ini file");
            exit(EXIT_FAILURE);
        /* } else { */
        /*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); */
        }
    }

    utarray_new(src_files,&ut_str_icd);

    chdir(_wd);
    if (debug)
        log_debug("Set CWD to: %s\n", getcwd(NULL, 0));
}

EXPORT int config_handler(void* config, const char* section, const char* name, const char* value)
{
    /* log_debug("config_handler section %s: %s=%s", section, name, value); */
    struct configuration_s *pconfig = (struct configuration_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        if (verbose)
            log_debug("obazl version: %s", value);
        return 1;
    }

    if (MATCH("srcs", "dirs")) {
        /* log_debug("section: srcs; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            /* if (token[0] == '/') { */
            /*     log_error("Ini file: 'dir' values in section 'srcs' must be relative paths: %s", token); */
            /*     ini_error = true; */
            /*     return 0; */
            /* } else { */
                /* log_debug("pushing src dir: %s", token); */
                utarray_push_back(pconfig->src_dirs, &token);
                token = strtok(NULL, sep);
            /* } */
        }
        return 1;
    }

    if (MATCH("watch", "dirs")) {
        /* log_debug("section: watch; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'dir' values in section 'watch' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* log_debug("pushing watch dir: %s", token); */
                utarray_push_back(pconfig->watch_dirs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    /* if (MATCH("obazl", "repos")) { */
    /*     resolve_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "coq")) { */
    /*     resolve_coq_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "ocaml")) { */
    /*     resolve_ocaml_repos((char*)value); */
    /* } */

    /* if ( strncmp(section, "repo:", 5) == 0 ) { */
    /*     /\* printf("REPO section: %s (%s = %s)\n", section, name, value); *\/ */
    /*     char *the_repo = &section[5]; */

    /*     char *repo_dir = get_workspace_dir(the_repo); */
    /*     printf("repo: %s -> %s\n", the_repo, repo_dir); */

    /*     /\* tmp_repo = NULL; *\/ */
    /*     /\* HASH_FIND_STR(repo_map, the_repo, tmp_repo);  /\\* already in the hash? *\\/ *\/ */
    /*     /\* if (tmp_repo) { *\/ */
    /*     /\*     printf("%s -> %s\n", tmp_repo->name, tmp_repo->base_path); *\/ */
    /*     /\* } else { *\/ */
    /*     /\*     fprintf(stderr, "No WS repo found for '%s' listed in .obazlrc\n", the_repo); *\/ */
    /*     /\*     exit(EXIT_FAILURE); *\/ */
    /*     /\* } *\/ */
    /* } */

    /* if ( strcmp(section, "coqlibs") == 0 ) { */
    /*     struct lib_s *cl = calloc(1, sizeof *cl); */
    /*     cl->name = strdup(name); */
    /*     cl->path = strdup(value); */
    /*     pconfig->coqlibs[pconfig->libct] = cl; */
    /*     /\* printf("loaded lib %d (%p): %s -> %s\n", *\/ */
    /*     /\*        pconfig->libct, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct], *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->name, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->path); *\/ */
    /*     pconfig->libct++; */
    /* } */
    return 1;
}

