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

char *build_wd; /* BUILD_WORKING_DIRECTORY else NULL */
char *launch_dir; /* real launch dir */
/* path args passed to mibl relative to build_wd */

/* UT_string *ws_root; */
char *bws_root;                 /* base ws root */
char *ews_root;                 /* effective ws root */
char *traversal_root;           /* maybe not same as ws root */

/* UT_string *runfiles_root;       /\* bazel only *\/ */
/* UT_string *config_obazl; // obazl_d; */

#define MIBL    "mibl"
#define MIBL_S7 MIBL "/s7"
#define OIBL   "mibl"
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

/* UT_array *src_files;            /\* FIXME: put this in configuration_s? *\/ */
char *homedir;

/*
  FIXME: also deal with dune workspace roots
 */
EXPORT s7_pointer g_effective_ws_root(s7_scheme *s7,  s7_pointer args)
{
    char *dir;
    if ( s7_is_null(s7, args) ) {
        dir = getcwd(NULL, 0);
    } else {
        s7_int args_ct = s7_list_length(s7, args);
        if (args_ct == 1) {
            s7_pointer arg = s7_car(args);
            if (s7_is_string(arg)) {
                dir = s7_string(arg);
            }
        } else {
            // throw exception
        }
    }
    char *ews_root = effective_ws_root(dir);
    return s7_make_string(s7, ews_root);
}

char *effective_ws_root(char *dir)
{
   /* log_debug("effective_ws_root: %s", dir); */

   if (strncmp(homedir, dir, strlen(dir)) == 0) {
       log_debug("xxxx");
       return NULL;
   }

   UT_string *_ws_path;
   utstring_new(_ws_path);
   utstring_printf(_ws_path, "%s/%s", dir, "WORKSPACE.bazel");
   /* log_debug("Testing %s", utstring_body(_ws_path)); */
   int rc = access(utstring_body(_ws_path), R_OK);
    if (rc == 0) {
        /* log_debug("found %s", utstring_body(_ws_path)); */
        return dir;
    } else {
        utstring_new(_ws_path);
        utstring_printf(_ws_path, "%s/%s", dir, "WORKSPACE");
        int rc = access(utstring_body(_ws_path), R_OK);
        if (rc == 0) {
            /* log_debug("found %s", utstring_body(_ws_path)); */
            return dir;
        } else {
            return effective_ws_root(dirname(dir));
        }
    }
}

/*
 */
void _set_base_ws_root(void)
{
    bws_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (debug) log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bws_root);

    if (bws_root == NULL) {
        /* we're not in Bazel rte, but we may be in a Bazel WS. So
           look for nearest WORKSPACE.bazel (or WORKSPACE) file
           ancestor. */
        bws_root = effective_ws_root(getcwd(NULL,0));
        if (debug)
            log_debug("Found WS file at %s", bws_root);
    }
    ews_root = bws_root;  /* by default, effective ws == base ws */
    if (debug)
        log_debug("base ws root: %s", bws_root);

    /* utstring_new(ws_root); */
    /* if (bws_root == NULL) */
    /*     utstring_printf(ws_root, "%s", getcwd(NULL, 0)); */
    /* else */
    /*     utstring_printf(ws_root, "%s", bws_root); */
}

/* should always be called first, so launch dir gets set to cwd */
EXPORT void bazel_configure(void) // char *_exec_root)
{
    /* log_debug("bazel_configure"); */
    launch_dir = getcwd(NULL, 0);

    build_wd = getenv("BUILD_WORKING_DIRECTORY");
    if (debug) log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd);

    if (build_wd == NULL) {
        /* running outside of bazel */
        if (debug) log_debug("BUILD_WORKING_DIRECTORY: null");
        build_wd = launch_dir;
    }

    if (debug) {
        log_debug("build_wd: %s (=BUILD_WORKING_DIRECTORY)", build_wd);
        log_debug("launch_dir: %s", launch_dir);
    }

    homedir = getenv("HOME");
    /* log_debug("HOME: %s", homedir); */

    /* utstring_new(runfiles_root); */
    /* utstring_printf(runfiles_root, "%s", getcwd(NULL, 0)); */
    /* if (debug) */
    /*     log_debug("runfiles_root: %s", utstring_body(runfiles_root)); */

    _set_base_ws_root();

    /* **************** */
    /* project-local .config/miblrc config file */
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s",
                    bws_root, MIBL_INI_FILE);

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        if (verbose || debug)
            log_warn("NOT FOUND: miblrc config file %s",
                     utstring_body(obazl_ini_path));
        //FIXME: also look in XDG_CONFIG_HOME
    } else {
        ini_error = false;
        if (verbose || debug)
            log_info("FOUND: miblrc config file %s",
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
    /* utarray_new(src_files,&ut_str_icd); */
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

