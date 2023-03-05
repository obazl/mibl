/* globals */

#include <stdbool.h>
#include <unistd.h>

/* #if EXPORT_INTERFACE */
#include "utarray.h"
#include "utstring.h"
#include "log.h"
/* #endif */

#include "ini.h"

#include "config_mibl.h"

extern const UT_icd ut_str_icd;

extern bool bzl_mode;
extern int  verbosity;

#if INTERFACE
#define HOME_MIBL ".mibl"
#define MIBL_INI_FILE ".config/miblrc"
#endif

#define MIBL    "mibl"
#define MIBL_SCHEMA_VERSION "0.1.0"
#define MIBL_S7 MIBL "/s7"
/* #define OIBL   "mibl" */

UT_string *config_mibl;        /* work string */

#if EXPORT_INTERFACE
struct mibl_config_s {
    char *schema_version;
    int libct;
    bool emit_bazel;
    bool emit_mibl;
    bool emit_parsetree;
    bool log_mibl;
    bool log_parsetree;
    bool log_exports;
    bool log_starlark;
    UT_array *pkgs;
    UT_array *exclude_dirs;      /* overrides include_dirs */
    UT_array *include_dirs;
    UT_array *watch_dirs;       /* string list */
    /* struct lib_s *ocamllibs[10]; /\* is 10 enough? *\/ */
    /* struct lib_s *coqlibs[10]; /\* is 10 enough? *\/ */
};
#endif

struct mibl_config_s mibl_config = {
    .schema_version = MIBL_SCHEMA_VERSION,
    .emit_parsetree = false,
    .emit_mibl      = false,
    .emit_bazel  = true,
    .log_exports      = false,
    .log_mibl      = false,
    .log_parsetree = false,
    .log_starlark  = false,
    .libct          = 0
};

LOCAL int _miblrc_handler(void* config, const char* section, const char* name, const char* value)
{
#if defined(DEBUG_TRACE)
    if (trace_mibl)
        log_debug("config_handler section %s: %s=%s", section, name, value);
#endif

    struct mibl_config_s *pconfig = (struct mibl_config_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        if (verbose)
            log_debug("obazl version: %s", value);
        return 1;
    }

    if (MATCH("mibl", "emit")) {
        if (verbose && verbosity > 1) log_debug("miblrc [mibl] emit: %s", value);
        if (strncmp(value, "starlark", 8) == 0) {
            pconfig->emit_bazel = true;
        }
        else if (strncmp(value, "none", 4) == 0) {
            pconfig->emit_parsetree = false;
            pconfig->emit_mibl = false;
            pconfig->emit_bazel = false;
        }
        else if (strncmp(value, "mibl", 4) == 0) {
            pconfig->emit_mibl = true;
        }
        else if (strncmp(value, "parsetree", 9) == 0) {
            pconfig->emit_parsetree = true;
        } else {
            log_error("mibl ini file: invalid value %s for 'emit' in section 'mibl'; allowed values: parsetree, mibl, starlark, none", value);
            ini_error = true;
            return 0;
        }

        return 1;
    }

    if (MATCH("mibl", "log")) {
        if (verbose && verbosity > 1) log_debug("miblrc [mibl] log: %s", value);
        if (strncmp(value, "exports", 7) == 0) {
            pconfig->log_exports = true;
        }
        else if (strncmp(value, "mibl", 4) == 0) {
            pconfig->log_mibl = true;
        }
        else if (strncmp(value, "parsetree", 9) == 0) {
            pconfig->log_parsetree = true;
        }
        else if (strncmp(value, "starlark", 8) == 0) {
            pconfig->log_starlark = true;
        }
        else {
            log_error("mibl ini file: invalid value %s for 'log' in section 'mibl'; allowed values: exports, mibl, parsetree", value);
            ini_error = true;
            return 0;
        }
        return 1;
    }

    if (MATCH("mibl", "pkg")) {
#if defined(DEBUG_TRACE)
        if (debug_mibl) log_debug("section: mibl; entry: pkg");
#endif
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'pkg' values in section 'mibl' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                log_debug("miblrc pushing pkg: %s", token);
                utarray_push_back(pconfig->pkgs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    /* FIXME: normalize filepaths. remove leading ./ and embedded ../ */
    /* disallow leading / and ../ */
    if (MATCH("srcs", "exclude")) {
#if defined(DEBUG_TRACE)
        if (debug_mibl)
            log_debug("section: srcs; entry: exclude; val: %s", value);
#endif
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'exclude' values in section 'srcs' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* log_debug("pushing exclude dir: %s", token); */
                utarray_push_back(pconfig->exclude_dirs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    if (MATCH("srcs", "include")) {
        /* log_debug("section: srcs; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'include' values in section 'srcs' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* FIXME: support concats, e.g. foo/bar:foo/baz */
                utarray_push_back(pconfig->include_dirs, &token);
                token = strtok(NULL, sep);
            }
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

EXPORT void mibl_configure(void)
{
#if defined(DEBUG_TRACE)
    if (debug_mibl)
        log_debug("mibl_configure");
#endif
    /* **************** */
    /* project-local .config/miblrc config file */

    utarray_new(mibl_config.pkgs, &ut_str_icd);
    utarray_new(mibl_config.exclude_dirs, &ut_str_icd);
    utarray_new(mibl_config.include_dirs, &ut_str_icd);
    utarray_new(mibl_config.watch_dirs, &ut_str_icd);

    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s",
                    bws_root, MIBL_INI_FILE);

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        if (verbose)
            log_warn("NOT FOUND: miblrc config file %s",
                     utstring_body(obazl_ini_path));
        //FIXME: also look in XDG_CONFIG_HOME
    } else {
        ini_error = false;
        /* if (verbose) */
        /*     log_info("loading miblrc config file: %s", */
        /*              utstring_body(obazl_ini_path)); */

        /* PARSE INI FILE */
        rc = ini_parse(utstring_body(obazl_ini_path), _miblrc_handler, &mibl_config);

        if (rc < 0) {
            //FIXME: deal with missing .config/miblrc
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
        if (verbose && verbosity > 1)
            log_info("Loaded miblrc config file: %s",
                     utstring_body(obazl_ini_path));
    }

    utarray_sort(mibl_config.include_dirs, strsort);
    utarray_sort(mibl_config.exclude_dirs, strsort);

#if defined(DEBUG_MIBL)
    if (debug_mibl)
        dump_mibl_config();
#endif
}
