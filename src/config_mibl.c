/* globals */

#include <stdbool.h>
#include <unistd.h>

#include "log.h"

#include "ini.h"

/* #include "s7.h" */

#include "config_mibl.h"

extern const UT_icd ut_str_icd;

extern bool bzl_mode;
extern int  verbosity;

#if defined(DEBUG_TRACE)
bool mibl_debug_mibl = false;
bool mibl_debug_miblrc   = false;
/* bool mibl_debug_traversal = false; */
/* bool mibl_debug_traversal_opam = false; */
/* bool mibl_trace_mibl   = false; */
#endif

#if INTERFACE
#define HOME_MIBL ".mibl"
#define MIBL_INI_FILE ".config/miblrc"
#endif

#define MIBL    "mibl"
#define MIBL_SCHEMA_VERSION "0.1.0"
#define MIBL_S7 MIBL "/s7"
/* #define OIBL   "mibl" */

/* s7_scheme *s7; */

UT_string *config_mibl;        /* work string */

#if EXPORT_INTERFACE
#include "utstring.h"
#include "utarray.h"

struct mibl_config_s {
    char *schema_version;
    int libct;
    bool load_miblrc; // allows --no-miblrc
    //FIXME: remove debug flags, they're globals guarded by DEBUG_TRACE?
    bool debug_ppx;
    bool debug_dune_rules;
    bool debug_deps;
    //
    bool halt_after_parsetree;
    //FIXME emits: all but parsetree are s7 global vars
    bool emit_starlark;
    bool emit_mibl;
    bool emit_parsetree;
    //FIXME show: remove all, they're s7 globals
    bool show_project;
    bool show_parsetree;
    bool show_exports;
    bool show_starlark;
    UT_array *pkgs;
    UT_array *exclude_dirs;      /* overrides include_dirs */
    UT_array *include_dirs;
    UT_array *watch_dirs;       /* string list */
    /* struct lib_s *ocamllibs[10]; /\* is 10 enough? *\/ */
    /* struct lib_s *coqlibs[10]; /\* is 10 enough? *\/ */
};
#endif

/* global singleton */
struct mibl_config_s mibl_config = {
    .schema_version   = MIBL_SCHEMA_VERSION,
    .load_miblrc      = true,
    .debug_ppx        = false,
    .debug_dune_rules = false,
    .debug_deps       = false,
    .halt_after_parsetree = false,
    .emit_parsetree   = false,
    .emit_mibl        = false,
    .emit_starlark    = false,
    .show_exports     = false,
    .show_project     = false,
    .show_parsetree   = false,
    .show_starlark    = false,
    .libct            = 0
};

// returns 1 on success
LOCAL int _miblrc_handler(void* config, const char* section, const char* name, const char* value)
{
#if defined(DEBUG_TRACE)
    if (mibl_debug_miblrc)
        log_trace("_miblrc_handler, section %s: %s=%s", section, name, value);
#endif

    struct mibl_config_s *pconfig = (struct mibl_config_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        if (verbose)
            log_debug("obazl version: %s", value);
        return 1;
    }

    if (MATCH("mibl", "flag")) {
        if (verbose && verbosity > 1)
            log_debug("miblrc [mibl] debug: %s", value);

        //FIXME? directly use mibl_s7_set_flag instead of mibl_config struct
        //FIXME? we do not want to depend on s7 here, so we need to store any flags from miblrc into mibl_config

        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            /* log_debug("miblrc [mibl] flags token: %s", token); */
            mibl_s7_set_flag(token, true);
            token = strtok(NULL, sep);
        }
        return 1;
    }

    if (MATCH("mibl", "debug")) {
        if (verbose && verbosity > 1) log_debug("miblrc [mibl] debug: %s", value);
        if (strncmp(value, "deps", 4) == 0) {
            pconfig->debug_deps = true;
        }
    }

    if (MATCH("mibl", "emit")) {
        if (verbose && verbosity > 1) log_debug("miblrc [mibl] emit: %s", value);
        if (strncmp(value, "starlark", 8) == 0) {
            pconfig->emit_starlark = true;
        }
        else if (strncmp(value, "none", 4) == 0) {
            pconfig->emit_parsetree = false;
            pconfig->emit_mibl = false;
            pconfig->emit_starlark = false;
        }
        else if (strncmp(value, "mibl", 4) == 0) {
            pconfig->emit_mibl = true;
        }
        else if (strncmp(value, "parsetree", 9) == 0) {
            pconfig->emit_parsetree = true;
        } else {
            log_error("mibl ini file: invalid value %s for 'emit' in section 'mibl'; allowed values: parsetree, mibl, starlark, none", value);
            /* ini_error = true; */
            return 0;
        }
        return 1;
    }

    if (MATCH("mibl", "halt-after")){
        if (strncmp(value, "parsetree", 9) == 0) {
            pconfig->halt_after_parsetree = true;
        }
    }

    if (MATCH("mibl", "log")) {
        if (verbose && verbosity > 1) log_debug("miblrc [mibl] log: %s", value);
        if (strncmp(value, "exports", 7) == 0) {
            pconfig->show_exports = true;
        }
        else if (strncmp(value, "project", 7) == 0) {
            pconfig->show_project = true;
        }
        else if (strncmp(value, "parsetree", 9) == 0) {
            pconfig->show_parsetree = true;
        }
        else if (strncmp(value, "starlark", 8) == 0) {
            pconfig->show_starlark = true;
        }
        else {
            log_error("mibl ini file: invalid value %s for 'log' in section 'mibl'; allowed values: exports, mibl, parsetree", value);
            /* ini_error = true; */
            return 0;
        }
        return 1;
    }

    if (MATCH("mibl", "pkg")) {
#if defined(DEBUG_TRACE)
        if (mibl_debug_mibl) log_debug("section: mibl; entry: pkg");
#endif
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'pkg' values in section 'mibl' must be relative paths: %s", token);
                /* ini_error = true; */
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
        if (mibl_debug_miblrc)
            log_debug("section: srcs; entry: exclude; val: %s", value);
#endif
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'exclude' values in section 'srcs' must be relative paths: %s", token);
                /* ini_error = true; */
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
                /* ini_error = true; */
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
                /* ini_error = true; */
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

EXPORT void mibl_check_tools(void) {
    /* is shell available? */
    int rc = system(NULL);
    if (rc == 0) {
        fprintf(stderr, "No system shell available\n");
        exit(EXIT_FAILURE);
    }

    /* FIXME: not portable.  instead, scan $PATH...? */
    /* if (system("which ocamldep > /dev/null 2>&1")) { */
    /*     fprintf(stderr, "Cmd 'ocamldep' not found, but it is required by the conversion tool. If it is installed, try running 'eval $(opam env)'.\n"); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    /* if (system("which foobar > /dev/null 2>&1")) { */
    /*     fprintf(stderr, RED "ERROR: " CRESET "Command 'foobar' not found. Please run 'opam install ocamldep'.\n"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
}

EXPORT void show_mibl_config(void)
{
    log_info(GRN "mibl configuration summary:" CRESET);

    log_info("\tschema_version: %s"   , mibl_config.schema_version);
    log_info("\tload_miblrc: %d"      , mibl_config.load_miblrc);
    log_info("\tdebug_ppx: %d"        , mibl_config.debug_ppx);
    log_info("\tdebug_dune_rules: %d" , mibl_config.debug_dune_rules);
    log_info("\tdebug_deps: %d"       , mibl_config.debug_deps);
    log_info("\temit_parsetree: %d"   , mibl_config.emit_parsetree);
    log_info("\temit_mibl: %d"        , mibl_config.emit_mibl);
    log_info("\temit_starlark: %d"    , mibl_config.emit_starlark);
    log_info("\thalt_after_parsetree: %d", mibl_config.halt_after_parsetree);
    log_info("\tshow_exports: %d"     , mibl_config.show_exports);
    log_info("\tshow_project: %d"     , mibl_config.show_project);
    log_info("\tshow_parsetree: %d"   , mibl_config.show_parsetree);
    log_info("\tshow-starlark: %d"    , mibl_config.show_starlark);
    log_info("\tlibct: %d"            , mibl_config.libct);

    char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.pkgs,p))) {
        log_info("  pkg: %s",*p);
    }
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.include_dirs,p))) {
        log_info("  include: %s",*p);
    }
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.exclude_dirs,p))) {
        log_info("  exclude: %s",*p);
    }
    log_info(GRN "End mibl configuration summary." CRESET);
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.watch_dirs,p))) {
        log_info("  pkg: %s",*p);
    }
    fflush(NULL);
}

LOCAL void _load_user_mibl_config(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace(BLU "_load_user_mibl_config" CRESET);
#endif
    // WARNING: $HOME not accessible when BAZEL_TEST defined
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path,
                    "%s/.config/mibl/miblrc",
                    getenv("HOME"));

    int rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        if (verbose)
            log_warn("NOT FOUND: user miblrc config file %s",
                     utstring_body(obazl_ini_path));
        return;
    }

    if (verbose && verbosity > 1)
        log_info("loading user miblrc config file: %s",
                 utstring_body(obazl_ini_path));

    /* PARSE INI FILE */
    rc = ini_parse(utstring_body(obazl_ini_path), _miblrc_handler, &mibl_config);

    if (rc < 0) { // libinih convention
        //FIXME: deal with missing .config/miblrc
        perror("ini_parse");
        log_fatal("Can't load/parse ini file: %s",
                  utstring_body(obazl_ini_path));
        exit(EXIT_FAILURE);
    }
    if (verbose)
        log_info("Loaded user miblrc: %s",
                 utstring_body(obazl_ini_path));
    /* } */

    utarray_sort(mibl_config.include_dirs, strsort);
    utarray_sort(mibl_config.exclude_dirs, strsort);
}

LOCAL void _load_ws_mibl_config(void)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace(BLU "_load_ws_mibl_config" CRESET);
#endif
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s",
                    rootws, MIBL_INI_FILE);

    int rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        //FIXME: also look in XDG_CONFIG_HOME
        utstring_renew(obazl_ini_path);
        utstring_printf(obazl_ini_path, "%s/.miblrc", rootws);
        rc = access(utstring_body(obazl_ini_path), R_OK);
        if (rc) {
            if (verbose && verbosity > 1)
                log_info("Project miblrc not found %s",
                         utstring_body(obazl_ini_path));
            return;
        }
    }

    if (verbose)
        log_info("loading miblrc config file: %s",
                 utstring_body(obazl_ini_path));

    /* PARSE INI FILE */
    rc = ini_parse(utstring_body(obazl_ini_path), _miblrc_handler, &mibl_config);

    if (rc < 0) { // libinih convention
        //FIXME: deal with missing .config/miblrc
        perror("ini_parse");
        log_fatal("Can't load/parse ini file: %s",
                  utstring_body(obazl_ini_path));
        exit(EXIT_FAILURE);
    }
    if (verbose)
        log_info("Loaded project miblrc: %s",
                 utstring_body(obazl_ini_path));
    /* } */

    utarray_sort(mibl_config.include_dirs, strsort);
    utarray_sort(mibl_config.exclude_dirs, strsort);
}

EXPORT void mibl_configure(void)
{
    /* mibl_debug_mibl = true; */
    /* mibl_debug_miblrc = true; */
#if defined(DEBUG_TRACE)
    if (mibl_trace)
        log_trace(UBLU "mibl_configure" CRESET);
#endif
    /* **************** */
    /* project-local .config/miblrc config file */

    utarray_new(mibl_config.pkgs, &ut_str_icd);
    utarray_new(mibl_config.exclude_dirs, &ut_str_icd);
    utarray_new(mibl_config.include_dirs, &ut_str_icd);
    utarray_new(mibl_config.watch_dirs, &ut_str_icd);

    /* if (getenv("BAZEL_TEST")) goto summary; */

    if (mibl_config.load_miblrc) {
        if ( getenv("BAZEL_TEST") ) {
            if ( getenv("HOME") ) {
                // bazel run test_target
                _load_user_mibl_config();
                _load_ws_mibl_config();
            }
            // else bazel test test_target
        } else {
            // bazel run non_test_target
            _load_user_mibl_config();
            _load_ws_mibl_config();
        }
    }

    if (verbose && verbosity > 1) {
        show_mibl_config();
    }
}
