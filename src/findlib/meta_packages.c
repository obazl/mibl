#include <stdbool.h>

#include "log.h"
#include "utarray.h"
#include "meta_packages.h"

#if EXPORT_INTERFACE
struct obzl_meta_package {
    char *name;
    char *path;
    char *directory;            /* subdir */
    char *metafile;
    obzl_meta_entries *entries;          /* list of struct obzl_meta_entry */
};
#endif

EXPORT char *obzl_meta_package_name(obzl_meta_package *_pkg)
{
    return _pkg->name;
}

EXPORT char *obzl_meta_package_dir(obzl_meta_package *_pkg)
{
    return _pkg->directory;
}

EXPORT char *obzl_meta_package_directory_prop(obzl_meta_package *_pkg)
{
    /* char *d = obzl_meta_directory_property(_pkg->entries); */

    return _pkg->directory;
}

EXPORT char *obzl_meta_package_src(obzl_meta_package *_pkg)
{
    return _pkg->metafile;
}

EXPORT obzl_meta_entries *obzl_meta_package_entries(obzl_meta_package *_pkg)
{
    return _pkg->entries;
}

/* **************************************************************** */
EXPORT int obzl_meta_package_subpkg_count(obzl_meta_package *_pkg)
{
    /* obzl_meta_entries *entries = _pkg->entries; */
    obzl_meta_entry *e = NULL;
    /* obzl_meta_package *subpkg = NULL; */

    int pkg_ct = 0;

    for (int i = 0; i < obzl_meta_entries_count(_pkg->entries); i++) {
        e = obzl_meta_entries_nth(_pkg->entries, i);
        if (e->type == OMP_PACKAGE) {
            pkg_ct++;
            /* subpkg = e->package; */
            pkg_ct += obzl_meta_package_subpkg_count(e->package);
        }
    }
    return pkg_ct;
}

/* **************************************************************** */
EXPORT bool obzl_meta_package_has_archives(obzl_meta_package *_pkg)
{
    //FIXME: use a has_archives flag

    /* obzl_meta_entries *entries = _pkg->entries; */
    obzl_meta_entry *e = NULL;
    for (int i = 0; i < obzl_meta_entries_count(_pkg->entries); i++) {
        e = obzl_meta_entries_nth(_pkg->entries, i);
        if (e->type == OMP_PROPERTY) {
            if (strncmp(e->property->name, "archive", 7) == 0) {
                return true;
            }
        }
    }
    return false;
}

EXPORT bool obzl_meta_package_has_plugins(obzl_meta_package *_pkg)
{
    //FIXME: use a has_plugins flag

    /* obzl_meta_entries *entries = _pkg->entries; */
    obzl_meta_entry *e = NULL;
    for (int i = 0; i < obzl_meta_entries_count(_pkg->entries); i++) {
        e = obzl_meta_entries_nth(_pkg->entries, i);
        if (e->type == OMP_PROPERTY) {
            if (strncmp(e->property->name, "plugin", 7) == 0) {
                return true;
            }
        }
    }
    return false;
}

EXPORT bool obzl_meta_package_has_subpackages(obzl_meta_package *_pkg)
{
    //FIXME: use a has_subpackages flag
    log_debug("obzl_meta_package_has_subpackages");
    obzl_meta_entries *entries = _pkg->entries;
    obzl_meta_entry *e = NULL;

    for (int i = 0; i < obzl_meta_entries_count(entries); i++) {
        e = obzl_meta_entries_nth(_pkg->entries, i);
        log_debug("entry type: %d", e->type);
        if (e->type == OMP_PROPERTY) {
            log_debug("Property entry: %s", e->property->name);
        } else {
            if (e->type == OMP_PACKAGE) {
                log_debug("Package entry: %s", e->package->name);
                return true;
            }
        }
    }
    return false;
}

EXPORT obzl_meta_property *obzl_meta_package_property(obzl_meta_package *_pkg, char *_name)
{
#if DEBUG_PACKAGES
    log_trace("obzl_meta_package_property('%s')", _name);
#endif
    /* utarray_find requires a sort; not worth the cost */
    obzl_meta_entry *e = NULL;
    for (int i = 0; i < obzl_meta_entries_count(_pkg->entries); i++) {
        e = obzl_meta_entries_nth(_pkg->entries, i);
        if (e->type == OMP_PROPERTY) {
            if (strncmp(e->property->name, _name, 256) == 0) {
                return e->property;
            }
        }
        /* log_debug("iteration %d", i); */
    }
    return NULL;
}

obzl_meta_values *resolve_setting_values(obzl_meta_setting *_setting,
                                         obzl_meta_flags *_flags,
                                         obzl_meta_settings *_settings)
{
    log_debug("resolve_setting_values, opcode: %d", _setting->opcode);
    obzl_meta_values * vals = obzl_meta_setting_values(_setting);
    /* log_debug("vals ct: %d", obzl_meta_values_count(vals)); */
    if (_setting->opcode == OP_SET)
        return vals;

    /* else OP_UPDATE */

    UT_array *resolved_values;
    utarray_new(resolved_values, &ut_str_icd);
    utarray_concat(resolved_values, vals->list);

    /* for each flag, search settings for matching flag */
    int settings_ct = obzl_meta_settings_count(_settings);
    struct obzl_meta_setting *a_setting;

    int flags_ct    = obzl_meta_flags_count(_flags);
    /* printf("\tflags_ct: %d\n", flags_ct); */
    struct obzl_meta_flag *a_flag = NULL;

    for (int i=0; i < flags_ct; i++) {
        a_flag = obzl_meta_flags_nth(_flags, i);
        for (int j=0; j < settings_ct; j++) {
            a_setting = obzl_meta_settings_nth(_settings, j);
            if (a_setting == _setting) continue; /* don't match self */

            obzl_meta_flags *setting_flags = obzl_meta_setting_flags(a_setting);

            if (setting_flags == NULL) {
                /* always match no flags, e.g. 'requires = "findlib.internal"' */
                obzl_meta_values *vs = obzl_meta_setting_values(a_setting);
                /* printf("xconcatenating\n"); fflush(stdout); fflush(stderr); */
                utarray_concat(resolved_values, vs->list);
                continue;
            }

            int ct = obzl_meta_flags_count(setting_flags);
            if (ct > 1) {
                /* only try to match singletons? */
                continue;
            }

            obzl_meta_flag *setting_flag = obzl_meta_flags_nth(setting_flags, 0);
            if (setting_flag->polarity == a_flag->polarity) {
                if (strncmp(setting_flag->s, a_flag->s, 32) == 0) {
                    log_debug("matched flag");
                    /* we have found a setting with exactly one flag, that matches the search flag */
                    /* now we check the setting's opcode - it should always be SET? */
                    /* then add its values list */
                    obzl_meta_values *vs = obzl_meta_setting_values(a_setting);
                    utarray_concat(resolved_values, vs->list);
                    /* dump_setting(4, a_setting); */
                }
            }
        }
    }
    obzl_meta_values *new_values = (obzl_meta_values*)calloc(sizeof(obzl_meta_values),1);
    new_values->list = resolved_values;
    return new_values;
}

/* **************** */
EXPORT UT_array *pkg_deps(struct obzl_meta_package *_pkg,
                          UT_array *_deps)
{
    log_trace("pkg_deps");
    obzl_meta_entries *entries = obzl_meta_package_entries(_pkg);

    char *property = "requires";
    struct obzl_meta_property *deps_prop = NULL;
    deps_prop = obzl_meta_entries_property(entries, property);
    if ( deps_prop == NULL ) {
        log_warn("Prop '%s' not found for pkg: %s.", property, _pkg->name);
        return NULL;
    }

    obzl_meta_settings *settings = obzl_meta_property_settings(deps_prop);
    obzl_meta_setting *setting = NULL;

    int settings_ct = obzl_meta_settings_count(settings);
    if (settings_ct == 0) {
        log_info("No settings for %s", obzl_meta_property_name(deps_prop));
        return NULL;
    } else {
        log_info("settings count: %d", settings_ct);
    }

    int settings_no_ppx_driver_ct = obzl_meta_settings_flag_count(settings, "ppx_driver", false);

    settings_ct -= settings_no_ppx_driver_ct;

    log_info("settings count w/o ppx_driver: %d", settings_ct);

    if (settings_ct == 0) {
        log_info("No deps for %s", obzl_meta_property_name(deps_prop));
        return NULL;
    }

    obzl_meta_values *vals;
    obzl_meta_value *dep_name = NULL;

    UT_string *condition_name;
    utstring_new(condition_name);

    log_debug("iterating settings");
    for (int i = 0; i < settings_ct; i++) {
        setting = obzl_meta_settings_nth(settings, i);
        log_debug("setting %d", i+1);

        obzl_meta_flags *flags = obzl_meta_setting_flags(setting);
        /* int flags_ct; // = 0; */
        if (flags != NULL) {
            /* register_flags(flags); // why? */
            int flags_ct = obzl_meta_flags_count(flags);
            log_debug("flags_ct: %d", flags_ct);
        }

        if (obzl_meta_flags_has_flag(flags, "ppx_driver", false)) {
            continue;
        }

        bool has_conditions;
        if (flags == NULL)
            utstring_printf(condition_name, "//conditions:default");
        else
            has_conditions = obzl_meta_flags_to_selection_label(flags, condition_name);

        char *condition_comment = obzl_meta_flags_to_comment(flags);
        log_debug("condition_comment: %s", condition_comment);

        /* 'requires' usually has no flags; when it does, empirically we find only */
        /*   ppx pkgs: ppx_driver, -ppx_driver */
        /*   pkg 'batteries': requires(mt) */
        /*   pkg 'num': requires(toploop) */
        /*   pkg 'findlib': requires(toploop), requires(create_toploop) */

        /* Multiple settings on 'requires' means multiple flags; */
        /* empirically, this only happens for ppx packages, typically as */
        /* requires(-ppx_driver,-custom_ppx) */
        /* the (sole?) exception is */
        /*   pkg 'threads': requires(mt,mt_vm), requires(mt,mt_posix) */

        /* if (settings_ct > 1) { */
        /*     fprintf(ostream, "%*s\"X%s%s\": [ ## predicates: %s\n", */
        /*             (1+level)*spfactor, sp, */
        /*             utstring_body(condition_name), */
        /*             (has_conditions)? "" : "", */
        /*             condition_comment); */
        /* } */

        vals = obzl_meta_setting_values(setting);
        /* vals = resolve_setting_values(setting, flags, settings); */
        /* vals = obzl_meta_setting_values(setting); */
        log_debug("vals ct: %d", obzl_meta_values_count(vals));
        /* dump_values(0, vals); */
        /* now we handle UPDATE settings */

        log_debug("iterating values");
        for (int j = 0; j < obzl_meta_values_count(vals); j++) {
            dep_name = obzl_meta_values_nth(vals, j);
            /* log_debug("property val[%d]: '%s'", j, *dep_name); */

            char *s = (char*)*dep_name;

            log_debug("DEP: %s", s);

            /* special case: uchar */
            if ((strncmp(s, "uchar", 5) == 0)
                && (strlen(s) == 5)){
                /* log_debug("OMITTING UCHAR dep"); */
                continue;
            }

            /* special case: threads */
            /* if ((strncmp(s, "threads", 7) == 0) */
            /*     && (strlen(s) == 7)){ */
            /*     /\* log_debug("OMITTING THREADS dep"); *\/ */
            /*     continue; */
            /* } */

            /* while (*s) { */
            /*     /\* printf("s: %s\n", s); *\/ */
            /*     if(s[0] == '.') { */
            /*         /\* log_info("Hit"); *\/ */
            /*         s[0] = '/'; */
            /*     } */
            /*     s++; */
            /* } */

            /* emit 'deps' attr labels */

            /* if (settings_ct > 1) { */
            /*     fprintf(ostream, "%*s\"@FIXME: %s//%s\",\n", */
            /*             (2+level)*spfactor, sp, *dep_name, pkg); */
            /* } else { */
            /*     /\* first convert pkg string *\/ */
            /*     /\* char *s = (char*)*dep_name; *\/ */
            /*     /\* char *tmp; *\/ */
            /*     /\* while(s) { *\/ */
            /*     /\*     tmp = strchr(s, '/'); *\/ */
            /*     /\*     if (tmp == NULL) break; *\/ */
            /*     /\*     *tmp = '.'; *\/ */
            /*     /\*     s = tmp; *\/ */
            /*     /\* } *\/ */
            /*     /\* then extract target segment *\/ */
            /*     char *delim1 = strchr(*dep_name, '/'); */
            /*     /\* log_debug("WW *dep_name: %s; delim1: %s, null? %d\n", *\/ */
            /*     /\*         *dep_name, delim1, (delim1 == NULL)); *\/ */

            /*     if (delim1 == NULL) { */
            /*         if (special_case_multiseg_dep(ostream, dep_name, delim1)) */
            /*             continue; */
            /*         else { */
            /*             /\* single-seg pkg, e.g. ptime  *\/ */

            /*             // handle core libs: dynlink, str, unix, etc. */
            /*             if ((strncmp(*dep_name, "bigarray", 8) == 0) */
            /*                 && strlen(*dep_name) == 8) { */
            /*                 fprintf(ostream, "%*s\"@ocaml//bigarray\",\n", */
            /*                         (1+level)*spfactor, sp); */
            /*             } else { */
            /*                 if ((strncmp(*dep_name, "unix", 4) == 0) */
            /*                     && strlen(*dep_name) == 4) { */
            /*                     fprintf(ostream, "%*s\"@ocaml//unix\",\n", */
            /*                             (1+level)*spfactor, sp); */
            /*                 } else { */
            /*                     //NOTE: we use @foo instead of @foo//:foo */
            /*                     // seems to work */
            /*                     /\* fprintf(ostream, *\/ */
            /*                     /\*         "%*s\"@%s\",\n", *\/ */
            /*                     /\*         (1+level)*spfactor, sp, *\/ */
            /*                     /\*         *dep_name); *\/ */
            /*                     fprintf(ostream, */
            /*                             "%*s\"@%s//lib/%s\",\n", */
            /*                             (1+level)*spfactor, sp, */
            /*                             *dep_name, *dep_name); */
            /*                 } */
            /*             } */
            /*         } */
            /*     } else { */
            /*         /\* multi-seg pkg, e.g. lwt.unix, ptime.clock.os *\/ */
            /*         if (special_case_multiseg_dep(ostream, dep_name, delim1)) */
            /*             continue; */
            /*         else { */
            /*             int repo_len = delim1 - (char*)*dep_name; */
            /*             fprintf(ostream, "%*s\"@%.*s//lib/%s\",\n", */
            /*                     (1+level)*spfactor, sp, */
            /*                     repo_len, */
            /*                     *dep_name, */
            /*                     delim1+1); */
            /*             /\* (1+level)*spfactor, sp, repo, pkg, *dep_name); *\/ */
            /*         } */
            /*     } */
            /* } */
        }
        /* if (settings_ct > 1) { */
        /*     fprintf(ostream, "%*s],\n", (1+level)*spfactor, sp); */
        /* } */
        free(condition_comment);
    }

    return _deps;
}
