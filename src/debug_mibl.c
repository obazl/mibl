/* #include "log.h" */
/* #include "utarray.h" */

#include "debug_mibl.h"

void dump_mibl_config(void)
{
    log_debug("mibl_config:");
    char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.include_dirs,p))) {
        log_debug("  include: %s",*p);
    }
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.exclude_dirs,p))) {
        log_debug("  exclude: %s",*p);
    }
}

