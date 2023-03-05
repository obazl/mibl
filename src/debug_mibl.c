#include <stdbool.h>

#include "log.h"
#include "utarray.h"

#include "debug_mibl.h"

bool debug_mibl = false;
bool debug_mibl_crawl = false;
bool debug_miblrc   = false;

bool trace_mibl   = false;

void dump_mibl_config(void)
{
    log_debug("dump_mibl_config:");
    char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.include_dirs,p))) {
        log_debug("  include: %s",*p);
    }
    p = NULL;
    while ( (p=(char**)utarray_next(mibl_config.exclude_dirs,p))) {
        log_debug("  exclude: %s",*p);
    }
    log_debug("dump_mibl_config finished");
}

