/* common config stuff */

#include <stdbool.h>
#include "globals.h"

bool verbose = false;
bool bzl_mode = false;
bool enable_jsoo = true;

int rc;
int verbosity;
int errnum;
int indent = 2;
int delta = 2;
int level = 0;
int spfactor = 4;
char *sp = " ";

int dunefile_ct = 0;
int file_ct = 0;
int dir_ct  = 0;

