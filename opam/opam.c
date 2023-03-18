#include <unistd.h>

/* #include "bazel_config.h" */
/* #include "mibl_config.h" */
/* #include "s7_config.h" */
/* #include "ansi_colors.h" */
/* #include "load_project.h" */

#include "ini.h"
#include "log.h"
#include "mibl.h"
#include "libopam.h"
#include "opam.h"

extern bool debug;
extern bool trace;
extern bool verbose;

/* extern char *ews_root; */
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

s7_pointer _load_load_opam(s7_scheme *s7)
{
    s7_pointer _load_opam;
    /* load-opam defined in c */
    _load_opam = s7_name_to_value(s7, "load-opam");
    if (_load_opam == s7_undefined(s7)) {
        log_error("unbound symbol: load-opam");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "load-opam")));
    }
    return _load_opam;
}

int main(int argc, char *argv[])
{
    char *opts = "p:hdtvx";
    int opt;
    char *pkgarg = NULL;

    bool exit_on_error = false;

    while ((opt = getopt(argc, argv, opts)) != -1) {
        switch (opt) {
        case '?':
            fprintf(stderr, "uknown opt: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case ':':
            fprintf(stderr, "uknown option: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case 'd':
            debug = true;
            break;
        case 'h':
            /* _print_usage(); */
            printf("help msg ...\n");
            exit(EXIT_SUCCESS);
            break;
        case 'p':
            log_debug("package: %s\n", optarg);
            pkgarg = strdup(optarg);
            /* remove trailing '/' */
            int len = strlen(pkgarg);
            if (pkgarg[len-1] == '/') {
                pkgarg[len-1] = '\0';
            }
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        case 'x':
            exit_on_error = true;
        default:
            ;
        }
    }

    /* config in this order: first bazel, then mibl, then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    mibl_configure();
    /* later: config and run s7 stuff */

    /* for dev, we run the crawler directly */
    load_opam();

    printf("exiting...\n");
}
