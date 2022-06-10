#include <unistd.h>

#include "bazel_config.h"
#include "mibl_config.h"
#include "s7_config.h"
#include "ansi_colors.h"
#include "dune_load.h"

extern bool debug;
extern bool trace;
extern bool verbose;

int main(int argc, char *argv[])
{
    char *opts = "hdtv";
    int opt;
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
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        default:
            ;
        }
    }
    /* config in this order: bazel then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    s7_configure();

    //TODO: cli args for root and path
    char *rootdir;
    char *pathdir;

    rootdir = "obazl/mibl";

    pathdir = "test/a";

    /* launching from test/ws */
    /* pathdir = "a/b"; // ws/a/WORKSPACE.bazel */

    /* pathdir = "test/baddot"; */
    /* pathdir = "test/includes/mwe"; */
    /* pathdir = "test/filetypes"; */
    /* pathdir = "test/mwe"; */

    /* pathdir = "test/tezos/lib_clic"; */
    /* pathdir = "test/tezos/lib_requester"; */
    /* pathdir = "test/tezos/lib_stdlib_unix"; */
    /* pathdir = "test/tezos/lib_test"; */
    /* pathdir = "test/tezos/lib_workers"; */
    /* pathdir = "test/tezos/openapi"; */
    /* pathdir = "test/tezos/proto_000_Ps9mPmXa"; */

    /* pathdir = "test/mina"; */
    /* pathdir = "test/mina/ocaml-sodium"; */

    /* **************** */
    /* rootdir = "tweag/tezos"; */
    /* pathdir = "src/bin_node"; */
    /* pathdir = "src/lib_clic"; */
    /* pathdir = "src/lib_stdlib_unix"; */
    /* pathdir = "src/proto_alpha"; */
    /* pathdir = "src/lib_protocol_environment"; */

    /* **************** */
    /* rootdir = "minadev/gitfork"; */
    /* pathdir = "src"; */
    /* pathdir = "src/lib/snarky/src"; */

    s7_pointer pkg_tbl = dune_load(rootdir, pathdir);

    printf(BGRN "pkg_tbl:" CRESET "\n%s\n", s7_object_to_c_string(s7, pkg_tbl));

    /* printf("*load-path*: %s\n", */
    /*        s7_object_to_c_string(s7, */
    /*                              s7_load_path(s7) */
    /*                              )); */

    if (verbose) {
        printf("dir count: %d\n", dir_ct);
        printf("file count: %d\n", file_ct);
        printf("dunefile count: %d\n", dunefile_ct);
    }
}
