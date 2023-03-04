#include <unistd.h>             /* getcwd */

/* FIXME: use mkhdrs to elim 'unused function' warnings */
/* #include "bazel_config.h" */
/* #include "mibl_config.h" */
/* #include "s7_config.h" */
/* #include "ansi_colors.h" */
/* #include "load_dune.h" */

#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "s7.h"
#include "libmibl.h"
#include "log.h"

#include "unity.h"

extern bool debug;

s7_scheme *s7;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    log_info("setup");
}

void tearDown(void) {
    log_info("teardown");
}

void test_function_should_doBlahAndBlah(void) {
    log_info("test stuff");
    char *rootdir;
    char *pathdir;

    rootdir = "obazl/mibl/test";
    pathdir = "test/cases/dune/stanzas/rule/deps/glob";

    s7_pointer pkg_tbl = load_dune(rootdir, pathdir);

    log_debug(BGRN "pkg_tbl:" CRESET "\n%s",
           s7_object_to_c_string(s7, pkg_tbl));

    /* (pkg (hash-table-ref pkgs arg)) */
    /* s7_pointer ht_ref = s7_name_to_value(s7, "hash-table-ref"); */
    /* if (ht_ref == s7_undefined(s7)) { */
    /*     printf("unbound symbol: hash-table-ref"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer pkg_key = s7_make_string(s7, "dune/stanzas/library/deps/select"); */
    /* s7_pointer pkg = s7_call(s7, ht_ref, s7_list(s7, 2, pkg_tbl, pkg_key)); */
    /* printf(BGRN "pkg:" CRESET " %s\n", TO_STR(pkg)); */
}

void test_function_should_doAlsoDoBlah(void) {
    log_info("more test stuff");
}

int main(void) {

    /* debug = true; */
    bazel_configure(); // getcwd(NULL, 0));
    mibl_configure();
    s7 = s7_configure();

    initialize_mibl_data_model(s7);

    UNITY_BEGIN();
    RUN_TEST(test_function_should_doBlahAndBlah);
    RUN_TEST(test_function_should_doAlsoDoBlah);
    UNITY_END();

    s7_shutdown(s7);
    return 0;
}
