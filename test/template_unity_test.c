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

void setUp(void) {
    log_info("setup");
}

void tearDown(void) {
    log_info("teardown");
}

void test_function_should_doBlahAndBlah(void) {
    log_info("test stuff");
}

void test_function_should_doAlsoDoBlah(void) {
    log_info("more test stuff");
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_function_should_doBlahAndBlah);
    RUN_TEST(test_function_should_doAlsoDoBlah);
    return UNITY_END();
}
