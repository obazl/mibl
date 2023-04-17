/*
  test dsl:action->mibl, destructure-dsl-string, parse-pct-var
 */

#include <libgen.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "libmibl.h"

#include "unity.h"


#if defined(DEBUG_TRACE)
/* extern bool debug; */
extern bool mibl_debug_bazel;
extern int  mibl_debug_level;
extern bool mibl_debug_mibl;
extern bool mibl_debug_miblrc;
extern bool mibl_debug_scm;
extern bool mibl_debug_s7_config;
extern bool mibl_debug_traversal;
extern bool mibl_trace;
extern bool mibl_trace_bazel;
/* extern bool trace_mibl; */
#endif

extern UT_string *mibl_runfiles_root;
extern bool verbose;

s7_scheme *s7;

char *sexp_action;
char *sexp_expected;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

UT_string *setter;

UT_string *sexp;
s7_pointer actual;
s7_pointer expected;

    /* char *s = s7_object_to_c_string(s7, actual); */
    /* log_debug("result: %s", s); */
    /* free(s); */

/* **************************************************************** */
/* fn: dsl:action->mibl */
/* e.g. for (action (foo args ...))
   arg passed to dsl:action->mibl: ((foo args ...))
*/

/* bash cases: string/sym args; embedded pctvars; */

/* THE PROBLEM: we need to identify the tool(s) and may need to infer
   i/o files. If the arg is a string, that may be impossible, since it
   could contain an entire, possibly complex script. */
/* So we can only rely on heuristics: the first arg is likely to be the tool. */
void test_bash(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    sexp_action = "'((bash \"diff -a %{deps}\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . bash) (:cmd-lines (:line \"diff\" \"-a\" ::inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
/* s7_flush_output_port(s7, s7_current_output_port(s7)); */
/* char *s = s7_object_to_c_string(s7, actual); */
/* log_debug("result: %s", s); */
/* free(s); */
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* internal whitespace */
    sexp_action = "'((bash   \"diff   -a   %{deps}\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . bash) (:cmd-lines (:line \"diff\" \"-a\" ::inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* split cmd into multiple lines */
    sexp_action = "'((bash \"diff -a\" \n \"%{deps} foo\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . bash) (:cmd-lines (:line \"diff\" \"-a\") (:line ::inputs \"foo\"))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* sym args */
    sexp_action = "'((bash diff -a %{deps}))";
    sexp_expected = "'((:cmd (:tool . diff) (:shell . bash) (:args -a ::inputs)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_action = "'((bash test.sh))";
    sexp_expected = "'((:cmd (:tool . test.sh) (:shell . bash) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_action   = "'((bash \"%{first_dep} > ffi_generated.ml\"))";
    sexp_expected = ""
        "'((:cmd (:tool (% . first_dep)) "
        "        (:shell . bash) "
        "        (:cmd-lines (:line (:string (% . first_dep)) \">\" \"ffi_generated.ml\"))))"
        ;
    sexp_expected = ""
        "'((:cmd (:tool (% . first_dep)) "
        "        (:shell . bash) "
        "        (:cmd-lines (:line (:string (% . first_dep)) \">\" \"ffi_generated.ml\"))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* add a flag */
    sexp_action   = "'((bash \"%{first_dep} -ml > ffi_generated.ml\"))";
    sexp_expected = ""
        "'((:cmd (:tool (% . first_dep)) (:shell . bash) (:cmd-lines (:line (:string (% . first_dep)) \"-ml\" \">\" \"ffi_generated.ml\"))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));


    /* pctvar w/prefix */
    sexp_action = "'((bash \"./%{first_dep} > %{targets}\"))";
    sexp_expected = ""
        "'((:cmd (:tool (:string \"./\" (% . first_dep))) (:shell . bash) (:cmd-lines (:line (:string \"./\" (% . first_dep)) \">\" ::outputs))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));


    /* asterisk */
    sexp_action   = "'((bash \"%{bin:shellcheck} -x *.sh\"))";
    sexp_expected = ""
        "'((:cmd (:tool (% bin shellcheck)) "
        "  (:shell . bash) "
        "  (:cmd-lines (:line (:string (% bin shellcheck)) \"-x\" \"*.sh\"))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));


/* (action  (bash "cp `grep '^ARCH=' %{conf} | cut -d'=' -f2`/\*.ml ."))) */
    /*
(action (bash "grep -q '\"ppx/ppx_here/test/dummy.mll\"' dummy.ml.pp")))
(action ))

(action (bash "%{first_dep} patdiff")))
(action (bash "./%{first_dep} > config.h")))
(action (bash "%{NODE} %{deps} > %{targets}")))
(action (bash "./rpc_test.exe regression")))
(action (bash "%{bin:shellcheck} -x patdiff-git-wrapper")))

(alias (name runtest) (deps ./main.exe) (action (bash %{deps})))
    */
}

/* some dune rules embed a complex shell script, e.g. ppx_inline_test/test/dune */
/* WARN: test does not work, chokes on the backslashes */
void test_bash_script(void) {
    TEST_IGNORE();

/* (set! (hook-functions *read-error-hook*) */
/*       (list (lambda (h) */

    s7_pointer in_port = s7_open_input_file(s7, "test/dune/action_dsl/dsl_bash_script.dune", "r");
    if (!s7_is_input_port(s7, in_port)) {
        char *s1;
        {fprintf(stderr, "%d: %s is not an input port?\n", __LINE__, s1 = s7_object_to_c_string(s7,in_port)); free(s1); exit(1);}
    }

    s7_pointer test_sexp = s7_read(s7, in_port);
    (void)test_sexp;
    char *s = s7_object_to_c_string(s7, test_sexp);
    log_debug("result: %s", s);
    free(s);

    /* char *test_str = "(destructure-dsl-string \"" S "\")"; */
    /* utstring_renew(sexp); */
    /* /\* utstring_printf(sexp, "(destructure-dsl-string \"" s "\)"); *\/ */
    /* actual = s7_eval_c_string(s7, test_str); //utstring_body(sexp)); */
    /* char *s = s7_object_to_c_string(s7, actual); */
    /* log_debug("result: %s", s); */
    /* free(s); */

    /* expected = s7_eval_c_string(s7, "'()"); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

/* cat also used in test_stdout */
void test_cat(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* directive arg: symbol */
    sexp_action   = "'((cat %{a} %{b} > output.ml))";
    sexp_expected = ""
        "'((:cmd (:tool . :cat) (:shell . #<unspecified>) (:args (% . a) (% . b) > output.ml)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

/* directives cmp, copy, diff take two args and possibly some options.
   args may be expressed as a single pctvar that resolves to two
   args.  tests mostly use diff. */
void test_base_two_args(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    // two sym args
    sexp_action = "'((diff a.txt b.txt))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two sym args plus args
    sexp_action = "'((diff -foo --bar a.txt b.txt))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args -foo --bar a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two sym args plus args
    sexp_action = "'((diff -U 3 a.txt b.txt))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args -U 3 a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two sym args plus args
    sexp_action = "'((cmp -i1:2 a.bin b.bin))";
    sexp_expected = "'((:cmd (:tool . :cmp) (:shell . #<unspecified>) (:args -i1:2 a.bin b.bin)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two string args
    sexp_action = "'((diff \"a.txt\" \"b.txt\"))";
    // string args converted to syms
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, sym+literal strings
    sexp_action = "'((diff a.txt \"b.txt\"))";
    // string args converted to syms
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, literal+sym strings
    sexp_action = "'((diff a.txt b.txt))";
    // string args converted to syms
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, pctvars
    sexp_action = "'((diff %{a} %{b}))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args (% . a) (% . b))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, sym+pctvar
    sexp_action = "'((diff a.txt %{b}))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt (% . b))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, string+pctvar
    sexp_action = "'((diff \"a.txt\" %{b}))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt (% . b))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, pctvar+sym
    sexp_action = "'((diff %{a} b.txt))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args (% . a) b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // two args, pctvar+string
    sexp_action = "'((diff %{a} \"b.txt\"))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args (% . a) b.txt)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // single pctvar arg
    sexp_action = "'((diff %{deps}))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args ::inputs)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    // single pctvar arg plus options
    sexp_action = "'((diff -a %{deps}))";
    sexp_expected = "'((:cmd (:tool . :diff) (:shell . #<unspecified>) (:args -a ::inputs)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* /\* internal whitespace *\/ */
    /* sexp_action = "'((bash   \"diff   -a   %{deps}\"))"; */
    /* sexp_expected = "'((:tool . \"diff\") (:shell . bash) (:cmd-lines (:line \"diff\" \"-a\" ::inputs)))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* /\* split cmd into multiple lines *\/ */
    /* sexp_action = "'((bash \"diff -a\" \n \"%{deps} foo\"))"; */
    /* sexp_expected = "'((:tool . \"diff\") (:shell . bash) (:cmd-lines (:line \"diff\" \"-a\") (:line ::inputs \"foo\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* /\* sym args *\/ */
    /* sexp_action = "'((bash diff -a %{deps}))"; */
    /* sexp_expected = "'((:shell . bash) (:tool . diff) (:args -a ::inputs))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* sexp_action = "'((bash test.sh))"; */
    /* sexp_expected = "'((:shell . bash) (:tool . test.sh) (:args))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* sexp_action = "'((bash \"%{first_dep} > ffi_generated.ml\"))"; */
    /* sexp_expected = "'((:tool (% . first_dep)) (:shell . bash) (:cmd-lines (:line (% . first_dep) \">\" \"ffi_generated.ml\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* /\* add a flag *\/ */
    /* sexp_action = "'((bash \"%{first_dep} -ml > ffi_generated.ml\"))"; */
    /* sexp_expected = "'((:tool (% . first_dep)) (:shell . bash) (:cmd-lines (:line (% . first_dep) \"-ml\" \">\" \"ffi_generated.ml\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */


    /* /\* pctvar w/prefix *\/ */
    /* sexp_action = "'((bash \"./%{first_dep} > %{targets}\"))"; */
    /* sexp_expected = "'((:tool (:string \"./\" (% . first_dep))) (:shell . bash) (:cmd-lines (:line (:string \"./\" (% . first_dep)) \">\" ::outputs)))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */


    /* /\* asterisk *\/ */
    /* sexp_action = "'((bash \"%{bin:shellcheck} -x *.sh\"))"; */
    /* sexp_expected = "'((:tool (% bin shellcheck)) (:shell . bash) (:cmd-lines (:line (% bin shellcheck) \"-x\" \"*.sh\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

void test_system(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    sexp_action = "'((system \"foo.sh\"))";
    sexp_expected = "'((:cmd (:tool . \"foo.sh\") (:shell . sh) (:cmd-lines (:line \"foo.sh\"))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_action = "'((system \"diff -a %{deps}\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . sh) (:cmd-lines (:line \"diff\" \"-a\" ::inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* internal whitespace */
    sexp_action = "'((system   \"diff   -a   %{deps}\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . sh) (:cmd-lines (:line \"diff\" \"-a\" ::inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* split cmd into multiple lines */
    sexp_action = "'((system \"diff -a\" \n \"%{deps} foo\"))";
    sexp_expected = "'((:cmd (:tool . \"diff\") (:shell . sh) (:cmd-lines (:line \"diff\" \"-a\") (:line ::inputs \"foo\"))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* sym args */
    sexp_action = "'((system diff -a %{deps}))";
    sexp_expected = "'((:cmd (:tool . diff) (:shell . sh) (:args -a ::inputs)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* sexp_action = "'((bash test.sh))"; */
    /* sexp_expected = "'((:shell . bash) (:tool . test.sh) (:args))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* sexp_action = "'((bash \"%{first_dep} > ffi_generated.ml\"))"; */
    /* sexp_expected = "'((:tool (% . first_dep)) (:shell . bash) (:cmd-lines (:line (% . first_dep) \">\" \"ffi_generated.ml\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */

    /* /\* add a flag *\/ */
    /* sexp_action = "'((bash \"%{first_dep} -ml > ffi_generated.ml\"))"; */
    /* sexp_expected = "'((:tool (% . first_dep)) (:shell . bash) (:cmd-lines (:line (% . first_dep) \"-ml\" \">\" \"ffi_generated.ml\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */


    /* /\* pctvar w/prefix *\/ */
    /* sexp_action = "'((bash \"./%{first_dep} > %{targets}\"))"; */
    /* sexp_expected = "'((:tool (:string \"./\" (% . first_dep))) (:shell . bash) (:cmd-lines (:line (:string \"./\" (% . first_dep)) \">\" (% . targets))))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */


    /* /\* asterisk *\/ */
    /* sexp_action = "'((bash \"%{bin:shellcheck} -x *.sh\"))"; */
    /* sexp_expected = "'((:tool (% bin shellcheck)) (:shell . bash) (:cmd-lines (:line (% bin shellcheck) \"-x\" \"*.sh\")))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

/* **************************************************************** */
void test_progn(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* (action (progn)) */
    sexp_action = "'((progn))";
    sexp_expected = "'(:cmds ())";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /*  */
    sexp_action   = "'((progn (echo \"foo\") (diff a.txt b.txt) (diff ../c.txt d.txt) (echo \"bar\")))";
    sexp_expected = ""
        "'(:cmds ((:cmd (:tool . :echo) (:shell . #<unspecified>) (:args \"foo\")) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args a.txt b.txt)) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args ../c.txt d.txt)) "
        "         (:cmd (:tool . :echo) (:shell . #<unspecified>) (:args \"bar\"))))"
        ;

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* redundant embedded progn */
    sexp_action = "'((progn \
    (echo \"Bootstrap check: comparing the stage 2 and stage 3 parsers...\n\") \
    (progn \
      (diff parser.stage2.ml parser.stage3.ml) \
      (diff ../stage2/parser.mli parser.mli) \
    ) \
    (echo \"Bootstrap check: done.\n\") \
  ))";
    sexp_expected = ""
        "'(:cmds ((:cmd (:shell . #<unspecified>) (:tool . :echo) (:args \"Bootstrap check: comparing the stage 2 and stage 3 parsers...\n\")) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args parser.stage2.ml parser.stage3.ml)) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args ../stage2/parser.mli parser.mli)) "
        "         (:cmd (:shell . #<unspecified>) (:tool . :echo) (:args \"Bootstrap check: done.\n\"))))"
        ;
    sexp_expected = ""
        "'(:cmds ((:cmd (:tool . :echo) (:shell . #<unspecified>) (:args \"Bootstrap check: comparing the stage 2 and stage 3 parsers...\n\")) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args parser.stage2.ml parser.stage3.ml)) "
        "         (:cmd (:tool . :diff) (:shell . #<unspecified>) (:args ../stage2/parser.mli parser.mli)) "
        "         (:cmd (:tool . :echo) (:shell . #<unspecified>) (:args \"Bootstrap check: done.\n\"))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    sexp_action   = ""
        "'((with-stdout-to %{targets} "
        "  (progn (cmp ../config/gen_services.exe %{deps}) "
        "         (cat uri_services_raw.ml))))"
        ;
    sexp_expected = ""
        "'((:stdout ::outputs) "
        "  (:cmds ((:cmd (:tool . :cmp) (:shell . #<unspecified>) (:args ../config/gen_services.exe ::inputs)) "
        "          (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args uri_services_raw.ml)))))"
        ;

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* same as above using (run ) */
    sexp_action   = ""
        "'((with-stdout-to %{targets} "
        "  (progn (run ../config/gen_services.exe %{deps}) "
        "         (cat uri_services_raw.ml))))"
        ;
    sexp_expected = ""
        "'((:stdout ::outputs) "
        "  (:cmds ((:cmd (:tool ../config/gen_services.exe) (:args ::inputs)) "
        "          (:cmd (:shell . sh) (:tool . :cat) (:args uri_services_raw.ml)))))"
        ;
    sexp_expected = ""
        "'((:stdout ::outputs) "
        "  (:cmds ((:cmd (:tool ../config/gen_services.exe) (:args ::inputs)) "
        "          (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args uri_services_raw.ml)))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /*

bisect_ppx:
(rule
 (targets assets.ml)
 (deps
  (:css coverage.css)
  (:js coverage.js)
  (:hljs ../vendor/highlight.js/highlight.pack.js))
 (action
  (with-stdout-to %{targets}
   (progn
    (echo "let css = {css|")
    (cat %{css})
    (echo "|css}")
    (echo "let js = {js|")
    (cat %{js})
    (echo "|js}")
    (echo "let highlight_js = {js|")
    (cat %{hljs})
    (echo "|js}")))))

(action
   (progn
     (with-stdout-to contains-input-name
       (bash "echo `grep '^ARCH=' %{conf} | cut -d'=' -f2`/emit.mlp"))
     (with-stdout-to %{targets}
       (progn
         (bash "echo \\# 1 \\\"`cat contains-input-name`\\\"")
         (bash "%{dep:../tools/cvt_emit.exe} < `cat contains-input-name`")))))

      cppo:
 (preprocess (per_module
  ((action (progn
    (run ocaml %{dep:compat.ml} %{input-file})
    (cat %{input-file}))) cppo_eval)))

menhir:

(rule
  (with-stdout-to standard_mly.ml
    (progn
      (echo "let contents = {|")
      (cat standard.mly)
      (echo "|}")
    )
  )
)

(action
  (chdir
   %{project_root}
   (progn
    (run expect-test %{test})
    (diff? %{test} %{test}.corrected))))
     */

}

void test_stdin(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* directive arg: symbol */
    sexp_action   = "'((with-stderr-to stderr.txt (run %{exe:foo})))";
    sexp_expected = "'((:stderr stderr.txt) (:cmd (:tool (% exe foo)) (:args)))";
    // NB: no (:shell) entry for 'run'
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: string */
    sexp_action =     "'((with-stderr-to \"stderr.txt\" (run %{exe:foo})))";
    sexp_expected = "'((:stderr \"stderr.txt\") (:cmd (:tool (% exe foo)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar sym */
    sexp_action =     "'((with-stderr-to %{stderr} (run %{exe:foo})))";
    sexp_expected = "'((:stderr (% . stderr)) (:cmd (:tool (% exe foo)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar string - ignored, redirect is always to a file */
    sexp_action =     "'((with-stderr-to \"%{stderr}\" (run %{exe:foo})))";
    sexp_expected = "'((:stderr (% . stderr)) (:cmd (:tool (% exe foo)) (:args)))";
                      /* ((:stderr (% . stderr)) (:cmd (:tool . run) (:args (% exe foo))))"; */
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar embedded in string: pfx */
    sexp_action =     "'((with-stderr-to \"foo/%{stderr}\" (run %{exe:foo})))";
    sexp_expected = "'((:stderr (:string \"foo/\" (% . stderr))) (:cmd (:tool (% exe foo)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar embedded in string: sfx */
    sexp_action =     "'((with-stderr-to \"%{outdir}/stderr.txt\" (run %{exe:foo})))";
    sexp_expected = "'((:stderr (:string (% . outdir) \"/stderr.txt\")) (:cmd (:tool (% exe foo)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar embedded in string: pfx, sfx */
    sexp_action =     "'((with-stderr-to \"foo/%{outdir}/stderr.txt\" (run %{exe:foo})))";
    sexp_expected = "'((:stderr (:string \"foo/\" (% . outdir) \"/stderr.txt\")) (:cmd (:tool  (% exe foo)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

/* NB: test use cat so no (:shell...)  */
void test_stdout(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* directive arg: symbol */
    sexp_action   = "'((with-stdout-to output.txt (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout output.txt) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: string */
    sexp_action   = "'((with-stdout-to \"output.txt\" (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout \"output.txt\") "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar sym */
    sexp_action   = "'((with-stdout-to %{outfile} (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout (% . outfile)) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar string - ignored, redirect is always to a file */
    sexp_action   = "'((with-stdout-to \"%{outfile}\" (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout (% . outfile)) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar w/string pfx */
    sexp_action   = "'((with-stdout-to \"foo/%{output}\" (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout (:string \"foo/\" (% . output))) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar w/string sf */
    sexp_action   = "'((with-stdout-to \"%{outdir}/out.txt\" (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout (:string (% . outdir) \"/out.txt\")) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* directive arg: pctvar embedded in string: pfx, sfx */
    sexp_action   = "'((with-stdout-to \"foo/%{outdir}/out.txt\" (cat %{deps})))";
    sexp_expected = ""
        "'((:stdout (:string \"foo/\" (% . outdir) \"/out.txt\")) "
        "  (:cmd (:tool . :cat) (:shell . #<unspecified>) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

/* mixed stdin/stderr/stdout */
void test_stdx(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* menhir/demos/calc-stratified */
    /* NB: no target, deps or action flds */
    sexp_action = ""
        /* "(rule " */
        "'((with-stdout-to calc.out "
        " (with-stdin-from calc.in "
        "   (run ./calc.exe))))"
        /* ")" */
        ;
    sexp_expected = ""
        "'((:stdout calc.out) "
        "  (:stdin calc.in) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* reverse order */
    sexp_action = ""
        /* "(rule " */
        "'((with-stdin-from calc.in "
        "  (with-stdout-to calc.out "
        "   (run ./calc.exe))))"
        /* ")" */
        ;
    sexp_expected = ""
        "'((:stdin calc.in) "
        "  (:stdout calc.out) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* std pctvar args */
    sexp_action = ""
        "'((with-stdout-to %{target} "
        "  (with-stdin-from %{deps} "
        "   (run ./calc.exe))))"
        ;
    sexp_expected = ""
        "'((:stdout ::outputs) "
        "  (:stdin ::inputs) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* std pctvar args, reversed */
    sexp_action = ""
        "'((with-stdin-from %{deps} "
        "  (with-stdout-to %{target} "
        "   (run ./calc.exe))))"
        ;
    sexp_expected = ""
        "'((:stdin ::inputs) "
        "  (:stdout ::outputs) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* **************** */
    /* user pctvar args */
    sexp_action = ""
        "'((with-stdout-to %{outfile} "
        "  (with-stdin-from %{infile} "
        "   (run ./calc.exe))))"
        ;
    sexp_expected = ""
        "'((:stdout (% . outfile)) "
        "  (:stdin  (% . infile)) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* user pctvar args, reversed */
    sexp_action = ""
        "'((with-stdin-from %{deps} "
        "  (with-stdout-to %{target} "
        "   (run ./calc.exe))))"
        ;
    sexp_expected = ""
        "'((:stdin ::inputs) "
        "  (:stdout ::outputs) "
        "  (:cmd (:tool ./calc.exe) (:args)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));


    /* **************** */
    /* sexp_action = "" */
    /*     "'(with-stdin-from %{deps}" */
    /*     "   (with-stdout-to %{target} " */
    /*     "     (chdir %{workspace_root} " */
    /*     "       (run ./tools/rewrite.exe -init))))" */
    /*     ; */
    /* sexp_expected = "'((:stdout \"output.txt\") (:cmd (:shell . sh) (:tool . :cat) (:args ::inputs)))"; */
    /* utstring_renew(sexp); */
    /* utstring_printf(sexp, "(%s %s)", fn, sexp_action); */
    /* actual = s7_eval_c_string(s7, utstring_body(sexp)); */
    /* expected = s7_eval_c_string(s7, sexp_expected); */
    /* TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected)); */
}

/* more recursion: (with_stdout_to foo (run (bash bar.sh ...))) etc. */
void test_run_with_stdout_to(void) {
    /* TEST_IGNORE(); */

    char *fn = "dsl:action->mibl";
    /*
    (action (with-stdout-to %{target} (run ../src/test.exe --kind merge .)))
    (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))
    (action  (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))

    dream: (with-stdout-to %{null} (run ocaml-crunch -m plain assets -o %{target}))
    menhir: (with-stdout-to menhir_flags.sexp (progn (echo "(") (cat %{dep:menhir_flags}) (echo ")")))
            (action (with-stdout-to %{target} (run ../src/test.exe --kind good .)))
 (action (run menhir --cmly %{deps}))
    */

    sexp_action = "'((with-stdout-to %{targets} (run %{dep:gen_primitives.sh})))";
    sexp_expected = "'((:stdout ::outputs) (:cmd (:tool (% dep gen_primitives.sh)) (:args)))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* run bash => insert (:shell .bash) */
    sexp_action = "'((with-stdout-to %{targets} (run bash myscript.sh %{inputs})))";
    sexp_expected = "'((:stdout ::outputs) (:cmd (:tool . myscript.sh) (:shell . bash) (:args (% . inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_run(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:action->mibl";

    /* %{deps} may expand to unquoted list, (:args a b).*/
    sexp_action = ""
        "'((run foo %{deps}))"
        ;
    sexp_expected = ""
        "'((:cmd (:tool foo) (:args ::inputs)))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* "%{deps}" - quotes must be retained, (:args (:string ...)) */
    /*  so cmd will be 'foo "a b"' not 'foo a b' */
    sexp_action = ""
        "'((run foo \"%{deps}\"))"
        ;
    sexp_expected = ""
        "'((:cmd (:tool foo) (:args (:line ::inputs))))"
        ;
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* **************** */
    sexp_action = ""
        "'((with-stdout-to %{targets} "
        "    (run bash myscript.sh %{inputs})))"
        ;
    sexp_expected = "'((:stdout ::outputs) (:cmd (:tool . myscript.sh) (:shell . bash) (:args (% . inputs))))";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, sexp_action);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, sexp_expected);
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /*

 (action (run %{deps}))
 (action (run %{exe:run.exe} ../../.. ))
  (action (run make %{targets} COMPUTE_DEPS=false)))
 (action (chdir %{workspace_root} (run config/pp.exe))))
(action (run sed s/%dls_get/%identity/ %{input-file}))
 (action  (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))
 (action (run %{test} -runner processes)))
 (action (run %{exe:main.exe})))
 (action (run qtest extract src/foo1.ml src/foo2.ml > %{targets})))
    (action (run ./pack.exe))
    (action (run piqic-ocaml --multi-format %{deps}))
    (action (with-stdout-to %{target} (run ../src/test.exe --kind good .)))
 (action (run bash test.sh))
 (action (run ./test.exe))
 (action (with-stdout-to %{target} (run ./test.exe)))
 (action (run atdgen -j %{deps}))
 (action (run esy atdgen -open Core -deriving-conv sexp_of -t %{deps}))
(run ./config/llvm_configurator.exe -filename %{deps})
(action (run tar -cvf %%{targets} %%{deps}))
 (action (run %%{make} %s%a %s))
    (action (with-stdout-to %{target} (run ../src/test.exe --kind merge .)))
    (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))
    (action  (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))

    dream: (with-stdout-to %{null} (run ocaml-crunch -m plain assets -o %{target}))
    menhir: (with-stdout-to menhir_flags.sexp (progn (echo "(") (cat %{dep:menhir_flags}) (echo ")")))
            (action (with-stdout-to %{target} (run ../src/test.exe --kind good .)))
 (action (run menhir --cmly %{deps}))

 ocaml_protoc:
(action (run ocaml-protoc -binary -pp -ml_out ./ %{deps}))
(action (run ocaml-protoc -pp -ml_out ./ %{deps}))
  (action (run ocaml-protoc
    -binary -pp
    -ml_out
    ./
    ./ test07.proto))

  (action (run ocaml-protoc
    -I ./
    -I ../../include/ocaml-protoc
    -I /usr/local/include
    -binary -pp
    -ocaml_all_types_ppx "deriving show"
    -ml_out
    ./
    ./ test01.proto))

    lwt/src/unix:
(action
  (chdir %{project_root}
   (run %{bin:cppo} -V OCAML:%{ocaml_version} %{ml} -o %{targets})))

(rule
 (targets
  unix_c_flags.sexp unix_c_library_flags.sexp lwt_features.h lwt_features.ml)
 (deps (:exe config/discover.exe) discover_arguments)
 (action (run %{exe})))

ocaml/runtime:
(rule
 (targets primitives)
 (mode    fallback)
 (deps
   ; matches the line structure of files in gen_primitives.sh
   alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c intern.c
     interp.c ints.c io.c
   lexing.c md5.c meta.c memprof.c obj.c parsing.c signals.c str.c sys.c
     callback.c weak.c
   finalise.c dynlink.c backtrace_byt.c backtrace.c
     afl.c
   bigarray.c runtime_events.c)
 (action  (with-stdout-to %{targets} (run %{dep:gen_primitives.sh}))))
;;; NB: gen_primitives.sh not listed in deps. Special var %{dep:...} expands relative to cwd.
;; e.g. in src/foo/dune, %{dep:bar} will expand to src/foo/bar.

ounit:
(test
  (name testRunnerProcesses)
  (package ounit2-lwt)
  (deps test.txt)
  (libraries ounit2 lwt lwt.unix ounit2-lwt)
  (action (run %{test} -runner processes)))

 (action (run %{ocaml_where}/expunge %{dep:topstart.exe} %{targets}
                    stdlib__Arg
                    stdlib__Array
                    ...
                    ; the rest
                    outcometree topdirs topeval toploop topmain topcommon
 ))

    */

}

/* **************************************************************** */
/* fn: parse-pct-var */
void test_pctvars(void) {
    /* TEST_IGNORE(); */
    char *fn = "parse-pct-var";

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn,  "\"%{foo}\"");
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, "'(:string (% . foo))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    utstring_renew(sexp);
    utstring_printf(sexp, "%s", "(parse-pct-var '%{foo})");
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, "'(% . foo)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    utstring_renew(sexp);
    utstring_printf(sexp, "%s", "(parse-pct-var \"%{foo:bar}\")");
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, "'(:string (% foo bar))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    utstring_renew(sexp);
    utstring_printf(sexp, "%s", "(parse-pct-var '%{foo:bar})");
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, "'(% foo bar)");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* ;; %{libexec:myscript.sh} */
}

/* **************************************************************** */
/* fn: destructure-dsl-string */
void test_embedded_pctvars(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:string->lines";

    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, "\"foo/%{a} bar/%{b}/baz\"");
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7, "'(:line (:string \"foo/\" (% . a)) (:string \"bar/\" (% . b) \"/baz\"))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void test_dsl_string(void) {
    /* TEST_IGNORE(); */
    char *fn = "dsl:string->lines";

    /* one line */
    char *cmd = "\"diff -u --label test.output --label test-partitions.output test.output test-partitions.output > diff-with-without-partitions || true\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, cmd);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7,
          "'(:line \"diff\" \"-u\" \"--label\" \"test.output\" \"--label\" \"test-partitions.output\" \"test.output\" \"test-partitions.output\" \">\" \"diff-with-without-partitions\" \"||\" \"true\")");

    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* broken into two lines */
    cmd = "\"diff -u --label test.output \n \
 --label test-partitions.output test.output test-partitions.output > diff-with-without-partitions || true\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, cmd);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7,
                                "'((:line \"diff\" \"-u\" \"--label\" \"test.output\") (:line \"--label\" \"test-partitions.output\" \"test.output\" \"test-partitions.output\" \">\" \"diff-with-without-partitions\" \"||\" \"true\"))");

    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));

    /* many lines */
    cmd = "\"diff -u\n --label test.output\n --label test-partitions.output\n test.output test-partitions.output\n >\n diff-with-without-partitions\n || true\"";
    utstring_renew(sexp);
    utstring_printf(sexp, "(%s %s)", fn, cmd);
    actual = s7_eval_c_string(s7, utstring_body(sexp));
    expected = s7_eval_c_string(s7,
                                "'((:line \"diff\" \"-u\")"
                                "(:line \"--label\" \"test.output\")"
                                "(:line \"--label\" \"test-partitions.output\")"
                                "(:line \"test.output\" \"test-partitions.output\")"
                                "(:line \">\")"
                                "(:line \"diff-with-without-partitions\")"
                                "(:line \"||\" \"true\"))");
    TEST_ASSERT_TRUE(s7_is_equal(s7, actual, expected));
}

void _print_usage(void) {
    printf("Usage:\t$ bazel test <tgt> [flags, options]\n");
    printf("Flags\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t--debug-config\t\tEnable all config debugging flags.\n");
    printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");

    printf("Options:\n");
    /* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); */

    printf("\t-r, --root <arg>"
           "\tStart traversal at <arg> (path relative to cwd).\n");
    printf("\t-p, --pkg <arg>"
           "\t\tProcess only <arg> (relative root path).\n");
}

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,

    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_MIBLRC,
    FLAG_DEBUG_MIBL_CRAWL,
    FLAG_DEBUG_SCM,
    FLAG_DEBUG_SCM_LOADS,

    FLAG_EMIT_PARSETREE,        /* config load_project to emit PARSETREE.mibl */

    FLAG_SHOW_CONFIG,
    FLAG_TRACE,
    FLAG_VERBOSE,

    LAST
};

static struct option options[] = {
    /* 0 */
    [OPT_ROOT] = {.long_name="root",.short_name='r',
                  .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_PKG] = {.long_name="pkg",.short_name='p',
                 .flags=GOPT_ARGUMENT_REQUIRED
    },
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBLRC] = {.long_name="debug-miblrc",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBL_CRAWL] = {.long_name="debug-mibl-crawl",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

extern bool emit_parsetree;

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    /* if (options[FLAG_SHOW_CONFIG].count) */
    /*     mibl_s7_set_flag("*mibl-show-config*", true); */

    if (options[FLAG_VERBOSE].count) {
        log_info("verbose ct: %d", options[FLAG_VERBOSE].count);
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("debug ct: %d", options[FLAG_DEBUG].count);
        /* debug = true; */
        mibl_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_DEBUG_CONFIG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
           log_info("debug_config ct: %d", options[FLAG_DEBUG_CONFIG].count);
        mibl_debug_bazel = true;
        /* debug_mibl = true; */
        mibl_debug_s7_config = true;
#else
        log_error("--debug-config only valid with -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_MIBLRC].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("debug_miblrc ct: %d", options[FLAG_DEBUG_MIBLRC].count);
        mibl_debug_miblrc = true;
#endif
    }

    if (options[FLAG_DEBUG_MIBL_CRAWL].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("mibl_debug_traversal ct: %d", options[FLAG_DEBUG_MIBL_CRAWL].count);
        mibl_debug_traversal = true;
#else
        log_error(RED "ERROR: " CRESET
                  "--debug-mibl-crawl requires -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_SCM].count) {
#if defined(DEBUG_TRACE)
        mibl_debug_scm = true;
#else
        log_warn("--debug-scm only takes effect for debug builds (-c dbg)");
#endif
    }
    if (options[FLAG_DEBUG_SCM_LOADS].count) {
        mibl_s7_set_flag("*mibl-debug-s7-loads*", true);
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("trace ct: %d", options[FLAG_TRACE].count);
        mibl_trace = true;
        mibl_trace_bazel = true;
        /* trace_mibl = true; */
#endif
    }

    if (options[FLAG_EMIT_PARSETREE].count) {
        if (getenv("BAZEL_TEST")) {
            /* log_warn("BAZEL_TEST: %s", getenv("BAZEL_TEST")); */
            if ( !getenv("BUILD_WORKSPACE_DIRECTORY") ) {
                fprintf(stderr,
                        RED "ERROR: " CRESET
                        "--emit-parsetree not supported under bazel test. Try 'bazel run'.\n");
                exit(EXIT_FAILURE);
            }
        }
        emit_parsetree = true;
    }
}

int main(int argc, char **argv)
{
    /* log_debug("ARGV[0]: %s", argv[0]); */
    /* log_debug("CWD: %s", getcwd(NULL, 0)); */
    /* log_debug("BWSD: %s", getenv("BUILD_WORKSPACE_DIRECTORY")); */
    /* log_debug("BWD: %s", getenv("BUILD_WORKING_DIRECTORY")); */
    /* log_debug("TESTTGT: %s", getenv("TEST_TARGET")); */
    /* log_debug("RUNFILES_DIR: %s", getenv("RUNFILES_DIR")); */

    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    _set_options(options);
    /* **************************************************************** */
    /* In test env:
       BAZEL_TEST 1
       TEST_TARGET - label of this target, relative to BUILD_WS_DIRECTORY
       BUILD_WORKSPACE_DIRECTORY - NULL
       BUILD_WORKING_DIRECTORY - NULL
       RUNFILES_DIR - set

       task: config s7 with runfiles, then chdir to
               BUILD_WORKSPACE_DIRECTORY/TEST_TARGET pkg

       alternative: put the project tree in runfiles. the drawback is
       that the paths would be relative to the launch dir, not the dir
       containing the test target. but we want that dir to be like a ws.

       alternative: argv[0] is the test executable in the test case
       dir. take its dirname. or use TEST_SRCDIR which is dirname(argv[0])

       But according the the Test Encyclopedia, we should not change
       directory, but use the runfiles (TEST_SRCDIR, TEST_WORKSPACE,
       etc.)

       https://bazel.build/reference/test-encyclopedia#initial-conditions:

       "The test runner must invoke each test with the path to the
       test executable in argv[0]. This path must be relative and
       beneath the test's current directory (which is in the runfiles
       tree, see below). The test runner should not pass any other
       arguments to a test unless the user explicitly requests it."

       "The initial working directory shall be
       $TEST_SRCDIR/$TEST_WORKSPACE."

       "File descriptors 1 (stdout) and 2 (stderr) shall be open for
       writing, but what they are attached to is unspecified. It could
       be a terminal, a pipe, a regular file, or anything else to
       which characters can be written."

       "Tests must not assume that any constant path is available for
       their exclusive use."

       "Tests should create files only within the directories
       specified by $TEST_TMPDIR and $TEST_UNDECLARED_OUTPUTS_DIR (if
       set). These directories will be initially empty. Tests must not
       attempt to remove, chmod, or otherwise alter these directories."

       "Tests must access inputs through the runfiles mechanism, or
       other parts of the execution environment which are specifically
       intended to make input files available."

       "Tests must not access other outputs of the build system at
       paths inferred from the location of their own executable."

       "Tests should avoid using paths containing .. components within
       the runfiles tree."

       "No directory, file, or symlink within the runfiles tree
       (including paths which traverse symlinks) should be writable.
       (It follows that the initial working directory should not be
       writable.) Tests must not assume that any part of the runfiles
       is writable, or owned by the current user (for example, chmod
       and chgrp may fail)."

       "The runfiles tree (including paths which traverse symlinks)
       must not change during test execution. Parent directories and
       filesystem mounts must not change in any way which affects the
       result of resolving a path within the runfiles tree."

     */

    utstring_new(mibl_runfiles_root);
    char *rfr = getenv("RUNFILES_DIR");
    /* log_debug("RFR: %s", rfr); */
    if (rfr)
        utstring_printf(mibl_runfiles_root, "%s", rfr);
    else
        utstring_printf(mibl_runfiles_root, "%s", getcwd(NULL, 0));

    mibl_s7_init();

    mibl_s7_init2(NULL, // options[OPT_MAIN].argument,
                  NULL); //options[OPT_WS].argument);

    // WARNING: when run under bazel test, user miblrc (XDG_DATA_HOME/.local/share) not accessible

     if (options[FLAG_SHOW_CONFIG].count) {
         log_debug("SHOW CONFIG");
        show_bazel_config();
        show_mibl_config();
        show_s7_config();
        exit(EXIT_SUCCESS);
     }

    /* struct mibl_config_s *mibl_config = mibl_s7_init(NULL, /\* script dir *\/ */
    /*                                                  NULL); /\* ws *\/ */
    /* (void)mibl_config; */
    /* we do not call mibl_s7_run - that's for running a -main script.
       here we run our script fragments in the test cases. iow we
       drive the processing from this c program instead of from a
       script with -main.
     */

    /* log_info("cwd: %s", getcwd(NULL, 0)); */
    /* show_bazel_config(); */
    /* show_mibl_config(); */
    /* show_s7_config(); */
    /* log_info("arg0: %s", argv[0]); */

    char *test_pgm = strdup(argv[0]); // free
    errno = 0;
    char *test_root = dirname(test_pgm);
    if (test_root == NULL) {
        perror(test_pgm);
    }

    utstring_new(sexp);

    UNITY_BEGIN();

    RUN_TEST(test_pctvars);
    RUN_TEST(test_embedded_pctvars);
    RUN_TEST(test_dsl_string);

    // dsl base directives
    RUN_TEST(test_bash);
    RUN_TEST(test_base_two_args);
    RUN_TEST(test_cat);
    RUN_TEST(test_system);

    // dsl inductive directives
    RUN_TEST(test_progn);
    //RUN_TEST(test_stdin);
    //RUN_TEST(test_stderr);
    RUN_TEST(test_stdout);
    RUN_TEST(test_stdx);
    RUN_TEST(test_run);
    RUN_TEST(test_run_with_stdout_to);

    utstring_free(sexp);
    return UNITY_END();
}

/*
(action (progn
   (run ocaml %{dep:compat.ml} %{input-file})
   (cat %{input-file}))) cppo_eval))

  pipe-stdout:

  jsoo:
   (action
  (with-stdout-to
   %{target}
   (pipe-stdout
    (run printf "echo \226\152\160")
    (run %{dep:./cat.exe}))))

(action
  (with-stdout-to
   %{target}
   (progn
    (run %{dep:md5.exe} %{dep:md5.bc.js})
    (run %{dep:md5.exe} %{dep:md5.bc.js} %{dep:md5.bc.js})
    (run %{dep:md5.exe} -offset 2000 -length 4000 %{dep:md5.bc.js})
    (pipe-stdout
     (echo "tests")
     (run %{dep:md5.exe}))
    (pipe-stdout
     (echo "teststeststests")
     (run %{dep:md5.exe} -offset 5 -length 5))
    (pipe-stdout
     (echo "teststeststests")
     (run %{dep:md5.exe} -offset 2 -length 5)))))

 */
