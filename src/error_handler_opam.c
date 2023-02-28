#include <assert.h>
/* #include <regex.h> */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "s7.h"
/* #include "log.h" */

#include "error_handler_opam.h"

/* #include "utarray.h" */
/* #include "utstring.h" */

s7_pointer old_err_port;
const char *errmsg = NULL;
s7_int gc_loc = -1;

#define ERRSEXP "(with-let (owlet) " \
    "(format #t \"file: ~A, line ~A\n\" error-file error-line))"

s7_pointer _s7_error_handler_opam(s7_scheme *s7, s7_pointer args)
{
   /* log_error("_s7_error_handler\n"); */
    /* log_info("err: %s", TO_STR(args)); */

    if (strstr(s7_string(s7_car(args)), "unexpected close paren:") != NULL) {
#if defined(DEBUG_TRACE)
        if (debug)
            printf(RED "Error: BAD DOT" CRESET "\n");
#endif

        s7_write(s7, s7_make_string(s7, "BADDOT"),
                 s7_current_error_port(s7));

        /* fprintf(stdout, RED "[begin error context]\n"); */
        /* s7_eval_c_string(s7, ERRSEXP); */
        /* char *sexp = "(do ((e (outlet (owlet)) (outlet e))) " */
        /*     "((eq? e (rootlet))) " */
        /*     "(format () \"~{~A ~}~%\" e)) "; */
        /* s7_eval_c_string(s7, sexp); */
        /* s7_write(s7, */
        /*          s7_make_string(s7, s7_car(args)), */
        /*          // s7_string(s7_car(args)), */
        /*          s7_current_error_port(s7)); */
        /* fprintf(stdout, "[end error context]" CRESET "\n"); */
        /* exit(EXIT_FAILURE); */
        return s7_t(s7);
    } else {
        //TODO: write to error port
        fprintf(stdout, RED "Error:" CRESET " %s\n",
                s7_string(s7_car(args)));
        fprintf(stdout, RED "[begin error context]\n");
        s7_eval_c_string(s7, ERRSEXP);
        char *sexp = "(do ((e (outlet (owlet)) (outlet e))) "
            "((eq? e (rootlet))) "
            "(format () \"~{~A ~}~%\" e)) ";
        s7_eval_c_string(s7, sexp);
        s7_write(s7,
                 /* s7_make_string(s7, s7_car(args)), */
                 // s7_string(s7_car(args)),
                 /* TO_STR(s7_car(args)), */
                 s7_car(args),
                 s7_current_error_port(s7));
        fprintf(stdout, "[end error context]" CRESET "\n");

        /* (stacktrace) has no effect(?) */
        /* s7_eval_c_string(s7, "(stacktrace)"); */

        /* printf("EXIT ON ERROR? %s\n" TO_STR(s7_name_to_value(s7, *exit-on-error*))); */

        if (s7_name_to_value(s7, "*exit-on-error*") == s7_t(s7)) {
            fprintf(stdout, RED "exiting..." CRESET "\n");
            exit(EXIT_FAILURE);
        }

        /* s7_pointer eline = s7_eval_c_string(s7, "(with-let (owlet) error-line"); */
        /* fprintf(stderr, "file: %s, line: %s\n", TO_STR(efile), TO_STR(eline)); */

        /* fprintf(stderr, "%s\n", TO_STR(owlet)); */
        /* fprintf(stderr, "\n"); */
        /* fprintf(stderr, "%s\n", TO_STR(owlet)); */
        return(s7_f(s7));
    }
}

s7_pointer _s7_read_error_handler_opam(s7_scheme *s7, s7_pointer args)
{
    fprintf(stderr, RED "READ ERROR:" CRESET " %s\n",
            s7_string(s7_car(args)));
    s7_eval_c_string(s7, ERRSEXP);
    return(s7_f(s7));
}

void init_error_handlers_opam(void)
{
    /* s7_read_thunk_catcher = s7_make_function(s7, "s7-read-thunk-catcher", */
    /*                                          _s7_read_thunk_catcher, */
    /*                                          2, 0, false, ""); */

   s7_define_function(s7, "error-handler",
                       _s7_error_handler_opam, 1, 0, false,
                       "our error handler");

    s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) \n\
                            (list (lambda (hook) \n\
                                    (error-handler \n\
                                      (apply format #f (hook 'data))) \n\
                                    (set! (hook 'result) 'our-error))))");

    /* read-error-hook evidently only catches problems with # names
       and \ escapes, not general read errors. */
    s7_define_function(s7, "read-error-handler",
                       _s7_read_error_handler_opam, 1, 0, false,
                       "our read error handler");

    /* s7_eval_c_string(s7, "(set! (hook-functions *read-error-hook*) \n\ */
    /*                         (list (lambda (hook) \n\ */
    /*                                 (read-error-handler \n\ */
    /*                                   (apply format #f (hook 'data))) \n \ */
    /*                                 (set! (hook 'result) 'READ-error))))"); */


}

void error_config_opam(void)
{
    /* if (trace) log_trace(BLU "error_config" CRESET); */

    old_err_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_err_port != s7_nil(s7)) {
        /* if (s7_is_output_port(s7, old_err_port)) { */
        /*     s7_flush_output_port(s7, old_err_port); */
        /* } */
        gc_loc = s7_gc_protect(s7, old_err_port);
    }
    s7_flush_output_port(s7, s7_current_error_port(s7));
}

void close_error_config_opam(void) // s7_pointer err_port)
{
    s7_close_output_port(s7, s7_current_error_port(s7));

    s7_set_current_error_port(s7, old_err_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);
}
