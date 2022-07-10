#include "s7.h"
#include "log.h"
#include "utstring.h"

#include "error_handler.h"

s7_pointer old_err_port;
const char *errmsg = NULL;
int gc_loc = -1;

#define ERRSEXP "(with-let (owlet) " \
    "(format #t \"file: ~A, line ~A\n\" error-file error-line))"

s7_pointer s7_error_handler(s7_scheme *sc, s7_pointer args)
{
    /* log_error("error: %s\n", s7_string(s7_car(args))); */
    fprintf(stderr, RED "ERROR:" CRESET " %s\n", s7_string(s7_car(args)));
    s7_eval_c_string(s7, ERRSEXP);

    /* s7_pointer eline = s7_eval_c_string(s7, "(with-let (owlet) error-line"); */
    /* fprintf(stderr, "file: %s, line: %s\n", TO_STR(efile), TO_STR(eline)); */

    /* fprintf(stderr, "%s\n", TO_STR(owlet)); */
    /* fprintf(stderr, "\n"); */
    /* fprintf(stderr, "%s\n", TO_STR(owlet)); */
    return(s7_f(sc));
}

void init_error_handling(void)
{
    s7_define_function(s7, "error-handler",
                       s7_error_handler, 1, 0, false,
                       "our error handler");

    /* if (with_error_hook) */
    s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) \n\
                            (list (lambda (hook) \n\
                                    (error-handler \n\
                                      (apply format #f (hook 'data))) \n\
                                    (set! (hook 'result) 'our-error))))");
}

void error_config(void)
{
    old_err_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_err_port != s7_nil(s7))
        gc_loc = s7_gc_protect(s7, old_err_port);
    /* s7_flush_output_port(s7, old_err_port); */
    /* s7_flush_output_port(s7, s7_current_error_port(s7)); */

}

void close_error_config(void) // s7_pointer err_port)
{
    s7_close_output_port(s7, s7_current_error_port(s7));

    s7_set_current_error_port(s7, old_err_port);
    if (gc_loc != -1)
        s7_gc_unprotect_at(s7, gc_loc);
}

char *dunefile_to_string(UT_string *dunefile_name)
{
    /* FIXME: use malloc, this will rarely be called */
    /* 16K should be enough for any dunefile */
#define BUFSZ 16384
    static char buffer[BUFSZ];
    memset(buffer, '\0', BUFSZ);
    /* FIXME: what about e.g. unicode in string literals? */

    FILE *inFp = fopen(utstring_body(dunefile_name), "r");
    fseek(inFp, 0, SEEK_END);
    uint64_t fileSize = ftell(inFp);
    /* log_debug("filesize: %d", fileSize); */
    if (fileSize > BUFSZ) {
        log_error("dune file size (%d) > BUFSZ (%d)\n", fileSize, BUFSZ);
        exit(EXIT_FAILURE);     /* FIXME: exit gracefully */
    }
    rewind(inFp);

    char *fixbuf = malloc(fileSize + 1);
    memset(fixbuf, '\0', fileSize);

    uint64_t outFileSizeCounter = fileSize;

    /* we fread() bytes from inFp in COPY_BUFFER_MAXSIZE increments,
       until there is nothing left to fread() */
    int read_ct = 0;
    do {
        if (outFileSizeCounter > BUFSZ) {
            /* probably won't see a 16K dune file */
            read_ct = fread(buffer, 1, (size_t) BUFSZ, inFp);
            if (read_ct != BUFSZ) {
                if (ferror(inFp) != 0) {
                    log_error("fread error 1 for %s\n",
                              utstring_body(dunefile_name));
                    exit(EXIT_FAILURE); //FIXME: exit gracefully
                }
            }
            /* log_debug("writing"); */
            outFileSizeCounter -= BUFSZ;
        }
        else {
            read_ct = fread(buffer, 1, (size_t) outFileSizeCounter, inFp);
            if (read_ct != outFileSizeCounter) {
                if (ferror(inFp) != 0) {
                    log_error("fread error 2 for %s\n",
                              utstring_body(dunefile_name));
                    exit(EXIT_FAILURE); //FIXME: exit gracefully
                } else {
                    if (feof(inFp) == 0) {
                        log_error("fread error 3 for %s\n",
                                  utstring_body(dunefile_name));
                        exit(EXIT_FAILURE); //FIXME: exit gracefully
                    }
                }
            }
            outFileSizeCounter = 0ULL;
        }
    } while (outFileSizeCounter > 0);
    /* log_debug("readed %d bytes", read_ct); */
    fclose(inFp);

    // FIXME: loop over the entire buffer

    char *cursor = strstr((const char*) buffer, ".)");
    if (cursor == NULL) {
        // FIXME: should not happen, we only get here if s7_read choke
        // on ".)"
    } else {
        /* log_debug("FOUND \".)\" at pos: %d", cursor - buffer); */
        size_t ct = strlcpy(fixbuf, (const char*)buffer, cursor - buffer);
        if (ct >= BUFSZ) {
            // output string has been truncated
        }
        fixbuf[cursor - buffer] = '\0';
        ct = strlcat(fixbuf, "./", BUFSZ);
        if (ct >= BUFSZ) {
            // output string has been truncated
        }
        /* log_debug("first seg: %s", fixbuf); */
        /* log_debug("first seg len: %d", strlen((char*)fixbuf)); */
        /* log_debug("cursor - buffer = %d", cursor - buffer); */
        /* log_debug("second seg %s", buffer + 225); */
        ct = strlcat((char*)fixbuf, buffer + (cursor - buffer) + 1, BUFSZ);
        if (ct >= BUFSZ) {
            // output string has been truncated
        }
        /* log_debug("fixed: %s", (char*)fixbuf); */

    }
    return fixbuf;
}

s7_pointer fix_baddot(UT_string *dunefile_name)
{
    log_debug("fix_baddot");

    char *dunestring = dunefile_to_string(dunefile_name);

    /* now s7_read using string port */

    /* first config err handling. clears out prev. error */
    error_config();

    /* stanza accumulator */
    s7_pointer stanzas = s7_list(s7, 0);

    s7_pointer sport = s7_open_input_string(s7, dunestring);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if (!s7_is_input_port(s7, sport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            s7_shutdown(s7);
            exit(EXIT_FAILURE);
        }
    }
    if (debug)
        log_debug("s7_open_input_string for error correction");

    /* read all stanzas in dunefile */
    while(true) {
        /* log_debug("iter"); */
        s7_pointer stanza = s7_read(s7, sport);
        /* log_debug("stanza: %s", stanza); */
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            if (debug) log_error("[%s\n]", errmsg);
            s7_close_input_port(s7, sport);
            s7_shutdown(s7);
            exit(EXIT_FAILURE);
            break;
        }
        if (stanza == s7_eof_object(s7)) break;
        log_debug("stanza: %s", TO_STR(stanza));
        if (s7_is_null(s7,stanzas)) {
            stanzas = s7_list(s7, 1, stanza);
        } else{
            stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
        }
    }
    s7_close_input_port(s7, sport);
    /* leave error config as-is */
    free(dunestring);
    return stanzas;
}
