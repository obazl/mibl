#include <errno.h>
#include <stddef.h>

/* #include "log.h" */
/* #include "s7.h" */

/* #if EXPORT_INTERFACE */
/* #include "utarray.h" */
/* #include "utstring.h" */
/* #endif */

/* #if defined(DEBUG_TRACE) */
/* #include "debug.h" */
/* #endif */


#include "s7.h"

#include "dune_readers.h"

#if LOCAL_INTERFACE
#include <string.h>
#endif

extern const UT_icd ut_str_icd;

extern s7_scheme *s7;
extern UT_string *opam_switch_bin;
extern bool verbose;
#if defined(DEBUG_TRACE)
extern bool debug;
extern bool trace;
#endif

#define TO_STR(x) s7_object_to_c_string(s7, x)

/* s7_pointer */
void *read_dune_package(UT_string *dunefile_name)
{
    //FIXME: this duplicates the code in load_dune:_read_dunefile
#if defined(DEBUG_TRACE)
    if (trace) log_trace("read_dune_package: %s", utstring_body(dunefile_name));
#endif

    char *dunestring = dunefile_to_string(dunefile_name);
/* #if defined(DEBUG_TRACE) */
/*     if (debug) log_debug("readed str: %s", dunestring); */
/* #endif */

    /* stanza accumulator */
    s7_pointer stanzas = s7_list(s7, 0);

    s7_pointer sport = s7_open_input_string(s7, dunestring);

    const char *errmsg;
    if (!s7_is_input_port(s7, sport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            printf(RED "ERROR" CRESET "s7_open_input_string failed\n");
            log_error("[%s\n]", errmsg);
            s7_quit(s7);
            exit(EXIT_FAILURE);
        }
    }

#if defined(DEBUG_TRACE)
    if (debug) log_debug("s7 reading stanzas");
#endif

    /* read all stanzas in dunefile */
    while(true) {
/* #if defined(DEBUG_TRACE) */
/*         if (debug) log_debug("iter"); */
/* #endif */
        s7_pointer stanza = s7_read(s7, sport);
        /* FIXME: error checks */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* if ((errmsg) && (*errmsg)) { */
        /*     if (debug) log_error("[%s\n]", errmsg); */
        /*     s7_close_input_port(s7, sport); */
        /*     s7_quit(s7); */
        /*     exit(EXIT_FAILURE); */
        /*     break; */
        /* } */
        if (stanza == s7_eof_object(s7)) break;
        if (s7_is_null(s7,stanzas)) {
            stanzas = s7_list(s7, 1, stanza);
        } else{
            stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
        }
    }
    s7_close_input_port(s7, sport);
#if defined(DEBUG_TRACE)
    if (debug) log_debug("finished reading");
#endif

    /* s7_gc_unprotect_at(s7, baddot_gc_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    /* free(dunestring); */
    return stanzas;
}

//s7_pointer
EXPORT UT_array *get_pkg_executables(void *_stanzas)
/* UT_string *dune_pkg_file) */
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("get_pkg_executables");
#endif
    s7_pointer stanzas = (s7_pointer) _stanzas;
    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *bins;
    utarray_new(bins, &ut_str_icd);

    /* s7_pointer stanzas = read_dune_package(dune_pkg_file); */

    s7_pointer iter, binfile;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * exec_sexp =
        "(let ((files (assoc 'files (cdr stanzas))))"
        "  (if files"
        "      (let ((bin (assoc 'bin (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer executables = s7_eval_c_string_with_environment(s7, exec_sexp, e);

    if (executables == s7_unspecified(s7))
        return bins;

#if defined(DEBUG_TRACE)
    if (debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        log_debug(RED "executables" CRESET ": %s", TO_STR(executables));
    }
#endif

    /* /\* result is list of executables installed in $PREFIX/bin *\/ */
    /* if (s7_is_list(s7, executables)) { */
    /*     if (verbose) { */
    /*         printf(GRN "EXECUTABLES:" CRESET " %s\n", */
    /*                /\* " for %s: %s\n", *\/ */
    /*                /\* utstring_body(dune_pkg_file), *\/ */
    /*                TO_STR(executables)); */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, executables);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter))
        fprintf(stderr, "%d: %s is not an iterator\n",
                __LINE__, TO_STR(iter));
    if (s7_iterator_is_at_end(s7, iter))
        fprintf(stderr, "%d: %s is prematurely done\n",
                __LINE__, TO_STR(iter));

    char *f;
    while (true) {
        binfile = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEBUG_TRACE)
        log_debug("\tbin: %s", TO_STR(binfile));
#endif
        f = TO_STR(binfile);
        utarray_push_back(bins, &f);
    }
        /* utstring_renew(opam_bin); */
        /* utstring_printf(opam_bin, "%s/%s", */
        /*                 utstring_body(opam_switch_bin), */
        /*                 TO_STR(binfile)); */

        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s/bin/%s", */
        /*                 obazl, pkg, TO_STR(binfile)); */
        /* rc = symlink(utstring_body(opam_bin), */
        /*              utstring_body(outpath)); */
        /* if (rc != 0) { */
        /*     if (errno != EEXIST) { */
        /*         perror(NULL); */
        /*         fprintf(stderr, "exiting\n"); */
        /*         exit(EXIT_FAILURE); */
        /*     } */
        /* } */
        /* if (!emitted_bootstrapper) */
        /*     emit_local_repo_decl(bootstrap_FILE, pkg); */

        /* fprintf(ostream, "exports_files([\"%s\"])\n", TO_STR(binfile)); */
        /* fprintf(ostream, "## src: %s\n", utstring_body(opam_bin)); */
        /* fprintf(ostream, "## dst: %s\n", utstring_body(outpath)); */
    /* } */
    return bins;
}

EXPORT UT_array *get_pkg_stublibs(char *pkg, void *_stanzas)
/* UT_string *dune_pkg_file) */
{
#if defined(DEBUG_TRACE)
    if (trace) log_trace("get_pkg_stublibs");
#endif
    s7_pointer stanzas = (s7_pointer) _stanzas;
/* #if defined(DEBUG_TRACE) */
/*     log_debug("stanzas: %s", TO_STR(stanzas)); */
/* #endif */

    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *stubs;
    utarray_new(stubs, &ut_str_icd);

    /* s7_pointer stanzas = read_dune_package(dune_pkg_file); */

    s7_pointer iter, stublib_file;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * stublibs_sexp =
        "(let ((files (assoc 'files (cdr stanzas))))"
        "  (if files"
        "      (let ((bin (assoc 'stublibs (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer stublibs = s7_eval_c_string_with_environment(s7, stublibs_sexp, e);

    if (stublibs == s7_unspecified(s7))
        return stubs;

#if defined(DEBUG_TRACE)
    if (debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        log_debug(RED "STUBLIBS" CRESET ": %s", TO_STR(stublibs));
    }
#endif

    /* result is list of stublibs installed in $PREFIX/bin */
    /* if (s7_is_list(s7, stublibs)) { */
    /*     if (verbose) { */
    /*         log_info(GRN "%s stublibs:" CRESET " %s", */
    /*                  pkg, */
    /*                  /\* " for %s: %s\n", *\/ */
    /*                  /\* utstring_body(dune_pkg_file), *\/ */
    /*                  TO_STR(stublibs)); */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, stublibs);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter))
        fprintf(stderr, "%d: %s is not an iterator\n",
                __LINE__, TO_STR(iter));
    if (s7_iterator_is_at_end(s7, iter))
        fprintf(stderr, "%d: %s is prematurely done\n",
                __LINE__, TO_STR(iter));

    char *f;
    while (true) {
        stublib_file = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEBUG_TRACE)
        log_debug("\tstublib: %s", TO_STR(stublib_file));
#endif
        f = TO_STR(stublib_file);
        utarray_push_back(stubs, &f);
    }
    return stubs;
}

char *dunefile_to_string(UT_string *dunefile_name)
{
#if defined(DEBUG_TRACE)
    if (trace)
        log_trace("dunefile_to_string: %s", utstring_body(dunefile_name));
#endif
    /* core/dune file size: 45572 */
    // 2K
#define BUFSZ 131072
    static char inbuf[BUFSZ];
    memset(inbuf, '\0', BUFSZ);
    static char outbuf[BUFSZ + 20];
    memset(outbuf, '\0', BUFSZ);

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    FILE *instream = fopen(utstring_body(dunefile_name), "r");
    if (instream == NULL) {
        printf(RED "ERROR" CRESET "fopen failure: %s\n",
               utstring_body(dunefile_name));
        perror(NULL);
        exit(EXIT_FAILURE);
    } else {
#if defined(DEBUG_TRACE)
        if (debug) log_debug("fopened %s", utstring_body(dunefile_name));
#endif
    }
    fseek(instream, 0, SEEK_END);
    uint64_t fileSize = ftell(instream);
#if defined(DEBUG_TRACE)
    if (debug) log_debug("filesize: %d", fileSize);
#endif

    if (fileSize > BUFSZ) {
        printf(RED "ERROR:" CRESET " dune file '%s' size (%llu) > BUFSZ (%d)\n", utstring_body(dunefile_name), fileSize, BUFSZ);
        log_error("dune file size (%d) > BUFSZ (%d)", fileSize, BUFSZ);
        exit(EXIT_FAILURE);     /* FIXME: exit gracefully */
    }
    rewind(instream);

    /* char *outbuf = malloc(fileSize + 1); */
    /* memset(outbuf, '\0', fileSize); */

    uint64_t outFileSizeCounter = fileSize;

    /* we fread() bytes from instream in COPY_BUFFER_MAXSIZE increments,
       until there is nothing left to fread() */
    int read_ct = 0;
    do {
        /* printf("reading...\n"); */
        if (outFileSizeCounter > BUFSZ) {
            /* probably won't see a 16K dune file */
            read_ct = fread(inbuf, 1, (size_t) BUFSZ, instream);
            if (read_ct != BUFSZ) {
                if (ferror(instream) != 0) {
                    printf(RED "ERROR" CRESET " fread error 1 for %s\n",
                              utstring_body(dunefile_name));
                    log_error("fread error 1 for %s\n",
                              utstring_body(dunefile_name));
                    exit(EXIT_FAILURE); //FIXME: exit gracefully
                } else {
                    printf("xxxxxxxxxxxxxxxx\n");
                }
            } else {
                printf("aaaaaaaaaaaaaaaa\n");
            }
            /* log_debug("writing"); */
            outFileSizeCounter -= BUFSZ;
        }
        else {
            read_ct = fread(inbuf, 1, (size_t) outFileSizeCounter, instream);
#if defined(DEBUG_TRACE)
            if (debug) log_debug("read_ct: %d", read_ct);
#endif
            if (read_ct != outFileSizeCounter) {
                if (ferror(instream) != 0) {
                    printf(RED "ERROR" CRESET "fread error 2 for %s\n",
                              utstring_body(dunefile_name));
                    log_error("fread error 2 for %s\n",
                              utstring_body(dunefile_name));
                    exit(EXIT_FAILURE); //FIXME: exit gracefully
                } else {
                    if (feof(instream) == 0) {
                        printf(RED "ERROR" CRESET "fread error 3 for %s\n",
                              utstring_body(dunefile_name));
                        log_error("fread error 3 for %s\n",
                                  utstring_body(dunefile_name));
                        exit(EXIT_FAILURE); //FIXME: exit gracefully
                    } else {
                        /* printf("bbbbbbbbbbbbbbbb\n"); */
                    }
                }
            }
            outFileSizeCounter = 0ULL;
        }
    } while (outFileSizeCounter > 0);
#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug(RED "readed" CRESET " %d bytes", read_ct);
        /* log_debug(RED "readed string:" CRESET " '%s'", inbuf); */
    }
#endif
    fclose(instream);

    // FIXME: loop over the entire inbuf
    char *inptr = (char*)inbuf;
    char *outptr = (char*)outbuf;
    char *cursor = inptr;

    while (true) {
        cursor = strstr(inptr, ".)");

/* https://stackoverflow.com/questions/54592366/replacing-one-character-in-a-string-with-multiple-characters-in-c */

        if (cursor == NULL) {
/* #if defined(DEBUG_TRACE) */
/*             if (debug) log_debug("remainder: '%s'", inptr); */
/* #endif */
            size_t ct = strlcpy(outptr, (const char*)inptr, fileSize); // strlen(outptr));
            (void)ct;           /* prevent -Wunused-variable */
/* #if defined(DEBUG_TRACE) */
/*             if (debug) log_debug("concatenated: '%s'", outptr); */
/* #endif */
            break;
        } else {
#if defined(DEBUG_TRACE)
            if (debug) log_error("FOUND and fixing \".)\" at pos: %d", cursor - inbuf);
#endif
            size_t ct = strlcpy(outptr, (const char*)inptr, cursor - inptr);
#if defined(DEBUG_TRACE)
            if (debug) {
                log_debug("copied %d chars", ct);
                /* log_debug("to buf: '%s'", outptr); */
            }
#endif
            if (ct >= BUFSZ) {
                printf("output string has been truncated!\n");
            }
            outptr = outptr + (cursor - inptr) - 1;
            outptr[cursor - inptr] = '\0';
            ct = strlcat(outptr, " ./", BUFSZ);
            outptr += 3;

            inptr = inptr + (cursor - inptr) + 1;
            /* printf(GRN "inptr:\n" CRESET " %s\n", inptr); */

            if (ct >= BUFSZ) {
                printf(RED "ERROR" CRESET "write count exceeded output bufsz\n");
                exit(EXIT_FAILURE);
                // output string has been truncated
            }
        }
    }
    return outbuf;
}
