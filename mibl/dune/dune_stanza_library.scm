;; (display "dune/dune_stanza_library.scm loading ...") (newline)

;; (load "dune_stanza_fields.scm")
;; (load "lookup_tables.scm")
;; (load "string.scm")
;; (load "utils.scm")
;; (require pp.scm)

;; (define (ppx-fld->args ppx)
;;   )

;; a null lib has an empty modules fld:  (modules)
(define (null-library? stanza)
  ;; (format #t "null-library? ~A" stanza) (newline)
  (let ((modules (assoc-val :modules (cdr stanza))))
    (if modules
        (if (null? (cdr modules))
            #t
            #f)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e.g. mina/src/lib/with_hash: (instrumentation (backend bisect_ppx))
(define (normalize-instrumentation fld-assoc)
  (format #t "normalize-instrumentation: ~A\n" fld-assoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inline_tests - generate ocaml_test
;; task: construct 'test' stanza to serve as test runner?
;; "Under the hood, a test executable is built by dune."

;; special cases: no 'backend' fld needed for the following preprocess
;; ppx libs:
;;   ppx_inline_test
;;   ppx_expect

;; question: should we "select" ppx_inline_test compilation? If we do
;; not intend to run inline tests, then we should not need to compile
;; with inline test support.

;; e.g. if no test, then omit ppx preproc on modules? but what if we
;; have multiple ppx libs? in that case, just omit ppx_inline_test
;; from the ppx_executable.

;; BUT: see ppx_inline_test/src/dune - the lib stanza specifies
;; 'inline_test.backend' that seems to build the required executable?

;; yes, 'backend' means build/run testrunner.
;; see https://dune.readthedocs.io/en/stable/tests.html?highlight=generate_runner#defining-your-own-inline-test-backend

(define (normalize-inline_tests fld-assoc stanza-alist)
  (format #t "normalize-inline_tests: ~A\n" fld-assoc))

(define (dune-library->mibl pkg stanza)
  (format #t "~A: ~A\n" (blue "DUNE-LIBRARY->MIBL")
          (assoc-val 'name (cdr stanza)))
  (format #t "stanza: ~A\n" stanza)

  ;; FIXME: if not wrapped => :archive
  ;; else => :library

  (let* ((stanza-alist (cdr stanza))
         (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                               stanza-alist
                               (append stanza-alist
                                       (list '(modules :standard)))))
         (_ (format #t "STANZA ALIST: ~A\n" stanza-alist))

         ;; (privname (assoc-val 'name stanza-alist))
         (wrapped? (if-let ((wrapped (assoc-val 'wrapped stanza-alist)))
                           (if (equal? 'false (car wrapped))
                                  #f
                                  #t)
                           #t))
         ;; (submods (lib-stanza-submodules stanza-alist))
         ;; (stanza-alist (cons submods stanza-alist))
         ;; (_ (format #t "STANZA ALIST + SUBMODS: ~A\n" stanza-alist))

         (mibl-stanza (dune-library-fields->mibl pkg stanza-alist wrapped?))
         )

    ;; namespaced?
    (let ((res (list (cons (if wrapped? :ns-archive :library)
                           mibl-stanza))))
      res)
    )
  ) ;; end normalize-stanza-library

;; (display "loaded dune/dune_stanza_library.scm") (newline)

