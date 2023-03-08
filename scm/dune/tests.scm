;; (define (normalize-stanza-test pkg-path srcfiles stanza)

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

(define (-inline-tests->args ws pkg inline-tests)
  (if *mibl-debugging*
      (format #t "~A: ~A\n" (ublue "-inline-tests->args") inline-tests))
  (let* ((result
          (map (lambda (fld)
                 (if *mibl-debugging*
                     (format #t "~A: ~A~%" (uwhite "fld") fld))
                 (case (car fld)
                   ((flags)
                    (normalize-stanza-fld-flags fld :runtime))
                   ((modes) (cons :modes (cdr fld)))
                   ((deps)
                    `(:runtime-data-deps
                      ,@(expand-terms* ws (cdr fld) pkg '())))
                   ;; (expand-rule-deps ws pkg rule-alist))
                   (else
                    (error 'FIXME
                           (format #f "inline_tests unrecognize fld: ~A"
                                   fld)))))
               inline-tests))
         (result (append
                  '((:opts ("-inline-test-lib" . $LIBNAME)))
                  '((:inline-tests #t)) result)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (bgred "result") result))
    result))

;; handle both (preprocess) and (inline_tests) (and what else?)
(define (inline-tests->mibl ws pkg inline-tests stanza-alist)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "inline-tests->mibl") inline-tests))
  (let ((ilts (-inline-tests->args ws pkg inline-tests)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (ublue "ilts") ilts))
    (if-let ((pp-assoc (assoc 'preprocess stanza-alist)))
            (begin
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (blue "pp-assoc") pp-assoc))
              (if-let ((ppx (preprocess-fld->mibl pp-assoc stanza-alist)))
                      (begin
                        (if *mibl-debugging*
                            (format #t "~A: ~A~%" (bgyellow "ppx") ppx))
                        `(:ppx ,@(append (cdr ppx)
                                       ilts)))
                      ;; else no ppx in (preprocess)
                      (begin
                        `(:inline-tests ,@ilts))))
            ;; else (inline_tests) only, no (preprocess)
            `(:inline-tests ,@ilts))))

(define (dune-test->mibl ws pkg stanza)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "dune-test->mibl") stanza))

  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (_ (if *mibl-debugging* (format #t "~A: ~A~%" (white "pkg-path") pkg-path)))
         (stanza-alist (cdr stanza))
         (privname (assoc 'name stanza-alist)))
    (let ((t (list :test ;; (car stanza)
                   (remove
                    '()
                    (map (lambda (fld-assoc)
                           (case (car fld-assoc)
                             ((name) (normalize-stanza-fld-name
                                      pkg-path privname stanza-alist))
                             ((public_name) '())
                             ;; ((name) (normalize-stanza-fld-name (cadr fld-assoc)))
                             ((flags) (normalize-stanza-fld-flags (cadr fld-assoc)))
                             ((foreign_stubs)
                              (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))
                             (else fld-assoc)))
                         (cdr stanza))))))
      (if *mibl-debugging*
          (format #t "~A: ~A~%" (red "mibl t stanza") t))
      t)))

(define (normalize-stanza-tests pkg-path ocaml-srcs stanza)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "normalize-stanza-tests") stanza))
  (dune-executables->mibl :test pkg-path ocaml-srcs stanza))
