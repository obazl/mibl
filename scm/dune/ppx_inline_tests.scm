;; test cases:  ppx_assert, ppx_bench

(define (ppx-inline-tests! ws)
  (if (or *mibl-debug-ppx* *mibl-debug-s7*)
      (format #t "~A~%" (ublue "ppx-inline-tests!")))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                ;; (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                ;;     (begin
                ;;       (format #t "~A: ~A~%" (ugreen "ppx for pkg") (car kv))
                ;;       ;; (format #t "~A: ~A~%" (ugreen "pkg") kv))
                ;;     ))
                (let* ((pkg-path (car kv))
                       (_ (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                              (format #t "~%~A: ~A  //~A~%" (bggreen "pkg") (car kv) pkg-path)))
                       (pkg (cdr kv))
                       (dune (assoc :mibl pkg))
                       (stanzas (cdr dune))
                       (shared-ppx (if-let ((shared (assoc-val :shared-ppx stanzas)))
                                           (car shared) '()))
                       (shared-deps (if-let ((shared (assoc-val :shared-deps stanzas)))
                                            (car shared) '()))
                       )
                       (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                           (begin
                             (format #t "~A: ~A~%" (ugreen "shared-ppx") shared-ppx)
                        (format #t "~A: ~A~%" (ugreen "shared-deps") shared-deps)))
                  (for-each (lambda (stanza)
                              ;; (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                              ;;     (format #t "~A: ~A~%" (green "stanza")
                              ;;             stanza))
                              (case (car stanza)
                                ((:ns-archive)
                                 (format #t "~A: ~A~%" (red "ns-archive") stanza)
                                 (let* ((stanza-alist (cdr stanza))
                                        (ppx-id (assoc-val :ppx stanza-alist)))
                                   (format #t "~A: ~A~%" (red "ppx id") ppx-id)
                                   (if ppx-id
                                       (let* ((ppx (assoc-val ppx-id shared-ppx))
                                              (manifest (assoc-val :manifest ppx)))
                                         (format #t "~A: ~A~%" (red "ppx") ppx)
                                         (format #t "~A: ~A~%" (red "manifest") manifest)
                                         (if (member '@ppx_inline_test//lib/ppx_inline_test manifest)
                                             (begin
                                               (format #t "~A~%" (red "INLINE TEST"))
                                               (set-car! stanza :ppx-inline-test-lib)))
                                         )))
                                 )
                                ((:shared-ppx))
                                ((:shared-deps))
                                (else
                                  (format #t "~A: ~A~%" (red "other")
                                          stanza))
                                 )
                              ;; (format #t "~A: ~A~%" (red "STANZA")
                              ;;         stanza)
                              )
                            stanzas)
                  (values)))
              pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (red "pkg") (car kv))
                (let* ((pkg (cdr kv))
                       (dune (assoc :mibl pkg))
                       (stanzas (cdr dune)))
                  (for-each (lambda (s)
                              (format #t "~A: ~A~%" (red "stanza") s))
                            stanzas)))
              pkgs)
    '())
  )

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
  (if (or *mibl-debug-s7* *mibl-debug-tests*)
      (format #t "~A: ~A\n" (ublue "-inline-tests->args") inline-tests))
  (let* ((result
          (map (lambda (fld)
                 (if (or *mibl-debug-s7* *mibl-debug-tests*)
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
    result))

;; handle both (preprocess) and (inline_tests) (and what else?)
(define (inline-tests->mibl ws pkg inline-tests stanza-alist)
  (if (or *mibl-debug-s7* *mibl-debug-tests*)
      (begin
        (format #t "~A: ~A~%" (ublue "inline-tests->mibl") inline-tests)
        (format #t "~A: ~A~%" (ublue "stanza-alist") stanza-alist)))
  (let ((ilts (-inline-tests->args ws pkg inline-tests)))
    (if (or *mibl-debug-s7* *mibl-debug-tests*)
        (format #t "~A: ~A~%" (ublue "ilt args") ilts))
    `(:inline-tests ,@ilts)))
    ;; (if-let ((pp-assoc (assoc 'preprocess stanza-alist)))
    ;;         (begin
    ;;           (if (or *mibl-debug-s7* *mibl-debug-tests*)
    ;;               (format #t "~A: ~A~%" (blue "pp-assoc") pp-assoc))
    ;;           (if-let ((ppx (preprocess-fld->mibl pp-assoc stanza-alist)))
    ;;                   (begin
    ;;                     (if (or *mibl-debug-s7* *mibl-debug-tests*)
    ;;                         (format #t "~A: ~A~%" (bgyellow "ppx") ppx))
    ;;                     `(:ppx ,@(append (cdr ppx)
    ;;                                    ilts)))
    ;;                   ;; else no ppx in (preprocess)
    ;;                   (begin
    ;;                     `(:inline-tests ,@ilts))))
    ;;         ;; else (inline_tests) only, no (preprocess)
    ;;         `(:inline-tests ,@ilts))))
