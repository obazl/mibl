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

;; (define (dune-library-fields->mibl pkg stanza-alist wrapped?)
;;   (format #t "~A: ~A\n" (blue "DUNE-LIBRARY-FIELDS->MIBL") stanza-alist)
;;   (format #t "~A: ~A\n" (blue "pkg") pkg)
;;   (format #t "~A: ~A\n" (blue "wrapped?") wrapped?)

;;   ;; 'libraries' and 'modules' flds may return multiple flds
;;   ;; so excluded them from mapping, handle separately
;;   ;; 'libraries' fld may generate :directs, :seldeps and :conditionals
;;   (let* ((deps (if-let ((deps (assoc-val 'libraries stanza-alist)))
;;                        (dune-library-deps-fld->mibl deps) '()))
;;          (_ (format #t "mibldeps: ~A\n" deps))
;;          ;; FIXME: deal with private_modules too
;;          (modules (-get-modules pkg stanza-alist))
;;          (flds (map
;;                 (lambda (fld-assoc)
;;                   (format #t "lib fld-assoc: ~A\n" fld-assoc)
;;                   (case (car fld-assoc)
;;                     ((name) `(:privname ,(cadr fld-assoc)))
;;                     ((public_name) `(:pubname ,(cadr fld-assoc)))

;;                     ((flags) (normalize-stanza-fld-flags fld-assoc :mod))
;;                     ((library_flags) (normalize-stanza-fld-flags fld-assoc :lib))
;;                     ;; (values) returns "nothing"
;;                     ((libraries) (values)) ;; handled separately
;;                     ((modules) (values)) ;; handled separately

;;                     ;; (format #t "direct: ~A, indirect ~A\n"
;;                     ;;         direct indirect)
;;                     ;; (let* ((raw `((:raw ,fld-assoc)))
;;                     ;;        (direct (if (null? direct)
;;                     ;;                    raw
;;                     ;;                    (cons `(:direct ,@direct) raw)))
;;                     ;;        (indirect (if (null? indirect)
;;                     ;;                      direct
;;                     ;;                      (cons `(:indirect
;;                     ;;                              ,@(reverse indirect))
;;                     ;;                            direct)))
;;                     ;;        ;; if not wrapped, add :resolver
;;                     ;;        (resolver (if-let ((wrapped (assoc :wrapped stanza-alist)))
;;                     ;;                          (if (equal? wrapped 'false)
;;                     ;;                              #f
;;                     ;;                              #t)
;;                     ;;                          ;; default: ns archive with resolver module
;;                     ;;                          (cons `(:resolver ,privname)
;;                     ;;                                indirect)))
;;                     ;;        (result `(:submodules ,resolver))) ;;FIXME: (:deps ((:submodules ...)(:resolver ...)))
;;                     ;;   ;; (format #t "XRESULT ~A: ~A\n" privname result)
;;                     ;;   result)
;;                     ;; ))

;;                     ;;    ;;   ;; ((inline_tests)
;;                     ;;    ;;   ;;  (normalize-inline_tests fld-assoc stanza-alist))

;;                     ;;    ;;   ((instrumentation)
;;                     ;;    ;;    (normalize-instrumentation fld-assoc))

;;                     ;;    ;;   ((ocamlopt_flags) fld-assoc) ;; vendors/numeric/lib
;;                     ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
;;                     ;;    ;;   ;;         pkg-path (car fld-assoc))

;;                     ;;    ;;   ((private_modules)
;;                     ;;    ;;    ;; (private_modules <modules>) specifies a list of modules that will
;;                     ;;    ;;    ;; be marked as private. Private modules are inaccessible from outside
;;                     ;;    ;;    ;; the libraries they are defined in. Note that the private_modules
;;                     ;;    ;;    ;; field is not merged in modules, which represents the total set of
;;                     ;;    ;;    ;; modules in a library. If a directory has more than one stanza and
;;                     ;;    ;;    ;; thus a modules field must be specified, <modules> still need to be
;;                     ;;    ;;    ;; added in modules.
;;                     ;;    ;;    ;; for bazel? these are deps but not submodules?
;;                     ;;    ;;    ;; (format #t "unhandled lib fld: ~A:: ~A\n"
;;                     ;;    ;;    ;;         pkg-path (car fld-assoc))
;;                     ;;    ;;    fld-assoc)

;;                     ;;    ;;   ((modules_without_implementation) fld-assoc)

;;                     ;;    ;;   ((preprocess)
;;                     ;;    ;;    (let ((pp-attrs (normalize-preproc-attrs
;;                     ;;    ;;                     (cdr fld-assoc) stanza-alist)))
;;                     ;;    ;;      ;; e.g. (((:ppx "ppx_tezos_stdlib") (:ppx-args (-foo -bar 42))))
;;                     ;;    ;;      ;; (format #t "PP ATTRS: ~A\n" pp-attrs)
;;                     ;;    ;;      `,@pp-attrs))

;;                     ;;    ;;   ((public_name) '())

;;                     ;;    ;;   ((synopsis) fld-assoc)

;;                     ((wrapped) (values))
;;                     ;; `(:namespaced ,(if wrapped? #t #f)))

;;                     ;;    ;;   ;; c stuff

;;                     ;;    ;;   ((foreign_stubs)
;;                     ;;    ;;    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

;;                     ;;    ;;   ((c_names) fld-assoc) ;; e.g. vendors/numerics/lib
;;                     ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
;;                     ;;    ;;   ;;         pkg-path (car fld-assoc))

;;                     ;;    ;;   ((c_flags) fld-assoc) ;; e.g. vendors/numerics/lib
;;                     ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
;;                     ;;    ;;   ;;         pkg-path (car fld-assoc))

;;                     ;;    ;;   ((c_library_flags) fld-assoc) ;; e.g. src/lib_sapling
;;                     ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
;;                     ;;    ;;   ;;         pkg-path (car fld-assoc))

;;                     (else
;;                      (begin
;;                        ;; (format #t "unhandled lib fld: ~A\n" fld-assoc)
;;                        fld-assoc))
;;                     ) ;; end case
;;                   ) ;; end lamda
;;                 stanza-alist) ;; end map
;;                )) ;; end let bindings
;;     ;; now handle libraries and modules
;;     (format #t "~A: ~A\n" (red "DEPS FLDS") deps)
;;     (let ((depslist (remove '()
;;                             (list :deps
;;                                   (if-let ((fixed (assoc :fixed deps)))
;;                                           fixed '())
;;                                   (if-let ((conds (assoc :conditionals deps)))
;;                                           conds '())
;;                                   (if-let ((seldeps (assoc :seldeps deps)))
;;                                           seldeps '()))))
;;           (submods (if-let ((submods-assoc (assoc :submodules modules)))
;;                            (let ((submods-list (cdr submods-assoc)))
;;                              (cons :submodules (sort! submods-list sym<?)))
;;                            '()))
;;           (subsigs (if-let ((ssigs (assoc :subsigs modules)))
;;                            ssigs '())))
;;       (format #t "SUBMODS:: ~A\n" submods)
;;       (format #t "SUBSIGS:: ~A\n" subsigs)
;;       (append flds (remove '() (list depslist
;;                                      (if wrapped? '(:namespaced #t)
;;                                          '(:namespaced #f))
;;                                      submods
;;                                      subsigs))))))

         ;; (normalized-stanza (if namespaced
         ;;                        (cons '(:namespaced #t) normalized-stanza)
         ;;                        normalized-stanza))
         ;; ;; (ppx (lib-stanza->ppx stanza-alist))
         ;; )

;; (define foobar
;;   (let (
;;          (normalized-stanza
;;           (remove
;;            '()
;;     ;; (if ppx
;;     ;;     (format #t "PPX stanza: ~A\n" ppx))

;;     ;; if 'modules' fld is missing then add all modules
;;     (let ((result
;;            (if-let ((mods (assoc :submodules normalized-stanza)))
;;                    (list :library normalized-stanza)
;;                    (if (null? srcfiles)
;;                        (list :library normalized-stanza)
;;                        ;; no 'modules', so add all
;;                        (list :library
;;                              (cons
;;                               `(:submodules ,(srcfiles->modstbl srcfiles))
;;                               normalized-stanza))))))
;;       ;; (if ppx
;;       ;;     (list result ppx)
;;       ;;     result))))
;;       result))
;; )

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
    (let ((res (list (cons (if wrapped? :archive :library) mibl-stanza))))
      res)
    )
  ) ;; end normalize-stanza-library

;; (display "loaded dune/dune_stanza_library.scm") (newline)

