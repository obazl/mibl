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

;; caveat: pps ppx_inline_test implies (inline_tests), but not the
;; other way around.
(define (lib-stanza->ppx stanza-alist)
  (if-let ((preproc (assoc 'preprocess stanza-alist)))
          (if-let ((ppx (assoc 'pps (cdr preproc))))
                  (begin
                    ;; (format #t "lib-stanza->ppx ~A\n" stanza-alist)
                    (let* ((stanza-name (symbol->string
                                         (cadr (assoc 'name stanza-alist))))
                           (ppx-alist (normalize-ppx-attrs (cdr ppx)
                                                           stanza-name))

                           (ppx-name (string-append "ppx_" stanza-name))
                           ;; (args (ppx-fld->args ppx))
                           (inline-tests? (if (assoc 'inline_tests stanza-alist)
                                              #t #f)))
                      ;; (format #t "X: ~A\n" ppx-alist)
                      `(:ppx ((:name ,ppx-name)
                              (:args (-bar))
                              (:deps ((:constant (ppx_inline_test))))))))
                  #f)
          #f))

(define (normalize-lib-fields stanza-alist)
  (format #f "normalize-lib-fields: ~A\n" stanza-alist)
  (map
   (lambda (fld-assoc)
     (format #t "lib fld-assoc: ~A\n" fld-assoc)
     (case (car fld-assoc)
       ((name) fld-assoc)
       ((public_name) fld-assoc)

       ((flags) (normalize-stanza-fld-flags fld-assoc))

  ;;    ;;   ((library_flags)
  ;;    ;;    (let ((lf (normalize-stanza-fld-lib_flags fld-assoc)))
  ;;    ;;      ;; (format #t "lib flags: ~A\n" lf)
  ;;    ;;      lf))
  ;;    ;;   ((libraries) (normalize-stanza-fld-libraries fld-assoc))
  ;;    ;;   ((modules)
  ;;    ;;    ;; (let-values (((direct indirect)
  ;;    ;;    ;; FIXME: deal with private_modules too
  ;;    ;;    (let ((submods-ht (modules->modstbl (cdr fld-assoc)
  ;;    ;;                                        srcfiles)))
  ;;    ;;      ;; (format #t "submods-ht: ~A\n" submods-ht)
  ;;    ;;      `(:submodules ,submods-ht)))

  ;;    ;;   ;; (format #t "direct: ~A, indirect ~A\n"
  ;;    ;;   ;;         direct indirect)
  ;;    ;;   ;; (let* ((raw `((:raw ,fld-assoc)))
  ;;    ;;   ;;        (direct (if (null? direct)
  ;;    ;;   ;;                    raw
  ;;    ;;   ;;                    (cons `(:direct ,@direct) raw)))
  ;;    ;;   ;;        (indirect (if (null? indirect)
  ;;    ;;   ;;                      direct
  ;;    ;;   ;;                      (cons `(:indirect
  ;;    ;;   ;;                              ,@(reverse indirect))
  ;;    ;;   ;;                            direct)))
  ;;    ;;   ;;        ;; if not wrapped, add :resolver
  ;;    ;;   ;;        (resolver (if-let ((wrapped (assoc :wrapped stanza-alist)))
  ;;    ;;   ;;                          (if (equal? wrapped 'false)
  ;;    ;;   ;;                              #f
  ;;    ;;   ;;                              #t)
  ;;    ;;   ;;                          ;; default: ns archive with resolver module
  ;;    ;;   ;;                          (cons `(:resolver ,privname)
  ;;    ;;   ;;                                indirect)))
  ;;    ;;   ;;        (result `(:submodules ,resolver))) ;;FIXME: (:deps ((:submodules ...)(:resolver ...)))
  ;;    ;;   ;;   ;; (format #t "XRESULT ~A: ~A\n" privname result)
  ;;    ;;   ;;   result)
  ;;    ;;   ;; ))

  ;;    ;;   ((foreign_stubs)
  ;;    ;;    (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

  ;;    ;;   ;; ((inline_tests)
  ;;    ;;   ;;  (normalize-inline_tests fld-assoc stanza-alist))

  ;;    ;;   ((instrumentation)
  ;;    ;;    (normalize-instrumentation fld-assoc))

  ;;    ;;   ((ocamlopt_flags) fld-assoc) ;; vendors/numeric/lib
  ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
  ;;    ;;   ;;         pkg-path (car fld-assoc))

  ;;    ;;   ((private_modules)
  ;;    ;;    ;; (private_modules <modules>) specifies a list of modules that will
  ;;    ;;    ;; be marked as private. Private modules are inaccessible from outside
  ;;    ;;    ;; the libraries they are defined in. Note that the private_modules
  ;;    ;;    ;; field is not merged in modules, which represents the total set of
  ;;    ;;    ;; modules in a library. If a directory has more than one stanza and
  ;;    ;;    ;; thus a modules field must be specified, <modules> still need to be
  ;;    ;;    ;; added in modules.
  ;;    ;;    ;; for bazel? these are deps but not submodules?
  ;;    ;;    ;; (format #t "unhandled lib fld: ~A:: ~A\n"
  ;;    ;;    ;;         pkg-path (car fld-assoc))
  ;;    ;;    fld-assoc)

  ;;    ;;   ((modules_without_implementation) fld-assoc)

  ;;    ;;   ((preprocess)
  ;;    ;;    (let ((pp-attrs (normalize-preproc-attrs
  ;;    ;;                     (cdr fld-assoc) stanza-alist)))
  ;;    ;;      ;; e.g. (((:ppx "ppx_tezos_stdlib") (:ppx-args (-foo -bar 42))))
  ;;    ;;      ;; (format #t "PP ATTRS: ~A\n" pp-attrs)
  ;;    ;;      `,@pp-attrs))

  ;;    ;;   ((public_name) '())

  ;;    ;;   ((synopsis) fld-assoc)

  ;;    ;;   ((wrapped) '()) ;; src/lib_protocol_environment

  ;;    ;;   ;; c stuff
  ;;    ;;   ((c_names) fld-assoc) ;; e.g. vendors/numerics/lib
  ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
  ;;    ;;   ;;         pkg-path (car fld-assoc))

  ;;    ;;   ((c_flags) fld-assoc) ;; e.g. vendors/numerics/lib
  ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
  ;;    ;;   ;;         pkg-path (car fld-assoc))

  ;;    ;;   ((c_library_flags) fld-assoc) ;; e.g. src/lib_sapling
  ;;    ;;   ;; (format #t "unhandled lib fld: ~A:: ~A\n"
  ;;    ;;   ;;         pkg-path (car fld-assoc))

       (else
        (begin
          ;; (format #t "unhandled lib fld: ~A\n" fld-assoc)
          fld-assoc))
       ) ;; end case
     ) ;; end lamda
   stanza-alist) ;; end map
  )

         ;; (normalized-stanza (if namespaced
         ;;                        (cons '(:namespaced #t) normalized-stanza)
         ;;                        normalized-stanza))
         ;; ;; (ppx (lib-stanza->ppx stanza-alist))
         ;; )


(define (normalize-stanza-library stanza) ;; pkg-path srcfiles stanza)
  (begin
    (format #t "NORMALIZE-stanza-library ~A\n"
            (assoc-val 'name (cdr stanza)))
    (newline))

  ;; FIXME: if not wrapped => :ns-archive
  ;; else => :library

  (let* ((stanza-alist (cdr stanza))
         ;; (_ (format #t "stanza alist: ~A\n" stanza-alist))

         (privname (assoc-val 'name stanza-alist))
         (namespaced? (if-let ((wrapped (assoc-val 'wrapped stanza-alist)))
                              (if (equal? 'false (car wrapped))
                                  #f
                                  #t)
                              #t))
         (_ (format #t "stanza-alist ~A\n" stanza-alist))

         (normalized-stanza (normalize-lib-fields stanza-alist))
         )
    ;; namespaced?
    normalized-stanza
    )
  ) ;; end normalize-stanza-library

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
;;  ppx fld:
;; e.g. (preprocess (pps ppx1 -foo ppx2 -- -bar 42))

;; The list of libraries will be ppx1 and ppx2 and the command line
;; arguments will be: -foo -bar 42.
;; Libraries listed here should be libraries implementing an OCaml AST
;; rewriter and registering themselves using the
;; ocaml-migrate-parsetree.driver API.
;; Dune will build a single executable by linking all these libraries
;; and their dependencies.
;; Note that it is important that all these libraries are linked with
;; -linkall. Dune automatically uses -linkall when the (kind ...)
;; field is set to ppx_rewriter or ppx_deriver.
;; [Ed: i.e. dune looks for 'kind' in the dunefiles of the libdeps]

;; special case: ppx_inline_test
;; dune doc does not explicitly say how tests are run, it just says
;; to use "(inline_tests)" and exec the test(s) by running 'dune runtests'".
;; (https://dune.readthedocs.io/en/stable/tests.html#inline-tests)

;; but the ppx_inline_test docs say:
;; "Tests are only executed when both these conditions are met:
;;   the executable containing the tests is linked with ppx_inline_test.runner.lib
;;   the executable containing the tests is called with command line arguments:
;;       your.exe inline-test-runner libname [options]
;; (https://github.com/janestreet/ppx_inline_test)

;; so evidently, for each 'library' containing "(inline_tests)" (and
;; ppx_inline_test?), we need to both compile the modules with
;; ppx_inline_test, and then build an executable to run the tests.

;; ppx attribs for ocaml_module:
;; ppx, ppx_args, ppx_data (runtime deps), ppx_print
;; ppx_tags - removed. was specific to ppx_inline_test. use ppx_args,
;; or 'runtime_args' attrib of ppx_executable.

;; special cases:
;; ppx_inline_test: must add ppx_args=["-inline-test-lib", "foo"]
;; dune doc: https://dune.readthedocs.io/en/stable/tests.html#inline-tests

;; normalize-ppx-attrs derives :ppx* flds for the stanza to be emitted
(define normalize-ppx-attrs
  (let ((+documentation+
         "(normalize-ppx-attrs ppx stanza-name) derives :ppx* flds for the stanza to be emitted"))
    (lambda (ppx stanza-name) ;; stanza-alist)
      (format #t "NORMALIZE-PPX-ATTRS ~A: ~A\n" stanza-name ppx)

      ;; NB: :scope defaults to :all, but will be a list of modules
      ;; for 'per_module' ppxes (not yet implemented).

      (let ((ppx-name (string-append "ppx_" stanza-name)))
        ;; e.g. (pps ppx1 -foo ppx2 -- -bar 42)
        (let recur ((ppx ppx)
                    (ppx-libs '())
                    (ppx-args '()))
          (format #t "car: ~A\n" ppx)
          (if (null? ppx)
              (if (null? ppx-args)
                  `((:ppx
                     (((:name ,ppx-name)
                       (:scope :all)
                       (:deps ,(reverse ppx-libs))))))
                  `((:ppx
                     (((:name ,ppx-name)
                       (:scope :all)
                       (:deps ,(reverse ppx-libs))
                       (:args ,ppx-args))))))
              (if (equal? 'ppx_inline_test (car ppx))
                  (begin
                    ;; ppx_inline_test implies fld (inline_tests),
                    ;; plus runtime arg "-inline-test-lib foo"
                    ;; ignore (inline_tests)?
                    (recur (cdr ppx)
                           (cons (car ppx) ppx-libs)
                           (concatenate
                            `("-inline-test-lib" ,stanza-name)
                            ppx-args))
                    )
                  (if (equal? '-- (car ppx))
                      ;; everything after '--' is an arg
                      `((:ppx
                         (((:name ,ppx-name)
                           (:scope :all)
                           (:deps ,(reverse ppx-libs))
                           (:args ,(concatenate
                                    ppx-args
                                    (reverse
                                     (let recur2 ((ppx2 (cdr ppx))
                                                  (args '()))
                                       (if (null? ppx2)
                                           args
                                           (recur2
                                            (cdr ppx2)
                                            (cons
                                             (if (number? (car ppx2))
                                                 (number->string
                                                  (car ppx2))
                                                 (car ppx2))
                                             args)))))))))))
                      (if (string-prefix? "-"
                                          (symbol->string (car ppx)))
                          (recur (cdr ppx)
                                 ppx-libs
                                 (cons (car ppx) ppx-args))
                          (recur (cdr ppx)
                                 (cons (car ppx) ppx-libs)
                                 ppx-args))))))))))

(define (analyze-pps-action ppx-action stanza-name)
  (normalize-ppx-attrs (cdr ppx-action) stanza-name))

(define (ppxmods->name ppx-modules)
  (apply string-append
         (map symbol->string ppx-modules))
  )

;; per_module preprocs: may be actions or ppxes
(define normalize-ppx-attrs-per_module
  ;; (preprocess (per_module
  ;;              (<spec1> <module-list1>)
  ;;              (<spec2> <module-list2>)
  ;;              ...))
  ;; warning: example wraps entries, e.g. ((<spec1> <module-list1>))
  ;; e.g.
  ;; (per_module
  ;;  (((action (run ./pp.sh X=1 %{input-file})) foo bar))
  ;;  (((action (run ./pp.sh X=2 %{input-file})) baz)))
  ;; or:
  ;; (per_module
  ;;  ((action (run ./pp.sh X=1 %{input-file})) foo bar)
  ;;  ((action (run ./pp.sh X=2 %{input-file})) baz))

  ;; returns attrs for ppx preprocs, ignores actions, which are
  ;; handled separately.
  ;; for ppx, an alist of ppx specs, e.g.:
  ;; (:ppx ((:exe "ppx_name1_string") (:args (arg1 arg2...)))
  ;;       ((:exe "ppx_name2_string") (:args (arg1 arg2...))))
  ;; testing:
  ;; (per_module
  ;;  ((pps ppx1 -foo1 ppx2 -- -bar1 42) mod1 mod2)
  ;;  ((pps ppx3 -foo3 ppx4 -- -bar4 43) mod3 mod4))
  (let ((+documentation+ "(normalize-ppx-attrs-per_module ppx stanza-name) derives :ppx* flds for the stanza to be emitted"))
    (lambda (ppx-list stanza-name)
      (format #t "NORMALIZE-PPX-ATTRS-PER_MODULE ~A\n" ppx-list)
      ;; stanza-alist)
      '()
      ;; ppx-list == list of lists
      ;; each item in ppx-list is one list (<action> <modslist>)
      (let ((ppxes
             (let recur ((ppx ppx-list)
                         (ppx-ct (length ppx-list))
                         (ppx-specs '()))
               (format #t "per-mod PPX: ~A\n" (if (null? ppx) '() (car ppx)))
               (if (null? ppx)
                   ppx-specs
                   ;; (car ppx): (<action> <modlist>)

                   (if (null? (cdr (car ppx)))
                       ;; ppx: ((<action> <modslist>))
                       (let* ((ppx-item (caar ppx))
                              (ppx-action (car ppx-item))
                              (ppx-modules (cdr ppx-item)))
                         (recur (cdr ppx) (- ppx-ct 1)
                                (cons `((:action ,ppx-action) (:scope ,ppx-modules))
                                      ppx-specs)))
                       ;; else ppx: (<action> <modslist>)
                       (let* ((ppx-item (car ppx))
                              (ppx-action (car ppx-item))
                              (ppx-modules (cdr ppx-item)))
                         (format #t "per-mod PPX-ACTION: ~A\n" ppx-action)
                         (recur (cdr ppx) (- ppx-ct 1)
                                (cons
                                 (if (equal? (car ppx-action) 'pps)
                                     `(,@(analyze-pps-action
                                          ppx-action
                                          (string-append
                                           (symbol->string stanza-name)
                                           "_"
                                           (ppxmods->name ppx-modules)))
                                       (:scope ,ppx-modules))
                                     `((:action ,ppx-action)
                                       (:scope ,ppx-modules)))
                                 ppx-specs)
                                ))
                       )))))
        `(:ppx ,ppxes)))))

(define normalize-ppx-attrs-staged
  (let ((+documentation+ "(normalize-ppx-attrs-staged ppx stanza-alist) derives :ppx* flds from Dune 'staged_pps' field."))
    (lambda (ppx stanza-alist)
      ;; (format #t "NORMALIZE-PPX-ATTRS-STAGED ~A\n" ppx) ;; stanza-alist)
      '()
      ;;   (let recur ((ppx ppx)
      ;;               (ppx-libs '()) ;; ignore these - not used in attr
      ;;               (ppx-args '()))
      ;;     (format #t "car: ~A\n" ppx)
      ;;     (if (null? ppx)
      ;;         `((:ppx ,ppx-name)
      ;;           (:ppx-args ,ppx-args))
      ;;         (if (equal? '-- (car ppx))
      ;;             ;; everything after '--' is an arg
      ;;             `((:ppx ,ppx-name)
      ;;               (:ppx-args ,(reverse
      ;;                            (let recur2 ((ppx2 (cdr ppx))
      ;;                                         (args ppx-args))
      ;;                              (if (null? ppx2)
      ;;                                  args
      ;;                                  (recur2
      ;;                                   (cdr ppx2) (cons (car ppx2) args)))))))
      ;;             (if (string-prefix? "-"
      ;;                                 (symbol->string (car ppx)))
      ;;                 (recur (cdr ppx) ppx-libs (cons (car ppx) ppx-args))
      ;;                 (recur (cdr ppx) (cons (car ppx) ppx-libs) ppx-args))))))
      )))

;; e.g. lib_stdlb: (preprocess (pps ppx_inline_test))
;; preprocess args: no_preprocessing (default), (action <action>)
;; (pps <ppx-rewriters-and-flags>)
;; (staged_pps <ppx-rewriters-and-flags>)
;; future_syntax

;; for testing:
 ;; (preprocess (action "foo")
 ;;             (pps ppx1 -foo ppx2 -- -bar 42)
 ;;             (per_module
 ;;              ((pps ppx1 -foo1 ppx2 -- -bar1 42) mod1 mod2)
 ;;              ((pps ppx3 -foo3 ppx4 -- -bar4 43) mod3 mod4))
 ;;             ;; (per_module
 ;;             ;;  ((action (run ./pp.sh X=1 %{input-file})) foo2 bar2)
 ;;             ;;  ((action (run ./pp.sh X=2 %{input-file})) baz2))
 ;;             ;; (per_module
 ;;             ;;  (((action (run ./pp.sh X=1 %{input-file})) foo bar))
 ;;             ;;  (((action (run ./pp.sh X=2 %{input-file})) baz)))
 ;;             (staged_pps ppx1 -foo ppx2 -- -bar 42))

;; returns (values flds ppx_stanza)
(define normalize-preproc-attrs
  (let ((+documentation+ "(normalize-preproc-attrs pp-assoc stanza-alist) converts (preprocess ...) subfields 'pps' and 'per_module' to :ppx* flds for use in generating OBazl targets. Does not convert 'action' subfield, since it does not correspond to any OBazl rule attribute ('(action...)' generates a genrule."))
    (lambda (pp-fld stanza-alist)
      ;; (format #t "NORMALIZE-PREPROC-ATTRS: ~A\n" pp-fld)
      (remove '()
              (map (lambda (pp)
                     ;; (format #t "PP: ~A\n" pp)
                     (case (car pp)
                       ((action)
                        ;; "(preprocess (action <action>)) acts as if you had
                        ;; setup a rule for every file of the form:
                        ;; (rule
                        ;;  (target file.pp.ml)
                        ;;  (deps   file.ml)
                        ;;  (action (with-stdout-to %{target}
                        ;;           (chdir %{workspace_root} <action>))))"
                        ;; So an action pp has no role in ocaml_module, we ignore it
                        ;; here; elsewhere we use it to generate a genrule
                        (format #t "IGNORING pp action ~A\n" pp)
                        '())
                       ((pps)
                        ;; normalize-ppx-attrs result:
                        ;;   ((:ppx ....) (:ppx-args ...))
                        (let ((nm (cadr (assoc 'name stanza-alist))))
                          `,@(normalize-ppx-attrs (cdr pp)
                                                  (symbol->string nm))))

                       ((per_module)
                        (let ((nm (cadr (assoc 'name stanza-alist))))
                          (normalize-ppx-attrs-per_module (cdr pp) nm)))

                       ((staged_pps)
                        (normalize-ppx-attrs-staged (cdr pp) stanza-alist))
                       (else
                        (error 'bad-arg
                               (format #f "unexpected 'preprocess' subfield: ~A\n"
                                       pp)))
                       ))
                   pp-fld)))))

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

;; (display "loaded dune/dune_stanza_library.scm") (newline)
