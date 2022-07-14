;; caveat: pps ppx_inline_test implies (inline_tests), but not the
;; other way around.
(define (lib-stanza->ppx stanza-alist)
  (if-let ((preproc (assoc 'preprocess stanza-alist)))
          (if-let ((ppx (assoc 'pps (cdr preproc))))
                  (begin
                    ;; (format #t "lib-stanza->ppx ~A\n" stanza-alist)
                    (let* ((stanza-name (symbol->string
                                         (cadr (assoc 'name stanza-alist))))
                           (ppx-alist (-pps->mibl (cdr ppx)
                                                           stanza-name))

                           (ppx-name (string-append "ppx_" stanza-name))
                           ;; (args (ppx-fld->args ppx))
                           (inline-tests? (if (assoc 'inline_tests stanza-alist)
                                              #t #f)))
                      ;; (format #t "X: ~A\n" ppx-alist)
                      `(:ppx (;; (:name ,ppx-name)
                              (:args (-bar))
                              (:manifest ((:constant (ppx_inline_test))))))))
                  #f)
          #f))

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

;; -pps->mibl derives :ppx* flds for the stanza to be emitted
(define -pps->mibl
  (let ((+documentation+
         "(-pps->mibl stanza-name ppxes) derives :ppx* flds for the stanza to be emitted"))
    (lambda (stanza-name ppxes) ;; stanza-alist)
      (format #t "~A ~A: ~A\n" (blue "-pps->mibl") stanza-name ppxes)

      ;; NB: :scope defaults to :all, but will be a list of modules
      ;; for 'per_module' ppxes (not yet implemented).

      (let ((ppx-name 'ppx.exe)) ;; (string-append "ppx_" stanza-name)))
        ;; e.g. (pps ppx1 -foo ppx2 -- -bar 42)
        (let recur ((ppx ppxes)
                    (ppx-libs '())
                    (ppx-args '()))
          (format #t "car: ~A\n" ppx)
          (if (null? ppx)
              (if (null? ppx-args)
                  (cons :ppx
                        (list ;; `(:name ,ppx-name)
                              '(:scope :all)
                              `(:manifest ,@(reverse ppx-libs))))
                  (cons :ppx
                        (list ;; `(:name ,ppx-name)
                              (:scope :all)
                              `(:manifest ,(reverse ppx-libs))
                              `(:args ,ppx-args))))
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
                      ;; TODO: group options as pairs (-foo x)
                      (cons :ppx
                            (list ;; `(:name ,ppx-name)
                                  '(:scope :all)
                                  `(:manifest ,@(reverse ppx-libs))
                                  `(:args ,@(cdr ppx))
                                    ;;,(concatenate
                                          ;;  ppx-args
                                            ;; ,(reverse
                                            ;;  (let recur2 ((ppx2 (cdr ppx))
                                            ;;               (args '()))
                                            ;;    (if (null? ppx2)
                                            ;;        args
                                            ;;        (recur2
                                            ;;         (cdr ppx2)
                                            ;;         (cons
                                            ;;          (if (number? (car ppx2))
                                            ;;              (number->string
                                            ;;               (car ppx2))
                                            ;;              (car ppx2))
                                            ;;          args)))))
                                            ))
                      (if (string-prefix? "-"
                                          (symbol->string (car ppx)))
                          (recur (cdr ppx)
                                 ppx-libs
                                 (cons (car ppx) ppx-args))
                          (recur (cdr ppx)
                                 (cons (car ppx) ppx-libs)
                                 ppx-args))))))))))

(define (analyze-pps-action ppx-action stanza-name)
  (-pps->mibl stanza-name (cdr ppx-action)))

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
(define preprocess-fld->mibl
  (let ((+documentation+ "(preprocess-fld->mibl pp-assoc stanza-alist) converts (preprocess ...) subfields 'pps' and 'per_module' to :ppx* flds for use in generating OBazl targets. Does not convert 'action' subfield, since it does not correspond to any OBazl rule attribute ('(action...)' generates a genrule."))

    (lambda (pp-assoc stanza-alist)
      (format #t "~A: ~A\n" (blue "preprocess-fld->mibl") pp-assoc)
      (let ((ppx-data (assoc-val 'preprocessor_deps stanza-alist))
            (ppx (map (lambda (pp)
                        (format #t "PP: ~A\n" pp)
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
                           ;; -pps->mibl result:
                           ;;   ((:ppx ....) (:ppx-args ...))
                           (let ((nm (cadr (assoc 'name stanza-alist))))
                             `,@(-pps->mibl (symbol->string nm) (cdr pp))))

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
                      (cdr pp-assoc))))
        (if ppx-data
            (append ppx (list (cons :ppx-data ppx-data)))
            ppx)))))

