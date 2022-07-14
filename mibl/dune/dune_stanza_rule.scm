;; (display "dune/dune_stanza_rule.scm loading ...") (newline)

(define (rule->ocaml-outfile-names stanza)
  ;; (format #t "rule->ocaml-outfile-names: ~A\n" stanza)
  (let ((result
         (let* ((rule-alist (cdr stanza))
                (targets (if-let ((targets (assoc 'targets rule-alist)))
                                 (cadr targets) #f)))
           ;; (format #t "targets: ~A\n" targets)
           (if targets
               (let ((target (if (symbol? targets)
                                 (symbol->string targets)
                                 targets)))
                 ;; (format #t "targets ml?: ~A\n" targets)

                 (if (or (string-suffix? ".mli" target)
                         (string-suffix? ".ml" target))
                     target
                     '()))
               '()))))
    ;; (format #t "ocaml outfiles: ~A\n" result)
    (if (list? result)
        result
        (list result))))

(define (normalize-copy-rule pkg-path action stanza srcfiles)
  (format #t "NORMALIZE-COPY-RULE: ~A: ~A\n" pkg-path action)
  ;; (format #t "  STANZA: ~A\n" stanza)

  (let* ((src (cadr action))
         (dst (caddr action)))

    `(:copy
      (:src ,src)
      (:dst ,dst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://dune.readthedocs.io/en/stable/dune-files.html#rule
;; (rule
;;  (target[s] <filenames>)
;;  (action  <action>)
;;  <optional-fields>)

;; <optional-fields> are:
;; (deps <deps-conf list>) to specify the dependencies of the rule.
;; (mode <mode>)
;; (fallback) is deprecated and is the same as (mode fallback)
;; (locks (<lock-names>))

;; (alias <alias-name>) specify the alias this rule belongs to.
;; Building this alias means building the targets of this rule.

;; (package <package>) specify the package this rule belongs to. This
;; rule will be unavailable when installing other packages in release
;; mode.

;; (enabled_if <blang expression>) specifies the boolean condition
;; that must be true for the rule to be considered.

;; NB: devs may use 'targets' for a single output file.

;; rules always have an action? exactly one?

;; The OBazl rule we emit depends on the fields:
;;     target(s) field implies Bazel genrule
;;     action w/o target implies ocaml_test?

;; deps:
;;  (deps data.ml services.ml main.mli main.ml (:src_dir TEZOS_PROTOCOL))
;;  (deps (alias runtest_sandbox))
;; dune-specific, from tezos/src/proto_genesis/lib_protocol:
;;  (deps .tezos_protocol_genesis.objs/native/tezos_protocol_genesis.cmx)


;; run action: multiple ways to express tool:
;;   (deps    (:exe bip39_tests.exe)) (action  (run %{exe})))
;;   (action  (run %{exe:pbkdf_tests.exe})))

;; this one is just for forcing a build of the exes?
;; if so we can ignore it
;; (rule
;;  (alias buildtest)
;;  (deps bench_tool.exe bench_simple.exe)
;;  (action progn))

;; this one runs an executable dep, so we emit ocaml_test
;; (rule
;;  (alias runbench_alpha_simple)
;;  (deps bench_simple.exe)
;;  (action (chdir %{workspace_root} (run %{exe:bench_simple.exe}))))

;; write a file:
;; (rule
;;  (targets environment.ml)
;;  (action
;;   (write-file %{targets}
;;               "module Name = struct let name = \"009-PsFLoren\" end
;; include Tezos_protocol_environment.MakeV2(Name)()
;; module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
;; ")))

;; e.g.
;; (:action (:tool cat)...) => :cat
;; chdir:  (:action (:tool ...sth...)...) => :rule
(define (-action->rule-tag action)
  (if-let ((tag (assoc-in '(:cmd :tool) action)))
          (case (cadr tag)
            ((cat) :action-cat)
            ((cmp) :action-cmp)
            ((copy) :action-copy)
            ((diff) :action-diff)
            ((echo) :action-echo)
            ((progn) :action-progn)
            ((system) :action-system)
            ((with-stdout-to) :action-with-stdout-to)
            ((write-file) :action-write-file)
            (else ':RULE))
          (if-let ((tag (assoc-in '(:run :tool) action)))
                  (begin :action-run)
                  (error 'missing-tool "rule stanza missing tool"))))

;; Normalization: omit the rules we don't need. The remaining rules
;; require package- (or global-) level analysis, so leave that to the
;; emitter.

;; actions:
;;  (with-stdout-to %{targets}
       ;; (chdir %{workspace_root}
       ;;   (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

;; run actions:
;; (run %{exe})
;; (run %{exe:test.exe})
;; (run %{exe:test_tezos.exe})
;; (run %{exe:../config/discover.exe} -ocamlc %{ocamlc})
;; (run %{bin:tezos-protocol-compiler} ./)

;; (rule (target[s] <filenames>) (action  <action>) <optional-fields>)
;; q: can a rule stanza have multiple actions?

;; (define (normalize-stanza-rule pkg-path srcfiles stanza)

(define dune-rule->mibl
  (let ((+documentation+ "INTERNAL. Updates pkg arg, returns normalized stanza. stanza: raw dune stanza (input); nstanza: normalized (output)"))
    (lambda (pkg stanza)
      (format #t "~A: ~A\n" (blue "dune-rule->mibl") stanza)
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (let* ((pkg-path (assoc-val :pkg-path pkg))
             (rule-alist (cdr stanza))
             ;; (_ (format #t "rule-alist: ~A\n" rule-alist))
             ;; (_ (format #t "target: ~A\n" (assoc 'target rule-alist)))
             ;; (_ (format #t "Targets: ~A\n" (assoc-val 'targets rule-alist)))

             ;; Step 1: rule deps don't depend on targets, so do first
             (deps (expand-rule-deps pkg rule-alist))
             ;; (_ (format #t "EXPANDED rule deps: ~A\n" deps))

             ;; Step 2: 'target' and 'targets' fields list files generated
             ;; by the action. Add them to the pkg :modules and :files
             ;; assocs.
             (targets (if-let ((tgt (assoc-val 'target rule-alist)))
                              tgt
                              (if-let ((tgts
                                        (assoc-val 'targets rule-alist)))
                                      tgts
                                      '())))
             ;; add to pkg fields
             (pkg (if (null? targets)
                      pkg
                      (set! pkg (update-pkg-with-targets! pkg targets))))
             ;; (_ (format #t "rule: updated pkg: ~A\n" pkg))

             ;; normalize
             (targets (expand-targets pkg targets deps))
             ;; (_ (format #t "EXPANDED rule targets: ~A\n" targets))

             (mode (if-let ((m (assoc-val 'mode rule-alist))) (car m)))
             (fallback (if-let ((fb (assoc-val 'fallback rule-alist)))
                                 (car fb)))
             (locks (assoc-val 'locks rule-alist))
             (alias (if-let ((a (assoc-val 'alias rule-alist))) (car a)))
             (package (if-let ((p (assoc-val 'package rule-alist))) (car p)))

             (enabled-if (if-let ((p (assoc-val 'enabled_if rule-alist)))
                                 (car p)))
             )
        ;; rule type is determined by 'action' field, which can be:
        ;; bash, copy, run, etc.
        ;; every rule stanza has an action field
        ;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions
        (if (assoc 'action rule-alist)
            ;; the main dispatcher is 'normalize-action'
            (let* ((nzaction (normalize-action pkg rule-alist targets deps))
                   (_ (format #t "~A: ~A\n"
                              (red "normalized action") nzaction))
                   ;; (rule-tag (-action->rule-tag nzaction))
                   (r-alist (list (cons ':targets targets)
                                  ;; (cons ':deps deps)
                                  (cons ':action nzaction)))
                   (r-alist (if deps (cons `(:deps ,@deps) r-alist)
                                r-alist))
                   (r-alist (if package (cons (list ':package package)
                                                 r-alist)
                                r-alist))
                   (r-alist (if mode (cons (list ':mode mode) r-alist)
                                r-alist))
                   (r-alist (if locks (cons (list ':locks locks) r-alist)
                                r-alist))
                   (r-alist (if fallback (cons (list ':fallback fallback)
                                                 r-alist)
                                r-alist))
                   (r-alist (if enabled-if (cons (list ':enabled-if
                                                         enabled-if)
                                                 r-alist)
                                r-alist))
                   (r-alist (if alias (cons (list ':alias alias) r-alist)
                                r-alist)))
              ;; (list (cons rule-tag r-alist)))
-              (list (cons ':rule r-alist)))
            (format #t "UNHANDLED RULE: ~A\n" rule-alist))
        ))))

    ;; (let ((result (map (lambda (fld-assoc)
    ;;                      ;; (display (format #f "fld: ~A" fld-assoc)) (newline)
    ;;                      (let ((fld (if (pair? fld-assoc) fld-assoc (list fld-assoc))))


    ;;                        (case (car fld-assoc)
    ;;                          ((action)
    ;;                           (normalize-action action target targets deps))

    ;;                          (else (cons 'rule fld-assoc)))))

    ;;                    (cdr stanza))))
    ;;   ;; (cons 'rule
    ;;         result
    ;;         ;; )
    ;;   )
;; ))

  ;; (let* ((s
  ;;         (map (lambda (fld-assoc)
  ;;                (let ((fld (if (pair? fld-assoc)
  ;;                               fld-assoc
  ;;                               (list fld-assoc))))
  ;;                  (case (car fld-assoc)
  ;;                    ((target)
  ;;                     )
  ;;                    ((targets)
  ;;                     )
  ;;                    ((action)
  ;;                     ;; (action progn), (action (progn)) ???
  ;;                     ;; used to force build of deps?
  ;;                     )
  ;;                    ((alias) ;; used (always?) to force tgt build and
  ;;                     ;; thus build of deps. no other way to refer to
  ;;                     ;; the rule, no 'name' attrib.

  ;;                     ;;ignore - no need
  ;;                             ;; in bazel but - should we generate a
  ;;                             ;; Bazel alias?
  ;;                     '())
  ;;                    ((deps) ;; examples:
  ;;                     ;; (deps (:exe bip39_tests.exe))

  ;;                     ;; (deps bench_tool.exe bench_simple.exe) ;;lib_shell/bench

  ;;                     ;; (deps (universe) (:script get-git-info.mlt))

  ;;                     ;; (deps (glob_files *.ml{,i}))
  ;;                     ;; (deps (glob_files contracts/*))
  ;;                     ;; (deps (package tezos-protocol-demo-noops))

  ;;                     ;; (deps (alias runtest_requester))
  ;;                     ;; (deps (alias runtest_proto_alpha) (alias runtest_saturation_fuzzing) (alias runtest_test_tez_repr))

  ;;                     ;; pass it on, emitter must match the :exe val
  ;;                     ;; against targets in the pkg

  ;;                     )
  ;;                    ;; we'll ignore these, but pass them thru anyway
  ;;                    ((mode)    `(:mode    ,(cadr fld-assoc)))
  ;;                    ((locks)   `(:locks   ,(cadr fld-assoc)))
  ;;                    ((package) `(:package ,(cadr fld-assoc)))
  ;;                    (else
  ;;                     ))))
  ;;              (cdr stanza))))
  ;;   s)
  ;; )

;; (define (normalize-stanza-rule stanza)
;;   ;; (display (format #f "dir: ~A" pfx)) (newline)
;;   ;; (display (format #f "normalize-stanza-rule: ~A" stanza)) (newline)
;;   ;; (rule
;;   ;;  (target[s] <filenames>)
;;   ;;  (action  <action>)
;;   ;;  <optional-fields>)
;;   ;; <action> produces target <filename(s)>

;;   ;; <optional-fields>:
;;   ;; (deps <deps-conf list>)
;;   ;; (mode <mode>): standard, fallback, various promote-*
;;   ;; (fallback) is deprecated and is the same as (mode fallback)
;;   ;; (locks (<lock-names>)) run action while holding locks
;;   ;; (alias <alias-name>) Building alias = building targets of this rule.
;;   ;; (package <package>) rule will be unavailable when installing other packages in release mode.
;;   ;; (enabled_if <blang expression>) must be true for the rule to be considered.

;;   ;; short syntax:
;;   ;; (rule
;;   ;;  (target b)
;;   ;;  (deps   a)
;;   ;;  (action (copy %{deps} %{target})))
;;   ;;  => (rule (copy a b))

;;   ;; IMPORTANT: obazl ignores 'promote' stuff. we do not write to the
;;   ;; src tree, ever. However, dune rules may write to the source tree.
;;   ;; example: tezos/lib_version has a rule that runs a git cmd to get
;;   ;; version metadata and writes it to a file in the source tree.
;;   ;; probably pretty common practice. The Bazel way: using stamping,
;;   ;; and/or use a separate tool to maintain source files like this.

;;   ;; e.g. tezos/src/lib_requester/test:
;;   ;; (rule (alias runtest_requester) (action (run %{exe:test_requester.exe})))

;;   ;; tezos/src/lib_version:
;;   ;; (rule (targets generated_git_info.ml) (deps (universe) (:script get-git-info.mlt)) (action (with-stdout-to %{targets} (run %{ocaml} unix.cma %{script}))))

;;   ;; TODO: handle %{...} syntax
;;   (let ((result (map (lambda (fld-assoc)
;;                        ;; (display (format #f "fld: ~A" fld-assoc)) (newline)
;;                        (let ((fld (if (pair? fld-assoc) fld-assoc (list fld-assoc))))


;;                          (case (car fld-assoc)
;;                            ((name) (list :name
;;                                          (cons (cadr fld-assoc)
;;                                                (normalize-module-name
;;                                                 (cadr fld-assoc)))))
;;                            (else fld-assoc))))
;;                      (cdr stanza))))
;;     (cons 'rule (list result))))

;; (format #t "loaded dune/dune_stanza_rule.scm\n")
