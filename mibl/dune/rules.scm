;; (display "dune/dune_stanza_rule.scm loading ...") (newline)

(load "dune/expanders.scm")

(define (normalize-action-rule ws pkg rule-alist targets deps)
  (format #t "~A: ~A\n" (ublue "normalize-action-rule") rule-alist)
  (format #t "deps: ~A\n" deps)
  (format #t "targets: ~A\n" targets)
  (format #t "ws: ~A\n" ws)
  (let* ((nzaction (normalize-action ws pkg rule-alist targets deps))
         (_ (format #t "~A: ~A\n"
                    (blue "normalized action") nzaction))
         (package (if-let ((p (assoc-val 'package rule-alist))) (car p)))
         (mode (if-let ((m (assoc-val 'mode rule-alist))) (car m)))
         (fallback (if-let ((fb (assoc-val 'fallback rule-alist)))
                           (car fb)))
         (locks (assoc-val 'locks rule-alist))
         (alias (if-let ((a (assoc-val 'alias rule-alist))) (car a)))
         (package (if-let ((p (assoc-val 'package rule-alist))) (car p)))

         (enabled-if (if-let ((p (assoc-val 'enabled_if rule-alist)))
                             (car p)))

         ;; (rule-tag (-action->rule-tag nzaction))
         ;; (_ (format #t "rule-tag: ~A\n" rule-tag))
         ;; (_ (format #t "~A: ~A\n" (green "TARGETS") targets))
         (r-alist (if (null? targets)
                      (list (if (assoc :progn nzaction)
                                (cons :actions (cdar nzaction))
                                (cons :actions nzaction)))
                      (list targets ;;(list :outputs `,@targets)
                            ;; (cons ':deps deps)
                            (if (assoc :progn nzaction)
                                (cons :actions (cdar nzaction))
                                (cons :actions nzaction)))))
         (r-alist (if-let ((ctx (assoc :ctx nzaction)))
                          (cons ctx r-alist)
                          r-alist))
         (r-alist (if deps ;; (null? deps) ???
                      (cons deps r-alist)
                      ;; (cons `(:Deps ,@deps) r-alist)
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
    (list (cons ':rule r-alist))))

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

(define dune-rule->mibl
  (let ((+documentation+ "INTERNAL. Updates pkg arg, returns normalized stanza. stanza: raw dune stanza (input); nstanza: miblized (output)"))
    (lambda (ws pkg stanza)
      (format #t "~A: ~A\n" (blue "dune-rule->mibl") stanza)
      (format #t "~A: ~A\n" (green "ws") ws)
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
             (rule-alist (cdr stanza))
             (_ (format #t "rule-alist: ~A\n" rule-alist))
             ;; (_ (format #t "target: ~A\n" (assoc 'target rule-alist)))
             ;; (_ (format #t "Targets: ~A\n" (assoc-val 'targets rule-alist)))

             ;; Step 1: rule deps don't depend on targets, so do first
             (deps (expand-rule-deps ws pkg rule-alist))
             (_ (format #t "~A: ~A\n" (red "expanded rule deps") deps))

             ;; Step 2: 'target' and 'targets' fields list files generated
             ;; by the action. Add them to the pkg :modules and :files
             ;; assocs.
             (targets (if-let ((tgt (assoc-val 'target rule-alist)))
                              tgt
                              (if-let ((tgts
                                        (assoc-val 'targets rule-alist)))
                                      tgts
                                      '())))
             (_ (format #t "~A: ~A~%" (red "Targets") targets))

             ;; add targets to pkg fields
             (pkg (if (null? targets)
                      pkg
                      (begin
                        ;; (set! pkg
                        (update-pkg-files! pkg targets)
                              ;; )
                        pkg)))
             ;; (_ (format #t "~A: ~A\n" (yellow "updated pkg") pkg))

             ;; normalize
             (targets (cons :outputs (expand-targets ws pkg targets deps)))
             (_ (format #t "~A: ~A\n" (red "expanded rule targets") targets))

             )

        (format #t "~A: ~A~%" (yellow "iterating deps") deps)
        (if deps
            (for-each (lambda (dep)
                        ;; dep forms:
                        ;; (:foo (:pkg a/b/c)(:tgt "foo.sh"))
                        ;; (::pkg foo-bar-baz)
                        ;; (tezos-protocol-demo-noops ::pkg)
                        (format #t "~A: ~A~%" (ucyan "dep") dep)
                        (case (cadr dep)
                        ;; (if (eq? ::pkg (cadr dep))
                          ((::pkg) (cdr dep))
                          (else
                              (format #t "~A: ~A~%" (red "filegroup dep?") dep)
                              (let* ((lbl-tag (car dep))
                                     (lbl (cdr dep))
                                     (pkg (assoc-val :pkg lbl))
                                     (tgt-tag (caadr lbl))
                                     (_ (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag))
                                     (tgt (case tgt-tag
                                            ((:tgt)
                                             (assoc-val :tgt lbl))
                                            ((:tgts)
                                             (assoc-val :tgts lbl))
                                            ((:glob)
                                             (assoc-val :glob lbl))
                                            ((:fg)
                                             (assoc-val :fg lbl))
                                            (else
                                             (error 'fixme "label pair lacks :tgt and :tgts"))))
                                     (fg-tag (if (eq? tgt-tag :fg)
                                                 (format #f "*~A*" tgt)
                                                 tgt)))
                                ;; (format #t "~A: ~A~%" (red "pkg") pkg)
                                ;; (format #t "~A: ~A~%" (red "lbl-tag") lbl-tag)
                                ;; (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag)
                                ;; (format #t "~A: ~A~%" (red "tgt") tgt)
                                ;; (format #t "~A: ~A~%" (red "pkg-path") pkg-path)
                                (if (not (equal? (format #f "~A" pkg) pkg-path))
                                    (if (eq? tgt-tag :fg)
                                        (update-filegroups-table! ws pkg (string->keyword fg-tag) tgt)))))))
                      (cdr deps)))

        (format #t "~%~A: ~A~%~%" (red "DISPATCHING  on action") rule-alist)

        ;; rule type is determined by 'action' field, which can be:
        ;; bash, copy, run, etc.
        ;; every rule stanza has an action field
        ;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions
        ;; sadly, the documentation does not match the implementation,
        ;; which allows (rule (<action> ...)) w/o and explicit 'action fld.
        (let* ((mibl-rule
                (cond
                 ((assoc 'action rule-alist)
                  (format #t "handling action rule\n" )
                  (format #t "targets: ~A~%" targets)
                  (format #t "deps: ~A~%" deps)
                  (normalize-action-rule ws pkg rule-alist
                                         (if (null? targets) (list :targets) targets)
                                         (if deps deps (list :deps))))

                 ;; rules whose action is not wrapped by (run ...)
                 ;; TODO: use dispatch table dune-action-cmds-no-dsl
                 ;; if stanza contains a field in dune-dsl-cmds then dispatch
                 (else
                  (let ((action (find-if (lambda (fld)
                                           ;; (format #t "~A: ~A~%" (red "RULE FLD") fld)
                                           (member (car fld) dune-dsl-cmds))
                                         rule-alist)))
                    (format #t "~A: ~A~%" (red "FOUND dsl cmd") action)
                    (if action
                        (let ((rule-alist (map (lambda (fld)
                                                 (if (equal? (car fld) (car action))
                                                     (list 'action fld)
                                                     fld))
                                               rule-alist)))
                          (format #t "~A: ~A~%" (red "updated alist") rule-alist)
                          (normalize-action-rule ws pkg rule-alist targets
                                                 (if deps deps (list :deps))))
                        (error 'no-action (format #f "rule without action: ~A" rule-alist)))))))
               (mibl-rule (prune-mibl-rule mibl-rule)))
                ;; ((assoc 'copy rule-alist)
                ;;  (format #t "handling copy rule\n" )
                ;;  (normalize-copy-rule ws pkg rule-alist targets deps))

                ;; ((assoc 'with-stdout-to rule-alist)
                ;;  (format #t "handling write-file rule\n" )
                ;;  (normalize-action-with-outputs-to-dsl
                ;;   ws pkg rule-alist targets deps))

                ;; ((assoc 'write-file rule-alist)
                ;;  (format #t "handling write-file rule\n" )
                ;;  (normalize-action-write-file
                ;;   ws pkg rule-alist targets deps))

                ;; (else
                ;;  (error 'unhandled-rule
                ;;         (format #f "unhandled rule: ~A" rule-alist))))))
          (format #t "~A: ~A~%" (green "mibl-rule") mibl-rule)
          ;; (update-exports-table-with-targets! ws
          ;;                                     :FIXME ;; tag
          ;;                                     (assoc-in '(:rule :outputs) mibl-rule) ;; name
          ;;                                     pkg-path (assoc-in '(:rule :outputs) mibl-rule))
          mibl-rule)))))

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
