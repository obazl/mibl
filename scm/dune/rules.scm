;; (display "dune/dune_stanza_rule.scm loading ...") (newline)

(require 'expanders.scm)

(define (normalize-action-rule ws pkg rule-alist targets deps)
  (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
      (begin
        (format #t "~A\n" (ublue "normalize-action-rule"))
        (format #t "~A: ~A\n" (blue "rule-alist") rule-alist)
        (format #t "~A: ~A\n" (blue "deps") deps)
        (format #t "~A: ~A\n" (blue "targets") targets)
        (format #t "~A: ~A\n" (blue "ws") ws)))
  (let* ((nzaction (actions:normalize ws pkg rule-alist targets deps))
         (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                (format #t "~A: ~A\n"
                        (green "normalized action") nzaction)))
         ;; (_ (error 'X "STOP normalize-action"))
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
         ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "rule-tag: ~A\n" rule-tag)))
         ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "~A: ~A\n" (green "TARGETS") targets)))
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
      (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
          (begin
            (format #t "~A\n" (bgblue "dune-rule->mibl"))
            (format #t "~A: ~A\n" (blue "ws") ws)
            (format #t "~A: ~A\n" (blue "pkg") pkg)
            (format #t "~A: ~A\n" (blue "stanza") stanza)))
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
             (rule-alist (cdr stanza))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "rule-alist: ~A\n" rule-alist)))
             ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "target: ~A\n" (assoc 'target rule-alist))))
             ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "Targets: ~A\n" (assoc-val 'targets rule-alist))))

             ;; Step 1: rule deps don't depend on targets, so do first
             (deps (expand-rule-deps ws pkg rule-alist))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (red "expanded rule deps") deps)))

             ;; Step 2: 'target' and 'targets' fields list files generated
             ;; by the action. Add them to the pkg :modules and :files
             ;; assocs.
             (targets (if-let ((tgt (assoc-val 'target rule-alist)))
                              tgt
                              (if-let ((tgts
                                        (assoc-val 'targets rule-alist)))
                                      tgts
                                      '())))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (green "targets") targets)))

             ;; 'target' may be omitted with with-stdout-to
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (green "rule-alist") rule-alist)))

             (stdout-tgt (if-let ((stdout (assoc-in '(action with-stdout-to) rule-alist)))
                                 (let ((stdout (cadr stdout)))
                                   (if (string-prefix? "%{" (format #f "~A" stdout))
                                       #f
                                       stdout))
                                 #f))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "stdout-tgt") stdout-tgt)))

             ;; if with-stdout-to is listed in targets, remove dups
             (targets (remove-duplicates
                       (if (truthy? stdout-tgt)
                           (cons stdout-tgt targets)
                           targets)))

             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (green "targets") targets)))

             ;; add targets to pkg fields
             (pkg (if (null? targets)
                      pkg
                      (begin
                        ;; (set! pkg
                        (update-pkg-files! pkg targets)
                              ;; )
                        pkg)))
             ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "~A: ~A\n" (yellow "updated pkg") pkg)))

             ;; normalize
             (targets (cons :outputs (expand-targets ws pkg targets deps)))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (red "expanded rule targets") targets)))
             )

        (if deps
            (begin
              (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (green "iterating deps") deps))
              (for-each (lambda (dep)
                          ;; dep forms:
                          ;; (:foo (:pkg a/b/c)(:tgt "foo.sh"))
                          ;; (::opam-pkg foo-bar-baz)
                          ;; (tezos-protocol-demo-noops ::opam-pkg)
                          (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                              (format #t "~A: ~A~%" (ucyan "dep") dep))
                          (case (cdr dep)
                            ((::opam-pkg) (cdr dep))
                            (else
                             (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                                 (format #t "~A: ~A~%" (red "filegroup dep?") dep))
                             (let* ((lbl-tag (car dep))
                                    (lbl (cdr dep))
                                    (pkg (assoc-val :pkg lbl))
                                    (tgt-tag (caadr lbl))
                                    (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag)))
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
                                       (update-filegroups-table!
                                        ws pkg-path pkg (string->keyword fg-tag) tgt)))))))
                        (cdr deps))))

        (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
            (format #t "~A: ~A~%" (ugreen "DISPATCHING  on action") rule-alist))

        ;; rule type is determined by 'action' field, which can be:
        ;; bash, copy, run, etc.
        ;; every rule stanza has an action field
        ;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions
        ;; sadly, the documentation does not match the implementation,
        ;; which allows (rule (<action> ...)) w/o and explicit 'action fld.
        (let* ((mibl-rule
                (cond
                 ((assoc 'action rule-alist)
                  (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                      (begin
                        (format #t "handling action rule\n" )
                        (format #t "targets: ~A~%" targets)
                        (format #t "deps: ~A~%" deps)))
                  (normalize-action-rule ws pkg rule-alist
                                         (if (null? targets) (list :targets) targets)
                                         (if deps deps (list :deps))))

                 ;; rules whose action is not wrapped by (run ...)
                 ;; TODO: use dispatch table dune-action-cmds-no-dsl
                 ;; if stanza contains a field in dune-dsl-cmds then dispatch
                 (else
                  ;;FIXME: this finds just one action, can list have more than one?
                  ;; no - it would use progn?
                  ;; so we don't need find-if, we can just check car?
                  (let ((action (find-if (lambda (fld)
                                           ;; (format #t "~A: ~A~%" (red "RULE FLD") fld)
                                           (member (car fld) dune-dsl-cmds))
                                         rule-alist)))
                    (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                        (format #t "~A: ~A~%" (green "FOUND dsl cmd") action))
                    (if action
                        (let ((rule-alist (map (lambda (fld)
                                                 (if (equal? (car fld) (car action))
                                                     (list 'action fld)
                                                     fld))
                                               rule-alist)))
                          (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                              (format #t "~A: ~A~%" (green "updated alist") rule-alist))
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
               (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                   (format #t "~A: ~A~%" (green "mibl-rule") mibl-rule))
         ;; (_ (error 'X "STOP prune"))

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
