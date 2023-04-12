;; (display "dune/dune_stanza_rule.scm loading ...") (newline)

(require 'expanders.scm)

;;FIXME: better name
;;FIXME: this mess constructs the entire rule alist
(define (-normalize-rule-action ws pkg tools rule-alist targets deps)
  (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
      (begin
        (format #t "~A\n" (ublue "-normalize-rule-action"))
        (format #t "~A: ~A\n" (blue "rule-alist") rule-alist)
        (format #t "~A: ~A\n" (blue "tools") tools)
        (format #t "~A: ~A\n" (blue "deps") deps)
        (format #t "~A: ~A\n" (blue "targets") targets)
        (format #t "~A: ~A\n" (blue "ws") ws)))
  (let* ((mibl-action (rule-action:normalize ws pkg rule-alist tools targets deps))
         (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                (format #t "~A: ~A\n"
                        (green "normalized action") mibl-action)))
         ;; normalized action: (:cmd (:tool...) (:args...)) plus stdio (:stdout...) etc.

         (new-action (dsl:action->mibl (assoc-val 'action rule-alist)))
         (mibl-trace-let "new-action" new-action)

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

         ;; (rule-tag (-action->rule-tag mibl-action))
         ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "rule-tag: ~A\n" rule-tag)))
         ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "~A: ~A\n" (green "TARGETS") targets)))
         (mibl-trace-let "mibl-action" mibl-action)
         (r-alist (if (null? targets)
                      (list (if (assoc :progn mibl-action)
                                ;; (cons :actions (cdar mibl-action))
                                (cdar mibl-action)
                                `,@mibl-action))
                                ;; (cons :actions mibl-action)))
                      (list targets ;;(list ::outputs `,@targets)
                            ;; (cons ':deps deps)
                            (if (assoc :progn mibl-action)
                                (cdar mibl-action)
                                ;; (cons :actions (cdar mibl-action))
                                `,@mibl-action))))
                                ;;(cons :actions mibl-action)))))
         (mibl-trace-let "r-alist 1" r-alist)
         ;; (_  (error 'x "x"))
         (r-alist (if-let ((ctx (assoc :ctx mibl-action)))
                          (cons ctx r-alist)
                          r-alist))
         (mibl-trace-let "r-alist 2" r-alist)
         (r-alist (cons tools r-alist))
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
                      r-alist))
         (r-alist (append r-alist `((:test-cmd ,@new-action))))
         (mibl-trace-let "r-alist end" r-alist)
         )
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

(define (-handle-rule-targets ws rule-alist deps pkg)
  (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "-handle-rule-targets") rule-alist))
  ;; 'target' and 'targets' fields list files generated
  ;; by the action. Add them to the pkg :modules and :files
  ;; alists.
  ;; We always use the plural 'targets'
  (let* ((targets (if-let ((tgt (assoc-val 'target rule-alist)))
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
         (targets (cons ::outputs (expand-targets ws pkg targets deps)))
         (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                (format #t "~A: ~A\n" (red "expanded rule targets") targets)))
         )
    targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dune rule stanza grammar:
;; (rule
;;  (action <action>)
;;  ;; <optional-fields>
;;  (target ...) or (targets ...)  can be omitted if inferrable from action
;;  (deps ...)
;;  (package <pkg>)
;;  (mode ...)
;;  (fallback) - deprecated, same as (mode fallback)
;;  (locks ...)
;;  (alias ...) or (aliases ...) "Building this alias means building the targets of this rule."
;;  (enabled-if ...)
;; )

;; action field grammar:
;; (action <dsl expr>)
;; dsl: see https://dune.readthedocs.io/en/stable/concepts.html#user-actions

;; Unfortunately, the 'action' field seems to be optional in practice,
;; so we have a separate rule, -dune-rule-inferred->mibl, below.

;; rules with explicit 'action field:
(define -dune-rule-explicit->mibl
  (let ((+documentation+ "INTERNAL. Updates pkg arg, returns normalized stanza. stanza: raw dune stanza (input); nstanza: miblized (output)"))
    (lambda (ws pkg stanza)
      (mibl-trace-entry "-dune-rule-explicit->mibl" "")
      (mibl-trace "ws" ws)
      (mibl-trace "pkg" (assoc-val :pkg-path pkg))
      (mibl-trace "stanza" stanza)
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (let* ((pkg-path (assoc-val :pkg-path pkg))
             (rule-alist (cdr stanza))
             (mibl-trace-let "rule-alist" rule-alist)

             ;;FIXME: 'action optional, 'progn may also be used
             ;; e.g. menhir/src/dune
             ;; TESTING ONLY dsl:action->mibl
             (action (dsl:action->mibl (assoc-val 'action rule-alist)))
             (mibl-trace-let "rule action" action)
             ;; (_ (error 'x "X"))

             ;; Step 1: rule deps don't depend on targets, so do first
             (deps (expand-rule-deps ws pkg rule-alist))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (green "expanded rule deps") deps)))

             (targets (-handle-rule-targets ws rule-alist deps pkg))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (green "expanded targets") targets)))

             ;; all dune rule stanzas have an action, so they have a tool
             (tools `(::tools ,(gensym))) ;; gensym is a placeholder, to be replaced by set-cdr!
             (mibl-trace-let "tmp ::tools" tools)
             )

        (if deps
            ;; FIXME: this updates filegroups (globs) table; can we run it as a pipeline task?
            (begin
              (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (green "iterating deps") deps))
              (for-each (lambda (dep)
                          ;; dep forms:
                          ;; (:foo (:pkg a/b/c)(:tgt "foo.sh"))
                          ;; (::opam-pkg foo-bar-baz)
                          ;; (tezos-protocol-demo-noops ::opam-pkg)
                          (mibl-trace "dep" dep test: *mibl-debug-rule-stanzas*)
                          (case (cdr dep)
                            ((::opam-pkg) (cdr dep))
                            (else
                             (mibl-trace "filegroup dep?" dep test: *mibl-debug-rule-stanzas*)
                             ;; e.g. dep == (:css (:glob . "glob_STAR.css"))
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
                  (let ((nrule (-normalize-rule-action ws pkg tools rule-alist
                                                       (if (null? targets) (list :targets) targets)
                                                       (if deps deps (list :deps)))))
                    (mibl-trace "NRULE" nrule)
                    (mibl-trace "naction" action)
                    ;; (error 'dbg "DBG")
                    nrule))
                    ;; (concatenate nrule `(:test-action ,action))))

                 ;; rules whose action is not wrapped by (run ...)
                 ;; TODO: use dispatch table dune-action-cmds-no-dsl
                 ;; if stanza contains a field in dune-dsl-cmds then dispatch
                 (else
                  (error 'rule "rule w/o action?")
                  ;;OBSOLETE: use -dune-rule-inferred->mibl below
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
                          (-normalize-rule-action ws pkg tools rule-alist targets
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
          ;;                                     (assoc-in '(:rule ::outputs) mibl-rule) ;; name
          ;;                                     pkg-path (assoc-in '(:rule ::outputs) mibl-rule))

          ;; (append mibl-rule `((:test-action ,action)))
          mibl-rule
          )))))

;; rule stanza with implicit targets and deps
;; https://dune.readthedocs.io/en/stable/dune-files.html#inferred-rules
(define -dune-rule-inferred->mibl
  (let ((+documentation+ "INTERNAL. Updates pkg arg, returns normalized stanza. stanza: raw dune stanza (input); nstanza: miblized (output)"))
    (lambda (ws pkg stanza)
      (mibl-trace-entry "-dune-rule-inferred->mibl" "")
      (mibl-trace "ws" ws)
      (mibl-trace "pkg" (assoc-val :pkg-path pkg))
      (mibl-trace "stanza" stanza)
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (let* ((pkg-path (assoc-val :pkg-path pkg))
             (rule-alist (cdr stanza))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "rule-alist: ~A\n" rule-alist)))
             ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "target: ~A\n" (assoc 'target rule-alist))))
             ;; (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*) (format #t "Targets: ~A\n" (assoc-val 'targets rule-alist))))

             ;; Step 1: rule deps don't depend on targets, so do first
             (deps (expand-rule-deps ws pkg rule-alist))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (green "expanded rule deps") deps)))

             (targets (-handle-rule-targets ws rule-alist deps pkg))
             (_ (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                    (format #t "~A: ~A\n" (green "expanded targets") targets)))

             ;; all dune rule stanzas have an action, so they have a tool
             (tools `(::tools ,(gensym))) ;; gensym is a placeholder, to be replaced by set-cdr!
             (mibl-trace-let "tmp ::tools" tools)
             )

        (if deps
            ;; FIXME: this updates filegroups (globs) table; can we run it as a pipeline task?
            (begin
              (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (green "iterating deps") deps))
              (for-each (lambda (dep)
                          ;; dep forms:
                          ;; (:foo (:pkg a/b/c)(:tgt "foo.sh"))
                          ;; (::opam-pkg foo-bar-baz)
                          ;; (tezos-protocol-demo-noops ::opam-pkg)
                          (mibl-trace "dep" dep test: *mibl-debug-rule-stanzas*)
                          (case (cdr dep)
                            ((::opam-pkg) (cdr dep))
                            (else
                             (mibl-trace "filegroup dep?" dep test: *mibl-debug-rule-stanzas*)
                             ;; e.g. dep == (:css (:glob . "glob_STAR.css"))
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

        (let* ((mibl-rule
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
                          (-normalize-rule-action ws pkg tools rule-alist targets
                                                 (if deps deps (list :deps))))
                        (error 'no-action (format #f "rule without action: ~A" rule-alist)))))
               (mibl-rule (prune-mibl-rule mibl-rule)))
          (if (or *mibl-debug-rule-stanzas* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (green "mibl-rule") mibl-rule))

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
         ;; (_ (error 'X "STOP prune"))

          ;; (update-exports-table-with-targets! ws
          ;;                                     :FIXME ;; tag
          ;;                                     (assoc-in '(:rule ::outputs) mibl-rule) ;; name
          ;;                                     pkg-path (assoc-in '(:rule ::outputs) mibl-rule))
          mibl-rule)))))

(define dune-rule->mibl
  (let ((+documentation+ "INTERNAL. Updates pkg arg, returns normalized stanza. stanza: raw dune stanza (input); nstanza: miblized (output)"))
    (lambda (ws pkg stanza)
      (mibl-trace-entry "dune-rule->mibl" "")
      (mibl-trace "ws" ws)
      (mibl-trace "pkg" (assoc-val :pkg-path pkg))
      (mibl-trace "stanza" stanza)
      ;; for other stanza types we can normalize fields in isolation. For
      ;; 'rule' stanzas, we need a higher level of analysis, so we cannot
      ;; 'map' over the fields. Instead we extract the fields into local
      ;; vars.

      (if (assoc 'action (cdr stanza))
          (-dune-rule-explicit->mibl ws pkg stanza)
          (-dune-rule-inferred->mibl ws pkg stanza)))))

;; (format #t "loaded dune/dune_stanza_rule.scm\n")
