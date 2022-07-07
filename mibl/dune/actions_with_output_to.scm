;; dune/actions_with_output_to.scm

;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions

;; (with-<outputs>-to <file> <DSL>) to redirect the output to a file, where <outputs> is one of: stdout, stderr or outputs (for both stdout and stderr)

;; (ignore-<outputs> <DSL>) to ignore the output, where <outputs> is one of: stdout, stderr or outputs

;; (with-stdin-from <file> <DSL>) to redirect the input from a file

;; (with-accepted-exit-codes <pred> <DSL>) specifies the list of expected exit codes for the programs executed in <DSL>. <pred> is a predicate on integer values, and is specified using the Predicate language. <DSL> can only contain nested occurrences of run, bash, system, chdir, setenv, ignore-<outputs>, with-stdin-from and with-<outputs>-to. This action is available since dune 2.0.

;;
;; (action
;;  (with-stdout-to %{targets}
;;    (chdir %{workspace_root}
;;       (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "000_Ps9mPmXa"))))

;;
;; src/proto_003_PsddFKi3/lib_protocol/dune.inc
;; (action (with-stdout-to %{targets}
;;            (chdir %{workspace_root}
;;              (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

 ;; (action (run %{bin:tezos-protocol-compiler}  ./))

;; action may be a sequence of subactions, e.g.
;; (chdir %{workspace_root} (run %{cmd} "%{arg1}" "arg2"))
;; hypothesis: each action after the first is in a list
;; hypothesis 2: first action could also be a list?
;; ((chdir %{workspace_root}) (run %{cmd} "%{arg1}" "arg2"))
;; this would make an alist whose keys are action verbs
;; recursive parser:

(load "dune/dune_action_deps.scm")
(load "dune/expanders.scm")
(load "dune/api_utils.scm")

        ;;   (let* ((run-list
        ;;         (if (equal? 'chdir (car action))
        ;;             (caddr action) ;; skip chdir
        ;;             action))
        ;;      (prog-atom (cadr run-list))
        ;;      (prog-str (if (symbol? prog-atom)
        ;;                    (symbol->string prog-atom) prog-atom)))
        ;; (if (char=? #\% (string-ref prog-str 0))
        ;;     (let ((prog-vname (substring prog-str
        ;;                                  2 (- (length prog-str) 1))))
        ;;       ;; (format #t "  prog-vname: ~A\n" prog-vname)
        ;;       ;; return "std" exec names as-is; they will be resolved by
        ;;       ;; emitter. convert the others to bazel labels.
        ;;       (cond

        ;;        ;;FIXME: prefixe names may contain '../' etc.
        ;;        ;; e.g. %{exe:../config/discover.exe}

        ;;        ((string-prefix? "bin:" prog-vname)
        ;;         ;; (format #t "BIN prog: ~A\n" prog-vname)
        ;;         prog-atom)
        ;;        ((string-prefix? "exe:" prog-vname)
        ;;         ;; (format #t "EXE prog: ~A\n" prog-vname)
        ;;         prog-atom)
        ;;        ((string-prefix? "libexec:" prog-vname)
        ;;         ;; (format #t "LIBEXEC prog: ~A\n" prog-vname)
        ;;         prog-atom)
        ;;        ((string-prefix? "lib:" prog-vname)
        ;;         ;; (format #t "LIB prog: ~A\n" prog-vname)
        ;;         prog-atom)
        ;;        (else
        ;;         (format #t "CUSTOM progvar: ~A\n" prog-vname)
        ;;         (resolve-local-toolname pkg-path prog-vname action stanza))))))
        ;;   )))


;; action: (with-<outputs>-to <file> <DSL>) to redirect stdout to a file
;; <file> = usually %{targets} or %{target}???
;; (define (normalize-with-stdout-to pkg-path action stanza srcfiles)

;; (define (normalize-with-stdout-to pkg stanza-alist)
;; (define (normalize-action-with-stdout-to-dsl pkg action-alist targets deps)
;;   (format #t "NORMALIZE-ACTION-WITH-STDOUT-TO-DSL: ~A\n" action-alist)
;;   (format #t "  TARGETS: ~A\n" targets)
;;   (format #t "  DEPS: ~A\n" deps)
;;   (let* ((action-alist (assoc-val 'with-stdout-to action-alist))
;;          (_ (format #t "with-stdout-to runlist: ~A\n" action-alist))
;;          (stdout (car action-alist))
;;          (_ (format #t "with-stdout-to stdout: ~A\n" stdout))
;;          (raw-cmds (cadr action-alist))
;;          (_ (format #t "with-stdout-to cmd-list: ~A\n" raw-cmds))
;;          (cmd-alist (cmd-list->cmd-alist raw-cmds)) ;; '() '()))
;;          (_ (format #t "cmd-alist: ~A\n" cmd-alist))
;;          (cmd-list (expand-cmd-list cmd-alist targets deps))
;;          (cmd-list (append cmd-list `(:stdout ,stdout))))
;;     `((:cmd-list ,cmd-list)
;;       ;; (:raw ,raw-cmds)
;;       )))

    ;; (if (> (length dsl) 1)
    ;;     (format #t "normalize-cmd-dsl ct: ~A\n" (length dsl)))

    ;; (let-values (((filedeps vars env-vars universe aliases unresolved)
    ;;               (expand-rule-deps pkg-path
    ;;                            #f ;; tool-tag
    ;;                            tool
    ;;                            (assoc 'deps rule-alist)
    ;;                            srcfiles)))
    ;; (let ((action-deps (expan
    ;;   (format #t "filedeps: ~A\n" filedeps)
    ;;   (format #t "vars: ~A\n" vars)
    ;;   (format #t "env-vars: ~A\n" env-vars)
    ;;   (format #t "universe: ~A\n" universe)
    ;;   (format #t "aliases: ~A\n" aliases)
    ;;   (format #t "unresolved: ~A\n" unresolved)

      ;; (let ((cmd (if universe
      ;;                (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
      ;;                (normalize-cmd-dsl pkg-path
      ;;                                   target targets
      ;;                                   dsl
      ;;                                   (if filedeps (reverse filedeps)
      ;;                                       '())
      ;;                                   vars))))

      ;;   (format #t "wso cmd: ~A\n" cmd)
      ;;   ;; (format #t "xDSL: ~A\n" dsl)
      ;;   ;; (let ((deps (assoc ':deps cmd)))
      ;;   ;;   (for-each (lambda (dep)
      ;;   ;;               (format #t "D: ~A\n" dep))
      ;;   ;;             (cadr deps)))

      ;;   `(:rule ;; with-stdout-to
      ;;     (:out ,outfile)
      ;;     (:cmd ,cmd)
      ;;     (:vars ,vars)
      ;;     ;; ,(if (null? filedeps) '() `(:filedeps ,@(cdr filedeps)))
      ;;     (:raw ,stanza)))
    ;; )

