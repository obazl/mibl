;; (display "dune/dune_actions.scm loading...") (newline)

(load "dune/dune_action_run.scm")
(load "dune/actions_with_output_to.scm")

;; (system <cmd>), (bash <cmd>), (echo <string>)
(define (normalize-action-shell-cmd pkg action action-alist targets deps)
  ;; FIXME: shell cmd args may include filename literals; find way to expand?
  ;; FIXME: may include ${target}
  ;; FIXME: in general: expand all '${}' in args
  (format #t "NORMALIZE-ACTION-SHELL-CMD ~A\n" action)
  (let* ((tool (if (eq? action 'system) 'sh action))
         (action-args (assoc-val action action-alist)))
    `((:cmd
       ((:tool ,tool)
        ,(cons :args action-args))))))

(define (normalize-action-chdir-dsl pkg action-alist targets deps)
  (format #t "NORMALIZE-ACTION-CHDIR-DSL ~A\n" action-alist)
  (let* ((chdir-alist (assoc-val 'chdir action-alist))
         (_ (format #t "chdir-alist: ~A\n" chdir-alist))
         (ctx (car chdir-alist))
         (action (caadr chdir-alist))
         (action-alist (cdadr chdir-alist))
         (cmd (if-let ((cmd-fn (assoc-val action dune-action-cmds-dsl)))
                      (let ((cmd-list (apply (car cmd-fn)
                                             (list pkg action-alist targets deps))))
                        cmd-list)
                      (begin
                        (format #t "UNHANDLED ACTION: ~A\n" action)
                        stanza))))
    (list (cons :cmd cmd)
          `(:ctx ,ctx))))

;; cmp, copy, copy#, diff, diff?
(define (normalize-action-file-op pkg action action-alist targets deps)
  (format #t "NORMALIZE-ACTION-FILE-OP, pkg: ~A\n" pkg)
  (format #t "  action-alist: ~A\n" action-alist)
  (let* ((tool action) ;; (run-action->toolname pkg-path action stanza))
         (action-args (assoc-val action action-alist))
         (args (expand-filelist action-args
                               pkg ;; paths
                               ;; action-alist
                               '()))
         (_ (format #t "expanded copy args: ~A\n" args)))
    `((:cmd
       (:tool ,tool)
       ,(cons :args args)))))

(define (normalize-action-ignore-outputs-dsl action stanza)
  (format #t "NORMALIZE-ACTION-OUTPUTS-DSL ~A\n" action)
  '()
  )

(define (normalize-action-ignore-stderr-dsl action stanza)
  (format #t "NORMALIZE-ACTION-STDERR-DSL ~A\n" action)
  '()
  )

(define (normalize-action-ignore-stdout-dsl action stanza)
  (format #t "NORMALIZE-ACTION-STDOUT-DSL ~A\n" action)
  '()
  )

(define (normalize-action-no-infer-dsl action stanza)
  (format #t "NORMALIZE-ACTION-NO-INFOR-DSL ~A\n" action)
  '()
  )

(define (normalize-action-no-infer-dsl action stanza)
  (format #t "NORMALIZE-ACTION-NO-INFOR-DSL ~A\n" action)
  '()
  )

(define (normalize-action-pipe-outputs-dsl action stanza)
  (format #t "NORMALIZE-ACTION-PIPE-OUTPUTS-DSL ~A\n" action)
  '()
  )

(define (normalize-action-pipe-stderr-dsl action stanza)
  (format #t "NORMALIZE-ACTION-PIPE-STDERR-DSL ~A\n" action)
  '()
  )

(define (normalize-action-pipe-stdout-dsl action stanza)
  (format #t "NORMALIZE-ACTION-PIPE-STDOUT-DSL ~A\n" action)
  '()
  )

(define (normalize-action-progn-dsl action stanza)
  (format #t "NORMALIZE-ACTION-PROGN-DSL ~A\n" action)
  '()
  )

(define (normalize-action-run-dsl pkg action-alist targets deps)
  (format #t "NORMALIZE-ACTION-RUN-DSL ~A\n" action-alist)
  (let ((cmd (car (expand-cmd-list action-alist targets deps))))
    (format #t "RUN CMD: ~A\n" cmd)
    cmd))

(define (normalize-action-setenv-dsl action stanza)
  (format #t "NORMALIZE-ACTION-SETENV-DSL ~A\n" action)
  '()
  )

(define (normalize-action-with-accepted-exit-codes-dsl action stanza)
  (format #t "NORMALIZE-ACTION-WITH-ACCEPTED-EXIT-CODES-DSL ~A\n" action)
  '()
  )

(define (normalize-action-with-outputs-to-dsl action stanza)
  (format #t "NORMALIZE-ACTION-WITH-OUTPUTS-TO-DSL ~A\n" action)
  '()
  )

(define (normalize-action-with-stderr-to-dsl action stanza)
  (format #t "NORMALIZE-ACTION-WITH-STDERR-TO-DSL ~A\n" action)
  '()
  )

(define (normalize-action-with-stdin-from-dsl action stanza)
  (format #t "NORMALIZE-ACTION-WITH-STDIN-FROM-DSL ~A\n" action)
  '()
  )

(define (normalize-action-write-file pkg action action-alist targets deps)
  ;; action stanza)
  ;; action target targets deps)
  (format #t "NORMALIZE-WRITE-FILE ~A\n" action)
  (format #t "    action-alist ~A\n" action-alist)

  (let* ((args (assoc-val 'write-file action-alist))
         (file (car args))
         (_ (format #t "file: ~A\n" file))
         (content `(:content ,(cadr args)))
         (args (expand-filelist (list file)
                                pkg ;; paths
                                ;; action-alist
                                '())))
    `((:cmd-list
       ((:tool ,action)
        ,(cons :args (list args content)))))))

    ;; `(:write-file
    ;;   (:out ,outfile)
    ;;   (:content ,(car str))
    ;;   ,(if deps `(:deps ,@(cdr deps)) '(:deps ()))
    ;;   (:raw ,stanza))))

;; expand-deps
;; (:name <dependencies>) available as %{name} in actions.
;; (file <filename>) or simply <filename>
;; (alias <alias-name>): e.g. (alias src/runtest)
;; (alias_rec <alias-name>): recursively in all children directories wherever it is defined. For instance: (alias_rec src/runtest) might depend on (alias src/runtest), (alias src/foo/bar/runtest), …
;; (glob_files <glob>)
;; (source_tree <dir>): depend on all srcs in the subtree at root <dir>
;; (universe): depend on everything in the universe; this is only for dependencies in the installed world, you must still specify all dependencies that come from the workspace.
;; (package <pkg>) depend on all files installed by <package>, as well as on the transitive package dependencies of <package>. This can be used to test a command against the files that will be installed
;; (env_var <var>): depend on the value of the environment variable <var>. If this variable becomes set, becomes unset, or changes value, the target will be rebuilt.
;; (sandbox <config>): require a particular sandboxing configuration. <config> can be one (or many) of:
;; always: the action requires a clean environment.
;; none: the action must run in the build directory.
;; preserve_file_kind: the action needs the files it reads to look like normal files (so dune won’t use symlinks for sandboxing

;; ((chdir %{workspace_root} (run %{libexec:tezos-protocol-environment-packer:s_packer} "sigs" %{deps})))

;; dsl expr will usually contain vars of form %{foo}. such vars may be
;; defined in some other package, e.g.
;; %{libexec:tezos-protocol-environment-packer:s_packer} or locally,
;; within the 'rule' stanza itself. There are (at least) two kinds of such
;; 'local' vars: field names (e.g. field 'deps' => %{deps}),
;; and vars defined within the 'deps' fld, e.g.
;; (:src_dir <filename>) => ${src_dir}

;; external vars will be resolved by emitter (after normalization).

;; output: ((:tool %{libexec:tezos-protocol-environment-packer:s_packer})
;;          (:args ("sigs" %{deps}))
;;          (:vars ((:deps ...)
;;                  (:src_dir TEZOS_PROTOCOL)
;;                  (:other_var ...))))

(define (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
  ;; (format #t "NORMALIZE-CMD-DSL-UNIVERSE: ~A\n" dsl)
  ;; special case: using 'universe' dep and passing e.g. unix.cma
  ;; e.g.
  ;; (rule
  ;;  (targets generated_git_info.ml)
  ;;  (deps (universe) (:script get-git-info.mlt))
  ;;  (action (with-stdout-to %{targets} (run %{ocaml} unix.cma %{script}))))

  ;; 'universe' means: "depend on everything in the universe. This is
  ;; for cases where dependencies are too hard to specify."

  ;; atm we punt and make the developer deal with this by hand
  `((:tool #f)
    (:deps '())
    (:args '())
    (:universe #t) ;; emitter does sth special when it finds this
    (:raw ,dsl))
  )

;;FIXME: for tools use sh to see if executable, not file-exists?
(define resolve-string-arg
  ;; to resolve argstrings with multiple %{} vars we need to loop/recur
  (lambda (pkg-path arg vars)
    (format #t "RESOLVE-STRING-ARG: ~A\n" arg)
    (format #t " vars: ~A\n" vars)
    (if (or (equal? 'bash arg) (equal? "bash" arg))
        'bash
        ;; else scan arg to see if it contains %{} vars, replace if found
        (if-let ((beg-delim (string-contains arg "%{")))
            (let* ((tok-pfx (substring arg (+ beg-delim 2)))
                   (end-delim (string-index tok-pfx
                                            (lambda (ch)
                                              (char=? ch #\}))))
                   (tok (string-take tok-pfx end-delim))
                   (key (string->symbol (string-append ":" tok))))
              (format #t "substituting in '~A'\n" arg)
              (format #t "tok-pfx: ~A\n" tok-pfx)
              (format #t "tok: ~A\n" tok)
              (format #t "key: ~A\n" key)
              (if-let ((val-assoc (assoc key vars)))
                      (let* ((val (cadr val-assoc))
                             (subst (case (car val)
                                      ((:_srcfile)
                                       (let ((r (string-append
                                                 "//"
                                                 (cadr val)
                                                 ":" (caddr val))))
                                         (format #t "_srcfile: ~A\n" r)
                                         r))

                                      ((:_genfile)
                                       (string-append "//"
                                                      (cadr val)
                                                      ":" (caddr val)))
                                      (else
                                       )))
                             ;; (locn (list :_location subst))
                             (locn (string-append "$(location "
                                                  subst ")"))
                             (new (string-replace arg locn
                                                  beg-delim
                                                  (+ beg-delim end-delim
                                                     3 ;; %, {, and }
                                                     ))))
                        (format #t "NEW: ~A\n" new)
                        (resolve-string-arg pkg-path new vars))
                      ;; else not a var key
                      arg))
            (let ((arg (normalize-toolname pkg-path arg)))
              (if (file-exists? arg)
                  (list :_srcfile
                        (dirname arg)
                        (basename arg))
                  (if (string-index arg (lambda (ch) (equal ch #\space)))
                      arg
                      (list :_genfile
                            (dirname arg) (basename arg)))))))))

(define (normalize-tool-tag tag)
  (let ((tag-str (if (symbol? tag) (symbol->string tag) tag)))
    (if (string-prefix? "%{" tag-str)
        (string->symbol
         (string-append ":" (substring tag-str 2 (- (length tag-str) 1))))
        tag)))

(define (normalize-progn-action pkg stanza-alist)
  (format #t "NORMALIZE-PROGN-ACTION: ~A\n" stanza-alist)
  #t)

(define (Xnormalize-progn-action pkg-path action stanza srcfiles)
  (format #t "NORMALIZE-PROGN-ACTION: ~A\n" action)
  ;; tezos: (action progn), (action (progn)), (action (progn (...) ...))
  ;; missing but maybe possible: (action progn ...)

  ;; empty progn: (action (progn)) - forces eval of rule for side-effects?

  ;; examples:
  ;; (rule (alias runtest)
  ;;       (package tezos-protocol-004-Pt24m4xi)
  ;;       (deps (alias runtest_sandbox))
  ;;       (action (progn)))  <<== null action?
  ;; "Building this alias means building the targets of
  ;; this rule." Presumably that means deps too.
  ;; "The typical use of the alias stanza is to define tests:
  ;; (rule (alias   runtest)
  ;;       (action (run %{exe:my-test-program.exe} blah)))
  ;; "[T]o define a test a pair of alias and
  ;; executable stanzas are required."

  ;; more common:
  ;; (progn
  ;;  (bash "touch .depend")
  ;;  (run make %{targets} COMPUTE_DEPS=false)
  ;;  (bash "rm .depend")))))))

  ;; (let* ((rule-alist (cdr stanza))
  ;;        (stanza-type (if (assoc 'alias rule-alist) :alias-cmd :run-cmd))

  (let* ((rule-alist (cdr stanza))
         (alias (assoc 'alias rule-alist))

         (progn (cdadr action))
         (stanza-type (if (null? progn) :null-cmd :run-cmd))

         (_ (format #t "progn: ~A\n" progn))
         (deps (assoc 'deps rule-alist))
         (_ (format #t "deps: ~A\n" deps)))

    (let-values (((filedeps vars env-vars universe aliases unresolved)
                  (expand-deps pkg-path
                               #f
                               #f
                               deps
                               srcfiles)))
      (format #t "r filedeps: ~A\n" filedeps)
      (format #t "r vars: ~A\n" vars)
      (format #t "r env-vars: ~A\n" env-vars)
      (format #t "r universe: ~A\n" universe)
      (format #t "r aliases: ~A\n" aliases)
      (format #t "r unresolved: ~A\n" unresolved)

      ;; (for-each (lambda (filedep)
      ;;             (format #t "~A\n" filedep))
      ;;           (reverse filedeps))
      ;; '())))

      (let* (
             (result `((:pkg ,pkg-path)
                       (:raw ,stanza)))
             (result (if (null? vars)
                         result
                         (acons :vars vars result)))
             (result (if (null? filedeps)
                         result
                         (acons :deps filedeps result)))
             ;; (result (if (null? outfile)
             ;;             result
             ;;             (acons (:out outfile) result)))
             (result (if-let ((alias (assoc 'alias rule-alist)))
                             (acons :alias (last alias) result)
                             result)))
        `(,stanza-type ,result)))))

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
    ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dune-action-cmds-no-dsl
  ;; primary cmds that do NOT take a DSL argument
  `((bash       ,normalize-action-shell-cmd) ;; (bash <cmd>)
    (cat        ,normalize-action-file-op)  ;; (cat <file>)
    (cmp        ,normalize-action-file-op) ;; (cmp <file1> <file2>)
    (copy       ,normalize-action-file-op) ;; (copy <src> <dst>)
    (copy#      ,normalize-action-file-op) ;; copy & add header line
    (diff       ,normalize-action-file-op) ;; (diff <file1> <file2>)
    (diff?      ,normalize-action-file-op) ;; (diff? <file1> <file2>)
    (echo       ,normalize-action-shell-cmd) ;; (echo <string>)
    (system     ,normalize-action-shell-cmd) ;; (system <cmd>) uses sh
    (write-file ,normalize-action-write-file) ;; (write-file <file> <string>)
    ))

(define dune-action-cmds-dsl
  ;; primary cmds that DO take a DSL argument
  `((chdir ,normalize-action-chdir-dsl) ;; (chdir <dir> <DSL>)
    ;; (ignore-<outputs> <DSL>)
    (ignore-outputs ,normalize-action-ignore-outputs-dsl)
    (ignore-stderr ,normalize-action-ignore-stderr-dsl)
    (ignore-stdout ,normalize-action-ignore-stdout-dsl)
    (no-infer ,normalize-action-no-infer-dsl) ;; (no-infer <DSL>)
    ;; (pipe-<outputs> <DSL> <DSL> <DSL>...)
    (pipe-outputs ,normalize-action-pipe-outputs-dsl)
    (pipe-stderr ,normalize-action-pipe-stderr-dsl)
    (pipe-stdout ,normalize-action-pipe-stdout-dsl)
    (progn ,normalize-action-progn-dsl)       ;; (progn <DSL>...)
    (run ,normalize-action-run-dsl)       ;; (progn <DSL>...)
    (setenv ,normalize-action-setenv-dsl)     ;; (setenv <var> <value> <DSL>)
                                       ;; NB: (run env FOO=bar ...)
    ;; (with-accepted-exit-codes <pred> <DSL>)
    (with-accepted-exit-codes ,normalize-action-with-accepted-exit-codes-dsl)
    ;; (with-<outputs>-to <file> <DSL>)
    (with-outputs-to ,normalize-action-with-outputs-to-dsl)
    (with-stderr-to ,normalize-action-with-stderr-to-dsl)
    (with-stdout-to ,normalize-action-with-stdout-to-dsl)
    ;; (with-stdin-from <file> <DSL>)
    (with-stdin-from ,normalize-action-with-stdin-from-dsl)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (normalize-action pkg stanza-alist targets deps) ;; rule stanza
  (format #t "NORMALIZE-action: ~A\n" stanza-alist)
  (let* ((action-assoc (assoc 'action stanza-alist))
         (action-alist (assoc-val 'action stanza-alist))
         (action (if (pair? (car action-alist)) ;; e.g. (action (tool ...))
                     (caar action-alist)
                     ;; else (action tool ...)
                     (cadr action-alist))))
    (format #t "  action action: ~A\n" action)
    (format #t "  action alist: ~A\n" action-alist)

    (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
            (let ((cmd-list (apply (car cmd-fn)
                            (list pkg action action-alist targets deps))))
              cmd-list)
            (if-let ((cmd-fn (assoc-val action dune-action-cmds-dsl)))
                    (let ((cmd-list (apply (car cmd-fn)
                                      (list pkg action-alist targets deps))))
                      cmd-list)
                    (begin
                      (format #t "UNHANDLED ACTION: ~A\n" action)
                      stanza)))))

;;     (case action  ;; (car action)
;;     ;;   ((copy#) (normalize-copy-action pkg-path action stanza srcfiles))
;;     ;;   ((copy) (normalize-copy-action pkg-path action stanza srcfiles))

;;       ((run)
;;        ;; (normalize-run-action pkg action stanza-alist))
;;        (let ((cmd-list (normalize-run-action pkg action-alist targets deps)))
;;          (format #t "RUN ACTION: ~A\n" cmd-list)
;;          (set-cdr! action-assoc (list cmd-list))
;;          cmd-list))


;;       ;; pkg-path action stanza srcfiles))

;;       ((progn) (normalize-progn-action pkg stanza-alist))
;;        ;; (normalize-progn-action pkg-path action stanza srcfiles))

;;       ((with-stdout-to)
;;        (let ((cmd-list (normalize-with-stdout-to
;;                         pkg action-alist targets deps)))
;;          ;; (set-cdr! action-assoc (list cmd-list))
;;          cmd-list))
;;        ;; (normalize-with-stdout-to pkg stanza-alist))
;;        ;; (normalize-with-stdout-to pkg-path action stanza srcfiles))

;;     ;;   ((with-stderr-to) stanza)
;;     ;;   ((with-stdin-from) stanza)
;;     ;;   ((with-outputs-to) stanza)

;;     ;;   ((write-file) (normalize-write-file action stanza))

;;       (else
;;        (format #t "UNHANDLED ACTION\n")
;;        stanza))
;; ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (display "loaded dune/dune_actions.scm") (newline)
