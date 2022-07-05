;; (display "dune/dune_actions.scm loading...") (newline)

(load "dune/dune_action_run.scm")
(load "dune/actions_with_output_to.scm")

(define (normalize-action-bash action stanza)
  (format #t "NORMALIZE-ACTION-BASH ~A\n" action)
  '()
  )

(define (normalize-action-cat action stanza)
  (format #t "NORMALIZE-ACTION-CAT ~A\n" action)
  '()
  )

(define (normalize-action-chdir-dsl action stanza)
  (format #t "NORMALIZE-ACTION-CHDIR-DSL ~A\n" action)
  '()
  )

(define (normalize-action-cmp action stanza)
  (format #t "NORMALIZE-ACTION-CMP ~A\n" action)
  '()
  )

;; (copy <src> <dst>), (diff file1 file2) etc
(define (normalize-action-file-binop pkg action action-alist targets deps)
  (format #t "NORMALIZE-ACTION-FILE-BINOP, pkg: ~A\n" pkg)
  (format #t "  action-alist: ~A\n" action-alist)
  (let* ((tool action) ;; (run-action->toolname pkg-path action stanza))
         (action-args (assoc-val action action-alist))
         (args (expand-deplist action-args
                               pkg ;; paths
                               action-alist
                               '()))
         (_ (format #t "expanded copy args: ~A\n" args)))
    `((:cmd-list
       ((:tool ,tool)
        ,(cons :args args))))))

;; (define (normalize-action-copy pkg action action-alist targets deps)
;;   (format #t "NORMALIZE-ACTION-COPY, pkg: ~A\n" pkg)
;;   (format #t "  action-alist: ~A\n" action-alist)
;;   (let* ((tool action) ;; (run-action->toolname pkg-path action stanza))
;;          (action-args (assoc-val 'copy action-alist))
;;          (args (expand-deplist action-args
;;                                pkg ;; paths
;;                                action-alist
;;                                '()))
;;          ;; (cpy-src (car action-args))
;;          ;; (cpy-dst (cadr action-args))
;;          ;; (args (list cpy-src cpy-dst))
;;          (_ (format #t "expanded copy args: ~A\n" args)))
;;     `((:cmd-list
;;        ((:tool ,tool)
;;         ,(cons :args args))))))

(define (normalize-action-copy# action stanza)
  (format #t "NORMALIZE-ACTION-COPY# ~A\n" action)
;; (define (normalize-copy-action pkg-path action stanza srcfiles)
;;   (format #t "NORMALIZE-COPY-ACTION: ~A: ~A\n" pkg-path action)
  ;; (format #t "  STANZA: ~A\n" stanza)

  (let* ((rule-alist (cdr stanza))
         (stanza-type (if (assoc 'alias rule-alist) :alias-cmd :run-cmd))

         (tool (run-action->toolname pkg-path action stanza))
         (_ (format #t "TOOL: ~A\n" tool))

         (tool-tag (normalize-tool-tag (cadadr action)))
         (_ (format #t "TOOL-TAG: ~A\n" tool-tag))

         (run-list (cadr action)) ;; (run <tool> <arg1> ...)
         ;; (_ (format #t "run-list: ~A\n" run-list))

         (target (if-let ((target (assoc 'target rule-alist)))
                         (cadr target) #f))
         ;; (_ (format #t "target: ~A\n" target))
         (targets (if-let ((targets (assoc 'targets rule-alist)))
                          (cadr targets)
                          #f))
         ;; (_ (format #t "targets: ~A\n" targets))

         ;;FIXME: run actions for "alias runtest" etc. have no target(s) fld
         (outfile (if target target
                      (if targets targets
                          #f)))

         ;; (_ (format #t "outfile: ~A\n" outfile))

         (args (run-action->args pkg-path action run-list))
         ;; (_ (format #t "CMD ARGS: ~A\n" args))

         (dsl run-list)

         ;; (deps (run-action->deps pkg-path tool stanza))
         ;; (_ (format #t "CMD DEPS: ~A\n" deps))

         ;;        (dsl (cadr (cdadr action)))
         ;;        ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.
         ;; (cmd `((:tool ,tool)
         ;;        (:args ,args)
         ;;        (:raw ,action)))
         ;;        (target (assoc 'target rule-alist))
         ;;        (targets (assoc 'targets rule-alist))
         ;;        (outfile (if (equal? file '%{targets})
         ;;                     (cadr targets)
         ;;                     (if (equal? file '%{target})
         ;;                         (cadr target)
         ;;                         (begin
         ;;                           (format #t "WARNING: write-file out: ~A\n" file)
         ;;                           file))))
         )
    ;; (format #t "DSL: ~A\n" dsl)

    (let-values (((filedeps vars env-vars universe aliases unresolved)
                  (expand-deps pkg-path
                               #f ;; tool-tag ;; FIXME: tool-tag
                               #f ;; tool
                               (assoc 'deps rule-alist)
                               srcfiles)))
      ;; (format #t "r filedeps: ~A\n" filedeps)
      ;; (format #t "r vars: ~A\n" vars)
      ;; (format #t "r env-vars: ~A\n" env-vars)
      ;; (format #t "r universe: ~A\n" universe)
      ;; (format #t "r aliases: ~A\n" aliases)
      ;; (format #t "r unresolved: ~A\n" unresolved)

      (let ((cmd (if universe
                     (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
                     (normalize-cmd-dsl pkg-path
                                        target
                                        targets
                                        (list dsl)
                                        (if filedeps
                                            (reverse filedeps)
                                            '())
                                        (if vars vars '())))))
        (format #t "r CMD: '~A'\n" cmd)

        ;;   (if (not (null? vars))
        ;;       (format #t "DEPS VARS: ~A\n" vars))

        ;;   (format #t "DSL: ~A\n" dsl)

        `(,stanza-type ;;:run-cmd
          (:out ,outfile)
          (:cmd ,cmd) ;; contains deps?
          (:vars ,vars)
          (:raw ,stanza)))))
  )

;; (define (normalize-action-diff pkg action-alist targets deps); action stanza)
;;   (format #t "NORMALIZE-ACTION-DIFF ~A\n" action)
;;   '()
;;   )

(define (normalize-action-diff? action stanza)
  (format #t "NORMALIZE-ACTION-DIFF ~A\n" action)
  (format #t "ERROR! 'diff?' not supported (semantics unclear).\n")
  '()
  )

(define (normalize-action-echo action stanza)
  (format #t "NORMALIZE-ACTION-ECHO ~A\n" action)
  '()
  )

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

(define (normalize-action-system action stanza)
  (format #t "NORMALIZE-ACTION-SYSTEM ~A\n" action)
  '()
  )

(define (normalize-action-write-file action stanza)
  ;; action target targets deps)
  (format #t "NORMALIZE-WRITE-FILE ~A\n" action)

  ;; CAVEAT: a write-file action may have 'deps or other fields, which
  ;; are not necessarily included in the out string.

  (let* ((file (cadadr action))
         (str (cdr (cdadr action)))
         (rule-alist (cdr stanza))
         (deps (assoc 'deps rule-alist))
         (target (assoc 'target rule-alist))
         (targets (assoc 'targets rule-alist))
         ;; outfile will usually be '%{targets}' or '%{target}', but
         ;; could be a literal, or ?
         (outfile (if (equal? file '%{targets})
                      ;; for write-file, must be singleton?
                      (cadr targets)
                      (if (equal? file '%{target})
                          (cadr target)
                          (begin
                            (format #t "WARNING: write-file out: ~A\n" file)
                            file)))))

    ;;     (format #t "  File: ~A ~A\n" target targets))
    ;; (format #t "  String: ~A\n" str)

    `(:write-file
      (:out ,outfile)
      (:content ,(car str))
      ,(if deps `(:deps ,@(cdr deps)) '(:deps ()))
      (:raw ,stanza))))

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

(define (normalize-cmd-dsl pkg-path target targets dsl filedeps vars)
  ;; assumption: dsl is a list of one list of commands
  ;; maybe: cmd list always contains at least one (run ...)
  ;; common: cmd list contains ((chdir <dir> (run ...))); we ignore chdir
  (format #t "NORMALIZE-CMD-DSL: ~A\n" dsl)
  (format #t " filedeps: ~A\n" filedeps)
  (format #t " vars: ~A\n" vars)

  ;; WARNING!!! Cmd args must not be reordered, and args that are
  ;; targets must be identifiable, so the emitter can wrap them in
  ;; "$(location ...)"

  (let recur ((cmd-dsl dsl) ;; (if (null? dsl) '() (car dsl)))
              (tool #f)
              (deps filedeps)
              (args '()))
    (format #t "recur on cmd-dsl: ~A\n" cmd-dsl)
    (cond
     ((null? cmd-dsl)
    ;; (if (null? cmd-dsl)
        ;; this should not happen since we expect a (run...) to be handled
        `((:tool ,tool)
          (:deps ,deps)
          (:args ,(reverse
                   (expand-cmd-args pkg-path target targets
                                     args filedeps vars)))
          (:raw ,dsl)))
     ((pair? (car cmd-dsl))
      ;; (format #t "PAIR (car cmd-dsl): ~A\n" (car cmd-dsl))
      (cond
       ((equal? 'bash (caar cmd-dsl))
        (format #t "BASH: ~A\n" cmd-dsl)
        (recur (cdar cmd-dsl) 'bash deps args))

       ((equal? 'copy# (caar cmd-dsl))
        (format #t "COPY#: ~A\n" cmd-dsl)
        (recur (cdar cmd-dsl) 'copy# deps args))

       ((equal? 'copy (caar cmd-dsl))
        (format #t "COPY: ~A\n" cmd-dsl)
        (recur (cdar cmd-dsl) 'copy deps args))

       ((equal? 'chdir (caar cmd-dsl))
        (format #t "SKIPPING CHDIR\n")
        (begin ;; skip chdir
          (if (not (pair? (caddr (car cmd-dsl))))
              (error 'bad-arg
                     (format #f "WARN: chdir <dir> not followed by list: ~A\n"
                             (caddr (car cmd-dsl)))))
          (recur (cddr (car cmd-dsl)) tool deps args)))

       ((equal? 'run (caar cmd-dsl))
        (format #t "RUN ~A\n" cmd-dsl)
        (let run-recur ((cmd-args (cddr (car cmd-dsl)))
                        ;; (run-deps deps) ;;FIXME: deps is already fixed?
                        (run-args args))
          (format #t "RUN-RECUR cmd-args: ~A\n" cmd-args)
          (format #t "          run-args: ~A\n" run-args)
          ;; (format #t "(car cmd-args): ~A\n"
          ;;         (if (null? cmd-args) '() (car cmd-args)))
          (if (null? cmd-args)
              (begin
                (format #t "FINISHED, tool: ~A\n" (cadr (car cmd-dsl)))
                (let* ((sexp
                       `((:tool ,(resolve-tool (cadr (car cmd-dsl))
                                        pkg-path
                                        target targets
                                        (list (cadr (car cmd-dsl)))
                                        filedeps
                                        vars))
                         ;; (action->toolname pkg-path action stanza)
                         (:raw ,dsl)))
                      (sexp (if (null? args)
                                sexp
                                (:args ,(expand-cmd-args pkg-path
                                                          target targets
                                                          (reverse run-args)
                                                          filedeps
                                                          vars))))
                      (sexp (if (null? deps)
                                sexp
                                (acons :deps deps sexp))))
                      sexp))

              ;; from here on we expect only args
              ;; problem: we can't decide of a string arg is a filedep??
              ;; caveat: dune vars of form %{foo} may be passed as
              ;; either string or symbol
              ;; assume: a dune var passed as cmd arg is also a dep
              (cond
               ((number? (car cmd-args))
                (run-recur (cdr cmd-args)
                           ;; run-deps
                           (cons
                            ;;(number->string (car cmd-args))
                            `(:_nbr ,(car cmd-args))
                            run-args)))
               ((string? (car cmd-args))
                (format #t "STRING cmd arg: ~A\n" cmd-args)
                (if (string-prefix? "%{" (car cmd-args))
                    (begin
                      (format #t "ARG VAR: ~A\n" cmd-args)
                      (run-recur (cdr cmd-args)
                                 ;; run-deps ;;(cons (car cmd-args) run-deps)
                                 (cons (car cmd-args) run-args)))
                    (begin
                      (format #t "ARG STRING: ~A\n" cmd-args)
                      (run-recur (cdr cmd-args)
                                 ;; run-deps
                                 (cons
                                  `(:_string ,(car cmd-args))
                                  run-args)))))
               ((symbol? (car cmd-args))
                (format #t "SYMBOL cmd arg: ~A\n" cmd-args)
                (let ((arg-str (symbol->string (car cmd-args))))
                  (cond

                   ((string-prefix? "%{bin:" arg-str)
                    (format #t "BIN arg: ~A\n" arg-str)
                    (run-recur (cdr cmd-args)
                               ;;run-deps
                               (cons arg-str run-args)))

                   ((string-prefix? "%{lib:" arg-str)
                    (format #t "LIB arg: ~A\n" arg-str)
                    (run-recur (cdr cmd-args)
                               ;;run-deps
                               (cons arg-str run-args)))

                   ((string=? "%{deps}" arg-str)
                    (format #t "DEPS arg\n")
                    (run-recur (cdr cmd-args)
                               ;;run-deps
                               (cons arg-str run-args)))

                   ((string-prefix? "%{" arg-str)
                    (format #t "VARx\n")
                    (cond
                     ((string-prefix? "%{dep:" arg-str)
                      (format #t "DEP: var\n")
                      (format #t "pkg-path: ~A\n" pkg-path)
                      (let* ((dep-path (string-drop-right ;; drop '}'
                                        (string-drop arg-str 6) 1))
                             (segs (string-split dep-path #\/))
                             (seg-ct (length segs))
                             (resolved (let recur ((segs segs)
                                                   (path pkg-path))
                                         (if (null? segs)
                                             path
                                             ;; FIXME: what if embedded, a/../b
                                             (if (string=? ".." (car segs))
                                                 (let ((last-slash
                                                        (string-index-right
                                                         path (lambda (ch) (char=? ch #\/)))))
                                                   (if last-slash
                                                       (recur (cdr segs)
                                                              (string-drop-right
                                                               path (- (length path) last-slash)))
                                                       ;; no slash in pkg-path
                                                       (recur (cdr segs) ".")))
                                                 (recur (cdr segs) path)
                                                 )))))
                        (format #t "DEP PATH: ~A\n" dep-path)
                        (run-recur (cdr cmd-args)
                                   ;; run-deps ;; (cons arg-str run-deps)
                                   (cons arg-str run-args))))
                     (else ;; e.g. ${prim}
                      (format #t "OTHER VAR: ~A\n" cmd-args)
                      (let* ((dep-path (string-drop-right ;; drop '}'
                                        (string-drop arg-str 6) 1))
                             (segs (string-split dep-path #\/))
                             (seg-ct (length segs))
                             (resolved (let recur ((segs segs)
                                                   (path pkg-path))
                                         (if (null? segs)
                                             path
                                             ;; FIXME: what if embedded, a/../b
                                             (if (string=? ".." (car segs))
                                                 (let ((last-slash
                                                        (string-index-right
                                                         path (lambda (ch) (char=? ch #\/)))))
                                                   (if last-slash
                                                       (recur (cdr segs)
                                                              (string-drop-right
                                                               path (- (length path) last-slash)))
                                                       ;; no slash in pkg-path
                                                       (recur (cdr segs) ".")))
                                                 (recur (cdr segs) path)
                                                 )))))
                        (format #t "DEP PATH: ~A\n" dep-path)
                        (run-recur (cdr cmd-args)
                                   ;; run-deps ;; (cons arg-str run-deps)
                                   (cons arg-str run-args)))
                      )))
                   (else
                    (format #t "OTHER SYM\n")
                    (run-recur (cdr cmd-args)
                                     ;; run-deps
                                     (cons (car cmd-args) run-args))))))
               (else ;; not string, sym, or number
                (error 'wtf "WTF? ~A\n" (car cmd-dsl))
                )))))
       (else ;; car not chdir, not run
        (error 'wtf2 "WTF2? ~A" (car cmd-dsl)))
       ))

     ((symbol? (car cmd-dsl))
      (let* ((arg-str (symbol->string (car cmd-dsl)))
             (arg (resolve-string-arg pkg-path arg-str vars)))
        (format #t "resolved: ~A\n" arg)
        (recur (cdr cmd-dsl) tool deps (cons arg args))))

     ((string? (car cmd-dsl))
      (let ((arg (resolve-string-arg pkg-path (car cmd-dsl) vars)))
        (recur (cdr cmd-dsl) tool deps (cons arg args))))

     (else ;; not (pair? (car cmd-dsl))
      (format #t "ATOM (car cmd-dsl): ~A\n" cmd-dsl)
      (recur (cdr cmd-dsl) tool deps (cons (car cmd-dsl) args))))
    ))

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
  `((bash       ,normalize-action-bash) ;; (bash <cmd>)
    (cat        ,normalize-action-cat)  ;; (cat <file>)
    (cmp        ,normalize-action-file-binop) ;; (cmp <file1> <file2>)
    (copy       ,normalize-action-file-binop) ;; (copy <src> <dst>)
    (copy#      ,normalize-action-file-binop) ;; copy & add header line
    (diff       ,normalize-action-file-binop) ;; (diff <file1> <file2>)
    (diff?      ,normalize-action-diff?) ;; (diff? <file1> <file2>)
    (echo       ,normalize-action-echo) ;; (echo <string>)
    (system     ,normalize-action-system) ;; (system <cmd>)
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
    (progn, normalize-action-progn-dsl)       ;; (progn <DSL>...)
    (setenv, normalize-action-setenv-dsl)     ;; (setenv <var> <value> <DSL>)
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
