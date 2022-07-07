(display "loading dune/expanders.scm\n")

;; several expansions: targets, cmd tools, cmd args
;; deps (see dune_action_deps.scm)

;; examples: (run bash ...), (run cp %{deps} ./)
;; (run env CARGO_NET_GIT_FETCH_WITH_CLI=true cargo build --release)
;; (action (run ./discover.exe))
;; (run bindings_gen/snarky_bn382_ctypes_stubs.exe)
;; (run
;;   %{libexec:tezos-protocol-compiler:replace}
;;   %{libexec:tezos-protocol-compiler:dune_protocol.template}
;;   "foo.ml"
;;   %{libexec:tezos-protocol-compiler:final_protocol_versions})))
;; (action (run %{bin:tezos-protocol-compiler}  ./))
;; (run %{<} %{targets})
;; (action (run %{deps} --test))
(define expand-run-tool
  ;; (lambda (tool pkg-path target targets args filedeps vars)
  (lambda (tool targets deps)
    (format #t "expand-run-tool ~A\n" tool)
    ;; for now, leave it to clients to decide what to do with it
    (let ((x (expand-cmd-args (list tool) targets deps)))
      x)))

;; expand-cmd-args:
    ;; if it is a 'dep:' var %{dep:rejections.sh}, use file-exists?
    ;; if it is a resolvable var (e.g. ?) look it up in
    ;; vars and test executability.
    ;; if it is a bin:, lib: etc. var, return it as-is
    ;; and let the emitter decide how to resolve it.
    ;; otherwise, if tool is plain string check with file-exists?
    ;; if no, "bash" check if executable
    ;; bash is special since genrule supports 'bash_cmd'
    ;; for other well-known programs (sh, sed, awk, etc.) just check
    ;; for executable bit.
    ;; tool will go in 'exec_tools' of genrule, and also in 'srcs' if
    ;; it is a local file.
    ;; (let ((tool (if (symbol? tool) (symbol->string tool)
    ;;                (if (string? tool) tool
    ;;                    (error 'wrong-type)))))
    ;;   ;; (format #t "tool arg to expand-run-tool must be string or symbol")))))
    ;;   (if (equal? tool "bash")
    ;;       'bash
    ;;       (if (string-prefix? "%{ocaml_where}/" tool)
    ;;           ;; e.g. %{ocaml_where}/expunge in <ocaml>toplevel/dune
    ;;           ;; assume basename (expunge) is a bazel target
    ;;           (expand-cmd-args pkg-path
    ;;                             target targets args filedeps vars)

    ;;           ;; else
    ;;           (expand-cmd-args
    ;;            pkg-path target targets args filedeps vars))))))

(define expand-string-arg
  (lambda (arg targets deps)
    (format #t "EXPAND-STRING-arg: ~A\n" arg)
    (cond
     ((string=? "%{deps}" arg)
      (let* ((kw (substring arg 6 (- (length arg) 1)))
             (keysym (string->keyword kw)))
        (format #t "kw ~A\n" kw)
        (format #t "keysym ~A\n" keysym)
        deps))

      ;; (concatenate
      ;;  filedeps
      ;;  ;;  ;; FIXME: create (:target . arg) pairs so
      ;;  ;;  ;; emitter will be able to distinguish
      ;;  ;;  ;; between e.g. string args and target args.
      ;;  (expand-cmd-args pkg-path
      ;;                   target targets
      ;;                   (cdr args) filedeps vars)))

     ((string=? "%{targets}" arg)
      (format #t "ARG TARGETS\n")
      (format #t " targets val: ~A\n" targets)
      `(:targets ,targets))
            ;; (expand-cmd-args pkg-path
            ;;                  target targets
            ;;                  (cdr args) filedeps vars)))

     ((string=? "%{target}" arg)
      (format #t "ARG TARGET\n")
      :TARGET)
      ;; (cons arg
      ;;       (expand-cmd-args pkg-path
      ;;                        target targets
      ;;                        (cdr args) filedeps vars)))

     ((string-prefix? "%{bin:" arg)
      (format #t "BIN: ~A\n" arg)
      (let* ((kw (substring arg 6 (- (length arg) 1)))
             (keysym (string->keyword kw)))
        (format #t "kw ~A\n" kw)
        (format #t "keysym ~A\n" keysym)
        `((:pkg :bin)
                (:tgt ,keysym))))
      ;; (cons
      ;;  (list :_bin arg)
      ;;  (expand-cmd-args pkg-path
      ;;                   target targets
      ;;                   (cdr args) filedeps vars)))

     ((string-prefix? "%{exe:" arg)
      (format #t "EXE: ~A\n" arg)
      (format #t "pkg-path: ~A\n" pkg-path)
      :EXE)
      ;; (let* ((sfx (substring arg 6 (- (length arg) 1)))
      ;;        (tool (string-trim '(#\}) sfx)))
      ;;   (cons
      ;;    (list :_exe "." tool)
      ;;    (expand-cmd-args pkg-path
      ;;                     target targets
      ;;                     (cdr args) filedeps vars))))

     ((string-prefix? "%{libexec:" arg)
      (format #t "LIBEXEC: ~A\n" arg)
      (let* ((kw (substring arg 9 (- (length arg) 1)))
             (keysym (string->keyword kw)))
        (format #t "kw ~A\n" kw)
        (format #t "keysym ~A\n" keysym)
        `((:pkg :libexec)
                (:tgt ,keysym))))
      ;; (cons
      ;;  (list :_libexec arg)
      ;;  (expand-cmd-args pkg-path
      ;;                   target targets
      ;;                   (cdr args) filedeps vars)))

     ((string-prefix? "%{lib:" arg)
      (format #t "LIB: ~A\n" arg)
      (format #t "pkg-path: ~A\n" pkg-path)
      :LIB)
      ;; (cons
      ;;  (list :_lib arg)
      ;;  (expand-cmd-args pkg-path
      ;;                   target targets
      ;;                   (cdr args) filedeps vars)))

     ((string-prefix? "%{dep:" arg)
      ;; "dep:<path> expands to <path>"
      ;; (cons `(:fixme ,arg)
      (format #t "DEP: var\n")
      (format #t "pkg-path: ~A\n" pkg-path)
      :DEP)
      ;; (let* ((dep-path (string-drop-right ;; drop '}'
      ;;                   (string-drop arg 6) 1))
      ;;        (segs (string-split dep-path #\/))
      ;;        (seg-ct (length segs))
      ;;        (resolved (let recur ((segs segs)
      ;;                              (path pkg-path))
      ;;                    (format #t "Recur: ~A; path: ~A\n"
      ;;                            segs path)
      ;;                    (if (null? segs)
      ;;                        path ;; should not happen?
      ;;                        (if (string=? ".." (car segs))
      ;;                            (let ((last-slash
      ;;                                   (string-index-right
      ;;                                    path (lambda (ch) (char=? ch #\/)))))
      ;;                              (if last-slash
      ;;                                  (recur (cdr segs)
      ;;                                         (string-drop-right
      ;;                                          path (- (length path) last-slash)))
      ;;                                  ;; no slash in pkg-path
      ;;                                  (recur (cdr segs) ".")))
      ;;                            ;; all leading '..' processed, no further recursion
      ;;                            ;; FIXME: what if '..' is embedded, e.g. a/../b?
      ;;                            ;; segs may still contain multiple segs;
      ;;                            ;; we need all but the last to be added to the path
      ;;                            ;; so we can form a label of form //a/b/c:x
      ;;                            (if (> (length segs) 1)
      ;;                                (begin
      ;;                                  ;; e.g. path: runtime/caml, segs (".." ".." "tools" "make-version-header.sh");
      ;;                                  (let ((fpath (string-append path "/"
      ;;                                                              (string-join segs "/"))))
      ;;                                    (if (file-exists? fpath)
      ;;                                        (let* ((path-segs (reverse (cdr (reverse segs))))
      ;;                                               (segpath (string-join path-segs "/"))
      ;;                                               (last (car (reverse segs)))
      ;;                                               (p (if (equal? "." path)
      ;;                                                      segpath
      ;;                                                      (string-append path "/" segpath))))
      ;;                                          (list :_srcfile p last)
      ;;                                          (list :_genfile p last)))))
      ;;                                ;; one seg left
      ;;                                (let ((fpath (string-append path "/" (car segs))))
      ;;                                  (if (file-exists? fpath)
      ;;                                      (list :_srcfile path (car segs))
      ;;                                      ;; (string-append "$(location //" fpath ")") ;; (list :_srcfile fpath)
      ;;                                      (list :_genfile path (car segs))
      ;;                                      ;; (string-append "$(location //" fpath ")") ;; (list :_genfile fpath)
      ;;                                      )))
      ;;                            )))))
      ;;   (format #t "Resolved dep path: ~A\n" resolved)
      ;;   (cons resolved
      ;;         (expand-cmd-args pkg-path
      ;;                          target targets
      ;;                          (cdr args)
      ;;                          filedeps vars))))
     ;; (run-recur (cdr cmd-args)
     ;;            ;; run-deps ;; (cons arg run-deps)
     ;;            (cons arg run-args))))

     ((string-prefix? "%{" arg)
      (format #t "VAR: ~A\n" arg)
      (format #t "deps: ~A\n" deps)
      (let* ((kw (substring arg 2 (- (length arg) 1)))
             (keysym (string->keyword
                      (string-append ":" kw))))
        (format #t "kw ~A\n" kw)
        (format #t "keysym ~A\n" keysym)
        (if-let ((val (assoc keysym deps)))
                (begin
                  (format #t "VAR VAL: ~A\n" val)
                  ;; (cons val
                  ;;       (expand-cmd-args (cdr args)
                  ;;                        targets deps)))
                  val)
                ;; not a dep, try installed execs
                (cons
                 :VAR val))))
     ;; (expand-cmd-args pkg-path
     ;;                  target targets
     ;;                  (cdr args)
     ;;                  filedeps vars)))))

     (else ;; give arg is string
      (format #t "OTHER STRING: ~A\n" arg)
      arg)
      ;; (cons
      ;;  arg
      ;;  ;;(resolve-string-arg pkg-path arg vars)
      ;;  (expand-cmd-args (cdr args) targets deps))))))
     )))

(define expand-cmd-args
  (lambda (args targets deps)
  ;; (lambda (pkg-path target targets args filedeps vars)
  ;; (lambda (pkg-path target targets args filedeps vars)
    (format #t "EXPAND-CMD-ARGS: ~A\n" args)

    (let ((result
           (if (null? args)
               '()

               ;; (let ((arg (if (symbol? (car args))
               ;;                (symbol->string (car args))
               ;;                (car args))))
               (let ((arg (car args)))
                 (cond
                  ((pair? arg) ;; e.g. (:_string "struct")
                   (let ((subarg (expand-cmd-args (car args) targets deps)))
                     (cons subarg
                           (expand-cmd-args (cdr args) targets deps))))
                   ;; (expand-cmd-args pkg-path
                         ;;                  target targets
                         ;;                  (cdr args) filedeps vars)))

                  ;; ((number? arg)
                  ;;  (cons arg
                  ;;        (expand-cmd-args pkg-path
                  ;;                         target targets
                  ;;                         (cdr args) filedeps vars)))

                  ((symbol? arg)
                   (begin
                     (format #t "ARGSYM: ~A\n" arg)
                     (cond
                      ((eq? '%{deps} arg)
                       (cons deps
                             (expand-cmd-args (cdr args) targets deps)))
                      ((eq? '%{workspace_root} arg)
                       (cons :WS-ROOT
                             (expand-cmd-args (cdr args) targets deps)))
                      ((eq? '%{targets} arg)
                       (cons targets
                             (expand-cmd-args (cdr args) targets deps)))
                      ((eq? '%{target} arg)
                       (cons (cons :target (cdar targets))
                             (expand-cmd-args (cdr args) targets deps)))
                      (else
                       (let ((sarg (expand-string-arg (symbol->string arg)
                                                      targets deps)))
                         (cons sarg
                               (expand-cmd-args (cdr args) targets deps)))))))

                  ((string? arg)
                   (let ((sarg (expand-string-arg arg targets deps)))
                     (cons sarg
                           (expand-cmd-args (cdr args) targets deps))))
                            ;; pkg-path target targets
                            ;; (cdr args) filedeps vars)))))

                  (else ; not number, pair, string
                   (format #t
                           "WARNING: not a nbr, pair, or string: ~A\n" arg)
                   ))
                 ))))
      ;; (format #t "resolved: ~A\n" result)
      result)))

(define expand-targets
  (lambda (paths targets deps)
    (format #t "expand-targets ~A\n" targets)
    (let ((xtargets (expand-deps targets paths '())))
      (format #t "expanded targets ~A\n" xtargets)
      xtargets)))

;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions
(define dune-dsl-cmds
  ;; all except 'run' and 'dynamic-run'
  '(bash cat chdir cmp copy copy# diff diff? echo ignore-outputs-to
ignore-stderr-to ignore-stdout-to no-infer pipe-outputs pipe-stderr
pipe-stdout progn setenv system with-accepted-exit-codes
with-outputs-to with-stderr-to with-stdin-from with-stdout-to
write-file))

(define (expand-cmd-list -raw-cmds targets deps)
  (format #t "EXPAND-CMD-LIST: ~A\n" -raw-cmds)

  (let recur ((raw-cmds -raw-cmds)
              (tool #f)
              (expanded-cmds '())
              (args '()))
    (format #t " cmd-list RECUR: ~A\n" raw-cmds)
    (format #t "    expanded: ~A\n" expanded-cmds)
    (format #t "    tool: ~A\n" tool)
    (if (null? raw-cmds)
        expanded-cmds

        (if (pair? (car raw-cmds))
            (let* ((initial (recur (car raw-cmds) tool expanded-cmds args)))
              (recur (cdr raw-cmds)
                     tool
                     (append expanded-cmds initial) args))

            ;; car is atom
            (let* ((kw (car raw-cmds)))
              (if (member kw dune-dsl-cmds)
                  `(((:tool ,kw)
                     ,(cons :args
                             (expand-cmd-args (cdr raw-cmds) targets deps))))

                  (if (equal? kw 'run)
                      ;; skip 'run'
                      (recur (cdr raw-cmds) tool expanded-cmds
                             (expand-cmd-args args targets deps))

                      ;; atom or list following 'run'
                      (if (pair? kw)
                          ;; e.g. (run (cmd ...))
                          (recur kw tool expanded-cmds
                                   (expand-cmd-args args targets deps))

                          ;; e.g. (run %{bin:foo} ...)
                          (list
                           (list (cons :tool
                                       (car (expand-run-tool kw targets deps)))
                                (cons :args
                                       (expand-cmd-args (cdr raw-cmds)
                                                        targets deps))))))))))))
                  ;; ;; else must be an arg
                  ;; (expand-cmd-args (cdr raw-cmds) targets deps)))

             ;; ((equal? 'run (car raw-cmds))
             ;;  (format #t "RUN ~A\n" raw-cmds)
             ;;  (recur (cdr raw-cmds)
             ;;         (append expanded-cmds initial)
             ;;         targets deps))
             ;; (else ;; car not chdir, not run
             ;;  (error 'wtf2 "WTF2? ~A" (car raw-cmds)))
             ;; ))))

(display "loaded dune/expanders.scm\n")
