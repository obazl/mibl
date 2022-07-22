;; (display "loading dune/expanders.scm\n")

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
  (lambda (tool pkg targets deps)
    (format #t "~A: ~A (~A)~%" (blue "expand-run-tool") tool (type-of tool))
    (format #t "~A: ~A~%" (green "deps") deps)
    ;; at this point we do not have enough info to decide if this tool
    ;; is a filename literal, produced by this pkg, or by another proj
    ;; pkg, or by and import. So we punt and leave it up to the
    ;; resolver to decide later.
    (case tool
      ((cp) :copy)
      (else
       (let ((tool (format #f "~A" tool)))
         (cond
          ((string-prefix? "%{" tool) (-expand-pct-tool tool))
          (else (-expand-literal-tool!?
                 (car (assoc-val :pkg-path pkg))
                 tool deps))))))))

;; (let ((xtool (handle-filename-literal-dep
;;               tool deps pkg '())))
;;   xtool)))

;; (let ((x (expand-cmd-args (list tool) targets deps)))
;;   x)))

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

(define (-expand-pct-tool arg)
  (format #t "~A: ~A~%" (blue "-expand-pct-tool") arg)
  (if (string-suffix? "}" arg)
      (let* ((kw (substring arg 2 (- (length arg) 1)))
             (_ (format #t "kw ~A\n" kw))
             (keysym (string->keyword kw)))
        (format #t "keysym ~A\n" keysym)
        keysym)
      ;; else %{foo}.bar
      (let* ((bname (bname arg))
             (_ (format #t "bname: ~A~%" bname))
             (ext (filename-extension arg))
             (_ (format #t "ext: ~A~%" ext))
             (kw (substring bname 2 (- (length bname) 1)))
             (_ (format #t "kw ~A\n" kw))
             (keysym (if ext
                         (string->keyword (string-append ":" kw ext))
                         (string->keyword (string-append ":" kw)))))
        (format #t "keysym ~A\n" keysym)
        ;; resolver will do lookup
        keysym
        )))

;; may update deps
(define (-expand-literal-tool!? pkg-path tool deps)
  (format #t "~A: ~A (~A)~%" (blue "-expand-literal-tool!?")
          tool (type-of tool))
  (format #t "deps: ~A~%" deps)

  (let ((tool (if (string-prefix? "./" tool)
                  (string-drop tool 2) tool)))
    (if (null? (cdr deps))
        ;; infer dep for tool
        (let ((tool-kw (string->keyword (format #f "~A" tool))))
          (set-cdr! deps
                    (list (list tool-kw (cons :pkg pkg-path)
                                (cons :tgt tool))))
          tool-kw)
        ;; else
        (let ((tool (format #f "~A" tool))
              (t
               (find-if (lambda (dep)
                          (format #t "~A: ~A~%" (green "dep") dep)
                          (let* ((pkg-tgt (cdr dep))
                                 (tgt (if-let ((tgt (assoc-val :tgt pkg-tgt)))
                                              (equal? tool tgt)
                                              (if-let ((tgts
                                                        (assoc-val :tgts pkg-tgt)))
                                                      (equal? tool tgts)
                                                      (error 'fixme "missing :tgt and :tgts in dep")))))
                            ;;(dep-cadr (format #f "~A" (cadr dep))))
                            ;; (format #t "~A: ~A (~A)~%" (red "dep-cadr")
                            ;;         dep-cadr (type-of dep-cadr))
                            ;; (format #t "~A: ~A~%" (red "teq?")
                            ;;         (eq? (type-of tool) (type-of dep-cadr)))
                            ;; (format #t "~A: ~A~%" (red "veq?")
                            ;;         (equal? tool dep-cadr))
                            ;; (if (eq? (type-of tool) (type-of dep-cadr))
                            ;;     (equal? tool dep-cadr)
                            ;;     #f)
                            tgt))
                        (cdr deps))))
          (format #t "~A: ~A~%" (red "T") t)
          (if t (car t) tool))
        )))

;; updates deps
(define (-infer-dep! arg deps pkg)
  (format #t "~A: ~A (~A)~%" (blue "-infer-dep") arg (type-of arg))
  (format #t "~A: ~A~%" (red "deps") deps)
  (format #t "~A: ~A~%" (red ":signatures") (assoc-val :signatures pkg))
  (format #t "~A: ~A~%" (red ":structures") (assoc-val :structures pkg))
  (format #t "~A: ~A~%" (red ":modules") (assoc-val :modules pkg))
  ;; if extension == .ml search structs then modules
  ;; if extension == .mli search sigs
  ;; else search :files, :scripts, ...

  (cond
   ((fnmatch "*.ml" arg 0)
    (let ((struct (find-if (lambda (f-assoc)
                               ;; (format #t "~A: ~A~%" (white "f-assoc")
                               ;;         f-assoc)
                               (find-if (lambda (f)
                                          ;; (format #t "~A: ~A (~A)~%" (white "f") (cdr f) (type-of (cdr f)))
                                          (equal? (format #f "~A" arg)
                                                  (format #f "~A" (cdr f))))
                                        (cdr f-assoc)))
                             (if-let ((structs (assoc-val :structures pkg)))
                                     structs
                                     '()))))

        (format #t "~A: ~A~%" (white "struct") struct)
        (if struct
            (let ((key (string->keyword arg)))
              (format #t "~A: ~A~%" (magenta "INFERRED DEP") struct)
              (set-cdr! deps (cons (cons key
                                         (list (cons :pkg (assoc-val :pkg-path pkg))
                                               (cons :tgt arg)))
                                   (cdr deps)))
              key)
            ;; else search :modules
            (let ((struct (find-if (lambda (m-assoc)
                                     (format #t "~A: ~A~%" (white "m-assoc") m-assoc)
                                     (find-if (lambda (m)
                                                (format #t "~A: ~A (~A)~%" (white "m") (cdr m) (type-of (cdr m)))
                                                (let ((ml (cdr m)))
                                                  (format #t "~A: ~A~%" (white "(cdr m)") ml)
                                                  (equal? (format #f "~A" arg)
                                                          (format #f "~A" ml))))
                                        (cdr m-assoc)))
                             (if-let ((modules (assoc-val :modules pkg)))
                                     modules
                                     '()))))
              (if struct
                  (let ((key (string->keyword arg)))
                    (format #t "~A: ~A~%" (magenta "INFERRED DEP") struct)
                    (set-cdr! deps (cons (cons key
                                               (list (cons :pkg (assoc-val :pkg-path pkg))
                                                     (cons :tgt arg)))
                                         (cdr deps)))
                    key)
                  ;; else no matching file
                  ;; PROBLEM: distinguish between string args and labels of targets in other pkgs
                  ;; solution: second pass, to check exports table
                  (begin
                    (format #t "~A: ~A~%" (red "NO MATCHING FILE") struct)
                    ;;(string->keyword arg)
                    arg))))))

   ((fnmatch "*.mli" arg 0)
    (error 'fixme "not yet implemented"))

   (else
    (error 'fixme "not yet implemented"))))

;; string args may include suffixed vars, e.g. %{test}.corrected
(define expand-string-arg
  (lambda (arg pkg targets deps)
    (format #t "~A: ~A\n" (blue "expand-string-arg") arg)
    (format #t "  targets: ~A\n" targets)
    (format #t "  deps: ~A\n" deps)

    (cond
     ((string=? "./" arg) ::pkg-dir)

     ((string=? "%{deps}" arg)
      (let* ((kw (substring arg 6 (- (length arg) 1)))
             (keysym (string->keyword kw)))
        ;; (format #t "kw ~A\n" kw)
        ;; (format #t "keysym ~A\n" keysym)
        deps))

     ((string=? "%{targets}" arg)
      ;; (format #t "ARG TARGETS\n")
      ;; (format #t " targets val: ~A\n" targets)
      `(:outputs ,targets))

     ((string=? "%{target}" arg)
      ;; (format #t "ARG TARGET\n")
      :TARGET)

     ;; pass std %{<kind>:foo} as ::kind:foo, let resolver handle
     ;; by looking them up in the exports table
     ((string-prefix? "%{" arg)
      ;; %{foo} or %{foo}.suffix
      (format #t "VAR: ~A\n" arg)
      (format #t "deps: ~A\n" deps)
      (-expand-pct-tool arg))

     (else ;; arg is string
      ;; search pkg files for inferred input dep
      (if-let ((inferred-dep (-infer-dep! arg deps pkg)))
              inferred-dep
              arg))
      ;; (cons
      ;;  arg
      ;;  ;;(resolve-string-arg pkg-path arg vars)
      ;;  (expand-cmd-args (cdr args) targets deps))))))
     )))

(define expand-cmd-args
  (lambda (args pkg targets deps)
    (format #t "~A: ~A\n" (blue "expand-cmd-args") args)
    (format #t "  targets: ~A\n" targets)
    (format #t "  deps: ~A\n" deps)

    (let ((result
           (if (null? args)
               (begin
                 (format #t "~A, returning: ~A~%" "bottomed out" '())
                 '())

               ;; (let ((arg (if (symbol? (car args))
               ;;                (symbol->string (car args))
               ;;                (car args))))
               (let ((arg (car args)))
                 (cond
                  ((pair? arg) ;; e.g. (:_string "struct")
                   (let ((subarg (expand-cmd-args (car args) pkg targets deps)))
                     (cons subarg
                           (expand-cmd-args (cdr args) pkg targets deps))))
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
                     ;; (format #t "ARGSYM: ~A\n" arg)
                     (cond
                      ((eq? '%{deps} arg)
                       (let ((exp (expand-cmd-args (cdr args) pkg targets deps))
                             ;; (xdeps (-deps->srcs-attr "fake" deps))
                             )
                         (append '(::deps) exp)))
                      ((eq? '%{workspace_root} arg)
                       (cons :WS-ROOT
                             (expand-cmd-args (cdr args) pkg targets deps)))

                      ((eq? '%{targets} arg)
                       ;; (format #t "~A: ~A~%" (green "%{targets}") targets)
                       ;; (cons targets
                       ;;       (expand-cmd-args (cdr args) pkg targets deps))
                       (cons :outputs
                             (expand-cmd-args (cdr args) pkg targets deps)))

                      ((eq? '%{target} arg)
                       (cons (cons :target (cdar targets))
                             (expand-cmd-args (cdr args) pkg targets deps)))
                      (else
                       (let ((sarg (expand-string-arg (symbol->string arg)
                                                      pkg targets deps)))
                         (cons sarg
                               (expand-cmd-args (cdr args) pkg targets deps)))))))

                  ((string? arg)
                   (let ((sarg (expand-string-arg arg pkg targets deps)))
                     (append (list sarg)
                           (expand-cmd-args (cdr args) pkg targets deps))))
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
  (lambda (ws paths targets deps)
    (format #t "~A: ~A\n" (blue "expand-targets") targets)
    (let ((xtargets (expand-deps ws targets paths '())))
      (format #t "Expanded targets ~A\n" xtargets)
      xtargets)))

(define (expand-deps ws deplist paths expanded-deps)
  (format #t "~A: ~A\n" (blue "expand-deps") deplist)
  (format #t "paths: ~A\n" paths)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; (let ((pkg-path (car (assoc-val :pkg-path paths)))
  ;;       (ws-root (car (assoc-val :ws-path paths))))
  (if (null? deplist)
      (begin
        ;; (format #t "finished deplist: ~A\n" expanded-deps)
        expanded-deps)
      (if (pair? (car deplist))
          (expand-deps ws (car deplist)
                          paths ;; stanza-alist
                          (expand-deps ws
                           (cdr deplist) paths expanded-deps))
          ;; car is atom
          (let* ((kw (car deplist)))
            (if-let ((depfn (assoc-val kw dune-dep-handlers)))
                    (let ((res (apply (car depfn) (list paths
                                                        deplist))))
                      ;; (format #t "depfn res: ~A\n" res)
                      ;; (format #t "expanded-deps: ~A\n" expanded-deps)
                      ;; we're done, depfn consumed cdr

                      ;; FIXME: merge all lists with :_ key
                      ;; we get one such list for each glob_files
                      ;; (if (pair? (car res)) ;; e.g. ((:_ "foo/bar/..."))
                          ;; (if (equal? :_ (caar res))
                          ;;     (format #t "TO MERGE ~A\n" res))
                      (append (if (pair? (car res)) res (list res))
                              expanded-deps))

                    ;; else car of deplist not a keyword
                    ;; must be either a ':' tagged dep or a filename literal
                    ;; or, if its a cmd arg, may be %{foo}
                    (let ((dep (format #f "~A" (car deplist))))
                      (cond
                       ((char=? #\: (string-ref dep 0))
                          (begin
                            ;; (format #t "TAGGED DEP : ~A\n" deplist)
                            (handle-tagged-dep
                             ws deplist paths expanded-deps)))

                       ;; ((char=? #\% (string-ref dep 0))
                       ;;  )

                       (else
                          ;; must be a filename literal?
                          ;; return (:static <path> <fname>)
                          ;; or (:dynamic <path> <fname>)
                          (begin
                            ;; (format #t "LIT DEP : ~A\n" deplist)
                            (handle-filename-literal-dep
                             ws dep deplist paths
                             ;; stanza-alist
                             expanded-deps))))))
            ))))

;; expand-deps: deps -> file-deps, vars, env-vars
(define (expand-rule-deps ws paths stanza-alist)
  ;; updates stanza-alist
  (format #t "~A: ~A\n" (blue "expand-rule-deps") stanza-alist)
  ;; (let ((stanza-alist (cdr stanza)))
  (if-let ((deps-assoc (assoc 'deps stanza-alist)))
          (let* ((deplist (assoc-val 'deps stanza-alist))
                 (_ (format #t "main deplist: ~A\n" deplist))
                 (result (expand-deps ws deplist paths '())))
            (format #t "DEPLIST EXPANDED: ~A\n" result)
            (cons :deps result))
          #f))

(define (expand-cmd-list pkg -raw-cmds targets deps)
  (format #t "~A: ~A\n" (blue "expand-cmd-list") -raw-cmds)
  (format #t "~A: ~A~%" (green "deps") deps)

  (let recur ((raw-cmds -raw-cmds)
              (tool #f)
              (expanded-cmds '())
              (args '()))
    ;; (format #t " cmd-list RECUR: ~A\n" raw-cmds)
    ;; (format #t "    expanded: ~A\n" expanded-cmds)
    ;; (format #t "    tool: ~A\n" tool)
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
                  `(((:Tool (string->symbol
                             (format #f ":~A" ,kw)))
                     (:args
                      ,@(expand-cmd-args (cdr raw-cmds) pkg targets deps))))

                  (if (equal? kw 'run)
                      ;; skip 'run'
                      (recur (cdr raw-cmds) tool expanded-cmds
                             (expand-cmd-args args pkg targets deps))

                      ;; atom or list following 'run'
                      (if (pair? kw)
                          ;; e.g. (run (cmd ...))
                          (recur kw tool expanded-cmds
                                   (expand-cmd-args args pkg targets deps))

                          ;; kw in in list of dune std tools, must be
                          ;; (run %{bin:foo} ...), (run ../foo/bar.exe) etc.
                          (list
                           (list (list :tool
                                       (expand-run-tool kw pkg targets deps))
                                `(:args
                                  ,@(expand-cmd-args (cdr raw-cmds)
                                                     pkg targets deps))))))))))))
                  ;; ;; else must be an arg
                  ;; (expand-cmd-args (cdr raw-cmds) pkg targets deps)))

             ;; ((equal? 'run (car raw-cmds))
             ;;  (format #t "RUN ~A\n" raw-cmds)
             ;;  (recur (cdr raw-cmds)
             ;;         (append expanded-cmds initial)
             ;;         targets deps))
             ;; (else ;; car not chdir, not run
             ;;  (error 'wtf2 "WTF2? ~A" (car raw-cmds)))
             ;; ))))

;; (display "loaded dune/expanders.scm\n")
