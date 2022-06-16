;; (display "dune/dune_actions.scm loading...") (newline)

(load "dune/dune_action_run.scm")

;; https://dune.readthedocs.io/en/stable/concepts.html?highlight=progn#user-actions

;; available actions:

;; (run <prog> <args>) <prog> is resolved locally if it is available
;; in the current workspace, otherwise it is resolved using the PATH

;; (dynamic-run <prog> <args>) to execute a program that was linked against dune-action-plugin library. <prog> is resolved in the same way as in run

;; (chdir <dir> <DSL>) to change the current directory

;; (setenv <var> <value> <DSL>) to set an environment variable

;; (with-<outputs>-to <file> <DSL>) to redirect the output to a file, where <outputs> is one of: stdout, stderr or outputs (for both stdout and stderr)

;; (ignore-<outputs> <DSL>) to ignore the output, where <outputs> is one of: stdout, stderr or outputs

;; (with-stdin-from <file> <DSL>) to redirect the input from a file

;; (with-accepted-exit-codes <pred> <DSL>) specifies the list of expected exit codes for the programs executed in <DSL>. <pred> is a predicate on integer values, and is specified using the Predicate language. <DSL> can only contain nested occurrences of run, bash, system, chdir, setenv, ignore-<outputs>, with-stdin-from and with-<outputs>-to. This action is available since dune 2.0.

;; (progn <DSL>...) to execute several commands in sequence

;; (echo <string>) to output a string on stdout

;; (write-file <file> <string>) writes <string> to <file>
;;    Bazel: genrule, or custom rule using ctx.actions.write?
;;    to the extent that writing src to a file is common,
;;    an obazl rule might be in order,
;;    e.g. ocaml_string_template or some such.
;;    for now, custom tezos rules: tezos_template, tezos_file, etc.
;;    or just a simple genrule
;;    see tezos-protocol-compiler/dune_protocol.template

;; e.g. (rule (alias buildtest) (deps test_clic.exe) (action (progn)))

;; e.g.
;; (rule
;;  (targets environment.ml)
;;  (action
;;   (write-file %{targets}
;;               "... string content ...
;; ")))
;; => tezos_file(name = "envfile", out = "environment.ml",
;;               content = """...""")
;; or
;; tezos_template(name = "envfile" out = "environment.ml"
;;                template = "filename.tpl", data = "...")

;; (cat <file>) to print the contents of a file to stdout

;; (copy <src> <dst>) to copy a file

;; (copy# <src> <dst>) to copy a file and add a line directive at the beginning

;; (system <cmd>) to execute a command using the system shell: sh on Unix and cmd on Windows

;; (bash <cmd>) to execute a command using /bin/bash. This is obviously not very portable

;; (diff <file1> <file2>) is similar to (run diff <file1> <file2>) but is better and allows promotion. See Diffing and promotion for more details

;; (diff? <file1> <file2>) is similar to (diff <file1> <file2>) except that <file2> should be produced by a part of the same action rather than be a dependency, is optional and will be consumed by diff?.

;; (cmp <file1> <file2>) is similar to (run cmp <file1> <file2>) but allows promotion. See Diffing and promotion for more details

;; (no-infer <DSL>) to perform an action without inference of dependencies and targets. This is useful if you are generating dependencies in a way that Dune doesn’t know about, for instance by calling an external build system.

;; (pipe-<outputs> <DSL> <DSL> <DSL>...) to execute several actions (at least two) in sequence, filtering the <outputs> of the first command through the other command, piping the standard output of each one into the input of the next. This action is available since dune 2.7.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples from tezos:

;; (action (progn))  ;; in many places


;; src/proto_000_Ps9mPmXa/lib_protocol/dune:
;; (action
;;  (run
;;   %{libexec:tezos-protocol-compiler:replace}
;;   %{libexec:tezos-protocol-compiler:dune_protocol.template}
;;   "dune.inc.gen"
;;   %{libexec:tezos-protocol-compiler:final_protocol_versions}))
;; ...
;; (action (diff dune.inc dune.inc.gen))

;; src/proto_000_Ps9mPmXa/lib_protocol/dune.inc:
;; (action
;;   (write-file %{targets}
;;               "module Name = struct let name = \"000-Ps9mPmXa\" end
;; include Tezos_protocol_environment.MakeV0(Name)()
;; module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
;; "))
;; ...
;; (action
;;  (with-stdout-to %{targets}
;;    (chdir %{workspace_root}
;;       (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "000_Ps9mPmXa"))))


;; src/proto_003_PsddFKi3/lib_protocol/dune.inc
;; (action (with-stdout-to %{targets}
;;            (chdir %{workspace_root}
;;              (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

 ;; (action (run %{bin:tezos-protocol-compiler}  ./))

;; src/bin_proxy_server/dune
;;  (action (run %{lib:tezos-tooling:lint.sh} %{deps}))

;; src/bind_codec:
;; (rule
;;  (action
;;   (progn
;;    (write-file void_for_linking-alpha.empty "")
;;    (write-file void_for_linking-005-PsBabyM1.empty "")
;;    (write-file void_for_linking-006-PsCARTHA.empty "")
;;    (write-file void_for_linking-007-PsDELPH1.empty "")
;;    (write-file void_for_linking-008-PtEdo2Zk.empty "")
;;    (write-file void_for_linking-009-PsFLoren.empty "")
;;    (write-file void_for_linking-010-PtGRANAD.empty "")
;; )))

(define (normalize-write-file action stanza) ;; action target targets deps)
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

;; OBSOLETE: uses hack to deal with '../'
;; returns (:_srcfile <path> <fname>) or (:_genfile <path> <fname>)
(define (make-filedep-arg pkg-path dep tag vars)
 ;; (if (equal? dep "%{ocamlc}")
  ;;     (begin
  (format #t "make-filedep-arg dep: ~A, tag: ~A\n" dep tag)
  (format #t "make-filedep-arg vars: ~A\n" vars)
  ;;))

  ;; (let* ((dep-str (if (symbol? dep) (symbol->string dep) dep))
  ;;        (fname (string-append pkg-path "/" dep-str)))

  (let* ((dep-str (if (symbol? dep)
                      (symbol->string dep)
                      (if (pair? dep)
                          (if (equal? 'file (car dep))
                              (cadr dep)
                              ;; ???
                              )
                          ;; ??
                          )))
         (dep-str (if (symbol? dep-str) (symbol->string dep-str)
                      dep-str))
         (_ (format #t "dep-str ~A\n" dep-str))
         (segs (string-split dep-str #\/))
         (seg-ct (length segs)))
    (let recur ((segs segs)
                (path pkg-path) ;; so that we can drop '..', '../..' etc
                (file ""))
      (format #t "segs: ~A\n" segs)
      (format #t "path: ~A\n" path)
      (if (null? segs)
          (let* ((fname (if (string=? "" path) path (string-append path "/" file)))
                 (_ (format #t "exists? ~A: ~A\n"
                            fname (file-exists? fname)))
                 (result (let ((triple (list (if (file-exists? fname)
                                                 :_srcfile :_genfile)
                                             path
                                             file)))
                           (if tag (list tag triple) triple))))
            (format #t "fdep: ~A\n" result)
            result)
          ;; segs remaining
          (cond
           ((string=? (car segs) ".") ;; (format #t "DOT seg")
            (recur (cdr segs) path file))

           ((string=? (car segs)"..") ;; (format #t "DOTDOT seg")
            (format #t "backing up one level\n")
            (let ((last-slash (string-index-right path
                                                  (lambda (ch)
                                                    (char=? ch #\/)))))
              (if last-slash
                  (recur (cdr segs)
                         (string-drop-right
                          path (- (length path) last-slash))
                         file)
                  ;; no slash in path
                  (recur (cdr segs)
                         "" ;; "."
                         file))))

           ((string-prefix? "%{" (car segs))
            (format #t "filedep VAR: ~A\n" (car segs))
            ;; (if (equal? dep "%{ocamlc}")
            ;;     (format #t "var ref: ~A\n" (car segs)))
            (if (string-index (car segs)
                              (lambda (ch) (char=? #\: ch)))
                (recur (cdr segs) path (car segs))
                (let* ((tag
                        (string->symbol
                         (string-append
                          ":"
                          (substring (car segs)
                                     2 (- (length (car segs)) 1)))))
                       (_ (format #t "Tag: ~A\n" tag))
                       (var-pair (if-let ((v (assoc tag vars)))
                                         (cdr v)
                                         (if-let ((vv (stdlib-tbl tag)))
                                                 (list vv "")
                                                 (if-let ((vv
                                                           (executables-tbl tag)))
                                                         (list vv "")
                                                         #f))))
                       ;; (_ (format #t "var-pair: ~A\n" var-pair))
                       (var-val (if var-pair
                                    (string-append (car var-pair)
                                                   ":"
                                                   (cadr var-pair)))))
                  ;; (format #t "var-val: ~A\n" var-val)
                  (recur (cdr segs) (car var-pair) (cadr var-pair)))))

            (else
             ;; (format #t "xxxx (car segs): ~A (~A)\n"
             ;;         (car segs) (type-of (car segs)))
             (if (null? (cdr segs)) ;; last seg?
                 (recur (cdr segs) path (car segs))
                        ;; (string-append path ":" (car segs)))
                 (recur (cdr segs)
                        (if (string=? "" path)
                            (car segs)
                            (string-append path "/" (car segs)))
                        file)))))
             )))

;; ACTION DEPS
;; https://dune.readthedocs.io/en/stable/concepts.html#dependency-specification
;; examples:
;; (deps (glob_files *.ml{,i}))
;; (deps (universe) (:script get-git-info.mlt))
;; (deps filea fileb ...)
;; file literals may contain '..'
;; dependency on dune-produced artifact:
;; (deps .tezos_protocol_004_Pt24m4xi.objs/native/tezos_protocol_004_Pt24m4xi.cmx)
;; (deps (glob_files contracts/*))
;; (deps (alias foo) (alias bar)

;; labelled deps - the labels can be used in 'action' fld,
;; e.g. (run ${gen} ${targets})
;; ':<' often used like this:
;; (rule
;;   (targets foo bar)
;;   (deps (:< gen.sh) (universe))
;;   (action (run %{<} %{targets})))

;; mix labelled and literals
;; (deps (:exe gen/bip39_generator.exe) gen/bip39_english.txt)

;; globbing
;; (deps
;;   index.html
;;   (:css (glob_files *.css))
;;   (:js foo.js bar.js)
;;   (:img (glob_files *.png) (glob_files *.jpg)))

;; car of deps list is either one of these, or a filename, or a kw
;; label (e.g. :exe, :<, etc.)
;; called recursively
(define (expand-deplist deplist pkg stanza expanded-deps)
  (format #t "expand-DEPLIST: ~A\n" deplist)
  (format #t "pkg: ~A\n" pkg)
  (let ((pkg-path (car (assoc-val :pkg-path pkg)))
        (ws-root (car (assoc-val :ws-path pkg))))
    (if (null? deplist)
        (begin
          (format #t "finished deplist: ~A\n" expanded-deps)
          expanded-deps)
        (if (pair? (car deplist))
            (expand-deplist (car deplist)
                            pkg stanza
                            (expand-deplist
                             (cdr deplist) pkg stanza expanded-deps))
            ;; car is atom
            (let* ((kw (car deplist)))
              (if-let ((depfn (assoc-val kw dune-dep-handlers)))
                      (let ((res (apply (car depfn) (list pkg
                                                          deplist))))
                        (format #t "res: ~A\n" res)
                        (format #t "expanded-deps: ~A\n" expanded-deps)
                        ;; we're done, depfn consumed cdr
                        (append expanded-deps res))

                      ;; else car of deplist not a keyword
                      ;; must be either a ':' named dep or a filename literal
                      (let ((dep (if (symbol? (car deplist))
                                     (symbol->string (car deplist))
                                     (car deplist))))
                        (if (char=? #\: (string-ref dep 0))
                            (begin
                              (format #t "NAMED DEP : ~A\n" deplist)
                              deplist)

                            ;; else must be a filename literal
                            ;; return (:static <path> <fname>)
                            ;; or (:dynamic <path> <fname>)
                            (let* ((_ (format #t "dep: ~A\n" dep))
                                   (path (string-append pkg-path
                                                        "/" dep))
                                   (_ (format #t "path: ~A\n" path))
                                   (kind (if (file-exists? path)
                                             :static :dynamic))
                                   (rp (if (eq? kind :static)
                                           (resolve-pkg-path path ws-root)
                                           (normalize-path path))))
                              (format #t "FILENAME LITERAL : ~A\n" dep)
                              (format #t "rp : ~A\n" rp)
                              (format #t "kind : ~A\n" kind)
                              ;; find it and resolve pkg path
                              ;; if not found mark it as :dynamic
                              (expand-deplist (cdr deplist)
                                              pkg stanza
                                              (cons (list kind rp)
                                                    expanded-deps)))
                            ;; (let ((dep (make-filedep-arg pkg-path
                            ;;                            (cadar deps)
                            ;;                            (caar deps) '())))
                            ;;   )
                            )))
              )))))

;; expand-deps: deps -> file-deps, vars, env-vars
(define (expand-action-deps pkg stanza)
  (format #t "expand-action-DEPS: ~A\n" stanza)
  (let ((stanza-alist (cdr stanza)))
    (let ((deplist (assoc-val 'deps stanza-alist)))
      (format #t "main deplist: ~A\n" deplist)
      (expand-deplist deplist pkg stanza '()))))

(define (Xexpand-action-deps pkg-path tool-tag tool deps-assoc srcfiles)
  ;; NB: tool not used; tool-tag used only for var defs

  (format #t "EXPAND-DEPS ~A\n" pkg-path)
  (format #t " x tool: ~A\n" tool)
  (format #t " x tool-tag: ~A\n" tool-tag)
  (format #t " x deps ~A\n" deps-assoc)

  ;; (format #t "expand-deps tool: ~A (type: ~A)\n" tool (type-of tool))
  ;; extract:
  ;; 1. file deps: '(file ..), <filename>, '(glob_files ...)
  ;; 2. dep vars:  '(:name deps)
  ;; 3. '(universe) ...
  ;; 4. '(env_var ...)
  ;; 5. aliases - resolve and include with files?

  (if deps-assoc
      (let recur ((deps (cdr deps-assoc))
                  (filedeps '())
                  (vars '())
                  (env-vars '())
                  (universe #f)
                  (aliases '())
                  (unresolved '()))
        (format #t "RECUR at ~A\n" (if (null? deps) '() (car deps)))
        (if (null? deps)
            (values filedeps vars env-vars universe aliases unresolved)
            (if (pair? (car deps))
                (cond
                 ((equal? (caar deps) 'alias)
                  (begin (format #t "ALIAS\n")
                       (recur (cdr deps)
                              (concatenate
                               `(:_alias ,(cadar deps)) filedeps)
                               vars env-vars universe aliases unresolved)))
                 ((equal? (caar deps) 'glob_files)
                  (format #t "GLOB dep: ~A\n" (car deps))
                  ;;FIXME: s7 has a C glob fn, enable it
                  ;; for now, hardcode what's neede for tezos
                  ;; (format #t "srcfiles: ~A\n" srcfiles)
                  (let* ((pattern-s (cadar deps))
                         (pattern (if (symbol? pattern-s)
                                      (symbol->string pattern-s)
                                      pattern-s))
                         (pred
                          (cond
                           ((equal? "*.ml{,i}" pattern)
                            (lambda (f)
                              (or
                               (string-suffix? ".mli" f)
                               (string-suffix? ".ml" f))))
                           ((equal? "*.ml" pattern)
                            (lambda (f) (string-suffix? ".ml" f)))
                           ((equal? "*.mli" pattern)
                            (lambda (f) (string-suffix? ".mli" f)))
                           ;; ocaml compiler
                           ((equal? "caml/*.h" pattern)
                            (lambda (f) (string-suffix? ".h" f)))

                           ;; default match none, for now
                           (else
                            (lambda (f) #f))))
                         (files
                          (if (equal? pattern "contracts/*")
                              (map (lambda (f)
                                     `(:_srcfile
                                       ,(string-append pkg-path "/contracts")
                                       ,f))
                                   (directory->list
                                    (string-append pkg-path "/contracts")))
                              (let recur ((srcs srcfiles) (result '()))
                                (if (null? srcs) result
                                    (if (pred (car srcs))
                                        (let* ((f (car srcs))
                                               (testf (string-append
                                                       pkg-path "/" f))
                                               (kw (if (file-exists? testf)
                                                       ':_srcfile
                                                       ':_genfile)))
                                          (recur (cdr srcs)
                                                 (cons
                                                  (list kw
                                                        pkg-path (car srcs))
                                                  result)))
                                        (recur (cdr srcs) result)))))))
                    ;; (format #t "globbed files: ~A\n" files)
                    (recur (cdr deps)
                           (concatenate files filedeps)
                           vars env-vars universe aliases unresolved)))

                 ((equal? (caar deps) 'file)
                  (format #t "FILE dep: ~A\n" deps)
                  ;; (recur (cdr deps) (cons (car deps) result))
                  (recur (cdr deps)
                         (cons
                          (make-filedep-arg pkg-path (cadr deps) #f '())
                          filedeps)
                         vars env-vars universe aliases unresolved))

                 ((equal? (car deps) '(universe))
                  (recur (cdr deps)
                         filedeps vars env-vars #t aliases unresolved))

                 ;; name defn
                 ((char=? #\: (string-ref (symbol->string (caar deps)) 0))
                  (format #t "VAR DEFN : ~A\n" deps)
                  (let ((dep (make-filedep-arg pkg-path
                                               (cadar deps)
                                               (caar deps) '())))
                    (recur
                     (cdr deps)
                     ;; filedeps arg:
                     (if (equal? tool-tag (car dep))
                         filedeps
                         (cons dep filedeps))
                     (cons dep vars)
                     env-vars universe aliases unresolved)))

                 (else
                  (recur (cdr deps)
                         filedeps vars env-vars universe aliases
                         (cons (car deps) unresolved))))

                ;; not a pair, must be a filedep
                (recur (cdr deps)
                       (cons
                        (make-filedep-arg pkg-path (car deps) #f vars)
                        filedeps)
                       vars env-vars universe aliases unresolved))))
      (values #f #f #f #f #f #f)))

;; e.g.
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

(define resolve-tool
  (lambda (tool pkg-path target targets args filedeps vars)
    (format #t "RESOLVE-tool ~A\n" tool)
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
    (let ((tool (if (symbol? tool) (symbol->string tool)
                   (if (string? tool) tool
                       (error 'wrong-type)))))
                              ;; (format #t "tool arg to resolve-tool must be string or symbol")))))
      (if (equal? tool "bash")
          'bash
          (if (string-prefix? "%{ocaml_where}/" tool)
              ;; e.g. %{ocaml_where}/expunge in <ocaml>toplevel/dune
              ;; assume basename (expunge) is a bazel target
              (resolve-cmd-args pkg-path
                                target targets args filedeps vars)

              ;; else
              (resolve-cmd-args
               pkg-path target targets args filedeps vars))))))

(define resolve-cmd-args
  (lambda (pkg-path target targets args filedeps vars)
    (format #t "RESOLVE-CMD-ARGS: ~A\n" args)
    (format #t " cmd vars: ~A\n" vars)
    (if (null? args) args
        (let ((result
               (if (null? args)
                   '()
                   (let ((arg (if (symbol? (car args))
                                  (symbol->string (car args))
                                  (car args))))
                     (cond
                      ((pair? arg) ;; e.g. (:_string "struct")
                       (cons arg
                             (resolve-cmd-args pkg-path
                                               target targets
                                               (cdr args) filedeps vars)))
                      ((number? arg)
                       (cons arg
                             (resolve-cmd-args pkg-path
                                               target targets
                                               (cdr args) filedeps vars)))

                      ((string? arg)
                       (cond
                        ((string=? "%{deps}" arg)
                         (concatenate
                          filedeps
                          ;;  ;; FIXME: create (:target . arg) pairs so
                          ;;  ;; emitter will be able to distinguish
                          ;;  ;; between e.g. string args and target args.
                          (resolve-cmd-args pkg-path
                                            target targets
                                            (cdr args) filedeps vars)))

                        ((string=? "%{targets}" arg)
                         (format #t "ARG TARGETS\n")
                         (format #t " targets val: ~A\n" targets)
                         (cons `(:targets ,targets) ;; arg
                               (resolve-cmd-args pkg-path
                                                 target targets
                                                 (cdr args) filedeps vars)))

                        ((string=? "%{target}" arg)
                         (format #t "ARG TARGET\n")
                         (cons arg
                               (resolve-cmd-args pkg-path
                                                 target targets
                                                 (cdr args) filedeps vars)))

                        ((string-prefix? "%{bin:" arg)
                         (format #t "BIN: ~A\n" arg)
                         (format #t "pkg-path: ~A\n" pkg-path)
                         (cons
                          (list :_bin arg)
                          (resolve-cmd-args pkg-path
                                            target targets
                                            (cdr args) filedeps vars)))

                        ((string-prefix? "%{exe:" arg)
                         (format #t "EXE: ~A\n" arg)
                         (format #t "pkg-path: ~A\n" pkg-path)
                         ;; remove %{exe: and }
                         (let* ((sfx (substring arg 6 (- (length arg) 1)))
                                (tool (string-trim '(#\}) sfx)))
                           (cons
                            (list :_exe "." tool)
                            (resolve-cmd-args pkg-path
                                              target targets
                                              (cdr args) filedeps vars))))

                        ((string-prefix? "%{libexec:" arg)
                         (format #t "LIBEXEC: ~A\n" arg)
                         (format #t "pkg-path: ~A\n" pkg-path)
                         (cons
                          (list :_libexec arg)
                          (resolve-cmd-args pkg-path
                                            target targets
                                            (cdr args) filedeps vars)))

                        ((string-prefix? "%{lib:" arg)
                         (format #t "LIB: ~A\n" arg)
                         (format #t "pkg-path: ~A\n" pkg-path)
                         (cons
                          (list :_lib arg)
                          (resolve-cmd-args pkg-path
                                            target targets
                                            (cdr args) filedeps vars)))

                        ((string-prefix? "%{dep:" arg)
                         ;; "dep:<path> expands to <path>"
                         ;; (cons `(:fixme ,arg)
                         ;;       (resolve-cmd-args pkg-path (cdr args) filedeps vars))
                         (format #t "DEP: var\n")
                         (format #t "pkg-path: ~A\n" pkg-path)
                         (let* ((dep-path (string-drop-right ;; drop '}'
                                           (string-drop arg 6) 1))
                                (segs (string-split dep-path #\/))
                                (seg-ct (length segs))
                                (resolved (let recur ((segs segs)
                                                      (path pkg-path))
                                            (format #t "Recur: ~A; path: ~A\n"
                                                    segs path)
                                            (if (null? segs)
                                                path ;; should not happen?
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
                                                    ;; all leading '..' processed, no further recursion
                                                    ;; FIXME: what if '..' is embedded, e.g. a/../b?
                                                    ;; segs may still contain multiple segs;
                                                    ;; we need all but the last to be added to the path
                                                    ;; so we can form a label of form //a/b/c:x
                                                    (if (> (length segs) 1)
                                                        (begin
                                                          ;; e.g. path: runtime/caml, segs (".." ".." "tools" "make-version-header.sh");
                                                          (let ((fpath (string-append path "/"
                                                                                      (string-join segs "/"))))
                                                            (if (file-exists? fpath)
                                                                (let* ((path-segs (reverse (cdr (reverse segs))))
                                                                       (segpath (string-join path-segs "/"))
                                                                       (last (car (reverse segs)))
                                                                       (p (if (equal? "." path)
                                                                              segpath
                                                                              (string-append path "/" segpath))))
                                                                  (list :_srcfile p last)
                                                                  (list :_genfile p last)))))
                                                        ;; one seg left
                                                        (let ((fpath (string-append path "/" (car segs))))
                                                          (if (file-exists? fpath)
                                                              (list :_srcfile path (car segs))
                                                              ;; (string-append "$(location //" fpath ")") ;; (list :_srcfile fpath)
                                                              (list :_genfile path (car segs))
                                                              ;; (string-append "$(location //" fpath ")") ;; (list :_genfile fpath)
                                                              )))
                                                    )))))
                           (format #t "Resolved dep path: ~A\n" resolved)
                           (cons resolved
                                 (resolve-cmd-args pkg-path
                                                   target targets
                                                   (cdr args)
                                                   filedeps vars))))
                        ;; (run-recur (cdr cmd-args)
                        ;;            ;; run-deps ;; (cons arg run-deps)
                        ;;            (cons arg run-args))))

                        ((string-prefix? "%{" arg)
                         (format #t "VAR: ~A\n" arg)
                         (format #t "pkg-path: ~A\n" pkg-path)
                         (let* ((kw (substring arg 2 (- (length arg) 1)))
                                (keysym (string->symbol (string-append ":" kw))))
                           (format #t "kw ~A\n" kw)
                           (format #t "keysym ~A\n" keysym)
                           (if-let ((val (assoc keysym vars)))
                                   (begin
                                     (format #t "VAR VAL: ~A\n" val)
                                     (cons (cadr val)
                                           (resolve-cmd-args pkg-path
                                                             target targets
                                                             (cdr args) filedeps vars)))
                                   ;; not a var, try installed execs
                                   (cons
                                    :VAR
                                    (resolve-cmd-args pkg-path
                                                      target targets
                                                      (cdr args)
                                                      filedeps vars)))))

                        (else
                         (format #t "OTHER STRING: ~A\n" arg)
                         (cons (resolve-string-arg pkg-path arg vars)
                               (resolve-cmd-args
                                pkg-path target targets
                                (cdr args) filedeps vars)))))

                      (else ; not number, pair, string
                       (format #t
                               "WARNING: not a nbr, pair, or string: ~A\n" arg)
                       ))
                     ))))
          ;; (format #t "resolved: ~A\n" result)
          result))))

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
                   (resolve-cmd-args pkg-path target targets
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
                                (:args ,(resolve-cmd-args pkg-path
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

;; action: (with-<outputs>-to <file> <DSL>) to redirect stdout to a file
;; <file> = usually %{targets} or %{target}???
(define (normalize-with-stdout-to pkg-path action stanza srcfiles)
  (format #t "normalize-with-stdout-to: ~A: ~A\n" pkg-path action)
  (format #t "rule stanza: ~A\n" stanza)

  ;; FIXME: if 'targets' is a list the following won't work
  (let* ((rule-alist (cdr stanza))
         ;; (_ (format #t "RULE-ALIST: ~A\n" rule-alist))
         ;; rule-alist keys: targets, deps, action, ???

         (target (if-let ((target (assoc 'target rule-alist)))
                         (cadr target) #f))
         ;; (_ (format #t "target: ~A\n" target))
         ;; ;; NB: 'targets' often used instead of 'target' for a single target
         ;; ;; for 'with-<output>-to' it MUST be a singleton
         (targets (if-let ((targets (assoc 'targets rule-alist)))
                          (if (= 2 (length targets))
                              (cadr targets)
                              (error 'bad-targets-fld
                                     "'targets' fld of 'with-stdout-to' takes 1 arg\n"))
                          #f))
         ;; (_ (format #t "targets: ~A\n" targets))

         (outfile (if target target
                      (if targets targets
                          (error 'bad-arg
                                 (format #f
                                         "rule for 'with-stdout-to' missing target/targets field\n")))))
         ;; (_ (format #t "outfile: ~A\n" outfile))

         (with-stdout-to (cadr action)) ;; drop 'action' key
         ;; (_ (format #t "with-stdout-to runlist: ~A\n" with-stdout-to))
         ;; ASSUMPTION: (with-stdout-to ...) is the only arg to 'action'
         ;; with-stdout-to == (with-stdout-to %{targets} (...actions...)))
         ;; (ofile (cadr with-stdout-to))
         ;; (_ (format #t "with-stdout-to outfile: ~A\n" ofile))

         (dsl (cddr with-stdout-to))
         ;; (_ (format #t "with-stdout-to dsl: ~A\n" dsl))
         ;; ASSUMPTION: dsl is 2nd and last arg to 'with-outputs-to'
         ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.

         (tool (with-stdout-to->toolname pkg-path
                                         (car dsl) ;; dsl is list of lists
                                         stanza))
         ;; (_ (format #t "TOOL 1: ~A\n" tool))
         )

    ;; (if (> (length dsl) 1)
    ;;     (format #t "normalize-cmd-dsl ct: ~A\n" (length dsl)))

    (let-values (((filedeps vars env-vars universe aliases unresolved)
                  (expand-deps pkg-path
                               #f ;; tool-tag
                               tool
                               (assoc 'deps rule-alist)
                               srcfiles)))
      (format #t "filedeps: ~A\n" filedeps)
      (format #t "vars: ~A\n" vars)
      (format #t "env-vars: ~A\n" env-vars)
      (format #t "universe: ~A\n" universe)
      (format #t "aliases: ~A\n" aliases)
      (format #t "unresolved: ~A\n" unresolved)

      (let ((cmd (if universe
                     (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
                     (normalize-cmd-dsl pkg-path
                                        target targets
                                        dsl
                                        (if filedeps (reverse filedeps)
                                            '())
                                        vars))))

        (format #t "wso cmd: ~A\n" cmd)
        ;; (format #t "xDSL: ~A\n" dsl)
        ;; (let ((deps (assoc ':deps cmd)))
        ;;   (for-each (lambda (dep)
        ;;               (format #t "D: ~A\n" dep))
        ;;             (cadr deps)))

        `(:rule ;; with-stdout-to
          (:out ,outfile)
          (:cmd ,cmd)
          (:vars ,vars)
          ;; ,(if (null? filedeps) '() `(:filedeps ,@(cdr filedeps)))
          (:raw ,stanza))))
    ))

(define (with-stdout-to->toolname pkg-path action stanza)
  ;; (format #t "with-stdout-to->toolname: ~A: ~A\n" pkg-path action)
  ;; (with-stdout-to %{targets} (chdir %{workspace_root} (run <tool>...)))

  ;; (if (equal? 'chdir (car action))
      (let* ((run-list
                (if (equal? 'chdir (car action))
                    (caddr action) ;; skip chdir
                    action))
             (prog-atom (cadr run-list))
             (prog-str (if (symbol? prog-atom)
                           (symbol->string prog-atom) prog-atom)))
        (if (char=? #\% (string-ref prog-str 0))
            (let ((prog-vname (substring prog-str
                                         2 (- (length prog-str) 1))))
              ;; (format #t "  prog-vname: ~A\n" prog-vname)
              ;; return "std" exec names as-is; they will be resolved by
              ;; emitter. convert the others to bazel labels.
              (cond

               ;;FIXME: prefixe names may contain '../' etc.
               ;; e.g. %{exe:../config/discover.exe}

               ((string-prefix? "bin:" prog-vname)
                ;; (format #t "BIN prog: ~A\n" prog-vname)
                prog-atom)
               ((string-prefix? "exe:" prog-vname)
                ;; (format #t "EXE prog: ~A\n" prog-vname)
                prog-atom)
               ((string-prefix? "libexec:" prog-vname)
                ;; (format #t "LIBEXEC prog: ~A\n" prog-vname)
                prog-atom)
               ((string-prefix? "lib:" prog-vname)
                ;; (format #t "LIB prog: ~A\n" prog-vname)
                prog-atom)
               (else
                (format #t "CUSTOM progvar: ~A\n" prog-vname)
                (resolve-local-toolname pkg-path prog-vname action stanza))))))
      ;; first arg not chdir
      ;; (error 'bad-arg (format #f "first arg not 'chdir: ~A\n" action))
      ;; )
)
        ;; (format #t "  prog-atom: ~A\n" prog-atom))))

        ;; (format #t "  prog-atom: ~A\n" prog-atom))))

(define (normalize-tool-tag tag)
  (let ((tag-str (if (symbol? tag) (symbol->string tag) tag)))
    (if (string-prefix? "%{" tag-str)
        (string->symbol
         (string-append ":" (substring tag-str 2 (- (length tag-str) 1))))
        tag)))

(define (normalize-copy-action pkg-path action stanza srcfiles)
  (format #t "NORMALIZE-COPY-ACTION: ~A: ~A\n" pkg-path action)
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
          (:raw ,stanza))))))

(define (normalize-progn-action pkg-path action stanza srcfiles)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; junk

         ;; (filedeps (map (lambda (dep)
         ;;                  (format #t "resolving dep ~A\n" dep)
         ;;                  (make-filedep-arg pkg-path dep #f '()))
         ;;                (cdr deps)))
         ;; (_ (format #t "filedeps: ~A\n" filedeps))

         ;; (run-list (cadr action)) ;; (run <tool> <arg1> ...)
         ;; (_ (format #t "run-list: ~A\n" run-list))

         ;; (target (if-let ((target (assoc 'target rule-alist)))
         ;;                 (cadr target) #f))
         ;; (_ (format #t "target: ~A\n" target))

         ;; (targets (if-let ((targets (assoc 'targets rule-alist)))
         ;;                  (cadr targets)
         ;;                  #f))
         ;; (_ (format #t "targets: ~A\n" targets))

         ;; ;;FIXME: run actions for "alias runtest" etc. have no target(s) fld
         ;; (outfile (if target target
         ;;              (if targets targets
         ;;                  #f)))
         ;; (_ (format #t "outfile: ~A\n" outfile))

         ;; (args (run-action->args pkg-path action run-list))
         ;; (_ (format #t "CMD ARGS: ~A\n" args))

         ;; (dsl run-list)

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
         ;; '()))

    ;; (format #t "DSL: ~A\n" dsl)

    ;; (let-values (((filedeps vars env-vars universe aliases unresolved)
    ;;               (expand-deps pkg-path
    ;;                            tool-tag ;; FIXME: tool-tag
    ;;                            tool
    ;;                            (assoc 'deps rule-alist)
    ;;                            srcfiles)))
    ;;   ;; (format #t "r filedeps: ~A\n" filedeps)
    ;;   ;; (format #t "r vars: ~A\n" vars)
    ;;   ;; (format #t "r env-vars: ~A\n" env-vars)
    ;;   ;; (format #t "r universe: ~A\n" universe)
    ;;   ;; (format #t "r aliases: ~A\n" aliases)
    ;;   ;; (format #t "r unresolved: ~A\n" unresolved)

    ;;   (let ((cmd (if universe
    ;;                  (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
    ;;                  (normalize-cmd-dsl pkg-path
    ;;                                     target
    ;;                                     targets
    ;;                                     (list dsl)
    ;;                                     (if filedeps
    ;;                                         (reverse filedeps)
    ;;                                         '())
    ;;                                     (if vars vars '())))))
    ;;     (format #t "r CMD: '~A'\n" cmd)

    ;;     ;;   (if (not (null? vars))
    ;;     ;;       (format #t "DEPS VARS: ~A\n" vars))

    ;;     ;;   (format #t "DSL: ~A\n" dsl)

    ;;     `(:rule
    ;;       (:out ,outfile)
    ;;       (:cmd ,cmd) ;; contains deps?
    ;;       (:vars ,vars)
    ;;       (:raw ,stanza))))
;; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (cond
  ;;  ((eq? 'progn (cadr action)) ;; (action progn)
  ;;   ;; (format #t "    progn: simple\n")
  ;;   stanza)

  ;;  ((equal? '(progn) (cadr action)) ;; (action (progn)) - null action?
  ;;   ;; (format #t "  Action: empty progn\n")
  ;;   ;; this is a null action. used with alias, to force build of deps?
  ;;   ;; usually alias is 'runtest'?
  ;;   ;; but dune "aliases" like runtest are group names, not true aliases.
  ;;   ;; so there is no corresponding Bazel target.

  ;;   ;; e.g.
  ;;   ;; (rule (alias runtest)
  ;;   ;;       (package tezos-protocol-004-Pt24m4xi)
  ;;   ;;       (deps (alias runtest_sandbox))
  ;;   ;;       (action (progn)))

  ;;   ;; verify we also have an alias:
  ;;   ;; (let* ((rule-alist (cdr stanza))
  ;;   ;;        (alias (assoc 'alias rule-alist)))
  ;;   ;;   ;; aliases used by tezos: runtest, buildtest
  ;;   ;;   (if alias
  ;;   ;;       (format #t "    progn alias: ~A\n" alias)
  ;;   ;;       (format #t "    progn w/o alias: ~A\n" stanza))
  ;;   ;;   )
  ;;   stanza)

  ;;  (else ;; (action (progn ...))
  ;;   ;; (format #t "    progn: compound\n")
  ;;   stanza))
  ;; )

;; (define (normalize-action pkg-path action stanza srcfiles)
;;   action)

;; (define (normalize-action pkg-path action stanza srcfiles)
(define (normalize-action pkg stanza-alist) ;; rule stanza
  (format #t "NORMALIZE-action: ~A\n" stanza-alist)
  (let* ((action-alist (assoc-val 'action stanza-alist))
         (action (if (pair? (car action-alist)) ;; e.g. (action (tool ...))
                     (caar action-alist)
                     ;; else (action tool ...)
                     (cadr action-alist))))
    (format #t "  action action: ~A\n" action)
    (format #t "  action alist: ~A\n" action-alist)
    (case action  ;; (car action)
    ;;   ((copy#) (normalize-copy-action pkg-path action stanza srcfiles))
    ;;   ((copy) (normalize-copy-action pkg-path action stanza srcfiles))

      ((run) (normalize-run-action pkg action stanza-alist))
      ;; pkg-path action stanza srcfiles))

    ;;   ((progn) (normalize-progn-action pkg-path action stanza srcfiles))

    ;;   ((with-stdout-to)
    ;;    (normalize-with-stdout-to pkg-path action stanza srcfiles))

    ;;   ((with-stderr-to) stanza)
    ;;   ((with-stdin-from) stanza)
    ;;   ((with-outputs-to) stanza)

    ;;   ((write-file) (normalize-write-file action stanza))

      (else
       (format #t "UNHANDLED ACTION\n")
       stanza))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (display "loaded dune/dune_actions.scm") (newline)
