;; (display "dune/dune_action_run.scm loading...") (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUN ACTION
;; (run <prog> <args>) as primary action of rule stanza
;; 'run' may be embedded in other stanzas, e.g.
;; (with-stdout-to %{targets} (chdir %{workspace_root} (run ...)))

;; vars:
;; lib:<public-library-name>:<file> expands to the installation path of the file <file> in the library <public-library-name>. If <public-library-name> is available in the current workspace, the local file will be used, otherwise the one from the installed world will be used.
;; e.g.
  ;; (run
  ;;   %{libexec:tezos-protocol-compiler:replace}
  ;;   %{libexec:tezos-protocol-compiler:dune_protocol.template}
  ;;   "dune.inc.gen"
  ;;   %{libexec:tezos-protocol-compiler:final_protocol_versions})))
;; e.g. from tezos lib_sapling/binding:
;; (rule
;;  (targets rustzcash_ctypes_stubs.ml rustzcash_ctypes_c_stubs.c)
;;  (deps    (:gen ./rustzcash_ctypes_gen.exe))
;;  (action  (run %{gen} %{targets})))

;; toolname may start with './', '../', etc. and may contain '/'
;; e.g. gen/bip39_generator.exe
;; FIXME: rename, works for any file, not just tools
(define (normalize-toolname pkg-path tool)
  (if *mibl-debugging*
      (format #t "normalize-toolname: ~A\n" tool))
  (let* ((segs (string-split tool #\/))
         (seg-ct (length segs)))
    (let recur ((segs segs)
                (result pkg-path))
      (if *mibl-debugging*
          (format #t "recur segs: ~A, result: ~A\n" segs result))
      (if (null? segs)
          result
          (cond
            ((equal? (car segs) ".")
             (if *mibl-debugging*
                 (format #t "~A\n" "tool DOT seg"))
             (recur (cdr segs) result))
            ((equal? (car segs) "..")
             (if *mibl-debugging*
                 (format #t "~A\n" "tool DOTDOT seg"))
             (let ((last-slash (string-index-right result
                                                   (lambda (ch)
                                                     (char=? ch #\/)))))
               (if last-slash
                   (recur (cdr segs)
                          (string-drop-right
                           path (- (length path) last-slash)))
                   ;; no slash in path
                   (recur (cdr segs) ::wsroot)) ;; "."))
               ;; (recur (cdr segs)
               ;;        (string-drop-right result
               ;;                           (- (length result) last-slash)))
               ))
            (else
             (if *mibl-debugging*
                 (format #t "ELSE car segs: ~A\n" (car segs)))
             (if (null? (cdr segs))
                 (recur (cdr segs) (string-append result "/" (car segs)))
                 (recur (cdr segs)
                        (string-append result "/" (car segs)))))))
             )))
  ;; (if (string-prefix? "./" tool)
  ;;     (string-append ":" (substring exe 2))
  ;;                       exe)

(define (resolve-local-toolname pkg-path toolname action stanza)
  (if *mibl-debugging*
      (begin
        (format #t "RESOLVE-local-toolname: ~A:: ~A\n" pkg-path toolname)
        (format #t " stanza: ~A\n" stanza)))
  (let ((deps (assoc 'deps (cdr stanza))))
    ;; (format #t "deps: ~A\n" deps)
    (cond
     ((equal? (string->symbol toolname) 'deps)
      ;; (if (equal? (string->symbol toolname) 'deps)
      (if (not (= (length deps) 2))
          (error 'bad-arg
                 (format #f "Unexpected run prog ~A\n" action))
          (let ((exec (cadr deps)))
            (string-append ":" (if (symbol? exec)
                                   (symbol->string exec)
                                   exec)))))
     (else
      (if deps
          (begin
            (if *mibl-debugging*
                (format #t "rlt DEPS:: ~A\n" deps))
            ;; e.g. (deps (:exe gen/bip39_generator.exe) ...)
            ;; e.g. (deps (:gen gen.exe))
            (let ((deps-list (cdr deps)))
              ;; (format #t "deps-list: ~A\n" deps-list)
              (if (pair? (car deps-list))
                  (if (equal? (string-append ":" toolname)
                              (symbol->string (caar deps-list)))
                      (let ((exe (symbol->string (cadr (car deps-list)))))
                        (if *mibl-debugging*
                            (format #t "pkg ~A;  exe: ~A\n" pkg-path exe))
                        (normalize-toolname pkg-path exe)
                        ;; (if (string-prefix? "./" exe)
                        ;;     (string-append ":" (substring exe 2))
                        ;;     exe)
                        )
                      (string-append "FIXME1-" toolname))
                  ;; else ???
                  (string-append "FIXME2-" toolname)))))))
     ))

;; (define (run-action->toolname pkg-path run-alist) ;; stanza)
;;   (format #t "run-action->toolname: ~A: ~A\n" pkg-path run-alist)
;;   ;; run-alist taken from (run ...) : e.g.
;;   ;; (bash %{libexec:tezos-stdlib:test-ocp-indent.sh} %{deps}))

;;   ;; tool examples:
;;   ;; (run (progn))
;;   ;; (run %{bin:tezos-protocol-compiler} ...)
;;   ;; (run %{libexec:tezos-protocol-compiler:replace} ...)
;;   ;; (run %{exe:main.exe} -v -q)
;;   ;;      [and generally (run ${exe:foo.exe} ...)]

;;   ;; (run %{<} %{targets})
;;   ;; (run %{deps} ...) where (deps ...) declares an executable, e.g.
;;   ;; (rule ... (deps gen.exe) (action (run %{deps} ...)))
;;   ;; (run bash ...)
;;   ;; (run ./main.exe "test" "Unit")

;;   ;; (run cp %{deps} ./)

;;   ;; tezos src/lib_sapling/binding:
;;   ;; (rule
;;   ;;  (targets rustzcash_ctypes_stubs.ml rustzcash_ctypes_c_stubs.c)
;;   ;;  (deps    (:gen ./rustzcash_ctypes_gen.exe))
;;   ;;  (action  (run %{gen} %{targets})))

;;   (let* (;; (run-list (cadr action-alist))
;;          (prog-atom (car run-alist))
;;          (prog-str (if (symbol? prog-atom)
;;                        (symbol->string prog-atom) prog-atom)))
;;     (format #t "  prog-atom: ~A\n" prog-atom)
;;     (format #t "  prog-str: ~A\n" prog-str)

;;     (if (char=? #\% (string-ref prog-str 0))
;;         ;; extract from %{ }
;;         (let ((prog-vname (substring prog-str
;;                                      2 (- (length prog-str) 1))))
;;           ;; (format #t "  prog-vname: ~A\n" prog-vname)
;;           ;; return "std" exec names as-is; they will be resolved by
;;           ;; emitter. convert the others to bazel labels.
;;           (cond

;;            ;;FIXME: prefixe names may contain '../' etc.
;;            ;; e.g. %{exe:../config/discover.exe}

;;            ((string-prefix? "bin:" prog-vname)
;;             ;; (format #t "BIN prog: ~A\n" prog-vname)
;;             prog-atom)
;;            ((string-prefix? "exe:" prog-vname)
;;             ;; (format #t "EXE prog: ~A\n" prog-vname)
;;             prog-atom)
;;            ((string-prefix? "libexec:" prog-vname)
;;             ;; (format #t "LIBEXEC prog: ~A\n" prog-vname)
;;             prog-atom)
;;            ((string-prefix? "lib:" prog-vname)
;;             ;; (format #t "LIB prog: ~A\n" prog-vname)
;;             prog-atom)
;;            (else
;;             ;; user-tagged executable dep, e.g. %{<}
;;             (format #t "CUSTOM progvar2: ~A\n" prog-vname)
;;             (let ((kw (string->keyword (string-append ":" prog-vname))))
;;               (format #t "CUSTOM kw: ~A\n" kw)
;;               kw))
;;             ;; (resolve-local-toolname pkg-path prog-vname action-alist stanza))
;;            ))
;;         ;; else not a ${} var
;;         (cond
;;          ((equal? 'bash prog-atom) prog-atom)
;;          (else prog-atom))
;;         )
;;         ;; else (char=? #\% (string-ref prog-str 0))
;;         ))
        ;; (format #t "  prog-atom: ~A\n" prog-atom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e.g. src/proto_004_Pt24m4xi/lib_protocol/dune:
;; (rule
;;  (targets "dune.inc.gen")
;;  (deps TEZOS_PROTOCOL (glob_files *.ml) (glob_files *.mli))
;;  (action
;;   (run
;;     %{libexec:tezos-protocol-compiler:replace}
;;     %{libexec:tezos-protocol-compiler:dune_protocol.template}
;;     "dune.inc.gen"
;;     %{libexec:tezos-protocol-compiler:final_protocol_versions})))

(define (run-action->args pkg-path action run-list)
  (let recur ((args (cddr run-list))
              (result '()))
    args))

;; e.g.
;; (deps TEZOS_PROTOCOL (glob_files *.ml) (glob_files *.mli))
;; (deps (glob_files *.ml{,i}))
;; (deps error.mli error.ml ...)
;; (deps .tezos_protocol_demo_counter.objs/native/tezos_protocol_demo_counter.cmx)
;; (deps (alias runtest_sandbox))
;; with custom var defns:
;; (deps error.mli error.ml ... (:src_dir TEZOS_PROTOCOL))
;; (deps (:gen ./rustzcash_ctypes_gen.exe))
;; (deps (:legacy_builder ../legacy_store/legacy_store_builder.exe))
;; (deps (universe) (:script get-git-info.mlt))
;; lots of :exe
;; (deps (:exe gen/bip39_generator.exe) gen/bip39_english.txt)
;; (deps (:exe bip39_tests.exe))
;; etc.
(define (run-action->deps pkg-path tool rule-alist)
  ;; if the tool is listed in the deps, remove it
  (if *mibl-debugging*
      (begin
        (format #t "RUN-ACTION->DEPS: ~A\n" pkg-path)
        (format #t "rule-alist: ~A\n" rule-alist)))

  (define resolve-depvar
    ;; e.g. (:exe gen/bip39_generator.exe)
    (lambda (dep-pair)
      (let* ((kw (car dep-pair))
             (dep (cadr dep-pair))
             (dep-label (normalize-toolname pkg-path
                                            (if (symbol? dep)
                                                (symbol->string dep)
                                                dep))))
        (values kw dep-label))))

  ;; (let ((rule-alist (cdr stanza)))
    (if-let ((deps (assoc-val 'deps rule-alist)))
            (let recur ((deps deps)
                        (result '()))
              (if *mibl-debugging*
                  (format #t "deps: ~A\n" (if (null? deps) '() deps)))
              ;; (format #t "result: ~A\n" result)
              (if (null? deps)
                  (reverse result)
                  (if (pair? (car deps))
                      (case (caar deps)
                        ;; ((alias)
                        ;;  (format #t "ALIAS dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))
                        ;; ((alias_rec)
                        ;;  (format #t "ALIAS_REC dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))

                        ((glob_files)
                         (if *mibl-debugging*
                             (format #t "GLOB dep: ~A\n" (car deps)))
                         (recur (cdr deps) (cons (car deps) result)))

                        ((file)
                         ;; (format #t "FILE dep: ~A\n" (car deps))
                         (recur (cdr deps) (cons (car deps) result)))

                        ;; ((source_tree)
                        ;;  (format #t "SOURCE_TREE dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))
                        ;; ((universe)
                        ;;  (format #t "UNIVERSE dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))
                        ((package)
                         (if *mibl-debugging*
                             (format #t "WARNING: dep fld 'package' not yet supported: ~A\n" (car deps)))
                         (recur (cdr deps) (cons (car deps) result)))
                        ;; ((env_var)
                        ;;  (format #t "ENV_VAR dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))
                        ;; ((sandbox)
                        ;;  (format #t "SANDBOX dep: ~A\n" (car deps))
                        ;;  (recur (cdr deps) (cons (car deps) result)))
                        ((alias alias_rec source_tree universe package
                                env_var sandbox)
                         (error 'unsupported-dep
                                (format #f "dep kw '~A' not yet supported"
                                        (caar deps))))

                        (else ;; either custom kw or unknown
                         (if (equal? #\: (string-ref
                                          (symbol->string (caar deps)) 0))
                             (let-values (((kw lbl)
                                           (resolve-depvar (car deps))))
                               (recur (cdr deps)
                                      ;; omit dep == tool
                                      (if (equal? tool lbl)
                                          (begin
                                            ;; (format #t "OMITTING (~A ~A)\n"
                                            ;;         kw lbl)
                                            result)
                                          (cons `(,kw ,lbl) result))))
                             (error 'unknown-dep
                                    (format #f "dep kw '~A' unknown"
                                            (caar deps))))))
                      ;; car is not pair, must be file dep
                      (recur (cdr deps) (cons
                                         (normalize-toolname pkg-path
                                            (if (symbol? (car deps))
                                                (symbol->string (car deps))
                                                (car deps)))
                                         result)))))
            ;; else no deps field
            '()))

;; (define (normalize-run-action pkg-path action stanza srcfiles)
(define (normalize-run-action pkg action-alist targets deps)
  (if *mibl-debugging*
      (format #t "NORMALIZE-run-action: ~A\n" action-alist))
  (let* ((stanza-type :run-cmd)
         ;; (rule-alist stanza-alist) ;; (cdr stanza))
         ;; (action-alist (assoc-val 'action rule-alist))
         (_ (if *mibl-debugging* (format #t "action-alist: ~A\n" action-alist)))
         (run-alist (assoc-val 'run action-alist))
         (_ (if *mibl-debugging* (format #t "run-alist: ~A\n" run-alist)))

         (pkg-path (assoc-val :pkg-path pkg))
         ;; (_ (if *mibl-debugging* (format #t "pkg-path: ~A\n" pkg-path)))

         (tool (run-action->toolname pkg-path run-alist))
         (_ (if *mibl-debugging* (format #t "TOOL: ~A\n" tool)))

         ;; (tool-tag (normalize-tool-tag (cadadr action)))
         ;; (_ (if *mibl-debugging* (format #t "TOOL-TAG: ~A\n" tool-tag)))

         ;; (run-list (cadr action)) ;; (run <tool> <arg1> ...)
         ;; ;; (_ (if *mibl-debugging* (format #t "run-list: ~A\n" run-list)))

         ;; (target (if-let ((target (assoc 'target rule-alist)))
         ;;                 (cadr target) #f))
         ;; ;; (_ (if *mibl-debugging* (format #t "target: ~A\n" target)))
         ;; (targets (if-let ((targets (assoc 'targets rule-alist)))
         ;;                  (cadr targets)
         ;;                  #f))
         ;; ;; (_ (if *mibl-debugging* (format #t "targets: ~A\n" targets)))

         ;; ;;FIXME: run actions for "alias runtest" etc. have no target(s) fld
         ;; (outfile (if target target
         ;;              (if targets targets
         ;;                  '())))

         ;; ;; (_ (if *mibl-debugging* (format #t "outfile: ~A\n" outfile)))

         (args (cdr run-alist))
          ;;(run-action->args pkg-path action run-list))
         (_ (if *mibl-debugging* (format #t "CMD ARGS: ~A\n" args)))

         ;; (dsl run-list)

         ;; (deps
         ;;  (expand-rule-deps pkg stanza-alist)
         ;;  ;;(run-action->deps pkg-path tool rule-alist)
         ;;  )
         (_ (if *mibl-debugging* (format #t "CMD DEPS: ~A\n" deps)))

         ;; ;;        (dsl (cadr (cdadr action)))
         ;; ;;        ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.
         (cmd `((:tool ,tool)
                ;; (:targets ,targets)
                ;; (:deps ,deps)
                (:args ,args)
                (:raw ,action-alist))))
                ;; (target (assoc 'target rule-alist))
                ;; (targets (assoc 'targets rule-alist))
                ;; (outfile (if (equal? file '%{targets})
                ;;              (cadr targets)
                ;;              (if (equal? file '%{target})
                ;;                  (cadr target)
                ;;                  (begin
                ;;                    (format #t "WARNING: write-file out: ~A\n" file)
                ;;                    file))))
         cmd))

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

    ;;   (let* ((cmd (if universe
    ;;                  (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
    ;;                  (normalize-cmd-dsl pkg-path
    ;;                                     target
    ;;                                     targets
    ;;                                     (list dsl)
    ;;                                     (if filedeps
    ;;                                         (reverse filedeps)
    ;;                                         '())
    ;;                                     (if vars vars '()))))
    ;;          (result `((:cmd ,cmd) ;; contains deps?
    ;;                    (:pkg ,pkg-path)
    ;;                    (:raw ,stanza)))
    ;;          (result (if vars
    ;;                      (acons :vars vars result)
    ;;                      result))
    ;;          (result (if (null? outfile)
    ;;                      result
    ;;                      (acons (:out outfile) result)))
    ;;          (result (if-let ((alias (assoc 'alias rule-alist)))
    ;;                          (acons :alias (last alias) result)
    ;;                          result)))
    ;;     `(,stanza-type ,result)
    ;;     ))
    ;; #t))

;; (display "loaded mibl/dune_action_run.scm") (newline)
