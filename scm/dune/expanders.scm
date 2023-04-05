(if *mibl-debug-s7-loads*
    (format #t "loading dune/expanders.scm\n"))

(load "dune/api_utils.scm")

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

;; returns kw used as key in :tool list, e.g. :tools/rewrite.exe
(define expand-run-tool
  (lambda (ws tool tools pkg targets deps)
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (begin
          (format #t "~A: ~A (~A)~%" (ublue "expand-run-tool") tool (type-of tool))
          (format #t "~A: ~A~%" (green "deps") deps)))

    (case tool
      ((%{bin:ocamlc}) ::ocamlc)
      ((%{bin:node}) ::node)
      ((cp) ::copy) ;;FIXME: use lookup table from constants.scm
      ((node)
       (set-cdr! deps
                 (append
                  `((::tools (::node . ::unresolved)))
                  (cdr deps)))
       ::node)

      ((%{deps}) :deps) ;; ::deps
       ;; (set-cdr! deps
       ;;           (append
       ;;            (list (cons ::tools
       ;;                        (list
       ;;                         (cons search-key ;; arg-kw
       ;;                               ;; (list (cons :pkg pkg-path)
       ;;                               ;;       (cons :tgt arg))
       ;;                               ::unresolved
       ;;                               ))))
       ;;            (cdr deps)))
       ;; :FOO)

      (else
       (let ((tool (format #f "~A" tool)))
         (cond
          ((string-prefix? "%{" tool)
           (-expand-pct-tool!? ws tool :tool pkg deps))

          (else
           (let ((expanded-tool (-expand-literal-tool!? ws (assoc-val :pkg-path pkg)
                                                        tool tools deps)))
             (mibl-trace "expanded-tool" expanded-tool)
             (mibl-trace "deps" deps)
             ;; (error 'X "STOP expand-run-tool")
            expanded-tool))))))))

(define (find-in-exports ws search-key)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "find-in-exports") search-key))
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws *mibl-project*)))))
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (green "exports") exports))
    (hash-table-ref exports search-key)))

(define (-find-executable-for-name nm pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (format #t "~A: ~A pkg: ~A~%" (ublue "-find-executable-for-name") nm (assoc-val :pkg-path pkg)))
  (let ((nm (if (keyword? nm) (keyword->symbol nm)))
        (stanzas (assoc-val :mibl pkg)))
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (blue "stanzas") stanzas))
    (find-if (lambda (stanza)
               (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                   (format #t "~A: ~A~%" (blue "stanza") stanza))
               (if (equal? (car stanza) :executable)
                   (let* ((stanza-alist (cdr stanza))
                          (privname (assoc-val :privname stanza-alist)))
                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                         (format #t "~A: ~A =? ~A~%" (cyan "testing") nm privname))
                     (cond
                      ((equal? nm privname) #t)

                      ;; main.bc
                      ((member 'byte (assoc-val* :modes stanza-alist))
                       (string=? (format #f "~A" nm) (format #f "~A.bc" privname)))

                      ;; main.exe
                      ((string=? (format #f "~A" nm) (format #f "~A.exe" privname)) #t)
                      (else #f)))
                   #f))
             stanzas)))

(define (-match-dep pfx key dep)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (format #t "~A: ~A/~A ? ~A~%" (ublue "-match-dep") pfx key dep))
)

;; may update :deps
(define (-find-in-deps!? nm deps pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "-find-in-deps") nm)
        (format #t "~A: ~A~%" (blue "deps") deps)))
  (let ((d (find-if (lambda (dep)
                      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (blue "dep") dep))
                      ;;FIXME: special tags ::tools, ::opam-pkgs, ::unresolved, etc.
                      (cond
                       ((equal? (car dep) nm)
                        (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (uwhite "matched?") dep))
                        #t)
                       ((member (cdr dep) '(::unresolved ::opam-pkg)) #f)
                       (else #f)))
                      (cdr deps))))
    (if d d
        ;; else
        (if-let ((matching-exe (-find-executable-for-name nm pkg)))
                (let ((tgt (format #f "~A.exe" (assoc-val :privname (cdr matching-exe)))))
                  ;; TODO: update :deps for stanza
                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      (format #t "~A: ~A~%" (bggreen "pctvar resolved") tgt))
                  (let ((newdep (list (cons nm (list `(:pkg . ,(assoc-val :pkg-path pkg)) `(:tgt . ,tgt))))))
                    (set-cdr! deps (append (cdr deps) newdep))
                    #t)
                  )
                ))))

;; may update :deps
(define (-expand-dep-pct-var!? ws pfx sym deps pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (format #t "~A: pfx: ~A; sym: ~A~%"
          (ublue "-expand-dep-pct-var!?") pfx sym))
  ;; arg form:  dep:<path>
  ;; search deps for <path>
  ;; if not found, search pkg files? no need?
  ;; add it to pkg files as :dynamic?
  ;; add it to :deps
  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (search-key (symbol->keyword sym))
         (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "searching deps") deps)))
         (d (-find-in-deps!? search-key deps pkg)))
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (yellow "found?") d))

    (if d
        search-key
        ;; if not found in :deps fld:
        ;; 1. look for executable stanza in pkg with matching name
        ;; 2. search pkg files and add it to :deps if found
        ;; 3. search exports table
        ;; 4. mark it ::unresolved and let a later pass resolve it

        (let ((v (format #f "~A" sym))
              (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "searching pkg files") deps)))
              (expanded
               ;; FIXME: already searched :deps, no need for this:
               (if-let ((tag (deps->tag-for-file deps search-key)))
                       ;; found it
                       tag
                       ;; else not found, so search pkg files for file not listed in :deps
                       ;; if found, -infer-dep! adds to :deps
                       ;; otherwise, search exports table, finally assume its a filename literal
                       (if-let ((inferred-dep
                                 (-infer-dep! ws sym #|search-key|# deps pkg)))
                               inferred-dep
                               (begin
                                 (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                     (format #t "~A: ~A~%" (red "unresolved") search-key))

                                 ;; first search exports tbl
                                 ;;FIXME: this is pointless since the exports tbl is incomplete
                                 ;; unresolved args must be resolved by later pass
                                 (if-let ((found (find-in-exports ws search-key)))
                                         (let* ((pkg (assoc-val :pkg found))
                                                (tgt (assoc-val :tgt found))
                                                (entry `((,search-key (:pkg . ,pkg) (:tgt . ,tgt)))))
                                           (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                               (format #t "~A: ~A~%" (red "found") found))
                                           ;; (error 'STOP "STOP exp")
                                           (set-cdr! deps (append (cdr deps) entry))
                                           entry)
                                         (let ((x (handle-filename-literal-arg ws sym pkg)))
                                           (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                               (begin
                                                 (format #t "~A: ~A~%" (bgred "RESOLVED") x)
                                                 (format #t "~A: ~A~%" (red "deps") deps)))
                                           (set-cdr! deps (append (cdr deps) x))
                                           x))
                                 ))))

              (dep (list (symbol->keyword sym))))
          (symbol->keyword sym)
          ;; (set-cdr! deps
          ;;           (append (list dep)
          ;;                   (cdr deps)))
          ))))

;; expand-cmd-args*:
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

;; may update deps
;; case:
;; %{deps} => ::deps
;; %{deps:main.bc} => look for executable in same pkg, name maine, mode byte
;; (%{dep:rejections.sh} => (:deps (:rejections.sh (:pkg...)(:tgt...)))
;;  %{bin:tezos-protocol-compiler} => (:deps (::bin tezos...))
;;  %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL})
;;      =>(:deps (::lib tezos...))

;; directives supporting pct-vars: action; what else?
;; in an action directive, pct-var may be cmd or cmd arg, and may
;; refer to or imply a deps item.

;; task: map pct-var to label or path or string
(define (-expand-pct-arg!? ws arg kind pkg deps)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A~%" (ublue "-expand-pct-arg!?"))
        (format #t "~A: ~A (type: ~A)~%" (blue "arg")
                arg (type-of arg))
        (format #t "~A: ~A~%" (blue "kind") kind)
        (format #t "~A: ~A~%" (blue "deps") deps)))

  (case arg
    ;; action directives:
    ((%{deps}) :deps) ;; ::deps?
    ((%{^}) :deps) ; expands to the list of deps, separated by spaces.
    ((%{target}) :outputs)
    ((%{targets}) :outputs)
   (else (pct-var->keyword arg)))
  ;; plus prefix + parameter, see below

    ;; ;; general
    ;; ((%{project_root} %{workspace_root}
    ;;    %{CC} %{CXX}
    ;;    %{ocaml_bin} %{ocaml} %{ocamlc} %{ocamlopt}
    ;;    %{ocaml_version} %{ocaml_where}
    ;;    %{arch_sixtyfour} %{null}
    ;;    %{ext_obj} %{ext_asm} %{ext_lib} %{ext_dll} %{ext_exe} %{ext_plugin}
    ;;    %{profile} %{context_name}
    ;;    %{os_type} %{architecture} %{model} %{system}
    ;;    %{ignoring_promoted_rule})
    ;;  ;; just map to s7 keywords, let the emitter figure them out
    ;;  (let* ((pct-var-kw (pct-var->keyword arg))
    ;;         (match ;; find arg in deps
    ;;          (find-if (lambda (dep)
    ;;                     (equal? (car dep) pct-var-kw))
    ;;                   (cdr deps))))
    ;;    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
    ;;        (format #t "~A: ~A~%" (yellow "match?") match))
    ;;    (if match
    ;;        pct-var-kw
    ;;        ;; else not in :deps
    ;;        (error 'fixme
    ;;               (format #f "arg not in deps: ~A" pct-var-kw))
    ;;        )))

    ;; ((%{coq:version} %{coq:version.major} %{coq:version.minor}
    ;;    %{coq:version.suffix} %{coq:ocaml-version}
    ;;    %{coq:coqlib} %{coq:coq_native_compiler_default})
    ;;  (pct-var->keyword arg))

    ;; ;; else decompose and check pfx
    ;; (else
    ;;  (let-values (((sym pfx sfx) (parse-pct-var arg)))
    ;;    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
    ;;        (format #t "~A: ~A, ~A: ~A~%"
    ;;            (green "arg pfx") pfx (green "sfx") sfx))

    ;;    (if pfx
    ;;        (case pfx
    ;;          ((:dep)
    ;;            (begin
    ;;              ;; prefixed pctvars will never be already in :deps?
    ;;              ;; e.g. lib:foo, bin:foo, etc.
    ;;              (if (or *mibl-debug-expanders* *mibl-debug-s7*)
    ;;                  (format #t "~A: ~A~%" (ured "deps before") deps))
    ;;              (set-cdr! deps
    ;;                        (append
    ;;                         (list (list (symbol->keyword sym)
    ;;                                     ;;(symbol->keyword pfx)
    ;;                                     ::unresolved))
    ;;                         (cdr deps)))
    ;;              (if (or *mibl-debug-expanders* *mibl-debug-s7*)
    ;;                  (format #t "~A: ~A~%" (ured "deps after") deps))
    ;;              (symbol->keyword sym)))

    ;;          ((:cmo :cmi :cma :cmx :cmxa) )
    ;;          ((:env) )                  ; e.g. %{env:BIN=/usr/bin}
    ;;          ((:ocaml-config)
    ;;           ;; ocaml-config:v is for every variable v in the output of ocamlc -config
    ;;           )

    ;;          ((:lib) (pct-var->keyword arg))
    ;;          ((:lib-available) )
    ;;          ((:lib-private) )
    ;;          ((:libexec) )
    ;;          ((:libexec-private) )
    ;;          ((:bin) )
    ;;          ((:bin-available) )
    ;;          ((:exe) )
    ;;          ((:version) )
    ;;          ((:read) )
    ;;          ((:read-lines) )
    ;;          ((:read-strings) )

    ;;          (else
    ;;           )))
    ;;    ))
    ;; )
  )

;; may update deps
(define (-expand-pct-tool!? ws arg kind pkg deps)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A (~A)~%" (ublue "-expand-pct-tool!?")
                arg (type-of arg))
        (format #t "deps: ~A~%" deps)))

  (let-values (((sym pfx sfx) (parse-pct-var arg)))
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A, ~A: ~A~%"
                (uwhite "arg pfx") pfx (uwhite "sfx") sfx))

    ;; find arg in deps (assumption: pfx is not :deps)
    (let* ((search-key (if (equal? pfx :dep)
                           (symbol->keyword sfx)
                           (symbol->keyword sym))) ;; (pct-var->keyword arg))
           (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A (kw? ~A)~%" (yellow "search-key")
                      search-key (keyword? search-key))))
           ;; (_ (if (equal? pfx :dep) (error 'STOP "STOP pct-tool")))
           (match (find-if (lambda (dep)
                             (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                 (format #t "~A: ~A (~A)~%"
                                     (yellow "testing dep") dep (type-of (car dep))))
                             ;; (format #t "~A: ~A~%" (yellow "kw?") (keyword?
                             ;;                                       (car dep)))
                             (equal? (car dep) search-key))
                           (cdr deps))))
      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (format #t "~A: ~A~%" (yellow "match?") match))
      (if match
          ;;(let ((arg-kw (string->keyword arg)))
          ;; move it from (:deps) to (:deps ::tools)
          (begin
            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (red "pct tool in deps") match))
            (set-cdr! deps
                      (append
                       (list (list ::toolsA match
                                   #| (list search-key ;; arg-kw
                                   (cons :pkg pkg-path)
                                   (cons :tgt arg))|# ))
                       (alist-delete (list search-key #|arg-kw|# ) (cdr deps)))))

          ;; else infer it must be imported from exports tbl
          ;; FIXME FIXME: only works after first pass adds everything to exports tbl
          (begin
            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (format #t "~A: ~A, ~A~%" (red "tool NOT in deps") sym deps))
            (if-let ((found (find-in-exports ws (symbol->keyword sym))))
                    (begin
                      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                          (begin
                            (format #t "~A: ~A~%" (bgred "kw") (symbol->keyword sym))
                            (format #t "~A: ~A~%" (bgred "found") found)))
                      (set-cdr! deps
                                (append
                                 (list (cons ::toolsB
                                             (list
                                              (cons (symbol->keyword sym)
                                                    ::unresolved
                                                    ))))
                                 (cdr deps))))
                      ;; else set to ::unresolved
                      (set-cdr! deps
                                (append
                                 (list (cons ::toolsC
                                             (list
                                              (cons (if (equal? :dep pfx)
                                                        (symbol->keyword sfx)
                                                        (symbol->keyword sym)) ;; search-key ;; arg-kw
                                                    ;; (list (cons :pkg pkg-path)
                                                    ;;       (cons :tgt arg))
                                                    ::unresolved
                                                    ))))
                                 (cdr deps))))
            ;; return kw
            ))
      search-key)))

    ;; (if (null? (cdr deps))
    ;;     ;; infer dep for arg
    ;;     (let* ((arg-kw (string->keyword (format #f "~A" arg)))
    ;;            (_
    ;;             (format #t "~A: ~A~%" (white "inferring dep for arg") arg-kw))
    ;;            )
    ;;       (set-cdr! deps
    ;;                 (list (cons ::tools
    ;;                             (list (cons arg-kw `((:pkg ,pkg-path)))
    ;;                             (cons :tgt arg)))))
    ;;       arg-kw)
    ;;     ;; else
    ;;     (let ((arg (format #f "~A" arg))
    ;;           (t
    ;;            (begin
    ;;              (format #t "~A: ~A for ~A~%" (green "searching deps") (cdr deps) arg)
    ;;              (find-if (lambda (dep)
    ;;                         (format #t "~A: ~A~%" (green "find test dep") dep)
    ;;                         (let* ((pkg-tgt (cdr dep))
    ;;                                (tgt (if-let ((tgt (assoc-val :tgt pkg-tgt)))
    ;;                                             (equal? arg tgt)
    ;;                                             (if-let ((tgts
    ;;                                                       (assoc-val :tgts pkg-tgt)))
    ;;                                                     (equal? arg tgts)
    ;;                                                     (error 'fixme "missing :tgt and :tgts in dep")))))
    ;;                           tgt))
    ;;                       (cdr deps)))))
    ;;       (if t
    ;;           (let ((arg-kw (string->keyword arg)))
    ;;             (format #t "~A: ~A~%" (red "arg in deps") t)
    ;;             ;; move it from (:deps) to (deps ::tools)
    ;;             (set-cdr! deps
    ;;                       (append
    ;;                        (list (list ::tools
    ;;                                    (list arg-kw
    ;;                                          (cons :pkg pkg-path)
    ;;                                          (cons :tgt arg))))
    ;;                        (alist-delete (list arg-kw) (cdr deps))))
    ;;             (car t))
    ;;           ;; else
    ;;           (let ((_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "arg not in deps") arg)))
    ;;                 (arg-kw (string->keyword arg)))
    ;;             (set-cdr! deps
    ;;                       (append
    ;;                        (list (cons ::tools
    ;;                                   (list (cons arg-kw
    ;;                                               `((:pkg ,pkg-path)))
    ;;                                         (cons :tgt arg))))
    ;;                        deps)))))
    ;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; may update deps
(define (-expand-literal-tool!? ws pkg-path tool tools deps)
  (mibl-trace-entry"-expand-literal-tool!?" "")
  (mibl-trace "tool" tool) ;; (type-of tool))
  (mibl-trace "tools" tools) ;; (type-of tool))
  (mibl-trace "deps" deps)

  (let ((tool (if (string-prefix? "./" tool)
                  (string-drop tool 2) tool)))
    (if (null? (cdr deps)) ;; meaning?
        ;; infer dep for tool
        (let* ((tool-kw (string->keyword (format #f "~A" tool)))
               (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (white "inferring ::unresolved for tool") tool-kw)))
               )
          (set-cdr! deps
                    (list (cons ::toolsD
                                (list
                                 (cons tool-kw ::unresolved
                                       ;; (list (cons :pkg pkg-path)
                                       ;;       (cons :tgt tool))
                                       )))))
          tool-kw)
        ;; else search existing deps
        (let ((tool (format #f "~A" tool))
              (t
               (begin
                 (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                     (format #t "~A: ~A~%" (green "searching deps") (cdr deps)))
                 (find-if (lambda (dep)
                            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                (format #t "~A: ~A~%" (green "checking dep") dep))
                            ;; dep forms:  (:<kw> (:pkg ...) (:tgt ...))
                            ;; or (foo ::unresolved), (foo ::opam-pkg)
                            ;; or
                            (cond
                              ((member (cdr dep) '(::unresolved))
                               (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                   (format #t "~A: ~A~%" (ured "::UNRESOLVED") dep))
                               #f)
                              ((member (cdr dep) '(::opam-pkg))
                               (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                   (format #t "~A: ~A~%" (ured "::opam-pkgs") dep))
                               #f)
                             ((alist? (cdr dep))
                              (let* ((lbl-list (cdr dep))
                                     (lbl-pkg (assoc-val :pkg lbl-list))
                                     (lbl-tgt (if-let ((tgt (assoc-val :tgt lbl-list)))
                                                  (equal? tool tgt)
                                                  (if-let ((tgts (assoc-val #|:tgts|# :glob lbl-list)))
                                                          (equal? tool tgts)
                                                          (error 'fixme "missing :tgt and :tgts in dep")))))
                                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                    (begin
                                      (format #t "~A: ~A~%" (uwhite "lbl-list") lbl-list)
                                      (format #t "~A: ~A~%" (uwhite "tgt") lbl-tgt)))
                                lbl-tgt))
                             (else
                              (error 'fixme
                                     (format #f "unexpected dep form: ~A" dep)))))
                          (cdr deps)))))
          (if t
              (let ((tool-kw (string->keyword tool)))
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (begin
                      (format #t "~A: ~A~%" (red "literal tool in deps") t)
                      (format #t "~A: ~A~%" (red "tool") tool)))
                ;; move it from (:deps) to (deps ::tools)
                (set-cdr! deps
                          (append
                           (list (list ::toolsE
                                       (list (car t) ;; tool-kw
                                             (cons :pkg pkg-path)
                                             (cons :tgt tool))))
                           (alist-delete (list tool-kw) (cdr deps))))
                (car t))
              ;; else did not find t in deps. try exports
              ;  ...

               ;; else fs path to some other pkg, like ../../foo/bar
              (begin
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (blue "TOOL NOT in deps") tool))
               ;; inference: must(?) be a path ref to sth in another pkg
               ;; e.g. ../gen_stubs/gen_stubs.exe
               ;; task: normalize path, split into pkg and tgt for label
               (if (string-index (format #f "~A" tool) (lambda (ch) (char=? ch #\/)))
                   ;; relative path like gen/gen.exe or ../gen_stubs/gen_stubs.exe
                   (begin
                     (let* ((tool-kw (string->keyword tool))
                            (full-path (format #f "~A/~A" pkg-path tool))
                            (canonical-path (->canonical-path full-path))
                            (pkg (dirname canonical-path))
                            (tgt (basename canonical-path)))
                       (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                           (begin
                             (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
                             (format #t "~A: ~A~%" (blue "full-path") full-path)
                             (format #t "~A: ~A~%" (blue "canonical-path") canonical-path)))
                       ;; (error 'X "STOP expand-literal-tool")
                       (set-cdr! tools `((,tool-kw (:pkg . ,pkg) (:tgt . ,tgt))))
                       ;; (set-cdr! deps
                       ;;           (append
                       ;;            `((::toolsF
                       ;;              (,tool-kw
                       ;;               (:pkg . ,pkg)
                       ;;               (:tgt . ,tgt)
                       ;;               ;;(:lbl . ,(format #f "//~A:~A" pkg tgt))
                       ;;               )))
                       ;;            (cdr deps)))
                       tool-kw))
                   ;; else must be in cwd, or in exports table to be resolved in separate pass
                   (begin
                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                         (format #t "~A: ~A~%" (yellow "tool runresolved") tool))
                     (set-cdr! deps
                               (append
                                `((::toolsG
                                   (,(string->keyword tool) . ::unresolved)))
                                (cdr deps)))
                     (string->keyword tool))))))
        )))

;; may update deps
(define (find-sigfile-in-pkg-files!? arg deps pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "find-sigfile-in-pkg-files") arg)
        (format #t "~A: ~A~%" (blue ":signatures") (assoc-val :signatures pkg))))

  (let ((sig
         (find-if (lambda (f-assoc)
                    ;; (format #t "~A: ~A~%" (white "f-assoc")
                    ;;         f-assoc)
                    (find-if (lambda (f)
                               ;; (format #t "~A: ~A (~A)~%" (white "f") (cdr f) (type-of (cdr f)))
                               (equal? (format #f "~A" arg)
                                       (format #f "~A" (cdr f))))
                             (cdr f-assoc)))
                  (if-let ((sigs (assoc-val :signatures pkg)))
                          sigs
                          '()))))
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (white "searched pkg :signatures:") sig))
    (if sig
        (let ((key (string->keyword arg)))
          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (magenta "sigfile INFERRED dep") sig))
          (set-cdr! deps (cons (cons key
                                     (list (cons :pkg (assoc-val :pkg-path pkg))
                                           (cons :tgt arg)))
                               (cdr deps)))
          key)
        ;; else search :modules
        (let ((sig
               (find-if (lambda (m-assoc)
                          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                              (format #t "~A: ~A~%" (white "m-assoc") m-assoc))
                          (find-if (lambda (m)
                                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                         (format #t "~A: ~A (~A)~%" (white "m") (cdr m) (type-of (cdr m))))
                                     (let ((ml (cdr m)))
                                       (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                           (format #t "~A: ~A~%" (white "(cdr m)") ml))
                                       (equal? (format #f "~A" arg)
                                               (format #f "~A" ml))))
                                   (cdr m-assoc)))
                        (if-let ((modules (assoc-val :modules pkg)))
                                modules
                                '()))))
          (if sig
              (let ((key (string->keyword arg)))
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (magenta "sig INFERRED DEP") sig))
                (set-cdr! deps (cons (cons key
                                           (list (cons :pkg (assoc-val :pkg-path pkg))
                                                 (cons :tgt arg)))
                                     (cdr deps)))
                key)
              (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (white "searched pkg :modules:") sig)))
              #f))))

;; may update deps
(define (find-structfile-in-pkg-files!? arg deps pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (blue "find-structfile-in-pkg-files") arg)
        (format #t "~A: ~A~%" (red ":structures") (assoc-val :structures pkg))
        (format #t "~A: ~A~%" (red ":modules") (assoc-val :modules pkg))))

  (let* ((struct-files (if-let ((files (assoc :structures pkg)))
                               (cdr files) '()))
         (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (magenta "pkg struct-files") struct-files)))
         (struct-files (append
                        (if-let ((sfiles (assoc-val :static struct-files)))
                                sfiles '())
                        (if-let ((dfiles (assoc-val :dynamic struct-files)))
                                dfiles '())))
         (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (begin
                  (format #t "~A: ~A~%" (cyan "combined struct-files") struct-files)
                  (format #t "~A~%" (uwhite "searching struct-files")))))
         (struct
          (find-if (lambda (f-assoc)
                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                         (format #t "~A: ~A~%" (white "f-assoc") f-assoc))
                     (equal? (format #f "~A" arg) (format #f "~A" (cdr f-assoc))))
                   struct-files)))
    ;; (if-let ((structs (assoc-val :structures pkg)))
    ;;         structs
    ;;         '()))))

    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (white "struct") struct))
    (if struct
        (let ((key (string->keyword arg)))
          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (magenta "structfile INFERRED dep") struct))
          (set-cdr! deps (cons (cons key
                                     (list (cons :pkg (assoc-val :pkg-path pkg))
                                           (cons :tgt arg)))
                               (cdr deps)))
          key)
        ;; else search :modules
        (let ((_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A~%" (uwhite "searching modules"))))
              (struct
               (find-if (lambda (m-assoc)
                          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                              (format #t "~A: ~A~%" (white "m-assoc") m-assoc))
                          (find-if (lambda (m)
                                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                         (format #t "~A: ~A (~A)~%" (white "m") (cdr m) (type-of (cdr m))))
                                     (let ((ml (cdr m)))
                                       (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                           (format #t "~A: ~A~%" (white "(cdr m)") ml))
                                       (equal? (format #f "~A" arg)
                                               (format #f "~A" ml))))
                                   (cdr m-assoc)))
                        (if-let ((modules (assoc-val :modules pkg)))
                                modules
                                '()))))
          (if struct
              (let ((key (string->keyword arg)))
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (magenta "INFERRED DEP") struct))
                (set-cdr! deps (cons (cons key
                                           (list (cons :pkg (assoc-val :pkg-path pkg))
                                                 (cons :tgt arg)))
                                     (cdr deps)))
                key)
              (begin
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (ublue "not found in pkg files") arg))
                #f))))))

;; updates :outputs. called by normalize-action-write-file if no (targets)
;; targets should be (:outputs)
(define (-infer-output! arg targets pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A (~A)~%" (blue "-infer-output!") arg (type-of arg))
        (format #t "~A: ~A~%" (red "targets") targets)
        (format #t "~A: ~A~%" (red ":signatures") (assoc-val :signatures pkg))
        (format #t "~A: ~A~%" (red ":structures") (assoc-val :structures pkg))
        (format #t "~A: ~A~%" (red ":modules") (assoc-val :modules pkg))))

  ;; if arg in targets return
  ;; else search pkgfiles

  ;; search pkg files - if found then we're overwriting? should not happen?
  ;; expected: arg is not found in pkg files, so we
  ;; 1. add to pkg files
  ;; 2. add to :targets

  (cond
   ((eq? (fnmatch "*.ml" (format #f "~A" arg) 0) 0)
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (red "Matched *.ml") arg))
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

      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (format #t "~A: ~A~%" (white "struct") struct))
      (if struct
          (let ((key (string->keyword arg)))
            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (magenta "INFERRED DEP") struct))
            (set-cdr! deps (cons (cons key
                                       (list (cons :pkg (assoc-val :pkg-path pkg))
                                             (cons :tgt arg)))
                                 (cdr deps)))
            key)
          ;; else search :modules
          (let ((struct (find-if (lambda (m-assoc)
                                   (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                       (format #t "~A: ~A~%" (white "m-assoc") m-assoc))
                                   (find-if (lambda (m)
                                              (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                                  (format #t "~A: ~A (~A)~%" (white "m") (cdr m) (type-of (cdr m))))
                                              (let ((ml (cdr m)))
                                                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                                    (format #t "~A: ~A~%" (white "(cdr m)") ml))
                                                (equal? (format #f "~A" arg)
                                                        (format #f "~A" ml))))
                                            (cdr m-assoc)))
                                 (if-let ((modules (assoc-val :modules pkg)))
                                         modules
                                         '()))))
            (if struct
                (let ((key (string->keyword arg)))
                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      (format #t "~A: ~A~%" (magenta "INFERRED DEP") struct))
                  (set-cdr! deps (cons (cons key
                                             (list (cons :pkg (assoc-val :pkg-path pkg))
                                                   (cons :tgt arg)))
                                       (cdr deps)))
                  key)
                ;; else no matching file
                ;; PROBLEM: distinguish between string args and labels of targets in other pkgs
                ;; solution: assume "foo.ml" is a file name
                (begin
                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      (format #t "~A: ~A~%" (red "NO MATCH in pkg files for") arg))
                  ;; 1. add to pkg files :structures or :modules
                  ;; 2. add to :targets
                  ;;(string->keyword arg)
                  arg))))))

   ((eq? (fnmatch "*.mli" (format #f "~A" arg) 0) 0)
    (error 'fixme (format #f "not yet implemented for: ~A" arg)))

   ;;((eq? (fnmatch "*.mli" (format #f "~A" arg) 0) 0)
   ((string=? "%{target}" (format #f "~A" arg))
    ;; targets should be a singleton list
    ;; e.g. (:outputs (:foo.txt (:pkg "a/b") (:tgt "foo.txt")))
    (let ((targets (car (cdr targets))))
      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (format #t "~A: ~A~%" (red "targets") targets))
      (car targets)))

   (else
    (let ((f (find-file-in-pkg-files!? arg targets pkg)))
      (if f
          (update-tagged-label-list! arg targets pkg))))))

;; search pkg files for arg, if found update deps
(define (-infer-dep! ws arg deps pkg)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A (~A)~%" (ublue "-infer-dep!") arg (type-of arg))
        (format #t "~A: ~A~%" (blue "pkg") pkg)
        (format #t "~A: ~A~%" (blue "deps") deps)
        (format #t "~A: ~A~%" (green "pkg-signatures") (assoc-val :signatures pkg))
        (format #t "~A: ~A~%" (green "pkg-structures") (assoc-val :structures pkg))
        (format #t "~A: ~A~%" (green "pkg-modules") (assoc-val :modules pkg))
        (format #t "~A: ~A~%" (green "pkg-files") (assoc-val :files pkg))))

  (let ((arg (format #f "~A" arg))) ;; (keyword->symbol arg))))
    (cond
     ((eq? 0 (fnmatch "*.ml" arg 0))
      (find-structfile-in-pkg-files!? arg deps pkg))

     ((eq? 0 (fnmatch "*.mli" arg 0))
      (find-sigfile-in-pkg-files!? arg deps pkg))

     (else
      (let ((f (find-file-in-pkg-files!? arg deps pkg)))
        (if (or *mibl-debug-expanders* *mibl-debug-s7*)
            (format #t "~A: ~A~%" (red "found file in pkg files?") f))
        (if f
            (update-tagged-label-list! arg deps pkg)
                #f))
      ;; (if (string? arg)
      ;;     ;; assume not a file to generate
      ;;     (begin
      ;;       (format #t "~A: ~A~%" (bgyellow "arg is string") arg)
      ;;       #f)
      ;;     ;; else search :files
      ;;     )
      ))))

;; string args may include suffixed vars, e.g. %{test}.corrected
(define expand-string-arg
  (lambda (ws arg pkg targets deps)
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (begin
          (format #t "~A: ~A\n" (ublue "expand-string-arg") arg)
          (format #t "  ~A: ~A\n" (blue "targets") targets)
          (format #t "  ~A: ~A\n" (blue "deps") deps)))

    (cond
     ((string=? "./" arg) ::pkg-dir)

     ((string-prefix? "./" arg)
      (expand-string-arg ws (string-left-trim "./" arg) pkg targets deps))

     ;; FIXME: pct-vars should already be handled by -expand-pct-arg?
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
      (error 'fixme "unimplemented: %{target}"))

     ;; pass std %{<kind>:foo} as ::kind:foo, let resolver handle
     ;; by looking them up in the exports table
     ((string-prefix? "%{" arg)
      ;; %{foo} or %{foo}.suffix
      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (begin
            (format #t "VAR: ~A\n" arg)
            (format #t "deps: ~A\n" deps)))
      (let ((pkg-path (assoc-val :pkg-path pkg)))
        (-expand-pct-arg!? ws arg :arg pkg deps)))

     (else
      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (format #t "~A: ~A~%" (white "String literal") arg))
      ;; arg is string. check deps and targets, then pkg files
      (if-let ((tag (deps->tag-for-file deps arg)))
              (begin
                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (green "File tag") tag))
                tag)
              (if-let ((tag (targets->tag-for-file targets arg)))
                      tag
                      ;; else search pkg files for file not listed in :deps
                      ;; if found, -infer-dep! adds to :deps
                      ;; (if-let ((inferred-dep (-infer-dep! ws arg deps pkg)))
                      ;;         inferred-dep
                      ;;         arg)
                      (if-let ((inferred-dep
                                (-infer-dep! ws arg #|search-key|# deps pkg)))
                              inferred-dep
                              (begin
                                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                    (format #t "~A: ~A~%" (red "string dep unresolved") arg))
                                ;; first search exports tbl
                                ;; pointless, tbl is incomplete?
                                (if-let ((found (find-in-exports ws (string->keyword arg))))
                                        (let* ((pkg (assoc-val :pkg found))
                                               (tgt (assoc-val :tgt found))
                                               (entry `((,(string->keyword arg) (:pkg . ,pkg) (:tgt . ,tgt)))))
                                          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                              (format #t "~A: ~A~%" (red "found") found))
                                          ;; (error 'STOP "STOP exp")
                                          entry)
                                        ;; we've searched deps, pkg files, exports with no luck.
                                        ;; its either an unresolved dep, or an output file, or a string arg
                                        ;; mark it unresolved and let later pass handle it?
                                        (let ((key (string->keyword arg)))
                                          (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                              (begin
                                                (format #t "~A: ~A~%" (bgred "unresolved string") arg)
                                                (format #t "~A: ~A~%" (bgred " deps") deps)))
                                          ;; (update-tagged-label-list! kw deps pkg)
                                          `(::unresolved ,arg)
                                          ;; (set-cdr! deps
                                          ;;           (append (cdr deps)
                                          ;;                   `((,key . ::unresolved))))
                                          ;; key
                                          ))
                                ))
                      )))
     ;; (cons
     ;;  arg
     ;;  ;;(resolve-string-arg pkg-path arg vars)
     ;;  (expand-cmd-args* (cdr args) targets deps))))))
     )))

(define expand-cmd-args*
  (lambda (ws args pkg targets deps)
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (begin
          (format #t "~A\n" (ublue "expand-cmd-args*"))
          (format #t "  ~A: ~A\n" (blue "args") args)
          (format #t "  ~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg))
          (format #t "  ~A: ~A\n" (blue "targets") targets)
          (format #t "  ~A: ~A\n" (blue "deps") deps)))

    (if (list? args)
        (let ((result
               (if (null? args)
                   (begin
                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                         (format #t "~A, returning: ~A~%" "bottomed out" '()))
                     '())

                   ;; (let ((arg (if (symbol? (car args))
                   ;;                (symbol->string (car args))
                   ;;                (car args))))
                   (let ((arg (car args)))
                     (cond
                      ((pair? arg) ;; e.g. (:_string "struct")
                       (let ((subarg (expand-cmd-args* ws (car args) pkg targets deps)))
                         (cons subarg
                               (expand-cmd-args* ws (cdr args) pkg targets deps))))
                      ;; (expand-cmd-args* pkg-path
                      ;;                  target targets
                      ;;                  (cdr args) filedeps vars)))

                      ((number? arg)
                       (cons arg (expand-cmd-args* ws (cdr args) pkg targets deps)))

                      ((symbol? arg)
                       (let ((arg-str (format #f "~A" arg)))
                         (if (string-prefix? "%{" arg-str)
                             (begin
                               (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                   (format #t "~A: ~A~%" (green "arg is pct-var") arg))
                               (if (or (eq? arg '%{target}) (eq? arg '%{targets}))
                                   ;; %{target}, %{targets} directly translate to :outputs
                                   (cons :outputs (expand-cmd-args* ws (cdr args) pkg targets deps))
                                   ;; else expand the pct-var
                                   (let* ((pkg-path (assoc-val :pkg-path pkg))
                                          (arg (-expand-pct-arg!? ws arg :arg pkg deps)))
                                     (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                         (format #t "~A: ~A~%" (red "expanded pct-var") arg))
                                     (cons arg
                                           (expand-cmd-args* ws (cdr args) pkg targets deps)))))

                             ;; else arg is sym but not pct-var.
                             ;; case 1: arg is a cmd string arg (not a filename)
                             ;; case 2: arg names a file
                             ;;   case 2a: file is input
                             ;;   case 2b: file is output
                             ;;   if its the last arg, probably output - but maybe not
                             ;;   check cmd - for some (e.g. copy, cmp) we can infer role
                             ;;   eg: (copy lib:foo:bar.mli bar.mli)
                             ;;   rule: if we cannot resolve qua file, and it looks like a filename
                             ;;   (e.g. due to extension), treat it as output file.
                              (begin
                                (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                    (format #t "~A: ~A~%" (green "arg is symbol") arg))
                                ;; try to expand it as string (i.e. filename).
                                ;; if that fails, mark it unresolved.
                                (let ((x (expand-cmd-args* ws
                                                           (list arg-str)
                                                           pkg targets deps)))
                                  ;; (format #t "~A: ~A~%" (bgred "XXXXXXXXXXXXXXXX") x)
                                  (if (and (list? (car x)) (eq? ::unresolved (caar x)))
                                      (cons `(::unresolved ,arg)
                                            (expand-cmd-args* ws (cdr args)
                                                        pkg targets deps))
                                       (cons (car x) (expand-cmd-args* ws (cdr args)
                                                        pkg targets deps))))
                                ))))
                      ;; old impl:
                      ;; (if (or (eq? arg '%{target}) (eq? arg '%{targets}))
                      ;;      (cons :outputs
                      ;;            (expand-cmd-args* ws (cdr args) pkg targets deps))

                      ;;      (let ((arg-str (format #f "~A" arg)))
                      ;;        (cond
                      ;;         ((string-prefix? "%{" arg-str)
                      ;;          ;; %{foo} or %{foo}.suffix
                      ;;          (let* ((pkg-path (assoc-val :pkg-path pkg))
                      ;;                 (arg (-expand-pct-arg!? ws arg :arg pkg deps)))
                      ;;            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      ;;                (format #t "~A: ~A~%" (uwhite "expanded arg") arg))
                      ;;            (cons arg
                      ;;                  (expand-cmd-args* ws (cdr args) pkg targets deps))))
                      ;;         ( ;; else sym but not pct-var
                      ;;          (expand-cmd-args* ws (cons arg-str (cdr args))
                      ;;                            pkg targets deps))))))

                      ((string? arg)
                       (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                           (format #t "~A: ~S~%" (red "arg is string") arg))
                       (if (string=? arg "")
                           (begin
                             (append (list :null-string)
                                     (expand-cmd-args* ws (cdr args) pkg targets deps)))
                           (if (char=? #\- (arg 0))
                               (append (list arg)
                                       (expand-cmd-args* ws (cdr args) pkg targets deps))
                               (let ((sarg (expand-string-arg ws arg pkg targets deps)))
                                 (append (list sarg)
                                         (expand-cmd-args* ws (cdr args) pkg targets deps))))))
                      ;; pkg-path target targets
                      ;; (cdr args) filedeps vars)))))

                      (else ; not number, pair, string
                       (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                           (format #t
                               "WARNING: not a nbr, pair, or string: ~A\n" arg))
                       ))
                     ))))
          ;; (format #t "~A: ~A\n" (ucyan "expanded-cmd-args") result)
          result)
        (cons arg (expand-cmd-args* ws (car args) pkg targets deps)))
        ))

(define expand-targets
  (lambda (ws pkg targets deps)
    (if (or *mibl-debug-expanders* *mibl-debug-s7*)
        (begin
          (format #t "~A\n" (ublue "expand-targets"))
          (format #t "~A: ~A\n" (blue "ws") ws)
          (format #t "~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg))
          (format #t "~A: ~A\n" (blue "targets") targets)
          (format #t "~A: ~A\n" (blue "deps") deps)))

    (let ((terms (expand-terms* ws targets pkg '())))
      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
          (format #t "~A: ~A\n" (red "expanded terms") terms))
      terms)))

;; expands items, e.g. pct-vars like %{deps}
(define (expand-terms* ws arglist pkg expanded-deps)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A\n" (ublue "expand-terms*"))
        (format #t "~A: ~A\n" (blue "arglist") arglist)
        (format #t "~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg))
        (format #t "~A: ~A\n" (blue "expanded-deps") expanded-deps)))
  ;; (let ((pkg-path (assoc-val :pkg-path pkg))
  ;;       (ws-root (assoc-val :ws-path pkg)))
  (if (null? arglist)
      expanded-deps
      (if (pair? (car arglist))
          (begin
            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (format #t "~A: ~A (t: ~A)~%" (bgred "term pair") (car arglist) (type-of (caar arglist))))
            (if (equal? 'package (caar arglist))
                (begin
                  ;; in executables, (package p) just means its part of that pkg
                  ;; in rules, (deps (package p)) means depend on everything in p
                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      (format #t "~A: ~A~%" (bgred "skipping term") (car arglist)))
                  (expand-terms* ws (cdr arglist) pkg expanded-deps))
                (let ((front (expand-terms* ws (car arglist) pkg '())))
                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                      (begin
                        (format #t "~A: ~A~%" (ured "expanded front") front)
                        (format #t "~A: ~A~%" (ured "now expanding") (cdr arglist))))
                  (expand-terms* ws (cdr arglist) pkg (append expanded-deps  front))
                  )))
          ;; car is atom
          (let* ((kw (car arglist)))
            (if-let ((depfn (assoc-val kw dune-dep-handlers)))
                    (let ((res (apply (car depfn)
                                      (list ws pkg
                                            arglist))))
                      (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                          (begin
                            (format #t "depfn res: ~A\n" res)
                            (format #t "expanded-deps: ~A\n" expanded-deps)))
                      ;; we're done, depfn consumed cdr
                      ;; return expanded-deps
                      ;; (if (null? expanded-deps)
                      ;;     (if (pair? (car res)) res (list res))
                      (append (if (pair? (car res)) res (list res)) expanded-deps))

                    ;; else car of arglist not in our handlers list
                    ;; must be either a ':' tagged dep or a filename literal
                    ;; or, if its a cmd arg, may be %{foo}
                    ;; or (package ...) or etc.

                    ;; convert to string and dispatch
                    (let ((dep (format #f "~A" (car arglist))))
                      (cond
                       ((char=? #\: (string-ref dep 0))
                        (begin
                          ;; (format #t "TAGGED DEP : ~A\n" arglist)
                          (handle-tagged-dep
                           ws arglist pkg expanded-deps)))

                       ;; ((char=? #\% (string-ref dep 0))
                       ;;  )

                       (else
                        (if-let ((found (find-in-exports ws (string->keyword dep))))
                                (begin
                                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                      (format #t "~A: ~A~%" (bggreen "xxxxxxxxxxxxxxxx") found))
                                  (let* ((pkg (assoc-val :pkg found))
                                         (tgt (assoc-val :tgt found))
                                         (entry `((,(string->keyword dep) (:pkg . ,pkg) (:tgt . ,tgt)))))
                                    entry))
                                ;; else must be a filename literal?
                                ;; return (:static <path> <fname>)
                                ;; or (:dynamic <path> <fname>)
                                (begin
                                  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                      (format #t "LIT DEP : ~A\n" arglist))
                                  (handle-filename-literal-dep
                                   ws dep arglist pkg
                                   ;; stanza-alist
                                   expanded-deps)))))))
            ))))

(define (expand-rule-deps ws pkg stanza-alist)
  ;; updates stanza-alist
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A\n" (ublue "expand-rule-deps"))
        (format #t "~A: ~A\n" (blue "ws") ws)
        (format #t "~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg))
        (format #t "~A: ~A\n" (blue "stanza-alist") stanza-alist)))

  ;; (let ((stanza-alist (cdr stanza)))

  (if-let ((deps-assoc (assoc 'deps stanza-alist)))
          (let* ((deplist (assoc-val 'deps stanza-alist))
                 (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                        (format #t "main deplist: ~A\n" deplist)))
                 ;; (stdout (assoc-in '(action with-stdout-to) stanza-alist))
                 ;; (stdout (if stdout (cadr stdout)))
                 ;; (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "stdout") stdout)))
                 ;; (deplist (if stdout (cons stdout deplist) deplist))
                 (_ (if (or *mibl-debug-expanders* *mibl-debug-s7*) (format #t "~A: ~A\n" (green "DEPLIST") deplist)))
                 (result (expand-terms* ws deplist pkg '())))
            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                (format #t "~A: ~A\n" (green "DEPLIST EXPANDED") result))
            `(:deps ,@result))
          #f))

(define (expand-cmd-list ws pkg -raw-cmds tools targets deps)
  (if (or *mibl-debug-expanders* *mibl-debug-s7*)
      (begin
        (format #t "~A\n" (ublue "expand-cmd-list"))
        (format #t "~A: ~A~%" (green "-raw-cmds") -raw-cmds)
        (format #t "~A: ~A~%" (green "targets") targets)
        (format #t "~A: ~A~%" (green "deps") deps)))

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
                      ,@(expand-cmd-args* ws (cdr raw-cmds) pkg targets deps))))

                  (if (equal? kw 'run)
                      ;; skip 'run'
                      (recur (cdr raw-cmds) tool expanded-cmds
                             (expand-cmd-args* ws args pkg targets deps))

                      ;; atom or list following 'run'
                      (if (pair? kw)
                          ;; e.g. (run (cmd ...))
                          (recur kw tool expanded-cmds
                                 (expand-cmd-args* ws args pkg targets deps))

                          ;; kw in in list of dune std tools, must be
                          ;; (run %{bin:foo} ...), (run ../foo/bar.exe) etc.
                          ;; e.g. (run ../gen_stubs/gen_stubs.exe %{jsoo} --except %{runtime})
                          (let ((tmp
                                  `((:tool
                                     ,(expand-run-tool ws kw tools pkg targets deps))
                                    (:args
                                      ,@(expand-cmd-args* ws (cdr raw-cmds)
                                                          pkg targets deps)))))
                            (if (or *mibl-debug-expanders* *mibl-debug-s7*)
                                (format #t "~A: ~A~%" (bggreen "tmp") tmp))
                            ;; (error 'X "STOP expanded run-tool")
                            tmp)))))))))
                  ;; ;; else must be an arg
                  ;; (expand-cmd-args* (cdr raw-cmds) pkg targets deps)))

             ;; ((equal? 'run (car raw-cmds))
             ;;  (format #t "RUN ~A\n" raw-cmds)
             ;;  (recur (cdr raw-cmds)
             ;;         (append expanded-cmds initial)
             ;;         targets deps))
             ;; (else ;; car not chdir, not run
             ;;  (error 'wtf2 "WTF2? ~A" (car raw-cmds)))
             ;; ))))

(provide 'expanders.scm)

(if *mibl-debug-s7-loads*
    (format #t "loaded dune/expanders.scm\n"))
