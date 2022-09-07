;; (display "loading dune/expanders.scm\n")

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
(define expand-run-tool
  (lambda (ws tool pkg targets deps)
    (format #t "~A: ~A (~A)~%" (ublue "expand-run-tool") tool (type-of tool))
    (format #t "~A: ~A~%" (green "deps") deps)

    (case tool
      ((cp) ::copy) ;;FIXME: use lookup table from constants.scm
      ((%{deps}) ::deps)
       ;; (set-cdr! deps
       ;;           (append
       ;;            (list (cons ::tools
       ;;                        (list
       ;;                         (cons search-key ;; arg-kw
       ;;                               ;; (list (cons :pkg pkg-path)
       ;;                               ;;       (cons :tgt arg))
       ;;                               ::import
       ;;                               ))))
       ;;            (cdr deps)))
       ;; :FOO)

      (else
       (let ((tool (format #f "~A" tool)))
         (cond
          ((string-prefix? "%{" tool)
           (-expand-pct-tool!? ws tool :tool pkg deps))

          (else
           (let ((tmp (-expand-literal-tool!? ws
                       (car (assoc-val :pkg-path pkg))
                       tool deps)))
             (format #t "~A: ~A~%" (ured "TMP") tmp)
             (format #t "~A: ~A~%" (ured "DEPS") deps)
             tmp))))))))

(define (-match-dep pfx key dep)
  (format #t "~A: ~A/~A ? ~A~%" (ublue "-match-dep") pfx key dep)
  (cond
   ((equal? (car dep) key)
    (format #t "~A: ~A~%" (uwhite "matched?") dep)
    #t)
   ((member (cadr dep) '(::import ::pkg)) #f)
   (else #f)))

;; may update :deps
(define (-expand-dep-pct-var!? ws pfx sym deps pkg)
  (format #t "~A: pfx: ~A; sym: ~A~%"
          (ublue "-expand-dep-pct-var!?") pfx sym)
  ;; arg form:  dep:<path>
  ;; search deps for <path>
  ;; if not found, search pkg files? no need?
  ;; add it to pkg files as :dynamic?
  ;; add it to :deps
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (search-key (symbol->keyword sym))
         (_ (format #t "~A: ~A~%" (yellow "searching deps") deps))
         (d (find-if (lambda (dep)
                       ;;FIXME: special tags ::tools, ::pkgs, ::import, etc.
                       (-match-dep pfx search-key dep))
                       ;; (equal? (car dep) search-key))
                     (cdr deps))))
    (format #t "~A: ~A~%" (yellow "found?") d)
    (if d
        search-key
        ;; not in :deps; search pkg files and add it to :deps if found
        (let ((v (format #f "~A" sym))
              (_ (format #t "~A: ~A~%" (yellow "searching pkg files") deps))
              (expanded
               (if-let ((tag (deps->tag-for-file deps search-key)))
                       tag
                       ;; else search pkg files for file not listed in :deps
                       ;; if found, -infer-dep! adds to :deps
                       (if-let ((inferred-dep
                                 (-infer-dep! sym #|search-key|# deps pkg)))
                               inferred-dep
                               (begin
                                 (format #t "~A: ~A~%"
                                         (red "unresolved") search-key)

                                 (let ((x (handle-filename-literal-arg ws sym pkg)))
                                   (format #t "~A: ~A~%" (bgred "RESOLVED") x)
                                   (format #t "~A: ~A~%" (red "deps") deps)
                                   (set-cdr! deps (append (cdr deps) x))
                                   x)

                                 ;; (let* ((path (dirname sym))
                                 ;;        (dep-path (if (string=? "./" path)
                                 ;;                      pkg-path
                                 ;;                      path)))
                                 ;; (set-cdr! deps
                                 ;;           (append (cdr deps)
                                 ;;                   `((,search-key
                                 ;;                      (:pkg . ,dep-path)
                                 ;;                      (:tgt . ,(basename sym)))))))
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
;; (%{dep:rejections.sh} => (:deps (:rejections.sh (:pkg...)(:tgt...)))
;;  %{bin:tezos-protocol-compiler} => (:deps (::bin tezos...))
;;  %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL})
;;      =>(:deps (::lib tezos...))
(define (-expand-pct-arg!? ws arg kind pkg deps)
  (format #t "~A: ~A (type: ~A)~%" (ublue "-expand-pct-arg!?")
          arg (type-of arg))
  (format #t "deps: ~A~%" deps)

  (case arg
    ((%{deps})
      ;; special case
     ::deps)

    (else
     (let-values (((sym pfx sfx) (parse-pct-var arg)))
       (format #t "~A: ~A, ~A: ~A~%"
               (uwhite "arg pfx") pfx (uwhite "sfx") sfx)

       (if pfx
           (if (equal? :dep pfx)
               (begin
                 (format #t "~A: ~A~%" (uwhite ":dep pfx") arg)
                 (let ((x (-expand-dep-pct-var!? ws pfx sfx deps pkg)))
                   (format #t "~A: ~A~%" (uwhite "dep pctvar") x)
                   ;; (set-cdr! deps
                   ;;           (append
                   ;;            (cdr deps)))
                   x))
               (begin
                 ;; prefixed pctvars will never be already in :deps?
                 ;; e.g. lib:foo, bin:foo, etc.
                 (format #t "~A: ~A~%" (ured "deps before") deps)
                 (set-cdr! deps
                           (append
                            (list (list (symbol->keyword sym)
                                        ;;(symbol->keyword pfx)
                                        ::import))
                            (cdr deps)))
                 (format #t "~A: ~A~%" (ured "deps after") deps)
                 (symbol->keyword sym)))
           ;; else no pfx
           (let* ((search-key (pct-var->keyword arg))
                  (_ (format #t "~A: ~A~%" (yellow "search-key") search-key))

                  ;; special case: %{deps}

                  (match
                   ;; find arg in deps
                   (find-if (lambda (dep)
                              (format #t "~A: ~A~%" (yellow "checking dep") dep)
                              ;; if dep == (::pkgs ...) ?
                              (equal? (car dep) search-key))
                            (cdr deps))))
             (format #t "~A: ~A~%" (yellow "match?") match)
             (if match
                 search-key
                 ;; else not in :deps
                 (error 'fixme
                        (format #f "arg not in deps: ~A" search-key))
                 )))))))

;; may update deps
(define (-expand-pct-tool!? ws arg kind pkg deps)
  (format #t "~A: ~A (~A)~%" (ublue "-expand-pct-tool!?")
          arg (type-of arg))
  (format #t "deps: ~A~%" deps)

  (let-values (((sym pfx sfx) (parse-pct-var arg)))
    (format #t "~A: ~A, ~A: ~A~%"
            (uwhite "arg pfx") pfx (uwhite "sfx") sfx)

    ;; find arg in deps
    (let* ((search-key  (symbol->keyword sfx)) ;; (pct-var->keyword arg))
           (_ (format #t "~A: ~A (kw? ~A)~%" (yellow "search-key")
                      search-key (keyword? search-key)))
           (match (find-if (lambda (dep)
                             (format #t "~A: ~A (~A)~%"
                                     (yellow "testing dep") dep (type-of (car dep)))
                             ;; (format #t "~A: ~A~%" (yellow "kw?") (keyword?
                             ;;                                       (car dep)))
                             (equal? (car dep) search-key))
                           (cdr deps))))
      (format #t "~A: ~A~%" (yellow "match?") match)
      (if match
          ;;(let ((arg-kw (string->keyword arg)))
          ;; move it from (:deps) to (:deps ::tools)
          (begin
            (format #t "~A: ~A~%" (red "tool in deps") match)
            (set-cdr! deps
                      (append
                       (list (list ::tools match
                                   #| (list search-key ;; arg-kw
                                   (cons :pkg pkg-path)
                                   (cons :tgt arg))|# ))
                       (alist-delete (list search-key #|arg-kw|# ) (cdr deps)))))

          ;; else infer it must be imported from exports tbl
          (begin
            (format #t "~A: ~A, ~A~%" (red "tool NOT in deps") search-key deps) ;; t)
            (set-cdr! deps
                      (append
                       (list (cons ::tools
                                   (list
                                    (cons search-key ;; arg-kw
                                          ;; (list (cons :pkg pkg-path)
                                          ;;       (cons :tgt arg))
                                          ::import
                                          ))))
                       (cdr deps)))
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
    ;;           (let ((_ (format #t "~A: ~A~%" (red "arg not in deps") arg))
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
(define (-expand-literal-tool!? ws pkg-path tool deps)
  (format #t "~A: ~A (~A)~%" (ublue "-expand-literal-tool!?")
          tool (type-of tool))
  (format #t "deps: ~A~%" deps)

  (let ((tool (if (string-prefix? "./" tool)
                  (string-drop tool 2) tool)))
    (if (null? (cdr deps))
        ;; infer dep for tool
        (let* ((tool-kw (string->keyword (format #f "~A" tool)))
               (_ (format #t "~A: ~A~%" (white "inferring ::import for tool") tool-kw))
               )
          (set-cdr! deps
                    (list (cons ::tools
                                (list
                                 (cons tool-kw ::import
                                       ;; (list (cons :pkg pkg-path)
                                       ;;       (cons :tgt tool))
                                       )))))
          tool-kw)
        ;; else search existing deps
        (let ((tool (format #f "~A" tool))
              (t
               (begin
                 (format #t "~A: ~A~%" (green "searching deps") (cdr deps))
                 (find-if (lambda (dep)
                            (format #t "~A: ~A~%" (green "find test dep") dep)
                            ;; dep forms:  (:<kw> (:pkg ...) (:tgt ...))
                            ;; or (foo ::import), (foo ::pkg)
                            ;; or
                            (cond
                              ((member (cadr dep) '(::import ::pkg))
                               (format #t "~A: ~A~%" (ured "IMPORT") dep)
                               #f)
                             ((alist? (cdr dep))
                              (let* ((lbl-list (cdr dep))
                                     (lbl-pkg (assoc-val :pkg lbl-list))
                                     (lbl-tgt (if-let ((tgt (assoc-val :tgt lbl-list)))
                                                  (equal? tool tgt)
                                                  (if-let ((tgts (assoc-val #|:tgts|# :globs lbl-list)))
                                                          (equal? tool tgts)
                                                          (error 'fixme "missing :tgt and :tgts in dep")))))
                                (format #t "~A: ~A~%" (uwhite "lbl-list") lbl-list)
                                (format #t "~A: ~A~%" (uwhite "tgt") lbl-tgt)
                                lbl-tgt))
                             (else
                              (error 'fixme
                                     (format #f "unexpected dep form: ~A" dep)))))
                          (cdr deps)))))
          (if t
              (let ((tool-kw (string->keyword tool)))
                (format #t "~A: ~A~%" (red "tool in deps") t)
                ;; move it from (:deps) to (deps ::tools)
                (set-cdr! deps
                          (append
                           (list (list ::tools
                                       (list tool-kw
                                             (cons :pkg pkg-path)
                                             (cons :tgt tool))))
                           (alist-delete (list tool-kw) (cdr deps))))
                (car t))
              ;; else
              (let ((tool-kw (string->keyword tool)))
                (format #t "~A: ~A~%" (red "tool NOT in deps") tool)
                (set-cdr! deps
                          (append
                           (list (cons ::tools
                                      (list (cons tool-kw
                                                  `((:pkg ,pkg-path)))
                                            (cons :tgt tool))))
                           (cdr deps))))))
        )))

;; may update deps
(define (find-sigfile-in-pkg-files!? arg deps pkg)
  (format #t "~A: ~A~%" (blue "find-sigfile-in-pkg-files") arg)
  (format #t "~A: ~A~%" (red ":signatures") (assoc-val :signatures pkg))

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
    (format #t "~A: ~A~%" (white "sig") sig)
    (if sig
        (let ((key (string->keyword arg)))
          (format #t "~A: ~A~%" (magenta "sigfile INFERRED dep") sig)
          (set-cdr! deps (cons (cons key
                                     (list (cons :pkg (assoc-val :pkg-path pkg))
                                           (cons :tgt arg)))
                               (cdr deps)))
          key)
        ;; else search :modules
        (let ((sig
               (find-if (lambda (m-assoc)
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
          (if sig
              (let ((key (string->keyword arg)))
                (format #t "~A: ~A~%" (magenta "sig INFERRED DEP") sig)
                (set-cdr! deps (cons (cons key
                                           (list (cons :pkg (assoc-val :pkg-path pkg))
                                                 (cons :tgt arg)))
                                     (cdr deps)))
                key)
              #f)))))

;; may update deps
(define (find-structfile-in-pkg-files!? arg deps pkg)
  (format #t "~A: ~A~%" (blue "find-structfile-in-pkg-files") arg)
  (format #t "~A: ~A~%" (red ":structures") (assoc-val :structures pkg))
  (format #t "~A: ~A~%" (red ":modules") (assoc-val :modules pkg))

  (let* ((struct-files (if-let ((files (assoc :structures pkg)))
                               (cdr files) '()))
         (_ (format #t "~A: ~A~%" (magenta "pkg struct-files") struct-files))
         (struct-files (append
                        (if-let ((sfiles (assoc-val :static struct-files)))
                                sfiles '())
                        (if-let ((dfiles (assoc-val :dynamic struct-files)))
                                dfiles '())))
         (_ (format #t "~A: ~A~%" (cyan "combined struct-files") struct-files))
         (_ (format #t "~A~%" (uwhite "searching struct-files")))
         (struct
          (find-if (lambda (f-assoc)
                    (format #t "~A: ~A~%" (white "f-assoc") f-assoc)
                    (equal? (format #f "~A" arg) (format #f "~A" (cdr f-assoc))))
                   struct-files)))
                  ;; (if-let ((structs (assoc-val :structures pkg)))
                  ;;         structs
                  ;;         '()))))

    (format #t "~A: ~A~%" (white "struct") struct)
    (if struct
        (let ((key (string->keyword arg)))
          (format #t "~A: ~A~%" (magenta "structfile INFERRED dep") struct)
          (set-cdr! deps (cons (cons key
                                     (list (cons :pkg (assoc-val :pkg-path pkg))
                                           (cons :tgt arg)))
                               (cdr deps)))
          key)
        ;; else search :modules
        (let ((_ (format #t "~A~%" (uwhite "searching modules")))
              (struct
               (find-if (lambda (m-assoc)
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
              (begin
                (format #t "~A: ~A~%" (ublue "not found in pkg files") arg)
                #f))))))

;; updates :outputs. called by normalize-action-write-file if no (targets)
;; targets should be (:outputs)
(define (-infer-output! arg targets pkg)
  (format #t "~A: ~A (~A)~%" (blue "-infer-output!") arg (type-of arg))
  (format #t "~A: ~A~%" (red "targets") targets)
  (format #t "~A: ~A~%" (red ":signatures") (assoc-val :signatures pkg))
  (format #t "~A: ~A~%" (red ":structures") (assoc-val :structures pkg))
  (format #t "~A: ~A~%" (red ":modules") (assoc-val :modules pkg))

  ;; if arg in targets return
  ;; else search pkgfiles

  ;; search pkg files - if found then we're overwriting? should not happen?
  ;; expected: arg is not found in pkg files, so we
  ;; 1. add to pkg files
  ;; 2. add to :targets

  (cond
   ((eq? (fnmatch "*.ml" (format #f "~A" arg) 0) 0)
    (format #t "~A: ~A~%" (red "Matched *.ml") arg)
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
                  ;; solution: assume "foo.ml" is a file name
                  (begin
                    (format #t "~A: ~A~%" (red "NO MATCH in pkg files for") arg)
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
      (format #t "~A: ~A~%" (red "targets") targets)
      (car targets)))

   (else
    (let ((f (find-file-in-pkg-files!? arg targets pkg)))
      (if f
          (update-tagged-label-list! arg targets pkg))))))

;; search pkg files for arg, if found update deps
(define (-infer-dep! arg deps pkg)
  (format #t "~A: ~A (~A)~%" (ublue "-infer-dep!") arg (type-of arg))
  ;; (format #t "~A: ~A~%" (uwhite "pkg") pkg)
  (format #t "~A: ~A~%" (uwhite "deps") deps)
  (format #t "~A: ~A~%" (uwhite "pkg-signatures") (assoc-val :signatures pkg))
  (format #t "~A: ~A~%" (uwhite "pkg-structures") (assoc-val :structures pkg))
  (format #t "~A: ~A~%" (uwhite "pkg-modules") (assoc-val :modules pkg))
  (format #t "~A: ~A~%" (uwhite "pkg-files") (assoc-val :files pkg))

  (let ((arg (format #f "~A" arg))) ;; (keyword->symbol arg))))
    (cond
     ((eq? 0 (fnmatch "*.ml" arg 0))
      (find-structfile-in-pkg-files!? arg deps pkg))

     ((eq? 0 (fnmatch "*.mli" arg 0))
      (find-sigfile-in-pkg-files!? arg deps pkg))

     (else
      (if (string? arg)
          ;; assume not a file to generate
          #f
          ;; else search :files
          (let ((f (find-file-in-pkg-files!? arg deps pkg)))
            (format #t "~A: ~A~%" (red "found file in pkg files?") f)
            (if f
                (update-tagged-label-list! arg deps pkg)
                #f)))))))

;; string args may include suffixed vars, e.g. %{test}.corrected
(define expand-string-arg
  (lambda (ws arg pkg targets deps)
    (format #t "~A: ~A\n" (blue "expand-string-arg") arg)
    (format #t "  targets: ~A\n" targets)
    (format #t "  deps: ~A\n" deps)

    (cond
     ((string=? "./" arg) ::pkg-dir)

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
      (format #t "VAR: ~A\n" arg)
      (format #t "deps: ~A\n" deps)
      (let ((pkg-path (car (assoc-val :pkg-path pkg))))
        (-expand-pct-arg!? ws arg :arg pkg deps)))

     (else
      (format #t "~A: ~A~%" (white "String literal") arg)
      ;; arg is string. check deps and targets, then pkg files
      (if-let ((tag (deps->tag-for-file deps arg)))
              tag
              (if-let ((tag (targets->tag-for-file targets arg)))
                      tag
                      ;; else search pkg files for file not listed in :deps
                      ;; if found, -infer-dep! adds to :deps
                      (if-let ((inferred-dep (-infer-dep! arg deps pkg)))
                              inferred-dep
                              arg))))
      ;; (cons
      ;;  arg
      ;;  ;;(resolve-string-arg pkg-path arg vars)
      ;;  (expand-cmd-args* (cdr args) targets deps))))))
     )))

(define expand-cmd-args*
  (lambda (ws args pkg targets deps)
    (format #t "~A: ~A\n" (ublue "expand-cmd-args*") args)
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
                   (let ((subarg (expand-cmd-args* ws (car args) pkg targets deps)))
                     (cons subarg
                           (expand-cmd-args* ws (cdr args) pkg targets deps))))
                   ;; (expand-cmd-args* pkg-path
                         ;;                  target targets
                         ;;                  (cdr args) filedeps vars)))

                  ;; ((number? arg)
                  ;;  (cons arg
                  ;;        (expand-cmd-args* pkg-path
                  ;;                         target targets
                  ;;                         (cdr args) filedeps vars)))

                  ((symbol? arg)
                   (format #t "~A: ~A~%" (red "arg is symbol") arg)
                   (if (or (eq? arg '%{target}) (eq? arg '%{targets}))
                       (cons :outputs
                             (expand-cmd-args* ws (cdr args) pkg targets deps))
                       (let ((arg-str (format #f "~A" arg)))
                         (cond
                          ((string-prefix? "%{" arg-str)
                           ;; %{foo} or %{foo}.suffix
                           (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
                                  (arg (-expand-pct-arg!? ws arg :arg pkg deps)))
                             (format #t "~A: ~A~%" (uwhite "expanded arg") arg)
                             (cons arg
                                   (expand-cmd-args* ws (cdr args) pkg targets deps))))
                          ( ;; else
                           (expand-cmd-args* ws (cons arg-str (cdr args))
                                             pkg targets deps))))))

                  ((string? arg)
                   (format #t "~A: ~A~%" (red "arg is string") arg)
                   (let ((sarg (expand-string-arg ws arg pkg targets deps)))
                     (append (list sarg)
                           (expand-cmd-args* ws (cdr args) pkg targets deps))))
                            ;; pkg-path target targets
                            ;; (cdr args) filedeps vars)))))

                  (else ; not number, pair, string
                   (format #t
                           "WARNING: not a nbr, pair, or string: ~A\n" arg)
                   ))
                 ))))
      ;; (format #t "~A: ~A\n" (ucyan "expanded-cmd-args") result)
      result)))

(define expand-targets
  (lambda (ws pkg targets deps)
    (format #t "~A: ~A\n" (blue "expand-targets") targets)
    (let ((xtargets (expand-args* ws targets pkg '())))
      (format #t "Expanded targets ~A\n" xtargets)
      xtargets)))

;; expands items, e.g. pct-vars like %{deps}
(define (expand-args* ws arglist pkg expanded-deps)
  (format #t "~A: ~A\n" (ublue "expand-args*") arglist)
  ;; (format #t "pkg: ~A\n" pkg)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; (let ((pkg-path (car (assoc-val :pkg-path pkg)))
  ;;       (ws-root (car (assoc-val :ws-path pkg))))
  (if (null? arglist)
      (begin
        ;; (format #t "finished arglist: ~A\n" expanded-deps)
        expanded-deps)
      (if (pair? (car arglist))
          (expand-args* ws (car arglist)
                          pkg ;; stanza-alist
                          (expand-args* ws
                           (cdr arglist) pkg expanded-deps))
          ;; car is atom
          (let* ((kw (car arglist)))
            (if-let ((depfn (assoc-val kw dune-dep-handlers)))
                    (let ((res (apply (car depfn)
                                      (list ws pkg
                                            arglist))))
                      ;; (format #t "depfn res: ~A\n" res)
                      ;; (format #t "expanded-deps: ~A\n" expanded-deps)
                      ;; we're done, depfn consumed cdr

                      (append expanded-deps
                              (if (pair? (car res)) res (list res))
                              ))

                    ;; else car of arglist not a keyword
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
                          ;; must be a filename literal?
                          ;; return (:static <path> <fname>)
                          ;; or (:dynamic <path> <fname>)
                          (begin
                            ;; (format #t "LIT DEP : ~A\n" arglist)
                            (handle-filename-literal-dep
                             ws dep arglist pkg
                             ;; stanza-alist
                             expanded-deps))))))
            ))))

(define (expand-rule-deps ws paths stanza-alist)
  ;; updates stanza-alist
  (format #t "~A: ~A\n" (blue "expand-rule-deps") stanza-alist)
  ;; (let ((stanza-alist (cdr stanza)))
  (if-let ((deps-assoc (assoc 'deps stanza-alist)))
          (let* ((deplist (assoc-val 'deps stanza-alist))
                 (_ (format #t "main deplist: ~A\n" deplist))
                 (result (expand-args* ws deplist paths '())))
            (format #t "DEPLIST EXPANDED: ~A\n" result)
            (cons :deps result))
          #f))

(define (expand-cmd-list ws pkg -raw-cmds targets deps)
  (format #t "~A: ~A\n" (ublue "expand-cmd-list") -raw-cmds)
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
                          (list
                           (list (list :tool
                                       (expand-run-tool ws kw pkg targets deps))
                                `(:args
                                  ,@(expand-cmd-args* ws (cdr raw-cmds)
                                                     pkg targets deps))))))))))))
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

;; (display "loaded dune/expanders.scm\n")
