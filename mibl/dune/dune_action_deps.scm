;; NB: the dune syms use undersccores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (handle-filename-literal-dep dep deplist pkg stanza expanded-deps)
  (format #t "handle-filename-literal-dep: ~A\n" dep)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  (let* ((_ (format #t "dep: ~A\n" dep))
         (pkg-path (car (assoc-val :pkg-path pkg)))
         (ws-root (car (assoc-val :ws-path pkg)))

         (path (string-append pkg-path
                              "/" dep))
         (_ (format #t "path: ~A\n" path))
         (kind (if (file-exists? path)
                   :static :dynamic))
         (expanded-path (if (eq? kind :static)
                 (resolve-pkg-path path ws-root)
                 (canonical-path path)))
         ;; (new-expanded-deps (alist-update-in! expanded-deps `(,kind)
         ;;                                      (lambda (p)
         ;;                                        (format #t "update fn here\n")
         ;;                                    (cons (list kind expanded-path)
         ;;                                          p))))
         )
    (format #t "FILENAME LITERAL : ~A\n" dep)
    (format #t "expanded: ~A\n" expanded-path)
    (format #t "kind : ~A\n" kind)
    ;; (format #t "new-expanded-deps : ~A\n" expanded-deps)
    ;; find it and resolve pkg path
    ;; if not found mark it as :dynamic
    (expand-deplist (cdr deplist)
                    pkg stanza
                    ;; new-expanded-deps
                    (if (null? expanded-deps)
                        (list (cons :_ expanded-path))
                        (list
                         (cons :_
                               (list expanded-path
                                     (cdar expanded-deps)))))
                    )))

(define (handle-named-dep deplist pkg stanza-alist expanded-deps)
  (format #t "handle-named-dep: ~A\n" deplist)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; kw :_ is reserved for non-named symlist
  ;; to avoid name clash, convert user keywords to double-colon, e.g.
  ;; :foo => ::foo
  (let ((lbl (symbol->keyword
              (string->symbol
               (string-append
                ":" (symbol->string
                     (keyword->symbol (car deplist)))))))
        (named (expand-deplist (cdr deplist)
                               pkg stanza-alist '()))) ;;expanded-deps)))
    (format #t "named dep lbl: ~A\n" lbl)
    (format #t "named dep: ~A\n" named)
    (if (pair? (car named))
        (if (equal? :_ (caar named))
            (cons (cons lbl (cdar named)) expanded-deps)
            (append (cons lbl named) expanded-deps))
        (cons (cons lbl named) expanded-deps))))

(define (handle-file-dep deplist)
  (format #t "handle-file-dep: ~A\n" deplist)
  deplist)

(define (handle-alias-dep deplist)
  (format #t "handle-alias-dep: ~A\n" deplist))

(define (handle-alias-rec-dep deplist)
  (format #t "handle-alias-rec-dep: ~A\n" deplist))

;; Globbing.
;; Dune: "Note that globs will match files that exist in the source
;; tree as well as buildable targets, so for instance you can depend
;; on *.cmi."  Not in OBazl.
;; Dune glob syntax:
;; \<char> matches exactly <char>, even if it’s a special character (*, ?, …)
;; * matches any sequence of characters, except if it comes first, in which case it matches any character that is not . followed by anything.
;; ** matches any character that is not . followed by anything, except if it comes first, in which case it matches anything.
;; ? matches any single character.
;; [<set>] matches any character that is part of <set>.
;; [!<set>] matches any character that is not part of <set>.
;; {<glob1>,<glob2>,...,<globn>} matches any string that is matched by one of <glob1>, <glob2>, etc.

;; We use BSD glob (man glob(3)) so there's a mismatch. We'll worry
;; about that later, most patterns will be like "*.ml", "*.ml{,i}",
;; "foo/*", etc.
(define (handle-glob-files-dep pkg deplist)
  (format #t "HANDLE-glob-files-dep: ~A\n" deplist)
  ;; (car deplist) == glob_files
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (ws-root (car (assoc-val :ws-path pkg)))
         (pattern (cadr deplist))
         (pattern-str (if (symbol? pattern)
                          (symbol->string pattern) pattern))
         ;; working dir is always ws root, so we prepend the pkg-path
         (pattern-str (string-append pkg-path "/" pattern-str))
         (g (glob.make)))
    (format #t "pkg-path: ~A\n" pkg-path)
    (format #t "pattern: ~A\n" pattern-str)
    (format #t "cwd: ~A\n" (pwd))
    (format #t "ews: ~A\n" effective-ws-root)

    (let ((old-wd (pwd)))
      ;; change to effective ws root before globbing
      (chdir effective-ws-root)
      (format #t "cwd after chdir: ~A\n" (pwd))
      (glob pattern-str GLOB_BRACE g)
      ;; list basename for files in this pkg (dir), since relative to pkg-dir
      ;; for others list the pkg prefix
      (let* ((globbed (glob.gl_pathv g))
             (depfiles (map (lambda (f)
                              (let ((dir (dirname f)))
                                (if (string=? dir pkg-path)
                                    ;; rel path == pkg-path
                                    (basename f)
                                    (resolve-pkg-path f ws-root)
                                    )))
                            globbed)))
        (format #t "globbed: ~A\n" globbed)
        (format #t "depfiles: ~A\n" depfiles)
        (globfree g)
        (chdir old-wd) ;; restore prev wd
        (list (cons :_ depfiles))))))

(define (handle-glob-files-rec-dep deplist)
  (format #t "handle-glob-files-rec-dep: ~A\n" deplist))

(define (handle-source-tree-dep deplist)
  (format #t "handle-source-tree-dep: ~A\n" deplist))

(define (handle-universe-dep deplist)
  (format #t "handle-universe-dep: ~A\n" deplist))

(define (handle-package-dep deplist)
  (format #t "handle-package-dep: ~A\n" deplist))

(define (handle-env-var-dep deplist)
  (format #t "handle-env-var-dep: ~A\n" deplist))

(define (handle-sandbox-dep deplist)
  (format #t "handle-sandbox-dep: ~A\n" deplist))

(define (handle-include-dep deplist)
  (format #t "handle-include-dep: ~A\n" deplist))

(define dune-dep-handlers
  `((file ,handle-file-dep)
    (alias ,handle-alias-dep)
    (alias_rec ,handle-alias-rec-dep)
    (glob_files ,handle-glob-files-dep)
    (glob_files_rec ,handle-glob-files-rec-dep)
    (source_tree ,handle-source-tree-dep)
    (universe ,handle-universe-dep)
    (package ,handle-package-dep)
    (env_var ,handle-env-var-dep)
    (sandbox ,handle-sandbox-dep)
    (include ,handle-include-dep)))

(define (expand-deplist deplist pkg stanza-alist expanded-deps)
  (format #t "expand-DEPLIST: ~A\n" deplist)
  (format #t "pkg: ~A\n" pkg)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; (let ((pkg-path (car (assoc-val :pkg-path pkg)))
  ;;       (ws-root (car (assoc-val :ws-path pkg))))
  (if (null? deplist)
      (begin
        (format #t "finished deplist: ~A\n" expanded-deps)
        expanded-deps)
      (if (pair? (car deplist))
          (expand-deplist (car deplist)
                          pkg stanza-alist
                          (expand-deplist
                           (cdr deplist) pkg stanza-alist expanded-deps))
          ;; car is atom
          (let* ((kw (car deplist)))
            (if-let ((depfn (assoc-val kw dune-dep-handlers)))
                    (let ((res (apply (car depfn) (list pkg
                                                        deplist))))
                      ;; (format #t "res: ~A\n" res)
                      ;; (format #t "expanded-deps: ~A\n" expanded-deps)
                      ;; we're done, depfn consumed cdr

                      ;; FIXME: merge all lists with :_ key
                      ;; we get one such list for each glob_files
                      (if (equal? :_ (caar res))
                          (format #t "TO MERGE ~A\n" res))
                      (append expanded-deps res))

                    ;; else car of deplist not a keyword
                    ;; must be either a ':' named dep or a filename literal
                    (let ((dep (if (symbol? (car deplist))
                                   (symbol->string (car deplist))
                                   (car deplist))))
                      (if (char=? #\: (string-ref dep 0))
                          (begin
                            (format #t "NAMED DEP : ~A\n" deplist)
                            (handle-named-dep
                             deplist pkg stanza-alist expanded-deps))

                          ;; else must be a filename literal
                          ;; return (:static <path> <fname>)
                          ;; or (:dynamic <path> <fname>)
                          (begin
                            (format #t "LIT DEP : ~A\n" deplist)
                            (handle-filename-literal-dep
                             dep deplist pkg stanza-alist expanded-deps)

                            ;; (let ((dep (make-filedep-arg pkg-path
                            ;;                            (cadar deps)
                            ;;                            (caar deps) '())))
                            ;;   )
                            ))))
            ))))

;; expand-deps: deps -> file-deps, vars, env-vars
(define (expand-action-deps pkg stanza-alist)
  (format #t "expand-action-DEPS, stanza-alist: ~A\n" stanza-alist)
  ;; (let ((stanza-alist (cdr stanza)))
    (let* ((deplist (assoc-val 'deps stanza-alist))
          (_ (format #t "main deplist: ~A\n" deplist))
          (result (expand-deplist deplist pkg stanza-alist '())))
      (format #t "RESULT: ~A\n" result)
      result))

(format #t "loaded dune/dune-action-deps.scm\n")
