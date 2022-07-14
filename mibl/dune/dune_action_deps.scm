;; NB: the dune syms use undersccores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION DEPS
;; https://dune.readthedocs.io/en/stable/concepts.html#deps-field
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

(define (-deps->srcs pkg-path deps)
  (format #t "~A: ~A\n" (blue "-deps->srcs") deps)
  ;; deps is a list of alists; key :maps to list of (:pkg :file) pairs
  ;; key :_ (anonymous) may map to multiple pairs
  ;; other keys are dune 'variables', each mapping to one (:pkg :file) pair
  (let* ((srcs (map (lambda (src-assoc)
                      (format #t "src-assoc: ~A~%" src-assoc)
                      (if (equal? :_ (car src-assoc))
                          ;; (:_ "foo.txt" "bar.txt" ...)
                          (begin (format #t "HHHHHHHHHHHHHHHH\n")
                          (map (lambda (srcfile)
                                 (format #t "srcfile: ~A~%" srcfile)
                                 (let ((dname (dirname srcfile))
                                       (bname (basename srcfile)))
                                   ;; (-pkg-path
                                   ;;      (car (assoc-val :pkg src-alist))))
                                   (format #f "~A"
                                           (if (equal dname pkg-path)
                                               bname srcfile))))
                               ;; (car
                               ;;  (assoc-val :file srcfile)))))
                               (cdr src-assoc)))
                          ;; else tagged (:foo . "foo.txt")
                          (begin (format #t "JJJJJJJJJJJJJJJJ\n")
                          (map (lambda (srcfile)
                                 (format #t "srcfile: ~A~%" srcfile)
                                 (let ((dname (dirname srcfile))
                                       (bname (basename srcfile)))
                                   ;; (-pkg-path
                                   ;;      (car (assoc-val :pkg src-alist))))
                                   (format #f "~A"
                                           (if (equal dname pkg-path)
                                               bname srcfile))))
                               ;; (car
                               ;;  (assoc-val :file srcfile)))))
                               (cdr src-assoc)))
                          ;; (format #f "~A"
                          ;;         (let* ((srcfile (cdr src-assoc))
                          ;;                (dname (dirname srcfile))
                          ;;                (bname (basename srcfile)))
                          ;;           (if (equal dname pkg-path)
                          ;;               bname srcfile)))
                          ))
                    deps))
         ;; srcs list may contain mix of strings and sublists
         (srcs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() srcs)))
    (format #t "SRCES: ~A\n" srcs)
    srcs))

(define (add-literal-to-expanded-deps expanded-path expanded-deps)
  (format #t "~A: ~A, ~A~%" (blue "add-literal-to-expanded-deps")
          expanded-path expanded-deps)
  ;; expanded deps is an alist like ((:_ "a.ml" "a.mli") (:css "a.css")...)
  ;; merge path into :_ list
    (alist-update-in! expanded-deps '(:_)
                      (lambda (old)
                        (let ((expanded (append old (list expanded-path))))
                          (format #t "old: ~A\n" old)
                          (format #t "expanded: ~A\n" expanded)
                          expanded))))

(define (handle-filename-literal-dep dep deplist paths expanded-deps)
  (format #t "~A: ~A\n" (blue "handle-filename-literal-dep") dep)
  ;; (format #t "expanded-deps: ~A\n" expanded-deps)
  (let* (;; (_ (format #t "dep: ~A\n" dep))
         (pkg-path (car (assoc-val :pkg-path paths)))
         (ws-root (car (assoc-val :ws-path paths)))
         ;; dep always relative: prepend pkg dir, may give path with .. segs
         ;; etc. then normalize
         (path (string-append pkg-path
                              "/" dep))
         ;; (_ (format #t "path: ~A\n" path))
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

         ;; (expanded-path
         ;;  (if (equal? pkg-path (dirname expanded-path))
         ;;      (basename expanded-path)
         ;;      expanded-path))
         )
    ;; (format #t "FILENAME LITERAL : ~A\n" dep)
    ;; (format #t "expanded: ~A\n" expanded-path)
    ;; (format #t "kind : ~A\n" kind)

                 ;; (depfiles (map (lambda (f)
                 ;;              (let ((dir (dirname f)))
                 ;;                (if (string=? dir pkg-path)
                 ;;                    ;; rel path == pkg-path
                 ;;                    (basename f)
                 ;;                    (resolve-pkg-path f ws-root)
                 ;;                    )))
                 ;;            globbed)))


    ;; (format #t "new-expanded-deps : ~A\n" expanded-deps)
    ;; find it and resolve pkg path
    ;; if not found mark it as :dynamic
    (expand-deps (cdr deplist)
                    paths ;; stanza
                    ;; new-expanded-deps
                    (if (null? expanded-deps)
                        ;; needs to be an alisg
                        ;; (list (list :_ expanded-path))
                        (list (list :_ expanded-path))
                                    ;; `((:pkg ,(dirname expanded-path))
                                    ;;   (:file ,(basename expanded-path)))))
                        (add-literal-to-expanded-deps
                         expanded-path
                         ;; `((:pkg ,(dirname expanded-path))
                         ;;   (:file ,(basename expanded-path)))
                         expanded-deps))
                    )))

(define (handle-tagged-dep deplist paths expanded-deps)
  (format #t "~A: ~A\n" (blue "handle-tagged-dep") deplist)
  ;; (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; kw :_ is reserved for non-tagged symlist
  ;; to avoid name clash, convert user keywords to double-colon, e.g.
  ;; :foo => ::foo
  (let ((lbl (symbol->keyword
              (string->symbol
               (string-append
                ":" (symbol->string
                     (keyword->symbol (car deplist)))))))
        (tagged (expand-deps (cdr deplist)
                                 paths ;;stanza-alist
                                 '()))) ;;expanded-deps)))
    (format #t "tagged dep lbl: ~A\n" lbl)
    (format #t "tagged dep: ~A\n" tagged)
    (if (pair? (car tagged))
        (if (equal? :_ (caar tagged))
            (append (list (cons lbl (cdar tagged))) expanded-deps)
            ;; (cons (cons lbl (cadar tagged))
            ;;       expanded-deps)
            (append (cons lbl tagged) expanded-deps))
        (cons (cons lbl tagged) expanded-deps))))

(define (handle-file-dep deplist)
  (format #t "handle-file-dep: ~A\n" deplist)
  deplist)

(define (handle-alias-dep paths deplist)
  (format #t "handle-alias-dep: ~A\n" deplist)
  (set-car! deplist :alias)
  deplist)

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
(define (handle-glob-files-dep paths deplist)
  (format #t "~A: ~A\n" (blue "handle-glob-files-dep") deplist)
  ;; (format #t "HANDLE-glob-files-dep paths: ~A\n" paths)
  ;; (car deplist) == glob_files
  (let* ((pkg-path (car (assoc-val :pkg-path paths)))
         (ws-root (car (assoc-val :ws-path paths)))
         (pattern (cadr deplist))
         (pattern-str (if (symbol? pattern)
                          (symbol->string pattern) pattern))
         ;; working dir is always ws root, so we prepend the pkg-path
         (pattern-str (string-append pkg-path "/" pattern-str))
         (g (glob.make))
         (_effective-ws-root (effective-ws-root)))
    ;; (format #t "pkg-path: ~A\n" pkg-path)
    ;; (format #t "pattern: ~A\n" pattern-str)
    ;; (format #t "cwd: ~A\n" (pwd))
    ;; (format #t "ews: ~A\n" _effective-ws-root)

    (let ((old-wd (pwd)))
      ;; change to effective ws root before globbing
      (chdir _effective-ws-root)
      ;; (format #t "cwd after chdir: ~A\n" (pwd))
      (glob pattern-str GLOB_BRACE g)
      ;; list basename for files in this pkg (dir), since relative to
      ;; pkg-dir for others list the pkg prefix.

      ;; no: user would have to test to determine which files to be
      ;; resolved relative to pkg.
      (let* ((globbed (glob.gl_pathv g))
             (depfiles (map (lambda (f)
                              (let ((dir (dirname f)))
                                (if (string=? dir pkg-path)
                                    ;; rel path == pkg-path
                                    ;; (basename f)
                                    (resolve-pkg-path f ws-root)
                                    (resolve-pkg-path f ws-root)
                                    )))
                            globbed))
             (sorted (sort! depfiles string<?)))
        (format #t "globbed: ~A\n" globbed)
        (format #t "depfiles: ~A\n" depfiles)
        (format #t "sorted: ~A\n" sorted)
        (globfree g)
        (chdir old-wd) ;; restore prev wd
        (list (cons :_ sorted))))))

(define (handle-glob-files-rec-dep deplist)
  (format #t "handle-glob-files-rec-dep: ~A\n" deplist))

(define (handle-source-tree-dep paths deplist)
  ;; (format #t "handle-source-tree-dep: ~A\n" deplist)
  (error 'unsupported
         (string-append "Found a 'source_tree' dependency in pkg '"
                        (car (assoc-val :pkg-path paths))
                        "'.  A 'source_tree' dependency in a rule dep usually means the rule should be replaced by a cc_library rule or something from rules_foreign_cc. I can't automate that, sorry.")))

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

;; (format #t "loaded dune/dune-action-deps.scm\n")
