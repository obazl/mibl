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

(define (add-literal-to-expanded-deps local? expanded-path expanded-deps)
  (format #t "~A~%" (blue "add-literal-to-expanded-deps"))
  (format #t "~A: ~A~%" (white "expanded-path") expanded-path)
  (format #t "~A: ~A~%" (white "expanded-deps") expanded-deps)
  ;; expanded deps is an alist like ((:_ "a.ml" "a.mli") (:css "a.css")...)
  (append expanded-deps (list expanded-path)))
  ;; (alist-update-in! expanded-deps
  ;;                   '() ;; (if local? '(:X:) '(:Y_))
  ;;                     (lambda (old)
  ;;                       (let ((expanded (if (null? old)
  ;;                                           expanded-path
  ;;                                           (append old
  ;;                                                   (list expanded-path)))))
  ;;                         (format #t "old: ~A\n" old)
  ;;                         (format #t "expanded: ~A\n" expanded)
  ;;                         expanded))))

;; if file is not in this pkg, add it to filegroups table
(define (handle-filename-literal-dep ws dep deplist paths expanded-deps)
  (format #t "~A: ~A\n" (blue "handle-filename-literal-dep") dep)
  (format #t "deplist: ~A\n" deplist)
  (format #t "paths: ~A\n" paths)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  (let* (;; (_ (format #t "dep: ~A\n" dep))
         (pkg-path (car (assoc-val :pkg-path paths)))
         (ws-path (car (assoc-val :ws-path paths)))
         ;; dep always relative: prepend pkg dir, may give path with .. segs
         ;; etc. then normalize

         (dep (format #f "~A" dep))

         ;; dep should always be relative to cwd, never absolute, so
         ;; we can prepend cwd and normalize

         (dep-path (format #f "~A/~A" pkg-path dep))
         (_ (format #t "dep-path: ~A~%" dep-path))

         (canonical-path (->canonical-path dep-path))
         (_ (format #t "canonical-path: ~A~%" canonical-path))

         (kind (if (file-exists? canonical-path)
                   :static :dynamic))
         (_ (format #t "~A: ~A~%" "kind" kind))

         (_ (format #t "(dirname canonical-path) ~A~%"
                    (dirname canonical-path)))
         (_ (format #t "pkg-path: ~A~%" (car (assoc :pkg-path paths))))

         (local? (equal? (dirname canonical-path)
                         (car (assoc-val :pkg-path paths))))
         (_ (format #t "local? ~A~%" local?))

         (path (if local?
                   dep
                   canonical-path))
                   ;; (string-append pkg-path "/" canonical-path)))
         (_ (format #t "path: ~A~%" path))

         ;; (expanded-path (if local? path canonical-path))
         (expanded-path canonical-path)
         (_ (format #t "~A: ~A~%" (bgred "expanded-path") expanded-path))

         (tgt (if (equal? pkg-path (dirname expanded-path))
                     (basename expanded-path)
                     (basename expanded-path)))
                     ;;(string->keyword (format #f "fg_~A" (basename expanded-path)))))
         (_ (format #t "~A: ~A~%" (bgred "TGT") tgt))

         (tgt-tag (if (equal? pkg-path (dirname expanded-path))
                      :tgt :tgt)) ;; :fg))

         ;; (tgt (if (eq? tgt-tag :fg)
         ;;          (format #f "__~A__" tgt)
         ;;          tgt))

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
    (format #t "FILENAME LITERAL : ~A (~A)\n" dep (type-of dep))
    (format #t "expanded-path: ~A\n" expanded-path)
    (format #t "tgt: ~A\n" tgt)
    (format #t "kind : ~A\n" kind)
    (format #t "new-expanded-deps : ~A\n" expanded-deps)
    ;; find it and resolve pkg path
    ;; if not found mark it as :dynamic
    (expand-args* ws (cdr deplist)
                    paths
                    (if (null? expanded-deps)
                        (list (list
                               (string->keyword (format #f "~A" dep))
                               (cons :pkg (dirname expanded-path))
                               (cons tgt-tag tgt #|(basename expanded-path)|# )))
                        (add-literal-to-expanded-deps
                         local?
                         (list
                          (string->keyword (format #f "~A" dep))
                          (cons :pkg (dirname expanded-path))
                          (cons tgt-tag tgt #|(basename expanded-path)|# ))
                         expanded-deps))
                    )))

(define (deps->tag-for-file deps arg)
  (format #t "~A: ~A~%" (ublue "deps->tag-for-file") arg)
  (format #t "~A: ~A~%" (uwhite "deps") deps)
  (let* ((key (string->keyword (format #f "~A" arg)))
         (match (find-if (lambda (dep)
                           (format #t "~A: ~A~%" (uwhite "dep") dep)
                           (case (cadr dep)
                             ((::import ::pkg) #f)
                             (else
                              (if (equal? (car dep) key)
                                  #t
                                  (let ((tgt (assoc-val :tgt (cdr dep))))
                                    (format #t "~A: ~A~%" (uwhite "tgt") tgt)
                                    (equal? tgt arg))))))
                         (cdr deps))))
    (format #t "~A: ~A~%" (blue "match") match)
    (if match (car match) #f)))

(define (targets->tag-for-file targets arg)
  (format #t "~A: ~A~%" (blue "targets->tag-for-file") arg)
  (format #t "~A: ~A~%" (white "targets") targets)
  (let* ((targets (cdr targets))
         (match (find-if (lambda (tgt)
                           (format #t "~A: ~A~%" (cyan "tgt") tgt)
                           (let ((tgt (assoc-val :tgt (cdr tgt))))
                             (format #t "~A: ~A~%" (cyan "tgt") tgt)
                             (equal? tgt arg)))
                         targets)))
    (format #t "~A: ~A~%" (cyan "match") match)
    (if match (car match) #f)))

;; tagged literals: (:foo foo.ml), (:foo ../foo.ml), (:foo bar/foo.ml)
(define (handle-tagged-literal-dep ws deplist paths expanded-deps)
  (format #t "~A: ~A\n" (blue "handle-tagged-literal-dep") deplist)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  (format #t "car deplist: ~A (~A)\n" (car deplist) (type-of (car deplist)))
  (let* ((tag (car deplist))
         (_ (format #t "~A: ~A~%" (yellow "tag is kw?") (keyword? tag)))
         ;; (lbl (string->keyword (format #f "~A"
         ;;                               (car deplist))))
         (tagged (expand-args* ws (cdr deplist)
                              paths ;;stanza-alist
                              '()))
         ;; expand-args* inserts tag derived from literal; remove it
         (tagged (cdar tagged))
         )
    (format #t "~A: ~A (kw? ~A)~%" (yellow "littag") tag (keyword? tag))
    (format #t "~A: ~A\n" (yellow "tagged lit") tagged)

    ;; if tagged val is glob_files, update pkg with filegroup
    (if (list? (cadr deplist))
        (if (eq? 'glob_files (car (cadr deplist)))
            (update-filegroups-table!
             ws (dirname canonical-path) tag pattern)))

    (if (symbol? tagged)
        (list (list tag tagged) `,@expanded-deps)
        (if (pair? (car tagged))
            ;; (if (equal? :: (caar tagged))
            ;;     (append (list (cons tag (cdar tagged))) expanded-deps)
            ;;     (if (equal? :_ (caar tagged))
            ;;         (append (list (cons tag (cdar tagged))) expanded-deps)
            `(,(cons tag tagged) ,@expanded-deps)
            (list (list tag tagged) expanded-deps)))))

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

;; this impl does the globbing
;; (define (x-handle-glob-files-dep paths globber)
;;   (format #t "~A: ~A\n" (blue "handle-glob-files-dep") globber)
;;   (format #t "  paths: ~A\n" paths)
;;   ;; (car globber) == glob_files
;;   (let* ((pkg-path (car (assoc-val :pkg-path paths)))
;;          (ws-path (car (assoc-val :ws-path paths)))
;;          ;; (pattern (cadr globber))
;;          (pattern (format #f "~A" (cadr globber)))
;;          ;; working dir is always ws root, so we prepend the pkg-path
;;          (pattern-str (string-append pkg-path "/" pattern))
;;          (g (glob.make))
;;          (_effective-ws-root (effective-ws-root)))
;;     (format #t "pkg-path: ~A\n" pkg-path)
;;     (format #t "pattern: ~A\n" pattern)
;;     (format #t "pattern-str: ~A\n" pattern-str)
;;     (format #t "cwd: ~A\n" (pwd))
;;     (format #t "ews: ~A\n" _effective-ws-root)

;;     (let ((old-wd (pwd)))
;;       ;; change to effective ws root before globbing
;;       (chdir _effective-ws-root)
;;       ;; (format #t "cwd after chdir: ~A\n" (pwd))
;;       (glob pattern-str GLOB_BRACE g)
;;       ;; list basename for files in this pkg (dir), since relative to
;;       ;; pkg-dir for others list the pkg prefix.

;;       ;; no: user would have to test to determine which files to be
;;       ;; resolved relative to pkg.
;;       (let* ((globbed (glob.gl_pathv g))
;;              (globbed (sort! globbed string<?))
;;              (depfiles (map (lambda (f)
;;                               (let ((dir (dirname f)))
;;                                 (if (string=? dir pkg-path)
;;                                     (basename f) ;; `(:: ,(basename f))
;;                                     (let* ((np (normalize-pkg-path f ws-path))
;;                                            (dname (dirname np))
;;                                            (bname (basename np)))
;;                                       np)
;;                                       ;; (format #f "~A:~A" dname bname))
;;                                     )))
;;                             globbed)))
;;         (format #t "globbed: ~A\n" globbed)
;;         (format #t "depfiles: ~A\n" depfiles)
;;         ;; (format #t "sorted: ~A\n" sorted)
;;         (globfree g)
;;         (chdir old-wd) ;; restore prev wd

;;         (list (if (string-index pattern (lambda (ch) (equal? ch #\/)))
;;                   (cons :_ depfiles)
;;                    `(:: ,@depfiles)))))))

(define (handle-glob-files-rec-dep deplist)
  (format #t "handle-glob-files-rec-dep: ~A\n" deplist))

(define (handle-glob-files-dep ws paths _pattern)
  (format #t "~A: ~A\n" (blue "handle-glob-files-dep") _pattern)
  ;; _pattern form: (glob_files ../../runtime/*.js)
  (format #t "  paths: ~A\n" paths)
  ;; (car _pattern) == glob_files
  (let* ((pkg-path (car (assoc-val :pkg-path paths)))
         (ws-root (car (assoc-val :ws-path paths)))
         ;; (pattern (cadr _pattern))
         (pattern (normalize-glob-pattern _pattern))
         ;; (pattern (format #f "~A" (cadr _pattern)))
         ;; ;; working dir is always ws root, so we prepend the pkg-path
         ;; (pattern-str (string-append pkg-path "/" pattern))
         (g (glob.make))
         (_effective-ws-root (effective-ws-root)))
    (format #t "pkg-path: ~A\n" pkg-path)
    (format #t "pattern: ~A\n" pattern)
    (format #t "pattern-str: ~A\n" pattern-str)
    (format #t "cwd: ~A\n" (pwd))
    (format #t "ews: ~A\n" _effective-ws-root)

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
             (globbed (sort! globbed string<?))
             (depfiles (map (lambda (f)
                              (let ((dir (dirname f)))
                                (if (string=? dir pkg-path)
                                    (basename f) ;; `(:: ,(basename f))
                                    (let* ((np (normalize-pkg-path f ws-root))
                                           (dname (dirname np))
                                           (bname (basename np)))
                                      np)
                                      ;; (format #f "~A:~A" dname bname))
                                    )))
                            globbed)))
        (format #t "globbed: ~A\n" globbed)
        (format #t "depfiles: ~A\n" depfiles)
        ;; (format #t "sorted: ~A\n" sorted)
        (globfree g)
        (chdir old-wd) ;; restore prev wd

        (list (if (string-index pattern (lambda (ch) (equal? ch #\/)))
                  (cons :_ depfiles)
                   `(:: ,@depfiles)))))))

(define (handle-tagged-glob-dep ws tagged-pattern paths expanded-deps)
  (format #t "~A: ~A\n" (ublue "handle-tagged-glob-dep") tagged-pattern)
  (format #t "expanded-deps: ~A\n" expanded-deps)
  ;; tagged-pattern form: (:css (glob_files ../css/*.css))
  (format #t "cadr tagged-pattern: ~A\n" (cadr tagged-pattern))
  ;; kw :_ is reserved for non-tagged symlist
  ;; to avoid name clash, convert user keywords to double-colon, e.g.
  ;; :foo => ::foo
  (let* ((lbl (string->keyword (format #f "~A" (keyword->symbol (car tagged-pattern)))))
         (pattern (cadr tagged-pattern))
         (_ (format #t "~A: ~A~%" (green "pattern") pattern))
         (pattern (format #f "~A" (cadr pattern)))
         (_ (format #t "~A: ~A~%" (green "pattern") pattern))
         ;; working dir is always ws root, so we prepend the pkg-path
         ;; (pattern-str (string-append pkg-path "/" pattern))

         (pkg-path (car (assoc-val :pkg-path paths)))
         (dep-path (format #f "~A/~A" pkg-path pattern))
         (_ (format #t "dep-path: ~A~%" dep-path))

         (canonical-path (->canonical-path dep-path))
         (_ (format #t "canonical-path: ~A~%" canonical-path))

         (tagged canonical-path)

         ;; (tagged
         ;;  (if (list? (cadr tagged-pattern))
         ;;      ;; do not process globs here, just create the target kw
         ;;      (if (eq? 'glob_files (car (cadr tagged-pattern)))
         ;;          (string->keyword (format #f "*~A"
         ;;                                   (keyword->symbol (car tagged-pattern))))
         ;;          #f)
         ;;      #f))
         ;; (tagged ;(if tagged tagged
         ;;             (expand-args* ws (cdr tagged-pattern)
         ;;                          paths ;;stanza-alist
         ;;                          '()))
         ) ;;expanded-deps)))
    (format #t "tagged dep lbl: ~A\n" lbl)
    (format #t "tagged dep: ~A\n" tagged)

    ;; if tagged val is glob_files, update pkg with filegroup
    (if (list? (cadr tagged-pattern))
        (if (eq? 'glob_files (car (cadr tagged-pattern)))
            (update-filegroups-table!
             ws (dirname canonical-path)
             lbl ;; (keyword->symbol lbl)
             (basename pattern))
            ))
    (let ((result
           ;; could not figure out how to handle empty expanded-list
           ;; without getting #<unspecified>, so hack alert:
           (if (null? expanded-deps)
               (list (list lbl
                           (cons :pkg (dirname tagged))
                           ;; NB: :tgt for singleton, :tgts for globs
                           (cons :glob lbl)))
               (list (list lbl
                           (cons :pkg (dirname tagged))
                           ;; NB: :tgt for singleton, :tgts for globs
                           (cons :glob lbl))
                     `,@expanded-deps))))
      (format #t "~A: ~A~%" (red "expanded-deps") expanded-deps)
      (format #t "~A: ~A~%" (red "GLOB RESULT") result)
      result)))

(define (handle-untagged-glob-dep ws paths _pattern)
  (format #t "~A: ~A\n" (ublue "handle-untagged-glob-dep") _pattern)
  ;; kw :_ is reserved for non-tagged symlist
  ;; to avoid name clash, convert user keywords to double-colon, e.g.
  ;; :foo => ::foo
  (let* ((pattern (cadr _pattern))
         (pkg-path (car (assoc-val :pkg-path paths)))
         (dep-path (format #f "~A/~A" pkg-path pattern))
         (_ (format #t "~A: ~A~%" (uwhite "dep-path") dep-path))

         (_ (format #t "~A: ~A~%" (bgred "cwd") (pwd)))
         (canonical-path (->canonical-path dep-path))
         (_ (format #t "~A: ~A~%" (uwhite "canonical-path") canonical-path))
         (lbl :glob)  ;;FIXME: derive tagname
         )

    (update-filegroups-table!
             ws (dirname canonical-path)
             lbl
             (basename pattern))

    (let ((result
           (list (list :glob
                           (cons :pkg (dirname canonical-path))
                           (cons :glob lbl)))))
      (format #t "~A: ~A~%" (red "GLOB RESULT") result)
      result)))

;; tagged deps: (:foo foo.sh), (:css (glob_files *.css)), what else?
;; called from expanders.scm::expand-args*
(define (handle-tagged-dep ws deplist paths expanded-deps)
  (format #t "~A: ~A\n" (ublue "handle-tagged-dep") deplist)
  ;; obsolete: kw :_ is reserved for non-tagged symlist
  ;; to avoid name clash, convert user keywords to double-colon, e.g.
  ;; :foo => ::foo
  (let ((tag (car deplist))
        (val (cdr deplist)))
    (if (list? (car val))
        (if (eq? 'glob_files (caar val))
            ;; e.g. (:css (glob_files *.css)
            (handle-tagged-glob-dep ws deplist paths expanded-deps)
            (error 'fixme "unhandled: non-glob tagged list var"))
        (handle-tagged-literal-dep ws deplist paths expanded-deps))))

(define (handle-file-dep deplist)
  (format #t "handle-file-dep: ~A\n" deplist)
  deplist)

(define (handle-alias-dep paths deplist)
  (format #t "handle-alias-dep: ~A\n" deplist)
  (set-car! deplist :alias)
  deplist)

(define (handle-alias-rec-dep deplist)
  (format #t "handle-alias-rec-dep: ~A\n" deplist))

(define (handle-source-tree-dep paths deplist)
  ;; (format #t "handle-source-tree-dep: ~A\n" deplist)
  (error 'unsupported
         (string-append "Found a 'source_tree' dependency in pkg '"
                        (car (assoc-val :pkg-path paths))
                        "'.  A 'source_tree' dependency in a rule dep usually means the rule should be replaced by a cc_library rule or something from rules_foreign_cc. I can't automate that, sorry.")))

(define (handle-universe-dep deplist)
  (format #t "handle-universe-dep: ~A\n" deplist)
  (error 'handle-universe-dep "FIXME: handle-universe-dep"))

(define (handle-package-dep ws pkg deplist)
  (format #t "~A: ~A~%" (ublue "handle-package-dep") deplist)
  (format #t "~A: ~A~%" (uwhite "pkg") pkg)
;; (package <pkg>) depends on all files installed by <package>, as well as on the transitive package dependencies of <package>. This can be used to test a command against the files that will be installed.
  ;; we will emit a filegroup for all files in the pkg
  (let* ((key (symbol->keyword (cadr deplist))))
    (list key ::pkg)))

(define (handle-env-var-dep deplist)
  (format #t "handle-env-var-dep: ~A\n" deplist))

(define (handle-sandbox-dep deplist)
  (format #t "handle-sandbox-dep: ~A\n" deplist))

(define (handle-include-dep deplist)
  (format #t "handle-include-dep: ~A\n" deplist))

;; called by expanders::expand-args*
(define dune-dep-handlers
  `((file ,handle-file-dep)
    (alias ,handle-alias-dep)
    (alias_rec ,handle-alias-rec-dep)
    (glob_files ,handle-untagged-glob-dep) ;; ,handle-glob-files-dep)
    (glob_files_rec ,handle-glob-files-rec-dep)
    (source_tree ,handle-source-tree-dep)
    (universe ,handle-universe-dep)
    (package ,handle-package-dep)
    (env_var ,handle-env-var-dep)
    (sandbox ,handle-sandbox-dep)
    (include ,handle-include-dep)))

;; (format #t "loaded dune/dune-action-deps.scm\n")
