;; NB: the dune syms use undersccores

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
    (format #t "pattern: ~A\n" pattern-str)
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
                                  ))) globbed)))
      (format #t "globbed: ~A\n" globbed)
      (format #t "depfiles: ~A\n" depfiles)
      (globfree g)
      depfiles))
  )

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

(format #t "loaded dune/dune-action-deps.scm\n")
