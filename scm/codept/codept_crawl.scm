
(define* (->codept root . dirs)
  (format #t "->CODEPT root: ~A dirs: ~A\n" root dirs)
  (if (not (null? dirs))
      (if (not (pair? dirs))
          (error 'bad-arg
                 (format #f "second arg to 'crawl' must be a list of dirs\n"))))

  (if (not (file-exists? ".obazl.d/tmp"))
      (system "mkdir -p .obazl.d/tmp"))
  (let ((outp
         (open-output-file ".obazl.d/tmp/codept.args")))

    ;; if no dirs passed, set root to ./ and dirs to root
    (let ((root (if (null? dirs) "." root))
          (dirs (if (null? dirs) (list root) dirs)))

      ;; (format #t "root: ~A, dirs: ~A\n" root dirs)

      (for-each
       (lambda (dir)
         (format #t "root ~A, dir ~A\n" root dir)
         (if (char=? #\/ (string-ref dir 0))
             (error 'wrong-type-arg "paths must be relative"))
         (if (not (directory? dir))
             (error 'wrong-type-arg
                    (string-append "Not a directory: " dir)))

         ;; we will recur on subdirs, so we need a new root
         (let recur-dir ((subroot root)
                         (dir dir))
           ;; (format #t "recurring on ~A/~A\n" subroot dir)
           (if (null? dir)
               (begin
                 ;; this should never happen, we only recur when we
                 ;; have a dir
                 (error 'null-dir (format #f "unexpected null dir\n")))

               (let* ((path
                       (if (equal? subroot ".")
                           dir
                           (string-append subroot "/" dir)))
                      (dirents (sort! (directory->list path) string<?))
                      (files
                       ;; rm ".", "..", "bazel-*", hidden dirs
                       (filter (lambda (dentry)
                                 (not (or (equal? ".." dentry)
                                          (equal? "." dentry)
                                          (string-prefix? "." dentry)
                                          (string-prefix? "bazel-" dentry))))
                               (cddr dirents)))
                      (subdirs (filter (lambda (f)
                                         (directory?
                                          (string-append path "/" f)))
                                       files))
                      (ocaml-srcs (filter (lambda (f)
                                            (or (string-suffix? ".ml" f)
                                                (string-suffix? ".mli" f)))
                                          files))
                      (modules (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".ml" f))
                                         files)))
                      (sigs (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".mli" f))
                                         files)))
                      )
                 ;; (format #t "path: ~A\n" path)
                 ;; (format #t "subdirs: ~A\n" subdirs)
                 (if files
                     (let ((group (if (equal? "." path) "_ROOT"
                                      (path->group-tag path))))
                       (if (not (string-prefix? "testsuite" group))
                           (begin
                             ;; first do all files in dir
                             (if (not (and (null? modules) (null? sigs)))
                                 (let ((fpaths (map (lambda (f)
                                                      (string-append path "/" f))
                                                    ocaml-srcs)))
                                   (format outp "~A[" (string-tr group #\/ #\_))
                                   (format outp "~A"
                                           (string-join fpaths ","))
                                   ;; (for-each (lambda (f)
                                   ;;             (if (or (string-suffix? ".mli" f)
                                   ;;                     (string-suffix? ".ml" f))
                                   ;;                 (format outp "~A/~A," path f)
                                   ;;                 ;; else nop
                                   ;;                 ))
                                   ;;           files)
                                   (format outp "]\n")
                                   )
                                 )
                             ;; then recur in subdirs
                             (for-each (lambda (subdir)
                                         (recur-dir path subdir))
                                       subdirs))
                           ;; skip testsuite subdirs, sth in there chokes codept
                           ))
                     ;; no more files, so no recursion
                     )))))
       ;; for each dir
       (if (null? dirs) (list ".") dirs)))
    (close-output-port outp))

  (format #t "running codept\n")

  ;; now run codept to produce codept.deps
  (system (string-append "codept -expand-deps -sexp -k "
                         "-args .obazl.d/tmp/codept.args "
                         "> .obazl.d/tmp/codept.deps "
                         "2> .obazl.d/tmp/codept.log"))

  ;; and now analyze codept.deps
  (let* ((codept-sexp (read-codept-depsfile ".obazl.d/tmp/codept.deps"))
         (_ (format #t "codept-sexp: ~A\n" codept-sexp))
         (codept-alist (cdr codept-sexp))
         (locals-assoc (assoc 'local codept-alist))
         (locals-alist-list (cadr locals-assoc))

         (deps-alist (cadr (assoc 'dependencies codept-alist)))
         (modules-tbl (make-hash-table))
         (deps-tbl (make-hash-table)))
    ;; each alist in locals-alist-list: keys module, ml, mli
    ;; (format #t "locals-alist-list: ~A\n" locals-alist-list)
    (for-each (lambda (local-dep)
                (let* ((file-assoc (assoc 'file local-dep))
                       (fpath (cadr file-assoc))
                       (deps-assoc (assoc 'deps local-dep))
                       (deps-list (if deps-assoc
                                      (cadr deps-assoc)
                                      '())))
                  (format #t "file-assoc: ~A\n" file-assoc)
                  (format #t "fpath: ~A\n" fpath)
                  (format #t "deps-assoc: ~A\n" deps-assoc)
                  (format #t "deps-list: ~A\n" deps-list)
                  (if-let ((e (hash-table-ref modules-tbl fpath)))
                          (begin ;; update old entry
                            (if (list? e)
                                (hash-table-set! modules-tbl fpath
                                                 (cons deps-list e))
                                (hash-table-set! modules-tbl fpath
                                                 (list deps-list e))))
                          ;; else new entry
                          (begin
                            ;; (format #t "adding ~A\n" mname)
                            (hash-table-set! modules-tbl fpath
                                             deps-list)))))

              deps-alist)
    modules-tbl))

