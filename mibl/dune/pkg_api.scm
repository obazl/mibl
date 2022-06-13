(display "pkg_api.scm") (newline)

;;FIXME: put this in file_utils.scm?
(define filename->file-assoc
  (let ((+documentation+ "For now just stringify and pair with ext."))
    (lambda (filename)
      (let* ((fname (if (symbol? filename) (symbol->string filename)
                        filename))
             (ext (filename-extension fname))
             (pname (principal-name fname)))
        (cons (string->keyword
               (string-drop ext 1)) fname)))))

(define (filename->kind filename)
  (let* ((fname (if (symbol? filename) (symbol->string filename)
                    (if (string? filename) filename
                        (error 'wrong-type-arg "target type should be symbol or string: ~A" filename))))
         (ext (filename-extension fname)))
    (cond
      ((string=? ext ".mli") :module)
      ((string=? ext ".ml") :module)

      ((string=? ext ".sh") :script)
      ((string=? ext ".js") :script)
      ((string=? ext ".py") :script)

      ((string=? ext ".c") :src)
      ((string=? ext ".h") :src)
      ((string=? ext ".cxx") :src)
      ((string=? ext ".rs") :src)

      ((string=? ext ".dat") :data)
      ((string=? ext ".json") :data)
      (else :file))))

;; we need any file that can serve as a dep target
(define update-pkg-with-targets!
  (let ((+documentation+ "INTERNAL. Add tgts to :modules (or :files etc) fld of pkg."))
    (lambda (pkg tgts)
      (format #t "update-pkg-with-targets!\n")
      (format #t "  package: ~A\n" pkg)
      (format #t "  targets: ~A\n" tgts)

      ;; get the alists we might update
      (let* (;;(pkg (car pkg*))
             (modules-assoc (assoc :modules pkg))
             ;; (modules (if modules-assoc (cdr modules-assoc) #f))
             (scripts (if modules-assoc (assoc-val :scripts pkg) #f))
             (data    (if modules-assoc (assoc-val :data pkg) #f))
             (files-assoc (if (assoc :files pkg)
                              (assoc-val :files pkg) #f)))

        ;; for each tgt, decide its kind: ml/mli, or other
        ;; then update the pkg fld: :modules, :scripts, :files, :data
        ;; since we're updating pkg use for-each
        (for-each
         (lambda (tgt)
           (let ((kind (filename->kind tgt)))
             (case kind
               ((:module)
                (format #t ":module: ~A\n" tgt)
                (if modules-assoc
                    (begin
                      (format #t "modules-assoc (before): ~A\n"
                              modules-assoc)
                      (set-cdr! modules-assoc
                                (append (list (filename->module-assoc tgt))
                                        (cdr modules-assoc)))
                      (format #t "modules-assoc (after): ~A\n"
                              modules-assoc))
                    ;; else
                    (begin
                      (format #t "initializing modules-assoc\n")
                      (set! modules-assoc (cons :modules (list tgt)))
                      (format #t "modules-assoc (after): ~A\n"
                              modules-assoc))))
               (else
                (format #t ":other: ~A\n" tgt)
                (format #t "files-assoc: ~A\n" files-assoc)
                (alist-update-in! pkg '(:files :dynamic)
                                  (lambda (old)
                                    (format #t "OLD: ~A\n" old)
                                    (if (null? old)
                                        (filename->file-assoc tgt)
                                        (cons
                                         (filename->file-assoc tgt)
                                         old))))
                ;; (if-let (files-assoc (assoc-in '(:files :dynamic) pkg))
                ;;     (begin
                ;;       (format #t "files-assoc (before): ~A\n"
                ;;               files-assoc)
                ;;       (alist-update-in! pkg '(:files :dynamic)
                ;;                         (lambda (old)
                ;;                           (append
                ;;                            old (filename->file-assoc tgt))))
                ;;       ;; (set-cdr! files-assoc
                ;;       ;;           (cons (filename->file-assoc tgt)
                ;;       ;;                 (cdr files-assoc)))
                ;;       (format #t "files-assoc (after): ~A\n"
                ;;               files-assoc))
                ;;     ;; else
                ;;     (begin
                ;;       (format #t "initializing files-assoc of pkg ~A\n" pkg)
                ;;       (set! pkg
                ;;             (append pkg
                ;;                     (list
                ;;                      (list :files
                ;;                            (list :dynamic
                ;;                                  (filename->file-assoc tgt))))))
                ;;       ))
                )) ;; case
             )) ;; lambda
         tgts) ;; for-each
        (format #t "pkg (after): ~A\n" pkg)
        pkg))))

;; e.g. (cdr (:standard (symbol "\\") legacy_store_builder))
(define (pkg->module-names pkg) ;; seq)
  (let ((modules (assoc-val :modules pkg)))
    (map car modules)))

  ;; (let recur ((assoc :
  ;;             (modnames '()))
  ;;   (if (null? srcfiles)
  ;;       modnames
  ;;       (let ((m (file-name->module-name (car srcfiles))))
  ;;         (if (member m modnames) ;; avoid .ml/.mli dups
  ;;             (recur (cdr srcfiles) modnames)
  ;;             (recur (cdr srcfiles) (cons m modnames)))))))

