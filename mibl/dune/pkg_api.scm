(display "pkg_api.scm") (newline)

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
      (format #t "  targets: ~A\n" tgts)

      ;; get the alists we might update
      (let* ((modules-assoc (assoc :modules pkg))
             (modules       (cdr modules-assoc))
             (scripts (assoc-val :scripts pkg))
             (data    (assoc-val :data pkg))
             (files   (assoc-val :scripts pkg)))

        ;; for each tgt, decide its kind: ml/mli, or other
        ;; then update the pkg fld: :modules, :scripts, :files, :data
        ;; since we're updating pkg use for-each
        (for-each
         (lambda (tgt)
           (let ((kind (filename->kind tgt)))
             (case kind
               ((:module)
                (begin
                  (format #t ":module: ~A\n" tgt))
                (if modules-assoc
                    (begin
                      (format #t "modules-assoc (before): ~A\n"
                              modules-assoc)
                      (set-cdr! modules-assoc
                                (append (list (filename->module-assoc tgt))
                                        (cdr modules-assoc)))
                      (format #t "modules-assoc (after): ~A\n"
                              modules-assoc))
                    (set-cdr! modules-assoc (list tgt)))
                )
               (else (format #t ":other: ~A\n" tgt)))))
         tgts)
        (format #t "modules-assoc: ~A\n" modules-assoc)
        #t))))

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

