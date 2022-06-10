;; (display "dune/dune_normalize.scm loading ...") (newline)

(define (normalize-stanzas pkg-path
                           ;; dune-project-stanzas
                           srcfiles stanzas)
  (format #t "normalize-stanzas, pkg: ~A\n" pkg-path)
  (format #t "srcfiles: ~A\n" srcfiles)
  (let* ((i 0)
         (normed (remove '()
                         (map
                          (lambda (stanza)
                            ;; (if (string=? pkg-path
                            ;;             "src/lib_protocol_environment/structs")
                            ;; (format #t "NORMALIZING stanza ~A. ~A\n" i stanza)
                            ;; )
                            (let ((normed (normalize-dune-stanza
                                           pkg-path
                                           ;; dune-project-stanzas
                                           srcfiles ;; s/b '() ??
                                           stanza)))
                              (set! i (+ i 1))
                              normed))
                          (cadr stanzas)))))
    ;; normed
    ;; (format #t "normalized stanzas: ~A\n" normed)

    ;; 'executables' normalizes to a list of 'executable' so we flatten
    (let ((result (let recur ((stanzas normed))
                    (if (null? stanzas) '()
                        (begin
                          ;; (format #t "  Stanza: ~A\n" (car stanzas))
                          (if (pair? (caar stanzas))
                              (concatenate (car stanzas)
                                           (recur (cdr stanzas)))
                              (concatenate
                               `(,(car stanzas))
                               (recur (cdr stanzas)))))))))
      ;; (format #t "Renormalized stanzas: ~A\n" result)
      result)
    ))

(define (ocamllex->outfile-names ocamllex-stanza)
  (let* ((normalized-ocamllex (normalize-stanza-ocamllex ocamllex-stanza))
         (modules (cadr normalized-ocamllex))
         ;; (_ (format #t "lex modules ~A\n" modules))
         (outfiles (map
                    (lambda (module)
                      (string-append (symbol->string module) ".ml"))
                    modules)))
    outfiles))

(define (pkg-genfiles pkg-alist)
  ;; (format #t "pkg-genfiles: ~A\n" pkg-alist)
  (if-let ((stanzas (assoc :stanzas pkg-alist)))
          (let recur ((stanzas (cadr stanzas))
                      (genfiles '()))
            (if (null? stanzas)
                (begin
                  ;; (format #t "found genfiles: ~A\n" genfiles)
                  genfiles)
                (let ((stanza (car stanzas)))
                  (case (car stanza)
                    ((ocamllex) ;; e.g. (ocamllex point_parser)
                     (recur (cdr stanzas)
                            (concatenate
                             (ocamllex->outfile-names stanza)
                             genfiles)))
                    ((rule)
                     ;; if rule generates an ocaml srcfile, add it
                     (recur (cdr stanzas)
                            (concatenate
                             (rule->ocaml-outfile-names stanza)
                             genfiles)))
                    (else
                     (recur (cdr stanzas) genfiles)))
                  )))))

(define (xnormalize-pkg-tbl pkg-tbl)
  (format #t "xNORMALIZE-PKG-TBL ct: ~A\n" (hash-table-entries pkg-tbl))
  (let ((pt (for-each (lambda (pkg-kv)
                        (format #t "xPKG-KV: ~A\n" pkg-kv)
                        (let* ((pkg-alist (cdr pkg-kv))
                               (format #t "pkg-alist: ~A\n" pkg-alist)
                               (pkg-path (assoc-val :pkg-path pkg-alist))
                               (_ (format #t "pkg-path: ~A\n"  pkg-path))

                               ;; (dune-project-stanzas (read-dune-project-file pkg-path))
                               ;; (static-files (if-let ((s (assoc
                               ;;                            :srcfiles
                               ;;                            pkg-alist)))
                               ;;                       s;;(cadr s)
                               ;;                       '()))
                               ;; (_ (format #t "static-files: ~A\n"
                               ;;            static-files))

                               ;; (genfiles (if (assoc :pkg-path pkg-alist)
                               ;;               (pkg-genfiles pkg-alist)
                               ;;               '()))
                               ;; (_ (format #t "genfiles: ~A\n"
                               ;;            genfiles))

                               ;; ;; (_ (if (not (null? genfiles))
                               ;; ;;        (begin
                               ;; ;;          (format #t "aaaa ~A\n" static-files)
                               ;; ;;          (format #t "xxxx ~A\n" genfiles))
                               ;; ;;        ))
                               ;; (srcfiles (if (null? genfiles)
                               ;;               static-files
                               ;;               (list
                               ;;                :srcfiles
                               ;;                (if (null? static-files)
                               ;;                    `((:ocaml
                               ;;                       ((:generated ,genfiles))))
                               ;;                    (alist-update-in!
                               ;;                     (cadr static-files)
                               ;;                     '(:ocaml)
                               ;;                     (lambda (old-assoc)
                               ;;                       ;; (format #t "old srcs: ~A\n" old-assoc)
                               ;;                       (list
                               ;;                        (cons `(:generated ,genfiles)
                               ;;                              (cadr old-assoc)))))))))
                               )
                          ;; (format #t "SRCFILES: ~A\n" srcfiles)
                          (if-let ((stanzas-lst (assoc :stanzas pkg-alist)))
                                  (let* ((stanzas (normalize-stanzas
                                                   pkg-path
                                                   ;; dune-project-stanzas
                                                   srcfiles stanzas-lst))
                                         ;; FIXME: update-in! pkg?
                                         (new-pkg (if (null? srcfiles)
                                                      (list
                                                       (list :pkg-path pkg-path)
                                                       (list :dune-project
                                                             dune-project-stanzas)
                                                       (list :stanzas stanzas))
                                                      ;; else has srcfiles
                                                      (list
                                                       (list :pkg-path pkg-path)
                                                       (list :dune-project
                                                             dune-project-stanzas)
                                                       (sort-srcfiles srcfiles)
                                                       (list :stanzas stanzas)))))
                                    (hash-table-set! pkg-tbl
                                                     pkg-path
                                                     new-pkg))
                                  (begin
                                    (format #t "MISSING :stanzas ~A\n"
                                            pkg-path)
                                    (let ((new-pkg
                                           (list
                                            (list :pkg-path pkg-path)
                                            (sort-srcfiles srcfiles)
                                            (list :stanzas #f))))
                                      (hash-table-set! pkg-tbl
                                                       pkg-path
                                                       new-pkg))
                                    )
                                  )
                          #f))
                      pkg-tbl)))
  ;; return updated pkg table
    pkg-tbl))

;; (display "loaded dune/dune_normalize.scm") (newline)
