(display "normalize.scm") (newline)

(define modules-ht (make-hash-table)) ;; FIXME

;; original: dune_stanzas.scm Xnormalize-stanza
(define (normalize-dune-stanza pkg stanza)
  (format #t "NORMALIZE-dune-stanza: ~A\n" (car stanza))
  (let* ((stanza-alist (cdr stanza))
         (_ (format #t "stanza-alist ~A\n" stanza-alist))
         (_ (if-let ((nm (assoc 'name stanza-alist)))
                    (format #t "name: ~A\n" nm)
                    (format #t "unnamed\n")))
         (xstanza
          (case (car stanza)
            ((rule)
             (set! pkg (normalize-rule-stanza! pkg stanza)))
                       ;; pkg-path ocaml-srcs stanza))

              ;; ((alias) (normalize-stanza-alias stanza))
              ;; ((copy_files#) (normalize-stanza-copy_files pkg-path stanza))
              ;; ((copy_files) (normalize-stanza-copy_files pkg-path stanza))
              ;; ((copy#) (normalize-stanza-copy pkg-path stanza))
              ;; ((copy) (normalize-stanza-copy pkg-path stanza))
              ;; ((data_only_dirs) (normalize-stanza-data_only_dirs stanza))
              ;; ((env) (normalize-stanza-env stanza))
              ;; ((executable) (normalize-stanza-executable :executable
              ;;                pkg-path ocaml-srcs stanza))

              ;; ((executables) (normalize-stanza-executables :executables
              ;;                 pkg-path ocaml-srcs stanza))

              ;; ((install) (normalize-stanza-install
              ;;             pkg-path
              ;;             ;;dune-project-stanzas
              ;;             stanza))

              ((library)
               (begin
                 ;; (format #t "Normalizing lib stanza ~A\n"
                 ;;         (assoc-val 'name stanza-alist))
                 (if (null-library? stanza)
                     '()
                     (let ((submodules-assoc (modules-fld->submodules-fld
                                              (assoc 'modules (cdr stanza))
                                              (assoc :modules pkg)
                                              modules-ht))
                           ;; add submodules-assoc to stanza
                           )
                       (format #t "submodules-assoc: ~A\n" submodules-assoc)
                       (normalize-library-stanza submodules-assoc)))))
                      ;; pkg-path ocaml-srcs stanza))))

              ;; ((ocamllex) (normalize-stanza-ocamllex stanza))

              ;; ((ocamlyacc) (normalize-stanza-ocamllex stanza))

              ;; ((test) (normalize-stanza-test pkg-path ocaml-srcs stanza))
              ;; ((tests) (normalize-stanza-tests pkg-path ocaml-srcs stanza))

              ;; ((:dune-project) stanza)

              (else
               (format #t "normalize-dune-stanza unhandled: ~A\n" stanza)))))
    ;; (format #t "normalized pkg: ~A\n" pkg)
    pkg))

  ;;   ;; update global public -> private name table
  ;;   ;; (case (car stanza)
  ;;   ;;   ((executables)
  ;;   ;;    (begin))
  ;;   ;;   ((library)
  ;;   ;;    (begin
  ;;   ;;       (let* ((private-name (assoc-in '(:name :private) (cadr s)))
  ;;   ;;              (public-name  (assoc      :public_name   (cadr s))))
  ;;   ;;         (if (and private-name public-name)
  ;;   ;;             (begin
  ;;   ;;               ;; (format #t "writing ~A => ~A\n" private-name public-name)
  ;;   ;;               (hash-table-set! private-name->public-name
  ;;   ;;                                (cadr private-name)
  ;;   ;;                                (cadr public-name)))))
  ;;   ;;       ))
  ;;   ;;   )

  ;;   ;; return normalized stanza
  ;;   s))

(define (normalize-dune-stanzas pkg)
  (format #t "NORMALIZE-dune-stanzas, pkg: ~A"
          ;;(assoc-val :pkg-path pkg)
          (assoc-val :dune-stanzas pkg)
          ) (newline)
  (map
   (lambda (stanza)
     ;; (format #t "STANZA: ~A" (cdr stanza)) (newline)
     (format #t "STANZA nm: ~A" (car stanza)) (newline)
     (let ((normed (normalize-dune-stanza pkg stanza)))
                    ;; pkg-path
                    ;; ;; dune-project-stanzas
                    ;; srcfiles ;; s/b '() ??
                    ;; stanza)))
       (format #t "normalized: ~A\n" normed)
       normed))
   (assoc-val :dune-stanzas pkg))
  )

    ;; normed
    ;; (format #t "normalized stanzas: ~A\n" normed)

;; (define foo
;;     ;; 'executables' normalizes to a list of 'executable' so we flatten
;;     (let ((result (let recur ((stanzas normed))
;;                     (if (null? stanzas) '()
;;                         (begin
;;                           ;; (format #t "  Stanza: ~A\n" (car stanzas))
;;                           (if (pair? (caar stanzas))
;;                               (concatenate (car stanzas)
;;                                            (recur (cdr stanzas)))
;;                               (concatenate
;;                                `(,(car stanzas))
;;                                (recur (cdr stanzas)))))))))
;;       ;; (format #t "Renormalized stanzas: ~A\n" result)
;;       result)
;;     )

;; (define (normalize-pkg pkg)
;;   (pretty-print pkg) (newline) (newline)
;;   (for-each (stanza)
;;             (normalize-stanza stanza)
;;             (assoc-val :stanzas pkg)))

;; (define (normalize-pkg-tbl pkg-tbl)
;;   (format #t "NORMALIZE-PKG-TBL ct: ~A\n" (hash-table-entries pkg-tbl))
;;   (let ((npt (for-each (lambda (pkg-kv)
;;                         (normalize-pkg (cdr pkg-kv)))
;;                       pkg-tbl)))
;;     npt))

