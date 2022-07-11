;; (display "normalize.scm") (newline)

;; (define modules-ht (make-hash-table)) ;; FIXME

;; apodoses in 'select' clauses are not pkg-level build targets
;; remove them from :structures, :signatures
(define (-mark-apodoses! pkg)
  (format #t "-mark-apodoses! ~A\n" pkg)
  (if-let ((conditionals (assoc-in '(:dune :library :conditionals) pkg)))
         ;; conditionals val: list of alists
          (let* ((apodoses (apply append
                                 (map (lambda (x)
                                        (let ((sels-alist
                                               (car (assoc-val :selectors x)))
                                              (defaults-alist
                                                (car (assoc-val :default x))))
                                          (format #t "SELS ~A\n" sels-alist)
                                          (cons
                                           defaults-alist
                                           (map cdr sels-alist))))
                                      (cdr conditionals))))
                (apodoses (map symbol->string apodoses)))
            (format #t "MARKING ~A\n" apodoses)

            (let ((sigs-static (assoc-in '(:signatures :static) pkg))
                  (structs-static (assoc-in '(:structures :static) pkg)))
              (format #t "structs-static: ~A\n" structs-static)
              (for-each (lambda (s)
                          (format #t "struct: ~A\n" s)
                          (if (member (last (last s)) apodoses)
                              (set-car! s :_)))
                        (cdr sigs-static))
              (for-each (lambda (s)
                          (format #t "struct: ~A\n" s)
                          (if (member (last (last s)) apodoses)
                              (set-car! s :_)))
                        (cdr structs-static))
              ))))

(define (_trim-pkg-sigs-structs! pkg)
  ;;;; sigs
  (if-let ((sigs (assoc-in '(:signatures :static) pkg)))
          (if (null? (cdr sigs))
                (assoc-update! :signatures
                               pkg
                               (lambda (old)
                                 (format #t "OLD: ~A\n" old)
                                 (set-cdr! old '())))))
  (if-let ((sigs (assoc-in '(:signatures :dynamic) pkg)))
          (if (null? (cdr sigs))
              (assoc-update! :signatures
                             pkg
                             (lambda (old)
                               (format #t "OLD: ~A\n" old)
                               (set-cdr! old '())))))

  (if-let ((sigs (assoc :signatures pkg)))
          (if (null? (cdr sigs))
              (dissoc! '(:signatures) pkg)))

  ;;;; structs
  (if-let ((structs (assoc-in '(:structures :static) pkg)))
          (if (null? (cdr structs))
              (assoc-update! :structures
                             pkg
                             (lambda (old)
                               (format #t "OLD: ~A\n" old)
                               (set-cdr! old '())))))

  (if-let ((structs (assoc-in '(:structures :dynamic) pkg)))
          (if (null? (cdr structs))
              (assoc-update! :structures
                             pkg
                             (lambda (old)
                               (format #t "OLD: ~A\n" old)
                               (set-cdr! old '())))))

  (if-let ((structs (assoc :structures pkg)))
          (if (null? (cdr structs))
                (dissoc! '(:structures) pkg))))

;; original: dune_stanzas.scm Xnormalize-stanza
(define (dune-stanza->mibl pkg stanza nstanzas)
  (format #t "~A: ~A\n" (blue "dune-stanza->mibl") stanza)
  (format #t "  PKG: ~A\n" pkg)
  ;; (format #t "  nstanzas: ~A\n" nstanzas)
  (let* ((stanza-alist (cdr stanza))
         ;; (_ (format #t "stanza-alist ~A\n" stanza-alist))
         ;; (_ (if-let ((nm (assoc 'name stanza-alist)))
         ;;            (format #t "name: ~A\n" nm)
         ;;            (format #t "unnamed\n")))
         (xstanza
          (case (car stanza)
            ((rule)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-rule->mibl pkg stanza))))

            ((library)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-library->mibl pkg stanza))))

            ;; ((alias) (normalize-stanza-alias stanza))
            ;; ((copy_files#) (normalize-stanza-copy_files pkg-path stanza))
            ;; ((copy_files) (normalize-stanza-copy_files pkg-path stanza))
            ;; ((copy#) (normalize-stanza-copy pkg-path stanza))
            ;; ((copy) (normalize-stanza-copy pkg-path stanza))
            ;; ((data_only_dirs) (normalize-stanza-data_only_dirs stanza))
            ;; ((env) (normalize-stanza-env stanza))
            ;; ((executable) (normalize-stanza-executable :executable
            ;;                pkg-path ocaml-srcs stanza))
            ((executable)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-executable->mibl pkg stanza))))

            ;; ((executables) (normalize-stanza-executables :executables
            ;;                 pkg-path ocaml-srcs stanza))

            ;; ((install) (normalize-stanza-install
            ;;             pkg-path
            ;;             ;;dune-project-stanzas
            ;;             stanza))

            ;; ((ocamllex) (normalize-stanza-ocamllex stanza))

            ;; ((ocamlyacc) (normalize-stanza-ocamllex stanza))

            ;; ((test) (normalize-stanza-test pkg-path ocaml-srcs stanza))
            ;; ((tests) (normalize-stanza-tests pkg-path ocaml-srcs stanza))

            ;; ((:dune-project) stanza)

              (else
               (format #t "dune-stanza->mibl unhandled: ~A\n" stanza)))))
    ;; (format #t "normalized pkg: ~A\n" pkg)

    (-mark-apodoses! pkg)

    ;; remove empty :signatures, :structures
    (_trim-pkg-sigs-structs! pkg)

    pkg))

(define (dune-pkg->mibl pkg)
  (format #t "~A: ~A\n" (blue "dune-pkg->mibl") pkg)
  (let* ((nstanzas (list :dune))
         (pkg+ (append pkg (list nstanzas))))
    ;; (format #t "STANZAS COPY: ~A\n" dune-stanzas)
    ;; (set-car! dune-stanzas :dune-stanzas)
    (let ((new-pkg
           (map
            (lambda (stanza)
              ;; (format #t "STANZA COPY: ~A\n" stanza)
              (let ((normed (dune-stanza->mibl pkg+ stanza nstanzas)))
                ;; pkg-path
                ;; ;; dune-project-stanzas
                ;; srcfiles ;; s/b '() ??
                ;; stanza)))
                ;; (format #t "NORMALIZED: ~A\n" normed)
                normed))
            ;; (cdr dune-stanzas))))
            (assoc-val 'dune pkg+))))
      ;; (format #t "NEW PKG: ~A\n" pkg+)
      pkg+)))
