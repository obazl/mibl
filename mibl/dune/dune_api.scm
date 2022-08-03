;; (display "normalize.scm") (newline)

;; (define modules-ht (make-hash-table)) ;; FIXME

;; apodoses in 'select' clauses are not pkg-level build targets
;; remove them from :structures, :signatures
(define (-mark-apodoses! pkg)
  ;; (format #t "-mark-apodoses! ~A\n" pkg)
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

(define (-trim-pkg! pkg)
  ;; (format #t "~A: ~A~%" (blue "-trim-pkg!") pkg)

  ;; remove null lists from :dune alist
  (let ((dune (assoc :dune pkg)))
    (set-cdr! dune (remove '() (cdr dune))))
  ;; deps
  (if-let ((deps (assoc-in '(:dune :rule :deps) pkg)))
          (begin
            ;; (format #t "~A: ~A~%" (red "trimming deps") deps)
            (if (null? (cdr deps))
                (alist-update-in! pkg '(:dune :rule)
                                  (lambda (old)
                                    (dissoc! '(:deps) old))))))

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

(define (dune-stanza->mibl ws pkg stanza nstanzas)
  (format #t "~A: ~A\n" (blue "dune-stanza->mibl") stanza)
  (format #t "pkg: ~A\n" pkg)
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
                        (dune-rule->mibl ws pkg stanza))))

            ((library)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-library->mibl ws pkg stanza))))

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
                        (dune-executable->mibl ws pkg :executable stanza))))

            ;; ((executables) (normalize-stanza-executables :executables
            ;;                 pkg-path ocaml-srcs stanza))

            ((test)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-executable->mibl ws pkg :test stanza))))
                        ;; (dune-test->mibl ws pkg stanza))))
            ;; (normalize-stanza-test pkg-path ocaml-srcs stanza))

            ;; ((tests) (normalize-stanza-tests pkg-path ocaml-srcs stanza))


            ((alias)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-alias->mibl ws pkg stanza))))


            ;; ((install) (normalize-stanza-install
            ;;             pkg-path
            ;;             ;;dune-project-stanzas
            ;;             stanza))

            ;; ((ocamllex) (normalize-stanza-ocamllex stanza))

            ;; ((ocamlyacc) (normalize-stanza-ocamllex stanza))

            ;; ((:dune-project) stanza)

              (else
               ;; (format #t "~A: ~A\n" (red "unhandled") stanza)
               (error 'fixme (format #f "~A: ~A~%" (red "unhandled stanza") stanza))))))
    ;; (format #t "normalized pkg: ~A\n" pkg)

    (-mark-apodoses! pkg)

    ;; remove empty fields
    (-trim-pkg! pkg)

    pkg))

(define (dune-pkg->mibl ws pkg)
  (format #t "~A: ~A\n" (blue "dune-pkg->mibl") pkg)
  (format #t "~A: ~A\n" (green "ws") ws)
  (let* ((nstanzas (list :dune )) ;; hack to make sure pkg is always an alist
         (pkg+ (append pkg (list nstanzas)))
         ;;(pkg+ pkg)
         )
    (format #t "pkg+: ~A\n" pkg+) ;; (assoc 'dune pkg+))
    ;; (set-car! dune-stanzas :dune-stanzas)
    (if (assoc 'dune pkg+)
        (let ((new-pkg
               (map
                (lambda (stanza)
 ;; (format #t "STANZA COPY: ~A\n" stanza)
                  (let ((normed (dune-stanza->mibl ws
                                 pkg+ stanza nstanzas)))
                    ;; pkg-path
                    ;; ;; dune-project-stanzas
                    ;; srcfiles ;; s/b '() ??
                    ;; stanza)))
                    ;; (format #t "NORMALIZED: ~A\n" normed)
                    normed))
                ;; (cdr dune-stanzas))))
                (assoc-val 'dune pkg+))))
          (format #t "~A: ~A\n" (red "NEW PKG") pkg+)
          pkg+)
        (begin
          (format #t "~A: ~A\n"
                  (red "WARNING: pkg w/o dunefile")
                  (assoc-val :pkg-path pkg))
          pkg))))
