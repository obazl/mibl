;; (display "normalize.scm") (newline)

;; (define modules-ht (make-hash-table)) ;; FIXME

;; apodoses in 'select' clauses are not pkg-level build targets
;; remove them from :structures, :signatures
(define (-mark-apodoses! pkg)
  (format #t "~A: ~A\n" (ublue "-mark-apodoses!") (assoc-val :pkg-path pkg))
  ;; (if (equal? (car (assoc-val :pkg-path pkg)) "compiler/tests-ocaml/lib-bytes-utf")
  ;;     (begin
  ;;       (format #t "~A: ~A~%" (bgred "pkg") pkg))
  ;;       (error 'x "apod"))
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
              ))
          (format #t "~A~%" (uwhite "no conditionals"))
          ))

(define (-trim-pkg! pkg)
  (format #t "~A: ~A~%" (blue "-trim-pkg!") pkg) ;; (assoc-val :pkg-path pkg))

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

(define (dune-env->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (ublue "dune-env->mibl") stanza)
  ;; (env
  ;;  (<profile1> <settings1>)
  ;;  (<profile2> <settings2>)
  ;;  ...
  ;;  (<profilen> <settingsn>))
  (let* ((stanza-alist (cdr stanza))
         (res
          (map
           (lambda (profile)
             (format #t "~A: ~A~%" (uwhite "env profile") profile)
             (cons (symbol->keyword (car profile))
                   (map (lambda (fld-assoc)
                          (case (car fld-assoc)
                            ;; ((name) (cons :privname (cadr fld-assoc)))
                            ;; ((public_name) (cons :pubname (cadr fld-assoc)))

                            ((flags) (normalize-stanza-fld-flags fld-assoc :compile))
                            ((ocamlc_flags) (normalize-stanza-fld-flags fld-assoc :ocamlc))
                            ((ocamlopt_flags) (normalize-stanza-fld-flags fld-assoc :ocamlopt))
                            ((link_flags) (normalize-stanza-fld-flags fld-assoc :link))

                            ;; ((c_flags) (normalize-stanza-fld-flags fld-assoc :archive))
                            ;; ((cxx_flags) (normalize-stanza-fld-flags fld-assoc :archive))

                            ((env-vars) (cons :env-vars
                                              (cdr fld-assoc)))
                            ;; ((menhir_flags) (values))

                            ;; ((js_of_ocaml) (values))

                            ;; ((binaries) (values))
                            ;; ((inline_tests) (values))
                            ;; ((odoc) (values))
                            ;; ((coq) (values))
                            ;; ((formatting) (values))

                            (else
                             (error 'fixme (format #f "unhandled env fld: ~A~%" fld-assoc)))
                            ) ;; end case
                          ) ;; end lambda
                        (cdr profile)) ;; end map
                   )) ;; end lamda
           stanza-alist)))
    (list (cons :env
                res))))

(define (dune-tuareg->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (ublue "dune-tuareg->mibl") stanza)
  (list (list :tuareg
               (list 'FIXME))))

(define (dune-stanza->mibl ws pkg stanza nstanzas)
  (format #t "~A: ~A\n" (blue "dune-stanza->mibl") stanza)
  ;; (format #t "pkg: ~A\n" pkg)
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
             (let* ((mibl-stanza (dune-executable->mibl ws pkg :executable stanza))
                    (x (append (cdr nstanzas) mibl-stanza)))
               (format #t  "~A: ~A~%" (yellow "mibl-stanza") mibl-stanza)
               (format #t  "~A: ~A~%" (yellow "x") x)
               (set-cdr! nstanzas x)))

            ((executables)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-executables->mibl
                         ws pkg :executable stanza))))

            ((tests)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-executables->mibl ws pkg :test stanza))))

            ((test)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-executable->mibl ws pkg :test stanza))))

            ((alias)
             (if (assoc 'action stanza-alist)
                 (begin
                   ;; action fld removed from alias stanza in dune 2.0

                   ;; earlier versions may use it, so we convert to
                   ;; std rule stanza with alias fld
                   (format #t "~A: ~A~%" (red "stanza before") stanza)
                   (let ((n (car (assoc-val 'name stanza-alist))))
                     (set! stanza (cons :rule
                                        `((alias ,n)
                                          ,@(dissoc '(name) (cdr stanza))))))
                   (format #t "~A: ~A~%" (red "stanza after") stanza)
                   (set-cdr! nstanzas
                             (append
                              (cdr nstanzas)
                              (dune-rule->mibl ws pkg stanza)))
                   )
                 (set-cdr! nstanzas
                           (append
                            (cdr nstanzas)
                            (dune-alias->mibl ws pkg stanza)))))

            ((install)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-install->mibl ws pkg stanza))))

            ((ocamllex)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (lexyacc->mibl :ocamllex ws pkg stanza))))

            ((ocamlyacc)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (lexyacc->mibl :ocamlyacc
                                                  ws pkg stanza))))

            ((menhir)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (menhir->mibl ws pkg stanza))))

            ((env)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-env->mibl ws pkg stanza))))

            ;; ((:dune-project) stanza)

            ((tuareg)
             (set-cdr! nstanzas
                       (append
                        (cdr nstanzas)
                        (dune-tuareg->mibl ws pkg stanza))))

            ((data_only_dirs) (values)) ;;FIXME

            ((documentation) (values)) ;;FIXME

            ((:sh-test) ;; ???
             (values))

            (else
               ;; (format #t "~A: ~A\n" (red "unhandled") stanza)
               (error 'fixme (format #f "~A: ~A~%" (red "Unhandled stanza") stanza))))))
    ;; (format #t "~A: ~A\n" (uwhite "normalized pkg") pkg)
    ;; (format #t "~A~%" (bgred "UPKG-MODULES"))
    ;; (for-each (lambda (m) (format #t "\t~A~%" m)) (assoc-val :modules pkg))

    (-mark-apodoses! pkg)

    ;; remove empty fields
    (-trim-pkg! pkg)

    pkg))

(define (dune-pkg->mibl ws pkg)
  (format #t "~A: ~A\n" (blue "dune-pkg->mibl")
          (assoc-val :pkg-path pkg))
  ;; (format #t "~A: ~A\n" (green "ws") ws)
  (let* ((nstanzas (list :dune )) ;; hack to make sure pkg is always an alist
         (pkg+ (append pkg (list nstanzas)))
         ;;(pkg+ pkg)
         )
    ;; (format #t "pkg+: ~A\n" pkg+) ;; (assoc 'dune pkg+))
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

          ;; (format #t "~A: ~A\n" (red "NEW PKG") pkg+)
          (let* ((@ws (assoc-val ws -mibl-ws-table))
                 (exports (car (assoc-val :exports @ws))))
            (format #t "~A: ~A~%" (red "exports table") exports))

          pkg+)
        (begin
          (format #t "~A: ~A\n"
                  (red "WARNING: pkg w/o dunefile")
                  (assoc-val :pkg-path pkg))
          pkg))))
