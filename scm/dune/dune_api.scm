(if *mibl-debug-s7-loads*
    (format #t "loading dune/dune_api.scm~%"))

;; (define modules-ht (make-hash-table)) ;; FIXME

;; apodoses in 'select' clauses are not pkg-level build targets
;; remove them from :structures, :signatures
(define (-mark-apodoses! pkg)
  (mibl-trace "-mark-apodoses!" pkg)
  (if-let ((conditionals (assoc-in '(:mibl :library :conditionals) pkg)))
          ;; conditionals val: list of alists
          (let* ((apodoses (apply append
                                  (map (lambda (x)
                                         (let ((sels-alist
                                                (car (assoc-val :selectors x)))
                                               (defaults-alist
                                                 (car (assoc-val :default x))))
                                           (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                                               (format #t "SELS ~A\n" sels-alist))
                                           (cons
                                            defaults-alist
                                            (map cdr sels-alist))))
                                       (cdr conditionals))))
                 (apodoses (map symbol->string apodoses)))
            (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                (format #t "MARKING ~A\n" apodoses))

            (let ((sigs-static (assoc-in '(:signatures :static) pkg))
                  (structs-static (assoc-in '(:structures :static) pkg)))
              (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                  (format #t "structs-static: ~A\n" structs-static))
              (for-each (lambda (s)
                          (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                              (format #t "struct: ~A\n" s))
                          (if (member (last (last s)) apodoses)
                              (set-car! s :_)))
                        (cdr sigs-static))
              (for-each (lambda (s)
                          (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                              (format #t "struct: ~A\n" s))
                          (if (member (last (last s)) apodoses)
                              (set-car! s :_)))
                        (cdr structs-static))
              ))
          (if (or *mibl-debug-all* *mibl-debug-cleanup*)
              (format #t "~A~%" (uwhite "no conditionals")))
          ))

;; hack
(define (-trim-pkg! pkg)
  (if (or *mibl-debug-all* *mibl-debug-cleanup*)
      (format #t "~A: ~A~%" (blue "-trim-pkg!") (assoc-val :pkg-path pkg)))

  ;; remove null lists from :mibl alist
  (let ((dune (assoc :mibl pkg)))
    (set-cdr! dune (remove '() (cdr dune))))

  ;; deps
  (if-let ((deps (assoc-in '(:mibl :rule :deps) pkg)))
          (begin
            ;; (format #t "~A: ~A~%" (red "trimming deps") deps)
            (if (null? (cdr deps))
                (alist-update-in! pkg '(:mibl :rule)
                                  (lambda (old)
                                    (dissoc! '(:deps) old))))))

  ;;;; sigs
  (if-let ((sigs (assoc-in '(:signatures :static) pkg)))
          (if (null? (cdr sigs))
                (assoc-update! :signatures
                               pkg
                               (lambda (old)
                                 (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                                     (format #t "OLD: ~A\n" old))
                                 (set-cdr! old '())))))
  (if-let ((sigs (assoc-in '(:signatures :dynamic) pkg)))
          (if (null? (cdr sigs))
              (assoc-update! :signatures
                             pkg
                             (lambda (old)
                               (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                                   (format #t "OLD: ~A\n" old))
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
                               (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                                   (format #t "OLD: ~A\n" old))
                               (set-cdr! old '())))))

  (if-let ((structs (assoc-in '(:structures :dynamic) pkg)))
          (if (null? (cdr structs))
              (assoc-update! :structures
                             pkg
                             (lambda (old)
                               (if (or *mibl-debug-all* *mibl-debug-cleanup*)
                                   (format #t "OLD: ~A\n" old))
                               (set-cdr! old '())))))

  (if-let ((structs (assoc :structures pkg)))
          (if (null? (cdr structs))
                (dissoc! '(:structures) pkg))))

(define (dune-env->mibl ws pkg stanza)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ublue "dune-env->mibl") stanza))
  ;; (env
  ;;  (<profile1> <settings1>)
  ;;  (<profile2> <settings2>)
  ;;  ...
  ;;  (<profilen> <settingsn>))
  (let* ((stanza-alist (cdr stanza))
         (res
          (map
           (lambda (profile)
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (uwhite "env profile") profile))
             (cons (symbol->keyword (car profile))
                   (map (lambda (fld-assoc)
                          (case (car fld-assoc)
                            ;; ((name) (cons :privname (cadr fld-assoc)))
                            ;; ((public_name) (cons :findlib-name (cadr fld-assoc)))

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
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ublue "dune-tuareg->mibl") stanza))
  (list (list :tuareg
               (list 'FIXME))))

;; pkg-alist is member of (cdr stanza-assoc)
(define (dune-stanza->mibl ws pkg-alist stanza-assoc mibl-stanzas)
  (if *mibl-debug-all*
      (begin
        (format #t "~A\n" (ublue "dune-stanza->mibl"))
        (format #t "~A: ~A\n" (blue "stanza-assoc") stanza-assoc)
        (format #t "~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg-alist))
        (format #t "~A: ~A\n" (blue "mibl-stanzas") mibl-stanzas)))
  ;; (format #t "pkg-alist: ~A\n" pkg-alist)
  ;; (format #t "  mibl-stanzas: ~A\n" mibl-stanzas)
  ;; (error 'x "X")
  (let* ((stanza-alist (cdr stanza-assoc)))
    ;; (format #t "~A: ~A~%" (green "stanza-alist") stanza-alist)

    ;; stanza types: rule, library, executable(s), test(s), alias,
    ;; install, ocamllex, ocamlyacc, menhir, env, tuareg,
    ;; data_only_dirs, documentation, deprecated_library_name
    (case (car stanza-assoc)
      ((rule)
       ;; (format #t "~A~%" (green "RULE"))
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  `,@(dune-rule->mibl ws pkg-alist stanza-assoc)))
       ;; (mibl-trace "mibl-stanzas" mibl-stanzas :color red :test #t)
       )

      ((library)
       ;; (format #t "~A~%" (green "LIB"))
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-library->mibl ws pkg-alist stanza-assoc))))

      ;; ((alias) (normalize-stanza-alias stanza-assoc))
      ;; ((copy_files#) (normalize-stanza-copy_files pkg-path stanza-assoc))
      ;; ((copy_files) (normalize-stanza-copy_files pkg-path stanza-assoc))
      ;; ((copy#) (normalize-stanza-copy pkg-path stanza-assoc))
      ;; ((copy) (normalize-stanza-copy pkg-path stanza-assoc))
      ;; ((data_only_dirs) (normalize-stanza-data_only_dirs stanza-assoc))
      ;; ((env) (normalize-stanza-env stanza-assoc))
      ;; ((executable) (normalize-stanza-executable :executable
      ;;                pkg-path ocaml-srcs stanza-assoc))
      ((executable)
       (let* ((mibl-stanza (dune-executable->mibl ws pkg-alist :executable stanza-assoc))
              (x (append (cdr mibl-stanzas) mibl-stanza)))
         (if *mibl-debug-all*
             (begin
               (format #t  "~A: ~A~%" (yellow "mibl-stanza") mibl-stanza)
               (format #t  "~A: ~A~%" (yellow "x") x)))
         (set-cdr! mibl-stanzas x)))

      ((executables)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executables->mibl
                   ws pkg-alist :executable stanza-assoc))))

      ((tests)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executables->mibl ws pkg-alist :test stanza-assoc))))

      ((test)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executable->mibl ws pkg-alist :test stanza-assoc))))

      ((alias)
       (if (assoc 'action stanza-alist)
           (begin
             ;; action fld removed from alias stanza in dune 2.0

             ;; earlier versions may use it, so we convert to
             ;; std rule stanza-assoc with alias fld
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (red "stanza-assoc before") stanza-assoc))
             (let ((n (car (assoc-val 'name stanza-alist))))
               (set! stanza-assoc (cons :rule
                                        `((alias ,n)
                                          ,@(dissoc '(name) (cdr stanza-assoc))))))
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (red "stanza-assoc after") stanza-assoc))
             (set-cdr! mibl-stanzas
                       (append
                        (cdr mibl-stanzas)
                        (dune-rule->mibl ws pkg-alist stanza-assoc)))
             )
           (set-cdr! mibl-stanzas
                     (append
                      (cdr mibl-stanzas)
                      (dune-alias->mibl ws pkg-alist stanza-assoc)))))

      ((install)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-install->mibl ws pkg-alist stanza-assoc))))

      ((ocamllex)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (lexyacc->mibl :lex ws pkg-alist stanza-assoc))))

      ((ocamlyacc)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (lexyacc->mibl :yacc ws pkg-alist stanza-assoc))))

      ((menhir)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (menhir->mibl ws pkg-alist stanza-assoc))))

      ((env)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-env->mibl ws pkg-alist stanza-assoc))))

      ;; ((:dune-project) stanza-assoc)

      ((tuareg)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-tuareg->mibl ws pkg-alist stanza-assoc))))

      ((data_only_dirs) (values)) ;;FIXME

      ((documentation) (values)) ;;FIXME

      ((deprecated_library_name) (values))

      ((generate_sites_module) (values))

      ((:sh-test) ;; ???
       (values))

      (else
       ;; (format #t "~A: ~A\n" (red "unhandled") stanza-assoc)
       (error 'fixme (format #f "~A: ~A~%" (red "unhandled stanza-assoc") stanza-assoc)))) ;;))
    ;; (format #t "~A: ~A\n" (uwhite "normalized pkg-alist") pkg-alist)
    ;; (format #t "~A~%" (bgred "UPKG-MODULES"))
    ;; (for-each (lambda (m) (format #t "\t~A~%" m)) (assoc-val :modules pkg-alist))

    (-mark-apodoses! pkg-alist)

    ;; remove empty fields
    (-trim-pkg! pkg-alist)

    pkg-alist))

(define (dune-pkg->mibl ws pkg-alist)
  (if *mibl-debug-all*
      (format #t "~A: ~A\n" (bgblue "dune-pkg->mibl")
              (assoc-val :pkg-path pkg-alist)))
  ;; (format #t "~A: ~A\n" (green "ws") ws)
  (let* ((mibl-stanzas (list :mibl)) ;; hack to make sure pkg-alist is always an alist
         (pkg-alist+ (append pkg-alist (list mibl-stanzas)))
         ;;(pkg-alist+ pkg-alist)
         )
    ;; (format #t "pkg-alist+: ~A\n" pkg-alist+) ;; (assoc 'dune pkg-alist+))
    ;; (set-car! dune-stanzas :dune-stanzas)
    (if (assoc 'dune pkg-alist+)
        (let ((new-pkg-alist
               (map
                (lambda (stanza-assoc)
 ;; (format #t "STANZA COPY: ~A\n" stanza)
                  (let ((normed (dune-stanza->mibl ws
                                 pkg-alist+ stanza-assoc mibl-stanzas)))
                    ;; pkg-path
                    ;; ;; dune-project-stanzas
                    ;; srcfiles ;; s/b '() ??
                    ;; stanza)))
                    ;; (format #t "NORMALIZED: ~A\n" normed)
                    normed))
                ;; (cdr dune-stanzas))))
                (assoc-val 'dune pkg-alist+))))

          ;; (format #t "~A: ~A\n" (red "NEW PKG-ALIST") pkg-alist+)
          (let* ((@ws (assoc-val ws *mibl-project*))
                 (exports (car (assoc-val :exports @ws))))
            (if *mibl-debug-all*
                (format #t "~A: ~A~%" (red "exports table") exports)))
          pkg-alist+)
        (begin
          (if *mibl-debug-all*
              (format #t "~A: ~A\n"
                  (red "WARNING: pkg-alist w/o dunefile")
                  (assoc-val :pkg-path pkg-alist)))
          pkg-alist))))

(if *mibl-debug-s7-loads*
    (format #t "loaded dune/dune_api.scm~%"))
