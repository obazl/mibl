(format #t "loading dune/fields.scm\n")

(define (-get-modules pkg deps wrapped? stanza-alist)
  (format #t "-get-modules: ~A\n" deps)
  (if deps
      (if-let ((submods+sigs-list
                (modules-fld->submodules-fld
                 (assoc 'modules stanza-alist)
                 ;; files
                 (assoc :modules pkg)
                 ;; deps
                 (assoc-val :signatures pkg)
                 (assoc-val :structures pkg)
                 )))
              (begin
                (format #t "submods+sigs-list: ~A\n" submods+sigs-list)
                (format #t "submodules-list: ~A\n"
                        (reverse (car submods+sigs-list)))
                (format #t "subsigs-list: ~A\n"
                        (reverse (cdr submods+sigs-list)))
                (if wrapped?
                    submods+sigs-list
                    (cons ':manifest submods+sigs-list)))
              '())
      '()))


;;FIXME: UPDATE FILE FIELDS (structs or sigs) with conditional targets
;; i.e. conditional target foo.ml => (:structs (:dynamic "foo.ml))
;; case: static a.mli, conditional a.ml
;;          -> (:modules (A (:ml "a.ml")(:mli :dyn "a.mli")))
;; of (:modules (A (:ml "a.ml")(:_mli "a.mli")))
;; e.g. prefix '_' means "dynamic"
;; then we can discard :static and :dynamic for (:modules)
(define dune-library-deps-fld->mibl
  (let ((+documentation+ "convert 'libraries' field of dune library stanza to mibl format. libdeps is a pair (fldname val)")
        (+signature+ '(dune-library-deps-fld->mibl libdeps pkg)))
    (lambda (libdeps pkg)
      (format #t "~A: ~A\n" (blue "DUNE-library-deps-fld->MIBL") libdeps)
      (let-values (((directs seldeps conditionals modules)
                    (analyze-libdeps libdeps)))
        (format #t "~A: ~A\n" (red "LIBDEPS pkg sigs")
                (assoc-val :signatures pkg))
        (format #t "~A: ~A\n" (red "LIBDEPS pkg structs")
                (assoc-val :structures pkg))
        (format #t "~A: ~A\n" (red "LIBDEPS DIRECTS") directs)
        (format #t "~A: ~A\n" (red "LIBDEPS SELDEPS") seldeps)
        (format #t "~A: ~A\n" (red "LIBDEPS CONDITIONALS") conditionals)
        (format #t "~A: ~A\n" (red "LIBDEPS MODULES") modules)

        ;; if conditionals: update :structures or :signatures pkg flds

        (let* ((deps (if (null? directs) '() directs))
               (deps (if (null? seldeps)
                         deps (cons deps
                                      (list (cons :seldeps seldeps)))))
               (deps (if (null? conditionals)
                         (list deps)
                         (append deps (list
                                       (list :conditionals conditionals))))))
          (filter (lambda (d) (not (null? d))) deps))))))

(define (dune-library-fields->mibl pkg stanza-alist wrapped?)
  (format #t "~A: ~A\n" (blue "DUNE-LIBRARY-FIELDS->MIBL") stanza-alist)
  (format #t "~A: ~A\n" (blue "pkg") pkg)
  (format #t "~A: ~A\n" (blue "wrapped?") wrapped?)

  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-library-deps-fld->mibl libdeps pkg)
                       '()))
         (_ (format #t "MIBLDEPS: ~A\n" deps))
         ;; FIXME: deal with private_modules too
         (modules (-get-modules pkg deps wrapped? stanza-alist))
         (flds (map
                (lambda (fld-assoc)
                  (format #t "lib fld-assoc: ~A\n" fld-assoc)
                  (case (car fld-assoc)
                    ((name) `(:privname ,(cadr fld-assoc)))
                    ((public_name) `(:pubname ,(cadr fld-assoc)))

                    ((flags) (normalize-stanza-fld-flags fld-assoc :mod))
                    ((library_flags) (normalize-stanza-fld-flags fld-assoc :lib))
                    ;; (values) returns "nothing"
                    ((libraries) (values)) ;; handled separately
                    ((modules) (values)) ;; handled separately

                    ((wrapped) (values))
                    (else
                     (begin
                       ;; (format #t "unhandled lib fld: ~A\n" fld-assoc)
                       fld-assoc))
                    ) ;; end case
                  ) ;; end lamda
                stanza-alist) ;; end map
               )) ;; end let bindings
    ;; now handle modules and submodules
    (format #t "~A: ~A\n" (red "DEPS FLDS") deps)
    (let* ((depslist
            (if deps (remove '()
                             (list :deps
                                   (if-let ((fixed (assoc :fixed deps)))
                                           fixed '())
                                   (if-let ((conds (assoc :conditionals deps)))
                                           conds '())
                                   (if-let ((seldeps (assoc :seldeps deps)))
                                           seldeps '())))
                '()))
           (_ (format #t "modules: ~A\n" modules))
           (submods
            (if modules
                (if-let ((submods-assoc (assoc :submodules modules)))
                        (let ((submods-list (cdr submods-assoc)))
                          (cons :submodules (sort! submods-list sym<?)))
                        '())
                '()))
           (subsigs (if modules
                        (if-let ((ssigs (assoc :subsigs modules)))
                                ssigs '())
                        '())))
          (format #t "SUBMODS:: ~A\n" submods)
          (format #t "SUBSIGS:: ~A\n" subsigs)
          (append flds (remove '() (list depslist
                                         (if wrapped? '(:namespaced #t)
                                             '(:namespaced #f))
                                         submods
                                         subsigs))))
    ))

(format #t "loaded dune/fields.scm\n")
