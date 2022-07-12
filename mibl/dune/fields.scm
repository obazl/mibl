(format #t "loading dune/fields.scm\n")

;; mv select targets to pkg :modules
;; rm select apodoses from pkg :structures, :signatures
;; xpkg: (p . pkg), so we can set! pkg by (set-cdr! x)
(define (-update-pkg-files! pkg conditional)
  (format #t "~A: ~A\n" (blue "-update-pkg-files!") conditional)
  (let* ((target (car (assoc-val :target conditional)))
         (modules (if-let ((ms (assoc-in '(:modules :static) pkg)))
                          ms
                          (begin
                            (set-cdr! pkg
                                      (append pkg '((:modules (:static)))))
                            (assoc-in '(:modules :static) (cdr pkg)))))
         (sigs (assoc-in '(:signatures :static) pkg))
         (structs (assoc-in '(:structures :static) pkg)))
    (format #t "~A: ~A\n" (red "pkg") pkg)
    (format #t "~A: ~A\n" (magenta "target") target)
    (format #t "~A: ~A\n" (magenta "pkg modules") modules)
    (format #t "~A: ~A\n" (magenta "pkg sigs") sigs)
    (format #t "~A: ~A\n" (magenta "pkg structs") structs)

    ;; remove apodoses from :structures, :signatures
    (let ((apodoses (cons
                     (car (assoc-val :default conditional))
                     (map cdr (assoc-val :selectors conditional)))))
      (format #t "~A: ~A\n" (red "apodoses") apodoses)
      ;; first structs
      (for-each (lambda (apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr structs))))
                    (format #t "apomatch: ~A\n" match)
                    (if match
                        (set-cdr! structs (dissoc! (list (car match))
                                                   (cdr structs))))
                    ))
                apodoses)
      ;; then sigs
      (for-each (lambda (apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr sigs))))
                    (format #t "apomatch: ~A\n" match)
                    (if match
                        (set-cdr! sigs (dissoc! (list (car match))
                                                (cdr sigs))))
                    ))
                apodoses)
      )

    ;; update :modules from :signatures
    (if (structfile? target)
        (let* ((sigtarget (string->symbol (format #f "~Ai" target)))
               (match (find-if (lambda (e)
                                 ;; (format #t "e: ~A\n" e)
                                 (eq? sigtarget (cdr e)))
                               (cdr sigs))))
          ;; fixme: (if match ...
          (let ((newmod (cons (car match)
                              (list (cons :ml_ target)
                                    (cons :mli (cdr match))))))
            (format #t "newmod (sigs): ~A\n" newmod)
            (set-cdr! modules (append (cdr modules) (list newmod)))
            (set-cdr! sigs (dissoc! match (cdr sigs))))
          (format #t "~A: ~A\n" (magenta "pkg modules") modules)
          )
        ;; update :modules from :structures
        (if (sigfile? target)
            (let* ((structtarget (string->symbol
                                  (string-drop-right (format #f "~A" target)
                                                     1)))
                   (_ (format #t "structtarget: ~A\n" structtarget))
                   (match (find-if (lambda (e)
                                     (format #t "e: ~A\n" e)
                                     (eq? structtarget (cdr e)))
                                   (cdr structs))))
              (format #t "match: ~A\n" match)
              (let ((newmod (cons (car match)
                                  (list (cons :ml (cdr match))
                                        (cons :mli_ target)))))
                (format #t "newmod (structs): ~A\n" newmod)
                (set-cdr! modules (append (cdr modules) (list newmod)))
                (set-cdr! structs (dissoc! match (cdr structs))))
              )))
    (format #t "~A: ~A\n" (magenta "pkg (updated)") pkg)
    ))


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
        (format #t "~A: ~A\n" (red "LIBDEPS DIRECTS") directs)
        (format #t "~A: ~A\n" (red "LIBDEPS SELDEPS") seldeps)
        (format #t "~A: ~A\n" (red "LIBDEPS CONDITIONALS") conditionals)
        (format #t "~A: ~A\n" (red "LIBDEPS MODULES") modules)

        ;; if conditionals: update :structures or :signatures pkg flds
        (if conditionals
            (let (;;(ctargets (conditional-targets conditionals))
                  (ctargets (fold (lambda (conditional accum)
                                    (format #t
                                            "conditional ~A\n" conditional)
                                    (set! pkg (-update-pkg-files! pkg
                                                        conditional))
                                    (append (assoc-val :target conditional)
                                          accum))
                                  '() conditionals)))
              (format #t "ctargets: ~A\n" ctargets)
              (format #t "updated pkg: ~A\n" pkg)
              ;; (error 'tmp "tmp")

              (let* ((deps (if (null? directs) '() directs))
                     (deps (if (null? seldeps)
                               deps (cons deps
                                          (list (cons :seldeps seldeps)))))
                     (deps (if (null? conditionals)
                               (list deps)
                               (append deps (list
                                             (cons :conditionals conditionals))))))
                (filter (lambda (d) (not (null? d))) deps))))))))

(define (-lib-flds->mibl stanza-alist)
  (format #t "~A: ~A~%" (blue "-lib-flds->mibl") stanza-alist)
  (map
   (lambda (fld-assoc)
     (format #t "lib fld-assoc: ~A\n" fld-assoc)
     (case (car fld-assoc)
       ((name) (cons :privname (cadr fld-assoc)))
       ((public_name) (cons :pubname (cadr fld-assoc)))

       ((flags) (normalize-stanza-fld-flags fld-assoc :mod))
       ((library_flags) (normalize-stanza-fld-flags fld-assoc :lib))
       ;; (values) returns "nothing"
       ((libraries) (values)) ;; handled separately
       ((modules) (values)) ;; handled separately

       ((wrapped) (values))
       (else
        (begin
          (format #t "~A: ~A\n" (red "unhandled lib fld") fld-assoc)
          fld-assoc))
       ) ;; end case
     ) ;; end lamda
   stanza-alist))

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
         ;; (_ (error 'tmp "tmp"))

         ;; FIXME: deal with private_modules too
         (modules (get-manifest pkg wrapped? stanza-alist)) ;;  deps
         (_ (format #t "~A: ~A\n" (red "get-modules") modules))
         (lib-flds (-lib-flds->mibl stanza-alist))
         (_ (format #t "lib-flds (mibl): ~A~%" lib-flds))
         (lib-flds (if wrapped?
                       (append (list (cons :ns
                                            (assoc-val :pubname lib-flds)))
                               lib-flds)
                       lib-flds))
         ) ;; end let bindings

    ;; now handle modules (modules fld) and submodules (deps fld)
    (format #t "~A: ~A\n" (red "DEPS") deps)
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
           ;; (submods
           ;;  (if modules
           ;;      (if-let ((submods-assoc (assoc :submodules modules)))
           ;;              (let ((submods-list (cdr submods-assoc)))
           ;;                (cons :submodules (sort! submods-list sym<?)))
           ;;              '())
           ;;      '()))
           ;; (subsigs (if modules
           ;;              (if-let ((ssigs (assoc :subsigs modules)))
           ;;                      ssigs '())
           ;;              '()))
           )
      ;; (format #t "SUBMODS:: ~A\n" submods)
      ;; (format #t "SUBSIGS:: ~A\n" subsigs)
      (append lib-flds (remove '() (list depslist
                                         modules
                                         ;;submods
                                         ;;subsigs
                                         ))))
    ))

(format #t "loaded dune/fields.scm\n")
