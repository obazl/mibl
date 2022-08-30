;; (format #t "loading dune/fields.scm\n")

;; mv select targets to pkg :modules
;; rm select apodoses from pkg :structures, :signatures
;; xpkg: (p . pkg), so we can set! pkg by (set-cdr! x)
;; FIXME: replace with pkg_api.scm::update-pkg-files!
(define (-update-pkg-files! pkg conditional)
  (format #t "~A: ~A\n" (ublue "-update-pkg-files!") conditional)
  (let* ((ctarget (car (assoc-val :target conditional)))
         (modules (if-let ((ms (assoc-val :modules pkg)))
                          ms
                          (begin
                            (set-cdr! pkg
                                      (append (cdr pkg)
                                              '((:modules))))
                            (assoc-val :modules (cdr pkg)))))
         ;;FIXME: dynamics too
         (sigs (assoc-in '(:signatures :static) pkg))
         (structs (assoc-in '(:structures :static) pkg)))
    (format #t "~A: ~A\n" (magenta "pkg") pkg)
    (format #t "~A: ~A\n" (magenta "cond target") ctarget)
    (format #t "~A: ~A\n" (magenta "pkg modules") modules)
    (format #t "~A: ~A\n" (magenta "pkg sigs") sigs)
    (format #t "~A: ~A\n" (magenta "pkg structs") structs)

    ;; remove apodoses from :structures, :signatures
    (format #t "~A~%" (red "removing apodoses from :structures"))
    (let ((apodoses (cons
                     (car (assoc-val :default conditional))
                     (map cdr (assoc-val :selectors conditional)))))
      (format #t "~A: ~A\n" (blue "collected apodoses") apodoses)
      ;; first structs
      (for-each (lambda (apo)
                  (format #t "~A: ~A~%" (uwhite "testing structs") apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr structs))))
                    (format #t "struct apomatch: ~A\n" match)
                    (if match
                        (set-cdr! structs (dissoc! (list (car match))
                                                   (cdr structs))))
                    ))
                apodoses)
      (format #t "~A~%" (red "removing apodoses from :signatures"))
      (for-each (lambda (apo)
                  (format #t "~A: ~A~%" (uwhite "testing sigs") apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr sigs))))
                    (format #t "sigs apomatch: ~A\n" match)
                    (if match
                        (set-cdr! sigs (dissoc! (list (car match))
                                                (cdr sigs))))
                    ))
                apodoses)
      )

    (format #t "~A: ~A~%" (red "updating :modules from :signatures") ctarget)
    (if (structfile? ctarget)
        (let* ((sigtarget (string->symbol (format #f "~Ai" ctarget)))
               (match (find-if (lambda (e)
                                 ;; (format #t "e: ~A\n" e)
                                 (eq? sigtarget (cdr e)))
                               (cdr sigs))))
          ;; fixme: (if match ...
          (let ((newmod (cons (car match)
                              (list (cons :ml_ ctarget)
                                    (cons :mli (cdr match))))))
            (format #t "newmod (sigs): ~A\n" newmod)
            (set-cdr! modules (append (cdr modules) (list newmod)))
            (set-cdr! sigs (dissoc! match (cdr sigs))))
          (format #t "~A: ~A\n" (magenta "pkg modules") modules)
          ))

    (format #t "~A~%" (red "updating :modules from :structures"))
    (if (sigfile? ctarget)
        (let* ((structtarget (string->symbol
                              (string-drop-right (format #f "~A" ctarget)
                                                 1)))
               (_ (format #t "structtarget: ~A\n" structtarget))
               (match (find-if (lambda (e)
                                 (format #t "e: ~A\n" e)
                                 (eq? structtarget (cdr e)))
                               (cdr structs))))
          (format #t "match: ~A\n" match)
          (let ((newmod (cons (car match)
                              (list (cons :ml (cdr match))
                                    (cons :mli_ ctarget)))))
            (format #t "newmod (structs): ~A\n" newmod)
            (set-cdr! modules (append (cdr modules) (list newmod)))
            (set-cdr! structs (dissoc! match (cdr structs))))
          ))
    (format #t "~A: ~A\n" (magenta "pkg (updated)") pkg)
    ))


;;FIXME: UPDATE FILE FIELDS (structs or sigs) with conditional targets
;; i.e. conditional target foo.ml => (:structs (:dynamic "foo.ml))
;; case: static a.mli, conditional a.ml
;;          -> (:modules (A (:ml "a.ml")(:mli :dyn "a.mli")))
;; of (:modules (A (:ml "a.ml")(:_mli "a.mli")))
;; e.g. prefix '_' means "dynamic"
;; then we can discard :static and :dynamic for (:modules)
(define dune-libraries-fld->mibl
  (let ((+documentation+ "convert 'libraries' field of dune library stanza to mibl format. libdeps is a pair (fldname val)")
        (+signature+ '(dune-libraries-fld->mibl libdeps pkg)))
    (lambda (libdeps pkg)
      (format #t "~A: ~A\n" (ublue "dune-libraries-fld->mibl") libdeps)
      (let-values (((directs seldeps conditionals modules)
                    (analyze-libdeps libdeps)))
        (format #t "~A: ~A\n" (blue ">LIBDEPS DIRECTS") directs)
        (format #t "~A: ~A\n" (blue ">LIBDEPS SELDEPS") seldeps)
        (format #t "~A: ~A\n" (blue ">LIBDEPS CONDITIONALS") conditionals)
        (format #t "~A: ~A\n" (blue ">LIBDEPS MODULES") modules)

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

;; (format #t "loaded dune/fields.scm\n")
