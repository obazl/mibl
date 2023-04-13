(if *mibl-debug-s7-loads*
    (format #t "loading dune/fields.scm\n"))

;; mv select targets to pkg :modules
;; rm select apodoses from pkg :structures, :signatures
;; xpkg: (p . pkg), so we can set! pkg by (set-cdr! x)
;; FIXME: replace with updaters.scm::update-pkg-files!

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
      (if *mibl-debug-all*
          (format #t "~A: ~A\n" (ublue "dune-libraries-fld->mibl") libdeps))
      (let-values (((directs seldeps conditionals) ;; modules)
                    (analyze-libdeps libdeps)))
        (if *mibl-debug-all*
            (begin
              (format #t "~A: ~A\n" (blue ">LIBDEPS DIRECTS") directs)
              (format #t "~A: ~A\n" (blue ">LIBDEPS SELDEPS") seldeps)
              (format #t "~A: ~A\n" (blue ">LIBDEPS CONDITIONALS") conditionals)
              ;; (format #t "~A: ~A\n" (blue ">LIBDEPS MODULES") modules)
              ))
        ;; if conditionals: update :structures or :signatures pkg flds
        ;; FIXME: we can do this later
        (if conditionals
            (let (;;(ctargets (conditional-targets conditionals))
                  (_ (if *mibl-debug-all* (format #t "~A~%" (uwhite "updating conditionals"))))
                  (ctargets (fold (lambda (conditional accum)
                                    (if *mibl-debug-all*
                                        (format #t "conditional ~A\n" conditional))
                                    (let ((newpkg (update-pkg-conditionals! pkg conditional)))
                                      (if *mibl-debug-all*
                                          (format #t "~A: ~A~%" (bgred "newpkg") newpkg))
                                      (set! pkg newpkg)
                                      (if *mibl-debug-all*
                                          (format #t "~A: ~A~%" (bgred "conditional") conditional))
                                      (cons (assoc-val :target conditional) accum)))
                                  '() conditionals)))
              (if *mibl-debug-all*
                  (begin
                    (format #t "~A: ~A\n" (uwhite "ctargets") ctargets)
                    (format #t "~A: ~A\n" (uwhite "updated pkg") pkg)))
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

(if *mibl-debug-s7-loads*
    (format #t "loaded dune/fields.scm\n"))
