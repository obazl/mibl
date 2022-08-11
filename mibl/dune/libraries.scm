;; (display "dune/dune_stanza_library.scm loading ...") (newline)

;; (load "dune_stanza_fields.scm")
;; (load "lookup_tables.scm")
;; (load "string.scm")
;; (load "utils.scm")
;; (require pp.scm)

;; (define (ppx-fld->args ppx)
;;   )

;; a null lib has an empty modules fld:  (modules)
(define (null-library? stanza)
  ;; (format #t "null-library? ~A" stanza) (newline)
  (let ((modules (assoc-val :modules (cdr stanza))))
    (if modules
        (if (null? (cdr modules))
            #t
            #f)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e.g. mina/src/lib/with_hash: (instrumentation (backend bisect_ppx))
(define (normalize-instrumentation fld-assoc)
  (format #t "normalize-instrumentation: ~A\n" fld-assoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inline_tests - generate ocaml_test
;; task: construct 'test' stanza to serve as test runner?
;; "Under the hood, a test executable is built by dune."

;; special cases: no 'backend' fld needed for the following preprocess
;; ppx libs:
;;   ppx_inline_test
;;   ppx_expect

;; question: should we "select" ppx_inline_test compilation? If we do
;; not intend to run inline tests, then we should not need to compile
;; with inline test support.

;; e.g. if no test, then omit ppx preproc on modules? but what if we
;; have multiple ppx libs? in that case, just omit ppx_inline_test
;; from the ppx_executable.

;; BUT: see ppx_inline_test/src/dune - the lib stanza specifies
;; 'inline_test.backend' that seems to build the required executable?

;; yes, 'backend' means build/run testrunner.
;; see https://dune.readthedocs.io/en/stable/tests.html?highlight=generate_runner#defining-your-own-inline-test-backend

(define (normalize-inline_tests fld-assoc stanza-alist)
  (format #t "normalize-inline_tests: ~A\n" fld-assoc))

(define (-map-lib-flds->mibl stanza-alist)
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
       ((modules_without_implementation)
        (cons :sigs (cdr fld-assoc)))

       ((preprocess) (preprocess-fld->mibl fld-assoc stanza-alist))
       ((preprocessor_deps) ;; handled along with preprocess fld
        (values))

       ((wrapped) (values))
       (else (error 'fixme (format #f "unhandled lib fld: ~A~%" fld-assoc)))
       ) ;; end case
     ) ;; end lamda
   stanza-alist))

(define (-lib-flds->mibl pkg stanza-alist wrapped?)
  (format #t "~A: ~A\n" (blue "-lib-flds->mibl") stanza-alist)
  (format #t "~A: ~A\n" (blue "pkg") pkg)
  (format #t "~A: ~A\n" (blue "wrapped?") wrapped?)

  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-library-deps-fld->mibl libdeps pkg)
                       '()))
         (_ (format #t "lib MIBLDEPS: ~A\n" deps))
         ;; (_ (error 'tmp "tmp"))

         ;; FIXME: deal with private_modules too
         (modules (get-manifest pkg wrapped? stanza-alist)) ;;  deps
         (_ (format #t "~A: ~A\n" (red "lib get-modules") modules))

         (lib-flds (-map-lib-flds->mibl stanza-alist))
         (_ (format #t "lib-flds (mibl): ~A~%" lib-flds))
         (lib-flds (if wrapped?
                       (append (list (cons :ns
                                           (if-let ((pn
                                                     (assoc-val :pubname lib-flds)))
                                                   pn
                                                   (assoc-val :privname lib-flds))))
                               lib-flds)
                       lib-flds))
         ) ;; end let bindings

    ;; now handle modules (modules fld) and submodules (deps fld)
    ;; (format #t "~A: ~A\n" (red "libDEPS") deps)
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
           ;; (_ (format #t "libModules: ~A\n" modules))
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

(define (dune-library->mibl ws pkg stanza)
  (format #t "~A: ~A\n" (blue "dune-library->mibl")
          (assoc-val 'name (cdr stanza)))
  (format #t "stanza: ~A\n" stanza)

  ;; FIXME: if not wrapped => :archive
  ;; else => :library

  ;; add lib names to exports table
  (let ((pkg-path (car (assoc-val :pkg-path pkg)))
        (exports (car (assoc-val :exports (assoc-val ws -mibl-ws-table))))
        (privname (assoc-val 'name (cdr stanza)))
        (pubname (assoc-val 'public_name (cdr stanza))))
    (format #t "hidden exports: ~A\n" exports)

    ;; libs may be referenced w/o ns, e.g. mylib,
    ;; or (in rule actions) w/ns, e.g. lib:mylib
    ;; we register both pub and priv names just to make sure refs are resolved

    (update-exports-table! ws
                           (string->symbol (format #f "~A" (car privname))) ;; key
                               (car privname) pkg-path)
    (update-exports-table! ws
                           (string->symbol (format #f "~A" (car pubname))) ;; key
                               (car privname) pkg-path)

    (if privname
        (update-exports-table! ws
                               (string->symbol (format #f "lib:~A" (car privname))) ;; key
                               (car privname) pkg-path))
    (if pubname
        (update-exports-table! ws
                               (string->symbol (format #f "lib:~A" (car pubname))) ;; key
                               (car privname) pkg-path))

    (let* ((stanza-alist (cdr stanza))
           (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                                 stanza-alist
                                 (append stanza-alist
                                         (list '(modules :standard)))))
           (_ (format #t "STANZA ALIST: ~A\n" stanza-alist))

           ;; (privname (assoc-val 'name stanza-alist))
           (wrapped? (if-let ((wrapped (assoc-val 'wrapped stanza-alist)))
                             (if (equal? 'false (car wrapped))
                                 #f
                                 #t)
                             #t))
           ;; (submods (lib-stanza-submodules stanza-alist))
           ;; (stanza-alist (cons submods stanza-alist))
           ;; (_ (format #t "STANZA ALIST + SUBMODS: ~A\n" stanza-alist))

           ;; CONVERT THE STANZA:
           (mibl-stanza (-lib-flds->mibl pkg stanza-alist wrapped?))
           (_ (format #t "~A: ~A~%" (uwhite "mibl-stanza") mibl-stanza))
           (mibl-stanza (filter (lambda (fld)
                                  ;; remove empties e.g. (:deps)
                                  (not (null? (cdr fld))))
                                mibl-stanza))
           )

      ;; namespaced?
      (let ((res (list (cons (if wrapped?
                                 (if *wrapped-libs-to-ns-archives*
                                     :ns-archive :ns-library)
                                 (if *unwrapped-libs-to-archives*
                                     :archive :library))
                             mibl-stanza))))
        res)
      )))

;; (display "loaded dune/dune_stanza_library.scm") (newline)

