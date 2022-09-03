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

(define (-inline-tests->mibl fld-assoc)
  (format #t "~A: ~A\n" (ublue "-inline-tests->mibl") fld-assoc)
  (let ((result
         (map (lambda (fld)
                (format #t "~A: ~A~%" (uwhite "fld") fld)
                (case (car fld)
                  ((flags)
                   ;; FIXME: modes. if (modes native),
                   ;; treat flags as ocamlopt-flags.
                   (normalize-stanza-fld-flags fld :compile))
                  ((modes) (cons :modes (cdr fld)))
                  (else
                   fld)))
              (cdr fld-assoc))))
    (format #t "~A: ~A~%" (bgred "result") result)
    result))

(define (fld-error stanza-sym fld-assoc)
  (stacktrace)
  (error 'FIXME (format #f "~A ~A: ~A~%"
                        (ured "unhandled stanza fld") stanza-sym fld-assoc)))

(define (-map-lib-flds->mibl stanza-alist)
  (format #t "~A: ~A~%" (blue "-map-lib-flds->mibl") stanza-alist)
  (map
   (lambda (fld-assoc)
     (format #t "lib fld-assoc: ~A\n" fld-assoc)
     (case (car fld-assoc)
       ((name) (cons :privname (cadr fld-assoc)))
       ((public_name) (cons :pubname (cadr fld-assoc)))

       ((flags) (normalize-stanza-fld-flags fld-assoc :compile))
       ((library_flags) (normalize-stanza-fld-flags fld-assoc :archive))
       ((ocamlc_flags) (normalize-stanza-fld-flags fld-assoc :ocamlc))
       ((ocamlopt_flags) (normalize-stanza-fld-flags fld-assoc :ocamlopt))

       ((inline_tests) (cons :inline-tests
                             (-inline-tests->mibl fld-assoc)))

       ((libraries) (values)) ;; handled separately
       ((modules) (values)) ;; handled separately
       ((modules_without_implementation)
        (cons :sigs (cdr fld-assoc)))
       ((empty_module_interface_if_absent) (fld-error 'library fld-assoc))
       ((private_modules) (fld-error 'library fld-assoc))
       ((root_module) (fld-error 'library fld-assoc))
       ((allow_overlapping_dependencies) (fld-error 'library fld-assoc))

       ((optional) (cons :optional #t))
       ((preprocess) (preprocess-fld->mibl fld-assoc stanza-alist))
       ((preprocessor_deps) (fld-error 'library fld-assoc))

       ((synopsis) (cons :docstring (cadr fld-assoc)))

       ((virtual_deps) (fld-error 'library fld-assoc))

       ((wrapped) (values))

       ((js_of_ocaml) (fld-error 'library fld-assoc))

       ;; cc
       ((c_flags) (fld-error stanza-kind fld-assoc))
       ((cxx_flags) (fld-error stanza-kind fld-assoc))
       ((c_library_flags) (fld-error 'library fld-assoc))
       ((c_types) (fld-error 'library fld-assoc))
       ((install_c_headers) (fld-error 'library fld-assoc))
       ((foreign_archives) (fld-error 'library fld-assoc))
       ((foreign_stubs) (fld-error 'library fld-assoc))
       ;; ppx
       ((kind) ;; ignore, not meaningful for obazl?
        (values))
       ((ppx_runtime_libraries) (fld-error 'library fld-assoc))

       ;; other
       ((enabled_if) (fld-error 'library fld-assoc))

       (else (error 'FIXME
                    (format #f "unhandled lib fld: ~A" fld-assoc)))
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
                       (dune-libraries-fld->mibl libdeps pkg)
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
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
        (exports (car (assoc-val :exports (assoc-val ws -mibl-ws-table))))
        (privname (if-let ((privname (assoc-val 'name (cdr stanza))))
                          (car privname) #f))
        (_ (format #t "~A: ~A~%" (uwhite "privname") privname))
        (pubname (if-let ((pubname (assoc-val 'public_name (cdr stanza))))
                         (car pubname) privname))
        (_ (format #t "~A: ~A~%" (uwhite "pubname") pubname))
        )
    ;; libs may be referenced w/o ns, e.g. mylib,
    ;; or (in rule actions) w/ns, e.g. lib:mylib
    ;; we register both pub and priv names just to make sure refs are resolved
    ;; and we register both with and without lib: tag

    (if privname
        (begin
          (update-exports-table! ws #f
                                 ;; (string->symbol (format #f "~A" privname))
                                 privname
                                 pkg-path privname)
          ;; (update-exports-table! ws :lib privname
          ;;                        pkg-path privname)
          ))

    (if pubname
        (begin
          (update-exports-table! ws #f
                                 ;; (string->symbol (format #f "~A" pubname))
                                 pubname
                                 pkg-path privname)
          (update-exports-table! ws :lib pubname
                                 pkg-path privname)))

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

           (kind (if wrapped?
                     (if *wrapped-libs-to-ns-archives*
                         :ns-archive :ns-library)
                     (if *unwrapped-libs-to-archives*
                         :archive :library)))
           )

      ;; SPECIAL CASE: if there is only one submodule and it has the
      ;; same name as the ns, then convert to non-namespaced :library
      ;; (ocaml_library) and remove (:ns . <nsname>) assoc.
      (let ((res
             (if-let ((ns (assoc-val :ns mibl-stanza)))
                     (begin
                       (format #t "~A: ~A~%" (uwhite "ns") ns)
                       (let ((submods (assoc-in '(:manifest :modules) mibl-stanza)))
                         (if (= 1 (length (cdr submods)))
                             (let ((submod (cadr submods))
                                   (ns-mod (normalize-module-name ns)))
                               (format #t "~A: ~A~%" (bgred "1 submod") submod)
                               (format #t "~A: ~A~%" (uwhite "ns mod") ns-mod)
                               (if (equal? ns-mod submod)
                                   (begin
                                     (format #t "~A: ~A~%" (bgred "converting to lib") mibl-stanza)
                                     (list (cons :library
                                                 (dissoc '(:ns) mibl-stanza))))
                                   ;; else 1 submodule w/diff name from ns name
                                   (list (cons kind mibl-stanza))))
                             ;; else multiple submodules
                             (list (cons kind mibl-stanza)))
                         ))
                     ;; else no nos
                     (list (cons kind mibl-stanza)))))
        (format #t "~A: ~A~%" (uwhite "lib result") res)
        ;; (error 'STOP "STOP libs")
        res)
      )))

;; (display "loaded dune/dune_stanza_library.scm") (newline)

