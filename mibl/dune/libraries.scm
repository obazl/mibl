(if *debugging*
    (format #t "dune/dune_stanza_library.scm loading ...~%"))

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
  (if *debugging*
      (format #t "normalize-instrumentation: ~A\n" fld-assoc)))

(define (fld-warning stanza-sym fld-assoc)
  (if *debugging*
      (format #t "~A - ~A ~A ~A: ~A~%"
          (ured "WARNING")
          (red "unhandled fld in")
          (ured stanza-sym)
          (red "stanza")
          fld-assoc)))

(define (fld-error stanza-sym fld-assoc)
  (stacktrace)
  (error 'FIXME (format #f "~A ~A: ~A~%"
                        (ured "unhandled stanza fld") stanza-sym fld-assoc)))

(define (-map-lib-flds->mibl ws pkg stanza-alist)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-map-lib-flds->mibl") stanza-alist))
  (let* ((stanza-name (car (assoc-val 'name stanza-alist)))
         (result (map
                 (lambda (fld-assoc)
                   (if *debugging*
                       (format #t "lib fld-assoc: ~A\n" fld-assoc))
                   (case (car fld-assoc)

                     ((libraries) (values)) ;; handled separately
                     ((modules) (values)) ;; handled separately
                     ((inline_tests) (values)) ;; handled separately
                     ;; ((ilts (assoc-val 'inline_tests stanza-alist)))
                     ;; (cons :inline-tests (inline-tests->mibl ws pkg
                     ;;                                         (cdr fld-assoc)
                     ;;                                         stanza-alist)))

                     ((preprocess) ;; NB: (preprocess pps) handled separately
                      ;; (if (equal? 'no_preprocessing (cadr fld-assoc))
                      ;;     (error 'PP "Unhandled pp: no_preprocess"))
                      ;; (if (equal? 'no_preprocessing (cadr fld-assoc))
                      ;;     (error 'PP "Unhandled pp: no_preprocess"))
                      ;; (if (assoc 'action (cdr fld-assoc))
                      ;;     (error 'PP (format #f "Unhandled pp action: ~A" fld-assoc)))
                      ;; TODO: handle (preprocess (action)), (preprocess no_preprocessing),
                      ;; and (preprocess future_syntax)
                      (values))

                     ((name) (cons :privname (cadr fld-assoc)))
                     ((public_name) (cons :pubname (cadr fld-assoc)))

                     ((flags) (normalize-stanza-fld-flags fld-assoc :compile))
                     ((library_flags) (normalize-stanza-fld-flags fld-assoc :archive))
                     ((ocamlc_flags) (normalize-stanza-fld-flags fld-assoc :ocamlc))
                     ((ocamlopt_flags) (normalize-stanza-fld-flags fld-assoc :ocamlopt))

                     ((modules_without_implementation)
                      (cons :sigs (cdr fld-assoc)))
                     ((empty_module_interface_if_absent) (fld-error 'library fld-assoc))
                     ((private_modules) (fld-error 'library fld-assoc))
                     ((root_module) (fld-error 'library fld-assoc))
                     ((allow_overlapping_dependencies) (fld-error 'library fld-assoc))

                     ((optional) (cons :optional #t))
                     ((preprocessor_deps) `(:unhandled ,@fld-assoc))

                     ((synopsis) (cons :docstring (cadr fld-assoc)))

                     ((virtual_deps) `(:unhandled ,@fld-assoc))

                     ((wrapped) (values))

                     ;; cc
                     ((c_flags) `(:unhandled ,@fld-assoc))

                     ((cxx_flags) (fld-error 'library fld-assoc))
                     ((c_library_flags) `(:unhandled ,@fld-assoc))
                     ((c_types) (fld-error 'library fld-assoc))
                     ((install_c_headers) (fld-error 'library fld-assoc))
                     ((foreign_archives) (foreign-archives->mibl fld-assoc))
                     ((c_names) (c_names->mibl stanza-name (cdr fld-assoc)))
                     ((foreign_stubs) (foreign-stubs->mibl
                                       (if-let ((n (assoc-val 'name stanza-alist)))
                                               (car n)
                                               (car (assoc-val 'public_name stanza-alist)))
                                       fld-assoc))

                     ((js_of_ocaml) (cons :jsoo (cdr fld-assoc)))

                     ;; ppx
                     ((kind) (if (eq? 'ppx_rewriter (cadr fld-assoc))
                                 `(:ppx-rewriter . #t)
                                 (if (eq? 'ppx_deriver (cadr fld-assoc))
                                     `(:ppx-deriver . #t))))

                     ((ppx_runtime_libraries) `(:ppx-codeps ,@(cdr fld-assoc)))

                     ((inline_tests.backend)
                      (cons :inline-tests-backend (cdr fld-assoc)))

                     ;; other
                     ((lint) (fld-warning 'library fld-assoc)
                      `(:unhandled ,fld-assoc))

                     ;; ((enabled_if) (fld-error 'library fld-assoc))

                     (else  (fld-error (car fld-assoc) fld-assoc))
                     ) ;; end case
                   ) ;; end lamda
                 stanza-alist)))
    (if *debugging*
        (format #t "~A: ~A~%" (red "REsult") result))
    result))

(define (-lib-flds->mibl ws pkg stanza-alist wrapped?)
  (if *debugging*
      (begin
        (format #t "~A: ~A\n" (blue "-lib-flds->mibl") stanza-alist)
        (format #t "~A: ~A\n" (blue "pkg") pkg)
        (format #t "~A: ~A\n" (blue "wrapped?") wrapped?)))

  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  ;; ppxes may involve by (preprocess) and (inline_tests), so do not map,
  ;; handle separately

  (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-libraries-fld->mibl libdeps pkg)
                       '()))
         (_ (if *debugging* (format #t "lib MIBLDEPS: ~A\n" deps)))
         ;; (_ (error 'tmp "tmp"))

         ;; FIXME: deal with private_modules too
         (modules (get-manifest pkg :lib wrapped? stanza-alist)) ;;  deps
         (_ (if *debugging* (format #t "~A: ~A\n" (red "lib get-modules") modules)))

         ;; FIXME: separate handling of (inline_tests) toplevel fld and ppx
         (ppx (if-let ((ilts (assoc-val 'inline_tests stanza-alist)))
                      (inline-tests->mibl ws pkg ilts stanza-alist)))

         (ppx (if ppx
                  ppx
                  (let ((preproc (assoc-val 'preprocess stanza-alist)))
                    (if *debugging*
                        (format #t "~A: ~A~%" (red "lib preproc") preproc))
                    (if preproc
                        (if (alist? preproc)
                            (if (assoc-in '(preprocess pps) stanza-alist)
                                (lib-ppx->mibl stanza-alist)
                                (lib-preproc->mibl stanza-alist))

                                ;; (if (assoc-in '(preprocess staged-pps) stanza-alist)
                                ;;     (lib-ppx->mibl stanza-alist)
                                ;;     (lib-preproc->mibl stanza-alist))
                                ;; )
                            (if (member 'future_syntax preproc)
                                `(:future-syntax #t)
                                (if (member 'no_preprocessing preproc)
                                    #f
                                    (error 'FIXME "bad (preprocessing) fld?"))))
                        #f))))

         ;; (ppx (lib-ppx->mibl stanza-alist))
         ;;(preprocess-fld->mibl fld-assoc stanza-alist))
         (_ (if (or *debug-ppx* *debugging*) (format #t "~A: ~A~%" (bgyellow ":PPX") ppx)))

         (lib-flds (-map-lib-flds->mibl ws pkg stanza-alist))
         (_ (if *debugging* (format #t "lib-flds (mibl): ~A~%" lib-flds)))

         (lib-flds (if wrapped?
                       (append (list (cons :ns ;;(assoc-val :privname lib-flds)))
                                           (if-let ((pn
                                                     (assoc-val :privname lib-flds)))
                                                   pn
                                                   (assoc-val :pubname lib-flds))
                                           ))
                               lib-flds)
                       lib-flds))
         (_ (if *debugging* (format #t "lib-flds (2): ~A~%" lib-flds)))
         (_ (if (or *debug-ppx* *debugging*) (format #t "lib-flds ppx: ~A~%" ppx)))
         (lib-flds (if ppx
                       (append lib-flds
                               (if (alist? ppx)
                                   ppx
                                   (list ppx)))
                       lib-flds))

         (_ (if *debugging* (format #t "lib-flds (3): ~A~%" lib-flds)))
         ) ;; end let bindings

    ;; now handle modules (modules fld) and submodules (deps fld)
    ;; (format #t "~A: ~A\n" (red "libDEPS") deps)
    (let* ((depslist
            (if deps (remove '()
                             (list :deps
                                   (if-let ((fixed (assoc :remote deps)))
                                           fixed '())
                                   (if-let ((conds (assoc :conditionals deps)))
                                           conds '())
                                   (if-let ((seldeps (assoc :seldeps deps)))
                                           seldeps '())))
                '()))
           ;; (_ (if *debugging* (format #t "libModules: ~A\n" modules))
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

      (append lib-flds
              (remove '() (list depslist
                                (cons
                                 (car modules)
                                 (append (cdr modules)
                                         `((:raw
                                            ,(assoc 'modules stanza-alist)))))
                                ;;submods
                                ;;subsigs
                                ))))
    ))

(define (dune-library->mibl ws pkg stanza)
  (if *debugging*
      (begin
        (format #t "~A: ~A\n" (blue "dune-library->mibl")
                (assoc-val 'name (cdr stanza)))
        (format #t "stanza: ~A\n" stanza)))

  ;; FIXME: if not wrapped => :archive
  ;; else => :library

  ;; add lib names to exports table
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
        (exports (car (assoc-val :exports (assoc-val ws -mibl-ws-table))))
        (privname (if-let ((privname (assoc-val 'name (cdr stanza))))
                          (car privname) #f))
        (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "privname") privname)))
        (pubname (if-let ((pubname (assoc-val 'public_name (cdr stanza))))
                         (car pubname) #f))
        (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "pubname") pubname)))
        )
    ;; libs may be referenced w/o ns, e.g. mylib,
    ;; or (in rule actions) w/ns, e.g. lib:mylib
    ;; we register both pub and priv names just to make sure refs are resolved
    ;; and we register both with and without lib: tag

    (if privname
        (begin
          (if *debugging*
              (format #t "~A: ~A~%" (ucyan "lib:adding privname to exports") privname))
          (update-exports-table! ws #f
                                 (assoc-val 'modes (cdr stanza))
                                 ;; (string->symbol (format #f "~A" privname))
                                 privname
                                 pkg-path privname)
          ;; (update-exports-table! ws :lib privname
          ;;                        pkg-path privname)
          ))

    (if pubname
        (begin
          (if *debugging*
              (format #t "~A: ~A~%" (ucyan "lib:adding pubname to exports") pubname))
          (update-exports-table! ws #f
                                 (assoc-val 'modes (cdr stanza))
                                 ;; (string->symbol (format #f "~A" pubname))
                                 pubname
                                 pkg-path privname)
          (update-exports-table! ws :lib (assoc-val :modes (cdr stanza))
                                 pubname pkg-path privname)

          ;; opam table entry
          ;; Key: 'js_of_ocaml-compiler
          ;; val: ((:lib (compiler/lib priv js_of_ocaml_compiler)
          ;;               ((:files
          ;;                 ;;(dir file file ...)
          ;;                 (compiler/bin-js_of_ocaml runtime.js)
          ;;                 (runtime bigarray.js ...))))
          ;;         (:bin (compiler/bin-js_of_ocaml js_of_ocaml)
          ;;               (compiler/bin-jsoo_minify jsoo_minify))
          ;;         (:man ...)
          ;;         (:sub (runtime-files ;; the pkg/lib public name
          ;;                (compiler/lib-runtime-files
          ;;                 js_of_ocaml_compiler_runtime_files))
          ;;               (findlib-support
          ;;                (compiler/lib-findlib-support
          ;;                 jsoo_findlib_support))
          ;;               ;; two levels
          ;;               (foo (path/to liba)
          ;;                    (:sub (path/b/to libb)))))

          (update-opam-table! ws :lib
                              pubname ;; opam pkg name
                              pubname ;; lib name
                              pkg-path
                              privname ;; lib name
                              )))

    (let* ((stanza-alist (cdr stanza))
           (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                                 stanza-alist
                                 (append stanza-alist
                                         (list '(modules :standard)))))
           (_ (if *debugging* (format #t "STANZA ALIST: ~A\n" stanza-alist)))

           ;; (privname (assoc-val 'name stanza-alist))
           (wrapped? (if-let ((wrapped (assoc-val 'wrapped stanza-alist)))
                             (if (equal? 'false (car wrapped))
                                 #f
                                 #t)
                             #t))
           ;; (submods (lib-stanza-submodules stanza-alist))
           ;; (stanza-alist (cons submods stanza-alist))
           ;; (_ (if *debugging* (format #t "STANZA ALIST + SUBMODS: ~A\n" stanza-alist)))

           ;; CONVERT THE STANZA:
           (mibl-stanza (-lib-flds->mibl ws pkg stanza-alist wrapped?))
           (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "mibl-stanza") mibl-stanza)))
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
      ;; (ocaml_library) and remove (:ns . <nsname>) assoc. OOPS! This
      ;; does not work since at this stage we may miss generated
      ;; files. So this case must be handled during emit stage.

      ;; NOPE - client code may assume this archive is namespaced, so
      ;; we cannot simplify.
      ;; (let ((res
      ;;        (if-let ((ns (assoc-val :ns mibl-stanza)))
      ;;                (begin
      ;;                  (format #t "~A: ~A~%" (uwhite "ns") ns)
      ;;                  (let ((submods (assoc-in '(:manifest :modules) mibl-stanza)))
      ;;                    (if (= 1 (length (cdr submods)))
      ;;                        (let ((submod (cadr submods))
      ;;                              (ns-mod (normalize-module-name ns)))
      ;;                          (format #t "~A: ~A~%" (bgred "1 submod") submod)
      ;;                          (format #t "~A: ~A~%" (uwhite "ns mod") ns-mod)
      ;;                          (if (equal? ns-mod submod)
      ;;                              (begin
      ;;                                (format #t "~A: ~A~%" (bgred "converting to lib") mibl-stanza)
      ;;                                (list (cons :library
      ;;                                            (dissoc '(:ns) mibl-stanza))))
      ;;                              ;; else 1 submodule w/diff name from ns name
      ;;                              (list (cons kind mibl-stanza))))
      ;;                        ;; else multiple submodules
      ;;                        (list (cons kind mibl-stanza)))
      ;;                    ))
      ;;                ;; else no nos
      ;;                (list (cons kind mibl-stanza)))))
      ;;   (format #t "~A: ~A~%" (uwhite "lib result") res)
      ;;   ;; (error 'STOP "STOP libs")
      ;;   res)
      (list (cons kind mibl-stanza))
      )))

;; (display "loaded dune/dune_stanza_library.scm") (newline)

