;; (display "dune/executables.scm loading ...\n")

;; executable optional fields:
;; public_name - intallation name; "same as adding following stanza:"
;;    (install
;;      (section bin)
;;      (files (<name>.exe as <public-name>)))
;; package
;; libraries
;; link_flags
;; link_deps
;; modules
;; root_module
;; modes
;; preprocess
;; preprocessor_deps
;; js_of_ocaml
;; flags
;; modules_without_implementation
;; allow_overlapping_dependencies
;; optional
;; enabled_if
;; promote
;; foreign_stubs
;; foreign_archives
;; forbidden_libraries
;; embed_in_plugin_libraries
;; ctypes
;; empty_module_interface_if_absent

;; NOTE: executable deps. In OBazl, executables (ocaml_binary) may
;; directly depend on modules only, not aggregates. It makes no sense
;; for an executable to depend on a library, unless that means "add
;; each module in the lib to the exectuable." IOW, we could list a
;; library in the modules field but not the deps field. So dune's
;; 'libraries' field makes no sense for executables.

;; The problem is that dune conflates compilation and linking. The
;; 'executable' stanza can list modules to be included in the
;; executable, so dune must compile them, which means the executable
;; stanza must have 'libraries' and other compile-related fields (e.g.
;; preprocess) to configure the module compilation.

;; So our task here is to disentangle the mess. First we expand the
;; 'modules' field, and tag the result :manifest. But then we need to
;; also need to detach the modules and 'libraries' field from the
;; executable stanza. It only needs the manifest, but we also need to
;; express the compilation part. So we generate a :compilation field
;; to hold that stuff.

;; We also need to disentangle the 'flags' field; for example '-open
;; Foo' is for module compilation, not linkage of executables.

(define (-exec-flags->mibl stanza-alist)
  (format #t "~A: ~A\n"
          (blue "-exec-flags->mibl") (assoc-val 'flags stanza-alist))
  (let* ((flags (assoc 'flags stanza-alist))
         (flags (normalize-stanza-fld-flags flags :exec))
         (ocamlc_flags (if-let ((ocflags (assoc 'ocamlc_flags stanza-alist)))
                               (cons :ocamlc (cdr ocflags)) '()))
         (ocamlopt_flags (if-let ((ocflags
                                   (assoc 'ocamlopt_flags stanza-alist)))
                                 (cons :ocamlopt (cdr ocflags)) '()))
         (link-flags (if-let ((lf (assoc 'link_flags stanza-alist)))
                             lf '()))
         ;; integrate flags into link-flags
         )
    ;; (_ (format #t "exec flags: ~A\n" flags))
    ;; (compile-flags '("-compile-flags"))
    ;; (link-flags '("-link_flags")))

    (let-values (((standard opens options flags)
                  (flags->mibl (assoc 'flags stanza-alist))))
      (format #t "~A: ~A~%" (red "standard") standard)
      (format #t "~A: ~A~%" (red "opens")    opens)
      (format #t "~A: ~A~%" (red "options")  options)
      (format #t "~A: ~A~%" (red "flags")    flags)
      (format #t "~A: ~A~%" (red "ocamlc_flags") ocamlc_flags)
      (format #t "~A: ~A~%" (red "ocamlopt_flags") ocamlopt_flags)
      (format #t "~A: ~A~%" (red "link_flags") link-flags)

      ;; deal with empty lists
      (let* ((cflags (remove '() (list standard opens options flags)))
             (tcflags (remove '() (list ocamlc_flags ocamlopt_flags)))
             (compile-flags (append cflags tcflags))
             (link-flags (if (null? flags) '()
                             (remove '()
                                     (list standard options
                                           (if (null? link-flags)
                                               flags
                                               (cons (car flags)
                                                     (append (cdr link-flags)
                                                             (cdr flags)))))))))

        (values
         (if (null? compile-flags) '() (list (cons :opts compile-flags)))
         (if (null? link-flags) '() (list (cons :opts link-flags))))
        ))))

(define (-exec-modules-fld->mibl stanza-alist)
  (format #t "~A: ~A\n" (blue "-exec-modules-fld->mibl") stanza-alist)
  (let* ((modules (assoc 'modules stanza-alist)))
    (format #t "x modules: ~A\n" modules)
    (if modules
        (if (> (length (cdr modules)) 1)
            (begin
              ;; (format #t "normalize-executable INFO: multiple modules for executable ~A\n"                                        pkg-path)
              )
            ;; just like the 'modules' fld of 'library' stanzas
            (if (not
                 (or (equal? privname (cadr modules))
                     (equal? (normalize-module-name privname)
                             (normalize-module-name (cadr modules)))))
                ;; error?
                (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
                        privname
                        (normalize-module-name (cadr modules)))))
        ;; else no 'modules' field
        )
    ))

(define (-map-link-flds->mibl stanza-alist)
  (map (lambda (fld-assoc)
         (format #t "link fld-assoc: ~A\n" fld-assoc)
         ;; (if (equal? pubname 'rpc_openapi)
         ;;     (format #t "rpc_openapi pubname: ~A\n"
         ;;                fld-assoc))
         (case (car fld-assoc)

           ;; link flds (executable)
           ((name) `(:main ,(normalize-module-name (cadr fld-assoc))))
           ((public_name) (values)) ;; `(:pubname ,(cadr fld-assoc)))
           ((package) ;;
            ;; (package <package>) if there is a
            ;; (public_name ...) field, this specifies
            ;; the package the executables are part of
            ;; interp: used for namespacing if this
            ;; executable is referenced by a `%{...}`
            ;; varexpr.
            `(:package ,(cadr fld-assoc)))
           ((forbidden_libraries) (error 'fixme "forbidden_libraries"))
           ((link_flags) (values)) ;; (cons :link-flags (cdr fld-assoc)))
           ((link_deps) (cons :link-deps (cdr fld-assoc)))
           ((optional) (error 'fixme "forbidden_libraries"))

           ;; cc fields
           ((ctypes) (error 'fixme "ctypes"))
           ((foreign_archives) (error 'fixme "foreign_archives"))
           ((foreign_stubs) (error 'fixme "foreign_stubs"))
           ;; (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

           ;; compile fields (library)
           ;; FIXME: handle separately, since it may be omitted
           ((modules) (values))
           ;; ((modules) (get-manifest pkg
           ;;                          ;; #t ;; deps
           ;;                          #f ;; wrapped?
           ;;                          stanza-alist))

           ;; ignore the rest
           ((libraries) (values))
           ((flags) (values))
           ((ocamlc_flags) (values))
           ((ocamlopt_flags) (values))

           ((root_module) (values))
           ((preprocess) (error 'fixme "unhandled: preprocess"))
           ((preprocess_deps) (error 'fixme "unhandled: preprocess_deps"))

           (else (error 'fixme fld-assoc))))
       stanza-alist))

(define (-map-compile-flds->mibl stanza-alist)
  ;; compile manifest: includes link manifest, plus whatever
  ;; source files are in :deps
  (map (lambda (fld-assoc)
         (format #t "compile fld-assoc: ~A\n" fld-assoc)
         (case (car fld-assoc)

           ;; compile fields (library)
           ((modules) (values))   ;; handled separately
           ((libraries) (values)) ;; handled separately
           ((flags) (values))      ;; handled separately
           ((ocamlc_flags) (values)) ;; handled separately
           ((ocamlopt_flags) (values)) ;; handled separately

           ((root_module) (error 'fixme "root_module"))
           ((preprocess) (error 'fixme "preprocess"))
           ((preprocess_deps) (error 'fixme "preprocess_deps"))

           ;; ignore the rest
           (else (values))))
       stanza-alist))

;; returns (values <compile-flds> <link-flds>)
(define (-exec-flds->mibl pkg stanza-alist)
  (format #t "~A: ~A\n" (ublue "-exec-flds->mibl") stanza-alist)

  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-library-deps-fld->mibl libdeps pkg)
                       '()))
         (_ (format #t "exec MIBLDEPS: ~A\n" deps))
         ;; (_ (error 'tmp "tmp"))

         ;; FIXME: deal with private_modules too
         (link-manifest (get-manifest pkg #f stanza-alist))
         (_ (format #t "~A: ~A\n" (red "link-manifest") link-manifest))

         ;; copy the modules part so we can later set-cdr! on just :compile
         (compile-manifest (let ((modules (assoc-val :modules
                                                     (cdr link-manifest))))
                             (list :manifest (cons :modules
                                                   (copy modules)))))
         (_ (format #t "~A: ~A\n" (red "compile-manifest") compile-manifest))

         (link-flds (-map-link-flds->mibl stanza-alist))
         (compile-flds (-map-compile-flds->mibl stanza-alist)))

    (format #t "~A: ~A\n" (red "compile-flds") compile-flds)

    ;; now handle manifest (modules fld) and submodules (deps fld)
    (format #t "~A: ~A\n" (red "x DEPS") deps)
    (let* ((depslist
            (if deps (remove '()
                             (list :deps
                                   (if-let ((fixed (assoc :fixed deps)))
                                           fixed '())
                                   (if-let ((conds (assoc :conditionals deps)))
                                           conds '())
                                   (if-let ((seldeps (assoc :seldeps deps)))
                                           seldeps '())))
                '())))
      (format #t "depslist: ~A\n" depslist)

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
      ;; )
      ;; (format #t "SUBMODS:: ~A\n" submods)
      ;; (format #t "SUBSIGS:: ~A\n" subsigs)
      ;; (append lib-flds (remove '() (list depslist
      ;;                                    manifest
      ;;                                    ;;submods
      ;;                                    ;;subsigs
      ;;                                    ))))

      ;; (compile-manifest (assoc :manifest link-flds)))

      ;;   (format #t "compile-manifest: ~A\n" compile-manifest)
      ;;   (format #t "compile-flds: ~A\n" compile-flds)
      ;;   (format #t "link-flds: ~A\n" link-flds)
      (values (cons :compile
                    (remove '(:deps)
                            (remove '()
                            (list compile-manifest compile-flds depslist))))
              ;; (remove '() (list compile-flds compile-manifest)))
              (cons :link (remove '() (append (list link-manifest)
                                              link-flds)))))))

;; (let-values (((compile-modules link-modules)
;;               ())))
;; )

;; (define (-compile-manifest compile-flds link-manifest deps pkg-modules)
;;   (format #t "~A: ~A, ~A~%"(blue "compile-manifest") compile-flds deps)
;;   (format #t "pkg-modules: ~A~%" pkg-modules)
;;   (format #t "link manifest: ~A~%" link-manifest)

;;   (if (null? compile-flds)
;;       (begin
;;         (format #t "NULL: ~A~%" compile-flds)
;;         (let ((ctargets (fold (lambda (conditional accum)
;;                               (format #t
;;                                       "conditional ~A\n" conditional)
;;                               ;; (set! pkg (-update-pkg-files! pkg
;;                               ;;                     conditional))
;;                               (append (assoc-val :target conditional)
;;                                       accum))
;;                             '()
;;                             (assoc-val :conditionals deps))))
;;         (format #t "CTARGETS: ~A~%" ctargets)
;;         (let* ((cmodules (map filename->module-name ctargets)))
;;           (format #t "CMODULES: ~A~%" cmodules)
;;           (cons :manifest (list
;;                            (cons :modules
;;                                 (append (cdar link-manifest) cmodules)))))))
;;       ;; else
;;       (let ((manifest (assoc-val :manifest compile-flds))
;;             (ctargets (fold (lambda (conditional accum)
;;                               (format #t
;;                                       "conditional ~A\n" conditional)
;;                               ;; (set! pkg (-update-pkg-files! pkg
;;                               ;;                     conditional))
;;                               (append (assoc-val :target conditional)
;;                                       accum))
;;                             '()
;;                             (assoc-val :conditionals deps))))
;;         (format #t "CTARGETS: ~A~%" ctargets)
;;         ;; now find the conditional targets in the pkg :modules list
;;         ;; (they should have already been added)
;;         ;; and add the found module to the manifest
;;         ;; easier: just ctarget -> module name
;;         (let* ((cmodules (map filename->module-name ctargets))
;;                (manimods (if manifest (car manifest)))
;;                (manisigs (if manifest (cadr manifest))))
;;           (format #t "CMODULES: ~A~%" cmodules)
;;           (format #t "MANIMODS: ~A~%" manimods)
;;           (format #t "MANISIGS: ~A~%" manisigs)
;;           (cons :manifest (remove '()
;;                                   (list
;;                                    (append manimods cmodules)
;;                                    (if (null? manisigs) '() manisigs)
;;                                    )))))))

(define (-executable->mibl kind ;; :executable || :test
                           pkg
                           privname pubname
                           stanza-alist) ;; srcfiles)
  (format #t "~A: ~A, ~A\n" (blue "-executable->mibl") privname pubname)
  (format #t "~A: ~A~%" (blue "kind") kind)
  ;; (if pubname
  ;;     (if (equal? pubname 'tezos-node)
  ;;         (format #t "stanza-alist: ~A\n" stanza-alist)))
  ;; NB: stanza-alist excludes 'names' and 'public_names'
  ;; if has modules list, one must match 'name'
  ;; WARNING: updating lookup tables now in separate phase
  ;; (update-public-exe-table pkg-path pubname pubname)
  ;; (update-public-exe-table pkg-path privname pubname)

  ;; 'modules' field must be handled separately since it may be
  ;; omitted, defaulting to (modules :standard)
  ;; ditto for 'flags', which must be split into link flags and
  ;; compile flags.
  (let-values (((compile-flags link-flags)
                (-exec-flags->mibl stanza-alist))

               ((compile-flds link-flds)
                (-exec-flds->mibl pkg stanza-alist)))

    (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
    (format #t "~A: ~A\n" (uyellow "compile-flds") compile-flds)

    (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)
    (format #t "~A: ~A\n" (uyellow "link-flds") link-flds)

    ;; (format #t "manifest: ~A\n" manifest)

    ;; (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
    ;;                      (dune-library-deps-fld->mibl libdeps pkg);;RENAME
    ;;                      '()))
    ;;        (_ (format #t "x MIBLDEPS: ~A\n" deps))
    ;;        ;; (compile-manifest (-compile-manifest
    ;;        ;;                    (cdr compile-flds)
    ;;        ;;                    (assoc-val :manifest (cdr link-flds))
    ;;        ;;                    deps (assoc-val :modules pkg)))
    ;;        )

    ;; (format #t "compile-manifest: ~A\n" compile-manifest)
    (list kind
          (cons :pubname pubname)
          (cons :privname privname)
          (append link-flds link-flags)
          (append compile-flds compile-flags))
    ;; (list compile-manifest
    ;; (cons :deps deps)))
    ))

;;FIXME: it's not clear how "exe:..." and "bin:..." work, so we
;;register both, with and without sfx ".exe"
;; (update-executables-table
;;  (string->symbol
;;   (string-append "bin:" (symbol->string pubname)))
;;  (string->symbol (string-append "//" pkg-path
;;                                 ":" (symbol->string pubname)
;;                                 ".exe")))
;; (update-executables-table
;;  (string->symbol
;;   (string-append "bin:" (symbol->string pubname) ".exe"))
;;  (string->symbol (string-append "//" pkg-path
;;                                 ":" (symbol->string pubname)
;;                                 ".exe")))
;; (update-executables-table
;;  (string->symbol
;;   (string-append "exe:" (symbol->string pubname)))
;;  (string->symbol (string-append "//" pkg-path
;;                                 ":" (symbol->string pubname)
;;                                 ".exe")))
;; (update-executables-table
;;  (string->symbol
;;   (string-append "exe:" (symbol->string pubname) ".exe"))
;;  (string->symbol (string-append "//" pkg-path
;;                                 ":" (symbol->string pubname)
;;                                 ".exe")))
;;     (if (equal? pubname 'tezos-node)
;;         (format #t "tezos-node stanza: ~A\n" s)))

;; WARNING: we dropped modules-ht
;; (if-let ((mods (assoc :modules flds-alist)))
;;         (list kind ;; :executable
;;               (concatenate
;;                `((:name ((:private ,privname)
;;                          (:public ,pubname))))
;;                flds-alist))

;;         ;; else 'modules' fld is missing, so add all modules
;;         ;; and don't forget to remove 'main' from the list

;;         (let* ((modules-ht (srcfiles->modstbl srcfiles)))
;;           (hash-table-set! modules-ht
;;                            (normalize-module-name privname)
;;                            :main)
;;           ;; (format #t "exec modules-ht: ~A:: ~A\n"
;;           ;;         pkg-path modules-ht)
;;           (list kind ;; :executable
;;                 (concatenate
;;                  `((:name ((:private ,privname)
;;                            (:public ,pubname))))
;;                  `((:modules ,modules-ht
;;                     ;; ((:main ,privname)
;;                     ;;  (:direct
;;                     ;;   ,@(sort!
;;                     ;;      modules
;;                     ;;      sym<?)))
;;                     ))
;;                  flds-alist))))
;;   ) ;; let flds-alist
;; ))))

;; (define (normalize-stanza-executable kind pkg-path srcfiles stanza)

(define (is-test-executable? ws pkg stanza)
  (format #t "~A: ~A~%" (blue "is-test-executable?") stanza)
  (let ((libdeps (assoc :libraries (cdr stanza))))
    #t))

;; this calls -executable-mibl
(define (dune-executable->mibl ws pkg kind stanza)
  ;; kind:: :executable || test
  ;; (let ((privname (cadr (assoc 'name (cdr stanza)))))
  (format #t "~A: ~A\n" (blue "dune-executable->mibl") pkg)
  (format #t "~A: ~A~%" (blue "kind") kind)

  ;; "<name> is a module name that contains the main entry point of
  ;; the executable." So 'name' must correspond to a .ml file. If
  ;; 'modules' is present, then is must include (explicitly or
  ;; implicitly) the 'name' module. OBazl will pass this 'name' module
  ;; in the 'main' attribute.

  ;; "There can be additional modules in the current directory, you
  ;; only need to specify the entry point."

  ;; Yet OCaml has no concept of "main entry point", so there is no
  ;; way to single out one module among several as 'main'. Top-level
  ;; code will simply be executed in link order. An intended main
  ;; entry point could be preceded by modules that do some kind of
  ;; initialization (e.g. read a file or socket and create a data
  ;; structure that main uses), and/or followed by modules that do
  ;; some kind of post-processing. Dune does not seem to accomodate
  ;; such structuring; presumably "entry point" means "last in link
  ;; order.  (Which is how OBazl treats the 'main' attribute.)

  ;; The modules are there to be linked into the executable; it does
  ;; not follow that there are any inter-deps among them. In
  ;; particular, they need not be deps of the main module.

  ;; The 'libraries' and 'flags' fields apply to all modules.

  ;; So to normalize an 'executable' stanza: expand the 'modules'
  ;; and 'libraries' fields; normalize 'name' and 'flags'.

  (let* ((stanza-alist (cdr stanza))
         (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                               stanza-alist
                               (append stanza-alist
                                       (list '(modules :standard)))))
         (pkg-path (car (assoc-val :pkg-path pkg)))
         ;; 'name' fld is required
         (privname (cadr (assoc 'name stanza-alist)))
         (pubname (if-let ((pubname (assoc 'public_name stanza-alist)))
                          (cadr pubname) privname))
         (modules (assoc 'modules stanza-alist))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist)))
    (format #t " N: ~A\n" privname)
    (format #t " Ms: ~A\n" modules)

    (if pubname
        (update-exports-table! ws
                               (if (is-test-executable? ws pkg stanza)
                                   :test :bin)
                               pubname pkg-path))

    ;; if has modules list, one must match 'name'
    (if modules
        (if (> (length (cdr modules)) 1)
            (begin
              ;; (format #t "normalize-stanza-executable WARNING: multiple modules for executable ~A\n"
              ;;         pkg-path)
              )
            ;; just like the 'modules' fld of 'library' stanzas
            (if (not
                 (or (equal? privname (cadr modules))
                     (equal? (normalize-module-name privname)
                             (normalize-module-name (cadr modules)))))
                ;; error?
                (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
                        privname
                        (normalize-module-name (cadr modules))))))

    (list (-executable->mibl kind
                             pkg privname pubname
                             filtered-stanza-alist))
    ))
;; (list :executable  ;; (car stanza)
;;       (remove '()
;;       (map (lambda (fld-assoc)
;;              ;; (format #t "exec fld-assoc: ~A\n" fld-assoc)
;;              ;; (let ((fld (if (pair? fld-assoc)
;;              ;;                fld-assoc
;;              ;;                (list fld-assoc))))
;;              (case (car fld-assoc)
;;                ((name)
;;                 (format #t "exec privname: ~A\n" (cadr privname))
;;                 ;;(let ((result
;;                 (let ((pubname (assoc 'public_name stanza-alist)))
;;                   (if pubname
;;                       (begin
;;                         (update-public-exe-table pkg-path
;;                                                  (cadr pubname)
;;                                                  (cadr pubname))
;;                         (update-public-exe-table pkg-path
;;                                                  (cadr privname)
;;                                                  (cadr pubname))
;;                         `(:name ((:private ,(cadr privname))
;;                                  (:public ,(cadr pubname)))))
;;                       (begin
;;                         (update-public-exe-table pkg-path (cadr privname)
;;                                                  (cadr privname))
;;                         `(:name ((:private ,(cadr privname))))))))
;;                   ;;     ))
;;                   ;; (format #t "XXXX ~A\n" result)
;;                   ;; result)
;;                 ;; )

;;                ((public_name) '())
;;                ((libraries) ;; => :deps
;;                 (normalize-exe-libraries fld-assoc stanza-alist))
;;                ((modules)  ;; => :modules
;;                 (let-values (((direct indirect)
;;                               (expand-modules-fld modules srcfiles)))
;;                   ;; (format #t
;;                   ;;         "    direct: ~A\n    indirect ~A\n"
;;                   ;;         direct indirect)
;;                   (let* ((raw `((:raw ,fld-assoc)))
;;                          (direct (if (null? direct)
;;                                      raw
;;                                      (cons `(:direct ,@direct) raw)))
;;                          (indirect (if (null? indirect)
;;                                        direct
;;                                        (cons `(:indirect
;;                                                ,@(reverse indirect))
;;                                              direct)))
;;                          (result `(:modules ,indirect)))
;;                     ;;(format #t "RESULT ~A: ~A\n" stanza-name result)
;;                     result)))
;;                ((flags) (normalize-stanza-fld-flags fld-assoc))
;;                ((foreign_stubs)
;;                 (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

;;                (else fld-assoc)))
;;            stanza-alist)))
;; ))

;; ((executables ((names test_clic) (libraries tezos-clic alcotest-lwt) (flags (:standard -open Tezos_stdlib -open Tezos_clic)))) (rule ((alias buildtest) (deps test_clic.exe) (action (progn)))) (rule ((alias runtest_clic) (action (run %{exe:test_clic.exe})))) (rule ((alias runtest) (package tezos-clic) (deps (alias runtest_clic)) (action (progn)))))

;; EXES normalized: (:executable ((:name ((:private main_snoop) (:module Main_snoop))) (public_names tezos-snoop) (package tezos-snoop) (libraries tezos-base tezos-base.unix tezos-stdlib-unix tezos-clic tezos-benchmark tezos-benchmark-examples tezos-shell-benchmarks tezos-benchmarks-proto-alpha str ocamlgraph pyml pyml-plot latex) (:opts ((:standard) (:opens ("Tezos_benchmark" "Tezos_stdlib_unix" "Tezos_base")) (:raw (:standard -open Tezos_base__TzPervasives -open Tezos_stdlib_unix -open Tezos_benchmark -linkall)) (:flags -open Tezos_base__TzPervasives -open Tezos_stdlib_unix -open Tezos_benchmark -linkall)))))
;; dune-executables->mibl: src/lib_store/test

;; "The optional fields [for 'tests stanza] that are supported are a
;; subset of the alias and executables fields. In particular, all
;; fields except for public_names are supported from the executables

;; stanza. Alias fields apart from name are allowed."
(define (dune-executables->mibl kind pkg-path srcfiles stanza)
  ;; kind:: :executable || :test
  (format #t "~A: ~A\n" (blue "dune-executables->mibl") pkg-path)
  (format #t "  kind:  ~A\n" kind)
  ;; (format #t "  stanza:  ~A\n" stanza)
  ;; (:executables (names test_clic) ...
  (let* ((stanza-alist (cdr stanza))
         (privnames (assoc 'names stanza-alist))
         (pubnames (if (equal? kind :executable)
                       (assoc 'public_names stanza-alist)
                       #f))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist))
         )
    ;; (format #t "stanza-alist: ~A\n" stanza-alist)
    ;; (format #t "filtered-stanza-alist: ~A\n" filtered-stanza-alist)
    (if (equal? kind :executable)
        (if (> (length (cdr privnames)) 1)
            (begin
              ;; (format #t "MULTIPLE NAMES\n")
              (if pubnames
                  (if (not (equal?
                            (length (cdr privnames))
                            (length (cdr pubnames))))
                      (error
                       'bad-arg
                       "names and public_names differ in length"))))))

    ;; we add the pub/priv names to the lookup table, so that the
    ;; emitter can resolve references to them.
    ;; dune emits *.exe and *.bc, which may be referred to
    ;; in dunefiles, so we add them to the lookup table too.

    (if pubnames
        (map (lambda (privname pubname)
               ;; (format #t "privname ~A, pubname: ~A\n" privname pubname)
               ;; (update-public-exe-table pkg-path pubname privname)
               ;; (update-public-exe-table pkg-path privname privname)
               (let ((x (-executable->mibl :executable
                                           pkg-path privname pubname
                                           filtered-stanza-alist srcfiles)))
                 (format #t "~A: ~A~%" (red "XXXXXXXXXXXXXXXX") x)
                 (prune-mibl-rule x)
               ))
             (cdr privnames) (cdr pubnames))

        ;; else no pubnames
        (map (lambda (privname)
               ;; (format #t "privname: ~A\n" privname)
               ;; (update-public-exe-table pkg-path privname privname)
               (let ((x (-executable->mibl kind
                                           ;; (if (equal? kind :executable)
                                           ;;     :executable
                                           ;;     (if (equal? kind :test)
                                           ;;         :test
                                           ;;         #f))
                                           pkg-path
                                           privname privname
                                           filtered-stanza-alist srcfiles)))
               ;; `(:executable ((:name ((:private ,privname)))))
               ;; (:module ,(normalize-module-name name))))))
               (prune-mibl-rule x)
               ))
             (cdr privnames)))))
