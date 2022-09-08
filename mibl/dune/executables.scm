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
      ;; (format #t "~A: ~A~%" (red "standard") standard)
      ;; (format #t "~A: ~A~%" (red "opens")    opens)
      ;; (format #t "~A: ~A~%" (red "options")  options)
      ;; (format #t "~A: ~A~%" (red "flags")    flags)
      ;; (format #t "~A: ~A~%" (red "ocamlc_flags") ocamlc_flags)
      ;; (format #t "~A: ~A~%" (red "ocamlopt_flags") ocamlopt_flags)
      ;; (format #t "~A: ~A~%" (red "link_flags") link-flags)

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
  (format #t "~A: ~A~%" (ublue "-map-link-flds->mibl") stanza-alist)
  (map (lambda (fld-assoc)
         (format #t "link fld-assoc: ~A\n" fld-assoc)
         ;; (if (equal? pubname 'rpc_openapi)
         ;;     (format #t "rpc_openapi pubname: ~A\n"
         ;;                fld-assoc))
         (case (car fld-assoc)

           ;; link flds (executable)
           ((name) (values)) ;; `(:main ,(normalize-module-name (cadr fld-assoc))))
           ((names) (values))
           ((public_name) (values))
           ((public_names) (values))
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

           ((preprocess) (values))
            ;; (error 'fixme
            ;;        (format #f "unhandled link fld: ~A" fld-assoc)))

            ((preprocess_deps) (values))
             ;; (error 'fixme
             ;;        (format #f "unhandled link fld: ~A" fld-assoc)))

           ((modes) (values)) ;; FIXME?

           (else (error 'fixme
                        (format #f "unhandled link fld: ~A" fld-assoc)))))
       stanza-alist))

(define (-map-compile-flds->mibl stanza-alist)
  (format #t "~A: ~A~%" (ublue "-map-compile-flds->mibl") stanza-alist)
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

           ((root_module)
            (error 'fixme
                   (format #f "unhandled compile fld: ~A" fld-assoc)))

           ((preprocess) (values))
            ;; (error 'fixme
            ;;        (format #f "unhandled compile fld: ~A" fld-assoc)))

           ((preprocess_deps) (values))
            ;; (error 'fixme
            ;;        (format #f "unhandled compile fld: ~A" fld-assoc)))

           ;; ignore the rest
           (else (values))))
       stanza-alist))

;; returns (values <compile-flds> <link-flds>)
(define (-exec-flds->mibl pkg privnames stanza-alist)
  (format #t "~A: ~A\n" (ublue "-exec-flds->mibl") stanza-alist)
  (format #t "~A: ~A~%" (blue "privnames") privnames)
  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  (let* ((mains (map normalize-module-name privnames))
         (deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-libraries-fld->mibl libdeps pkg)
                       '()))
         (_ (format #t "~A: ~A\n" (uwhite "exec MIBLDEPS") deps))
         ;; (_ (error 'tmp "tmp"))

         ;; FIXME: deal with private_modules too
         (pkg-manifest (get-manifest pkg #f stanza-alist))
         (_ (format #t "~A: ~A\n" (red "pkg-manifest") pkg-manifest))
         ;; pkg-manifest: (:manifest (:modules M1 M2 ...))

         (modules (let ((modules (assoc-val :modules (cdr pkg-manifest))))
                    modules))
                    ;; (filter (lambda (m) (not (member m mains))) modules)))
         (_ (format #t "~A: ~A\n" (red "modules") modules))

         (compile-manifest (list :manifest (cons :modules (copy modules))))
         (_ (format #t "~A: ~A\n" (red "compile-manifest") compile-manifest))

         (link-manifest (list :manifest (cons :modules (copy modules))))
         (_ (format #t "~A: ~A\n" (red "link-manifest") link-manifest))

         ;; (_ (format #t "~A: ~A\n" (red "XXXXXXXXXXXXXXXX") (eq? (assoc-val :modules (cdr link-manifest))
         ;;                                                        (assoc-val :modules (cdr compile-manifest)))))

         (link-flds (-map-link-flds->mibl stanza-alist))
         (_ (format #t "~A: ~A~%" (ured "link-flds") link-flds))
         (compile-flds (-map-compile-flds->mibl stanza-alist))
         (_ (format #t "~A: ~A\n" (red "compile-flds") compile-flds))
         )

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

;; (define (-executable->mibl kind ;; :executable || :test
;;                            pkg
;;                            privname pubname
;;                            stanza-alist) ;; srcfiles)
;;   (format #t "~A: ~A, ~A\n" (bgcyan "-executable->mibl") privname pubname)
;;   (format #t "~A: ~A~%" (blue "kind") kind)

;;   ;; NB wrt pubnames: "Moreover, you can use - for executables that
;;   ;; shouldn’t be installed."

;;   ;; NB: stanza-alist excludes 'names' and 'public_names'
;;   ;; if has modules list, one must match 'name'
;;   ;; WARNING: updating lookup tables now in separate phase
;;   ;; (update-public-exe-table pkg-path pubname pubname)
;;   ;; (update-public-exe-table pkg-path privname pubname)

;;   ;; 'modules' field must be handled separately since it may be
;;   ;; omitted, defaulting to (modules :standard)
;;   ;; ditto for 'flags', which must be split into link flags and
;;   ;; compile flags.

;;   (let-values (((compile-flags link-flags)
;;                 (-exec-flags->mibl stanza-alist)))
;;     (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
;;     (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)

;;     (let-values (((compile-flds link-flds)
;;                   (-exec-flds->mibl pkg stanza-alist)))

;;     (format #t "~A: ~A\n" (uyellow "compile-flds") compile-flds)
;;     (format #t "~A: ~A\n" (uyellow "link-flds") link-flds)

;;     ;; (format #t "manifest: ~A\n" manifest)

;;     ;; (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
;;     ;;                      (dune-libraries-fld->mibl libdeps pkg);;RENAME
;;     ;;                      '()))
;;     ;;        (_ (format #t "x MIBLDEPS: ~A\n" deps))
;;     ;;        ;; (compile-manifest (-compile-manifest
;;     ;;        ;;                    (cdr compile-flds)
;;     ;;        ;;                    (assoc-val :manifest (cdr link-flds))
;;     ;;        ;;                    deps (assoc-val :modules pkg)))
;;     ;;        )

;;     ;; (format #t "compile-manifest: ~A\n" compile-manifest)
;;     (list kind
;;           (cons :pubname pubname)
;;           (cons :privname privname)
;;           (append link-flds link-flags)
;;           (append compile-flds compile-flags))
;;     ;; (list compile-manifest
;;     ;; (cons :deps deps)))
;;     )))

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

(define unit-test-pkgs
  '(
    alcotest @alcotest//:alcotest
    ounit @ounit//:ounit
    ounit2 @ounit2//:ounit2
    )
  )

(define (-is-test-executable? ws pkg stanza)
  (format #t "~A: ~A~%" (ublue "-is-test-executable?") stanza)
  (case (car stanza)
    ((executable executables) #f)
    ((test tests) #t)
    (else
     (if (equal? (car stanza) 'tests)
         (let* ((stanza-alist (cdr stanza))
                (libdeps (assoc 'libraries stanza-alist))
                ;; (compile-deps (assoc-in '(:compile :deps :resolved) stanza-alist))
                )
           (format #t "~A: ~A~%" (uwhite "libdeps") libdeps)
           (let ((test? (find-if (lambda (dep)
                                   (member dep unit-test-pkgs))
                                 libdeps)))
             (format #t "~A: ~A~%" (blue "answer") test?)
             test?))))))

;; this calls -executable-mibl
(define (dune-executable->mibl ws pkg kind stanza)
  ;; kind:: :executable || test
  ;; (let ((privname (cadr (assoc 'name (cdr stanza)))))
  (format #t "~A: ~A\n" (blue "dune-executable->mibl") stanza)
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
         (_ (format #t "~A: ~A~%" (uwhite "stanza-alist") stanza-alist))
         (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                               stanza-alist
                               (append stanza-alist
                                       (list '(modules :standard)))))
         (pkg-path (car (assoc-val :pkg-path pkg)))
         ;; 'name' fld is required
         (privname (cadr (assoc 'name stanza-alist)))
         (pubname (if-let ((pubname (assoc 'public_name stanza-alist)))
                          (cadr pubname) #f))
         (package (if-let ((p (assoc-val 'package stanza-alist)))
                          (car p) #f))
         (_ (format #t "~A: ~A~%" (uwhite "package") package))

         (modules (assoc 'modules stanza-alist))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist)))
    (format #t "~A: ~A\n" (uwhite "privname") privname)
    (format #t "~A: ~A\n" (uwhite "pubname") pubname)
    (format #t " Ms: ~A\n" modules)

    (if pubname
        (begin
          (update-exports-table! ws
                                 (if (-is-test-executable? ws pkg stanza) :test :exe)
                                 ;; (string->symbol (format #f ":test:~A" pubname))
                                 ;; (string->symbol (format #f ":bin:~A" pubname)))
                                 privname
                                 pkg-path privname)
          (if (not (-is-test-executable? ws pkg stanza))
              (if package
                  (update-opam-table! ws :bin
                                      package
                                      pubname
                                      pkg-path
                                      privname ;; lib name
                                      )))))
    (if (and privname
             (not (equal? privname pubname)))
        (begin
          (update-exports-table! ws
                                 (if (-is-test-executable? ws pkg stanza) :test :exe)
                                 ;; (string->symbol (format #f ":test:~A" privname))
                                 ;; (string->symbol (format #f ":bin:~A" privname)))
                                 pubname
                                 pkg-path privname)
          (if (not (-is-test-executable? ws pkg stanza))
              (if package
                  (update-opam-table! ws :bin
                                      package
                                      pubname
                                      pkg-path
                                      privname ;; lib name
                                      )))))

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

    (let-values (((compile-flags link-flags)
                  (-exec-flags->mibl stanza-alist)))
      (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
      (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)

      (let-values (((compile-flds link-flds)
                    (-exec-flds->mibl pkg (list privname) stanza-alist)))

        (format #t "~A: ~A\n" (uyellow "x compile-flds") compile-flds)
        (format #t "~A: ~A\n" (uyellow "x link-flds") link-flds)

        ;; `(:main ,(normalize-module-name (cadr fld-assoc))))
        ;; (list (-executable->mibl kind
        ;;                          pkg privname pubname
        ;;                          filtered-stanza-alist))
        (list (cons kind
                    (list (cons :pubname pubname)
                          (cons :privname privname)
                          (cons :main (normalize-module-name privname))
                          (append link-flds link-flags)
                          (append compile-flds compile-flags))))
              ))))

;; "The optional fields [for 'tests stanza] that are supported are a
;; subset of the alias and executables fields. In particular, all
;; fields except for public_names are supported from the executables
;; stanza. Alias fields apart from name are allowed."
(define (dune-executables->mibl ws pkg kind stanza)
  ;; kind:: :executable || :test
  (format #t "~A: ~A\n" (bgblue "dune-executables->mibl") stanza)
  (format #t "~A:  ~A\n" (uwhite "kind") kind)
  (format #t "~A:  ~A\n" (uwhite "pkg") stanza)
  ;; (:executables (names test_clic) ...
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (stanza-alist (cdr stanza))
         (privnames (if (case kind ((:executable :test) #t) (else #f))
                        (if-let ((pubnames
                                 (assoc-val 'names stanza-alist)))
                               pubnames '())
                       '()))
         (pubnames (if (case kind ((:executable :test) #t) (else #f)) ;; (equal? kind :executable)
                       (if-let ((pubnames
                                 (assoc-val 'public_names stanza-alist)))
                               pubnames '())
                       '()))
         (privpubmodules (map normalize-module-name
                        (flatten (concatenate privnames pubnames))))
         (_ (format #t "~A: ~A~%" (yellow "privpubmodules") privpubmodules))
         (filtered-stanza-alist stanza-alist)
         ;; (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist))
         (text-exe? (-is-test-executable? ws pkg stanza))
         (testsuite-stanza (if text-exe? (list (cons :testsuite
                                                     (list (cons :name 'testsuite)
                                                           (cons :tests privnames))))
                               #f))
         )
    (format #t "~A: ~A~%" (uwhite "exec privnames") privnames)
    (format #t "~A: ~A~%" (uwhite "exec pubnames") pubnames)
    ;;(error 'fixme "STOP execs")

    ;; flags and (libraries) etc. apply to each of the executables
    ;; each name in (names) is one module;
    ;; omitted (modules) means none
    (let ((test-exe? (-is-test-executable? ws pkg stanza))
          (package (assoc-val 'package (cdr stanza))))

      (let-values (((compile-flags link-flags)
                    (-exec-flags->mibl stanza-alist)))
        (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
        (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)

        (let-values (((compile-flds link-flds)
                      (-exec-flds->mibl pkg privnames stanza-alist)))

          (format #t "~A: ~A\n" (uyellow "xs compile-flds") compile-flds)
          (format #t "~A: ~A\n" (uyellow "xs link-flds") link-flds)

          ;; (error 'fixme "STOP execs")

          (format #t "~A: ~A~%" (uwhite "privnames") privnames)
          (format #t "~A: ~A~%" (uwhite "pubnames") pubnames)
          ;; (format #t "filtered-stanza-alist: ~A\n" filtered-stanza-alist)
          ;; (if (equal? kind :executable)

          ;; FIXME: don't need to do this, we assume the dunefile is good?
          (if (> (length (cdr privnames)) 1)
              (begin
                ;; (format #t "MULTIPLE NAMES\n")
                (if (not (null? pubnames))
                    (if (not (equal?
                              (length (cdr privnames))
                              (length (cdr pubnames))))
                        (error
                         'bad-arg
                         "names and public_names differ in length"))
                    ))) ;;)

          ;; for executables, we add the pub/priv names to the lookup table, so that the
          ;; emitter can resolve references to them.
          ;; dune emits *.exe and *.bc, which may be referred to
          ;; in dunefiles, so we add them to the lookup table too.

          (cond
           ((and (not (null? pubnames))
                 (not (null? privnames)))
            (format #t "~A: ~A, ~A~%" (bgblue "iter priv/pubnames") pubnames privnames)
            (map (lambda (privname pubname)
                   (format #t "~A: ~A, ~A~%" (uyellow "emitting exec") privname pubname)
                   (let ((privmodule (normalize-module-name privname)))

                     (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
                         (begin
                           (update-exports-table! ws :exe pubname
                                                  pkg-path privname)
                           (update-exports-table! ws :exe privname
                                                  pkg-path privname)
                           (update-opam-table! ws :bin
                                               package
                                               pubname
                                               pkg-path
                                               privname ;; lib name
                                               )))

                     ;; NB: to avoid structure sharing we need to copy toplevel :manifest subtree
                     (let* (;; (link-flds (copy link-flds))
                            (_ (format #t "~A: ~A~%" (uyellow "link-flds") link-flds))
                            (link-manifest (assoc :manifest (cdr link-flds)))
                            (_ (format #t "~A: ~A~%" (uyellow "link-manifest") link-manifest))
                            (link-modules (assoc-in '(:manifest :modules) (cdr link-flds)))
                            (_ (format #t "~A: ~A~%" (uyellow "link-modules") (cdr link-modules)))
                            (_ (format #t "~A: ~A~%" (uyellow "privmodule") privmodule))
                            ;; (lmodules (filter (lambda (x) (member x privpubmodules)) (cdr link-modules)))
                            (lmodules (list privmodule))
                            (_ (set-cdr! link-flds `((:manifest (:modules ,@(copy lmodules))))))
                            ;; (_ (alist-update-in! (cdr link-flds) '(:manifest)
                            ;;                      (lambda (old) (append (list `(:main . ,privname)) old))))
                            (_ (format #t "~A: ~A~%" (uyellow "link-flds filtered") link-flds))

                            (-compile-flds (copy compile-flds))
                            (_ (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds))
                            (compile-manifest (assoc :manifest (cdr -compile-flds)))
                            (_ (format #t "~A: ~A~%" (uyellow "compile-manifest") compile-manifest))
                            (compile-deps (assoc :deps (cdr -compile-flds)))
                            (_ (format #t "~A: ~A~%" (uyellow "compile-deps") compile-deps))
                            (compile-modules (copy (assoc-in '(:manifest :modules) (cdr -compile-flds))))
                            (cmodules (filter (lambda (x) (not (member x privpubmodules))) (cdr compile-modules)))
                            (_ (set-cdr! -compile-flds `((:manifest (:modules ,@(cons privmodule cmodules)))
                                                         ,compile-deps)))
                            ;; (_ (alist-update-in! (cdr -compile-flds) '(:manifest :modules) (lambda (old) (cons privmodule cmodules))))
                            (_ (format #t "~A: ~A~%" (uyellow "-compile-modules") compile-modules))
                            (_ (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds))
                            (_ (format #t "~A: ~A~%" (uyellow "compile-flds") compile-flds))
                            )
                       (list kind
                             (if test-exe? (cons :in-testsuite 'testsuite) :FOO)
                             (cons :pubname pubname)
                             (cons :privname privname)
                             (cons :main (normalize-module-name privname))
                             (append link-flds link-flags)
                             (append -compile-flds compile-flags)))
                     ;; (let ((x (-executable->mibl kind
                     ;;                             pkg privname pubname
                     ;;                             filtered-stanza-alist)))
                     ;;   (format #t "~A: ~A~%" (red "XXXXXXXXXXXXXXXX") x)
                     ;;   (prune-mibl-rule x)
                     ;;   )
                     ))
                 privnames pubnames))

           ((null? pubnames)
            (format #t "~A: ~A~%" (bgblue "iter privnames") privnames)
            (append
             (if test-exe? testsuite-stanza '())
             (map (lambda (privname)
                    (format #t "~A: ~A\n" (uwhite "privname") privname)
                    (let ((privmodule (normalize-module-name privname)))
                      (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
                          (begin
                            (update-exports-table! ws :exe privname
                                                   pkg-path privname)
                            ;; (update-opam-table! ws :bin
                            ;;                     package
                            ;;                     pubname
                            ;;                     pkg-path
                            ;;                     privname ;; lib name
                            ;;                     )
                            ))

                      (let* (;; (link-flds (copy link-flds))
                             (_ (format #t "~A: ~A~%" (uyellow "link-flds") link-flds))
                             (link-manifest (assoc :manifest (cdr link-flds)))
                             (_ (format #t "~A: ~A~%" (uyellow "link-manifest") link-manifest))
                             (link-modules (assoc-in '(:manifest :modules) (cdr link-flds)))
                             (_ (format #t "~A: ~A~%" (uyellow "link-modules") (cdr link-modules)))
                             (_ (format #t "~A: ~A~%" (uyellow "privmodule") privmodule))
                             ;; (lmodules (filter (lambda (x) (member x privpubmodules)) (cdr link-modules)))
                             (lmodules (list privmodule))
                             (_ (set-cdr! link-flds `((:manifest (:modules ,@(copy lmodules))))))
                             ;; (_ (alist-update-in! (cdr link-flds) '(:manifest)
                             ;;                      (lambda (old) (append (list `(:main . ,privname)) old))))
                             (_ (format #t "~A: ~A~%" (uyellow "link-flds filtered") link-flds))

                             (-compile-flds (copy compile-flds))
                             (_ (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds))
                             (compile-manifest (assoc :manifest (cdr -compile-flds)))
                             (_ (format #t "~A: ~A~%" (uyellow "compile-manifest") compile-manifest))
                             (compile-deps (assoc :deps (cdr -compile-flds)))
                             (_ (format #t "~A: ~A~%" (uyellow "compile-deps") compile-deps))
                             (compile-modules (copy (assoc-in '(:manifest :modules) (cdr -compile-flds))))
                             ;; (cmodules (filter (lambda (x) (not (member x privpubmodules))) (cdr compile-modules)))
                             ;; (_ (set-cdr! -compile-flds `((:manifest (:modules ,@(cons privmodule cmodules)))
                             ;;                              ,compile-deps)))
                             (cmodules (list privmodule))
                             (_ (set-cdr! -compile-flds `((:manifest (:modules ,@(copy cmodules)))
                                                          ,compile-deps)))
                             ;; (_ (alist-update-in! (cdr -compile-flds) '(:manifest :modules) (lambda (old) (cons privmodule cmodules))))
                             (_ (format #t "~A: ~A~%" (uyellow "-compile-modules") compile-modules))
                             (_ (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds))
                             (_ (format #t "~A: ~A~%" (uyellow "compile-flds") compile-flds))
                             )

                        (list kind
                              ;; `,@(if test-exe? (cons :in-testsuite 'testsuite) '())
                              ;; (cons :in-testsuite 'testsuite)
                              (cons :privname privname)
                              (append link-flds link-flags)
                              (append -compile-flds compile-flags))

                        ;; (let ((x (-executable->mibl kind
                        ;;                             ;; (if (equal? kind :executable)
                        ;;                             ;;     :executable
                        ;;                             ;;     (if (equal? kind :test)
                        ;;                             ;;         :test
                        ;;                             ;;         #f))
                        ;;                             pkg-path
                        ;;                             privname privname
                        ;;                             filtered-stanza-alist srcfiles)))
                        ;;   ;; `(:executable ((:name ((:private ,privname)))))
                        ;;   ;; (:module ,(normalize-module-name name))))))
                        ;;   (prune-mibl-rule x)
                        ;;   )
                        )))
                  privnames)))

           ((null? privnames)
            (format #t "~A: ~A~%" (bgblue "iter pubnames") pubnames)
            (map (lambda (pubname)
                   ;; (format #t "privname: ~A\n" privname)
                   (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
                       (begin
                         (update-exports-table! ws :exe pubname
                                                pkg-path pubname)
                         (update-opam-table! ws :bin
                                             ;;FIXME: if package package else pubname
                                             package ;; opam pkg name
                                             pubname ;; lib name
                                             pkg-path
                                             pubname ;; lib name
                                             )))

                   (list kind
                         ;; `,@(if test-exe? (cons :in-testsuite 'testsuite) (values))
                         ;; (if test-exe? (cons :in-testsuite 'testsuite) (values))
                         ;; (cons :in-testsuite 'testsuite)
                         (cons :pubname pubname)
                         ;; (cons :privname privname)
                         (append link-flds link-flags)
                         (append compile-flds compile-flags))

                   ;; (let ((x (-executable->mibl kind
                   ;;                             ;; (if (equal? kind :executable)
                   ;;                             ;;     :executable
                   ;;                             ;;     (if (equal? kind :test)
                   ;;                             ;;         :test
                   ;;                             ;;         #f))
                   ;;                             pkg-path
                   ;;                             privname privname
                   ;;                             filtered-stanza-alist srcfiles)))
                   ;;   ;; `(:executable ((:name ((:private ,privname)))))
                   ;;   ;; (:module ,(normalize-module-name name))))))
                   ;;   (prune-mibl-rule x)
                   ;;   )
                   )
                 pubnames))

           (else
            (error 'fixme "executables missing flds names and public_names"))))))))
