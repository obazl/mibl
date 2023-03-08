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
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A\n"
              (ublue "-exec-flags->mibl") (assoc-val 'flags stanza-alist)))
  (let* ((flags (assoc 'flags stanza-alist))
         (flags (normalize-stanza-fld-flags flags :exec))
         (ocamlc_flags (if-let ((ocflags (assoc 'ocamlc_flags stanza-alist)))
                               (cons :ocamlc (cdr ocflags)) '()))
         (ocamlopt_flags (if-let ((ocflags
                                   (assoc 'ocamlopt_flags stanza-alist)))
                                 (cons :ocamlopt (cdr ocflags)) '()))
         (link-flags (if-let ((lf (assoc 'link_flags stanza-alist)))
                             lf '()))
         (link-flags (normalize-stanza-fld-flags link-flags :link))
         ;; integrate flags into link-flags
         )
    (if (or *mibl-debug-executables* *mibl-debugging*)
        (format #t "~A: ~A\n" (bgcyan "link-flags") link-flags))
    ;; (compile-flags '("-compile-flags"))
    ;; (link-flags '("-link_flags")))

    (let-values (((standard opens options flags)
                  (flags->mibl (assoc 'flags stanza-alist))))
      (if (or *mibl-debug-executables* *mibl-debugging*)
          (begin
            (format #t "~A: ~A~%" (red "standard") standard)
            (format #t "~A: ~A~%" (red "opens")    opens)
            (format #t "~A: ~A~%" (red "options")  options)
            (format #t "~A: ~A~%" (red "flags")    flags)
            (format #t "~A: ~A~%" (red "ocamlc_flags") ocamlc_flags)
            (format #t "~A: ~A~%" (red "ocamlopt_flags") ocamlopt_flags)
            (format #t "~A: ~A~%" (red "link_flags") link-flags)))

      ;; deal with empty lists
      (let* ((cflags (remove '() (list standard opens options flags)))
             (tcflags (remove '() (list ocamlc_flags ocamlopt_flags)))
             (compile-flags (append cflags tcflags))
             (link-flags (if (truthy? link-flags) (cdr link-flags) '()))
             ;; (link-flags (if (null? link-flags) '()
             ;;                 (remove '()
             ;;                         (list standard options
             ;;                               (if (null? link-flags)
             ;;                                   flags
             ;;                                   (cons (car flags)
             ;;                                         (append (cdr link-flags)
             ;;                                                 (cdr flags))))))))
             )

        (values ;;FIXME: add common flags
         (if (null? compile-flags) '() (list (cons :ccopts compile-flags)))
         (if (null? link-flags) '() (list (cons :llopts link-flags))))
        ))))

(define (-exec-modules-fld->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A\n" (blue "-exec-modules-fld->mibl") stanza-alist))
  (let* ((modules (assoc 'modules stanza-alist)))
    (if (or *mibl-debug-executables* *mibl-debugging*)
        (format #t "x modules: ~A\n" modules))
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
                (if (or *mibl-debug-executables* *mibl-debugging*)
                    (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
                            privname
                            (normalize-module-name (cadr modules))))))
        ;; else no 'modules' field
        )
    ))

(define (-map-compile-flds->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-map-compile-flds->mibl") stanza-alist))
  ;; compile manifest: includes link manifest, plus whatever
  ;; source files are in :deps
  (map (lambda (fld-assoc)
         ;; (format #t "compile fld-assoc: ~A\n" fld-assoc)
         (case (car fld-assoc)
           ((root_module)
            (error 'fixme
                   (format #f "unhandled compile fld: ~A" fld-assoc)))

           ;; ignore the rest - handled separately
           (else (values))))
       stanza-alist))

(define (-map-link-flds->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-map-link-flds->mibl") stanza-alist))
  (map (lambda (fld-assoc)
         ;; (format #t "link fld-assoc: ~A\n" fld-assoc)
         ;; (if (equal? pubname 'rpc_openapi)
         ;;     (format #t "rpc_openapi pubname: ~A\n"
         ;;                fld-assoc))
         (case (car fld-assoc)

           ((link_flags) (normalize-stanza-fld-flags fld-assoc :link))
           ((link_deps) (cons :link-deps (cdr fld-assoc)))

           ;; ignore the rest - handled separately
           (else (values))))
       stanza-alist))

(define (-map-cc-flds->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-map-cc-flds->mibl") stanza-alist))
  (map (lambda (fld-assoc)
         ;; (format #t "cc fld-assoc: ~A\n" fld-assoc)
         (case (car fld-assoc)
           ((ctypes) (error 'fixme "ctypes"))
           ((foreign_archives) (error 'fixme "foreign_archives"))
           ((foreign_stubs) (error 'fixme "foreign_stubs"))
           ;; (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

           (else (values))))
       stanza-alist))

(define (-map-common-flds->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-map-common-flds->mibl") stanza-alist))
  (map (lambda (fld-assoc)
         ;; (format #t "common fld-assoc: ~A\n" fld-assoc)
         (case (car fld-assoc)
           ;; ((name) `(:main ,(cdr fld-assoc)))
           ((package) ;;
            ;; (package <package>) if there is a
            ;; (public_name ...) field, this specifies
            ;; the package the executables are part of
            ;; interp: used for namespacing if this
            ;; executable is referenced by a `%{...}`
            ;; varexpr.
            `(:package ,(cadr fld-assoc)))

           ((root_module)
            (error 'fixme
                   (format #f "unhandled common fld: ~A" fld-assoc)))

           ((preprocess) ;;fld-assoc)
            (let ((preproc (assoc-val 'preprocess stanza-alist)))
              (if (or *mibl-debug-executables* *mibl-debugging*)
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
                  #f)))

           ((preprocess_deps) fld-assoc)
            ;; (error 'fixme
            ;;        (format #f "unhandled common fld: ~A" fld-assoc)))

           ((modes) `(:modes ,@(cdr fld-assoc))) ;; FIXME?

           ((forbidden_libraries) (error 'fixme "forbidden_libraries"))
           ((optional) (error 'fixme "fld: optional"))

           (else fld-assoc)))
       stanza-alist))

(define (-map-common-opts->mibl stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-map-common-flds->mibl") stanza-alist))
  (map (lambda (fld-assoc)
         ;; (format #t "common fld-assoc: ~A\n" fld-assoc)
         (case (car fld-assoc)
           ((flags) (normalize-stanza-fld-flags fld-assoc :common))
           ((ocamlc_flags)
            (normalize-stanza-fld-flags fld-assoc :ocamlc))

           ((ocamlopt_flags)
            (normalize-stanza-fld-flags fld-assoc :ocamlopt))

           (else (values))))
       stanza-alist))

;; returns (values <compile-flds> <link-flds>)
(define (-exec-flds->mibl pkg privnames stanza-alist)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (begin
        (format #t "~A: ~A\n" (ublue "-exec-flds->mibl") stanza-alist)
        (format #t "~A: ~A~%" (blue "privnames") privnames)))
  ;; 'libraries' and 'modules' flds may return multiple flds
  ;; so excluded them from mapping, handle separately
  ;; 'libraries' fld may generate :directs, :seldeps and :conditionals

  (let* ((deps (if-let ((libdeps (assoc-val 'libraries stanza-alist)))
                       (dune-libraries-fld->mibl libdeps pkg)
                       '()))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (uwhite "exec MIBLDEPS") deps)))
         ;; (_ (error 'tmp "tmp"))

         ;; pkg-manifest: (:manifest (:modules M1 M2 ...))
         ;; contains all modules in pkg
         (pkg-manifest (get-manifest pkg :exe #f stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (uwhite "pkg-manifest") pkg-manifest)))
         ;; FIXME: deal with private_modules too

         (mains (map normalize-module-name privnames))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (uwhite "mains") mains)))

         ;; exec-lib: pkg-manifest excluding main executables
         (exec-lib (let* ((modules (assoc-val :modules (cdr pkg-manifest)))
                          (exec-lib (filter (lambda (m) (not (member m mains))) modules)))
                     (if (truthy? exec-lib)
                         (cons :exec-libs exec-lib)
                         #f)))
         (exec-lib (if (truthy? exec-lib) (sort! exec-lib sym<?) exec-lib))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (uwhite "exec-lib") exec-lib)))

         ;; NB: pkg-manifest already includes conditionals
         ;; add conditionals to exec-lib
         ;; (conditionals (if-let ((clist (assoc-val :conditionals deps)))
         ;;                       (map (lambda (c)
         ;;                              (filename->module-name (assoc-val :target c)))
         ;;                            clist)
         ;;                       '()))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uwhite "conditionals") conditionals)))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (uwhite "exec-lib") exec-lib)))
         ;; (_ (error 'x "conds")))

         ;; ;; (compile-manifest (list :manifest (cons :modules (copy modules))))
         ;; (compile-manifest (if (truthy? modules)
         ;;                       `((:modules ,@(copy modules)))
         ;;                       (copy modules)))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (red "compile-manifest X") compile-manifest)))

         ;;FIXME: link-manifest is obsolete
         ;; (link-manifest (list :manifest (cons :modules (copy modules))))
         ;; (link-manifest `(:main (:modules ,(copy modules))))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (red "link-manifest") link-manifest)))

         (link-flds (-map-link-flds->mibl stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (ured "link-flds") link-flds)))

         (compile-flds (-map-compile-flds->mibl stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A\n" (red "compile-flds") compile-flds)))

         (common-flds (-map-common-flds->mibl stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (ured "common-flds") common-flds)))

         (common-opts (-map-common-opts->mibl stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (ured "COMMON-opts") common-opts)))

         (pps ())

         (cc-flds (-map-cc-flds->mibl stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (ured "cc-flds") cc-flds)))
         )
    ;; now handle manifest (modules fld) and submodules (deps fld)
    (if (or *mibl-debug-executables* *mibl-debugging*)
        (format #t "~A: ~A\n" (red "x DEPS") deps))
    (let* ((depslist
            (if (truthy? deps)
                (let* ((fixed (assoc :remote deps))
                       (conds (assoc :conditionals deps))
                       (seldeps (assoc :seldeps deps))
                       (alldeps (filter truthy? (list `,fixed conds seldeps))))
                  (if (or *mibl-debug-executables* *mibl-debugging*)
                      (format #t "~A: ~A~%" (bgred "alldeps") alldeps))
                  (if (truthy? alldeps)
                      `(:deps ,@alldeps)
                      '()))
                '())))
      (if (or *mibl-debug-executables* *mibl-debugging*)
          (format #t "~A: ~A~%" (uwhite "depslist") depslist))

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

      ;; (format #t "~A: ~A\n" (cyan "compile-manifest") compile-manifest)
      (if (or *mibl-debug-executables* *mibl-debugging*)
          (begin
            (format #t "~A: ~A\n" (cyan "compile-flds") compile-flds)
            (format #t "~A:: ~A\n" (cyan "stanza depslist") depslist)))
      ;;   (format #t "link-flds: ~A\n" link-flds)

      (values common-flds
              common-opts
              ;; `(:deps
              exec-lib
              (concatenate
                ;; compile-manifest
                (if (truthy? compile-flds) compile-flds '())
                ;; '((:WHERE :HERE))
                (if (truthy? depslist) depslist '())) ;; )
                link-flds
                ;; (remove '() (list compile-flds compile-manifest)))
                ;; (remove '() (append (list link-manifest)
                ;;                               link-flds))
                ;; `(:link ,@(remove '() (append (list link-manifest)
                ;;                               link-flds)))
                (if (truthy? cc-flds)
                    `(:cc ,cc-flds)
                    '())))))

(define unit-test-pkgs
  '(
    alcotest @alcotest//:alcotest
    ounit @ounit//:ounit
    ounit2 @ounit2//:ounit2
    )
  )

(define (-is-test-executable? ws pkg stanza)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-is-test-executable?") stanza))
  (case (car stanza)
    ((executable executables) #f)
    ((test tests) #t)
    (else
     (if (equal? (car stanza) 'tests)
         (let* ((stanza-alist (cdr stanza))
                (libdeps (assoc 'libraries stanza-alist))
                ;; (compile-deps (assoc-in '(:compile :deps :resolved) stanza-alist))
                )
           (if (or *mibl-debug-executables* *mibl-debugging*)
               (format #t "~A: ~A~%" (uwhite "libdeps") libdeps))
           (let ((test? (find-if (lambda (dep)
                                   (member dep unit-test-pkgs))
                                 libdeps)))
             (if (or *mibl-debug-executables* *mibl-debugging*)
                 (format #t "~A: ~A~%" (blue "answer") test?))
             test?))))))

(define (dune-executable->mibl ws pkg kind stanza)
  ;; kind:: :executable || test
  ;; (let ((privname (cadr (assoc 'name (cdr stanza)))))
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (begin
        (format #t "~A: ~A\n" (blue "dune-executable->mibl") stanza)
        (format #t "~A: ~A~%" (blue "kind") kind)))

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
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uwhite "stanza-alist") stanza-alist)))
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
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uwhite "package") package)))

         (modules (assoc 'modules stanza-alist))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist)))
    (if (or *mibl-debug-executables* *mibl-debugging*)
        (begin
          (format #t "~A: ~A\n" (uwhite "privname") privname)
          (format #t "~A: ~A\n" (uwhite "pubname") pubname)
          (format #t " Ms: ~A\n" modules)))

    (if pubname
        (begin
          (update-exports-table! ws
                                 (if (-is-test-executable? ws pkg stanza) :test :exe)
                                 (assoc-val 'modes stanza-alist)
                                 privname
                                 pkg-path privname)
          ;; (error 'fixme "STOP link modes")
          ;; (if (assoc-in '(:link :modes) stanza-alist)
          ;;     )
          (if (not (-is-test-executable? ws pkg stanza))
              (if package
                  (update-opam-table! ws :bin
                                      package
                                      pubname
                                      pkg-path
                                      privname ;; lib name
                                      )))))
    (if (and privname pubname
             (not (equal? privname pubname)))
        (error 'FIXME
               (format #f "name and public_name mismatch: ~A, ~A" privname pubname)))
        ;; (begin
        ;;   (update-exports-table! ws
        ;;                          (if (-is-test-executable? ws pkg stanza) :test :exe)
        ;;                          ;; (string->symbol (format #f ":test:~A" privname))
        ;;                          ;; (string->symbol (format #f ":bin:~A" privname)))
        ;;                          pubname
        ;;                          pkg-path privname)
        ;;   ;; (error 'fixme "STOP modes privname")
        ;;   ;; (if (assoc-in '(:link :modes) stanza-alist)
        ;;   ;;     )

        ;;   (if (not (-is-test-executable? ws pkg stanza))
        ;;       (if package
        ;;           (update-opam-table! ws :bin
        ;;                               package
        ;;                               pubname
        ;;                               pkg-path
        ;;                               privname ;; lib name
        ;;                               )))))

    ;; if has modules list, one must match 'name'
    ;; FIXME: not needed, we assume the dunefile is well-formed
    ;; (if modules
    ;;     (if (> (length (cdr modules)) 1)
    ;;         (begin
    ;;           ;; (format #t "normalize-stanza-executable WARNING: multiple modules for executable ~A\n"
    ;;           ;;         pkg-path)
    ;;           )
    ;;         ;; just like the 'modules' fld of 'library' stanzas
    ;;         (if (not
    ;;              (or (equal? privname (cadr modules))
    ;;                  (equal? (normalize-module-name privname)
    ;;                          (normalize-module-name (cadr modules)))))
    ;;             ;; error?
    ;;             (format #t "    WARNING: name/module mismatch: ~A : ~A\n"
    ;;                     privname
    ;;                     (normalize-module-name (cadr modules))))))

    ;; (let-values (((compile-flags link-flags)
    ;;               (-exec-flags->mibl stanza-alist)))
    ;;   (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
    ;;   (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)

      (let-values (((common-flds common-opts exec-lib compile-flds link-flds cc-flds)
                    (-exec-flds->mibl pkg (list privname) stanza-alist)))

        (if (or *mibl-debug-executables* *mibl-debugging*)
            (begin
              (format #t "~A: ~A\n" (uyellow "x common-flds") common-flds)
              (format #t "~A: ~A\n" (uyellow "x common-opts") common-opts)
              (format #t "~A: ~A\n" (uyellow "x compile-flds") compile-flds)
              (format #t "~A: ~A\n" (uyellow "x link-flds") link-flds)
              (format #t "~A: ~A\n" (uyellow "x cc-flds") cc-flds)))

        ;; `(:main ,(normalize-module-name (cadr fld-assoc))))
        ;; (list (-executable->mibl kind
        ;;                          pkg privname pubname
        ;;                          filtered-stanza-alist))
        (let ((result
              (list (cons kind
                    (remove '()
                            (append
                             `((:privname . ,privname))
                             (if pubname `((:pubname . ,pubname)) '())
                             `((:main . ,(normalize-module-name privname)))
                             (if (truthy? common-flds) common-flds '())
                             (if (truthy? common-opts) common-opts '())
                             (if (truthy? exec-lib) (list exec-lib) '())
                             (if (truthy? link-flds) link-flds '())
                             (if (truthy? compile-flds) (list compile-flds) '())
                             (if (truthy? cc-flds) (list cc-flds) '())
                             ))))))
          (if (or *mibl-debug-executables* *mibl-debugging*)
              (format #t "~A: ~A~%" (bgyellow "result") result))
          result))))

;; "The optional fields [for 'tests stanza] that are supported are a
;; subset of the alias and executables fields. In particular, all
;; fields except for public_names are supported from the executables
;; stanza. Alias fields apart from name are allowed."
(define (dune-executables->mibl ws pkg kind stanza)
  ;; kind:: :executable || :test
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (begin
        (format #t "~A: ~A\n" (bgblue "dune-executables->mibl") stanza)
        (format #t "~A:  ~A\n" (uwhite "kind") kind)
        (format #t "~A:  ~A\n" (uwhite "pkg") stanza)))
  ;; (:executables (names test_clic) ...
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (stanza-alist (cdr stanza))
         (privnames (if (case kind ((:executable :test) #t) (else #f))
                        (if-let ((privnames
                                 (assoc-val 'names stanza-alist)))
                               privnames '())
                       '()))
         (exemodules (map normalize-module-name privnames))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "exemodules") exemodules)))

         (pubnames (if (case kind ((:executable :test) #t) (else #f)) ;; (equal? kind :executable)
                       (if-let ((pubnames
                                 (assoc-val 'public_names stanza-alist)))
                               pubnames privnames)
                       privnames))


         ;;rename privpub to ??? stanza-modules?
         ;; NB: no need to unify pub and privnames, we only build privnames,
         ;; though pubnames may be used as target names. so use exemodules.
         (privpubmodules (remove-duplicates
                          (map normalize-module-name
                               (flatten (concatenate privnames pubnames)))))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (yellow "privpubmodules") privpubmodules)))
         (filtered-stanza-alist stanza-alist)
         ;; (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist))
         (text-exe? (-is-test-executable? ws pkg stanza))
         (testsuite-stanza (if text-exe? (list (cons :testsuite
                                                     (list (cons :name 'testsuite)
                                                           (cons :tests privnames))))
                               #f))
         )
    (if (or *mibl-debug-executables* *mibl-debugging*)
        (begin
          (format #t "~A: ~A~%" (uwhite "exec privnames") privnames)
          (format #t "~A: ~A~%" (uwhite "exec pubnames") pubnames)))
    ;; (error 'fixme "STOP execs")

    ;; flags and (libraries) etc. apply to each of the executables
    ;; each name in (names) is one module;
    ;; omitted (modules) means none
    (let ((test-exe? (-is-test-executable? ws pkg stanza))
          (package (assoc-val 'package (cdr stanza))))

      ;; (let-values (((compile-flags link-flags)
      ;;               (-exec-flags->mibl stanza-alist)))
      ;;   (format #t "~A: ~A\n" (uyellow "compile-flags") compile-flags)
      ;;   (format #t "~A: ~A\n" (uyellow "link-flags") link-flags)
      ;;   (error 'STOP "STOP wwwwwwwwwwwwwwww"))

        (let-values (((common-flds common-opts exec-lib compile-flds link-flds cc-flds)
                      (-exec-flds->mibl pkg privnames stanza-alist)))

          (if (or *mibl-debug-executables* *mibl-debugging*)
              (begin
                (format #t "~A: ~A\n" (uyellow "xs common-flds") common-flds)
                (format #t "~A: ~A\n" (uyellow "xs common-opts") common-opts)
                (format #t "~A: ~A\n" (uyellow "xs compile-flds") compile-flds)
                (format #t "~A: ~A\n" (uyellow "xs link-flds") link-flds)
                (format #t "~A: ~A\n" (uyellow "xs cc-flds") cc-flds)

                ;; (error 'fixme "STOP execs llopts")

                (format #t "~A: ~A~%" (uwhite "privnames") privnames)
                (format #t "~A: ~A~%" (uwhite "pubnames") pubnames)))
          ;; (format #t "filtered-stanza-alist: ~A\n" filtered-stanza-alist)
          ;; (if (equal? kind :executable)

          ;; FIXME: don't need to do this, we assume the dunefile is good?
          ;; (if (> (length (cdr privnames)) 1)
          ;;     (begin
          ;;       ;; (format #t "MULTIPLE NAMES\n")
          ;;       (if (not (null? pubnames))
          ;;           (if (not (equal?
          ;;                     (length (cdr privnames))
          ;;                     (length (cdr pubnames))))
          ;;               (error
          ;;                'bad-arg
          ;;                "names and public_names differ in length"))
          ;;           )))

          ;; for executables, we add the pub/priv names to the lookup table, so that the
          ;; emitter can resolve references to them.
          ;; dune emits *.exe and *.bc, which may be referred to
          ;; in dunefiles, so we add them to the lookup table too.

          ;; (cond
          ;;  ((and (not (null? pubnames))
          ;;        (not (null? privnames)))
          ;;   (format #t "~A: ~A, ~A~%" (bgblue "iter over priv/pubnames") pubnames privnames)

          (if (or *mibl-debug-executables* *mibl-debugging*)
              (format #t "~A: ~A, ~A~%" (bgred "ITERATING EXECUTABLES") privnames pubnames))
          (map (lambda (privname pubname)
                 (if (or *mibl-debug-executables* *mibl-debugging*)
                     (format #t "~A: ~A, ~A~%" (umagenta "encoding pubpriv exec") privname pubname))
                 (let ((privmodule (normalize-module-name privname)))

                   (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
                       (begin
                         (if (or *mibl-debug-executables* *mibl-debugging*)
                             (format #t "~A: ~A, ~A~%" (umagenta "updating exports w/exec") privname pubname))
                         (update-exports-table! ws :exe
                                                (assoc-val 'modes stanza-alist)
                                                pubname pkg-path privname)
                         (update-exports-table! ws :exe (assoc-val 'modes stanza-alist)
                                                privname pkg-path privname)
                         (update-exports-table! ws :bin (assoc-val 'modes stanza-alist)
                                                pubname pkg-path privname)
                         (update-exports-table! ws :bin (assoc-val 'modes stanza-alist)
                                                privname pkg-path privname)
                         (update-opam-table! ws :bin
                                             package
                                             pubname
                                             pkg-path
                                             privname ;; lib name
                                             )))

                   (if (or *mibl-debug-executables* *mibl-debugging*)
                       (format #t "~A: ~A, ~A~%" (bgmagenta "constructing pubpriv exec") privname pubname))
                   ;; NB: to avoid structure sharing we need to copy toplevel :manifest subtree
                   (let* (;; (link-flds (copy link-flds))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-flds") link-flds)))
                          ;; ;; (link-manifest (assoc :manifest (cdr link-flds)))
                          ;; (link-manifest (assoc :main (cdr link-flds)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-manifest") link-manifest)))
                          ;; ;; (link-modules (assoc-in '(:manifest :modules) (cdr link-flds)))
                          ;; (link-modules (assoc :main (cdr link-flds)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-modules") link-modules)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "privmodule") privmodule)))
                          ;; ;; (lmodules (filter (lambda (x) (member x privpubmodules)) (cdr link-modules)))

                          ;; (lmodules (list privmodule))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "lmodules") lmodules)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "exemodules") exemodules)))

                          ;; ;; do not link exemodules
                          ;; (filtered (filter (lambda (m) (not (member m exemodules))) (cdr link-modules)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-flds exe filtered") filtered)))

                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-flds unfiltered") link-flds)))
                          ;; (_ (set-cdr! link-flds `((:main (:modules ,@(copy lmodules))))))
                          ;; (_ (set-cdr! link-flds `((:manifest (:modules ,@(copy lmodules))))))
                          ;; (_ (alist-update-in! (cdr link-flds) '(:main) ;; '(:manifest :modules)
                          ;;                      (lambda (old)
                          ;;                        filtered
                          ;;                        ;;(append (list `(:main . ,privname)) filtered)
                          ;;                        )))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "link-flds filtered") link-flds)))

                          (-compile-flds (copy compile-flds))
                          (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "-compile-flds") -compile-flds)))

                          ;; (compile-manifest (assoc :manifest (cdr -compile-flds)))
                          ;; (compile-manifest (assoc-val :main (cdr -compile-flds)))
                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "compile-manifest") compile-manifest)))

                          ;; (compile-deps (dissoc '(:manifest) (cdr -compile-flds))) ;; (assoc :deps (cdr -compile-flds)))
                          (compile-deps (if (truthy? -compile-flds)
                                            (assoc :deps (cdr -compile-flds)) '()))
                          (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "compile-deps") compile-deps)))

                          ;; (compile-modules (copy (assoc-in '(:manifest :modules) (cdr -compile-flds))))
                          (compile-modules (if (truthy? -compile-flds)
                                               (copy (assoc-val :main (cdr -compile-flds))) '()))
                          (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "compile-modules A") compile-modules)))

                          ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "privpub modules") privpubmodules)))
                          ;; (cmodules (filter (lambda (x) (not (member x privpubmodules))) (cdr compile-modules)))

                          (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "exemodules") exemodules)))
                          (cmodules (if (and (truthy? compile-modules) (truthy? exemodules))
                                        (filter (lambda (x) (not (member x exemodules))) (cdr compile-modules))
                                        '()))
                          (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (umagenta "cmodules A") cmodules)))

                          ;; (_ (set-cdr! -compile-flds `((:manifest (:modules ,@(cons privmodule cmodules)))
                          (_ (if (and (truthy? -compile-flds) (truthy? cmodules))
                                 (if (truthy? compile-deps)
                                     (set-cdr! -compile-flds `((:main ,@(cons privmodule cmodules))
                                                               ,compile-deps))
                                     (set-cdr! -compile-flds `((:main ,@(cons privmodule cmodules)))))))
                          ;; (_ (alist-update-in! (cdr -compile-flds) '(:manifest :modules) (lambda (old) (cons privmodule cmodules))))
                          (_ (if (or *mibl-debug-executables* *mibl-debugging*)
                                 (begin
                                   (format #t "~A: ~A~%" (umagenta "-common-flds") common-flds)
                                   (format #t "~A: ~A~%" (umagenta "-common-opts") common-opts)
                                   (format #t "~A: ~A~%" (umagenta "-compile-modules") compile-modules)
                                   (format #t "~A: ~A~%" (umagenta "-compile-flds") -compile-flds)
                                   (format #t "~A: ~A~%" (umagenta "compile-flds") compile-flds)
                                   (format #t "~A: ~A~%" (umagenta "pubname") pubname))))
                          (mibl (append ;; kind
                                 `((:pubname . ,pubname))
                                 `((:privname . ,privname))
                                 `((:main . ,(normalize-module-name privname)))
                                 (if (truthy? common-flds) common-flds '())
                                 (if (truthy? common-opts) common-opts '())
                                 (if (truthy? exec-lib) (list exec-lib) '())
                                 (if (truthy? link-flds) link-flds '())
                                 (if (truthy? -compile-flds) (list -compile-flds) '())
                                 (if (truthy? cc-flds) cc-flds '())
                                 ))
                          (mibl (if test-exe?
                                    (append mibl '(:in-testsuite testsuite)) mibl))

                          (mibl (cons kind mibl)))
                     (if (or *mibl-debug-executables* *mibl-debugging*)
                         (format #t "~A: ~A~%" (umagenta "finished construction") mibl))
                     mibl)
                   ;; (let ((x (-executable->mibl kind
                   ;;                             pkg privname pubname
                   ;;                             filtered-stanza-alist)))
                   ;;   (prune-mibl-rule x)
                   ;;   )
                   ))
               privnames pubnames)
            ;;)

           ;; ((null? pubnames)
           ;;  (format #t "~A: ~A~%" (bgblue "iter privnames") privnames)
           ;;  (append
           ;;   (if test-exe? testsuite-stanza '())
           ;;   (map (lambda (privname)
           ;;          (format #t "~A: ~A~%" (uyellow "encoding priv exec") privname)
           ;;          (format #t "~A: ~A\n" (uwhite "privname") privname)
           ;;          (let ((privmodule (normalize-module-name privname)))
           ;;            (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
           ;;                (begin
           ;;                  ;; (error 'fixme "STOP privexe")
           ;;                  (update-exports-table! ws :foo privname
           ;;                                         pkg-path privname)
           ;;                  (update-exports-table! ws :bin privname
           ;;                                         pkg-path privname)
           ;;                  ;; (if (assoc 'modes stanza-alist)
           ;;                  ;;     (update-exports-table! ws :bin
           ;;                  ;;                            (format #f "~A.bc" privname)
           ;;                  ;;                        pkg-path privname)
           ;;                  ;;     )
           ;;                  ;; (format #t "~A: ~A~%" (red "s")
           ;;                  ;;         (assoc 'modes stanza-alist))
           ;;                  ;; (error 'fixme "STOP bc")

           ;;                  ;; (update-opam-table! ws :bin
           ;;                  ;;                     package
           ;;                  ;;                     pubname
           ;;                  ;;                     pkg-path
           ;;                  ;;                     privname ;; lib name
           ;;                  ;;                     )
           ;;                  ))

           ;;            (format #t "~A: ~A~%" (bgyellow "constructing priv exec") privname)
           ;;            (let* (;; (link-flds (copy link-flds))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "link-flds") link-flds)))

           ;;                   ;; (link-manifest (assoc :manifest (cdr link-flds)))
           ;;                   ;; (link-main (assoc :main link-flds))
           ;;                   ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "link-main") link-main)))

           ;;                   ;; (link-rest (dissoc '(:main) (cdr link-flds)))
           ;;                   ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "link-rest") link-rest)))

           ;;                   ;; (link-modules (assoc-in '(:manifest :modules) (cdr link-flds)))
           ;;                   ;; (link-modules (assoc :main link-flds))
           ;;                   ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "link-modules") (cdr link-modules))))
           ;;                   ;; (_ (if (equal? privname 'binary) (error 'fixme "STOP modes")))

           ;;                   ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "privmodule") privmodule)))
           ;;                   ;; (lmodules (filter (lambda (x) (member x privpubmodules)) (cdr link-modules)))

           ;;                   ;; (lmodules (list privmodule))

           ;;                   ;; (_ (set-cdr! link-flds `((:manifest (:modules ,@(copy lmodules)))
           ;;                   ;;                          ,@link-rest)))
           ;;                   ;; (_ (set-cdr! link-flds `((:main ,(copy privmodule))
           ;;                   ;;                          ,@link-rest)))
           ;;                   ;; (_ (alist-update-in! (cdr link-flds) '(:manifest)
           ;;                   ;;                      (lambda (old) (append (list `(:main . ,privname)) old))))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "link-flds") link-flds)))

           ;;                   (-compile-flds (copy compile-flds))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds)))

           ;;                   ;; (compile-manifest (assoc :manifest (cdr -compile-flds)))
           ;;                   (compile-manifest (if (truthy? -compile-flds)
           ;;                                         (if-let ((cm (assoc :modules (cdr -compile-flds))))
           ;;                                                 cm '())
           ;;                                         '()))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "compile-manifest") compile-manifest)))

           ;;                   ;; (compile-rest (dissoc '(:manifest) (cdr -compile-flds)))
           ;;                   (compile-rest (if (truthy? -compile-flds)
           ;;                                     (if-let ((cr (assoc :deps (cdr -compile-flds))))
           ;;                                             cr '())
           ;;                                     '()))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "compile-rest") compile-rest)))

           ;;                   ;; (compile-deps (assoc :deps (cdr -compile-flds)))
           ;;                   ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "compile-rest") compile-rest)))

           ;;                   ;; (compile-modules (copy (assoc-in '(:manifest :modules) (cdr -compile-flds))))
           ;;                   (compile-modules (if (truthy? -compile-flds)
           ;;                                        (copy (assoc-val :modules (cdr -compile-flds)))
           ;;                                        '()))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "compile :manifest :modules") compile-modules)))

           ;;                   ;; (cmodules (filter (lambda (x) (not (member x privpubmodules))) (cdr compile-modules)))
           ;;                   ;; (_ (set-cdr! -compile-flds `((:manifest (:modules ,@(cons privmodule cmodules)))
           ;;                   ;;                              ,compile-rest)))
           ;;                   (cmodules (list privmodule))

           ;;                   (_ (if (truthy? -compile-flds)
           ;;                          (set-cdr! -compile-flds (if (truthy? compile-rest)
           ;;                                                  `((:modules ,@(copy cmodules)) ,compile-rest)
           ;;                                                  `((:modules ,@(copy cmodules)))))))

           ;;                                                  ;; `((:manifest (:modules ,@(copy cmodules))) ,compile-rest)
           ;;                                                  ;; `((:manifest (:modules ,@(copy cmodules)))))))

           ;;                   ;; (_ (alist-update-in! (cdr -compile-flds) '(:manifest :modules) (lambda (old) (cons privmodule cmodules))))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "-compile-modules") compile-modules)))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "-compile-flds") -compile-flds)))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "compile-flds") compile-flds)))

           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "common-flds") common-flds)))
           ;;                   (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (uyellow "common-opts") common-opts)))
           ;;                   )

           ;;              (append (list kind)
           ;;                    ;; `,@(if test-exe? (cons :in-testsuite 'testsuite) '())
           ;;                    ;; (cons :in-testsuite 'testsuite)
           ;;                    `((:privname . ,privname))
           ;;                    `((:main . ,(normalize-module-name privname)))
           ;;                    (if (truthy? common-flds) common-flds '())
           ;;                    (if (truthy? common-opts) common-opts '())
           ;;                    (if (truthy? link-flds) link-flds '())
           ;;                    (if (truthy? -compile-flds) (list -compile-flds) '())
           ;;                    (if (truthy? cc-flds) (list cc-flds) '())
           ;;                    )

           ;;              ;; (let ((x (-executable->mibl kind
           ;;              ;;                             ;; (if (equal? kind :executable)
           ;;              ;;                             ;;     :executable
           ;;              ;;                             ;;     (if (equal? kind :test)
           ;;              ;;                             ;;         :test
           ;;              ;;                             ;;         #f))
           ;;              ;;                             pkg-path
           ;;              ;;                             privname privname
           ;;              ;;                             filtered-stanza-alist srcfiles)))
           ;;              ;;   ;; `(:executable ((:name ((:private ,privname)))))
           ;;              ;;   ;; (:module ,(normalize-module-name name))))))
           ;;              ;;   (prune-mibl-rule x)
           ;;              ;;   )
           ;;              )))
           ;;        privnames)))

           ;; ((null? privnames)
           ;;  (format #t "~A: ~A~%" (bgblue "iter pubnames") pubnames)
           ;;  (map (lambda (pubname)
           ;;         (format #t "~A: ~A, ~A~%" (ucyan "encoding pub exec") pubname)
           ;;         ;; (format #t "privname: ~A\n" privname)
           ;;         (if (not test-exe?) ;; (-is-test-executable? ws pkg stanza))
           ;;             (begin
           ;;               (update-exports-table! ws :exe pubname
           ;;                                      pkg-path pubname)
           ;;               (update-exports-table! ws :bin pubname
           ;;                                      pkg-path pubname)
           ;;               (update-opam-table! ws :bin
           ;;                                   ;;FIXME: if package package else pubname
           ;;                                   package ;; opam pkg name
           ;;                                   pubname ;; lib name
           ;;                                   pkg-path
           ;;                                   pubname ;; lib name
           ;;                                   )))

           ;;         (format #t "~A: ~A~%" (ucyan "constructing pub exec") pubname)
           ;;         (append (list kind)
           ;;               ;; `,@(if test-exe? (cons :in-testsuite 'testsuite) (values))
           ;;               ;; (if test-exe? (cons :in-testsuite 'testsuite) (values))
           ;;               ;; (cons :in-testsuite 'testsuite)
           ;;               `((:pubname . ,pubname))
           ;;               `(:main . ,(normalize-module-name pubname))
           ;;               (if (truthy? common-flds) common-flds '())
           ;;               (if (truthy? common-opts) (list common-opts) '())
           ;;               (if (truthy? link-flds) link-flds '())
           ;;               (if (truthy? -compile-flds) (list -compile-flds) '())
           ;;               (if (truthy? cc-flds) (list cc-flds) '())
           ;;               );; compile-flags))

           ;;         ;; (let ((x (-executable->mibl kind
           ;;         ;;                             ;; (if (equal? kind :executable)
           ;;         ;;                             ;;     :executable
           ;;         ;;                             ;;     (if (equal? kind :test)
           ;;         ;;                             ;;         :test
           ;;         ;;                             ;;         #f))
           ;;         ;;                             pkg-path
           ;;         ;;                             privname privname
           ;;         ;;                             filtered-stanza-alist srcfiles)))
           ;;         ;;   ;; `(:executable ((:name ((:private ,privname)))))
           ;;         ;;   ;; (:module ,(normalize-module-name name))))))
           ;;         ;;   (prune-mibl-rule x)
           ;;         ;;   )
           ;;         )
           ;;       pubnames))

           ;; (else
           ;;  (error 'fixme "executables missing flds names and public_names"))
           )))) ;)
