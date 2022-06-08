
(define (normalize-executable typ ;; :executable || :test
                              pkg-path privname pubname
                              stanza-alist srcfiles)
  (format #t "NORMALIZE-EXECUTABLE ~A, ~A\n" privname pubname)
  (format #t "    srcfiles: ~A\n" srcfiles)
  ;; (if pubname
  ;;     (if (equal? pubname 'tezos-node)
  ;;         (format #t "stanza-alist: ~A\n" stanza-alist)))
  ;; NB: stanza-alist excludes 'names' and 'public_names'
  ;; if has modules list, one must match 'name'

  ;; (update-public-exe-table pkg-path pubname pubname)
  ;; (update-public-exe-table pkg-path privname pubname)

  (let* ((modules (assoc 'modules stanza-alist)))
    (format #t "modules: ~A\n" modules)
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
        ;; no 'modules' field
        )

    (let ((s
           (remove '()
                   (map (lambda (fld-assoc)
                          (format #t "fld-assoc: ~A\n" fld-assoc)
                          ;; (if (equal? pubname 'rpc_openapi)
                          ;;     (format #t "rpc_openapi pubname: ~A\n"
                          ;;                fld-assoc))
                          (case (car fld-assoc)
                            ((name) '()) ;; already taken care of
                            ((public_name) '()) ;; already taken care of
                            ((libraries) ;; => :deps
                             (normalize-exe-libraries fld-assoc stanza-alist))
                            ((modules) ;; => :modules ht
                             (let ((modules-ht (modules->modstbl
                                                (cdr fld-assoc) srcfiles)))
                               (hash-table-set! modules-ht
                                                (normalize-module-name
                                                 privname)
                                                :main)
                               ;; (format #t "Exec modules-ht: ~A:: ~A\n"
                               ;;         pkg-path modules-ht)
                               `(:modules ,modules-ht)))

                             ;; (let-values (((direct indirect)
                             ;;               (modules->modstbl
                             ;;                srcfiles fld-assoc)))
                               ;; (let* ((raw `((:raw ,fld-assoc)))
                               ;;        (main (cons `(:main ,(normalize-module-name privname))
                               ;;                    raw))
                               ;;        (direct-filtered (if (null? direct) '()
                               ;;                             (begin
                               ;;                               ;; (format #t "REMOVING ~A from ~A\n"
                               ;;                               ;;         privname direct)
                               ;;                               (remove (normalize-module-name privname)
                               ;;                                       (remove privname direct)))))
                               ;;        (direct (if (null? direct-filtered)
                               ;;                    main
                               ;;                    (cons `(:direct
                               ;;                            ,@(sort! direct-filtered sym<?))
                               ;;                          main)))
                               ;;        (indirect (if (null? indirect)
                               ;;                      direct
                               ;;                      (cons `(:indirect
                               ;;                              ,@(sort!
                               ;;                                 indirect
                               ;;                                 sym<?))
                               ;;                            direct)))
                               ;;        (result `(:modules ,indirect)))
                               ;;   ;; (format #t "RESULT modules: ~A\n" result)
                               ;;   result)
                               ;; ))
                            ((flags) (normalize-stanza-fld-flags fld-assoc))
                            ((foreign_stubs)
                             (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))

                            ((package) ;;
                            ;; (package <package>) if there is a
                            ;; (public_name ...) field, this specifies
                            ;; the package the executables are part of
                            ;; interp: used for namespacing if this
                            ;; executable is referenced by a `%{...}`
                            ;; varexpr.
                             `(:package ,(cadr fld-assoc)))

                            (else fld-assoc)))
                        stanza-alist))))

      ;;FIXME: it's not clear how "exe:..." and "bin:..." work, so we
      ;;register both, with and without sfx ".exe"
      (update-executables-table
       (string->symbol
        (string-append "bin:" (symbol->string pubname)))
       (string->symbol (string-append "//" pkg-path
                                      ":" (symbol->string pubname)
                                      ".exe")))
      (update-executables-table
       (string->symbol
        (string-append "bin:" (symbol->string pubname) ".exe"))
       (string->symbol (string-append "//" pkg-path
                                      ":" (symbol->string pubname)
                                      ".exe")))
      (update-executables-table
       (string->symbol
        (string-append "exe:" (symbol->string pubname)))
       (string->symbol (string-append "//" pkg-path
                                      ":" (symbol->string pubname)
                                      ".exe")))
      (update-executables-table
       (string->symbol
        (string-append "exe:" (symbol->string pubname) ".exe"))
       (string->symbol (string-append "//" pkg-path
                                      ":" (symbol->string pubname)
                                      ".exe")))
      ;;     (if (equal? pubname 'tezos-node)
      ;;         (format #t "tezos-node stanza: ~A\n" s)))

      (if-let ((mods (assoc :modules s)))
              (list typ ;; :executable
                    (concatenate
                     `((:name ((:private ,privname)
                               (:public ,pubname))))
                     s))

              ;; else 'modules' fld is missing, so add all modules
              ;; and don't forget to remove 'main' from the list
              ;; (let* ((xmodules (srcs->module-names srcfiles)) ;;  '()
              ;;        (modules (remove (normalize-module-name privname)
              ;;                         (remove privname xmodules))))
              (let* ((modules-ht (srcfiles->modstbl srcfiles)))
                (hash-table-set! modules-ht
                                 (normalize-module-name privname)
                                 :main)
                ;; (format #t "exec modules-ht: ~A:: ~A\n"
                ;;         pkg-path modules-ht)
                (list typ ;; :executable
                      (concatenate
                       `((:name ((:private ,privname)
                                 (:public ,pubname))))
                       `((:modules ,modules-ht
                          ;; ((:main ,privname)
                          ;;  (:direct
                          ;;   ,@(sort!
                          ;;      modules
                          ;;      sym<?)))
                          ))
                       s)))))))

(define (normalize-stanza-executable typ pkg-path srcfiles stanza)
  ;; type:: :executable || test
  ;; (let ((privname (cadr (assoc 'name (cdr stanza)))))
  ;;   (format #t "NORMALIZE-STANZA-EXECUTABLE: ~A: ~A\n" pkg-path privname))
  ;;stanza)

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
         ;; 'name' fld is required
         (privname (cadr (assoc 'name stanza-alist)))
         (pubname (if-let ((pubname (assoc 'public_name stanza-alist)))
                          (cadr pubname) privname))
         (modules (assoc 'modules stanza-alist))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist)))
    ;; (format #t " N: ~A\n" privname)
    ;; (format #t " Ms: ~A\n" modules)

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

    (normalize-executable typ
                          pkg-path privname pubname
                          filtered-stanza-alist srcfiles)
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
;; normalize-stanza-executables: src/lib_store/test

;; "The optional fields [for 'tests stanza] that are supported are a
;; subset of the alias and executables fields. In particular, all
;; fields except for public_names are supported from the executables
;; stanza. Alias fields apart from name are allowed."
(define (normalize-stanza-executables typ pkg-path srcfiles stanza)
  ;; typ:: :executables || :tests
  (format #t "NORMALIZE-STANZA-EXECUTABLES: ~A\n" pkg-path) ;; ~A\n" stanza)
  (format #t "  typ:  ~A\n" typ)
  ;; (format #t "  stanza:  ~A\n" stanza)
  ;; (:executables (names test_clic) ...
  (let* ((stanza-alist (cdr stanza))
         (privnames (assoc 'names stanza-alist))
         (pubnames (if (equal? typ :executables)
                       (assoc 'public_names stanza-alist)
                       #f))
         (filtered-stanza-alist (alist-delete '(names public_names) stanza-alist))
         )
    ;; (format #t "stanza-alist: ~A\n" stanza-alist)
    ;; (format #t "filtered-stanza-alist: ~A\n" filtered-stanza-alist)
    (if (equal? typ :executables)
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
               (normalize-executable :executable
                                     pkg-path privname pubname
                                     filtered-stanza-alist srcfiles)
               ;; `(:executable ((:name ((:private ,privname)
               ;;                        (:public ,pubname)))))
               )
             (cdr privnames) (cdr pubnames))

        ;; else no pubnames
        (map (lambda (privname)
               ;; (format #t "privname: ~A\n" privname)
               ;; (update-public-exe-table pkg-path privname privname)
               (normalize-executable
                (if (equal? typ :executables)
                    :executable
                    (if (equal? typ :tests)
                        :test
                        #f))
                pkg-path privname privname filtered-stanza-alist srcfiles)
               ;; `(:executable ((:name ((:private ,privname)))))
                        ;; (:module ,(normalize-module-name name))))))
               )
             (cdr privnames)))))

