;; (display "loading @camlark//scm/codept.scm") (newline)

(load "opam.scm")
(load "utils.scm")

(define codept-cmd "/Users/gar/.opam/4.14.0/bin/codept")

;; task: list all src files in each pkg
;; task: put namespaced modules in codept "group"

;; lib and exec stanzas have :module flds

(define (module-name-equal? m fname)
  ;; (display (format #f "MODULE-NAME-EQUAL? ~A ~A : ~A"
  ;;                  m fname (string-prefix-ci?
  ;;                           (if (symbol? m) (symbol->string m) m)
  ;;                           fname)))
  ;; (newline)
  (string-prefix-ci?
   (string-append (if (symbol? m) (symbol->string m) m) ".")
   fname))

;; return both .ml and .mli files, with path
;; FIXME: we can have a module w/o srcfiles, if the module is generated
;; returns pair of mlfile, mlifile, in either order, "" is missing
(define (module-name->file-paths m-name path ocaml-srcs)
  ;; (format #t "module-name->file-paths ~A:: ~A\n" path m-name)
  (let recur ((srcs ocaml-srcs)
              (m1 #f)
              (m2 #f))
    (if (null? srcs)
        (values (if m1 m1 "") (if m2 m2 ""))

        ;; else
        (if (module-name-equal? m-name (car srcs))
            (if m1
                (values m1 (string-append path "/" (car srcs)))
                (recur (cdr srcs)
                       (string-append path "/" (car srcs))
                       m2))
            (recur (cdr srcs) m1 m2)))))

(define (module-name->file-names m-name srcfiles)
  ;; (newline)
  (if (null? srcfiles)
      '()
      (if (module-name-equal? m-name (car srcfiles))
          (cons (car srcfiles)
                       (module-name->file-names m-name (cdr srcfiles)))
          (module-name->file-names m-name (cdr srcfiles)))))

;;;;;;;;;;;;;;;; executable (singular)
;; for dune, the 'public_name' field means "install to system /bin
;; under this name".  for obazl we do not install anything.
(define (codept-emit-exec-args fs-path stanza ocaml-srcs out-port)
  ;; (format #t "codept-emit-exec-args ~A: ~A\n" fs-path stanza)
  (let* ((nm (cadr (assoc :name (cadr stanza))))
         (stanza-alist (cadr stanza))
         (ms (if-let ((ms (assoc :modules stanza-alist)))
                     (if (null? (cdr ms))
                         (begin
                           (format #t "WARNING: empty library: ~A:~A\n"
                                   fs-path nm)
                           '())
                         (cadr ms))
                     '())))
    ;; (format #t "exec modules: ~A\n" ms)
    (if (null? ms)
        ;; no explicit modules: emit all srcfiles
        (begin
          ;; (display (format #f "MODS: :standard (implicit)")) (newline)
          ;; (display (format #f "SRCS: ~A" ocaml-srcs)) (newline)
          (for-each (lambda (f)
                      (let ((fpath (string-append fs-path "/" f)))
                        (if (not (hash-table-ref emitted-files fpath))
                            (begin
                              (format out-port "~A" fpath)
                              (newline out-port)
                              (hash-table-set! emitted-files fpath #t)))))
                    ocaml-srcs))
          ;; (newline out-port))

        ;; else we have explicit modules list
        (begin
          ;; (display (format #f "MODS: ~A" ms)) (newline)
          ;; (display (format #f "SRCS: ~A" ocaml-srcs)) (newline)

          ;; (format out-port "~A[" nm)
          (for-each (lambda (dep-kv)
                      ;; (if (equal? :direct (cadr dep-kv)

                      (let-values
                          (((m1 m2)
                                (module-name->file-paths
                                 (car dep-kv)
                                 fs-path ocaml-srcs)))
                        ;; (format #t "m1: ~A\n" m1)
                        ;; (format #t "m2: ~A\n" m2)
                        (if m1
                            (if (not (hash-table-ref emitted-files m1))
                                (begin
                                  (format out-port "~A" m1)
                                  (newline out-port)
                                  (hash-table-set! emitted-files
                                                   m1 #t))))
                        (if m2
                            (if (not (hash-table-ref emitted-files m2))
                                (begin
                                  (format out-port "~A" m2)
                                  (newline out-port)
                                  (hash-table-set! emitted-files
                                                   m2 #t)))))
                      )

                      ;; (format out-port "~A"
                      ;;         ;; (string->symbol
                      ;;         ;; (string-append fs-path "/"
                      ;;         (module-name->file-paths m fs-path ocaml-srcs))
                      ;; )
                    ms))
          ;; (format out-port "]")
          ;; (newline out-port))
        )
    )
  )

(define (codept-emit-executable-args fs-path exec-stanzas ocaml-srcs out-port)
 ;; (format #t "codept-emit-executable-args fs-path: ~A\n" fs-path)
  ;; (newline)
  ;; (display (format #f "codept-emit-executable-args exec-stanzas: ~A" exec-stanzas))
  ;; (newline)
  (if (not (null? exec-stanzas))
      (begin
        (for-each (lambda (stanza)
                    (codept-emit-exec-args fs-path stanza ocaml-srcs out-port))
                  exec-stanzas)))
      ;; (begin (display (format #f "  no library stanzas for ~A" fs-path)) (newline)))
  )

;;;;;;;;;;;;;;;; executables
;; each name in the 'names' field corresponds to a module whose source
;; must be in the directory. the 'modules' field is as for libraries,
;; except the default is "ignore all" rather than 'include all".
;; note that anything in 'modules' will be a dependency of each executable.
;; to to emit codept args: iterate over names, emit the name and the modules.
;; Q: what does empty '(modules)' mean? all or none? presumably the latter
(define (codept-emit-execs-nm-arg fs-path nm stanza srcfiles out-port)
  ;; EFFICIENCY: we only need to pull the non-names fields once
  (let* ((ocaml-srcs (cadr (assoc-in '(:ocaml :static) srcfiles)))
         (modules (if-let ((modules (assoc 'modules (cdr stanza))))
                     (if (null? (cdr modules))
                         (begin
                           (display
                            (format #f "WARNING: empty library: ~A:~A"
                                    fs-path nm)) (newline)
                            '())
                         (cdr modules))
                     '())))
    (format out-port "~A\n"
            (string->symbol
             (module-name->file-paths nm fs-path ocaml-srcs)))))

(define (srcs-less-execs nms srcfiles)
  (let* ((exec-files (map (lambda (module-name)
                            (module-name->file-names module-name srcfiles))
                          nms))
         ;; exec-files should always be a subset of srcfiles
        (diff (lset-difference equal? srcfiles (flatten exec-files))))
    diff))

(define (codept-emit-execs-modules-args fs-path stanza srcfiles out-port)
  (let* ((ocaml-srcs (cadr (assoc-in '(:ocaml :static) srcfiles)))
         (modules (if-let ((modules (assoc 'modules (cdr stanza))))
                     (if (null? (cdr modules))
                         (begin
                           (display
                            (format #f "WARNING: empty library: ~A:~A"
                                    fs-path nm)) (newline)
                            '())
                         (cdr modules))
                     '())))
    ;; FIXME: modules_without_implementation?
    (if (null? modules) ;; all modules EXCEPT those in the names list
        (let* ((nms (cdr (assoc 'names (cadr stanza))))
               (module-deps (srcs-less-execs nms ocaml-srcs)))
          (for-each (lambda (f)
                      ;; if f not in modules_without_implementation?
                      (format out-port "~A\n"
                             (string->symbol
                              (string-append fs-path "/" f ","))))
                    module-deps))

        ;; else we have an explicit '(modules ...)' list
        (begin
          ;; (display (format #f "MODULES for executables stanza: ~A\n" modules))
          (for-each (lambda (module)
                    (if (pair? module)
                        (format outport "FIXME: ~A\n" module)
                        (if (or (symbol? module) (string? module))
                            (format out-port "~A\n"
                                    (module-name->file-paths
                                     module fs-path ocaml-srcs))
                            ;;
                            (format outport "FIXME2: ~A" module))))
                  modules))
        )
    )
  )

;; (define (codept-emit-executables-args fs-path execs-stanzas srcfiles out-port)
;;   ;; (display (format #f "codept-emit-executables-args ~A" fs-path)) (newline)
;;   (if (null? execs-stanzas)
;;       '()
;;       (for-each
;;        (lambda (stanza)
;;          ;; each executables stanza may contain multiple exec names
;;          (let* ((nms (cdr (assoc 'names (cadr stanza)))))
;;            (for-each (lambda (nm)
;;                        (codept-emit-execs-nm-arg fs-path nm stanza srcfiles out-port))
;;                      nms))
;;          ;; modules apply to all names, so emit only once
;;          (codept-emit-execs-modules-args fs-path stanza srcfiles out-port)
;;          )
;;        execs-stanzas)))

;; WARNING: for now, ignore libs with empty `(modules)`. We have some
;; example, tezos/src/lib_protocol_environment/s_packer/dune,
;; src/tooling/dune where it is used to make a "placeholder" needed to
;; install (and elsewhere use) an executable in the system
;; <prefix>/libexec dir.
;; emitted-files: cache to avoid dups
(define emitted-files (make-hash-table))
(define (codept-emit-lib-args fs-path stanza srcfiles out-port)
  ;; (format #t "codept-emit-lib-args: ~A\n" fs-path)
  ;; (format #t "  lib stanza: ~A\n" stanza)
  ;; (let ((modules (assoc-in '(:modules :direct) (cadr stanza))))
  ;;   (format #t "  Stanza: ~A\n" stanza)
  ;;   (format #t "  Modules: ~A\n" modules))

  ;; (if (equal? fs-path "src/lib_protocol_environment/sigs")
  ;;     (format #t " SIGS stanza: ~A\n" stanza))

  (let* ((stanza-alist (cadr stanza))
         (ocaml-srcs (if srcfiles srcfiles '()))
         ;; (ocaml-srcs (if-let ((ocaml-srcs (assoc :ocaml srcfiles)))
         ;;                     (cadr ocaml-srcs)
         ;;                     '()))
         ;; (_ (format #t "Srcs: ~A\n" srcfiles))

         (privname  (cadr (assoc-in '(:name :private) stanza-alist)))

         ;; root_module? - a generated resolver module for 'libraries' deps

         ;; modules_without_implementation - included in default :all,
         ;; must be added in case of (modules...) field, to get namespacing

         ;; private_modules - dune makes these private by hiding their
         ;; cmis? but we still need their deps. tezos has one example,
         ;; lib_rpc_http:RPC_logging.
         ;; FIXME: if no (modules...) then private ones will be
         ;; included by default for codept analysis. but if we have
         ;; (modules...)? do modules and private_modules overlap?

         ;; (private-modules (if-let ((pmodules (assoc 'private_modules
         ;;                                            (cdr stanza))))
         ;;                          (if (null? (cdr pmodules))
         ;;                              '() (cdr pmodules))
         ;;                          '()))

         ;; :direct modules correspond to srcfiles
         ;; (direct-modules (if-let ((direct-modules (assoc-in
         ;;                             '(:modules :direct)
         ;;                             (cadr stanza))))
         ;;             (if (null? direct-modules)
         ;;                 (begin
         ;;                   (display
         ;;                    (format #f "WARNING: empty library: ~A:~A"
         ;;                            fs-path privname)) (newline)
         ;;                    '())
         ;;                 (cadr direct-modules))
         ;;             '()))

         (ms (if-let ((ms (assoc :submodules stanza-alist)))
                     (if (null? (cdr ms))
                         (begin
                           (format #t "WARNING: empty library: ~A:~A\n"
                                   fs-path nm)
                           '())
                         (cadr ms))
                     '()))

         ;; (_ (if (equal? fs-path "src/lib_protocol_environment/sigs")
         ;;        (format #t " SIGS direct-modules: ~A\n" direct-modules)))

         ;; (_ (if (equal? fs-path "src/lib_protocol_environment/s_packer")
         ;;        (format #t " PACKER direct-modules: ~A\n" direct-modules)))

         ;; (modules-without-impl
         ;;  (if-let ((mwis (assoc 'modules_without_implementation
         ;;                        (cdr stanza))))
         ;;          (if (null? (cdr mwis)) '() (cdr mwis))
         ;;          '()))
         )
    ;; (format #t "lib ms: ~A\n" ms)
    (if (null? ms) ;; direct-modules)
        ;; default: all direct-modules (files) in directory
        ;; FIXME: exclude modules_without_implementation?

        (begin
              ;; (display (format #f "MODS: :standard (implicit)")) (newline)
              ;; (display (format #f "SRCS: ~A" srcfiles)) (newline)

              ;; (format out-port "-open\n~A\n" privname)
              (format out-port "~A[" privname)
              (for-each (lambda (f)
                          ;; if f not in modules_without_implementation?
                          (let ((fpath (string-append fs-path "/" f)))
                            (format #t "EMITTING: ~A\n" fpath)
                            (if (not (hash-table-ref emitted-files fpath))
                                (begin
                                  (format out-port "~A"
                                          (string-append fpath ","))
                                  (hash-table-set! emitted-files
                                                   fpath #t)))))
                        ocaml-srcs)
              (format out-port "]")
              (newline out-port))

        ;; else: we have an explicit '(modules...)' list
        (begin
          ;; (format #t "CODEPT path: ~A\n" fs-path)
          ;; ;;(format #t "  Stanza: ~A\n" stanza)
          ;; (format #t "  Modules: ~A\n" direct-modules)

          ;; special case: listed module is generated by another
          ;; target. in that case, (module-name->file-paths) returns
          ;; "". since the file is not generated yet, we can go ahead
          ;; and emit "".

          ;; (format out-port "-open\n~A\n" privname)
          (format out-port "~A[" privname)
          (for-each (lambda (dep-kv)
                      ;; (format #t "dep-kv: ~A\n" dep-kv)
                      ;; (format #t "path: ~A\n"
                      ;;         (module-name->file-paths
                      ;;                  m fs-path ocaml-srcs))
                      ;; (modules) entries may be: sym, (:standard ...) etc.
                      ;; (modules (:standard)) == same as omitting? i.e. all
                      ;; (file-for-module m srcfiles)

                      (let-values
                          (((m1 m2)
                            (module-name->file-paths
                             (car dep-kv) fs-path ocaml-srcs)))
                        ;; (format #t "m1: ~A\n" m1)
                        ;; (format #t "m2: ~A\n" m2)
                        (if m1
                            (if (not (hash-table-ref emitted-files m1))
                                (begin
                                  (format out-port "~A"
                                          (string-append m1 ","))
                                  (hash-table-set! emitted-files
                                                   m1 #t))))
                        (if m2
                            (if (not (hash-table-ref emitted-files m2))
                                (begin
                                  (format out-port "~A"
                                          (string-append m2 ","))
                                  (hash-table-set! emitted-files
                                                   m2 #t))))))

                    ;; "Note that the modules_without_implementation
                    ;; field is not merged in modules...". But we need
                    ;; to analyse their dependencies anyway, so we add
                    ;; them to codept args. The docs do not say
                    ;; whether such "modules" are namespaced; we
                    ;; assume yes.
                    ms) ;; direct-modules)
                    ;; (concatenate modules
                    ;;              modules-without-impl
                    ;;              private-modules))
          (format out-port "]")
          (newline out-port))
        )

    ;; (display (format out-port "   ns: ~A" privname))
    ;; (newline out-port)
    ;; (display (format out-port "   public_name: ~A" public_privname))
    ;; (newline out-port)
    ;; (display (format #f "   modules: ~A" modules))
    ;; (newline)
    )
  )

(define (codept-emit-library-args fs-path lib-stanzas ocaml-srcs out-port)
  ;; (if (equal? fs-path "src/lib_protocol_environment/sigs")
  ;;     (format #t " EMIT sigs stanza: ~A\n" lib-stanzas))
  ;; (format #t "codept-emit-library-args fs-path: ~A\n" fs-path)
  ;; (display (format #f "codept-emit-library-args lib-stanzas: ~A" lib-stanzas))
  (if (not (null? lib-stanzas))
      ;; only emit args for ocaml-srcs
      ;; (if-let ((srcs (assoc :ocaml ocaml-srcs)))
      (if ocaml-srcs
          (for-each (lambda (stanza)
                      (codept-emit-lib-args fs-path stanza
                                            ocaml-srcs out-port))
                  lib-stanzas)
        )
        )
      ;; (begin (display (format #f "  no library stanzas for ~A" fs-path)) (newline)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
(define (dune-emit-codept-args dune-pkg-tbls out-port)
  (format #t "dune-emit-codept-args\n")
  (for-each
   (lambda (dune-pkg-tbl)
     (for-each
      (lambda (pkg-kv)
        (let* ((fs-path (car pkg-kv))
               ;; pkg w/o stanza returns (:stanzas #f):
               (stanzas-assoc (assoc :stanzas (cdr pkg-kv)))
               )
          ;; (format #t "fs-path: ~A\n" fs-path)
          ;; (if (equal? fs-path "src/lib_protocol_environment/sigs")
          ;; (format #t " stanzas: ~A\n" stanzas-assoc)
          ;; )
          (if (cadr stanzas-assoc)
              (let ((stanzas (cadr stanzas-assoc))
                    (ocaml-srcs (if-let ((srcs
                                        (assoc-in '(:srcfiles :ocaml :static)
                                                  (cdr pkg-kv))))
                                      (cadr srcs)
                                      '()))
                    )
          ;; (display (format #f "PATH: ~A" fs-path))
          ;; (newline)

          ;; (display (format #f "stanzas: ~A" stanzas))
          ;; (newline)

          ;; (display (format #f "ocaml-srcs: ~A" ocaml-srcs))
          ;; (newline)

          ;; (display (format #f " libraries: ~A"
          ;;                  (assoc+ :library stanzas)))
          ;; (newline)

          (let ((lib-stanzas (assoc+ :library stanzas)))
            (if (not (null? lib-stanzas))
                (codept-emit-library-args fs-path lib-stanzas ocaml-srcs out-port)))

          (let ((exec-stanzas (assoc+ :executable stanzas)))
            ;; (format #t "ASSOC+ execs: ~A\n" exec-stanzas)
            (if (not (null? exec-stanzas))
                (begin
                  (codept-emit-executable-args fs-path exec-stanzas ocaml-srcs out-port))))

          (let ((test-stanzas (assoc+ :test stanzas)))
            (if (not (null? test-stanzas))
                (codept-emit-executable-args fs-path test-stanzas ocaml-srcs out-port)))

          ;; (let ((execs-stanzas (assoc+ 'executables stanzas)))
          ;;   (if (not (null? execs-stanzas))
          ;;       (codept-emit-executables-args fs-path execs-stanzas ocaml-srcs out-port)))

          ;; (display (format #f " executables: ~A"
          ;;                  (assoc+ :executables stanzas)))
          ;; (newline)
          ;; (display (format #f " alias: ~A"
          ;;                  (assoc+ :alias stanzas)))
          ;; (newline)))

          ;; install stanzas not needed for codept args
          ;; (let ((installs (assoc+ 'install stanzas)))
          ;;   (if (not (null? installs))
          ;;       (for-each (lambda (install)
          ;;                   (display (format #f " install stanzas: ~A: ~A"
          ;;                                    fs-path
          ;;                                    ;;install
          ;;                          (alist-delete 'files (cdr install))
          ;;                                    ))
          ;;                   (newline)
          ;;                   )
          ;;                 installs)))
          ))))
      (cadr dune-pkg-tbl)))
   dune-pkg-tbls)
  '())


;; (require libc.scm)
;; (require r7rs.scm)

;; (require cload.scm)

;; (autoload 'j0
;; 	  (lambda (e)
;; 	    (unless (provided? 'cload.scm)
;; 	      (load "cload.scm"))
;; 	    (c-define '(double j0 (double)) "" "math.h")
;; 	    (varlet e 'j0 j0)))


;; (c-define '(char* getcwd (char* size_t)) "" "unistd.h")

;; (load "libm.scm")
;; (provided? 'libm.scm)

;;;(load "libc.scm")

;;(load "r7rs.scm") ;; let-values

;; data structures

;; wss-tbl:  path => ws-alist
;; ws-alist: ((:name nm) (:root-path path) (:pkgs pkgs-tbl))
;; pkgs-tbl: pkg-path => pkg-alist
;; pkg-alist:   ((:pkg-path path) (:modules modules-tbl))
;; modules-tbl (hash-table): module name => module-alist
;; module-alist: ((:module-name mname)
;;                (:ml filename deplist)
;;                (:mli filename deplist))
;; deplist: vector of pairs [(pkg-path, module-name)]

(define (read-codept-depsfile depsfile)
  (let* ((p (open-input-file depsfile))
         (codept-sexp (read p)))
    (close-input-port p)
    codept-sexp))

(define (codept->version codept-sexp)
  (car codept-sexp))

;; 'dependencies' sexp
;; (dependencies
;;  ( (file src/bin_codec/additional_registrations.ml)
;;    (deps ((Z) (Registration) (Data_encoding))) )
;;  ...)
(define (codept->file-deps codept-sexp)
  (cadr (cadr codept-sexp)))

;; (local
;;   ( (module (Client_protocols_commands))
;;     (ml src/bin_client/client_protocols_commands.ml)
;;     (mli src/bin_client/client_protocols_commands.mli) )
;; ...)
(define (codept->local-modules codept-sexp)
  (cadr (caddr codept-sexp)))

(define (codept->unknown-modules codept-sexp)
  (cadr (cadddr codept-sexp)))

(define (file->deps f deps-list)
  ;; search deps table for (file f) entry
  (let* ((deps (assoc (list 'file f) deps-list))
         (ds (if (pair? (cdr deps))
                 (cadr (cadr deps))
                 '())))
    ds
    )
  )

;; FIXME: predicate for modules in the std distrib?
;; prob: %{lib:stdlib:camlinternalFormatBasics.cmi}
(define opam-root "/Users/gar/.opam")
;; (getenv "OPAMROOT") ;;  ".opam")

(define opam-switch "4.14.0") ;; "tezos")

;; codept-stdlib-cmd analyzes "stdlib" in lib/ocaml
(define codept-stdlib-cmd
  (string-append codept-cmd " -k -sexp "
                 opam-root "/" opam-switch
                 "/lib/ocaml/*.m* 2> /dev/null"))

(system codept-stdlib-cmd #t)

  ;; (string-append "codept -k -sexp "
  ;;                opam-root "/" opam-switch
  ;;                "/lib/ocaml/*.m* 2> /dev/null"))

(define codept-bin-cmd
  (string-append "opam var bin"))

  ;; (string-append
  ;;  "opam var bin --root " opam-root " --switch " opam-switch))

(define (make-stdlib-tbl)
  ;; (format #t "make-stdlib-tbl\n")
  (define stdlib-tbl (make-hash-table))
  (define executables-tbl (make-hash-table))
  (let* ((sexp-str (system codept-stdlib-cmd #t))
         ;; (_ (format #t "sexp-str: ~A\n" sexp-str))
         (codept-stdlib-sexp (with-input-from-string sexp-str read))
         ;; add opam "binaries" (executables)
         (opam-bin-dir (string-right-trim " \n" (system codept-bin-cmd #t)))
         (_ (format #t "bindir: ~A\n" opam-bin-dir))
         (execs (directory->list opam-bin-dir)))

    ;; enumerate modules from codept-stdlib-sexp 'local' assoc
    (for-each (lambda (module)
                ;; module:  ((module (String)) ...)
                ;; (format #t "stdlib module: ~A\n" module)
                (hash-table-set! stdlib-tbl (caadar module) #t)
                )
              (codept->local-modules codept-stdlib-sexp))

    ;; opam executables. dune code may refer to these with prefixes
    ;; ':' or 'bin:', so we add entries for each of these.
    (for-each (lambda (exe)
                (if (and (not (equal? exe "."))
                         (not (equal? exe "..")))
                    (begin
                      (hash-table-set! executables-tbl
                                       (string->symbol exe)
                                       (string-append
                                        "@ocaml//:" exe))
                      ;; with prefix 'bin:'
                      ;; FIXME: we could just drop the prefix for lookup?
                      (hash-table-set! executables-tbl
                                       (string->symbol
                                        (string-append "bin:" exe))
                                       (string-append
                                        ;;FIXME: make it @ocaml//bin: ?
                                        "@ocaml//:" exe))
                      ;; colon pfx, for e.g. (action (run %{ocamlc} ...))
                      ;;FIXME: do we really need this?
                      (hash-table-set! executables-tbl
                                       (string->symbol
                                        (string-append ":" exe))
                                       (string-append
                                        "@ocaml//:" exe))
                      )))
              execs)
    (values stdlib-tbl executables-tbl)))

;; e.g. String, Printf, List, Format, etc.
(define (remove-stdlib-deps deps)
  ;; (format #t "remove-stdlib-deps ~A\n" deps)
  ;; remove stdlib deps
  (if (null? deps)
      '()
      (let recur ((deps deps)
                  (clean '()))
        ;; (format #t "car deps: ~A\n" (caar deps))
        ;; (format #t "car deps stdlib?: ~A\n" (stdlib-tbl (caar deps)))
        (if (null? deps)
            (begin
              ;; (format #t "Done: ~A\n" clean)
              clean)
            (if (> (length (car deps)) 1)
                (recur (cdr deps) (cons (car deps) clean))
                (if-let ((m (stdlib-tbl (caar deps))))
                        (recur (cdr deps) clean)
                        (recur (cdr deps) (cons (car deps) clean))))))))

(define (dune->filedeps-tbl codept-sexp)
  (let* ((deps-alists (codept->file-deps codept-sexp))
         (filedeps-tbl (make-hash-table (length deps-alists))))
    ;; deps-alists: list of alists with keys 'file, 'deps
    (for-each (lambda (dep)
                ;;((file src/foo/bar.ml) (deps ((A B) (C))))
                (let* ((f (assoc 'file dep))
                       (modname (file-name->module-name
                                 (symbol->string (cadr f))))
                       (deps (assoc 'deps dep))
                       (deps (if deps
                                 (remove-stdlib-deps (cadr deps))
                                 '())))
                  ;; (format #t "file:     ~A\n" f)
                  ;; (format #t "filedeps: ~A\n" deps)
                  (hash-table-set! filedeps-tbl
                                   (symbol->string (cadr f))
                                   (if deps
                                       `((:deps ,deps)
                                         (:file ,(symbol->string (cadr f)))
                                         (:module ,modname))
                                       '()))))
              deps-alists)
    filedeps-tbl))

;; (define (codept-srcfile->depslist srcfile codept-sexp)
;;   ;; (format #t "srcfile: ~A\n" srcfile)
;;   ;; FIXME: preproc convert deps-alist to hash-table
;;   (let* ((deps-alist (codept->file-deps codept-sexp)))
;;     (if (equal? srcfile 'src/proto_009_PsFLoren/lib_protocol/alpha_context.ml)
;;         (format #t "alpha_context.ml deps: ~A\n" deps-alist))
;;     (let ((deps (let recur ((deps deps-alist))
;;                  (if (null? deps)
;;                      '()
;;                      (let* ((dep (car deps))
;;                             ;; dep: ((file <path>) (deps ...))
;;                             (f (cadar dep)))
;;                        ;; (format #t "dep-item: ~A\n" dep)
;;                        ;; (format #t "dep-item f: ~A\n" f)
;;                        (if (equal srcfile (symbol->string f))
;;                            (remove-stdlib-deps (cdr dep))
;;                            (recur (cdr deps))))))))
;;       deps)))

(define (filename->openers ns filename)
  ;; (format #t "filename->openers: ~A :: ~A\n" ns filename)
  (let recur ((pkg-tbls dune-pkg-tbls)
              (ct 0))
    (if (null? pkg-tbls)
        :foo
        (begin
          (if (string-prefix? (caar pkg-tbls) filename)
              (let* ((pkg-tbl (cadar pkg-tbls))
                     (pkg (pkg-tbl (-dirname filename)))
                     (stanzas (assoc :stanzas pkg)))
                (let recur2 ((stanzas (cadr stanzas)))
                  (if (null? stanzas)
                      '()
                      (if (equal? :library (caar stanzas))
                          (let*  ((lib (cadar stanzas))
                                 (nm (assoc :name lib)))
                            ;; (format #t "LIB ~A\n" lib)
                            ;; (format #t "NS ~A\n" (car ns))
                            (if (equal? (car ns) (cdadr nm))
                                (begin
                                       ;;(format #t "HIT: ~A\n" nm)
                                  (let ((opens (assoc-in
                                                '(:opts :opens) lib))
                                        (deps (assoc-in
                                               '(:deps :libs) lib))
                                        (isubmods (assoc-in
                                                   '(:modules :indirect) lib)))
                                         ;;(format #t "OPENS: ~A\n" opens)
                                         ;;(if opens (cadr opens) '())
                                         (list opens deps isubmods)))
                                (recur2 (cdr stanzas))))
                          (recur2 (cdr stanzas))))))
              (recur (cdr pkg-tbls) (+ ct 1)))))))

(define (make-module-alist ns module files)
  ;; module: (m1 m2...)
  ;; files: ((ml ...ml) (mli ...mli))
  ;; (let ((maybe-ml (caar files)
  ;;                 (:srcfiles ((:ml "foo.ml") (:mli "foo.mli")))

  (let* ((filename (symbol->string (cadar files)))
         (basename (bname filename))
         (m-alist (list
                   (list :name (car module))
                   (list :bname basename)
                   '(:singleton)
                   ;; (list :opens (reverse (filename->openers ns filename)))
                   (list :nss
                         (list
                          (list :ns ns)
                          (list :pkg-path (-dirname filename))))))
         (m-alist (if (assoc 'ml files)
                      (cons '(:ml) m-alist) m-alist))
         (m-alist (if (assoc 'mli files)
                      (cons '(:mli) m-alist) m-alist)))
    m-alist))

(define (update-dup! modules-tbl module-name filename item ns)
  ;; (format #t "UPDATE-DUP module: ~A\n  ITEM: ~A\n  FILE: ~A\n"
  ;;         module-name item filename)
  (if (assoc :singleton item)
      (let* ((nss (assoc :nss item))
             (new-ns (list (list :ns ns)
                           (list :pkg-path (-dirname filename))))
             (new-nss (cons new-ns (cdr nss))))
             ;; (new-item (cons
             ;;            (list :dup filename)
             ;;            item)))
        ;; (format #t "  new-nss: ~A\n" new-nss)
        ;; (format #t "  old nss: ~A\n" nss)
        (set-cdr! nss new-nss)
        ;; (hash-table-set! modules-tbl module-name new-item))
      ;; :aggregate
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (((module (Client_protocols_commands))
;;   (ml src/bin_client/client_protocols_commands.ml)
;;   (mli src/bin_client/client_protocols_commands.mli))
;;   ... or:
;; ( (module (Traced_sigs Hashtbl))
;;   (ml src/lib_lwt_result_stdlib/traced/sigs/hashtbl.ml)  )
;;  ...)
(define (codept->modules-table codept-sexp)
  (let* ((locals (codept->local-modules codept-sexp))
         (modules-tbl (make-hash-table 1024 equal?)))
    (for-each (lambda (local-module)
                ;; (format #t "local-module: ~A\n" local-module)
                ;; (format #t "  module: ~A\n" (caadar local-module))
                ;; local-module :: ((module (<name>)) (ml ...) (mli ...))

                (let* ((module-path (cadar local-module))
                       (module (caadar local-module))

                       ;; cdr: ((ml ...ml) (mli ...mli))
                       (files (cdr local-module))
                       ;; assumption: file dirname always the same for .ml, .mli
                       (filename (symbol->string (cadar files)))
                       (pos -1))

                  (let recur ((module module-path)
                              (ns '()))
                    (if (null? module)
                        '()
                        (if (null? (cdr module))
                            ;; last segment : type :singleton
                            (if-let ((item (hash-table-ref modules-tbl
                                                           (car module))))
                                    ;; singleton found in 1+ directories
                                    ;; or aggregate
                                    (begin
                                      (update-dup! modules-tbl module
                                                  filename item ns)
                                      ;; (format #t "dup singleton: ~A :: ~A\n"
                                      ;;         (car module) filename)
                                      ;; (hash-table-set! modules-tbl (car module)
                                      ;;                  (cons
                                      ;;                   (list :dup filename)
                                      ;;                   item))
                                      (recur (cdr module) ns))
                                    ;; else new item
                                    (begin
                                      (hash-table-set!
                                       modules-tbl (car module)
                                       (make-module-alist ns module files))
                                      (recur (cdr module) ns)))
                            ;; namespace segment - add to :submodules
                            (if-let ((item (hash-table-ref modules-tbl
                                                           (car module))))
                                    ;; duplicates expected for aggregate modules
                                    (begin
                                      ;; (format #t "ITEM: ~A\n" item)
                                      (alist-update-in!
                                       item
                                       '(:submodules :direct)
                                       (lambda (old)
                                         ;; (format #t "NS: ~A\n  OLD: ~A\n  NEW: ~A\n"
                                         ;;         (car module)
                                         ;;         (if (null? old)
                                         ;;             '() (cdr old))
                                         ;;         (cdr module))
                                         (if (null? old)
                                             "XX" ;;(cadr module)
                                             (if (member (car module) old)
                                                 old ;; (cdr old)
                                                 (cons
                                                  (cadr module)
                                                  (cdr old))))))
                                      (hash-table-set!
                                       modules-tbl (car module)
                                        item)
                                      (recur (cdr module)
                                        (cons (car module) ns))
                                 ;; maybe: counter of occurances, for qa check
                                 ;; (hash-table-set! modules-tbl (car module)
                                 ;;                  (cons filename item))
                                      )
                                    ;; else new item
                                    (let* ((opens_deps_submods
                                            (filename->openers (list (car module)) filename))
                                           (opens (if (car opens_deps_submods)
                                                      (if-let ((opens (cadr (car opens_deps_submods))))
                                                              (reverse opens) '())
                                                      '()))
                                           (deps  (if (cadr opens_deps_submods)
                                                      (if-let ((deps (cadr (cadr opens_deps_submods))))
                                                              (reverse deps) '())))
                                           (isubmods (if (caddr opens_deps_submods)
                                                         (if-let ((isubmods (cadr (caddr opens_deps_submods))))
                                                                 (reverse isubmods) '()))))
                                      (format #t "OPENS_DEPS_SUBMODS: ~A\n"
                                              opens_deps_submods)
                                      (hash-table-set!
                                       modules-tbl (car module)
                                       (list
                                        (list :name (car module))
                                        '(:aggregate)
                                        (list :opens opens)
                                        (list :deps deps)
                                        (list :submodules
                                              (list
                                               (list :direct
                                                    (cadr module))))
                                        (list :pkg-path
                                              (-dirname filename))))
                                      (recur (cdr module)
                                             (cons (car module) ns)))))))

             ;; module names may include '.', '/' ??
             ;; (do ((len (length filename))
             ;;      (i 0 (+ i 1)))
	     ;;     ((= i len))
	     ;;   (if (char=? (filename i) #\/)
	     ;;       (set! pos i)))
             ;; (if (positive? pos)
	     ;;     (list m (substring filename 0 pos)))
             module))
         locals)
    modules-tbl))

;; main
;; DEPRECATED: use codept->modules-tbl
;; (define (codept-sexp->modules-tbl codept-sexp)
;;   (let* ((filedeps-list (codept->file-deps codept-sexp))
;;          (modules-tbl (codept->modules-table))
;;          (pkgs-tbl (make-hash-table))
;;          )
;;     (for-each (lambda (local-module)
;;                   ;; (display (format #f "local: ~A" local-module)) (newline)
;;                   (let* ((m (cadar local-module))  ;; m := (Fuzz)

;;                          (module-name (car m))  ;; Fuzz
;;                          (files (cdr local-module)) ;; ((ml fuzz/fuzz.ml) (mli ...))

;;                          (mli-file (if-let (mlx (assq 'mli files))
;;                                            (cadr mlx)))
;;                          (mli-dirname (if mli-file (dirname mli-file) #f))
;;                          (mli-deps (if mli-file
;;                                        (file->deps mli-file deps-list)
;;                                        #f))

;;                          (ml-file (if-let (mlx (assq 'ml files))
;;                                           (cadr mlx)))
;;                          (ml-dirname (if ml-file (dirname ml-file) #f))
;;                          (ml-deps (if ml-file
;;                                       (file->deps ml-file deps-list)
;;                                       #f))

;;                          ;; NB we don't have to put filedeps in tbl,
;;                          ;; we can just call file->deps as needed
;;                          ;; file-deps combines ml, mli entries as list
;;                          (module-assoc (if mli-file
;;                                            (if (null? mli-deps)
;;                                                (list :mli
;;                                                      (list :file mli-file))
;;                                                (list :mli
;;                                                      (list :file mli-file)
;;                                                      (list :deps mli-deps)))
;;                                            #f))
;;                          ;; (_ (begin (display (format #f "MLI ~A" module-assoc))
;;                          ;;           (newline)))
;;                          (module-assoc (if module-assoc
;;                                            (if ml-file
;;                                                (if ml-deps
;;                                                    (list module-assoc
;;                                                          (list :ml
;;                                                                (list :file
;;                                                                      ml-file)
;;                                                                (list :deps
;;                                                                      ml-deps)))
;;                                                    (list module-assoc
;;                                                          (list :ml
;;                                                                (list :file
;;                                                                      ml-file))))
;;                                                module-assoc)
;;                                            (if ml-deps
;;                                                (list
;;                                                 :ml (list :file ml-file)
;;                                                 (list :deps ml-deps))
;;                                                '())))
;;                          ;; either path will do
;;                          (pkg-path (if ml-dirname ml-dirname mli-dirname))
;;                          )
;;                     ;; (display (format #f "pkgs-tbl: ~A" pkgs-tbl)) (newline)
;;                     (let ((pkg-alist (hash-table-ref pkgs-tbl pkg-path)))
;;                       (if pkg-alist
;;                           (let ((pkg-modules-map
;;                                  (cadr (assq :modules pkg-alist))))
;;                             ;; does pkg-modules-map contain this module?
;;                             (if (not (hash-table-ref pkg-modules-map
;;                                                      module-name))
;;                                 (hash-table-set!
;;                                  pkgs-tbl pkg-path
;;                                  (list
;;                                   (assq :pkg-path pkg-alist)
;;                                   ;; (car pkg-alist)
;;                                   (begin
;;                                     (hash-table-set!
;;                                      pkg-modules-map module-name
;;                                      module-assoc)
;;                                     (list :modules pkg-modules-map))))))
;;                           ;; pkg-path not in pkgs-tbl
;;                           (hash-table-set! pkgs-tbl
;;                                            pkg-path
;;                                            (list
;;                                             ;;(realpath pkg-path)
;;                                             (list :pkg-path pkg-path)
;;                                             (let ((ht (make-hash-table)))
;;                                               (begin (hash-table-set!
;;                                                       ht module-name
;;                                                       module-assoc)
;;                                                      (list :modules
;;                                                            ht)))))))
;;                     ))
;;               (codept->local-modules codept-sexp))
;;     ;; (display (format #f "PKGS-TBL ~A" pkgs-tbl))
;;     ;; (newline)
;;     pkgs-tbl))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;; (display (format #f "pkgs-map ~A" pkgs-map)) (newline)

;;     ;; (let ((keys (map (lambda (e) (car (values e))) modules-table)))
;;     ;;   (for-each (lambda (key)
;;     ;;               (let ((val (hash-table-ref modules-table key)))
;;     ;;                 (display (format #f "~A: ~D" key (length val)))
;;     ;;                 (newline)))
;;     ;;             (sort! keys (lambda (a b)
;;     ;;                           (string<? (symbol->string a)
;;     ;;                                     (symbol->string b))))))
;;     ;; test
;;     ;; (display (format #f "'Fuzz count ~A: ~A"
;;     ;;                    (length (hash-table-ref modules-table 'Fuzz))
;;     ;;                    (hash-table-ref modules-table 'Fuzz)))
;;     ;;   (newline)
;;     ;;   (for-each (lambda (pkg)
;;     ;;               (display (format #f "//~A:~A deps: ~A" pkg "Fuzz"
;;     ;;                                (hash-table-ref
;;     ;;                                 (hash-table-ref pkgs-map pkg)
;;     ;;                                 'Fuzz)))
;;     ;;               (newline))
;;     ;;             (hash-table-ref modules-table 'Fuzz))

;;       ;; (newline)
;;       ;; (display (format #f "modules-table keys: ~A"
;;       ;; )))
;;     ;; (display (format #f "modules table: ~A" modules-table))
;;     ;; (newline)
;;     ;; (display (format #f "modules table count: ~D"
;;     ;;                  (hash-table-entries modules-table)))
;;     ;; (newline)


;; suppose foo/bar is a workspace. then just crawling foo/bar from
;; proj root would use the wrong base dir. passing (foo/bar . ".")
;; will treat foo/bar as base dir. to process just foo/bar/a and
;; foo/bar/b, pass (foo/bar . '("a" "b"))
;; write codept.args, then process to produce codept.deps
(define (path->group-tag path)
  (string-tr path #\/ #\.))

(define* (->codept root . dirs)
  (format #t "->CODEPT root: ~A dirs: ~A\n" root dirs)
  (if (not (null? dirs))
      (if (not (pair? dirs))
          (error 'bad-arg
                 (format #f "second arg to 'crawl' must be a list of dirs\n"))))

  (if (not (file-exists? ".obazl.d/tmp"))
      (system "mkdir -p .obazl.d/tmp"))
  (let ((outp
         (open-output-file ".obazl.d/tmp/codept.args")))

    ;; if no dirs passed, set root to ./ and dirs to root
    (let ((root (if (null? dirs) "." root))
          (dirs (if (null? dirs) (list root) dirs)))

      ;; (format #t "root: ~A, dirs: ~A\n" root dirs)

      (for-each
       (lambda (dir)
         (format #t "root ~A, dir ~A\n" root dir)
         (if (char=? #\/ (string-ref dir 0))
             (error 'wrong-type-arg "paths must be relative"))
         (if (not (directory? dir))
             (error 'wrong-type-arg
                    (string-append "Not a directory: " dir)))

         ;; we will recur on subdirs, so we need a new root
         (let recur-dir ((subroot root)
                         (dir dir))
           ;; (format #t "recurring on ~A/~A\n" subroot dir)
           (if (null? dir)
               (begin
                 ;; this should never happen, we only recur when we
                 ;; have a dir
                 (error 'null-dir (format #f "unexpected null dir\n")))

               (let* ((path
                       (if (equal? subroot ".")
                           dir
                           (string-append subroot "/" dir)))
                      (dirents (sort! (directory->list path) string<?))
                      (files
                       ;; rm ".", "..", "bazel-*", hidden dirs
                       (filter (lambda (dentry)
                                 (not (or (equal? ".." dentry)
                                          (equal? "." dentry)
                                          (string-prefix? "." dentry)
                                          (string-prefix? "bazel-" dentry))))
                               (cddr dirents)))
                      (subdirs (filter (lambda (f)
                                         (directory?
                                          (string-append path "/" f)))
                                       files))
                      (ocaml-srcs (filter (lambda (f)
                                            (or (string-suffix? ".ml" f)
                                                (string-suffix? ".mli" f)))
                                          files))
                      (modules (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".ml" f))
                                         files)))
                      (sigs (map file-name->module-name
                                 (filter (lambda (f)
                                           (string-suffix? ".mli" f))
                                         files)))
                      )
                 ;; (format #t "path: ~A\n" path)
                 ;; (format #t "subdirs: ~A\n" subdirs)
                 (if files
                     (let ((group (if (equal? "." path) "_ROOT"
                                      (path->group-tag path))))
                       (if (not (string-prefix? "testsuite" group))
                           (begin
                             ;; first do all files in dir
                             (if (not (and (null? modules) (null? sigs)))
                                 (let ((fpaths (map (lambda (f)
                                                      (string-append path "/" f))
                                                    ocaml-srcs)))
                                   (format outp "~A[" (string-tr group #\/ #\_))
                                   (format outp "~A"
                                           (string-join fpaths ","))
                                   ;; (for-each (lambda (f)
                                   ;;             (if (or (string-suffix? ".mli" f)
                                   ;;                     (string-suffix? ".ml" f))
                                   ;;                 (format outp "~A/~A," path f)
                                   ;;                 ;; else nop
                                   ;;                 ))
                                   ;;           files)
                                   (format outp "]\n")
                                   )
                                 )
                             ;; then recur in subdirs
                             (for-each (lambda (subdir)
                                         (recur-dir path subdir))
                                       subdirs))
                           ;; skip testsuite subdirs, sth in there chokes codept
                           ))
                     ;; no more files, so no recursion
                     )))))
       ;; for each dir
       (if (null? dirs) (list ".") dirs)))
    (close-output-port outp))

  (format #t "running codept\n")

  ;; now run codept to produce codept.deps
  (system (string-append "codept -expand-deps -sexp -k "
                         "-args .obazl.d/tmp/codept.args "
                         "> .obazl.d/tmp/codept.deps "
                         "2> .obazl.d/tmp/codept.log"))

  ;; and now analyze codept.deps
  (let* ((codept-sexp (read-codept-depsfile ".obazl.d/tmp/codept.deps"))
         (_ (format #t "codept-sexp: ~A\n" codept-sexp))
         (codept-alist (cdr codept-sexp))
         (locals-assoc (assoc 'local codept-alist))
         (locals-alist-list (cadr locals-assoc))

         (deps-alist (cadr (assoc 'dependencies codept-alist)))
         (modules-tbl (make-hash-table))
         (deps-tbl (make-hash-table)))
    ;; each alist in locals-alist-list: keys module, ml, mli
    ;; (format #t "locals-alist-list: ~A\n" locals-alist-list)
    (for-each (lambda (local-dep)
                (let* ((file-assoc (assoc 'file local-dep))
                       (fpath (cadr file-assoc))
                       (deps-assoc (assoc 'deps local-dep))
                       (deps-list (if deps-assoc
                                      (cadr deps-assoc)
                                      '())))
                  (format #t "file-assoc: ~A\n" file-assoc)
                  (format #t "fpath: ~A\n" fpath)
                  (format #t "deps-assoc: ~A\n" deps-assoc)
                  (format #t "deps-list: ~A\n" deps-list)
                  (if-let ((e (hash-table-ref modules-tbl fpath)))
                          (begin ;; update old entry
                            (if (list? e)
                                (hash-table-set! modules-tbl fpath
                                                 (cons deps-list e))
                                (hash-table-set! modules-tbl fpath
                                                 (list deps-list e))))
                          ;; else new entry
                          (begin
                            ;; (format #t "adding ~A\n" mname)
                            (hash-table-set! modules-tbl fpath
                                             deps-list)))))

              deps-alist)
    modules-tbl))

;; (define modules-tbl (->codept "."))

;; (for-each (lambda (e)
;;             (format #t "~A\n" e))
;;           modules-tbl)

;; (hash-table-entries mtbl)

;; (for-each (lambda (local-alist)
;;                 (let* ((module-assoc (assoc 'module local-alist))
;;                        (module (cadr module-assoc))
;;                        (ml (assoc 'ml local-alist))
;;                        (mli (assoc 'mli local-alist))
;;                        (mname (if (pair? module)
;;                                   (car (reverse module))
;;                                   module))
;;                        (mpkg (if (pair? module)
;;                                  (reverse (cdr (reverse module)))
;;                                  #f)))
;;                   (format #t "module-assoc: ~A\n" module-assoc)
;;                   (format #t "module: ~A\n" module)
;;                   (format #t "mname: ~A, mpkg: ~A\n" mname mpkg)
;;                   (if-let ((e (hash-table-ref modules-tbl mname)))
;;                           (begin ;; update old entry
;;                             (if (list? e)
;;                                 (hash-table-set! modules-tbl mname
;;                                                  (cons mpkg e))
;;                                 (hash-table-set! modules-tbl mname
;;                                                  (list mpkg e))))
;;                           ;; else new entry
;;                           (begin
;;                             ;; (format #t "adding ~A\n" mname)
;;                             (hash-table-set! modules-tbl mname
;;                                              (list mpkg))))))
;;               deps-alist)
;
