;; (display "dune/dune_stanzas.scm loading ...") (newline)

;; (load "dune_stanza_executable.scm")
;; (load "dune_stanza_library.scm")
;; (load "dune_stanza_rule.scm")
;; (load "resolve_fs_refs.scm")
;; (load "utils.scm")

;;;;;;;;;;;;;;;;
;; copy-files options:
;; (alias <alias-name>), (mode <mode>), (enabled_if <blang expression>)
;; (copy_files <glob>) is equivalent to (copy_files (files <glob>))
(define (normalize-stanza-copy_files pkg-path stanza)
  (format #t "NORMALIZE-STANZA-COPY_FILES, path: ~A\n" pkg-path)
  (format #t "  stanza: ~A\n" stanza)
  ;; handle both copy_files and copy_files#
  ;; (copy_files
  ;;  <optional-fields>
  ;;  (files <glob>))
  ;; <optional-fields> are:
  ;; (alias <alias-name>) to specify an alias to which to attach the targets.
  ;; (mode <mode>) to specify how to handle the targets
  ;; (enabled_if <blang expression>)

  ;; The short form (copy_files <glob>)
  ;; is equivalent to (copy_files (files <glob>))

  ;; TODO: parse glob
  ;; glob: https://dune.readthedocs.io/en/stable/concepts.html#glob
  ;; glob :: path/pattern, path is optional

  ;; e.g.
  ;; (copy_files# byte/*.ml)
  ;; (copy_files bindings/{rustzcash_ctypes_c_stubs.c,rustzcash_ctypes_stubs.ml,rustzcash_ctypes_bindings.ml})

  (let ((op (car stanza))
        (args (cdr stanza)))
    (if (equal? op 'copy_files)
        (begin
          (let ((result (if (pair? (car args))
                            stanza
                            (list :copy-files
                                  (list (list 'files args))))))
            ;; (display (format #f "norm result: ~A" result)) (newline)
            result)
          )
        (if (equal? op 'copy_files#)
            (list :copy-files# (resolve-files pkg-path args))
            (error 'bad-arg
                   (format #f "unexpected stanza type: ~A\n" stanza))))))

(define (normalize-stanza-copy pkg-path stanza)
  (format #t "NORMALIZE-STANZA-COPY: ~A" stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (copy_files
  ;;  <optional-fields>
  ;;  (files <glob>))
  ;; <optional-fields> are:
  ;; (alias <alias-name>) to specify an alias to which to attach the targets.
  ;; (mode <mode>) to specify how to handle the targets
  ;; (enabled_if <blang expression>)

  ;; The short form (copy_files <glob>)
  ;; is equivalent to (copy_files (files <glob>))

  ;; TODO: parse glob
  ;; glob: https://dune.readthedocs.io/en/stable/concepts.html#glob
  ;; glob :: path/pattern, path is optional

  ;; e.g.
  ;; (copy_files bindings/{rustzcash_ctypes_c_stubs.c,rustzcash_ctypes_stubs.ml,rustzcash_ctypes_bindings.ml})

  (let ((result (if (pair? (cadr stanza))
                    stanza
                    (list (car stanza)
                          (list (list 'files (cdr stanza)))))))
    ;; (display (format #f "norm result: ~A" result)) (newline)
    result)
  )

;; ocamllex
;; (ocamllex <names>) is essentially a shorthand for:
;; (rule
;;  (target <name>.ml)
;;  (deps   <name>.mll)
;;  (action (chdir %{workspace_root}
;;           (run %{bin:ocamllex} -q -o %{target} %{deps}))))
;; To use a different rule mode, use the long form:
;; (ocamllex
;;  (modules <names>)
;;  (mode    <mode>))
;; e.g. (ocamllex point_parser), (ocamllex (point_parser))
;; (ocamllex (foo bar)), (ocamllex foo bar)
;; also possible: (ocamllex foo (bar baz))?
;; (ocamllex
;;  (modules lexer_impl))

(define (normalize-stanza-ocamllex stanza)
  ;; (format #t "normalize-stanza-ocamllex: ~A\n" stanza)
  (let recur ((modules (cdr stanza))
              (result '()))
    (if (null? modules)
        `(:ocamllex ,result)
        (if (pair? (car modules))
            ;; e.g. (modules lexer_impl), (foo bar), etc.
            (recur (cdr modules)
                   (concatenate
                    (let recur2 ((modules2 (car modules))
                                 (result2 '()))
                      (if (null? modules2)
                          result2
                          (if (equal? 'modules (car modules2))
                              (recur2 (cdr modules2) result2)
                              (recur2 (cdr modules2)
                                      (cons (car modules2) result2)))))
                    result))
            ;; else singletons
            (recur (cdr modules) (cons (car modules) result))))))

(define (normalize-stanza-ocamlyacc stanza)
  ;; (format #t "normalize-stanza-ocamlyacc: ~A\n" stanza)
  (let recur ((modules (cdr stanza))
              (result '()))
    (if (null? modules)
        `(:ocamlyacc ,result)
        (if (pair? (car modules))
            (recur (cdr modules)
                   (concatenate
                    (let recur2 ((modules2 (car modules))
                                 (result2 '()))
                      (if (null? modules2)
                          result2
                          (if (equal? 'modules (car modules2))
                              (recur2 (cdr modules2) result2)
                              (recur2 (cdr modules2)
                                      (cons (car modules2) result2)))))
                    result))
            ;; else singletons
            (recur (cdr modules) (cons (car modules) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (normalize-stanza-alias stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-alias: ~A" stanza)) (newline)
  (cons 'alias (list (cdr stanza))))

(define (normalize-stanza-data_only_dirs stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'data_only_dirs
        (list (list (cons :dirs (cdr stanza))))))

(define (normalize-stanza-env stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'env (list (cdr stanza))))

;; (define (update-public-exe-table pkg-path pubname filename)
;;   ;; (format #t "update-public-exe-table: ~A => ~A/~A\n"
;;   ;;         pubname pkg-path filename)
;;   (let* ((pubname (symbol->string pubname))

;;          ;; FIXME: only for executables
;;          (target-label (string-append "//" pkg-path ":"
;;                                       (symbol->string filename)
;;                                       ".exe")))

;;     (hash-table-set! public-exe->label
;;                      (string->symbol pubname) target-label)
;;     (hash-table-set! public-exe->label
;;                      (string->symbol (string-append pubname ".exe"))
;;                      target-label)
;;     (hash-table-set! public-exe->label
;;                      (string->symbol (string-append pubname ".bc"))
;;                      target-label)

;;     (let recur ((path-segs (reverse (string-split pkg-path #\/)))
;;                 (pfx ""))
;;       ;; (format #t "path-segs: ~A\n" path-segs)
;;       (if (null? path-segs)
;;           '()
;;           (let* ((pfx (string-append (car path-segs) "/" pfx))
;;                  (k (string-append pfx pubname)))
;;             ;; (format #t " k: ~A;  pfx:   ~A\n" k pfx)
;;             ;; (format #t " target label: ~A\n" target-label)
;;             (hash-table-set! public-exe->label
;;                              (string->symbol k) target-label)
;;             (hash-table-set! public-exe->label
;;                              (string->symbol
;;                               (string-append k ".exe"))
;;                              target-label)
;;             (hash-table-set! public-exe->label
;;                              (string->symbol
;;                               (string-append k ".bc"))
;;                              target-label)
;;             (recur (cdr path-segs) pfx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contingent dep example:
;; (select void_for_linking-alpha-protocol-plugin from
;;   (tezos-protocol-plugin-alpha -> void_for_linking-alpha-protocol-plugin.empty)
;;   (-> void_for_linking-alpha-protocol-plugin.empty))

(define (normalize-lib-select select)
  ;; (format #t "normalize-lib-select: ~A\n" select)
  ;; select: (select main from (b -> c)... (-> d))
  (let ((select-file (cadr select))
        (clauses (cdddr select)))
    ;; if clause RHSs are equal, then reason for select is to force
    ;; link of LHS lib

    ;; in each clause, LHS is both a lib and the select condition. we
    ;; map it to a config setting, so the result is: if config-setting
    ;; then add LHS lib to deps. so the user passes config settings,
    ;; selecting modules to link - build profiles (platforms)

    ;; the problem: need to resolve RHS to a bazel label. lacking
    ;; namespaces our only option is a lookup table.

    ;; e.g.
    ;; platform(
    ;; name = "std_protocols",
    ;; constraint_values = [
    ;;     ":proto_a",
    ;;     ":proto_b",
    ;;     ":proto_c",
    ;; ])
    ;; then a config_setting with these constraing values
    ;; then select on config_setting

    ;; bug dune select also produces a file target - the clausal RHS
    ;; copied to the main selected file. to support that, we need to
    ;; generate an ocaml_module target for the main select file, and
    ;; add it to the submodules list. the ocaml_module target must
    ;; select its 'struct' (src) attr using the select logic here.

    ;; so to convert a 'libraries' fld with selects, we should have
    ;; one condition per select, which means one config/constraint
    ;; setting per select. output will look like:
    ;; select({ "<LHSa-config-label>": ["<LHSa-tgt>", "<main-filea>"],
    ;;          "//condition:default": ["<main-filea>"]
    ;; })
    ;; + select({ "<LHSb-config-label>": ["<LHSb-label>", "<main-fileb>"]})
    ;;    ...})

    ;; normal form:
    ;; (:select
    ;;   ((:target foo)
    ;;    (:selectors ((LHS-labela RHS-labela)
    ;;                 (LHS-labelb RHS-labelb)
    ;;                 ...))
    ;;    (:default default-file)))

    ;; BUT: do we need to normalize? we cannot yet resolve LHS lib anyway.

    ;; (format #t "SELECT file: ~A\n" select-file)
    ;; (format #t "CLAUSES: ~A\n" clauses)
    ;; (let ((conditionals
    ;;        (map (lambda (clause)
    ;;               ;; last clause is (-> x), all others are (x -> y)
    ;;               (if (equal? '-> (car clause))
    ;;                   ;; last clause
    ;;                   )
    ;;               (let* ((lhs (car clause))
    ;;                      (rhs (caddr clause))
    ;;                      ;; names-tbl not yet built, emitter must do this:
    ;;                      ;;(label (assoc :label (names-tbl lhs)))
    ;;                      )
    ;;                 (if (not (equal? '-> (cadr clause)))
    ;;                     (error 'bad-arg' "ERROR: missing expected '->'"))
    ;;                 (list
    ;;                  (string-append "//config/" (symbol->string lhs))
    ;;                  (list (string-append "LABEL:" (symbol->string lhs))))))
    ;;             clauses))))
      select))

(define (normalize-exe-libraries libs-assoc stanza-alist)
  ;; (format #t "normalize-exe-libraries: ~A\n" libs-assoc)
  (let-values (((constant contingent)
                (let recur ((libs (cdr libs-assoc))
                            (constants '())
                            (contingents '()))
                  (if (null? libs)
                      (values constants contingents)
                      (if (list? (car libs))
                          (recur (cdr libs)
                                 constants
                                 (cons
                                  (if (equal? 'select (caar libs))
                                      (normalize-lib-select (car libs))
                                      (car libs))
                                  contingents))
                          (recur (cdr libs)
                                 (cons (car libs) constants)
                                 contingents))))))
    ;;(let ((contingent 
    ;; (format #t "constant deps: ~A\n" constant)
    ;; (format #t "contingent deps: ~A\n" contingent)
    `(:deps
      ((:constant ,(sort! constant sym<?))
       ,(cons :contingent contingent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (install
;;  (files hello.txt) ;; required field?
;;  (section share)  ;; required field?
;;  (package mypackage)) ;; optional field
;;
;; in src/proto_demo_noops/lib_protocol/dune:
;; (install
;;  (section lib)
;;  (package tezos-protocol-demo-noops)
;;  (files (TEZOS_PROTOCOL as raw/TEZOS_PROTOCOL)))
;; mapping: %{lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL}
;;          => //src/proto_demo_noops/lib_protocol:TEZOS_PROTOCOL

(define (normalize-exe-fname exefile)
  (let ((exe-fname (if (symbol? exefile)
                       (symbol->string exefile)
                       exefile)))
    ;; (format #t "exe-fname ~A\n" exe-fname)
    (if (string-suffix? ".exe" exe-fname)
        (let ((newexe (string-drop-right exe-fname 3)))
          (string->symbol
           (string-append newexe "exe")))
        (string->symbol exe-fname))))

;; (files (tezos-init-sandboxed-client.sh as tezos-init-sandboxed-client.sh))
;; (files (replace.exe as replace)
;;        dune_protocol dune_protocol.template final_protocol_versions)
;; (files foo)
;; (files (foo as a) (bar as b))
;; i.e. (files (file-name as link-name))
;; so we return a list of pairs (link-name file-name)
(define (normalize-install-files files)
  ;; (format #t "normalize-install-files: ~A\n" files)
  (let recur ((entries files)
              (links '())
              (files '()))
    (if (null? entries)
        (values links files)
        (if (list? (car entries))
            (if (equal? 3 (length (car entries)))
                (begin
                  ;; (format #t "LEN3: ~A\n" (car entries))
                (if (equal 'as (cadr (car entries)))
                    (begin
                      ;; e.g. (s_packer.exe as s_packer)
                      (recur (cdr entries)
                             (cons (caddr (car entries)) links) ;; s_packer
                             (cons (normalize-exe-fname
                                    (caar entries))  ;; s_packer.exe
                                   files)))
                    ;; else list of links == files
                    (recur (cdr entries)
                           (append (car entries) links)
                           (append (car entries) files))))
                ;; else list != 3, list of links == files
                (recur (cdr entries)
                           (append (car entries) links)
                           (append (car entries) files)))
            ;; else not a list
            (recur (cdr entries)
                   (cons (car entries) links)
                   (cons (car entries) files))))))

;; dune 'install' stanzas make executables available for use by other
;; stanzas; in bazel, labels eliminate the need for this, so install
;; stanzas just register mappings from installed name to label.
(define (normalize-stanza-install pkg-path
                                  ;; dune-project-stanzas
                                  stanza)
  ;; (format #t "normalize-stanza-install: ~A\n  ~A\n" pkg-path stanza)
  ;; (format #t "    dune-project: ~A\n" dune-project-stanzas)
  (let* ((stanza-alist (cdr stanza))
         (section (cadr (assoc 'section stanza-alist)))
         (dune-project-stanzas (read-dune-project-file pkg-path))
         (package (if-let ((pkg (assoc 'package stanza-alist)))
                          (cadr pkg)
                          (if-let ((pkg-name
                                    (assoc 'name dune-project-stanzas)))
                                  (cadr pkg-name)
                                  #f))))
    (let-values (((link-names file-names)
                  (normalize-install-files
                   (cdr (assoc 'files stanza-alist)))))
      ;; (format #t "link-names: ~A\n" link-names)
      ;; (format #t "file-names: ~A\n" file-names)

      (for-each (lambda (link-name file-name)
                  (update-executables-table
                   (string->symbol
                    (string-append (symbol->string section)
                                   ":" (symbol->string package)
                                   ":" (symbol->string link-name)))
                   (string->symbol
                    (string-append "//" pkg-path
                                   ":" (symbol->string file-name))))
                  )
                link-names file-names)

      ;; (format #t "dune pkg name: ~A\n" package)
      (cons :install (list (cdr stanza))))))

(define (normalize-stanza-test pkg-path srcfiles stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-test: ~A" stanza)) (newline)
  (let* ((stanza-alist (cdr stanza))
         (privname (assoc 'name stanza-alist)))
    (list (car stanza)
          (remove
           '()
           (map (lambda (fld-assoc)
                  (case (car fld-assoc)
                    ((name) (normalize-stanza-fld-name
                             pkg-path privname stanza-alist))
                    ((public_name) '())
                    ;; ((name) (normalize-stanza-fld-name (cadr fld-assoc)))
                    ((flags) (normalize-stanza-fld-flags (cadr fld-assoc)))
                    ((foreign_stubs)
                     (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))
                    (else fld-assoc)))
                (cdr stanza))))))

(define (normalize-stanza-tests pkg-path ocaml-srcs stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-tests: ~A" stanza)) (newline)
  (normalize-stanza-executables :tests
                                pkg-path ocaml-srcs stanza))

;; moved to normalize.scm (dune-stanza->mibl pkg stanza)
(define (Xnormalize-stanza pkg-path
                               ;;dune-project-stanzas
                               srcfiles
                               stanza)
  (format #t "NORMALIZE-STANZA: ~A\n" stanza)
  (format #t "    srcfiles: ~A\n" srcfiles)
  ;; dune-project-stanzas: sexp from (local) 'dune-project' file
  ;;    FIXME: used only for install stanzas, no need to read otherwise
  ;; stanza: (<stanza type> (flda ...) (fldb ...) ...)
  ;; (newline)
  ;; (format #t "raw: ~A\n" stanza)
  ;; (if (equal? "src/lib_client_base" (cadr (assoc :pkg-path (cdr stanza))))
  ;;     (format #t "dune-stanza->mibl: ~A" srcfiles))
  ;; (format #t "dune-stanza->mibl stanza: ~A\n" stanza)
  ;; (format #t "dune-stanza->mibl srcs: ~A\n" srcfiles)
  ;; (if (null? srcfiles)
  ;;     (format #t "pkg: ~A\n" pkg-path))

  (let* ((ocaml-srcs (if (null? srcfiles)
                         '()
                         (if-let ((srcs (assoc-in '(:ocaml :static)
                                                  (cadr srcfiles))))
                                 (cadr srcs) '())))
         (ocaml-srcs (if (null? srcfiles)
                         '()
                         (if-let ((srcs (assoc-in '(:ocaml :generated)
                                                  (cadr srcfiles))))
                                 (concatenate ocaml-srcs (cadr srcs))
                                 ocaml-srcs)))
         (_ (format #t "OCAML SRCS: ~A\n" (reverse ocaml-srcs)))
         ;; (normalized-stanza
         (s (case (car stanza)
              ((alias) (normalize-stanza-alias stanza))
              ((copy_files#) (normalize-stanza-copy_files pkg-path stanza))
              ((copy_files) (normalize-stanza-copy_files pkg-path stanza))
              ((copy#) (normalize-stanza-copy pkg-path stanza))
              ((copy) (normalize-stanza-copy pkg-path stanza))
              ((data_only_dirs) (normalize-stanza-data_only_dirs stanza))
              ((env) (normalize-stanza-env stanza))
              ((executable) (normalize-stanza-executable :executable
                             pkg-path ocaml-srcs stanza))

              ((executables) (normalize-stanza-executables :executables
                              pkg-path ocaml-srcs stanza))

              ((install) (normalize-stanza-install
                          pkg-path
                          ;;dune-project-stanzas
                          stanza))

              ((library)
               (if (null-library? stanza)
                   '()
                   (normalize-stanza-library pkg-path ocaml-srcs stanza)))

              ((ocamllex) (normalize-stanza-ocamllex stanza))

              ((ocamlyacc) (normalize-stanza-ocamllex stanza))

              ((rule) (normalize-stanza-rule
                       pkg-path ocaml-srcs stanza))
              ((test) (normalize-stanza-test pkg-path ocaml-srcs stanza))
              ((tests) (normalize-stanza-tests pkg-path ocaml-srcs stanza))

              ((:dune-project) stanza)

              (else
               (format #t "dune-stanza->mibl unhandled: ~A\n" stanza)))))

    ;; update global public -> private name table
    ;; (case (car stanza)
    ;;   ((executables)
    ;;    (begin))
    ;;   ((library)
    ;;    (begin
    ;;       (let* ((private-name (assoc-in '(:name :private) (cadr s)))
    ;;              (public-name  (assoc      :public_name   (cadr s))))
    ;;         (if (and private-name public-name)
    ;;             (begin
    ;;               ;; (format #t "writing ~A => ~A\n" private-name public-name)
    ;;               (hash-table-set! private-name->public-name
    ;;                                (cadr private-name)
    ;;                                (cadr public-name)))))
    ;;       ))
    ;;   )

    ;; return normalized stanza
    s))

;; (display "loaded dune/dune_stanzas.scm") (newline)
