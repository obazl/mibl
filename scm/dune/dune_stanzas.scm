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
  (if *mibl-debug-all*
      (begin
        (format #t "NORMALIZE-STANZA-COPY_FILES, path: ~A\n" pkg-path)
        (format #t "  stanza: ~A\n" stanza)))
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
  (if *mibl-debug-all*
      (format #t "NORMALIZE-STANZA-COPY: ~A" stanza))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (normalize-stanza-alias stanza)
;;   ;; (display (format #f "dir: ~A" pfx)) (newline)
;;   ;; (display (format #f "normalize-stanza-alias: ~A" stanza)) (newline)
;;   (cons 'alias (list (cdr stanza))))

(define (normalize-stanza-data_only_dirs stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'data_only_dirs
        (list (list (cons :dirs (cdr stanza))))))

(define (normalize-stanza-env stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-env: ~A" stanza)) (newline)
  (cons 'env (list (cdr stanza))))

;; (define (update-public-exe-table pkg-path findlib-name filename)
;;   ;; (format #t "update-public-exe-table: ~A => ~A/~A\n"
;;   ;;         findlib-name pkg-path filename)
;;   (let* ((findlib-name (symbol->string findlib-name))

;;          ;; FIXME: only for executables
;;          (target-label (string-append "//" pkg-path ":"
;;                                       (symbol->string filename)
;;                                       ".exe")))

;;     (hash-table-set! public-exe->label
;;                      (string->symbol findlib-name) target-label)
;;     (hash-table-set! public-exe->label
;;                      (string->symbol (string-append findlib-name ".exe"))
;;                      target-label)
;;     (hash-table-set! public-exe->label
;;                      (string->symbol (string-append findlib-name ".bc"))
;;                      target-label)

;;     (let recur ((path-segs (reverse (string-split pkg-path #\/)))
;;                 (pfx ""))
;;       ;; (format #t "path-segs: ~A\n" path-segs)
;;       (if (null? path-segs)
;;           '()
;;           (let* ((pfx (string-append (car path-segs) "/" pfx))
;;                  (k (string-append pfx findlib-name)))
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

;; wtf? what is an exe-library?
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
    `(:dePs
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace e.g. :rule by :write-file, :ocamlc, :node, etc.
;; depending on action tool
;; :executable by :test if deps include unit test pkg
(define (dune-stanzas->mibl pkg)
  (mibl-trace-entry "mibl-stanzas->mibl" pkg)

  (set! -sh-test-id 0)

  (if-let ((dune-pkg (assoc :mibl pkg)))
          (for-each
           (lambda (stanza)
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (magenta "stanza") stanza))
             ;; first do write-file etc.
             (case (car stanza)
               ((:rule)
                ;; if multiple cmds (progn) do not miblarkize
                (if *mibl-debug-all*
                    (format #t "~A: ~A~%" (red "cmd ct:")
                        (length (assoc :cmd (cdr stanza)))))
                ;; (assoc-in* '(:actions :cmd) (cdr stanza)))))
                (if (< (length (assoc :cmd (cdr stanza))) 2) ;; (assoc-in* '(:actions :cmd) (cdr stanza))) 2)
                    (let ((tool (assoc-in '(:cmd :tool) (cdr stanza)))) ;; (assoc-in '(:actions :cmd :tool) (cdr stanza))))
                      (if *mibl-debug-all*
                          (format #t "~A: ~A~%" (green "tool") tool))
                      (if tool
                          (let ((tool (cadr tool)))
                            (if *mibl-debug-all*
                                (format #t "~A: ~A~%" (green "tool") tool))
                            (case tool
                              ((:write-file) ;;FIXME: what if we have write-file in an alias rule?
                               (if *mibl-debug-all*
                                   (format #t "~A: ~A~%" (red "miblarking") stanza))
                               (set-car! stanza :write-file))

                              ((::cmp) (set-car! stanza :bindiff-test))
                              ((::diff) (set-car! stanza :diff-test))

                              ((:cppo) (set-car! stanza :cppo))

                              ((::ocamlc)
                               (if-let ((deps (assoc :deps (cdr stanza))))
                                       (set-car! deps :srcs))
                               (set-car! stanza :ocamlc))

                              ((::diff) (set-car! stanza :diff))
                              ((::node) (set-car! stanza :node))
                              (else ;; nop
                               '())))
                          ))))

               ((:ns-archive)
                ;; convert to archive if only one submodule
                ;; AND submodule name = ns name
                (if (= (length (cdr (assoc-in '(:manifest :modules) (cdr stanza))))
                       1)
                    (let ((mname (cdr (assoc-in '(:manifest :modules) (cdr stanza))))
                          (ns (assoc-val :ns (cdr stanza))))
                      (if (equal? mname ns)
                          (begin
                            (set-car! stanza :archive)
                            (set-cdr! stanza (dissoc '(:ns) (cdr stanza))))))))

               ((:executable)
                (if *mibl-debug-all*
                    (format #t "~A: ~A~%" (uwhite "miblarkizing executable") (car stanza)))
                (let* ((stanza-alist (cdr stanza))
                       (compile-deps (assoc-in '(:compile :deps :resolved) stanza-alist))
                       (prologue (assoc :prologue stanza-alist)))
                  (if *mibl-debug-all*
                      (begin
                        (format #t "~A: ~A~%" (uwhite "compile-deps") compile-deps)
                        (format #t "~A: ~A~%" (uwhite "prologue") prologue)))
                  (if compile-deps
                      (let ((test? (find-if (lambda (dep)
                                              (member dep unit-test-pkgs))
                                            (cdr compile-deps))))
                        (if test? (set-car! stanza :test) #f)))
                  ;; (if (truthy? prologue)
                  ;;     (update-pkg-prologues! prologue dune-pkg stanza-alist))
                  ))
               (else
                ))
             ;; aliases
             ;; (if (alist? (cdr stanza))
             ;;     (if (assoc :alias (cdr stanza))
             ;;         (-alias-args->miblark pkg stanza)))
             )
           (cdr dune-pkg))
      ;; else no dune file
      ))

;; renamed from (define (miblarkize ws)
;; convert dune stanza names to mibl keywords
(define (dune-stanzas->mibl-keywords ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *mibl-debug-all*
                    (format #t "~A: ~A~%" (blue "dune-pkg->miblx") kv))
                ;; dir may have dune-project but no dune file:
                (if (not (null? (cdr kv)))
                    ;; (dune-pkg->miblx ws (cdr kv)))
                    (dune-stanzas->mibl (cdr kv)))
                )
              pkgs)))


;; (display "loaded dune/dune_stanzas.scm") (newline)
