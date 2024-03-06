(define (-lib->miblx lib)
  (mibl-trace-entry "-lib->miblx" stanza)
  (format #t "LIB: ~A~%" lib)
  (let ((name (car lib)))
        (cons (cons :name name) (cdr lib))))
    ;; (if (eq? name :default)
    ;;     (cons '(:name . :DEFAULT) (cdr lib))

(define (mibl-libraries->miblx ws pkg stanza)
  (mibl-trace-entry "mibl-libraries->miblx" stanza)
  (format #t "pkg: ~A~%" pkg)
  (let ((libs-list
         (map
          (lambda (lib)
            (format #t "LIB STANZA: ~A\n" lib)
            (if (eq? (car lib) :default)
                (let* ((rp (assoc-val :realpath pkg))
                       (n (string->symbol (basename rp))))
                  (set! (car lib) n)))
            (cons :library
                  (-lib->miblx lib)))
            ;; (let ((normed (mibl-stanza->miblx ws
            ;;                                   pkg-alist+ stanza-assoc mibl-stanzas)))
            ;;   ;; pkg-path
            ;;   ;; ;; dune-project-stanzas
            ;;   ;; srcfiles ;; s/b '() ??
            ;;   ;; stanza)))
            ;;   ;; (format #t "NORMALIZED: ~A\n" normed)
            ;;   normed))
          ;; (cdr dune-stanzas))))
          (cdr stanza))))
    libs-list))
  ;; NB: "wrapped" refers to namespacing, not archiving

(define (xxxx-libraries->miblx ws pkg stanza)
  ;; add lib names to exports table
  (let* ((pkg-path (assoc-val :pkg-path pkg))
        (exports (car (assoc-val :exports (assoc-val ws *mibl-project*))))
        (privname (if-let ((privname (assoc-val 'name (cdr stanza))))
                          (car privname) #f))
        (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (uwhite "privname") privname)))
        (findlib-name (if-let ((findlib-name (assoc-val 'public_name (cdr stanza))))
                         (car findlib-name) #f))
        (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (uwhite "findlib-name") findlib-name)))
        )
    ;; libs may be referenced w/o ns, e.g. mylib,
    ;; or (in rule actions) w/ns, e.g. lib:mylib
    ;; we register both pub and priv names just to make sure refs are resolved
    ;; and we register both with and without lib: tag

    (if privname
        (begin
          (if *mibl-debug-all*
              (format #t "~A: ~A~%" (ucyan "lib:adding privname to exports") privname))
          (update-exports-table! ws #f
                                 (assoc-val 'modes (cdr stanza))
                                 ;; (string->symbol (format #f "~A" privname))
                                 privname
                                 pkg-path privname)
          ;; (update-exports-table! ws :lib privname
          ;;                        pkg-path privname)
          ))

    (if findlib-name
        (begin
          (if *mibl-debug-all*
              (format #t "~A: ~A~%" (ucyan "lib:adding findlib-name to exports") findlib-name))
          (update-exports-table! ws #f
                                 (assoc-val 'modes (cdr stanza))
                                 ;; (string->symbol (format #f "~A" findlib-name))
                                 findlib-name
                                 pkg-path privname)
          (update-exports-table! ws :lib (assoc-val :modes (cdr stanza))
                                 findlib-name pkg-path privname)

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
                              findlib-name ;; opam pkg name
                              findlib-name ;; lib name
                              pkg-path
                              privname ;; lib name
                              )))

    (let* ((stanza-alist (cdr stanza))
           (stanza-alist (if-let ((mods (assoc 'modules stanza-alist)))
                                 stanza-alist
                                 (append stanza-alist
                                         (list '(modules :standard)))))
           (_ (if *mibl-debug-all* (format #t "lib stanza alist: ~A\n" stanza-alist)))

           ;; (privname (assoc-val 'name stanza-alist))
           (wrapped? (if-let ((wrapped (assoc-val 'wrapped stanza-alist)))
                             (if (equal? 'false (car wrapped))
                                 #f
                                 #t)
                             #t))
           ;; (submods (lib-stanza-submodules stanza-alist))
           ;; (stanza-alist (cons submods stanza-alist))
           ;; (_ (if *mibl-debug-all* (format #t "STANZA ALIST + SUBMODS: ~A\n" stanza-alist)))

           ;; CONVERT THE STANZA:
           (mibl-stanza (-lib-flds->mibl ws pkg stanza-alist wrapped?))
           (mibl-trace-let "mibl-stanza" mibl-stanza)
           (mibl-stanza (filter (lambda (fld)
                                  ;; remove empties e.g. (:deps)
                                  (mibl-trace "fld" fld)
                                  (not (null? (cdr fld))))
                                mibl-stanza))

           (kind (if wrapped?
                     (if *mibl-wrapped-libs-to-ns-archives*
                         :ns-archive :ns-library)
                     (if *mibl-unwrapped-libs-to-archives*
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



;; updates mibl-stanzas
(define (mibl-stanza->miblx ws pkg-alist stanza-assoc mibl-stanzas)
  (if *mibl-debug-all*
      (begin
        (format #t "~A\n" (ublue "dune-stanza->miblx"))
        (format #t "~A: ~A\n" (blue "stanza-assoc") stanza-assoc)
        (format #t "~A: ~A\n" (blue "pkg") (assoc-val :pkg-path pkg-alist))
        (format #t "~A: ~A\n" (blue "mibl-stanzas") mibl-stanzas)))
  ;; (format #t "pkg-alist: ~A\n" pkg-alist)
  ;; (format #t "  mibl-stanzas: ~A\n" mibl-stanzas)
  ;; (error 'x "X")
  (let* ((stanza-alist (cdr stanza-assoc)))
    ;; (format #t "~A: ~A~%" (green "stanza-alist") stanza-alist)

    ;; stanza types: rule, library, executable(s), test(s), alias,
    ;; install, ocamllex, ocamlyacc, menhir, env, tuareg,
    ;; data_only_dirs, documentation, deprecated_library_name
    (format #t "car stanza-assoc: ~A~%" (car stanza-assoc))
    (case (car stanza-assoc)
      ((:libraries)
       (format #t "~A~%" (green "LIBS"))
       (let ((libs (mibl-libraries->miblx ws pkg-alist stanza-assoc)))
         (set-cdr! mibl-stanzas
                   (append
                    (cdr mibl-stanzas)
                    libs))
                  ))

      ((:modules)
       ;; (format #t "~A~%" (green "MODULES"))
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  '((:MODULES ()))
                  ;;`,@(dune-rule->mibl ws pkg-alist stanza-assoc)
                  ))
       ;; (mibl-trace "mibl-stanzas" mibl-stanzas :color red :test #t)
       )

      ((:generator)
       ;; (format #t "~A~%" (green "GENERATOR"))
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (list stanza-assoc)
                  ;;`,@(dune-rule->mibl ws pkg-alist stanza-assoc)
                  ))
       ;; (mibl-trace "mibl-stanzas" mibl-stanzas :color red :test #t)
       )

      ;; ((alias) (normalize-stanza-alias stanza-assoc))
      ;; ((copy_files#) (normalize-stanza-copy_files pkg-path stanza-assoc))
      ;; ((copy_files) (normalize-stanza-copy_files pkg-path stanza-assoc))
      ;; ((copy#) (normalize-stanza-copy pkg-path stanza-assoc))
      ;; ((copy) (normalize-stanza-copy pkg-path stanza-assoc))
      ;; ((data_only_dirs) (normalize-stanza-data_only_dirs stanza-assoc))
      ;; ((env) (normalize-stanza-env stanza-assoc))
      ;; ((executable) (normalize-stanza-executable :executable
      ;;                pkg-path ocaml-srcs stanza-assoc))
      ((executable)
       (let* ((mibl-stanza (dune-executable->mibl ws pkg-alist :executable stanza-assoc))
              (x (append (cdr mibl-stanzas) mibl-stanza)))
         (if *mibl-debug-all*
             (begin
               (format #t  "~A: ~A~%" (yellow "mibl-stanza") mibl-stanza)
               (format #t  "~A: ~A~%" (yellow "x") x)))
         (set-cdr! mibl-stanzas x)))

      ((executables)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executables->mibl
                   ws pkg-alist :executable stanza-assoc))))

      ((tests)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executables->mibl ws pkg-alist :test stanza-assoc))))

      ((test)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-executable->mibl ws pkg-alist :test stanza-assoc))))

      ((alias)
       (if (assoc 'action stanza-alist)
           (begin
             ;; action fld removed from alias stanza in dune 2.0

             ;; earlier versions may use it, so we convert to
             ;; std rule stanza-assoc with alias fld
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (red "stanza-assoc before") stanza-assoc))
             (let ((n (car (assoc-val 'name stanza-alist))))
               (set! stanza-assoc (cons :rule
                                        `((alias ,n)
                                          ,@(dissoc '(name) (cdr stanza-assoc))))))
             (if *mibl-debug-all*
                 (format #t "~A: ~A~%" (red "stanza-assoc after") stanza-assoc))
             (set-cdr! mibl-stanzas
                       (append
                        (cdr mibl-stanzas)
                        (dune-rule->mibl ws pkg-alist stanza-assoc)))
             )
           (set-cdr! mibl-stanzas
                     (append
                      (cdr mibl-stanzas)
                      (dune-alias->mibl ws pkg-alist stanza-assoc)))))

      ((install)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-install->mibl ws pkg-alist stanza-assoc))))

      ((ocamllex)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (lexyacc->mibl :lex ws pkg-alist stanza-assoc))))

      ((ocamlyacc)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (lexyacc->mibl :yacc ws pkg-alist stanza-assoc))))

      ((menhir)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (menhir->mibl ws pkg-alist stanza-assoc))))

      ((env)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-env->mibl ws pkg-alist stanza-assoc))))

      ;; ((:dune-project) stanza-assoc)

      ((tuareg)
       (set-cdr! mibl-stanzas
                 (append
                  (cdr mibl-stanzas)
                  (dune-tuareg->mibl ws pkg-alist stanza-assoc))))

      ((data_only_dirs) (values)) ;;FIXME

      ((documentation) (values)) ;;FIXME

      ((deprecated_library_name) (values))

      ((generate_sites_module) (values))

      ((:sh-test) ;; ???
       (values))

      (else
       ;; (format #t "~A: ~A\n" (red "unhandled") stanza-assoc)
       (error 'FIXME (format #f "~A: ~A~%" (red "unhandled mibl stanza-assoc") stanza-assoc)))) ;;))
    ;; (format #t "~A: ~A\n" (uwhite "normalized pkg-alist") pkg-alist)
    ;; (format #t "~A~%" (bgred "UPKG-MODULES"))
    ;; (for-each (lambda (m) (format #t "\t~A~%" m)) (assoc-val :modules pkg-alist))

    ;; (-mark-apodoses! pkg-alist)

    ;; remove empty fields
    ;; (-trim-pkg! pkg-alist)

    pkg-alist))

(define (mibl-pkg->miblx ws pkg-alist)
  (if *mibl-debug-miblx*
      (format #t "~A: ~A\n" (bgblue "mibl-pkg->miblx")
              (assoc-val :pkg-path pkg-alist)))
  ;; (format #t "~A: ~A\n" (green "ws") ws)
  (let* ((mibl-stanzas (list :miblx)) ;; hack to make sure pkg-alist is always an alist
         ;; (pkg-alist+ (append pkg-alist (list mibl-stanzas)))
         (pkg-alist+ pkg-alist)
         )
    ;; (format #t "pkg-alist+: ~A\n" pkg-alist+) ;; (assoc 'dune pkg-alist+))
    ;; (set-car! dune-stanzas :dune-stanzas)
    (if (assoc 'mibl pkg-alist+)
        (let ((new-pkg-alist
               (map
                (lambda (stanza-assoc)
  ;; (format #t "STANZA COPY: ~A\n" stanza-assoc)
                  (let ((normed (mibl-stanza->miblx ws
                                 pkg-alist+ stanza-assoc mibl-stanzas)))
                    ;; pkg-path
                    ;; ;; dune-project-stanzas
                    ;; srcfiles ;; s/b '() ??
                    ;; stanza)))
                    ;; (format #t "NORMALIZED: ~A\n" normed)
                    normed))
                ;; (cdr dune-stanzas))))
                (assoc-val 'mibl pkg-alist+))))

          ;; (format #t "~A: ~A\n" (red "NEW MIBL STANZAS") mibl-stanzas)
          ;; (format #t "~A: ~A\n" (red "NEW PKG-ALIST+") pkg-alist+)
          (let* ((@ws (assoc-val ws *mibl-project*))
                 (exports (car (assoc-val :exports @ws))))
            (if *mibl-debug-all*
                (format #t "~A: ~A~%" (red "exports table") exports)))
          ;; pkg-alist+)
          (append pkg-alist (cdr mibl-stanzas)))
        (begin
          (if *mibl-debug-all*
              (format #t "~A: ~A\n"
                  (red "WARNING: pkg-alist w/o BUILD.mibl")
                  (assoc-val :pkg-path pkg-alist)))
          pkg-alist))))

(define (mibl0->miblx ws)
  (mibl-trace-entry "mibl0->miblx" ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (pkg-assoc)
                            ;; key: pkg path (string)
                            ;; val: assoc-list
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr pkg-assoc))
                            (if (assoc 'mibl (cdr pkg-assoc))

                                (let ((mibl-pkg (mibl-pkg->miblx :@ (cdr pkg-assoc))))
                                  ;; (format #t "~A: ~A~%" (red "mibl-pkg->miblx") mibl-pkg)
                                  (hash-table-set! pkgs (car pkg-assoc) mibl-pkg)
                                  mibl-pkg)
                                (begin
                                  ;; (format #t "~A: ~A~%" (red "dune->mibl: no dune file") pkg-assoc)
                                  (cdr pkg-assoc))
                                ))
                         pkgs)))
        ;; (format #t "~A: ~A~%" (blue "mpkg-alist") mpkg-alist)
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (if *mibl-debug-all*
        (format #t "~A: ~A~%" (blue "mibl pkg ct") (length mpkg-alist)))
    mpkg-alist))
