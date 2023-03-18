(display "loading codept/codept_reader.scm") (newline)

;; (load "opam.scm")
;; (load "utils.scm")

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
;; (define codept-stdlib-cmd
;;   (string-append codept-cmd " -k -sexp "
;;                  opam-root "/" opam-switch
;;                  "/lib/ocaml/*.m* 2> /dev/null"))

;; (system codept-stdlib-cmd #t)

  ;; (string-append "codept -k -sexp "
  ;;                opam-root "/" opam-switch
  ;;                "/lib/ocaml/*.m* 2> /dev/null"))

;; (define codept-bin-cmd
;;   (string-append "opam var bin"))

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
