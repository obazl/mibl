;; (format #t "loading modules.scm\n")

;; mibl/dune/modules.scm

(define (filename->module-assoc filename)
  ;; (format #t "filename->module-assoc ~A\n" filename)
  (let* ((ext (filename-extension filename))
         (pname (principal-name filename))
         (mname (normalize-module-name pname)))
    ;; (format #t "mname: ~A\n" mname)
    (let ((a
           (cond
            ((string=? ext ".mli") (cons mname
                                         (list (list :mli filename))))
            ((string=? ext ".ml") (cons mname
                                        (list (list :ml filename))))
            (else #t))))
      ;; (format #t "f->m result: ~A" a)
      a)))

;; (define indirect-module-dep?
(define module-is-generated?
  (let ((+documentation+ "True if module name does *not* match a source file name."))
    (lambda (module srcfiles)
      ;; (format #t "module-is-generated? ~A : ~A\n" module srcfiles)
      (let recur ((srcfiles srcfiles))
        (if (null? srcfiles)
            #t
            (let* ((m (if (symbol? module) (symbol->string module)
                          (copy module)))
                   (bn (bname (car srcfiles))))

              (if (string=? m bn)
                  #f
                  (begin
                    (string-set! m 0 (char-downcase (string-ref m 0)))
                    (if (string=? m bn)
                        #f
                        (recur (cdr srcfiles)))))))))))

;; modules-assoc:
;;   (modules (:static (A (:ml "a.ml")) (:dynamic (B (:ml "b.ml")))))
;; (define (expand-std-modules modules-spec pkg-modules)
(define get-pkg-module-names
  (let ((+documentation+ "Returns module names from modules-assoc, which has the form (:modules (:static (A (:ml a.ml) (:mli a.mli)...) (:dynamic ...))")
        (+signature+ '(get-pkg-module-names modules-assoc)))
    (lambda (modules-assoc)
      ;; (format #t "GET-pkg-module-names, massoc: ~A\n" modules-assoc)
      (let* ((statics (assoc-val :static (cdr modules-assoc)))
         ;; (_ (format #t "statics: ~A\n" statics))
         (dynamics (assoc-val :dynamic (cdr modules-assoc)))
         ;; (_ (format #t "dynamics: ~A\n" dynamics))
         (both (map first (append statics dynamics)))
         )
        ;; (format #t "both: ~A\n" both)
        both))))

;;;;; expand dune constant ':standard' for modules
;; e.g.
;; src/proto_alpha/lib_parmeters: (modules :standard \ gen)
;; lib_client_base:: (modules (:standard bip39_english))

;; (define (expand-std-modules modules srcfiles)

(define (resolve-gentargets gentargets sigs structs)
  (format #t "resolve-gentargets: ~A\n" gentargets)
  (let ((resolved (map (lambda (f)
                         (format #t "f: ~A\n" f)
                         (let* ((fname (if (symbol? f) (symbol->string f) f))
                                (type (if (eq? 0 (fnmatch "*.mli" fname 0))
                                          :mli :ml))
                                (mname (file-name->module-name fname)))
                           (format #t "mname: ~A\n" mname)
                           (format #t "type: ~A\n" type)
                           (if (eq? type :ml)
                               (if-let ((sigmatch (assoc-in `(:static ,mname)
                                                            sigs)))
                                       (begin
                                         ;; remove from pkg :signatures
                                         (alist-update-in!
                                          sigs `(:static)
                                          (lambda (old)
                                            (format #t "old static: ~A\n"
                                                    old)
                                            (dissoc `(,mname) old)))
                                         `(:_ ,(car sigmatch)))
                                       `(:ml ,mname))
                               (if-let ((structmatch
                                         (assoc-in `(:static ,mname)
                                                   structs)))
                                       (begin
                                         ;; remove from pkg :structures
                                         (alist-update-in!
                                          structs `(:static)
                                          (lambda (old)
                                            (format #t "old static: ~A\n"
                                                    old)
                                            (dissoc `(,mname) old)))
                                         `(:_ ,(car structmatch)))
                                       `(:mli ,mname)))))
                       gentargets)))
    resolved))

(define expand-std-modules
  (let ((+documentation+ "expands a ':standard' part of a (modules :standard ...) clause. std-list: the ':standard' clause and any modifiers.  pkg-modules: list of source modules (paired .ml/.mli files), from mibl :modules fld; module-deps: :deps from 'libraries' field and possibly :genmodules (if 'select' is used). :genmodules contains selector clauses.")
        (+signature+ '(expand-std-modules std-list pkg-modules module-deps sigs structs)))
    ;; modules-ht)))
    (lambda (std-list pkg-modules module-deps sigs structs)

      (format #t "~A: ~A\n" (blue "EXPAND-std-modules") std-list)
      (format #t " pkg-modules: ~A\n" pkg-modules)
      (format #t " module-deps: ~A\n" module-deps)
      (format #t " sigs: ~A\n" sigs)
      (format #t " structs: ~A\n" structs)
      ;; std-list arg: everything after :standard
      ;; WARNING: the '\' is a symbol, but it does not print as '\,
      ;; rather it prints as (symbol "\\"); use same to compare, do
      ;; not compare car to 'symbol, like so:
      ;; (if (not (null? modules))
      ;;     (if (equal? (car modules) (symbol "\\"))
      ;;         (format #t "EXCEPTING ~A\n" (cdr modules))))

      (let ((modifiers (cdr std-list)) ;; car is always :standard
            (pkg-module-names (get-pkg-module-names pkg-modules))
            (genmodules (assoc :genmodules module-deps)))
        (format #t "pkg-module-names: ~A\n" pkg-module-names)
        (format #t "modifiers: ~A\n" modifiers)
        (format #t "genmodules: ~A\n" genmodules)
        ;; handling '/': in principle it can go anywhere:
        ;; (<sets1> \ <sets2>) is how the docs put it.
        ;; in practice it only seems to be used after :standard
        ;; e.g. (:standard \ foo) includes all modules except foo
        ;; also found: (modules (:standard) \ foo)
        (if-let ((slash (member (symbol "\\") modifiers)))
                (let* ((exclusions (cdr slash))
                       (exclusions (if (list? (car exclusions))
                                       (car exclusions) exclusions))
                       (exclusions (map normalize-module-name exclusions)))
                  (format #t "exclusions: ~A\n" exclusions)
                  (let ((winnowed (remove-if
                                   list
                                   (lambda (item)
                                     (let ((norm (normalize-module-name item)))
                                       ;; (format #t "item ~A\n" norm)
                                       ;; (format #t "mem? ~A: ~A\n" exclusions
                                       ;;         (member norm exclusions))
                                       (if (member norm exclusions) #t #f)))
                                   pkg-module-names)))
                    winnowed))
                ;; else no exclusions
                (let* ((gentargets (map (lambda (x)
                                          (assoc-val :target x))
                                        (cdr genmodules)))
                       (_ (format #t "gentargets: ~A\n" gentargets))
                       (gentargets (apply append gentargets))
                       ;; (gentargets (map file-name->module-name gentargets))
                       (_ (format #t "~A: ~A\n" (red "GENTARGETS") gentargets))
                       (genmodules (resolve-gentargets gentargets sigs structs))
                       )
                  (format #t "~A: ~A\n" (red "GENMODULES") genmodules)
                  (if genmodules
                      (append genmodules pkg-module-names)
                      pkg-module-names)))))))

;; was (define (modules->modstbl modules srcfiles) ;; lookup_tables.scm
;; expand (modules ...) and convert to (:submodules ...)
;; variants:
;; (modules) - empty, exclude all
;; (modules :standard) == (modules (:standard)) == omitted == include all
;; (modules foo bar) - include just those listed
;; WARNING: :standard cannot be fully expanded until all stanzas in
;; the package have been processed to discover generated files, e.g.
;; rule stanzas may generate files, (expressed by 'target' flds).
;; so rules should be processed first.

;;  expand-modules-fld!
(define modules-fld->submodules-fld
  ;;TODO: direct/indirect distinction. indirect are generated src files
  (let ((+documentation+ "Expand  'modules' field (of library or executable stanzas) and convert to :submodules assoc. modules-spec is a '(modules ...)' field from a library stanza; pkg-modules is the list of modules in the package: an alist whose assocs have the form (A (:ml a.ml)(:mli a.mli)), i.e. keys are module names.")
        (+signature+ '(modules-fld->submodules-fld modules-spec pkg-modules modules-deps sigs structs)))
        ;; modules-ht)))
    (lambda (modules-spec pkg-modules deps sigs structs) ;; modules-ht)
      (format #t "~A\n" (blue "MODULES-FLD->SUBMODULES-FLD"))
      (format #t "modules-spec: ~A\n" modules-spec)
      (format #t "pkg-modules: ~A\n" pkg-modules)
      (format #t "deps: ~A\n" deps)
      (format #t "sigs: ~A\n" sigs)
      (format #t "structs: ~A\n" structs)
      (if pkg-modules
          (if modules-spec
              (let* ((modules-spec (cdr modules-spec))
                     (pkg-module-names (get-pkg-module-names pkg-modules))
                     (_ (format #t "modules-spec: ~A\n" modules-spec))
                     (tmp (let recur ((modules modules-spec)
                                      (direct '()) ;; src files
                                      (indirect '())) ;; generated files
                            (format #t "RECUR modules ~A\n" modules)
                            (format #t "direct: ~A\n" direct)
                            (cond
                             ((null? modules)
                              (if (null? direct)
                                  (begin
                                    (format #t "null modules\n")
                                    '())
                                  (reverse direct)))

                             ((equal? :standard (car modules))
                              ;; e.g. (modules :standard ...)
                              (begin
                                (format #t "(equal? :standard (car modules))\n")
                                (let ((std-expansion (expand-std-modules
                                                      modules-spec
                                                      pkg-modules
                                                      deps
                                                      sigs structs)))
                                  (format #t "std-expansion: ~A\n"
                                          std-expansion)
                                  std-expansion)))

                             ((pair? (car modules))
                              (begin
                                (format #t "(pair? (car modules))\n")
                                ;; e.g. ((:standard)) ?
                                ;; or (modules (:standard) \ foo)
                                ;; or (A B C)
                                ;; just unroll and recur
                                (if (equal? '(:standard) (car modules))
                                    (modules-fld->submodules-fld
                                     (append
                                      (list 'modules :standard) (cdr modules))
                                     pkg-modules deps sigs structs)
                                    (modules-fld->submodules-fld (cons
                                                                  'modules
                                                                  (car modules))
                                                                 pkg-modules
                                                                 deps
                                                                 sigs
                                                                 structs))))

                             ;; inclusions, e.g. (modules a b c)
                             (else
                              (begin
                                (format #t "other - inclusions: ~A\n" modules)
                                (format #t "pkg-modules: ~A\n" pkg-module-names)
                                (if (member (normalize-module-name (car modules))
                                            pkg-module-names)
                                    (recur (cdr modules) (cons
                                                          (normalize-module-name
                                                           (car modules)) direct)
                                           indirect)
                                    (error 'bad-arg "included module not in list"))))
                             ) ;; cond
                            ))) ;; recur
                tmp) ;; let*
              ;; no modules-spec - default is all
              (get-pkg-module-names pkg-modules)
              ) ;; if modules-spec
          ;; else no modules in pkg
          '())
      ) ;; lamda
    ) ;; let
  ) ;; define

;; (define (expand-modules-fld modules srcfiles)
;;   ;; modules:: (modules Test_tezos)
;;   ;; (format #t "  expand-modules-fld: ~A\n" modules)
;;   ;; see also modules->modstbl in dune_stanza_fields.scm
;;   (let* ((modules (cdr modules)))
;;     (if (null? modules)
;;         (values '() '())
;;         ;; (let ((result
;;         (let recur ((modules modules)
;;                     (direct '())
;;                     (indirect '()))
;;           ;; (format #t "ms: ~A; direct: ~A\n" modules direct)
;;           (cond
;;            ((null? modules)
;;             (values direct indirect))

;;            ((equal? :standard (car modules))
;;             (let ((newseq (srcs->module-names srcfiles))) ;;  direct
;;               ;; (format #t "modules :STANDARD ~A\n" newseq)
;;               ;; (format #t "CDRMODS ~A\n" (cdr modules))
;;               (recur (cdr modules) (append newseq direct) indirect)))
;;            ;; (concatenate direct
;;            ;;              (norm-std-modules (cdr modules))))
;;            ((pair? (car modules))
;;             (let-values (((exp gen)
;;                           (recur (car modules) '() '())))
;;               (recur (cdr modules)
;;                      (concatenate exp direct)
;;                      (concatenate gen indirect))))

;;            ((indirect-module-dep? (car modules) srcfiles)
;;             (begin
;;               ;; (format #t "INDIRECT: ~A\n" (car modules))
;;               (recur (cdr modules)
;;                      direct (cons (car modules) indirect))))

;;            (else
;;             (recur (cdr modules)
;;                    (cons (car modules) direct)
;;                    indirect))))
;;         ;;      ))
;;         ;; ;;(format #t "RESULT: ~A\n" result)
;;         ;; (reverse result))
;;         ))
;;   )


;; (format #t "loaded modules.scm\n")
