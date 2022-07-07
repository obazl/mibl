(format #t "loading modules.scm\n")

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
(define (get-pkg-module-names modules-assoc)
  ;; (format #t "GET-pkg-module-names, massoc: ~A\n" modules-assoc)
  (let* ((statics (assoc-val :static (cdr modules-assoc)))
         ;; (_ (format #t "statics: ~A\n" statics))
         (dynamics (assoc-val :dynamic (cdr modules-assoc)))
         ;; (_ (format #t "dynamics: ~A\n" dynamics))
         (both (map first (append statics dynamics)))
         )
    ;; (format #t "both: ~A\n" both)
    both))

;;;;; expand dune constant ':standard' for modules
;; e.g.
;; src/proto_alpha/lib_parmeters: (modules :standard \ gen)
;; lib_client_base:: (modules (:standard bip39_english))

;; (define (expand-std-modules modules srcfiles)

(define (expand-std-modules modules-spec pkg-modules)
  (format #t "EXPAND-std-modules: ~A\n" modules-spec)
  ;; modules arg: everything after :standard
  ;; WARNING: the '\' is a symbol, but it does not print as '\,
  ;; rather it prints as (symbol "\\"); use same to compare, do
  ;; not compare car to 'symbol, like so:

  ;; (if (not (null? modules))
  ;;     (if (equal? (car modules) (symbol "\\"))
  ;;         (format #t "EXCEPTING ~A\n" (cdr modules))))

  ;; (format #t "  srcfiles: ~A\n" srcfiles)
  ;; (let ((module-names (srcs->module-names srcfiles)))
  (let ((spec (cdr modules-spec)) ;; car is always :standard
        (pkg-module-names (get-pkg-module-names pkg-modules)))
    (format #t "pkg-module-names: ~A\n" pkg-module-names)
    (format #t "spec: ~A\n" spec)
    ;; handling '/': in principle it can go anywhere:
    ;; (<sets1> \ <sets2>) is how the docs put it.
    ;; in practice it only seems to be used after :standard
    ;; e.g. (:standard \ foo) includes all modules except foo
    ;; also found: (modules (:standard) \ foo)
    (if-let ((slash (member (symbol "\\") spec)))
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
            pkg-module-names)))

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
  (let ((+documentation+ "Expand  'modules' field (of library or executable stanzas) and convert to :submodules assoc. modules-spec is a '(modules ...)' field from a library stanza; fs-modules is the list of modules in the package: an alist whose assocs have the form (A (:ml a.ml)(:mli a.mli)), i.e. keys are module names. Updates global modules-tbl.")
        (+signature+ '(modules-fld->submodules-fld modules-spec pkg-modules modules-ht)))
    (lambda (modules-spec pkg-modules modules-ht)
      (format #t "modules-fld->submodules-fld\n")
      (format #t "modules-spec: ~A\n" modules-spec)
      (format #t "pkg-modules: ~A\n" pkg-modules)
      (if modules-spec
          (let* ((modules (cdr modules-spec))
                 (pkg-module-names (get-pkg-module-names pkg-modules))
                 (_ (format #t "modules: ~A\n" modules))
                 (tmp (let recur ((modules modules)
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
                          (begin
                            (format #t "(equal? :standard (car modules))\n")
                            (let ((std-modules (expand-std-modules (cdr modules-spec)
                                                                   pkg-modules)))
                              (format #t "std-modules: ~A\n" std-modules)
                              std-modules)))

                         ((pair? (car modules))
                          (begin
                            (format #t "(pair? (car modules))\n")
                            ;; e.g. (:standard), (:standard foo), (:standard \ foo)
                            ;; or (modules (:standard) \ foo)
                            ;; or (A B C)
                            (if (equal? '(:standard) (car modules))
                                (modules-fld->submodules-fld
                                 (append
                                  (list 'modules :standard) (cdr modules))
                                 pkg-modules modules-ht)
                                (modules-fld->submodules-fld (cons 'modules (car modules))
                                                             pkg-modules modules-ht))))

                         ;; inclusions, e.g. (modules a b c)
                         (else
                          (begin
                            (format #t "other - inclusions: ~A\n" modules)
                            (format #t "pkg-modules: ~A\n" pkg-module-names)
                            (if (member (normalize-module-name (car modules))
                                        pkg-module-names)
                                (recur (cdr modules) (cons (car modules) direct)
                                       indirect)
                                (error 'bad-arg "included module not in list"))))
                         ) ;; cond
                        ))) ;; recur
            tmp) ;; let*
          ;; no modules-spec - default is all
          (get-pkg-module-names pkg-modules)
          ) ;; if modules-spec
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


(format #t "loaded modules.scm\n")
