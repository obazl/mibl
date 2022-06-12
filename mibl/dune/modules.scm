;; mibl/dune/modules.scm

(define (filename->module-assoc filename)
  (let* ((ext (filename-extension filename))
         (pname (principal-name filename))
         (mname (normalize-module-name pname)))
    (cond
     ((string=? ext ".mli") (cons mname
                                  (list (list :mli filename :gen))))
     ((string=? ext ".ml") (cons mname
                                 (list (list :ml filename :gen))))
     (else #t))
    ))

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


;;;;; expand dune constant ':standard' for modules
;; e.g.
;; src/proto_alpha/lib_parmeters: (modules :standard \ gen)
;; lib_client_base:: (modules (:standard bip39_english))
(define (standard-modules modules srcfiles)
  ;; modules arg: everything after :standard
  ;; (format #t "standard-modules: ~A\n" modules)
  ;; WARNING: the '\' is a symbol, but it does not print as '\,
  ;; rather it prints as (symbol "\\"); use same to compare, do
  ;; not compare car to 'symbol, like so:

  ;; (if (not (null? modules))
  ;;     (if (equal? (car modules) (symbol "\\"))
  ;;         (format #t "EXCEPTING ~A\n" (cdr modules))))

  ;; (format #t "  srcfiles: ~A\n" srcfiles)
  (let ((module-names (srcs->module-names srcfiles)))
    (if (null? modules)
        ;; (modules (:standard)) means same as omitting (modules), i.e.
        ;; all modules:
        (values module-names '())

        ;; NB: (:standard \ ...) => (:standard (symbol "\\") ...)
        (if (pair? (car modules))
            (if (equal? (caar modules) (symbol "\\"))
                (let ((exception (cadr modules)))
                  ;; (format #t "EXCEPTION1: ~A\n" exception)
                  (values (remove-if list
                                     (lambda (m)
                                       ;; (format #t "remove? ~A\n" m)
                                       (equal? exception m))
                                     module-names)
                          '()))
                ;; else :standard adds to default list, e.g.
                ;; (:standard (foo bar baz))
                ;; tezos example: src/lib_client_base:
                ;;     (modules (:standard bip39_english))
                (values (concatenate (map
                                      normalize-module-name
                                      (car modules))
                                     module-names)
                        '()))
            ;; else (car modules) is atom,
            ;; e.g. (:standard \ foo) or (:standard foo bar baz) ...
            (if (equal? (car modules) (symbol "\\"))
                (let ((exception (normalize-module-name (cadr modules))))
                  ;; (format #t "EXCEPTION2: ~A\n" exception)
                  (values (remove-if list
                                     (lambda (m)
                                       ;; (format #t "remove? ~A\n" m)
                                       (equal? exception m))
                                     module-names)
                          '()))
                (values (concatenate (map
                                      normalize-module-name
                                      modules)
                                     module-names)
                        '()))))))

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
(define expand-modules-fld!
  (let ((+documentation+ "Expand  'modules' field and convert to :submodules assoc. modules-spec is a '(modules ...)' field from a library stanza; fs-modules is the list of modules in the package: an alist whose assocs have the form (A (:ml a.ml)(:mli a.mli)), i.e. keys are module names. Updates global modules-tbl.")
        (+signature+ '(expand-modules-fld! modules-spec pkg-modules modules-ht)))
    (lambda (modules-spec pkg-modules modules-ht)
      (format #t "expand-modules-fld!\n")
      (format #t "modules-spec: ~A\n" modules-spec)
      (format #t "pkg-modules: ~A\n" pkg-modules)
      ;; #t)
      (let ((modules (cdr modules-spec)))
        (let recur ((modules modules)
                    (direct '()) ;; src files
                    (indirect '())) ;; generated files
          ;; (format #t "recur modules ~A\n" modules)
          (cond
           ((null? modules)
            (begin
              ;; (format #t "result mtbl: ~A\n" modules-tbl)
              (values direct indirect)))

    ;;        ((equal? :standard (car modules))
    ;;         (let-values (((std-modules rest)
    ;;                       (standard-modules (cdr modules) srcfiles)))
    ;;           (for-each (lambda (m)
    ;;                       (hash-table-set! modules-tbl
    ;;                                        m :direct))
    ;;                     std-modules)
    ;;           ;; (format #t "xxxx ~A\n" rest)
    ;;           (recur rest)))
    ;;        ;; e.g. lib_client_base::
    ;;        ;; (modules (:standard bip39_english))
    ;;        ;; or: (modules (:standard \ foo))

    ;;        ;; (let ((newseq (srcs->module-names srcfiles))) ;;  direct
    ;;        ;;    ;; (format #t "modules :STANDARD ~A\n" newseq)
    ;;        ;;    ;; (format #t "CDRMODS ~A\n" (cdr modules))
    ;;        ;;    (recur (cdr modules)
    ;;        ;;           (append newseq direct)
    ;;        ;;           indirect))

    ;;        ;; )

    ;;        ;; (concatenate direct
    ;;        ;;              (norm-std-modules (cdr modules))))

    ;;        ((pair? (car modules)) ;; e.g. ???
    ;;         ;; (format #t "TTTT ~A\n" (pair? (car modules)))
    ;;         ;; e.g. ? (modules foo (bar) baz)?
    ;;         ;; (let-values (((exp gen)
    ;;         ;;               (recur (car modules)))) ;;  '() '())))
    ;;         (let ((tbl (recur (car modules)))) ;;  '() '())))
    ;;           (recur (cdr modules))))
    ;;        ;; (concatenate exp direct)
    ;;        ;; (concatenate gen indirect))))

           ;; ((indirect-module-dep? (car modules) srcfiles)
           ((module-is-generated? (car modules) srcfiles)
            ;; module name has no corresponding file
            (begin
              ;; (format #t "INDIRECT: ~A\n" (car modules))
              (hash-table-set! modules-tbl
                               (car modules)
                               pkp-path)
                               ;; :indirect)
              (recur (cdr modules))))
           ;; direct
           ;; (cons (car modules) indirect))))

    ;;        (else
    ;;         (hash-table-set! modules-tbl
    ;;                          (normalize-module-name (car modules))
    ;;                          :direct)
    ;;         ;; (format #t "yyyy ~A\n" modules)
    ;;         (recur (cdr modules))))
    ;;       ;; (cons (car modules) direct)
    ;;       ;; indirect))
    ;;       ))
    ;;   ;;      ))
    ;;   ;; ;;(format #t "RESULT: ~A\n" result)
    ;;   ;; (reverse result))
    ;;   ) ;; end lambda
           ))
        ) ;; let
      ) ;; lamda
    ) ;; let
  )

