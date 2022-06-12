;; (display "dune/lookup_tables.scm loading ...") (newline)


;; (define opam-bin-dir (string-right-trim " \n" (system "opam var bin" #t)))

;; (define (make-stdbin-tbl)
;;   (let* ((stdbin-tbl (make-hash-table))
;;          (execs (directory->list opam-bin-dir)))
;;     (for-each (lambda (exe)
;;                 (if (and (not (equal? exe "."))
;;                          (not (equal? exe "..")))
;;                     (begin
;;                       (format #t "stdbin exe: ~A\n" exe)
;;                       (hash-table-set! stdbin-tbl exe
;;                                        (string-append
;;                                         "@ocaml//:" exe))
;;                       (hash-table-set! stdbin-tbl
;;                                        (string-append "bin:" exe)
;;                                        (string-append
;;                                         "@ocaml//:" exe))
;;                       )))
;;               execs)
;;     stdbin-tbl))

(define (update-executables-table src dst)
  ;; (format #t "update-executables-table: ~A => ~A\n" src dst)
  (hash-table-set! executables-tbl src dst))

(define (update-modtbl-null stanza)
  stanza)

(define (make-label path submodule)
  `(:label
    ,(string-append
      "//"
      (if (symbol? path) (symbol->string path) path)
      ":"
      (if (symbol? submodule) (symbol->string submodule) submodule))))

(define (make-ns-label path ns submodule)
  `(:label
    ((:module
    ,(string-append
      "//"
      (if (symbol? path) (symbol->string path) path)
      ":"
      (if (symbol? submodule) (symbol->string submodule) submodule)))
     (:ns
      ,(string-append
        "//"
        (if (symbol? path) (symbol->string path) path)
        ":"
        (if (symbol? ns) (symbol->string ns) ns))))))

;; FIXME: put executables in ns-tbl???
(define (update-ns-tbl-executable ns-tbl path stanza)
  ;; (format #t "update-ns-tbl-executable: ~A\n ~A\n" path stanza)
  ;; (assoc :name (cadr stanza)))
  (let* ((stanza-alist (cadr stanza))
         (privname (assoc-in '(:name :private) stanza-alist))
         (pubname (assoc-in '(:name :public) stanza-alist))
         (key (if pubname (cadr pubname) (cadr privname)))
         (modules (assoc :modules stanza-alist)))
    ;; stanza-alist: (<assoc> <assoc> ...)
    ;; (format #t "    privname: ~A\n" private-name)
    ;; (format #t "    pubname: ~A\n" pubname)
    ;; (format #t "    key: ~A\n" key)
    (hash-table-set! ns-tbl
                     key
                     `((:target ,key)
                       (:exe)
                       (:dir ,path)))

    (if modules
        (begin
          (if-let ((directs (assoc :direct (cadr modules))))
                  (for-each (lambda (module)
                              (hash-table-set! ns-tbl module
                                               `((:module ,module)
                                                 ,(make-label
                                                   path module)
                                                 (:direct)
                                                 (:dir ,path))))
                            (cdr directs)))
          (if-let ((indirects (assoc :indirect (cadr modules))))
                  (for-each (lambda (module)
                              (hash-table-set! ns-tbl module
                                               `((:module ,module)
                                                 ,(make-label
                                                   path module)
                                                 (:indirect)
                                                 (:dir ,path))))
                            (cdr indirects)))))
    ))

;; one entry for aggregate, and one for each submodule
(define (update-ns-tbl-library ns-tbl path stanza)
  (format #t "update-ns-tbl-library: ~A\n" path)
  ;; (format #t "    stanza: ~A\n" stanza)
    ;; stanza: (<assoc> <assoc> ...)
  (let* ((stanza-alist (cadr stanza))
         ;; (name (assoc :name stanza-alist))
         (privname (cadr (assoc-in '(:name :private) stanza-alist)))
         (pubname (if-let ((pname (assoc-in '(:name :public) stanza-alist)))
                          (cadr pname) #f))
         (modname (normalize-module-name privname))
         (wrapped? (if-let ((wrapped (assoc :wrapped stanza-alist)))
                           (if (equal? wrapped 'false) #f #t)
                           #t))
         (opts (assoc :opts stanza-alist))
         (submodules (assoc :submodules stanza-alist))
         (deps    (assoc :deps    stanza-alist)))

    ;; (format #t "aggregate: ~A\n" pubname)
    ;; aggregate key: pubname
    (if-let ((entry (hash-table-ref ns-tbl pubname)))
            (begin
              (format #t "DUP: ~A\n" pubname)
              (hash-table-set! ns-tbl pubname
                               (append entry '(:dup))))
            ;; else new ns entry, but only if :submodules not null
            ;; i.e. skip "dummy" libs (e.g. src/tooling)
            (if submodules
                (hash-table-set! ns-tbl pubname
                                 (list
                                  `(:name
                                    ((:public  . ,pubname)
                                     (:private . ,privname)
                                     (:module  . ,modname)))
                                  `(:dir ,path)
                                  '(:aggregate)
                                  (if wrapped? '(:ns-archive) '(:library))
                                  ;; (if-let ((wrapped (assoc 'wrapped stanza-alist)))
                                  ;;         (if (equal? 'false (cadr wrapped))
                                  ;;             '(:library)
                                  ;;             '(:ns-archive))
                                  ;;         '(:ns-archive))
                                  opts
                                  `(:submodules ,@(cdr submodules))
                                  deps))))
    ;; (format #t "submodules: ~A\n" submodules)
    ;; add a modtbl entry for each dep
    ;; (if submodules
    ;;     (begin
    ;;       (if-let ((directs (assoc :direct (cadr submodules))))
    ;;               (begin
    ;;                 (format #t "directs: ~A\n" directs)
    ;;                 (for-each (lambda (kv) ;; (submodule)
    ;;                           (format #t "kv: ~A\n" kv)
    ;;                           (let ((submodule (car kv)))
    ;;                             (hash-table-set! ns-tbl submodule
    ;;                                            `((:module ,submodule)
    ;;                                              ;; label is for ns!
    ;;                                              ,(make-ns-label
    ;;                                               path pubname submodule)
    ;;                                              (:ns ,modname)
    ;;                                              ;; (:srcfile)
    ;;                                              (:dir ,path)))))
    ;;                         (cadr directs))))
    ;;       (format #t "xxxx\n")
    ;;       (if-let ((indirects (assoc :indirect (cadr submodules))))
    ;;               (begin
    ;;                 (format #t "indirects: ~A\n" indirects)
    ;;                 (for-each (lambda (submodule)
    ;;                           (hash-table-set! ns-tbl submodule
    ;;                                            `((:module ,submodule)
    ;;                                              ,(make-ns-label
    ;;                                               path pubname submodule)
    ;;                                              (:ns ,modname)
    ;;                                              (:generated)
    ;;                                              (:dir ,path))))
    ;;                           (cdr indirects)))))
    ;;     ;; else
    ;;     )
    ))

;; (define (xupdate-modtbl-library modules-tbl path stanza)
;;   (format #t "update-modtbl-library: ~A\n" path)
;;   ;; (format #t "    stanza: ~A\n" stanza)
;;     ;; stanza: (<assoc> <assoc> ...)
;;   (let* ((stanza-alist (cadr stanza))
;;          ;; (name (assoc :name stanza-alist))
;;          (privname (cadr (assoc-in '(:name :private) stanza-alist)))
;;          (pubname (if-let ((pname (assoc-in '(:name :public) stanza-alist)))
;;                           (cadr pname) #f))
;;          (modname (normalize-module-name privname))
;;          (wrapped? (if-let ((wrapped (assoc :wrapped stanza-alist)))
;;                            (if (equal? wrapped 'false) #f #t)
;;                            #t))
;;          (opts (assoc :opts stanza-alist))
;;          (submodules (assoc :submodules stanza-alist))
;;          (deps    (assoc :deps    stanza-alist)))

;;     ;; (format #t "aggregate: ~A\n" pubname)
;;     ;; aggregate key: pubname
;;     (if-let ((entry (hash-table-ref modules-tbl pubname)))
;;             (begin
;;               (format #t "DUP: ~A\n" pubname)
;;               (hash-table-set! modules-tbl pubname
;;                                (append entry '(:dup))))
;;             ;; else new entry
;;             (hash-table-set! modules-tbl pubname
;;                              (list
;;                               `(:name
;;                                 ((:public  . ,pubname)
;;                                  (:private . ,privname)
;;                                  (:module  . ,modname)))
;;                               `(:dir ,path)
;;                               '(:aggregate)
;;                               (if-let ((wrapped (assoc 'wrapped stanza-alist)))
;;                                       (if (equal? 'false (cadr wrapped))
;;                                           '(:library)
;;                                           '(:ns-archive))
;;                                       '(:ns-archive))
;;                               opts
;;                               `(:submodules ,@(cdr submodules))
;;                               deps)))
;;     ;;(format #t "submodules: ~A\n" submodules)
;;     (if submodules
;;         (begin
;;           (if-let ((directs (assoc :direct (cadr submodules))))
;;                   (begin
;;                     (format #t "directs: ~A\n" directs)
;;                     (for-each (lambda (kv) ;; (submodule)
;;                               (format #t "kv: ~A\n" kv)
;;                               (let ((submodule (car kv)))
;;                                 (hash-table-set! modules-tbl submodule
;;                                                `((:module ,submodule)
;;                                                  ;; label is for ns!
;;                                                  ,(make-ns-label
;;                                                   path pubname submodule)
;;                                                  (:ns ,modname)
;;                                                  ;; (:srcfile)
;;                                                  (:dir ,path)))))
;;                             (cadr directs))))
;;           (format #t "xxxx\n")
;;           (if-let ((indirects (assoc :indirect (cadr submodules))))
;;                   (begin
;;                     (format #t "indirects: ~A\n" indirects)
;;                     (for-each (lambda (submodule)
;;                               (hash-table-set! modules-tbl submodule
;;                                                `((:module ,submodule)
;;                                                  ,(make-ns-label
;;                                                   path pubname submodule)
;;                                                  (:ns ,modname)
;;                                                  (:generated)
;;                                                  (:dir ,path))))
;;                             (cdr indirects))))))
;;     ))

(define (dune->ns-tbl pkg-tbls-list) ;; codept-sexp)
  (let ((ns-tbl (make-hash-table)))
    (for-each
     (lambda (pkg-tbl)
       ;; (newline)
       ;; (format #t "PKG-TBL: ~A\n" (car pkg-tbl))
       ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
       (for-each
        (lambda (pkg)
          ;; (newline)
          ;; (format #t "PKG path: ~A\n" (car pkg))
          ;; pkg: (<pkg-path> list of assocs)
          ;; (cdr pkg) : pkg-alist

          ;; add aggregates and their components
          (let ((stanzas (assoc :stanzas (cdr pkg))))
              (if (cadr stanzas)
                  (begin
                    ;; (format #t "STANZAS: ~A\n" "X") ;stanzas)
                    (for-each
                     (lambda (stanza)
                       ;; (format #t "STANZA: ~A\n" stanza)
                       ;; stanza-pair, e.g. (library <alist>)
                       (case (car stanza)
                         ;; ((alias) (update-modtbl-null stanza))
                         ;; ((copy_files) (update-modtbl-null stanza))
                         ;; ((data_only_dirs) (update-modtbl-null stanza))
                         ;; ((env) (update-modtbl-null stanza))

                         ;; FIXME: add executables to ns-tbl???
                         ;; ((:executable) (update-ns-tbl-executable
                         ;;                 ns-tbl
                         ;;                 (car pkg)
                         ;;                 stanza))

                         ;; executables have been rewritten as executable
                         ;; ((:executables) (update-ns-tbl-executables
                         ;;                  (car pkg) ns-tbl stanza))

                         ((install) (update-modtbl-null stanza))
                         ((:library) (update-ns-tbl-library
                                      ns-tbl
                                      (car pkg) ;; pkg-path
                                      stanza))
                         ((ocamllex) (update-modtbl-null stanza))
                         ((rule) (update-modtbl-null stanza))
                         ((test) (update-modtbl-null stanza))
                         ((tests) (update-modtbl-null stanza))

                         ;; ((:genrule)) ;; skip
                         ;; ((:write-file)) ;; skip
                         (else
                          (format #t "dune->ns-tbl unhandled: ~A\n"
                                  (car stanza)))))
                     (cadr stanzas)))
                  ;; else skip - pkg w/o dune stanzas
                  ;;(format #t "NO STANZAS: ~A\n" (car pkg))
                  ))

          ;; for each file: look it up in ns-tbl
          ;; if found, then it was added as a component of an aggregate
          ;; if not found, add it as a stand-alone
          (if-let ((srcfiles (assoc-in '(:srcfiles :ocaml :static)
                                       (cdr pkg))))
                  ;; (format #t "SRCFILES: ~A\n" srcfiles)
                  (for-each (lambda (file)
                              ;;(format #t "SRCFILE: ~A\n" file)
                              (begin))
                            (cadr srcfiles))
                  )
          )
        (cadr pkg-tbl)))
     pkg-tbls-list)
    ns-tbl))

;; (define (xdune->ns-tbl pkg-tbls-list codept-sexp)
;;   (let ((modules-tbl (make-hash-table)))
;;     (for-each
;;      (lambda (pkg-tbl)
;;        ;; (format #t "PKG-TBL: ~A\n" pkg-tbl)
;;        ;; pkg-tbl: (<dir-path> <pkg hash-tbl>) i.e. assoc pair
;;        (for-each
;;         (lambda (pkg)
;;           ;; (format #t "PKG path: ~A\n" (car pkg))
;;           ;; pkg: (<pkg-path> list of assocs)
;;           ;; (cdr pkg) : pkg-alist
;;           (if-let ((stanzas (assoc :stanzas (cdr pkg))))
;;                   (begin
;;                     ;; (format #t "STANZAS: ~A\n" "X") ;stanzas)
;;                     (for-each
;;                      (lambda (stanza)
;;                        ;; (format #t "STANZA: ~A\n" stanza)
;;                        ;; stanza-pair, e.g. (library <alist>)
;;                        (case (car stanza)
;;                          ((alias) (update-modtbl-null stanza))
;;                          ((copy_files) (update-modtbl-null stanza))
;;                          ((data_only_dirs) (update-modtbl-null stanza))
;;                          ((env) (update-modtbl-null stanza))
;;                          ((:executable) (update-modtbl-executable
;;                                          (car pkg) ;; path
;;                                          modules-tbl stanza))
;;                          ((:executables) (update-modtbl-executables
;;                                           (car pkg) ;; path
;;                                           modules-tbl stanza))
;;                          ((install) (update-modtbl-null stanza))
;;                          ((:library) (update-modtbl-library
;;                                      modules-tbl
;;                                      (car pkg) ;; pkg-path
;;                                      stanza))
;;                          ((ocamllex) (update-modtbl-null stanza))
;;                          ((rule) (update-modtbl-null stanza))
;;                          ((test) (update-modtbl-null stanza))
;;                          ((tests) (update-modtbl-null stanza))

;;                          ((:genrule)) ;; skip
;;                          ((:write-file)) ;; skip
;;                          (else
;;                           (format #t "dune->ns-tbl unhandled: ~A\n"
;;                                   (car stanza)))))
;;                      (cadr stanzas)))
;;                   ;; else skip - pkg w/o dune stanzas
;;                   ;;(format #t "NO STANZAS: ~A\n" (car pkg))
;;                   ))
;;         (cadr pkg-tbl)))
;;      pkg-tbls-list)
;;     modules-tbl))

;; make a hash table, key: module name, value: :direct
(define (srcfiles->modstbl srcfiles)
  (format #t "srcfiles->modstbl: ~A\n" srcfiles)
  (let* ((submods (srcs->module-names srcfiles))
        (submods-tbl (make-hash-table (length submods))))
    (for-each (lambda (submod)
                (hash-table-set! submods-tbl submod :direct))
              submods)
    (sort! submods sym<?)
    submods-tbl))

;; returns hash tbl keyed by module name,
;; vals are :direct | :indirect | :resolver
;; uses srcfiles to determine if a module is src or generated
;; handles :standard etc.
;; replaced by expand-modules-fld in modules.scm
(define (modules->modstbl modules srcfiles)
  ;; (format #t "modules->modstbl: ~A\n" modules)
  ;; modules-assoc:: (modules <list of lists>)

  ;; modules list: each module is in a list (from codept sexp) whose
  ;; car may be a namespace module.
  (let* ((modules-tbl (make-hash-table)))
    ;; (if (not (null? modules)) (format #t "modules list: ~A\n" modules))
    ;;     modules-tbl
    ;; (values '() '())
    (let recur ((modules modules))
      ;; (direct '()) ;; src files
      ;; (indirect '())) ;; generated files
      ;; (format #t "recur modules ~A\n" modules)
      (cond
       ((null? modules)
        (begin
          ;; (format #t "result mtbl: ~A\n" modules-tbl)
          modules-tbl))
       ;; (values direct indirect))

       ((equal? :standard (car modules))
        (let-values (((std-modules rest)
                      (standard-modules (cdr modules) srcfiles)))
          (for-each (lambda (m)
                      (hash-table-set! modules-tbl
                                       m :direct))
                    std-modules)
          ;; (format #t "xxxx ~A\n" rest)
          (recur rest)))
        ;; e.g. lib_client_base::
        ;; (modules (:standard bip39_english))
        ;; or: (modules (:standard \ foo))

       ;; (let ((newseq (srcs->module-names srcfiles))) ;;  direct
       ;;    ;; (format #t "modules :STANDARD ~A\n" newseq)
       ;;    ;; (format #t "CDRMODS ~A\n" (cdr modules))
       ;;    (recur (cdr modules)
       ;;           (append newseq direct)
       ;;           indirect))

       ;; )

       ;; (concatenate direct
       ;;              (norm-std-modules (cdr modules))))
       ((pair? (car modules)) ;; e.g. ???
        ;; (format #t "TTTT ~A\n" (pair? (car modules)))
        ;; e.g. ? (modules foo (bar) baz)?
        ;; (let-values (((exp gen)
        ;;               (recur (car modules)))) ;;  '() '())))
        (let ((tbl (recur (car modules)))) ;;  '() '())))
          (recur (cdr modules))))
                 ;; (concatenate exp direct)
                 ;; (concatenate gen indirect))))

       ((indirect-module-dep? (car modules) srcfiles)
        ;; module name has no corresponding file
        (begin
          ;; (format #t "INDIRECT: ~A\n" (car modules))
          (hash-table-set! modules-tbl
                           (car modules) :indirect)
          (recur (cdr modules))))
                 ;; direct
                 ;; (cons (car modules) indirect))))

       (else
        (hash-table-set! modules-tbl
                         (normalize-module-name (car modules))
                         :direct)
        ;; (format #t "yyyy ~A\n" modules)
        (recur (cdr modules))))
               ;; (cons (car modules) direct)
       ;; indirect))
       ))
          ;;      ))
          ;; ;;(format #t "RESULT: ~A\n" result)
          ;; (reverse result))
  )
 ;; ))
     ;;      (norm-std-modules modules)
     ;;      (car modules)))
     ;;  (cons :modules (list (car modules)))
     ;; (else (cons :modules modules)))))

;; (display "loaded dune/lookup_tables.scm") (newline)
