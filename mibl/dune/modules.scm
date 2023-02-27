(if *debugging*
    (format #t "loading modules.scm\n"))

;; mibl/dune/modules.scm

;;(define (  get-manifest pkg kind wrapped? stanza-alist) ;;  deps
(define (x-get-manifest pkg kind wrapped? stanza-alist spec) ;;  deps
  (if *debugging*
      (begin
        (format #t "~A: ~A\n" (ublue "x-get-manifest") stanza-alist)
        ;; (format #t "~A: ~A\n" (uwhite "pkg") pkg)
        (format #t "~A: ~A\n" (blue "spec") spec)))
  ;; (if deps
  (let* ((submods+sigs-list
          (modules-fld->submodules-fld kind
           ;; (assoc 'modules stanza-alist)
           spec
           ;; files
           (assoc :modules pkg)
           ;; deps
           (assoc :signatures pkg)
           (assoc :structures pkg)))
         (submods+sigs-list (if (equal? (cadr submods+sigs-list)
                                     '(:signatures))
                                (list (car submods+sigs-list))
                                submods+sigs-list))
         (submods+sigs-list (if (equal? (cdr submods+sigs-list)
                                     '(:modules))
                                (cdr submods+sigs-list)
                                ;; (sort! (cdr submods+sigs-list) sym<?)
                                submods+sigs-list)))
    (if *debugging*
        (format #t "~A: ~A\n" (uwhite "submods+sigs-list") submods+sigs-list))
    (if (null? submods+sigs-list)
        '()
        (cons :manifest
              (remove () submods+sigs-list)))))

(define (get-manifest pkg kind wrapped? stanza-alist) ;;  deps
  (if *debugging*
      (begin
        (format #t "~A: ~A\n" (ublue "get-manifest") stanza-alist)
        (format #t "~A: ~A\n" "pkg" pkg)
        (format #t "~A: ~A\n" "kind" kind)))
  ;; (if deps
  (let* ((submods+sigs-list
          (modules-fld->submodules-fld
           kind
           (assoc 'modules stanza-alist)
           ;; files
           (assoc :modules pkg)
           ;; deps
           (assoc :signatures pkg)
           (assoc :structures pkg)))
         (submods+sigs-list (if (equal? (cadr submods+sigs-list)
                                     '(:signatures))
                                (list (car submods+sigs-list))
                                submods+sigs-list))
         (submods+sigs-list (if (equal? (cdr submods+sigs-list)
                                     '(:modules))
                                (cdr submods+sigs-list)
                                ;; (cdr submods+sigs-list)
                                submods+sigs-list)))
    (if *debugging*
        (format #t "~A: ~A\n" (uwhite "submods+sigs-list") submods+sigs-list))
    (if (null? submods+sigs-list)
        '()
        (cons :manifest
              (remove () submods+sigs-list)))))
            ;; (let ((submods (reverse (car submods+sigs-list)))
            ;;       (subsigs (reverse (cdr submods+sigs-list))))
            ;;   (cons :manifest (remove '()
            ;;                            (list submods subsigs)))))))

;; only used for generated files, so returns :ml_, :mli_
(define (filename->module-assoc filename)
  (if *debugging*
      (format #t "~A: ~A\n" (ublue "filename->module-assoc") filename))
  (let* ((ext (filename-extension filename))
         (pname (principal-name filename))
         (mname (normalize-module-name pname)))
    ;; (format #t "mname: ~A\n" mname)
    (let ((a
           (cond
            ((string=? ext ".mli") (cons mname
                                         (list (cons :mli_ filename))))
            ((string=? ext ".ml") (cons mname
                                        (list (cons :ml_ filename))))
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
;;   (:modules (A (:ml "a.ml") (B (:ml_ "b.ml"))))
;;   (:structures (A . "a.ml") (B . "b.ml"))
;;   (:signatures (A . "a.mli") (B . "b.mli"))
;; (define (expand-std-modules modules-spec pkg-modules)
(define get-module-names
  (let ((+documentation+ "Returns module names from modules-assoc, which has the form ((:static (A (:ml a.ml) (:mli a.mli)...) (:dynamic ...))")
        (+signature+ '(get-module-names modules-alist)))
    (lambda (modules-alist)
      (if *debugging*
          (format #t "~A: ~A\n" (blue "get-module-names") modules-alist))
      (if modules-alist
          (case (car modules-alist)
            ((:modules)
             ;; (format #t "~A~%" (red ":MODULES"))
             (let ((mods (map car (cdr modules-alist))))
               ;; (format #t "mods: ~A~%" mods)
               mods))
             ;; (let* ((modules (if-let ((ms (assoc-val :modules modules-alist)))
             ;;                         (begin
             ;;                           (format #t "~A: ~A~%" (red "ms") ms)
             ;;                           (map car ms))
             ;;                         '())))
             ;;   modules))

            ((:structures)
             ;; (format #t "~A: ~A~%" (red ":STRUCTURES") (cdr modules-alist))
             (let* ((statics (if-let ((ms (assoc-val :static (cdr modules-alist))))
                                     (begin
                                       ;; (format #t "~A: ~A~%" (red "stuctstatic") ms)
                                       (map car ms))
                                     '()))
                    (dynamics (if-let ((ms (assoc-val :dynamic (cdr modules-alist))))
                                      (begin
                                        ;; (format #t "~A: ~A~%" (red "stuctdyn") ms)
                                        (map car ms))
                                      '()))
                    (structs (flatten (concatenate statics dynamics))))
               ;; (format #t "structs: ~A~%" structs)
               structs))

            ((:signatures)
             ;; (format #t "~A: ~A~%" (red ":SIGNATURES") (cdr modules-alist))
             (let ((sigs (if-let ((ms (assoc-val :static (cdr modules-alist))))
                                    (begin
                                      ;; (format #t "~A: ~A~%" (red "sigstatic") ms)
                                      (map car ms))
                                    '())))
               ;; (format #t "sigs: ~A~%" sigs)
               sigs))

            (else
             (error 'fixme "modules-alist missing key")))
          ;;  else no arg
          '()))))
          ;;   (format #t "modules: ~A\n" modules)
          ;;   modules)
          ;; '()))))

;;;;; expand dune constant ':standard' for modules
;; e.g.
;; src/proto_alpha/lib_parmeters: (modules :standard \ gen)
;; lib_client_base:: (modules (:standard bip39_english))

;; (define (expand-std-modules modules srcfiles)

(define (resolve-gentargets gentargets sigs structs)
  (if *debugging*
      (format #t "resolve-gentargets: ~A\n" gentargets))
  (let ((resolved (map (lambda (f)
                         (if *debugging*
                             (format #t "f: ~A\n" f))
                         (let* ((fname (if (symbol? f) (symbol->string f) f))
                                (type (if (eq? 0 (fnmatch "*.mli" fname 0))
                                          :mli :ml))
                                (mname (filename->module-name fname)))
                           (if *debugging*
                               (begin
                                 (format #t "mname: ~A\n" mname)
                                 (format #t "type: ~A\n" type)))
                           (if (eq? type :ml)
                               (if-let ((sigmatch (assoc-in `(:static ,mname)
                                                            sigs)))
                                       (begin
                                         ;; remove from pkg :signatures
                                         (alist-update-in!
                                          sigs `(:static)
                                          (lambda (old)
                                            (if *debugging*
                                                (format #t "old static: ~A\n" old))
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
                                            (if *debugging*
                                                (format #t "old static: ~A\n" old))
                                            (dissoc `(,mname) old)))
                                         `(:_ ,(car structmatch)))
                                       `(:mli ,mname)))))
                       gentargets)))
    resolved))

;; std-list arg: everything after :standard, e.g. (:standard \ foo)
;; WARNING: the '\' is a symbol, but it does not print as '\,
;; rather it prints as (symbol "\\"); use same to compare, do
;; not compare car to 'symbol, like so:
;; (if (not (null? modules))
;;     (if (equal? (car modules) (symbol "\\"))
;;         (format #t "EXCEPTING ~A\n" (cdr modules))))

;; handling '/': in principle it can go anywhere:
;; (<sets1> \ <sets2>) is how the docs put it.
;; in practice it only seems to be used after :standard
;; e.g. (:standard \ foo) includes all modules except foo
;; also found: (modules (:standard) \ foo)
(define expand-std-modules
  (let ((+documentation+ "expands a ':standard' part of a (modules :standard ...) clause. std-list: the ':standard' clause and any modifiers.  pkg-modules: list of source modules (paired .ml/.mli files), from mibl :modules fld; module-deps: :deps from 'libraries' field and possibly :conditionals (if 'select' is used). :conditionals contains (LHS -> RHS) clauses.")
        (+signature+ '(expand-std-modules std-list pkg-modules module-deps sigs structs)))
    ;; modules-ht)))
    (lambda (std-list pkg-modules sigs structs) ;;  module-deps
      (if *debugging*
          (begin
            (format #t "~A: ~A\n" (blue "EXPAND-std-modules") std-list)
            (format #t " pkg-modules: ~A\n" pkg-modules)
            ;; (format #t " module-deps: ~A\n" module-deps)
            (format #t " sigs: ~A\n" sigs)
            (format #t " structs: ~A\n" structs)))
      (let* ((modifiers (cdr std-list)) ;; car is always :standard
             (pkg-module-names (if pkg-modules
                                   (get-module-names pkg-modules)
                                   '()))
             (struct-module-names (get-module-names structs))
             (pkg-module-names (if struct-module-names
                                   (append struct-module-names
                                           pkg-module-names)
                                   pkg-module-names))
             (sig-module-names (get-module-names sigs))
             ;; (pkg-module-names (if sig-module-names
             ;;                       (append sig-module-names
             ;;                               pkg-module-names)
             ;;                       pkg-module-names)

             ;; FIXME: encode conditionals in sigs & structs pkg flds
             ;; as :dynamic
             ;; (so we need not pass deps around to expand std modules)

             ;; (conditionals (assoc :conditionals module-deps))
             )
        (if *debugging*
            (begin
              (format #t "pkg-module-names: ~A\n" pkg-module-names)
              (format #t "sig-module-names: ~A\n" sig-module-names)
              (format #t "modifiers: ~A\n" modifiers)
              ;; (format #t "conditionals: ~A\n" conditionals)
              ))
        (if-let ((slash (member (symbol "\\") modifiers)))
                (let* ((exclusions (cdr slash))
                       (exclusions (if (list? (car exclusions))
                                       (car exclusions) exclusions))
                       (exclusions (map normalize-module-name exclusions)))
                  (if *debugging*
                      (format #t "exclusions: ~A\n" exclusions))
                  (let ((winnowed (remove-if
                                   list
                                   (lambda (item)
                                     (let ((norm (normalize-module-name item)))
                                       ;; (format #t "item ~A\n" norm)
                                       ;; (format #t "mem? ~A: ~A\n" exclusions
                                       ;;         (member norm exclusions))
                                       (if (member norm exclusions) #t #f)))
                                   pkg-module-names)))
                    ;; returning
                    (values winnowed sigs)))
                ;; else no explicit exclusions, but, select apodoses
                ;; always excluded:
                (values pkg-module-names sig-module-names))
        ))))

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
  (let ((+documentation+ "Expand  'modules' field (of library or executable stanzas) and convert to pair of :submodules :subsigs assocs. modules-spec is a '(modules ...)' field from a library stanza; pkg-modules is the list of modules in the package: an alist whose assocs have the form (A (:ml a.ml)(:mli a.mli)), i.e. keys are module names.")
        (+signature+ '(kind modules-fld->submodules-fld modules-spec pkg-modules pkg-sigs pkg-structs))) ;;  modules-deps
        ;; modules-ht)))
    (lambda (kind modules-spec pkg-modules pkg-sigs pkg-structs)
      (if *debugging*
          (begin
            (format #t "~A\n" (ublue "modules-fld->submodules-fld"))
            (format #t "modules-spec: ~A\n" modules-spec)
            (format #t "pkg-modules: ~A\n" pkg-modules)
            ;; (format #t "deps: ~A\n" deps)
            (format #t "pkg-sigs: ~A\n" pkg-sigs)
            (format #t "pkg-structs: ~A\n" pkg-structs)))
      (if (equal? modules-spec '(modules))
          ;; exclude all modules (lib and exe)
          '((:modules) (:signatures))
          (if (or pkg-modules pkg-structs)
              (if modules-spec
                  (let* ((modules-spec (map normalize-module-name
                                            (cdr modules-spec)))
                         (pkg-module-names (if pkg-modules
                                               (get-module-names
                                                pkg-modules)
                                               '()))
                         (struct-module-names (get-module-names pkg-structs))
                         ;; (_ (if *debugging* (format #t "struct-module-names:: ~A\n" struct-module-names)))
                         (pkg-module-names (if struct-module-names
                                               (append struct-module-names
                                                       pkg-module-names)
                                               pkg-module-names))
                         ;; (_ (if *debugging* (format #t "pkg-module-names:: ~A\n" pkg-module-names)))
                         (sig-module-names (get-module-names pkg-sigs))
                         ;; (_ (if *debugging* (format #t "sig-module-names:: ~A\n" sig-module-names)))
                         ;; (_ (if *debugging* (format #t "modules-spec:: ~A\n" modules-spec)))
                         (tmp (let recur ((modules-spec modules-spec)
                                          (submods '())
                                          (subsigs '()))
                                ;; (format #t "RECUR modules-spec ~A\n" modules-spec)
                                ;; (format #t "  submods: ~A\n" submods)
                                ;; (format #t "  subsigs: ~A\n" subsigs)

                                (cond
                                 ((null? modules-spec)
                                  (if (null? submods)
                                      (begin
                                        ;; (format #t "null modules-spec\n")
                                        '())
                                      (begin
                                        ;; (format #t "DONE\n")
                                        (list
                                         (cons :modules (reverse submods))
                                         (if (null? subsigs)
                                             '() (cons :signatures subsigs))))))
                                 ;; (reverse submods)

                                 ((pair? (car modules-spec))
                                  (begin
                                    ;; (format #t "(pair? (car modules-spec))\n")
                                    ;; e.g. ((:standard)) ?
                                    ;; or (modules (:standard) \ foo)
                                    ;; or (A B C)
                                    ;; just unroll and recur
                                    (if (equal? '(:standard) (car modules-spec))
                                        (modules-fld->submodules-fld kind
                                         (append
                                          (list 'modules :standard) (cdr modules-spec))
                                         pkg-modules
                                         ;; deps
                                         pkg-sigs pkg-structs)
                                        (modules-fld->submodules-fld kind
                                         (cons
                                          'modules (car modules-spec))
                                         pkg-modules
                                         ;; deps
                                         pkg-sigs
                                         pkg-structs))))

                                 ((equal? :standard (car modules-spec))
                                  ;; e.g. (modules :standard ...)
                                  (begin
                                    ;; (format #t "modules-spec contains :standard\n")
                                    (let-values (((mods-expanded sigs-expanded)
                                                  (expand-std-modules
                                                   modules-spec
                                                   pkg-modules pkg-sigs pkg-structs)))
                                      ;; (format #t "mods-expanded: ~A\n" mods-expanded)
                                      ;; (format #t "sigs-expanded: ~A\n" sigs-expanded)
                                      ;; (format #t "updated pkg: ~A\n" pkg)
                                      ;; (error 'tmp "tmp")

                                      (list
                                       (cons :modules (reverse mods-expanded))
                                       (if sigs-expanded
                                           (cons :signatures
                                                 (reverse sigs-expanded))
                                           '())))))

                                 ;; inclusions, e.g. (modules a b c)
                                 (else
                                  (begin
                                    ;; (format #t "inclusions: ~A\n" modules-spec)
                                    ;; (format #t "pkg-modules: ~A\n" pkg-module-names)
                                    ;; (format #t "sig-modules: ~A\n" sig-module-names)
                                    (if (member (car modules-spec) pkg-module-names)
                                        (recur (cdr modules-spec)
                                               (cons (car modules-spec) submods)
                                               subsigs)
                                        (if (member (car modules-spec) sig-module-names)
                                            (recur (cdr modules-spec)
                                                   submods
                                                   (cons (car modules-spec) subsigs))
                                            (recur (cdr modules-spec)
                                                   (cons :FOOBAR submods)
                                                   subsigs)
                                            ;; (error 'bad-arg (format #f "included module not in list: ~A"
                                            ;;                         (car modules-spec)))
                                            ))))
                                 ) ;; cond
                                ))) ;; recur
                    tmp) ;; let*
                  ;; no modules-spec - default is all (lib) or none (exe)
                  (begin
                    (if *debugging*
                        (format #t "~A~%" (bgred "no modules-spec")))
                    (if (equal? kind :exe)
                        ;; exclude all
                        '((:modules) (:signatures))
                        (let* ((pkg-module-names (if pkg-modules
                                                 (get-module-names
                                                  pkg-modules)
                                                 '()))
                           ;; (_ (if *debugging* (format #t "~A: ~A\n" (white "pkg-module-names") pkg-module-names)))
                           (struct-module-names (get-module-names pkg-structs))
                           ;; (_ (if *debugging* (format #t "struct-module-names:: ~A\n" struct-module-names)))
                           (all-module-names (if struct-module-names
                                                 (append struct-module-names
                                                         pkg-module-names)
                                                 pkg-module-names))
                           ;; (_ (if *debugging* (format #t "all module-names:: ~A\n" all-module-names)))
                           (sig-module-names (get-module-names pkg-sigs))
                           ;; (_ (if *debugging* (format #t "sig-module-names:: ~A\n" sig-module-names)))
                           )

                      (list
                       (cons :modules all-module-names)
                       (if (null? sig-module-names)
                           '() (cons :signatures sig-module-names))))))
                  ) ;; if modules-spec
              (begin
                (if *debugging*
                    (format #t "modules-spec but no pkg modules nor structs\n"))
                '((:modules) (:signatures)))))
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
