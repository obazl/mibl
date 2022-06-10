;; (display "dune/dune_stanza_fields.scm loading ...") (newline)

;; (load "opam.scm")
;; (load "string.scm")
;; (load "srfi.scm")
;; (load "utils.scm")
;; (require pp.scm)

;;fld: (name tezos_sapling)
;; FIXME: remove
(define (normalize-stanza-fld-name pkg-path privname stanza-alist)
  ;; (format #t "normalize-stanza-fld-name ~A\n" privname)
  ;; (format #t "    stanza-alist: ~A\n" stanza-alist)
  (let ((pubname (assoc 'public_name stanza-alist)))
    (if pubname
        (begin
          ;; (update-public-exe-table pkg-path
          ;;                          (cadr pubname)
          ;;                          (cadr pubname))
          ;; (update-public-exe-table pkg-path
          ;;                          (cadr privname)
          ;;                          (cadr pubname))
          `(:name ((:private ,(cadr privname))
                   (:public ,(cadr pubname)))))
        (begin
          ;; (update-public-exe-table pkg-path (cadr privname)
          ;;                          (cadr privname))
          `(:name ((:private ,(cadr privname))))))))
  ;; `(:name ((:private ,fld)
  ;;          (:module ,(normalize-module-name fld)))))

(define (normalize-stanza-fld-names fld)
  ;; (format #t "normalize-stanza-fld-names ~A\n" fld)
  (if (> (length fld) 1)
      (list :names fld)
      `(:name ((:private ,(car fld))
               (:module ,(normalize-module-name (car fld)))))))
 ;; (list :name (cons (car fld) (normalize-module-name (car fld))))))

;;fld: (public_name tezos-sapling)
(define (normalize-stanza-fld-public_name fld)
  ;; (format #t "normalize-stanza-fld-public_name ~A\n" fld)
  (list :public_name fld))

;;fld: (libraries ctypes ctypes.foreign ctypes.stubs hex data-encoding tezos-crypto tezos-stdlib tezos-error-monad tezos-lwt-result-stdlib)
;; tezos/src/lib_signer_backends/unix:
;; (libraries ocplib-endian.bigstring
;;            ... etc. ...
;;            (select ledger.ml from
;;              (ledgerwallet-tezos -> ledger.available.ml)
;;              (-> ledger.none.ml)))
;;  [why is a .ml file in a 'libraries' field?
(define (analyze-select deps directs selects)
  ;; FIXME: extract module dep from select sexp and add to directs
  directs)

(define (analyze-libdeps libdeps)
  ;; (format #t "analyze-libdeps: ~A\n" libdeps)
  (let recur ((deps libdeps)
              (directs '()) ;; directs == public_name
              (selects '())
              (modules '()))
    (if (null? deps)
        (values directs selects modules)
        (if (pair? (car deps))
            ;; e.g. (select ...)
            (if (equal? (caar deps) 'select)
                (recur (cdr deps)
                       (analyze-select deps directs selects)
                       (cons (car deps) selects)
                       (libdep->module-name modules))
                (error 'bad-lib-dep "embedded pair whose car is not 'select'"))
            (if (equal? 'libraries (car deps))
                ;; skip the initial fld name "libraries"
                (recur (cdr deps)
                       directs selects
                       (libdep->module-name modules))
                (recur (cdr deps)
                       (cons (car deps) directs)
                       selects
                       (libdep->module-name modules)))))))

(define (normalize-stanza-fld-libraries fld)
  ;; (format #t "normalize-stanza-fld-libraries: ~A\n" fld)
  (let-values (((directs selects modules) (analyze-libdeps fld)))
    ;; (for-each (lambda (module)
    ;;             (update-opam-table module))
    ;;           directs)
    (list :deps
          (list (list :raw fld)
                (list :contingent (reverse selects))
                ;;(list :modules (reverse modules))
                (list :constant (reverse directs))))))

;; (modules Registerer), (modules (:standard \ legacy_store_builder))
;; (modules)
;; (modules (:standard (symbol "\\") delegate_commands delegate_commands_registration))
;; (define (normalize-stanza-fld-modules fld)
;;   ;; (format #t "normalize-stanza-fld-modules: ~A\n" fld)
;;   ;; (cons 'modules
;;   ;;       (if (list? (cadr fld))
;;   ;;           (cdr fld)
;;   ;;           (list (cdr fld)))))
;;   fld)

;; e.g. (cdr (:standard (symbol "\\") legacy_store_builder))
(define (srcs->module-names srcfiles) ;; seq)
  ;; (format #t "srcs->module-names: ~A => ~A\n" srcfiles seq)
  (let recur ((srcfiles srcfiles)
              (modnames '()))
    (if (null? srcfiles)
        modnames
        (let ((m (file-name->module-name (car srcfiles))))
          (if (member m modnames) ;; avoid .ml/.mli dups
              (recur (cdr srcfiles) modnames)
              (recur (cdr srcfiles) (cons m modnames)))))))

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
        ;; all modules
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fields:  foreign_stubs (src files); foreign_archives (precompiled imports)
;; stanza type: foreign_library - dune builds a c archive
;; eg: (foreign_stubs (language c) (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs)) (names rustzcash_ctypes_c_stubs))
(define (normalize-stanza-fld-foreign_stubs fld)
  ;; (foreign_stubs...) is for foreign _sources_, compare foreign_archives
  ;; (display (format #f "FLD foreign_stubs: ~A" fld)) (newline)

  ;; subfields:
  ;; language
  ;; names - specifies the names of source files.
  ;; flags - passed when compiling source files.
  ;;  e.g. (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs))
  ;; include_dirs tracked as deps, passed to compiler via -I
  ;; extra_deps
  fld)

;;fld: (c_library_flags (-L%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs -lrustzcash -lpthread))
(define (normalize-stanza-fld-c_library_flags fld)
  fld)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  FLAGS
;; examples:
;; (flags (:standard -open Tezos_stdlib -open Tezos_crypto -open Tezos_error_monad -open Tezos_error_monad.TzLwtreslib))
;; (flags (-I%{env:OPAM_SWITCH_PREFIX=}/lib/tezos-rust-libs))
;; (flags -short-paths -g -w @a-4-29-40-41-42-44-45-48-58-59-60))
;; (flags :standard -short-paths -warn-error -58)
;; (flags -warn-error -22+34+33+27)
;; (flags (:standard -short-paths -cclib -ljemalloc -w @a-4-29-40-41-42-44-45-48-58-59-60-66))
;; (flags :standard -lstdc++ -lpthread))
;; (flags --explain --unused-tokens)
;; (flags (:include ../../discover/dune.flags))
;; (js_of_ocaml (flags +nat.js --source-map --pretty))

;; normal form:
;; (flags ((:standard -linkall -opaque ...)
;;         (:open '(Tezos_stdlib Tezos_crypto ...)))
;;         ...???)

;; (:standard -open Tezos_stdlib -open Tezos_crypto -open
;; Data_encoding -open Tezos_error_monad -open
;; Tezos_error_monad.TzLwtreslib -open Tezos_rpc -open Tezos_clic
;; -open Tezos_micheline -open Tezos_event_logging)

(define (normalize-open opener)
  ;; truncate at '__', e.g. Tezos_base__TzPervasives => Tezos_base
  ;; why? whatever the submod depends on will be listed in its aggregator
  ;; (let ((ostr (symbol->string opener)))
  ;;   (if-let ((trunc-at (string-contains ostr "__")))
  ;;           (let ((result (string-take ostr trunc-at)))
  ;;             ;; (format #t "NORMED ~A => ~A\n" ostr result)
  ;;             result)
  ;;           ostr))
  opener)

(define (split-opens flags)
  ;; (format #t "split-opens: ~A\n" flags)
  ;; WARNING: preserve order of '-open' args!
  (let recur ((flags flags)
              (opens '())
              (opts  '())
              (std  #f))
    (if (null? flags)
        (values
         (reverse opens)
         opts std)
        (cond
         ((list? (car flags))
          (let-values (((-opens -opts -std) (split-opens (car flags))))
            (recur (cdr flags)
                   (concatenate -opens opens)
                   (concatenate -opts opts)
                   -std)))
         ((symbol? (car flags))
          (cond
           ((equal? (car flags) '-open)
            (recur (cddr flags)
                   (cons (normalize-open (cadr flags)) opens)
                   opts std))
           ((equal? (car flags) ':standard)
            (recur (cdr flags) opens opts #t))
           (else
            (recur (cdr flags) opens (cons (car flags) opts) std))))
         ((number? (car flags))
          ;; e.g. (flags (:standard -w -9 -nolabels))
          (recur (cdr flags) opens (cons (car flags) opts) std))
         (else
          ;; not symbol
          (if (string? (car flags))
              (if (string=? (car flags) "-open")
                  (recur (cddr flags)
                         (cons (normalize-open (cadr flags)) opens)
                         std)
                  (recur (cdr flags) opens (cons (car flags) opts)
                         std))
              ;; not symbol, not string
              (error 'bad-arg
                     (format #f "ERROR: unexpected flag type ~A"
                             flags))))))))

;; (flags :standard)
;; (flags (:standard -open Tezos_base__TzPervasives -open Tezos_micheline))
;; (:standard -linkall)
(define (normalize-stanza-fld-flags flags)
  (format #t "normalize-stanza-fld-flags: ~A\n" flags)
  (if flags
      ;; (let* ((flags (if (list? (cadr flags))
      ;;                   (cadr flags)
      ;;                   (list (cdr flags))))
      (let* ((flags (cdr flags))
             ;; FIXME: expand :standard
             ;; e.g. src/lib_store/legacy_store:
             ;;     (modules (:standard \ legacy_store_builder))
             (std (any (lambda (flag) (equal? flag :standard)) flags))
             (clean-flags (if std (remove :item :standard flags) flags)))
        ;; (format #t "DIRTY: ~A\n" flags)
        ;; (format #t "STD: ~A\n" std)
        ;; (format #t "CLEAN: ~A\n" clean-flags)
        (let-values (((opens opts std) (split-opens clean-flags)))
          ;; (format #t "OPENS: ~A\n" (reverse opens))
          ;; (format #t "OPTS: ~A\n" (reverse opts))
          ;; (format #t "STD: ~A\n" std)
          (list :opts
                (concatenate
                 (if std '((:standard)) '()) ;; FIXME: expand :standard flags
                 (if (null? opens) '() `((:opens ,opens)))
                 `((:raw (,flags)))
                 `((:flags ,(reverse opts)))))))
      #f))

;; library_flags
(define (normalize-stanza-fld-lib_flags lib-flags)
  ;; (format #t "normalize-stanza-fld-lib_flags: ~A" lib-flags)
  (if lib-flags
      (let ((lib-flags (if (list? (cadr lib-flags))
                       (cadr lib-flags)
                       (list (cdr lib-flags))))
            ;; FIXME: expand :standard
            (std (any (lambda (flag) (equal? flag :standard)) lib-flags)))
        (let-values (((opens opts std) (split-opens lib-flags)))
          ;; (format #t "  opens: ~A" opens)
          ;; (format #t "  opts: ~A"  opts)
          ;; (format #t "  std:  ~A"  std)
          (list :lib-flags
                (concatenate
                 (if std '((:standard)) '()) ;;FIXME: expand :standard flags
                 (if (null? opens) '() `((:opens ,opens)))
                 `((:raw ,lib-flags))
                 `((:flags ,@(if std
                                 (remove :item :standard lib-flags)
                                 lib-flags)))))))
      #f))

;; (display "loaded dune/dune_stanza_fields.scm") (newline)
