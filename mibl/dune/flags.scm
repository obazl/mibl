
;; WARNING: in principle we could see '(flags (:standard \ ...))'.
;; Haven't seen it in the wild, but we have seen:
;;     (modules (:standard \ ...)
;;     (dirs (:standard \ ...)

;; '(:standard \ ...) converts to:
;; '(:standard) (:exclusions ...)
;; e.g. (flags (:standard \ -flaga) -flagb -open amod ...)

;; split opts into boolean flags and (opt arg) pairs
(define (split-opts opts)
  (format #t "splitting opts: ~A\n" opts)
  ;; assumption: :standard has been removed
  ;; cases: arg is list or not: (flags (a b ...)) v. (flags a b ...)
  ;; case: embedded list, e.g. (flags a (b c) ...)

  ;; logic: if arg with leading '-' is followed by another with '-',
  ;; then its a boolean flag
  (let recur ((opts opts)
              (ostack '()) ;; option stack
              (options '())
              (flags '())
              (orphans '()))
    (format #t "opts: ~A\n" opts)
    (format #t "ostack: ~A\n" ostack)
    (if (null? opts)
        (if (null? ostack)
            (values options flags)
            (values options (cons (symbol(car ostack)) flags)))
        (let* ((rawopt (car opts))
               (_ (format #t "rawopt: ~A\n" rawopt))
               (opt (cond
                     ((string? rawopt) rawopt)
                     ((symbol? rawopt) (symbol->string rawopt))
                     ((number? rawopt) rawopt)
                     (else :unknown))))
          ;; (_ (format #t "OPT: ~A (type ~A)\n" opt (type-of opt)))
          (if (number? opt)
              (if (null? ostack)
                  (begin
                    (format #t "WARNING: numeric opt ~A without predecing optname\n" opt)
                    (recur (cdr opts)
                           ostack options flags (cons opt orphans)))
                  (recur (cdr opts) '()
                         (cons (cons (symbol (car ostack)) rawopt) options)
                         flags orphans))
              (if (string-prefix? "-" opt)
                  (if (null? ostack)
                      (recur (cdr opts)
                             (cons opt ostack) options flags orphans)
                      ;; prev must be a flag, new goes on ostack
                      (recur (cdr opts)
                             (list opt) options
                             (cons (symbol (car ostack)) flags) orphans))
                  ;; no '-', must be an opt val
                  (if (null? ostack)
                      (if (equal? (symbol "\\") rawopt)
                          (begin
                            (format #t "Got slash\n")
                            (recur (cdr opts) ostack options flags
                                   (cons opt orphans)))
                          (begin
                            (format #t
                                    "WARNING: value ~A without preceding -opt\n"
                                    opt)
                            (format #t "type: ~A\n" (type-of opt))
                            (recur (cdr opts) ostack options flags
                                   (cons opt orphans))))
                      ;; no '-' prefix, ostack contains prev '-' arg
                      (recur (cdr opts) '()
                             (cons (cons (symbol (car ostack)) rawopt) options)
                             flags orphans))))))))

(define (split-opens flags)
  (format #t "split-opens: ~A\n" flags)
  ;; WARNING: preserve order of '-open' args!
  (let recur ((flags flags)
              (opens '())
              (opts  '())
              (std  #f))
    (if (null? flags)
        (values opens opts std)
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
;; FIXME: check for car = flags or library_flags?
(define (normalize-stanza-fld-flags flags type)
  (format #t "normalize-stanza-fld-flags: ~A\n" flags)
  (if flags
      ;; (let* ((flags (if (list? (cadr flags))
      ;;                   (cadr flags)
      ;;                   (list (cdr flags))))
      (let* ((flags-val (cdr flags))
             ;; FIXME: expand :standard
             ;; e.g. src/lib_store/legacy_store:
             ;;     (modules (:standard \ legacy_store_builder))
             (top-std (any (lambda (flag) (equal? flag :standard))
                           flags-val))
             (clean-flags (if top-std
                              (remove :item :standard flags-val)
                              flags-val)))
        ;; (format #t "DIRTY: ~A\n" flags-val)
        ;; (format #t "STD: ~A\n" std)
        ;; (format #t "CLEAN: ~A\n" clean-flags)
        (let-values (((opens opts std) (split-opens clean-flags)))
          (let-values (((options bools) (split-opts (reverse opts))))
            ;; (format #t "OPENS: ~A\n" (reverse opens))
            ;; (format #t "OPTS: ~A\n" (reverse opts))
            ;; (format #t "STD: ~A\n" std)
            (format #t "OPTIONS: ~A\n" options)
            (format #t "FLAGS: ~A\n" bools)
            (list (if (eq? type :mod) :module-opts
                      (if (eq? type :lib) :archive-opts
                          :unknown-opts))
                  (concatenate
                   (if (or top-std std)
                       '((:standard)) '()) ;; FIXME: expand :standard flags
                   (if (null? opens) '()
                       (list (cons :opens (reverse opens))))
                   (if (null? options) '()
                       (list (cons :options (reverse options))))
                   (if (null? bools) '()
                       (list (cons :flags (reverse bools))))
                   `((:raw ,flags))
                   )))))
      #f))

;; (format #t "loaded: mibl/dune/flags.scm\n")

