(if *mibl-debug-s7-loads*
    (format #t "loading: dune/flags.scm\n"))

;; WARNING: in principle we could see '(flags (:standard \ ...))'.
;; Haven't seen it in the wild, but we have seen:
;;     (modules (:standard \ ...)
;;     (dirs (:standard \ ...)

;; '(:standard \ ...) converts to:
;; '(:standard) (:exclusions ...)
;; e.g. (flags (:standard \ -flaga) -flagb -open amod ...)

;; split opts into boolean flags and (opt arg) pairs
(define (split-opts opts)
  (if *mibl-debugging*
      (format #t "~A: ~A\n" (ublue "split-opts") opts))
  ;; assumption: :standard has been removed
  ;; cases: arg is list or not: (flags (a b ...)) v. (flags a b ...)
  ;; case: embedded list, e.g. (flags a (b c) ...)

  ;; logic: if arg with leading '-' is followed by another with '-',
  ;; then its a boolean flag EXCEPT warning options e.g. -w -7-37
  (let recur ((opts opts)
              (ostack '()) ;; option stack - at most one elt?
              (options '())
              (flags '())
              (orphans '()))
    (if *mibl-debugging*
        (begin
          (format #t "~A: ~A\n" (green "opts") opts)
          (format #t "~A: ~A\n" (green "ostack") ostack)))
    (if (null? opts)
        (if (null? ostack)
            (values options flags (reverse orphans))
            (values options (cons (symbol(car ostack)) flags) (reverse orphans)))
        (let* ((rawopt (car opts))
               ;; (_ (format #t "rawopt: ~A\n" rawopt))
               (opt (cond
                     ((string? rawopt) rawopt)
                     ((symbol? rawopt) (symbol->string rawopt))
                     ((number? rawopt) rawopt)
                     (else :unknown))))
          ;; (_ (format #t "OPT: ~A (type ~A)\n" opt (type-of opt)))
          (if (number? opt)
              (if (null? ostack)
                  (begin
                    (if *mibl-debugging*
                        (format #t "WARNING: numeric opt ~A without predecing optname\n" opt))
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
                      (begin
                        (if *mibl-debugging*
                            (format #t "~A: current: ~A, prev: ~A~%"
                                (bgred "hyphen prev") opt (car ostack)))
                        ;; special case: dashed warning nbrs following -w, e.g. -w -27
                        (if (or (equal? (car ostack) "-w")
                                (string-prefix? "-" (format #f (car ostack))))
                            ;; current is arg to prev -<opt>
                            (begin
                              (if *mibl-debugging*
                                  (begin
                                    (format #t "~A~%" (red "XXXXXXXXXXXXXXXX"))
                                    (format #t "~A: ~A~%" (red "ostack") ostack)
                                    (format #t "~A: ~A~%" (red "options") options)
                                    (format #t "~A: ~A~%" (red "flags") flags)
                                    (format #t "~A: ~A~%" (red "orphans") orphans)
                                    (format #t "~A: ~A~%" (red "opts") opts)))
                              (let ((prev (car ostack)))
                                (recur (cdr opts)
                                       (cons opt (cdr ostack)) ;; new opt replaces prev on stack
                                       options
                                       (cons prev flags)
                                       orphans)))
                            ;; else prev is flag, push current to ostack
                            (begin
                              (if *mibl-debugging*
                                  (format #t "~A~%" (green "ZZZZZZZZZZZZZZZZ")))
                              (recur (cdr opts)
                                     (list opt) ;; ostack
                                     options
                                     (cons (symbol (car ostack)) flags)
                                     orphans)))))
                  ;; no '-', must be an opt val
                  (if (null? ostack)
                      ;; no preceding -opt
                      (if (equal? (symbol "\\") rawopt)
                          (begin
                            ;; (format #t "Got slash\n")
                            ;; remaining args excluded
                            (recur '() ostack options flags
                                   (append orphans (cdr opts))))
                          (begin
                            (if *mibl-debugging*
                                (format #t
                                    "WARNING: value ~A without preceding -opt\n"
                                    opt))
                            ;; (format #t "type: ~A\n" (type-of opt))
                            (recur (cdr opts) ostack options flags
                                   (cons opt orphans))))
                      ;; no '-' prefix, ostack contains prev '-' arg
                      (recur (cdr opts) '()
                             (cons (cons (symbol (car ostack)) rawopt) options)
                             flags orphans))))))))

(define (split-opens flags)
  ;; (format #t "~A: ~A\n" (uwhite "split-opens") flags)
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

;; returns: (values std flags)
(define (link-flags->mibl stanza)
  ;; (format #t "~A: ~A\n" (ublue "executable-flags->mibl") stanza)
  (let* ((stanza-alist (cdr stanza))
         (link-flags (assoc-in '(:link :link-flags) stanza-alist))
         (link-opts  (assoc-in '(:link :common-opts :flags) stanza-alist))
         (link-std   (assoc-in '(:link :common-opts :standard) stanza-alist))
         (flags (remove '()
                        (append
                         (if link-flags (cdr link-flags) '())
                         (if link-opts (cdr link-opts) '())))))
    (values link-std flags)))

(define (normalize-stanza-fld-flags flags kind)
  (if *mibl-debugging*
      (format #t "~A: ~A\n" (ublue "normalize-stanza-fld-flags") flags))
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
                              (remove #|:item|# :standard flags-val)
                              flags-val)))
        ;; (format #t "DIRTY: ~A\n" flags-val)
        ;; (format #t "STD: ~A\n" std)
        ;; (format #t "CLEAN: ~A\n" clean-flags)
        (let-values (((opens opts std) (split-opens clean-flags)))
          (let-values (((options bools orphans) (split-opts (reverse opts))))
            ;; (format #t "OPENS: ~A\n" (reverse opens))
            ;; (format #t "OPTS: ~A\n" (reverse opts))
            ;; (format #t "STD: ~A\n" std)
            ;; (format #t "OPTIONS: ~A\n" options)
            ;; (format #t "FLAGS: ~A\n" bools)
            (cons
             (case kind
               ((:common) :opts)
               ((:compile) :opts)
               ((:ocamlc) :ocamlc-opts)
               ((:ocamlopt) :ocamlopt-opts)
               ((:archive) :archive-opts)
               ((:link) :link-opts)
               ((:runtime) :runtime-opts)
               ((:exec) :exec-opts)
               (else :unknown-opts))
            ;; (cons (if (eq? kind :compile) :opts
            ;;           (if (eq? kind :archive) :archive-opts
            ;;               (if (eq? kind :exec) :exec-opts
            ;;                   :unknown-opts)))
                  (remove
                   '() (list
                        (if (or top-std std)
                            '(:standard) '()) ;; FIXME: expand :standard flags
                        (if (null? opens) '()
                            (cons :opens (reverse opens)))
                        (if (null? options) '()
                            (cons :options (reverse options)))
                        (if (null? bools) '()
                            (cons :flags (reverse bools)))
                        (if (null? orphans) '()
                            (cons :exclusions (reverse orphans)))
                        ;; `((:raw ,flags))
                        ))))))
      #f))

;; returns (values <standard> <opens> <options> <flags>)
(define (flags->mibl flags)
  ;; (format #t "~A: ~A\n" (ublue "flags->mibl") flags)
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
        ;; (format #t "STD: ~A\n" top-std)
        ;; (format #t "CLEAN: ~A\n" clean-flags)
        (let-values (((opens opts std) (split-opens clean-flags)))
          (let-values (((options bools orphans) (split-opts (reverse opts))))
            ;; (format #t "OPENS: ~A\n" (reverse opens))
            ;; (format #t "OPTS: ~A\n" (reverse opts))
            ;; (format #t "STD: ~A\n" std)
            ;; (format #t "OPTIONS: ~A\n" options)
            ;; (format #t "FLAGS: ~A\n" bools)
            ;; FIXME: expand :standard flags
            (values
             (if (or top-std std) '(:standard) '())
             (if (null? opens) '()
                 (cons :opens (reverse opens)))
             (if (null? options) '()
                 (cons :options (reverse options)))
             (if (null? bools) '()
                 (cons :flags (reverse bools)))))))
        ;; else no flags
        (values '() '() '() '())))

(if *mibl-debug-s7-loads*
    (format #t "loaded: dune/flags.scm\n"))

