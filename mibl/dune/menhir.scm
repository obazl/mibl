(define (-split-menhir-opts options)
  (format #t "~A: ~A~%" (ublue "-split-menhir-opts") options)

  ;; --external-tokens Js_token
  ;; --unused-token TCommentLineDirective

  (let recur ((external-tokens '())
              (unused-tokens '())
              (other-options '())
              (opts options))
    (if (null? opts)
        (values external-tokens unused-tokens other-options)
        (let ((opt (car opts)))
          (case (car opt)
            ((--unused-token)
             (recur external-tokens
                    (cons (cdr opt) unused-tokens)
                    other-options (cdr opts)))
            ((--external-tokens)
             (recur (cons (cdr opt) external-tokens)
                    unused-tokens other-options (cdr opts)))
            (else
             (recur external-tokens unused-tokens
                    (cons opt other-options) (cdr opts)))))
           )))

(define (-normalize-menhir-fld-flags args)
  (format #t "~A: ~A\n" (ublue "-normalize-menhir-fld-flags") args)
  (let* ((top-std (any (lambda (flag) (equal? flag :standard))
                       args))
         (clean-flags (if top-std
                          (remove #|:item|# :standard args)
                          args)))
        (format #t "ARGS: ~A\n" args)
        (format #t "TOP-STD: ~A\n" top-std)
        (format #t "CLEAN: ~A\n" clean-flags)
        (let*-values (((opens opts std) (split-opens clean-flags))
                      ((options bools) (split-opts (reverse opts)))
                      ((external-tokens unused-tokens other-options)
                       (-split-menhir-opts options)))
          (format #t "~A: ~A~%" (uwhite "unused-tokens") unused-tokens)
          (let ((result
                 (remove
                  '() (list
                       (if (or top-std std)
                           '(:standard) '()) ;; FIXME: expand :standard flags
                       (if (null? opens) '()
                           (cons :opens (reverse opens)))
                       (if (null? external-tokens) '()
                           (cons :external-tokens (reverse external-tokens)))
                       (if (null? unused-tokens) '()
                           (cons :unused-tokens (reverse unused-tokens)))
                       (if (null? other-options) '()
                           (cons :options (reverse other-options)))
                       (if (null? bools) '()
                           (cons :flags (reverse bools)))
                       ;; `((:raw ,flags))
                       ))))
            `,@result))))

(define (menhir->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (bgblue "menhir->mibl") stanza)
  (let* ((stanza-alist (cdr stanza))
         (parsers (assoc-val 'modules stanza-alist)))
    (format #t "~A: ~A~%" (uwhite "menir parsers") parsers)
    (list (cons :menhir
                (map (lambda (fld)
                       (format #t "~A: ~A~%" (uwhite "menhir fld") fld)
                       (case (car fld)
                         ((modules)
                          (cons :parsers (cdr fld)))
                         ((flags)
                          (-normalize-menhir-fld-flags (cdr fld)))
                          ;; (cons :flags (cdr fld)))
                         ((merge_into)
                          (cons :base-name (cdr fld)))
                         ((infer)
                          (cons :type-inference (cdr fld)))
                         (else
                          (error 'MENHIR
                                 (format #f "unrecognized menhir fld: ~A" fld)))))
                     (cdr stanza))))))

  ;; tasks:
  ;; a) verify srcfile exists?
  ;; b) update pkg-modules and/or pkg-structures
    ;; (let* ((ext "mly")
    ;;        (_ (format #t "~A: ~A~%" (uwhite "ext") ext))
    ;;        (resolved (map (lambda (principal-fname)
    ;;                         ;;FIXME: verify file is in pkg :ocamllex
    ;;                         (let* ((mllfile (format #f "~A.~A" principal-fname ext))
    ;;                                (mlfile (format #f "~A.ml" principal-fname))
    ;;                                (mname (normalize-module-name principal-fname))
    ;;                                (pr (cons mname mllfile)))
    ;;                           ;; side-effects:
    ;;                           ;;(update-pkg-files-with-struct! pkg mlfile)
    ;;                           pr))
    ;;                       srcs)))
    ;; (list (cons :menhir resolved)))))
