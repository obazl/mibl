(define (-split-menhir-opts options)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "-split-menhir-opts") options))

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
  (if *debugging*
      (format #t "~A: ~A\n" (ublue "-normalize-menhir-fld-flags") args))
  (let* ((top-std (any (lambda (flag) (equal? flag :standard))
                       args))
         (clean-flags (if top-std
                          (remove #|:item|# :standard args)
                          args)))
    (if *debugging*
        (begin
          (format #t "ARGS: ~A\n" args)
          (format #t "TOP-STD: ~A\n" top-std)
          (format #t "CLEAN: ~A\n" clean-flags)))
    (let*-values (((opens opts std) (split-opens clean-flags))
                  ;; FIXME: handle exclusions
                  ((options bools exclusions) (split-opts (reverse opts)))
                  ((external-tokens unused-tokens other-options)
                   (-split-menhir-opts options)))
      (if *debugging*
          (format #t "~A: ~A~%" (uwhite "unused-tokens") unused-tokens))
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
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "menhir->mibl") stanza))
  (let* ((stanza-alist (cdr stanza))
         (parsers (assoc-val 'modules stanza-alist)))
    (if *debugging*
        (format #t "~A: ~A~%" (uwhite "menir parsers") parsers))
    (let ((spec
           (map (lambda (fld)
                  (if *debugging*
                      (format #t "~A: ~A~%" (uwhite "menhir fld") fld))
                  (case (car fld)
                    ((modules)
                     (cons :grammars (cdr fld)))
                    ((flags)
                     ;; menhir flags require different parsing than ocaml flags
                     (-normalize-menhir-fld-flags (cdr fld)))
                    ((merge_into)
                     (cons :base (cdr fld)))
                    ((infer)
                     (cons :type-inference (cdr fld)))
                    (else
                     (error 'MENHIR
                            (format #f "unrecognized menhir fld: ~A" fld)))))
                (cdr stanza))))
      (if *debugging*
          (format #t "~A: ~A~%" (bgmagenta "menhir spec") spec))
      ;; (error 'x "stop menhir")
      (let* ((cmd-unused (if-let ((unused (assoc-val :unused-tokens spec)))
                                 (format #f "~{--unused-token ~A~^ ~}" unused)
                                 ""))
             (cmd-flags (if-let ((flags (assoc-val :flags spec)))
                                (format #f "~{~A~^ ~}" flags) ""))
             (cmd-options (if-let ((opts (assoc-val :options spec)))
                                  (format #f "~{~A~^ ~}" opts) ""))
             (pkg-path (car (assoc-val :pkg-path pkg)))
             (cmd (format #f
                          (string-join
                           '("menhir"
                             "--unused-tokens" ;; ignore warnings
                             "~A" ;; cmd-options
                             "~A" ;; cmd-flags
                             "--depend"
                             "--ocamldep"
                             "\"ocamldep -modules\""
                             "~{~A/~A.mly~}"
                             "2> /dev/null"))
                          cmd-options cmd-flags
                          (flatten (map (lambda (g)
                                          (cons pkg-path g))
                                        (assoc-val :grammars spec))))))
        (if *debugging*
            (begin
              (format #t "~A: ~A~%" (ublue "cmd-unused") cmd-unused)
              (format #t "~A: ~A~%" (ublue "cmd-options") cmd-options)
              (format #t "~A: ~A~%" (ublue "cmd-flags") cmd-flags)
              (format #t "~A: ~A~%" (ublue "cmd") cmd)))
        ;; (let ((result (system cmd #t)))
        ;;   (format #t "~A: ~A~%" (bgred "result") result))
        (let* ((deps (string-trim '(#\newline) (system cmd #t)))
               (deps (string-split deps #\newline)))
          (for-each (lambda (dep)
                      (if *debugging*
                          (format #t "~A: ~A~%" (bgyellow "processing ocamldep") dep))
                      (let ((segs (string-split dep #\:)))
                        ;; (format #t "~A: ~A~%" (yellow "segs") segs)
                        (if (null? (cdr segs))
                            (begin)
                            (let* ((fpath (car segs))
                                   (fname (basename fpath))
                                   (kind (filename->kind fname))
                                   (mdeps (string-trim '(#\space) (cadr segs)))
                                   (mdeps (string-split mdeps #\space))
                                   (mdeps (map string->symbol mdeps))
                                   (_ (if *debugging* (format #t "~A: ~A~%" (red "mdeps") mdeps)))
                                   ;; eliminate mdeps not in this pkg
                                   (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                                   (_ (if *debugging* (format #t "~A: ~A~%" (red "filtered mdeps") mdeps)))
                                   )

                              (if (not (null? mdeps))
                                  (begin
                                    (if *debugging*
                                        (begin
                                          (format #t "~A ~A to ~A~%" (bgyellow "adding mdeps") mdeps fname)
                                          (format #t "~A: ~A~%" (uyellow "in pkg") pkg)))
                                    (set! spec (alist-update-in! spec '(:deps)
                                                                 (lambda (old)
                                                                   (remove-duplicates (append mdeps old))
                                                                   ;;(append spec `((:deps ,@mdeps)))
                                                                   )
                                                                 ))
                                    (update-stanza-deps pkg fname mdeps)
                                    ))

                              ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
                              ;; we retrieve the pkg-dep for fname and add the mdeps to it
                              (if *debugging*
                                  (begin
                                    (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                    (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                    (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)))
                              (if (not (null? mdeps))
                                  (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                          (begin
                                            (if *debugging*
                                                (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc))
                                            (if (proper-list? m-assoc)
                                                ;; its a module entry, (A (:ml a.ml) (:mli a.mli))
                                                (begin ;; if mdeps not empty
                                                  (set-cdr! m-assoc
                                                            (append (cdr m-assoc)
                                                                    (list (cons
                                                                           (if (eq? kind :struct)
                                                                               :ml-deps :mli-deps)
                                                                           mdeps))))
                                                  ;; (format #t "~A: ~A~%" (bgred "m-assoc after") m-assoc)
                                                  )
                                                ;; else its a struct entry, (A a.ml)
                                                (begin
                                                  (if *debugging*
                                                      (begin
                                                        (format #t "~A: ~A~%" (bgred "STRUCT ENTRY") m-assoc)
                                                        (format #t "~A: ~A~%" (bgred "adding mdeps") mdeps)))
                                                  (if (not (null? mdeps))
                                                      (set-cdr! m-assoc
                                                                (cons (cdr m-assoc)
                                                                      mdeps))))))
                                          ;;else
                                          (if *debugging*
                                              (format #t "~A: ~A~%" (blue "not found") m-assoc)))
                                  ;; else mdeps is null
                                  )))))
                    deps))
        (let ((tok (if-let ((tok (assoc-val :external-tokens spec)))
                           (car tok) #f)))
          (if tok
              (begin
                (if *debugging*
                    (format #t "~A: ~A~%" (bgred "tok") tok))
                (alist-update-in! spec '(:deps) (lambda (old) (remove tok old))))))
        (if *debugging*
            (format #t "~A: ~A~%" (ured "menhir spec") spec))
        ;; (error 'STOP "STOP menhir")
        (list (cons :menhir spec))))))

  ;; tasks:
  ;; a) verify srcfile exists?
  ;; b) update pkg-modules and/or pkg-structures
    ;; (let* ((ext "mly")
    ;;        (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "ext") ext)))
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
