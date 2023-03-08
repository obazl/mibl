(define (yacc-deps pkg principal-name)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "yacc-deps") principal-name))
  (let ((ws-path (car (assoc-val :ws-path pkg)))
        (pkg-path (car (assoc-val :pkg-path pkg)))
        (mly-src (format #f "~A.mly" principal-name)))
    (if *debugging*
        (begin
          (format #t "~A: ~A~%" (ublue "ws-path") ws-path)
          (format #t "~A: ~A~%" (ublue "pkg-path") pkg-path)
          (format #t "~A: ~A~%" (ublue "pwd") (pwd))
          (format #t "~A: ~A~%" (ublue "tmp-dir") *tmp-dir*)))

    (system (format #f "cp -v ~A/~A ~A" pkg-path mly-src *tmp-dir*))
    (let ((yacc-cmd
           (format #f "ocamlyacc ~A/~A" *tmp-dir* mly-src)))
      (if *debugging*
          (format #t "~A: ~A~%" (red "yacc-cmd") yacc-cmd))
      (system yacc-cmd)
      (let* ((ocamldep-cmd
              (format #f "ocamldep -one-line -modules -I ~A ~A/*" pkg-path *tmp-dir*))
             (deps (string-trim '(#\newline) (system ocamldep-cmd #t)))
             (file-deps (string-split deps #\newline)))
        (if *debugging*
            (begin
              (format #t "~A: ~A~%" (red "ocamldep-cmd") ocamldep-cmd)
              (format #t "~A: ~A~%" (bgblue "file-deps") file-deps)))

        ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
        ;;         (assoc :modules pkg))
        ;; (error 'stop "STOP yacc")

        (for-each (lambda (file-dep)
                    (if *debugging*
                        (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep))
                    (let ((segs (string-split file-dep #\:)))
                      ;; (format #t "~A: ~A~%" (yellow "segs") segs)
                      (if (null? (cdr segs))
                          (begin)
                          (let* ((fpath (car segs))
                                 (fname (basename fpath))
                                 (kind (filename->kind fname))
                                 (mdeps (string-trim '(#\space) (cadr segs)))
                                 (mdeps (string-split mdeps #\space))
                                 (mdeps (map string->symbol mdeps))
                                 ;; eliminate mdeps not in this pkg
                                 (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                                 )
                            (if *debugging*
                                (begin
                                  (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                  (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                  (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)))
                            (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                    ;; should not happen
                                    (begin
                                      (if *debugging*
                                          (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc))
                                      ;; (error 'stop (format #f "lex/yacc output file ~A already exists~%" fname))
                                      ;; (if (proper-list? m-assoc)
                                      ;;     ;; its a module entry, (A (:ml a.ml) (:mli a.mli))
                                      ;;     (begin
                                      ;;       (format #t "~A: ~A~%" (bgred "FOO") 99)
                                      ;;       (set-cdr! m-assoc
                                      ;;                 (append (cdr m-assoc)
                                      ;;                         (list (cons
                                      ;;                                (if (eq? kind :struct)
                                      ;;                                    :ml-deps :mli-deps)
                                      ;;                                mdeps))))
                                      ;;       (format #t "~A: ~A~%" (bgred "m-assoc after") m-assoc))
                                      ;;     ;; else its a struct entry, (A a.ml)
                                      ;;     (begin
                                      ;;       (format #t "~A: ~A~%" (bgred "BAR") 00)
                                      ;;       (set-cdr! m-assoc
                                      ;;                 (cons (cdr m-assoc)
                                      ;;                       mdeps))))
                                      )
                                    ;;else
                                    ;; (error 'stop "STOP yacc")
                                    (update-pkg-modules-with-module! pkg principal-name)
                                )))))
                  file-deps)))
    ;; (error 'stop "STOP yacc")
  ))

;; dune ocamllex/ocamlyacc take non-normalized module names
;; e.g. (ocamllex pb_parsing_lexer)
;; from this we produce (:lex (<modulename> . <src>))
;; e.g. (:lex (Pb_parsing_lexer . pb_parsing_lexer.mll))
;; :lex - alist of (Foo . foo.mll) pairs
;; :yacc - alist of (Foo . foo.mly) pairs
;; NB: usually the srcfiles are static, but in principle could be
;; generated by another rule.
(define (lexyacc->mibl tag ws pkg stanza)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "lexyacc->mibl") stanza))
  (let ((srcs (let recur ((modules (cdr stanza))
                          (result '()))
                (if (null? modules)
                    result
                    (if (pair? (car modules))
                        ;; e.g. (ocamllex (modules lexer_impl))
                        (recur (cdr modules)
                               (concatenate
                                (let recur2 ((modules2 (car modules))
                                             (result2 '()))
                                  (if (null? modules2)
                                      result2
                                      (if (equal? 'modules (car modules2))
                                          (recur2 (cdr modules2) result2)
                                          (recur2 (cdr modules2)
                                                  (cons (car modules2) result2)))))
                                result))
                        ;; else (ocamllex foo bar ...)
                        (recur (cdr modules)
                               (cons (car modules) result)))))))
    (if *debugging*
        (format #t "~A: ~A~%" (uwhite "lex/yacc srcs") srcs))
    ;; tasks:
    ;; a) verify srcfile exists?
    ;; b) update pkg-modules and/or pkg-structures
    (let* ((ext (case tag ((:lex) "mll")
                      ((:yacc) "mly")
                      (else (error 'fixme
                                   (format #t "~A: ~A~%" (bgred "unrecognized lex/yacc tag") tag)))))
           (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "ext") ext)))
           (resolved (map (lambda (principal-fname)
                            ;;FIXME: verify file is in pkg :ocamllex
                            (let* ((mllfile (format #f "~A.~A" principal-fname ext))
                                   (mlfile (format #f "~A.ml" principal-fname))
                                   (mname (normalize-module-name principal-fname))
                                   (pr (cons mname mllfile)))
                              (if *debugging*
                                  (format #t "~A: ~A~%" (uwhite "tag") tag))
                              ;; side-effects:
                              (case tag
                                ((:lex)
                                 ;;(update-pkg-files-with-struct! pkg mlfile)
                                 )
                                ((:yacc)
                                 ;; process it, then run ocamldep
                                 ;; (yacc-deps pkg principal-fname)
                                 ;; (error 'stop "STOP mllfile")
                                 )
                                (else
                                 (error 'fixme (format #f "~A" (bgred "unrecognized lex/yacc tag") tag))))
                              pr))
                          srcs)))
    (list (cons tag resolved)))))
