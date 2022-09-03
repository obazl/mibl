(define (menhir->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (ublue "menhir->mibl") stanza)
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
    (format #t "~A: ~A~%" (uwhite "menir srcs") srcs)
    ;; tasks:
    ;; a) verify srcfile exists?
    ;; b) update pkg-modules and/or pkg-structures
    (let* ((ext "mly")
           (_ (format #t "~A: ~A~%" (uwhite "ext") ext))
           (resolved (map (lambda (principal-fname)
                            ;;FIXME: verify file is in pkg :ocamllex
                            (let* ((mllfile (format #f "~A.~A" principal-fname ext))
                                   (mlfile (format #f "~A.ml" principal-fname))
                                   (mname (normalize-module-name principal-fname))
                                   (pr (cons mname mllfile)))
                              ;; side-effects:
                              ;;(update-pkg-files-with-struct! pkg mlfile)
                              pr))
                          srcs)))
    (list (cons :menhir resolved)))))
