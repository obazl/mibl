(define (yacc-deps pkg principal-name)
  (format #t "~A: ~A~%" (ublue "yacc-deps") principal-name)
  (let ((ws-path (car (assoc-val :ws-path pkg)))
        (pkg-path (car (assoc-val :pkg-path pkg)))
        (mly-src (format #f "~A.mly" principal-name)))
    (format #t "~A: ~A~%" (ublue "ws-path") ws-path)
    (format #t "~A: ~A~%" (ublue "pkg-path") pkg-path)
    (format #t "~A: ~A~%" (ublue "pwd") (pwd))
    (format #t "~A: ~A~%" (ublue "tmp-dir") *tmp-dir*)

    (system (format #f "cp -v ~A/~A ~A" pkg-path mly-src *tmp-dir*))
    (let ((yacc-cmd
           (format #f "ocamlyacc ~A/~A" *tmp-dir* mly-src)))
      (format #t "~A: ~A~%" (red "yacc-cmd") yacc-cmd)
      (system yacc-cmd)
      (let* ((ocamldep-cmd
              (format #f "ocamldep -one-line -modules -I ~A ~A/*" pkg-path *tmp-dir*))
             (deps (string-trim '(#\newline) (system ocamldep-cmd #t)))
             (file-deps (string-split deps #\newline)))
        (format #t "~A: ~A~%" (red "ocamldep-cmd") ocamldep-cmd)
        (format #t "~A: ~A~%" (bgblue "file-deps") file-deps)

        ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
        ;;         (assoc :modules pkg))
        ;; (error 'stop "STOP yacc")

        (for-each (lambda (file-dep)
                    (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep)
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
                            (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                            (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                            (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
                            (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                    ;; should not happen
                                    (begin
                                      (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc)
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
