
;; replace e.g. :rule by :write-file
(define (mibl-pkg->miblark pkg)
  (format #t "~A: ~A~%" (blue "mibl-pkg->miblark") (assoc-val :pkg-path pkg))

  (if (assoc :dune pkg)
      (for-each
       (lambda (stanza)
         (format #t "~A: ~A~%" (magenta "stanza") stanza)
         (case (car stanza)
           ((:rule)
            ;; if multiple cmds (progn) do not miblarkize
            (format #t "~A: ~A~%" (red "cmd ct:")
                    (length (assoc-in* '(:actions :cmd) (cdr stanza))))
            (if (< (length (assoc-in* '(:actions :cmd) (cdr stanza))) 2)
                (let ((tool (assoc-in '(:actions :cmd :tool) (cdr stanza))))
                  (format #t "~A: ~A~%" (green "tool") tool)
                  (if tool
                      (let ((tool (cadr tool)))
                        (format #t "~A: ~A~%" (green "tool") tool)
                        (case tool
                          ((:write-file)
                           (format #t "~A: ~A~%" (red "miblarking") stanza)
                           ;; (format #t "~A: ~A~%" (white "pkg before") pkg)
                           (set-car! stanza :write-file)
                           ;; (format #t "~A: ~A~%" (white "pkg after") pkg)
                           )

                          (else ;; nop
                           '())))
                      ))))
           (else
            )))
       (assoc-val :dune pkg))
      ;; else
      ))
