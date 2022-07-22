
(define (emit-mibl-pkg pkg)
  (format #t "~A: ~A~%" (yellow "emit-mibl-pkg") pkg)
  (if (not (assoc :dune pkg))
      (format #t "~A: ~A~%" (red "Missing :dune") (assoc-val :pkg-path pkg)))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/dune.mibl"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (format #t "OPEN ERROR: ~A~%" mibl-file))
                 )))
    (format #t "~A: ~A~%" (yellow "Emitting") mibl-file)
    (write pkg outp)
    (close-output-port outp)))

(define (emit-mibl)
  (format #t "~%~A~%" (yellow "emit-mibl"))
  (for-each (lambda (ws)
              (format #t "~A: ~A~%" (yellow "ws") ws)
              (let ((pkgs (car (assoc-val :pkgs (cdr ws)))))
                (format #t "~A: ~A~%" (yellow "pkgs") pkgs)
                (format #t "~A: ~A~%" (yellow "pkgs keys")
                        (hash-table-keys pkgs))
                (for-each (lambda (kv)
                            (format #t "~%~A: ~A~%" (yellow "emitting pkg") kv)
                            (emit-mibl-pkg (cdr kv)))
                          ;; pkgs is a hash-table
                          pkgs)
                ))
             -mibl-ws-table))
