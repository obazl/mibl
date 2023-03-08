(if *debug-loads*
    (format #t "loading libmibl.scm~%"))

(load "dune.scm")
(load "mibl_pp.scm")

(define (emit-mibl-pkg pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (yellow "emit-mibl-pkg") pkg)
      ;; (if (not (assoc :dune pkg))
      ;;     (format #t "~A: ~A~%" (red "Missing :dune") (assoc-val :pkg-path pkg)))
      )
  (load "s7/write.scm")
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/BUILD.mibl"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if *debugging*
        (format #t "~A: ~A~%" (yellow "Emitting") mibl-file))
    (pretty-print pkg outp)
    (close-output-port outp)))

(define (emit-mibl)
  (if *debugging*
      (format #t "~%~A~%" (yellow "emit-mibl")))
  (for-each (lambda (ws)
              (if *debugging*
                  (format #t "~A: ~A~%" (yellow "ws") ws))
              (let ((pkgs (car (assoc-val :pkgs (cdr ws)))))
                (if *debugging*
                    (begin
                      (format #t "~A: ~A~%" (yellow "pkgs") pkgs)
                      (format #t "~A: ~A~%" (yellow "pkgs keys")
                              (hash-table-keys pkgs))))
                (for-each (lambda (kv)
                            (if *debugging*
                                (format #t "~%~A: ~A~%" (yellow "emitting mibl pkg") kv))
                            (emit-mibl-pkg (cdr kv)))
                          ;; pkgs is a hash-table
                          pkgs)
                ))
             *mibl-project*))
