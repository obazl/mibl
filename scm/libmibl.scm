(if *mibl-debug-loads*
    (format #t "loading libmibl.scm~%"))

(load "dune.scm")
(load "mibl_pp.scm")

(define (emit-mibl-pkg pkg)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (yellow "emit-mibl-pkg") pkg)
      ;; (if (not (assoc :mibl pkg))
      ;;     (format #t "~A: ~A~%" (red "Missing :dune") (assoc-val :pkg-path pkg)))
      )
  (load "s7/write.scm")
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PKG.mibl"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (yellow "Emitting") mibl-file))
    (pretty-print pkg outp)
    (close-output-port outp)))

(define (emit-mibl-pkgs)
  (if *mibl-debugging*
      (format #t "~%~A~%" (yellow "emit-mibl")))
  (for-each (lambda (ws)
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (yellow "ws") ws))
              (let ((pkgs (car (assoc-val :pkgs (cdr ws)))))
                (if *mibl-debugging*
                    (begin
                      (format #t "~A: ~A~%" (yellow "pkgs") pkgs)
                      (format #t "~A: ~A~%" (yellow "pkgs keys")
                              (hash-table-keys pkgs))))
                (for-each (lambda (kv)
                            (if *mibl-debugging*
                                (format #t "~%~A: ~A~%" (yellow "emitting mibl pkg") kv))
                            (emit-mibl-pkg (cdr kv)))
                          ;; pkgs is a hash-table
                          pkgs)
                ))
             *mibl-project*))

(define (emit-mibl-ws ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (yellow "emit-mibl-ws") ws))
  (let* ((ws-path (car (assoc-val :path (cdr ws))))
         (mibl-file (format #f "~A/WS.mibl"  ws-path))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (yellow "Emitting") mibl-file))
    (mibl-pretty-print ws outp)
    (close-output-port outp)))

(define (emit-mibl-wss)
  (if *mibl-debugging*
      (format #t "~%~A~%" (yellow "emit-mibl-wss")))
  (for-each (lambda (ws)
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (yellow "ws") ws))
              (emit-mibl-ws ws))
            *mibl-project*))

(define (mibl-clean-mibl)
  (format #t "cleaning mibl - NOT IMPLEMENTED yet.\n")
  )
