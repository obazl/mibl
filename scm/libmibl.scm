(if *mibl-debug-loads*
    (format #t "loading libmibl.scm~%"))

(load "dune.scm")
(load "mibl_pp.scm")
(load "dune/pipeline.scm")

(define (emit-mibl-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-mibl-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PKG.mibl"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (mibl-pretty-print pkg outp)
    (close-output-port outp)))

(define (emit-s7-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-s7-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path
                                   (if *mibl-emit-parsetree*
                                       "/PARSETREE.s7"
                                       "/PKG.s7")))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string pkg :readable) outp)
    (close-output-port outp)))

(define (emit-mibl-pkgs)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-mibl")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-wss, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      (if (or *mibl-emit-mibl* *mibl-emit-s7* *mibl-emit-parsetree*)
          (for-each (lambda (ws)
                      (if (or *mibl-debug-emit* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (yellow "ws") ws))
                      (let ((pkgs (car (assoc-val :pkgs (cdr ws)))))
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (begin
                              (format #t "~A: ~A~%" (yellow "pkgs") pkgs)
                              (format #t "~A: ~A~%" (yellow "pkgs keys")
                                      (hash-table-keys pkgs))))
                        (for-each (lambda (kv)
                                    (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                        (format #t "~%~A: ~A~%" (yellow "emitting mibl pkg") kv))
                                    (if *mibl-emit-mibl*
                                        (emit-mibl-pkg (cdr kv)))
                                    (if *mibl-emit-s7*
                                        (emit-s7-pkg (cdr kv))))
                                  ;; pkgs is a hash-table
                                  pkgs)
                        ))
                    *mibl-project*))))

(define (emit-mibl-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
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
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (mibl-pretty-print ws outp)
    (close-output-port outp)))

(define (emit-parsetree-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PARSETREE.s7"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string pkg :readable) outp)
    (close-output-port outp)))

(define (emit-s7-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-s7-ws") ws))
  (let* ((ws-path (car (assoc-val :path (cdr ws))))
         (mibl-file (format #f "~A/WS.s7"  ws-path))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string ws :readable) outp)
    (close-output-port outp)))

;; (let* ((pkgs-ht (car (assoc-val :pkgs (cdr ws-mibl))))
;;        (ht-entries (cdr pkgs-ht)))
;;   (format #t \"~A~%\" pkgs-ht))

(define (emit-mibl-wss)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-mibl-wss")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-wss, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      (begin
        (if *mibl-emit-mibl*
            (for-each (lambda (ws)
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (yellow "ws") ws))
                        (emit-mibl-ws ws))
                      *mibl-project*))
        (if *mibl-emit-s7*
            (for-each (lambda (ws)
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (yellow "ws") ws))
                        (emit-s7-ws ws))
                      *mibl-project*)))))

(define (emit-parsetree-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-ws") ws))
  (let* ((ws-path (car (assoc-val :path (cdr ws)))))
    (if *mibl-emit-mibl*
        (let* ((mibl-file (format #f "~A/PARSETREE.mibl" ws-path))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file mibl-file))
                       (lambda args
                         (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                       )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (mibl-pretty-print ws outp)
          (close-output-port outp)))
    (if *mibl-emit-s7*
        (let* ((mibl-file (format #f "~A/PARSETREE.s7" ws-path))
                 (outp
                  (catch #t
                         (lambda ()
                           (open-output-file mibl-file))
                         (lambda args
                           (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                         )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (write (object->string ws :readable) outp)
          (close-output-port outp)))))

(define (emit-parsetree-project)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-project") *mibl-project*))
  (let* ((@ws (assoc-val :@ *mibl-project*))
         (ws-path (car (assoc-val :path (cdr @ws)))))
    (if *mibl-emit-mibl*
        (let* ((mibl-file (format #f
                                  (if *mibl-emit-result*
                                      "~A/EXPECTED.mibl"
                                      "~A/PARSETREE.mibl")
                                      ws-path))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file mibl-file))
                       (lambda args
                         (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                       )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (mibl-pretty-print *mibl-project* outp)
          (close-output-port outp)))
    (if *mibl-emit-s7*
        (let* ((mibl-file (format #f
                                  (if *mibl-emit-result*
                                      "~A/EXPECTED.s7"
                                      "~A/PARSETREE.s7")
                                      ws-path))
                 (outp
                  (catch #t
                         (lambda ()
                           (open-output-file mibl-file))
                         (lambda args
                           (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                         )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (write (object->string *mibl-project* :readable) outp)
          (close-output-port outp)))))

(define (emit-parsetrees)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-parsetree")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-parsetrees, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      ;; (for-each (lambda (ws)
      ;;             (emit-parsetree-ws ws))
      ;;           *mibl-project*)
      (emit-parsetree-project)
      ))

(define (emit-mibl-result)
  (emit-parsetree-project))

(define (mibl-clean-mibl)
  (format #t "cleaning mibl - NOT IMPLEMENTED yet.\n")
  )
