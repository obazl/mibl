;; Step 1:
;; run dune-pkg->mibl for all pkgs in ws
;; updates :pkgs entry of ws
(define (miblize ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "-miblize") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (pkg-assoc)
                            ;; key: pkg path (string)
                            ;; val: assoc-list
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr pkg-assoc))
                            (if (assoc 'dune (cdr pkg-assoc))

                                (let ((mibl-pkg (dune-pkg->mibl :@ (cdr pkg-assoc))))
                                  ;; (format #t "~A: ~A~%" (red "miblized") mibl-pkg)
                                  (hash-table-set! pkgs (car pkg-assoc) mibl-pkg)
                                  mibl-pkg)
                                (begin
                                  ;; (format #t "~A: ~A~%" (red "miblize: no dune file") pkg-assoc)
                                  (cdr pkg-assoc))
                                ))
                         pkgs)))
        ;; (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist)
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (blue "mibl pkg ct") (length mpkg-alist)))
    mpkg-alist))

