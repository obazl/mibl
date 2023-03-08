(define (update-opam-table! ws kind opam-pkg pubname pkg-path privname)
  ;; kind:  :lib, :bin, :man, :files, :sub
  (if *mibl-debugging*
      (begin
        (format #t "~A path: ~A~%" (bgmagenta "update-opam-table!") pubname)
        (format #t "~A: ~A~%" (green "kind") kind)
        (format #t "~A: ~A~%" (green "pkg-path") pkg-path)
        (format #t "~A: ~A~%" (green "privname") privname)
        (format #t "~A: ~A~%" (green "ws") ws)))

  (let* ((-ws (if (keyword? ws) (assoc-val ws *mibl-project*) ws))
         (ws-path (car (assoc-val :path -ws)))
         (_ (if *mibl-debugging* (format #t "~A: ~A~%" (bggreen "wspath") ws-path)))
         (privname (if (eq? kind :bin) (format #f "~A.exe" privname) privname))
         (opam (car (assoc-val :opam -ws)))
         (_ (if *mibl-debugging* (format #t "opam tbl: ~A\n" opam)))
         (opam-segs (string-split (format #f "~A" opam-pkg) #\.))
         (opam-pkg (if (null? (cdr opam-segs))
                       (car opam-segs) (car opam-segs)))
         (opam-pkg (string->symbol opam-pkg))
         (opam-subpkgs (if (null? (cdr opam-segs))
                          #f (cdr opam-segs))))
    (if *mibl-debugging*
        (begin
          (format #t "~A: ~A~%" (uwhite "opam-pkg") opam-pkg)
          (format #t "~A: ~A~%" (uwhite "opam-subpkgs") opam-subpkgs)))
    (if-let ((opkg (hash-table-ref opam opam-pkg)))
            (begin
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (uwhite "old opam-pkg") opkg))
              (if opam-subpkgs
                  ;; TODO: merge with existing :sub items
                  (alist-update-in! opkg opam-subpkgs
                                    (lambda (old)
                                      (append old
                                              `(;;(,(car opam-subpkgs)
                                                (:lib (,pkg-path ,privname))
                                                ))));)
                  (alist-update-in! opkg (list kind)
                                    (lambda (old)
                                      (append old
                                              (list
                                               (if (eq? kind :bin)
                                                  `(,pubname
                                                    (,pkg-path . ,privname))
                                                  `((,pkg-path . ,privname))))
                                                 )))
                  ;; (hash-table-set! opam opam-pkg
                  ;;                  `((,kind ,pkg-path ,privname)))
                  ))
            (begin
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (uwhite "adding opam-pkg") opam-pkg))
              (if opam-subpkgs
                  ;; TODO: merge with existing :sub items
                  (hash-table-set! opam opam-pkg
                                   `((:path . ,ws-path)
                                     (,(car opam-subpkgs)
                                      (:lib (,pkg-path ,privname)))))
                  (hash-table-set! opam opam-pkg
                                   `((:path . ,ws-path)
                                     ,(if (eq? kind :bin)
                                          `(,kind (,pubname
                                                  (,pkg-path . ,privname)))
                                          `(,kind (,pkg-path . ,privname))))))))
    (if *mibl-debugging*
        (format #t "~A: ~A\n" (ured "updated opam tbl") opam))
    ))

