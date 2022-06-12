
;; xform libraries field
;; (define (xlibraries-fld fld)
;;   )

;; xform modules field
;;;;; expand dune constant ':standard' for modules field
;; (define (xmodules fld)
;;   (let ((ms (assoc)

;; (define (x-name-fld pkg-path privname stanza-alist)
;;   ;; (format #t "normalize-stanza-fld-name ~A\n" privname)
;;   ;; (format #t "    stanza-alist: ~A\n" stanza-alist)
;;   (let ((pubname (assoc 'public_name stanza-alist)))
;;     (if pubname
;;         (begin
;;           ;; (update-public-exe-table pkg-path
;;           ;;                          (cadr pubname)
;;           ;;                          (cadr pubname))
;;           ;; (update-public-exe-table pkg-path
;;           ;;                          (cadr privname)
;;           ;;                          (cadr pubname))
;;           `(:name ((:private ,(cadr privname))
;;                    (:public ,(cadr pubname)))))
;;         (begin
;;           ;; (update-public-exe-table pkg-path (cadr privname)
;;           ;;                          (cadr privname))
;;           `(:name ((:private ,(cadr privname))))))))
