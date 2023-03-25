;; called by top-level convert routine
;; goes through every stanza in pkg, to extract shared ppxes
;; updates pkg-level :shared-ppx alist
(define (-handle-pkg-shared-ppx pkg-kv)
  (if (or *mibl-debug-ppx* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "-handle-pkg-shared-ppx") pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-ppx '())
         (dune (assoc :mibl pkg)))
    (if dune
        (for-each (lambda (stanza)
                    (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                        (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                    (let* ((shared-ct (length pkg-shared-ppx))
                           (ppx-fld (assoc :ppx (cdr stanza)))
                           (ppx (if ppx-fld (cdr ppx-fld) #f))
                           (ppxes-fld (assoc :ppxes (cdr stanza)))
                           (ppxes (if ppxes-fld (cdr ppxes-fld) #f))
                           )
                      (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                          (begin
                            (format #t "~A: ~A~%" (uyellow "ppx") ppx)
                            (format #t "~A: ~A~%" (uyellow "Ppx-fld") ppx-fld)
                            (format #t "~A: ~A~%" (uyellow "Ppxes-fld") ppxes-fld)))
                      (if ppx
                          (begin
                            (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                (format #t "~A: ~A~%" (bgred "ppx for pkg-shared-ppx") pkg-shared-ppx))
                            (let ((shared (rassoc ppx pkg-shared-ppx)))
                              (if (null? shared)
                                  (begin
                                    (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                        (format #t "~A: ~A~%" (bgred "adding shared ppx") ppx))
                                    (set! pkg-shared-ppx
                                          (append pkg-shared-ppx
                                                  `((,(+ 1 shared-ct) . ,ppx))))
                                    (set-cdr! ppx-fld (+ 1 shared-ct)))
                                  ;; else
                                  (begin
                                    (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                        (format #t "~A: ~A~%" (bgred "found shared ppx") shared))
                                    (set-cdr! ppx-fld shared-ct))))))
                      (if ppxes
                          (begin
                            (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                (format #t "~A: ~A~%" (bgred "ppxes for pkg-shared-ppx") ppxes))
                            (for-each (lambda (ppx-fld)
                                        (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                            (format #t "~A: ~A~%" (red "a ppx fld") ppx-fld))
                                        (let ((ppx (if ppx-fld (cdr ppx-fld) #f))
                                              (shared (rassoc ppx pkg-shared-ppx)))
                                          (if (null? shared)
                                              (begin
                                                (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                                    (format #t "~A: ~A~%" (bgred "adding shared ppx") ppx))
                                                (set! pkg-shared-ppx
                                                      (append pkg-shared-ppx
                                                              `((,(+ 1 shared-ct) . ,ppx))))
                                                (set-cdr! ppx-fld (+ 1 shared-ct))
                                                (set! shared-ct (+ 1 shared-ct)))
                                              ;; else
                                              (begin
                                                (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                                                    (format #t "~A: ~A~%" (bgred "found shared ppx") shared))
                                                (set-cdr! ppx-fld shared-ct)))))
                                      ppxes)))
                      ))
                  ;;                   (set-cdr! resolved (+ 1 shared-ct)))
                  ;;                 (begin
                  ;;                   ;; update stanza :ppxes with key
                  ;;                   (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared))
                  ;;                   (set-cdr! resolved (car shared))
                  ;;                   (format #t "~A: ~A~%" (ugreen "updated stanza") resolved)
                  ;;                   ))))))
                  ;; ))
                  (cdr dune)))
    ;; now add pkg-shared-ppx to pkg with key :shared-ppx
    ;; (format #t "~A~%" (bgred "updating pkg with :shared-ppx"))
    (if (truthy? pkg-shared-ppx)
        (set-cdr! dune (append (cdr dune) `((:shared-ppx ,pkg-shared-ppx)))))
    ;; (format #t "~A: ~A~%" (bgred "shared deps") pkg-shared-ppx)
    ))

;;FIXME: currently sharing is scoped to pkg level.
;;TODO: repo scoping: share ppxes across pkgs within repo
(define (handle-shared-ppx ws)
  (if (or *mibl-debug-ppx* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "handle-shared-ppx") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                ;; (if (or *mibl-debug-ppx* *mibl-debug-s7*)
                ;;     (format #t "~A: ~A~%" (ugreen "ppx for pkg") (car kv)))
                (let* ((pkg-path (car kv))
                       ;; (_ (if (or *mibl-debug-ppx* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr kv)))
                  (-handle-pkg-shared-ppx kv)))
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))
