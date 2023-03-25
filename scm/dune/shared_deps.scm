;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-handle-shared-deps pkg-kv)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "handle-shared-deps") pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-deps '()))
    (if-let ((dune (assoc :mibl pkg)))
            (begin
              (for-each (lambda (stanza)
                          (if *mibl-debug-s7*
                              (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                          (if (member (car stanza) '(:archive :library :ns-archive :ns-library :executable))
                              (let* ((shared-ct (length pkg-shared-deps))
                                     (deps (assoc :deps (cdr stanza))))
                                (if deps
                                    (let ((resolved (assoc :resolved (cdr deps))))
                                      (if *mibl-debug-s7*
                                          (begin
                                            (format #t "~A: ~A~%" (bgred "pkg-shared-deps") pkg-shared-deps)
                                            (format #t "~A: ~A~%" (ublue "deps") deps)
                                            (format #t "~A: ~A~%" (ublue "resolved") resolved)))
                                      (if resolved
                                          (if (not (number? (cdr resolved)))
                                              (let* ((deplist (cdr resolved))
                                                     (shared (rassoc deplist pkg-shared-deps)))
                                                (if *mibl-debug-s7*
                                                    (begin
                                                      (format #t "~A: ~A~%" (bgred "deplist") deplist)
                                                      (format #t "~A: ~A~%" (bgred "rassoc") shared)))
                                                (if (null? shared)
                                                    (begin
                                                      (if *mibl-debug-s7*
                                                          (format #t "~A: ~A~%" (green "adding") deplist))
                                                      (set! pkg-shared-deps
                                                            (append pkg-shared-deps
                                                                    `((,(+ 1 shared-ct) . ,deplist))))
                                                      (set-cdr! resolved (+ 1 shared-ct)))
                                                    (begin
                                                      ;; update stanza :deps with key
                                                      (if *mibl-debug-s7*
                                                          (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared)))
                                                      (set-cdr! resolved (car shared))
                                                      (if *mibl-debug-s7*
                                                          (format #t "~A: ~A~%" (ugreen "updated stanza") resolved))
                                                      ))))))))))
                        (cdr dune))
              ;; now add pkg-shared-deps to pkg with key :shared-deps
              ;; (format #t "~A~%" (bgred "updating pkg with :shared-deps"))
              (if (truthy? pkg-shared-deps)
                  (set-cdr! dune (append (cdr dune) `((:shared-deps ,pkg-shared-deps)))))))
    ;; (format #t "~A: ~A~%" (bgred "shared deps") pkg-shared-deps)
    '()))

(define (handle-shared-deps ws)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "handle-shared-deps") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (uwhite "pkgs") pkgs))
    (for-each (lambda (kv)
                (if *mibl-debug-s7*
                    (format #t "~A: ~A~%" (ugreen "dep for pkg") (car kv)))
                (let* ((pkg-path (car kv))
                       (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr kv)))
                  ;; (format #t "~A: ~A~%" (green "pkg") pkg)
                  ;;(if (member pkg-path *mibl-shared-deps*)
                  (-handle-shared-deps kv) ;; )
                  )
                    ;; (format #t "~A: ~A~%" (bgred "updated pkg") kv)
                )
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))
