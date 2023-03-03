;; test cases:  ppx_assert, ppx_bench

(define (ppx-inline-tests! ws)
  (if (or *debug-ppx* *debugging*)
      (format #t "~A~%" (ublue "ppx-inline-tests!")))
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                ;; (if (or *debug-ppx* *debugging*)
                ;;     (begin
                ;;       (format #t "~A: ~A~%" (ugreen "ppx for pkg") (car kv))
                ;;       ;; (format #t "~A: ~A~%" (ugreen "pkg") kv))
                ;;     ))
                (let* ((pkg-path (car kv))
                       (_ (if (or *debug-ppx* *debugging*)
                              (format #t "~%~A: ~A  //~A~%" (bggreen "pkg") (car kv) pkg-path)))
                       (pkg (cdr kv))
                       (dune (assoc :dune pkg))
                       (stanzas (cdr dune))
                       (shared-ppx (if-let ((shared (assoc-val :shared-ppx stanzas)))
                                           (car shared) '()))
                       (shared-deps (if-let ((shared (assoc-val :shared-deps stanzas)))
                                            (car shared) '()))
                       )
                       (if (or *debug-ppx* *debugging*)
                           (begin
                             (format #t "~A: ~A~%" (ugreen "shared-ppx") shared-ppx)
                        (format #t "~A: ~A~%" (ugreen "shared-deps") shared-deps)))
                  (for-each (lambda (stanza)
                              ;; (if (or *debug-ppx* *debugging*)
                              ;;     (format #t "~A: ~A~%" (green "stanza")
                              ;;             stanza))
                              (case (car stanza)
                                ((:ns-archive)
                                 (format #t "~A: ~A~%" (red "ns-archive") stanza)
                                 (let* ((stanza-alist (cdr stanza))
                                        (ppx-id (assoc-val :ppx stanza-alist)))
                                   (format #t "~A: ~A~%" (red "ppx id") ppx-id)
                                   (if ppx-id
                                       (let* ((ppx (assoc-val ppx-id shared-ppx))
                                              (manifest (assoc-val :manifest ppx)))
                                         (format #t "~A: ~A~%" (red "ppx") ppx)
                                         (format #t "~A: ~A~%" (red "manifest") manifest)
                                         (if (member '@ppx_inline_test//lib/ppx_inline_test manifest)
                                             (begin
                                               (format #t "~A~%" (red "INLINE TEST"))
                                               (set-car! stanza :ppx-inline-test-lib)))
                                         )))
                                 )
                                ((:shared-ppx))
                                ((:shared-deps))
                                (else
                                  (format #t "~A: ~A~%" (red "other")
                                          stanza))
                                 )
                              ;; (format #t "~A: ~A~%" (red "STANZA")
                              ;;         stanza)
                              )
                            stanzas)
                  (values)))
              pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (red "pkg") (car kv))
                (let* ((pkg (cdr kv))
                       (dune (assoc :dune pkg))
                       (stanzas (cdr dune)))
                  (for-each (lambda (s)
                              (format #t "~A: ~A~%" (red "stanza") s))
                            stanzas)))
              pkgs)
    '())
  )