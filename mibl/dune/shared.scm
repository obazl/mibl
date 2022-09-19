(define (-handle-pkg-shared-ppx pkg-kv)
  (format #t "~A: ~A~%" (ublue "-handle-pkg-shared-ppx") pkg-kv)
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-ppx '())
         (dune (assoc :dune pkg)))
    (if dune
        (for-each (lambda (stanza)
                    (format #t "~A: ~A~%" (bgblue "stanza") stanza)
                    (let* ((shared-ct (length pkg-shared-ppx))
                           (ppx-fld (assoc :ppx (cdr stanza)))
                           (ppx (if ppx-fld (cdr ppx-fld) #f)))
                      (format #t "~A: ~A~%" (uyellow "Ppx-fld") ppx-fld)
                      (if ppx
                          (begin
                            (format #t "~A: ~A~%" (bgred "pkg-shared-ppx") pkg-shared-ppx)
                            (let ((shared (rassoc ppx pkg-shared-ppx)))
                              (if (null? shared)
                                  (begin
                                    (format #t "~A: ~A~%" (bgred "adding shared ppx") ppx)
                                    (set! pkg-shared-ppx
                                          (append pkg-shared-ppx
                                                  `((,(+ 1 shared-ct) . ,ppx))))
                                    (set-cdr! ppx-fld (+ 1 shared-ct)))
                                  ;; else
                                  (begin
                                    (format #t "~A: ~A~%" (bgred "found shared ppx") shared)
                                    (set-cdr! ppx-fld shared-ct))))))))
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

(define (handle-shared-ppx ws)
  (format #t "~A: ~A~%" (ublue "handle-shared-ppx") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (ugreen "for pkg") kv)
                (let* ((pkg-path (car kv))
                       (_ (format #t "~A: ~A~%" (green "pkg-path") pkg-path))
                       (pkg (cdr kv)))
                  (-handle-pkg-shared-ppx kv)))
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-handle-pkg-shared-deps pkg-kv)
  (format #t "~A: ~A~%" (ublue "handle-pkg-shared-deps") pkg-kv)
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-deps '())
         (dune (assoc :dune pkg)))
    (for-each (lambda (stanza)
                (format #t "~A: ~A~%" (bgblue "stanza") stanza)
                (let* ((shared-ct (length pkg-shared-deps))
                       (deps (assoc :deps (cdr stanza))))
                  (if deps
                      (let ((resolved (assoc :resolved (cdr deps))))
                        (format #t "~A: ~A~%" (bgred "pkg-shared-deps") pkg-shared-deps)
                        (format #t "~A: ~A~%" (ublue "deps") deps)
                        (format #t "~A: ~A~%" (ublue "resolved") resolved)
                        (if resolved
                            (let* ((deplist (cdr resolved))
                                   (shared (rassoc deplist pkg-shared-deps)))
                              (format #t "~A: ~A~%" (bgred "deplist") deplist)
                              (format #t "~A: ~A~%" (bgred "rassoc") shared)
                              (if (null? shared)
                                  (begin
                                    (format #t "~A: ~A~%" (green "adding") deplist)
                                    (set! pkg-shared-deps
                                          (append pkg-shared-deps
                                                  `((,(+ 1 shared-ct) . ,deplist))))
                                    (set-cdr! resolved (+ 1 shared-ct)))
                                  (begin
                                    ;; update stanza :deps with key
                                    (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared))
                                    (set-cdr! resolved (car shared))
                                    (format #t "~A: ~A~%" (ugreen "updated stanza") resolved)
                                    ))))))))
              (cdr dune))
    ;; now add pkg-shared-deps to pkg with key :shared-deps
    ;; (format #t "~A~%" (bgred "updating pkg with :shared-deps"))
    (if (truthy? pkg-shared-deps)
        (set-cdr! dune (append (cdr dune) `((:shared-deps ,pkg-shared-deps)))))
    ;; (format #t "~A: ~A~%" (bgred "shared deps") pkg-shared-deps)
    ))

;; (:ns-archive
;;  (:ns . jsooexp_static_eval)
;;  (:privname . jsooexp_static_eval)
;;  (:inline-tests (:compile-opts (:flags -allow-output-patterns)) (deps (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe) (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
;;  (:compile-opts (:standard) (:opens Jsoo_compiler_expect_tests_helper))
;;  (:ppx (:scope :all) (:manifest @ppx_expect//lib/ppx_expect))
;;  (:deps (:resolved . 1))
;;  (:manifest (:modules Static_eval)))

;; for executables?

(define (-handle-stanza-shared-opts pkg-kv)
  (format #t "~A: ~A~%" (ublue "handle-stanza-shared-opts") pkg-kv)
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-opts '())
         (dune (assoc :dune pkg)))
    (for-each (lambda (stanza)
                (format #t "~A: ~A~%" (bgblue "stanza") stanza)
                (let* ((shared-ct (length pkg-shared-opts))
                       (opts-fld (assoc :compile-opts (cdr stanza))))
                  (if opts-fld
                      (begin
                        (format #t "~A: ~A~%" (bgred "pkg-shared-opts") pkg-shared-opts)
                        (format #t "~A: ~A~%" (ublue "opts-fld") opts-fld)
                        (let* ((opts (cdr opts-fld))
                               (shared (rassoc opts pkg-shared-opts)))
                          (format #t "~A: ~A~%" (bgred "rassoc") shared)
                          (if (null? shared)
                              (begin
                                (format #t "~A: ~A~%" (green "adding") opts)
                                (set! pkg-shared-opts
                                      (append pkg-shared-opts
                                              `((,(+ 1 shared-ct) . ,opts))))
                                (set-cdr! opts-fld (+ 1 shared-ct)))
                              (begin
                                ;; update stanza :opts with key
                                ;; (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared))
                                (set-cdr! opts-fld (car shared))
                                (format #t "~A: ~A~%" (ugreen "updated stanza") stanza)
                                )))))))
              (cdr dune))
    ;; now add pkg-shared-opts to pkg with key :shared-opts
    ;; (format #t "~A~%" (bgred "updating pkg with :shared-opts"))
    (if (truthy? pkg-shared-opts)
        (set-cdr! dune (append (cdr dune) `((:shared-compile-opts ,pkg-shared-opts)))))
    ;; (format #t "~A: ~A~%" (bgred "shared opts") pkg-shared-opts)
    ))

(define (handle-shared-deps ws)
  (format #t "~A: ~A~%" (ublue "handle-shared-deps") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (ugreen "for pkg") kv)
                (let* ((pkg-path (car kv))
                       (_ (format #t "~A: ~A~%" (green "pkg-path") pkg-path))
                       (pkg (cdr kv)))
                  ;; (format #t "~A: ~A~%" (green "pkg") pkg)
                  (if (member pkg-path *shared-deps*)
                      (-handle-pkg-shared-deps kv)))
                ;; (format #t "~A: ~A~%" (bgred "updated pkg") kv)
                )
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))

(define (handle-shared-opts ws)
  (format #t "~A: ~A~%" (ublue "handle-shared-opts") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (ugreen "for pkg") kv)
                (let* ((pkg-path (car kv))
                       (_ (format #t "~A: ~A~%" (green "pkg-path") pkg-path))
                       (pkg (cdr kv)))
                  ;; (format #t "~A: ~A~%" (green "pkg") pkg)
                  (if (member pkg-path *shared-deps*)
                      (-handle-stanza-shared-opts kv)))
                ;; (format #t "~A: ~A~%" (bgred "updated pkg") kv)
                )
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))