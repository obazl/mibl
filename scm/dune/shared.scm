;; called by top-level convert routine
;; goes through every stanza in pkg, to extract shared ppxes
;; updates pkg-level :shared-ppx alist
(define (-handle-pkg-shared-ppx pkg-kv)
  (if (or *mibl-debug-ppx* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "-handle-pkg-shared-ppx") pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-ppx '())
         (dune (assoc :mibl pkg)))
    (if dune
        (for-each (lambda (stanza)
                    (if (or *mibl-debug-ppx* *mibl-debugging*)
                        (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                    (let* ((shared-ct (length pkg-shared-ppx))
                           (ppx-fld (assoc :ppx (cdr stanza)))
                           (ppx (if ppx-fld (cdr ppx-fld) #f))
                           (ppxes-fld (assoc :ppxes (cdr stanza)))
                           (ppxes (if ppxes-fld (cdr ppxes-fld) #f))
                           )
                      (if (or *mibl-debug-ppx* *mibl-debugging*)
                          (begin
                            (format #t "~A: ~A~%" (uyellow "Ppx-fld") ppx-fld)
                            (format #t "~A: ~A~%" (uyellow "Ppxes-fld") ppxes-fld)))
                      (if ppx
                          (begin
                            (if (or *mibl-debug-ppx* *mibl-debugging*)
                                (format #t "~A: ~A~%" (bgred "ppx for pkg-shared-ppx") pkg-shared-ppx))
                            (let ((shared (rassoc ppx pkg-shared-ppx)))
                              (if (null? shared)
                                  (begin
                                    (if (or *mibl-debug-ppx* *mibl-debugging*)
                                        (format #t "~A: ~A~%" (bgred "adding shared ppx") ppx))
                                    (set! pkg-shared-ppx
                                          (append pkg-shared-ppx
                                                  `((,(+ 1 shared-ct) . ,ppx))))
                                    (set-cdr! ppx-fld (+ 1 shared-ct)))
                                  ;; else
                                  (begin
                                    (if (or *mibl-debug-ppx* *mibl-debugging*)
                                        (format #t "~A: ~A~%" (bgred "found shared ppx") shared))
                                    (set-cdr! ppx-fld shared-ct))))))
                      (if ppxes
                          (begin
                            (if (or *mibl-debug-ppx* *mibl-debugging*)
                                (format #t "~A: ~A~%" (bgred "ppxes for pkg-shared-ppx") ppxes))
                            (for-each (lambda (ppx-fld)
                                        (if (or *mibl-debug-ppx* *mibl-debugging*)
                                            (format #t "~A: ~A~%" (red "a ppx fld") ppx-fld))
                                        (let ((ppx (if ppx-fld (cdr ppx-fld) #f))
                                              (shared (rassoc ppx pkg-shared-ppx)))
                                          (if (null? shared)
                                              (begin
                                                (if (or *mibl-debug-ppx* *mibl-debugging*)
                                                    (format #t "~A: ~A~%" (bgred "adding shared ppx") ppx))
                                                (set! pkg-shared-ppx
                                                      (append pkg-shared-ppx
                                                              `((,(+ 1 shared-ct) . ,ppx))))
                                                (set-cdr! ppx-fld (+ 1 shared-ct))
                                                (set! shared-ct (+ 1 shared-ct)))
                                              ;; else
                                              (begin
                                                (if (or *mibl-debug-ppx* *mibl-debugging*)
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
  (if (or *mibl-debug-ppx* *mibl-debugging*)
      (format #t "~A: ~A~%" (ublue "handle-shared-ppx") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                ;; (if (or *mibl-debug-ppx* *mibl-debugging*)
                ;;     (format #t "~A: ~A~%" (ugreen "ppx for pkg") (car kv)))
                (let* ((pkg-path (car kv))
                       ;; (_ (if (or *mibl-debug-ppx* *mibl-debugging*) (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr kv)))
                  (-handle-pkg-shared-ppx kv)))
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-handle-shared-deps pkg-kv)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "handle-shared-deps") pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-deps '()))
    (if-let ((dune (assoc :mibl pkg)))
            (begin
              (for-each (lambda (stanza)
                          (if *mibl-debugging*
                              (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                          (if (member (car stanza) '(:archive :library :ns-archive :ns-library :executable))
                              (let* ((shared-ct (length pkg-shared-deps))
                                     (deps (assoc :deps (cdr stanza))))
                                (if deps
                                    (let ((resolved (assoc :resolved (cdr deps))))
                                      (if *mibl-debugging*
                                          (begin
                                            (format #t "~A: ~A~%" (bgred "pkg-shared-deps") pkg-shared-deps)
                                            (format #t "~A: ~A~%" (ublue "deps") deps)
                                            (format #t "~A: ~A~%" (ublue "resolved") resolved)))
                                      (if resolved
                                          (if (not (number? (cdr resolved)))
                                              (let* ((deplist (cdr resolved))
                                                     (shared (rassoc deplist pkg-shared-deps)))
                                                (if *mibl-debugging*
                                                    (begin
                                                      (format #t "~A: ~A~%" (bgred "deplist") deplist)
                                                      (format #t "~A: ~A~%" (bgred "rassoc") shared)))
                                                (if (null? shared)
                                                    (begin
                                                      (if *mibl-debugging*
                                                          (format #t "~A: ~A~%" (green "adding") deplist))
                                                      (set! pkg-shared-deps
                                                            (append pkg-shared-deps
                                                                    `((,(+ 1 shared-ct) . ,deplist))))
                                                      (set-cdr! resolved (+ 1 shared-ct)))
                                                    (begin
                                                      ;; update stanza :deps with key
                                                      (if *mibl-debugging*
                                                          (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared)))
                                                      (set-cdr! resolved (car shared))
                                                      (if *mibl-debugging*
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
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "handle-shared-deps") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (uwhite "pkgs") pkgs))
    (for-each (lambda (kv)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (ugreen "dep for pkg") (car kv)))
                (let* ((pkg-path (car kv))
                       (_ (if *mibl-debugging* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
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

;; (:ns-archive
;;  (:ns . jsooexp_static_eval)
;;  (:privname . jsooexp_static_eval)
;;  (:inline-tests (:compile-opts (:flags -allow-output-patterns)) (deps (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe) (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
;;  (:compile-opts (:standard) (:opens Jsoo_compiler_expect_tests_helper))
;;  (:ppx (:scope :all) (:manifest @ppx_expect//lib/ppx_expect))
;;  (:deps (:resolved . 1))
;;  (:manifest (:modules Static_eval)))

;; for executables?

(define (-handle-shared-opts pkg-kv)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "handle-shared-opts") pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-opts '()))
    (if-let ((dune (assoc :mibl pkg)))
            (begin
              (for-each (lambda (stanza)
                          (if *mibl-debugging*
                              (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                          (if (member (car stanza) '(:archive :library :ns-archive :ns-library :executable))
                              (let* ((shared-ct (length pkg-shared-opts))
                                     (opts-fld (assoc :opts (cdr stanza))))
                                (if opts-fld
                                    (if (not (number? (cdr opts-fld)))
                                        (begin
                                          (if *mibl-debugging*
                                              (begin
                                                (format #t "~A: ~A~%" (bgred "pkg-shared-opts") pkg-shared-opts)
                                                (format #t "~A: ~A~%" (ublue "opts-fld") opts-fld)))
                                          (let* ((opts (cdr opts-fld))
                                                 (shared (rassoc opts pkg-shared-opts)))
                                            (if *mibl-debugging*
                                                (format #t "~A: ~A~%" (bgred "rassoc") shared))
                                            (if (null? shared)
                                                (begin
                                                  (if *mibl-debugging*
                                                      (format #t "~A: ~A~%" (green "adding") opts))
                                                  (set! pkg-shared-opts
                                                        (append pkg-shared-opts
                                                                `((,(+ 1 shared-ct) . ,opts))))
                                                  (set-cdr! opts-fld (+ 1 shared-ct)))
                                                (begin
                                                  ;; update stanza :opts with key
                                                  ;; (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared))
                                                  (set-cdr! opts-fld (car shared))
                                                  (if *mibl-debugging*
                                                      (format #t "~A: ~A~%" (ugreen "updated stanza") stanza))
                                                  )))))))))
                        (cdr dune))
              ;; now add pkg-shared-opts to pkg with key :shared-opts
              ;; (format #t "~A~%" (bgred "updating pkg with :shared-opts"))
              (if (truthy? pkg-shared-opts)
                  (set-cdr! dune (append (cdr dune) `((:shared-compile-opts ,pkg-shared-opts)))))))
    ;; (format #t "~A: ~A~%" (bgred "shared opts") pkg-shared-opts)
    '()))

(define (handle-shared-opts ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "handle-shared-opts") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (ugreen "opt for pkg") (car kv)))
                (let* ((pkg-path (car kv))
                       (_ (if *mibl-debugging* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr kv)))
                  ;; (format #t "~A: ~A~%" (green "pkg") pkg)
                  (if *mibl-debugging*
                      (format #t "~A: ~A~%" (green "*mibl-shared-deps*") *mibl-shared-deps*))
                  ;; (if (member pkg-path *mibl-shared-deps*)
                  (-handle-shared-opts kv) ;;)
                  )
                ;; (format #t "~A: ~A~%" (bgred "updated pkg") kv)
                )
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))
