;; (:ns-archive
;;  (:ns . jsooexp_static_eval)
;;  (:privname . jsooexp_static_eval)
;;  (:inline-tests (:compile-opts (:flags -allow-output-patterns)) (deps (file ../../compiler/bin-js_of_ocaml/js_of_ocaml.exe) (file ../../compiler/bin-jsoo_minify/jsoo_minify.exe)))
;;  (:compile-opts (:standard) (:opens Jsoo_compiler_expect_tests_helper))
;;  (:ppx (:scope :all) (:manifest @ppx_expect//lib/ppx_expect))
;;  (:deps (:resolved . 1))
;;  (:manifest (:modules Static_eval)))

;; for executables?

;; shareable opts flds:
;;  flags -> :opts
;;  link_flags -> :link-opts
;;  ocamlc_flags -> :ocamlc_opts
;;  ocamlopt_flags -> :ocamlopt_opts

(define (-handle-shared-opts pkg-kv opt-type)
  (mibl-trace-entry "-Handle-shared-opts")
  (mibl-trace "opt-type" opt-type)
  ;; (if (or *mibl-debug-shared* *mibl-debug-all*)
  ;;     (format #t "~A: ~A - ~A~%" (ublue "-handle-shared-opts") opt-type pkg-kv))
  (let* ((pkg (cdr pkg-kv))
         (pkg-shared-opts '())) ;; we will set! this as we iterate over stanzas
    (if-let ((dune (assoc :mibl pkg)))
            (begin
              (for-each (lambda (stanza)
                          (if *mibl-debug-all*
                              (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                          (case (car stanza)
                            ((:archive :library :ns-archive :ns-library :executable)
                             ;; (if (member (car stanza) '(:archive :library :ns-archive :ns-library :executable))
                             (let* ((shared-ct (length pkg-shared-opts))
                                    (opts-fld (assoc opt-type (cdr stanza))))
                               (if opts-fld
                                   (if (not (number? (cdr opts-fld)))
                                       (begin
                                         (if *mibl-debug-all*
                                             (begin
                                               (format #t "~A: ~A~%" (green "pkg-shared-opts") pkg-shared-opts)
                                               (format #t "~A: ~A~%" (green "opts-fld") opts-fld)))
                                         (let* ((opts (cdr opts-fld))
                                                (shared (rassoc opts pkg-shared-opts)))
                                           (if *mibl-debug-all*
                                               (format #t "~A: ~A~%" (green "rassoc") shared))
                                           (if (null? shared)
                                               (begin
                                                 (if *mibl-debug-all*
                                                     (format #t "~A: ~A~%" (green "adding") opts))
                                                 (set! pkg-shared-opts
                                                       (append pkg-shared-opts
                                                               `((,(+ 1 shared-ct) . ,opts))))
                                                 (set-cdr! opts-fld (+ 1 shared-ct)))
                                               (begin
                                                 ;; update stanza :opts with key
                                                 ;; (format #t "~A: ~A => ~A~%" (ugreen "updating stanza") resolved (car shared))
                                                 (set-cdr! opts-fld (car shared))
                                                 (if *mibl-debug-all*
                                                     (format #t "~A: ~A~%" (ugreen "updated stanza") stanza))
                                                 ))))))))))
                        (cdr dune))
              ;; (format #t "~A: ~A~%" (green "updating pkg with shared opts") pkg-shared-opts)
              (if (truthy? pkg-shared-opts)
                  (let ((key (case opt-type
                               ((:opts) :shared-opts)
                               ((:link-opts) :shared-link-opts)
                               ((:ocamlc-opts) :shared-ocamlc-opts)
                               ((:ocamlopt-opts) :shared-ocamlopt-opts))))
                  (set-cdr! dune (append (cdr dune) `((,key ,pkg-shared-opts))))))))
    ;; (format #t "~A: ~A~%" (bgred "shared opts") pkg-shared-opts)
    '()))

(define (-handle-shared-opts-for-prologues pkg-kv opt-type)
  (if (or *mibl-debug-shared* *mibl-debug-all*)
      (format #t "~A: ~A - ~A~%" (ublue "-handle-shared-opts-for-prologues") opt-type pkg-kv))
  (let* ((pkg (cdr pkg-kv)))
         ;; (pkg-shared-opts '())) ;; we will set! this as we iterate over stanzas
    (if-let ((dune (assoc :mibl pkg)))
            (let* ((shared-opts (assoc :shared-opts (cdr dune)))
                   (shared-link-opts (assoc :shared-link-opts (cdr dune)))
                   (shared-ocamlc-opts (assoc :shared-ocamlc-opts (cdr dune)))
                   (shared-ocamlopt-opts (assoc :shared-ocamlopt-opts (cdr dune))))
              (begin
                (for-each (lambda (stanza)
                            (if *mibl-debug-all*
                                (format #t "~A: ~A~%" (bgblue "stanza") stanza))
                            (case (car stanza)
                              ((:prologues)
                               (for-each (lambda (prologue)
                                           (format #t "~A: ~A~%" (green "prologue") prologue)
                                           (let* ((opts (assoc :opts (cdr prologue)))
                                                  (_ (format #t "~A: ~A~%" (green "opts") opts))
                                                  (_ (format #t "~A: ~A~%" (green ":shared-opts") shared-opts))
                                                  (shared-opt (if shared-opts
                                                                  (rassoc (cdr opts) (cadr shared-opts))
                                                                  #f))

                                                  (link-opts (assoc :link-opts (cdr prologue)))
                                                  (shared-link-opt (if shared-link-opts
                                                                       (rassoc (cdr link-opts) (cadr shared-link-opts))
                                                                       #f))

                                                  (ocamlc-opts (assoc :ocamlc-opts (cdr prologue)))
                                                  (shared-ocamlc-opt (if shared-ocamlc-opts
                                                                         (rassoc (cdr ocamlc-opts) (cadr shared-ocamlc-opts))
                                                                         #f))

                                                  (ocamlopt-opts (assoc :ocamlopt-opts (cdr prologue)))
                                                  (shared-ocamlopt-opt (if shared-ocamlopt-opts
                                                                           (rassoc (cdr ocamlopt-opts) (cadr shared-ocamlopt-opts))
                                                                           #f))
                                                 )
                                             (if shared-opt (set-cdr! opts (car shared-opt)))
                                             (if shared-link-opt (set-cdr! link-opts (car shared-link-opt)))
                                             (if shared-ocamlc-opt (set-cdr! ocamlc-opts (car shared-ocamlc-opt)))
                                             (if shared-ocamlopt-opt (set-cdr! ocamlopt-opts (car shared-ocamlopt-opt)))
                                             )
                                           )
                                         (cdr stanza))
                               )))
                          (cdr dune))
                ;; (error 'x "X")
                )))
    '()))

(define (handle-shared-opts ws)
  (mibl-trace-entry "handle-shared-opts")
  (mibl-trace "ws" ws)

  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    ;; (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (pkg-kv)
                (if *mibl-debug-all*
                    (format #t "~A: ~A~%" (ugreen "opt for pkg") (car pkg-kv)))
                (let* ((pkg-path (car pkg-kv))
                       (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr pkg-kv)))
                  ;; (format #t "~A: ~A~%" (green "pkg") pkg)
                  (if *mibl-debug-all*
                      (format #t "~A: ~A~%" (green "*mibl-shared-deps*") *mibl-shared-deps*))
                  ;; (if (member pkg-path *mibl-shared-deps*)
                  (-handle-shared-opts pkg-kv :opts)
                  (-handle-shared-opts pkg-kv :link-opts)
                  (-handle-shared-opts pkg-kv :ocamlc-opts)
                  (-handle-shared-opts pkg-kv :ocamlopt-opts)

                  ;; since prologue opts are derived from exe stanzas, they do not update shared fld
                  ;; here we only update the prologues to reference the shared opts.
                  ;; WARNING: this must be done after all
                  ;; the other stanzas have been processed
                  ;; since they are what sets the shared
                  ;; flds.
                  (-handle-shared-opts-for-prologues pkg-kv :opts)
                  )
                ;; (format #t "~A: ~A~%" (bgred "updated pkg") pkg-kv)
                )
              pkgs)
    ;; (error 'STOP "STOP shd")
    '()))
