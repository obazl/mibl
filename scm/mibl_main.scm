
(define* (-main root-path pkg-path)
  (format #t "mibl_main: -main routine\n"))

(load "libmibl.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-mibl-load-project root path)
  (if *mibl-debugging*
      (begin
        (format #t "~A: ~A (~A)~%" (blue "-mibl-load-project") path (type-of path))
        (format #t "pwd: ~A~%" (pwd))))
  (let* ((_wss  (if path (mibl-load-project #|(pwd)|# path)
                    (mibl-load-project)))
         ;; (_ (format #t "~A: ~A~%" (green "_wss") _wss))
         )
    _wss))

(define (-resolve-labels ws)
  (resolve-labels! (assoc-val ws *mibl-project*)))

(define (-miblarkize ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "miblarkizing") kv)
                ;; dir may have dune-project but no dune file:
                (if (not (null? (cdr kv)))
                    (mibl-pkg->miblark (cdr kv)))
                )
              pkgs)))

(define* (-dune->mibl return root-path ws-path)
  ;; (set! *mibl-debugging* #t)
  (if *mibl-debugging*
      (format #t "convert_dune.scm::dune->obazl: ~A, ~A~%" root-path ws-path))
  ;; (format #t "*mibl-project*: ~A~%" *mibl-project*)
  ;; (format #t "BYE~%"))

  (if *mibl-debugging*
      (format #t "~A: ~A~%" (bgred "*mibl-emit-bazel-pkg*")
              *mibl-emit-bazel-pkg*))

  (set! *mibl-build-dyads* #t)
  (set! *mibl-shared-deps* '("compiler/tests-compiler")) ;;  "toplevel/bin"))

  ;; (set! *mibl-wrapped-libs-to-ns-archives* #f)
  ;; (set! *mibl-unwrapped-libs-to-archives* #f)

  ;; NB: :@ is key of the root workspace in *mibl-project*
  ;; (set! *mibl-debugging* #t)

  (-mibl-load-project root-path ws-path)

  (if *mibl-show-parsetree*
      (begin
        (format #t "PARSETREE~%")
        (mibl-debug-print-pkgs :@)
        (return)))

  (if *mibl-emit-parsetree*
      (begin
        (emit-parsetrees)))

  ;; (return)

  (miblize :@)

  ;; (return)

  (add-filegroups-to-pkgs :@)

  ;; (return)

  (normalize-manifests! :@)

  ;; (return)

  (normalize-rule-deps! :@)

  ;; (return)

  ;; start dune-specific
  (miblarkize :@)

  ;; (return)

  (resolve-pkg-file-deps :@)

  ;; (return)

  (resolve-labels! :@)

  ;; (return)

  (handle-shared-ppx :@)

  (if *mibl-shared-deps*
      (begin
        (handle-shared-deps :@)
        (handle-shared-opts :@)
        ))

  ;; (ppx-inline-tests! :@)

  (if *mibl-emit-wss*
      (begin
        (emit-mibl-wss)))

  (if *mibl-emit-pkgs*
      (emit-mibl-pkgs))

  (if *mibl-show-mibl*
      (begin
        (format #t "~A~%" (bgred "MIBL"))
        ;; (mibl-debug-print-pkgs :@)
        (mibl-debug-print-project)
        ;; (mibl-pretty-print *mibl-project*)
        ;;(return)
        ))

  ;; (return)

  ;; end dune-specific?

  ;; (if *mibl-emit-mibl*
  ;;     (emit-mibl))
  ;; (emit-mibl :@))

  ;; ;; (ws->opam-bundles :@)

  ;; (if *mibl-debugging*
  ;;     (format #t "~A: ~A~%" (green "selectors"))
  ;;         (remove-duplicates *select-protases*))

  (if *mibl-show-exports*
      (mibl-debug-print-exports-table :@))

  ;; (-dump-ppx :@)

  ;; (mibl-debug-print-filegroups :@)

  ;; (-dump-opam :@)
  ;; )
  (if (not *mibl-quiet*)
      (begin
        (format #t "~A: mibl_main Workspace root: ~A~%" (green "INFO") ws-path)
        (format #t "~A: mibl_main Processed ~A dunefiles.~%" (green "INFO") *mibl-dunefile-count*)))
  '())

(define* (-main root-path ws-path)
  (if *mibl-clean-mibl*
      (mibl-clean-mibl)
      (call-with-exit (lambda (return)
                        (-dune->mibl
                         (lambda ()
                           (if *mibl-show-mibl*
                               (mibl-debug-print-project))
                           (if (not *mibl-quiet*)
                               (format #t "~A: Returning...~%" (green "INFO")))
                           (return))
                         root-path ws-path)))))

