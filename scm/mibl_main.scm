
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

(define (-miblize ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "-miblize") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (kv)
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr kv))
                            (if (assoc 'dune (cdr kv))
                                (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv))))
                                  ;; (format #t "~A: ~A~%" (red "miblized") mibl-pkg)
                                  (hash-table-set! pkgs (car kv) mibl-pkg)
                                  mibl-pkg)
                                (begin
                                  ;; (format #t "~A: ~A~%" (red "miblize: no dune file") kv)
                                  (cdr kv))
                                ))
                         pkgs)))
        ;; (_ (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist))
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (blue "mibl pkg ct") (length mpkg-alist)))
    mpkg-alist))

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

  (let* (;;(_wss (-mibl-load-project root-path ws-path))
         (mpkgs (-miblize :@))
         (mpkgs (add-filegroups-to-pkgs :@))
         (mpkgs (normalize-manifests! :@))
         (mpkgs (normalize-rule-deps! :@))
         )
    ;; start dune-specific
    (miblarkize :@)
    (resolve-pkg-file-deps :@)

    (resolve-labels! :@)

    (handle-shared-ppx :@)

    (if *mibl-shared-deps*
        (begin
          (handle-shared-deps :@)
          (handle-shared-opts :@)
          ))

    ;; (ppx-inline-tests! :@)

    (if *mibl-emit-wss*
        (emit-mibl-wss))

    (if *mibl-emit-pkgs*
        (emit-mibl-pkgs))

    (if *mibl-show-mibl*
        (begin
          (format #t "~A~%" (bgred "MIBL"))
          (mibl-debug-print-pkgs :@)
          ;; (mibl-pretty-print *mibl-project*)
          (return)))

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
    )
  (if (not *mibl-quiet*)
        (begin
          (format #t "~A: Workspace root: ~A~%" (green "INFO") ws-path)
          (format #t "~A: Processed ~A dunefiles.~%" (green "INFO") *mibl-dunefile-count*)))
  '())

(define* (-main root-path ws-path)
  (if *mibl-clean-mibl*
      (mibl-clean-mibl)
      (call-with-exit (lambda (return)
                        (-dune->mibl return root-path ws-path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define* (-xdune->mibl root-path path)
;;   (format #t "mibl.scm::-main: ~A~%" path)
;;   ;; (format #t "*mibl-project*: ~A~%" *mibl-project*)
;;   ;; (format #t "BYE~%"))

;;   (set! *mibl-build-dyads* #f)
;;   ;; (set! *mibl-wrapped-libs-to-ns-archives* #f)
;;   ;; (set! *mibl-unwrapped-libs-to-archives* #f)

;;   (let* ((_wss (-mibl-load-project path))
;;          (_ (mibl-debug-print-pkgs :@))

;;          (mpkgs (-miblize :@))
;;          (mpkgs (add-filegroups-to-pkgs :@))
;;          (mpkgs (normalize-manifests! :@))
;;          (mpkgs (normalize-rule-deps! :@))

;;          (miblarkize :@)
;;          (resolve-pkg-file-deps :@)

;;          (resolve-labels! :@)

;;          (handle-shared-ppx :@)

;;          ;; (if *mibl-shared-deps*
;;          ;;     (begin
;;          ;;       (handle-shared-deps :@)
;;          ;;       (handle-shared-opts :@)
;;          ;;       ))

;;          ;; (_ (-emit-starlark :@))

;;          (_ (format #t "~A~%" (red "PKG DUMP")))
;;          (_ (mibl-debug-print-pkgs :@))
;;          (_ (mibl-debug-print-exports-table :@))
;;          (_ (mibl-debug-print-filegroups :@))

;;          )
;;     '()))

