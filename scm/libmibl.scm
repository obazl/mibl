(if *mibl-debug-s7-loads*
    (format #t "loading libmibl.scm~%"))

(load "dune.scm")
(load "mibl_pp.scm")
(load "dune/pipeline.scm")
;; (load "codept/codept_reader.scm")

(define* (parsetree->mibl return root-path ws-path)
  ;; (set! *mibl-debugging* #t)
  (if *mibl-debugging*
      (format #t "mibl_main.scm::dune->obazl: ~A, ~A~%" root-path ws-path))
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

  ;; parsetree always already produced by c code
  ;; (-mibl-load-project root-path ws-path)

  ;; (if *mibl-show-parsetree*
  ;;     (begin
  ;;       (format #t "mibl: PARSETREE~%")
  ;;       (mibl-debug-print-pkgs :@)
  ;;       (return)))

  ;; (if *mibl-emit-parsetree*
  ;;     (begin
  ;;       (emit-parsetrees)))

  ;; (return)

  (miblize :@)
  (add-filegroups-to-pkgs :@)
  (normalize-manifests! :@)
  (normalize-rule-deps! :@)
  (miblarkize :@)
  ;; (resolve-pkg-file-deps :@)  ;; OBSOLETE
  (resolve-labels! :@)
  (handle-shared-ppx :@)
  (if *mibl-shared-deps*
      (begin
        (handle-shared-deps :@)
        (handle-shared-opts :@)
        ))

  ;; (ppx-inline-tests! :@)


  (if *mibl-show-mibl*
      (begin
        (format #t "~A~%" (bgred "MIBL"))
        ;; (mibl-debug-print-pkgs :@)
        (mibl-debug-print-project)
        ;; (mibl-pretty-print *mibl-project*)
        ;;(return)
        ))

  (if *mibl-emit-wss*
      (begin
        (emit-mibl-wss)))

  (if *mibl-emit-pkgs*
      (emit-mibl-pkgs))

  (if *mibl-emit-result*
      (emit-mibl-result))

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
        (format #t "~A: libmibl Workspace root: ~A~%" (green "INFO") ws-path)
        (format #t "~A: libmibl Processed ~A dunefiles.~%" (green "INFO") *mibl-dunefile-count*)))
  '())

(define (emit-mibl-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-mibl-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PKG.mibl"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (mibl-pretty-print pkg outp)
    (close-output-port outp)))

(define (emit-s7-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-s7-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PKG.s7"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string pkg :readable) outp)
    (close-output-port outp)))

(define (emit-mibl-pkgs)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-mibl")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-wss, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      (if (or *mibl-emit-mibl* *mibl-emit-s7*)
          (for-each (lambda (ws)
                      (if (or *mibl-debug-emit* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (yellow "ws") ws))
                      (let ((pkgs (car (assoc-val :pkgs (cdr ws)))))
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (begin
                              (format #t "~A: ~A~%" (yellow "pkgs") pkgs)
                              (format #t "~A: ~A~%" (yellow "pkgs keys")
                                      (hash-table-keys pkgs))))
                        (for-each (lambda (kv)
                                    (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                        (format #t "~%~A: ~A~%" (yellow "emitting mibl pkg") kv))
                                    (if *mibl-emit-mibl*
                                        (emit-mibl-pkg (cdr kv)))
                                    (if *mibl-emit-s7*
                                        (emit-s7-pkg (cdr kv))))
                                  ;; pkgs is a hash-table
                                  pkgs)
                        ))
                    *mibl-project*))))

(define (emit-mibl-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-mibl-ws") ws))
  (let* ((ws-path (assoc-val :path (cdr ws)))
         (mibl-file (format #f "~A/WS.mibl"  ws-path))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (mibl-pretty-print ws outp)
    (close-output-port outp)))

(define (emit-parsetree-pkg pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-pkg") pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (mibl-file (string-append pkg-path "/PARSETREE.s7"))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string pkg :readable) outp)
    (close-output-port outp)))

(define (emit-s7-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-s7-ws") ws))
  (let* ((ws-path (assoc-val :path (cdr ws)))
         (mibl-file (format #f "~A/WS.s7"  ws-path))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file mibl-file))
                 (lambda args
                   (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                 )))
    (if (not *mibl-quiet*)
        (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
    (write (object->string ws :readable) outp)
    (close-output-port outp)))

;; (let* ((pkgs-ht (car (assoc-val :pkgs (cdr ws-mibl))))
;;        (ht-entries (cdr pkgs-ht)))
;;   (format #t \"~A~%\" pkgs-ht))

(define (emit-mibl-wss)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-mibl-wss")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-wss, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      (begin
        (if *mibl-emit-mibl*
            (for-each (lambda (ws)
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (yellow "ws") ws))
                        (emit-mibl-ws ws))
                      *mibl-project*))
        (if *mibl-emit-s7*
            (for-each (lambda (ws)
                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (yellow "ws") ws))
                        (emit-s7-ws ws))
                      *mibl-project*)))))

(define (emit-parsetree-ws ws)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-ws") ws))
  (let* ((ws-path (assoc-val :path (cdr ws))))
    (if *mibl-emit-mibl*
        (let* ((mibl-file (format #f "~A/PARSETREE.mibl" ws-path))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file mibl-file))
                       (lambda args
                         (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                       )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (mibl-pretty-print ws outp)
          (close-output-port outp)))
    (if *mibl-emit-s7*
        (let* ((mibl-file (format #f "~A/PARSETREE.s7" ws-path))
                 (outp
                  (catch #t
                         (lambda ()
                           (open-output-file mibl-file))
                         (lambda args
                           (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                         )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (write (object->string ws :readable) outp)
          (close-output-port outp)))))

(define (emit-parsetree-project stem)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (yellow "emit-parsetree-project") *mibl-project*))
  (let* ((@ws (assoc-val :@ *mibl-project*))
         (ws-path (assoc-val :path (cdr @ws))))
    ;;FIXME: mkdir .mibl
    (if *mibl-emit-mibl*
        (let* ((mibl-file (format #f "~A/.mibl/~A.mibl" ws-path stem))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file mibl-file))
                       (lambda args
                         (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                       )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (mibl-pretty-print *mibl-project* outp)
          (close-output-port outp)))
    (if *mibl-emit-s7*
        (let* ((mibl-file (format #f "~A/.mibl/~A.s7" ws-path stem))
                 (outp
                  (catch #t
                         (lambda ()
                           (open-output-file mibl-file))
                         (lambda args
                           (error 'OPEN_ERROR_EMIT (format #f "OPEN ERROR: ~A~%" mibl-file)))
                         )))
          (if (not *mibl-quiet*)
              (format #t "~A: libmibl emitting: ~A~%" (green "INFO") mibl-file))
          (write (object->string *mibl-project* :readable) outp)
          (close-output-port outp)))))

(define (emit-parsetrees)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~%~A~%" (yellow "emit-parsetree")))
  (if (not (or *mibl-emit-mibl* *mibl-emit-s7*))
      (format #t "~A: ~A~%" (red "WARNING") "To emit-parsetrees, one or both of *mibl-emit-mibl* and *mibl-emit-s7* must be set.")
      ;; (for-each (lambda (ws)
      ;;             (emit-parsetree-ws ws))
      ;;           *mibl-project*)
      (emit-parsetree-project "PARSETREE")
      ))

(define (emit-mibl-result)
  (emit-parsetree-project "EXPECTED"))

(define (mibl-clean-mibl)
  (format #t "cleaning mibl - NOT IMPLEMENTED yet.\n")
  )
