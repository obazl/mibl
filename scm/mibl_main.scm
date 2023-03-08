
(define* (-main root-path pkg-path)
  (format #t "mibl_main: -main routine\n"))

(load "libmibl.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-mibl-load-project path)
  (format #t "~A: ~A (~A)~%" (blue "-mibl-load-project") path (type-of path))
  (format #t "pwd: ~A~%" (pwd))
  (let* ((_wss  (if path (mibl-load-project #|(pwd)|# path)
                    (mibl-load-project)))
         ;; (_ (format #t "~A: ~A~%" (green "_wss") _wss))
         )
    _wss))

(define (-miblize ws)
  (if #t ;; *debugging*
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
    (if #t ;; *debugging*
        (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist)))
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

(define* (-main root-path path)
  (format #t "mibl.scm::-main: ~A~%" path)
  ;; (format #t "*mibl-project*: ~A~%" *mibl-project*)
  ;; (format #t "BYE~%"))

  (set! *build-dyads* #f)
  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  (let* ((_wss (-mibl-load-project "multilibs"))
         (_ (debug-print-pkgs :@))

         (mpkgs (-miblize :@))
         (mpkgs (add-filegroups-to-pkgs :@))
         (mpkgs (normalize-manifests! :@))
         (mpkgs (normalize-rule-deps! :@))

         (miblarkize :@)
         (resolve-pkg-file-deps :@)

         (resolve-labels! :@)

         (handle-shared-ppx :@)

         (if *shared-deps*
             (begin
               (handle-shared-deps :@)
               (handle-shared-opts :@)
               ))

         ;; (_ (-emit-starlark :@))

         (_ (format #t "~A~%" (red "PKG DUMP")))
         (_ (debug-print-pkgs :@))
         (_ (debug-print-exports-table :@))
         (_ (debug-print-filegroups :@))

         )
    '()))
