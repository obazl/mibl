;; derive :manifest for aggregate from (modules) list
(define (get-manifest pkg kind wrapped? stanza-alist) ;;  deps
  (if (or *mibl-debug-all* *mibl-debug-modules*)
      (begin
        (format #t "~A: ~A\n" (ublue "get-manifest") stanza-alist)
        (if *mibl-debug-show-pkgs* (format #t "~A: ~A\n" "pkg" pkg))
        (format #t "~A: ~A\n" "kind" kind)))
  ;; (if deps
  (let* ((submods+sigs-list
          ;; the main routine:
          (expand-modules-fld
           kind
           (assoc 'modules stanza-alist)
           ;; files
           (assoc :modules pkg)
           ;; deps
           (assoc :signatures pkg)
           (assoc :structures pkg)))
         (_ (if *mibl-debug-modules*
                (format #t "~A: ~A\n" (uwhite "1 submods+sigs-list") submods+sigs-list)))
         (submods+sigs-list (if (equal? (cadr submods+sigs-list)
                                     '(:signatures))
                                (list (car submods+sigs-list))
                                submods+sigs-list))
         (submods+sigs-list (if (equal? (cdr submods+sigs-list)
                                     '(:modules))
                                (cdr submods+sigs-list)
                                ;; (cdr submods+sigs-list)
                                submods+sigs-list)))
    (if (or *mibl-debug-all* *mibl-debug-modules*)
        (format #t "~A: ~A\n" (uwhite "submods+sigs-list") submods+sigs-list))
    (if (null? submods+sigs-list)
        '()
        (cons :manifest
              (remove () submods+sigs-list)))))
            ;; (let ((submods (reverse (car submods+sigs-list)))
            ;;       (subsigs (reverse (cdr submods+sigs-list))))
            ;;   (cons :manifest (remove '()
            ;;                            (list submods subsigs)))))))

;;FIXME: rename.  reconcile-pkg-files?
;; normalize-aggregate-manifests!: one for each aggregate, plus pkg files (:modules,
;; :signatures, :structures) are manifests.

;; updates stanzas

;; task 1: remove items from pkg-structs and pkg-sigs if they are also
;; in pkg-modules. this can happen with lex and yacc files.

;; task 2: re-populate the (:manifest :modules) of aggregates.

;; Stage 1 processing of aggregates resolves module deps with what's
;; avaiable in pkg modules and structs. but some dynamic files could
;; be added by a later stanza. So we need to repeat the process after
;; all dynamic files have been added to the pkg file fields.

;; Only add dynamics.

;; BUT: do this only for libs with default :standard modules (even
;; with exclusions), not for those that explicitly list their
;; manifests, e.g. (modules yojson).

;; (even then this may not always work, e.g. given (modules foo bar)
;; where bar is generated.

;;FIXME: not necessary???
(define (normalize-aggregate-manifests! ws)
  (if (or *mibl-debug-all* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (bgblue "normalize-aggregate-manifests!") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    (for-each (lambda (pkg-kv)
                (let* ((pkg-key (car pkg-kv))
                       (pkg (cdr pkg-kv))
                       (stanzas (assoc-val :mibl (cdr pkg-kv))))
                  ;; (format #t "~%~A: ~A~%" (bgcyan "pkg key") pkg-key)

                  ;; (format #t "~A: ~A~%" (bgmagenta ":structures before") (assoc :structures pkg))
                  ;; task 1: reconcile pkg files: :modules, :structures, :signatures etc.

                  ;; (set! pkg (normalize-pkg-files! pkg))


                  ;; (format #t "~A: ~A~%" (bgmagenta ":structures after") (assoc :structures pkg))
                  ;; (set! pkg (-normalize-pkg-files pkg))
                  ;; (format #t "~A: ~A~%" (bgred "normed pkg") pkg)
                  ;; (if (equal? "compiler/lib" (assoc-val :pkg-path (cdr pkg-kv)))
                  ;;     (error 'STOP "nmani"))

                  ;; for each aggregate stanza, resolve the (modules) fld
                  (if stanzas
                      (for-each (lambda (stanza)
                                  (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                      (format #t "~A: ~A~%" (blue "stanza") stanza))
                                  (case (car stanza)
                                    ((:ns-archive
                                      :archive
                                      :ns-library
                                      :library)
                                     (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                         (format #t "~A: ~A~%" (ured "aggregate")
                                                 (assoc-val :privname (cdr stanza))))
                                     (let* ((stanza-alist (cdr stanza))
                                            (_ (if (or *mibl-debug-all* *mibl-debug-updaters*) (format #t "~A: ~A~%" (ured "stanza-alist") stanza-alist)))
                                            (old-manifest (assoc ':manifest stanza-alist))
                                            (_ (if (or *mibl-debug-all* *mibl-debug-updaters*) (format #t "~A: ~A~%" (ured "old-manifest") old-manifest)))
                                            (mmods (assoc-in '(:manifest :raw) stanza-alist))
                                            (_ (if (or *mibl-debug-all* *mibl-debug-updaters*) (format #t "~A: ~A~%" (ured "mmods") mmods)))
                                            (manifest
                                             (get-manifest pkg :lib #t (cons (cadr mmods) stanza-alist)) ;; (cadr mmods))
                                             ;;(x-get-manifest pkg #t stanza-alist (cadr mmods))
                                             ))
                                       (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                           (begin
                                             (format #t "~A: ~A~%" (ured "structures") (assoc :structures stanza-alist))
                                             (format #t "~A: ~A~%" (uyellow "mmods") mmods)
                                             (format #t "~A: ~A~%" (uyellow "old manifest") old-manifest)
                                             (format #t "~A: ~A~%" (uyellow "manifest") manifest)))
                                       (set-cdr! old-manifest (cdr manifest))
                                       (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                           (format #t "~A: ~A~%" (uyellow "updated old manifest") old-manifest))
                                       ))

                                    ;; rule outputs should already be in pkg files
                                    ;; ((:rule)
                                    ;;  (format #t "~A~%" (ured "rule"))
                                    ;;  (for-each (lambda (o)
                                    ;;              (format #t "~A: ~A~%" (red "OUT") o)
                                    ;;              (let ((tgt (assoc-val :tgt (cdr o))))
                                    ;;                (format #t "~A: ~A~%" (red "tgt") tgt)
                                    ;;                (if (= (libc:fnmatch  "*.ml" tgt 0) 0)
                                    ;;                    (update-pkg-files-with-struct! pkg tgt))
                                    ;;                ))
                                    ;;            (assoc-val ::outputs (cdr stanza))))
                                    (else)))
                                stanzas)
                          )))
              pkgs)
    ;; (if (or *mibl-debug-all* *mibl-debug-updaters*)
    ;;     (begin
    ;;       (mibl-debug-print-pkgs :@)
    ;;       (error 'x "")))
    ))

