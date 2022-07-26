(define (update-tagged-label-list! filename tllist pkg)
  (format #t "~A: ~A ~A~%" (blue "update-tagged-label-list!")
          filename tllist)
  (let* ((fname (format #f "~A" filename))
         (key (string->keyword fname)))
    (set-cdr! tllist
              (cons (cons key
                          (list (cons :pkg (assoc-val :pkg-path pkg))
                                (cons :tgt fname)))
                         (cdr tllist)))
    key))

(define update-pkg-files!
  (let ((+documentation+ "INTERNAL. Add tgts to :modules (or :files etc) fld of pkg."))

    (lambda (pkg tgts)
      (format #t "~%~A: ~A~%" (magenta "UPDATE-PKG-FILES!") tgts)
      (format #t "  package: ~A\n" pkg)
      ;; (format #t "  targets: ~A\n" tgts)

      ;; get the alists we might update
      (let* (;;(pkg (car pkg*))
             (modules-assoc (assoc :modules pkg))
             ;; (modules (if modules-assoc (cdr modules-assoc) #f))
             ;; (scripts (if modules-assoc (assoc-val :scripts pkg) #f))
             ;; (data    (if modules-assoc (assoc-val :data pkg) #f))
             (files-assoc (if (assoc :files pkg)
                              (assoc-val :files pkg) #f)))
        (format #t "~A: ~A~%" (cyan "modules-assoc") modules-assoc)
        ;; (format #t "~A: ~A~%" (cyan "data") data)
        ;; (format #t "~A: ~A~%" (cyan "scripts") scripts)
        (format #t "~A: ~A~%" (cyan "files-assoc") files-assoc)
        ;; for each tgt, decide its kind: ml/mli, or other
        ;; then update the pkg fld: :modules, :scripts, :files, :data
        ;; since we're updating pkg use for-each
        (for-each
         (lambda (tgt)
           (format #t "~A: ~A~%" (red "tgt") tgt)
           (format #t "~A: ~A~%" (red "tgt str") (format #f "~A" tgt))
           (if (not (string-index (format #f "~A" tgt)
                                  (lambda (ch) (equal? ch #\/))))
               (let ((kind (filename->kind (format #f "~A" tgt))))
                 (format #t "~A: ~A~%" (red "kind") kind)
                 (case kind
                   ((:module)
                    (format #t ":module tgt: ~A\n" tgt)
                    ;; if we already have corresponding sig, move to :modules
                    ;; else update :structures
                    (let* ((m-assoc (filename->module-assoc tgt))
                           ;; m-assoc == (A (:ml "a.ml"))
                           (m-name (car m-assoc))
                           (pr (cadr m-assoc))
                           (_ (format #t "~A: ~A\n" (red "PR") pr))
                           (sigs (assoc :signatures pkg))
                           (_ (format #t "~A: ~A~%" (cyan "sigs") sigs))
                           ;; removes matching sig from :signatures
                           (matching-sig (-find-module-in-sigs!? m-name tgt sigs))
                           (_ (format #t "~A: ~A~%" (cyan "found?") matching-sig))
                           )
                      (if matching-sig
                          (begin ;; we know this file is not in :modules since it was in sigs
                            (format #t "~A: ~A~%" (red "updating :modules") m-name)
                            (alist-update-in! pkg `(:modules ,m-name)
                                              (lambda (old)
                                                (format #t "module OLD: ~A\n" old)
                                                (format #t "adding: ~A\n" matching-sig)
                                                (if (null? old)
                                                    (cons pr (list (cons :mli (cdr matching-sig))))
                                                    ;;(filename->module-assoc tgt)
                                                    (list
                                                     old
                                                     (list pr)
                                                     ;;(filename->module-assoc tgt)
                                                     )))))
                          ;; else no sig, so update :structures
                          (let* ((s-assoc (assoc-in '(:structures :dynamic) pkg))
                                 ;; (structures (if s-assoc (append (cadr s-assoc) '(Foo . bar)) '()))
                                 )
                            (format #t "~A: ~A~%" (red "UPDATING (:structures :dynamic) : ") s-assoc)
                            (alist-update-in! pkg `(:structures :dynamic)
                                              (lambda (old)
                                                (format #t "structures OLD: ~A\n" old)
                                                (format #t "adding: ~A\n" pr)
                                                (if (null? old)
                                                    (list
                                                     (cons m-name (cdr pr)))
                                                    ;; (list m-name (cdr pr))
                                                    ;;(filename->module-assoc tgt)
                                                    (cons
                                                     old
                                                     (list (cons :BAR #|m-name|# (cdr pr))) ;;)))
                                                     ;; (list (cons m-name (cdr pr)))
                                                     ;; structures
                                                     ;;(filename->module-assoc tgt)
                                                     )))))))
                    pkg)

                   (else
                    (format #t ":other: ~A\n" tgt)
                    ;; (format #t "files-assoc: ~A\n" files-assoc)
                    (alist-update-in! pkg '(:files :dynamic)
                                      (lambda (old)
                                        (format #t ":files :dynamic OLD: ~A\n" old)
                                        ;; (format #t "other tgt: ~A\n" tgt)
                                        (let ((fa (filename->file-assoc tgt))
                                              (tgtstr (if (symbol? tgt)
                                                          (symbol->string tgt)
                                                          tgt)))
                                          ;; (format #t "fa: ~A\n" fa)
                                          ;; (format #t "fa2: ~A\n"old)
                                          (if (null? old)
                                              (list tgtstr)
                                              (append old (list tgtstr))
                                              ;; (if (pair? fa)
                                              ;;     (cons fa old)
                                              ;;     (append fa old))
                                              )
                                          )))
                    ;; (if-let (files-assoc (assoc-in '(:files :dynamic) pkg))
                    ;;     (begin
                    ;;       (format #t "files-assoc (before): ~A\n"
                    ;;               files-assoc)
                    ;;       (alist-update-in! pkg '(:files :dynamic)
                    ;;                         (lambda (old)
                    ;;                           (append
                    ;;                            old (filename->file-assoc tgt))))
                    ;;       ;; (set-cdr! files-assoc
                    ;;       ;;           (cons (filename->file-assoc tgt)
                    ;;       ;;                 (cdr files-assoc)))
                    ;;       (format #t "files-assoc (after): ~A\n"
                    ;;               files-assoc))
                    ;;     ;; else
                    ;;     (begin
                    ;;       (format #t "initializing files-assoc of pkg ~A\n" pkg)
                    ;;       (set! pkg
                    ;;             (append pkg
                    ;;                     (list
                    ;;                      (list :files
                    ;;                            (list :dynamic
                    ;;                                  (filename->file-assoc tgt))))))
                    ;;       ))
                    )) ;; case
                 )
               ;; else '/' found in tgt
               )) ;; lambda
         tgts) ;; for-each
        ;; (format #t "pkg (after): ~A\n" pkg)
        pkg))))

;; %{bin:foo} etc. Dune uses those prefixes to reference installation
;; locations. Since we do not do any installation, they're just labels
;; to us. E.g. when we process an executable with public name foo, we
;; add 'bin:foo' to the exports table. Any target that uses it (in a
;; rule action for example) will refer to it as 'bin:foo', so we can
;; just look it up to find its Bazel label.

(define (update-exports-table! ws tag name pkg-path)
  (format #t "~A: ~A -> ~A\n" (magenta "update-exports-table!") name pkg-path)
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws -mibl-ws-table))))
         (key (case tag
                ((:bin) (symbol (format #f "bin:~A" name)))
                ((:lib) (symbol (format #f "lib:~A" name)))
                ((:libexec) (symbol (format #f "libexec:~A" name)))
                (else name)))
         (tag (case tag
                ((:bin :lib :libexec) (list (cons tag #t)))
                (else '())))
         (spec `(,@tag
                 (:pkg ,pkg-path)
                 (:tgt ,(format #f "~A" name)))))
    (format #t "hidden exports tbl: ~A\n" exports)

    (format #t "adding ~A to exports tbl\n" name)
    (hash-table-set! exports key spec)
    (format #t "updated exports tbl: ~A\n" exports)))

(define (update-exports-table-with-targets! ws targets pkg-path)
  (format #t "~A: ~A~%" (magenta "update-exports-table-with-targets!") targets)
  (if targets
      (for-each (lambda (target)
              (format #t "~A: ~A~%" (red "target") target)
              (let* ((pkg-tgt (cdr target))
                     (pkg (assoc-val :pkg pkg-tgt))
                     (tgt (assoc-val :tgt pkg-tgt)))
                (format #t "~A: ~A~%" (magenta "pkg") pkg)
                (format #t "~A: ~A~%" (magenta "tgt") tgt)

                (update-exports-table! ws :_ tgt pkg)

               ;; (case (car target)
                ;;   ((::)
                ;;    (update-exports-table! ws :_ (cadr target) pkg-path))

                ;;   ((:_)
                ;;    (error 'fixme "unhandled :_ target"))

                ;;   (else
                ;;    (if (list? (cadr target))
                ;;        ;; (:foo.sh (:pkg "foo/bar") (:tgt "foo.sh"))
                ;;        (error 'fixme (format #f "~A" "unhandled  target"))
                ;;        ;; else (:foo.sh "foo.sh")
                ;;        (update-exports-table! ws :_ (cadr target) pkg-path)))
                ;;   )
                ))
            (cdr targets))))

(define (update-filegroups-table! ws pkg-path tgt pattern)
  (format #t "~A: ~A~%" (magenta "update-filegroups-table!") pkg-path)
  (format #t "~A: ~A~%" (green "tgt") tgt)
  (format #t "~A: ~A~%" (green "pattern") pattern)

  (let* ((filegroups (car (assoc-val :filegroups
                                     (assoc-val ws -mibl-ws-table))))
         (glob? (string-index pattern (lambda (ch)
                                              (equal? ch #\*)))))

    (format #t "hidden filegroups tbl: ~A\n" filegroups)
    (format #t "adding ~A~A to filegroups tbl\n" pkg-path tgt)

    (let ((fgroups (hash-table-ref filegroups pkg-path)))
      (format #t "~A: ~A~%" (red "fgroups") fgroups)
      (if fgroups
          (hash-table-set! filegroups pkg-path
                           (append
                            fgroups
                            (list (cons tgt (if glob?
                                                (list (cons :glob pattern))
                                                pattern)))))
          ;; else new
          (hash-table-set! filegroups pkg-path
                           (list (cons tgt (if glob?
                                               (list (cons :glob pattern))
                                               pattern)))))
      (format #t "updated filegroups tbl: ~A\n" filegroups))))
