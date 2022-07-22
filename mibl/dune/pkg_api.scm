;; (display "pkg_api.scm") (newline)

;; return normalized path relative to ws-root
;; FIXME: returns #f for non-existent paths
;; use ->canonical-path instead
(define (normalize-pkg-path path ws-root)
  (format #t "~A: ~A~%" (blue "normalize-pkg-path") path)
  ;; path is not in pkg-path dir
  (let ((rp (realpath path '())))
    (format #t "f rel path: ~A\n" path)
    (format #t "f realpath: ~A\n" rp)
    (format #t "ws root: ~A\n" ws-root)
    (format #t "pfx?: ~A\n" (string-prefix? ws-root rp))
    (if (string-prefix? ws-root rp)
        (string-drop rp (+ 1 (string-length ws-root)))
        "problem")))

;;FIXME: put this in file_utils.scm?
(define filename->file-assoc
  (let ((+documentation+ "For now just stringify and pair with ext."))
    (lambda (filename)
      (let* ((fname (if (symbol? filename) (symbol->string filename)
                        filename))
             (ext (filename-extension fname))
             (pname (principal-name fname)))
        (cons (string->keyword (if ext (string-drop ext 1) ":file"))
              fname)))))

(define (filename->kind filename)
  (let* ((fname (format #f "~A" filename))
         (ext (filename-extension fname)))
    (cond
      ((string=? ext ".mli") :module)
      ((string=? ext ".ml") :module)

      ((string=? ext ".sh") :script)
      ((string=? ext ".js") :script)
      ((string=? ext ".py") :script)

      ((string=? ext ".c") :src)
      ((string=? ext ".h") :src)
      ((string=? ext ".cxx") :src)
      ((string=? ext ".rs") :src)

      ((string=? ext ".dat") :data)
      ((string=? ext ".json") :data)
      (else :file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; may update :signatures in pkg files
(define (-find-sig-in-sigs!? m-name tgt sigs)
  (format #t "~A: ~A~%" (blue "-find-sig-in-sigs!?") m-name)
  (format #t "~A: ~A~%" (white "tgt") tgt)
  (format #t "~A: ~A~%" (white "sigs") sigs)
  (if sigs
      (let* ((sig-alists (cdr sigs))
             (statics (assoc :static sig-alists))
             (dynamics (assoc :dynamic sig-alists))
             (fileset (if statics statics
                          (if dynamics dynamics
                              '()))))
        (format #t "~A: ~A~%" (white "statics") statics)
        (format #t "~A: ~A~%" (white "dynamics") dynamics)

        (let ((match (find-if (lambda (sig)
                                (format #t "~A: ~A~%" (green "sig") sig)
                                (equal? (format #f "~A" m-name)
                                        (format #f "~A" (car sig))))
                              (cdr fileset))))
          (format #t "~A: ~A~%" (red "MATCH") match)
          (if match
              ;; remove from pkg files, then return
              (begin
                (format #t "~A: ~A from: ~A~%" (red "removing") match fileset)
                (let ((new (dissoc (list (car match)) (cdr fileset))))
                  (format #t "~A: ~A~%" (red "new fs") new)
                  (set-cdr! fileset new)
                  match))

              (let ((match (find-if (lambda (sig)
                                      (format #t "dynamic ~A: ~A~%" (green "sig") sig)
                                      #f)
                                    (if dynamics
                                        (cdr dynamics)
                                        '()))))
                (if match
                    #f
                    #f)))))
      #f))

(define (-remove-from-pkg-files kind sig pkg)
  (format #t "~A: ~A, ~A~%" (blue "-remove-from-pkg-files") kind sig)
  ;; (format #t "~A: ~A~%" (blue "pkg") pkg)
  (let ((fileset (assoc kind pkg)))
    (format #t "~A: ~A~%" (cyan "fileset") fileset))
  )


(define update-pkg-with-targets!
  (let ((+documentation+ "INTERNAL. Add tgts to :modules (or :files etc) fld of pkg."))
    (lambda (pkg tgts)
      (format #t "~A: ~A~%" (magenta "update-pkg-with-targets!") tgts)
      ;; (format #t "  package: ~A\n" pkg)
      ;; (format #t "  targets: ~A\n" tgts)

      ;; get the alists we might update
      (let* (;;(pkg (car pkg*))
             (modules-assoc (assoc :modules pkg))
             ;; (modules (if modules-assoc (cdr modules-assoc) #f))
             (scripts (if modules-assoc (assoc-val :scripts pkg) #f))
             (data    (if modules-assoc (assoc-val :data pkg) #f))
             (files-assoc (if (assoc :files pkg)
                              (assoc-val :files pkg) #f)))
        ;; for each tgt, decide its kind: ml/mli, or other
        ;; then update the pkg fld: :modules, :scripts, :files, :data
        ;; since we're updating pkg use for-each
        (for-each
         (lambda (tgt)
           (let ((kind (filename->kind tgt)))
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
                       (matching-sig (-find-sig-in-sigs!? m-name tgt sigs))
                       (_ (format #t "~A: ~A~%" (cyan "matched") matching-sig))
                       )
                  (if matching-sig
                      (begin ;; we know this file is not in :modules since it was in sigs
                        (format #t "~A: ~A~%" (red "updating :modules") m-name)
                        (alist-update-in! pkg `(:modules ,m-name)
                                          (lambda (old)
                                            (format #t "module OLD: ~A\n" old)
                                            (if (null? old)
                                                (list pr (cons :mli (cdr matching-sig)))
                                                ;;(filename->module-assoc tgt)
                                                (append
                                                 old
                                                 (list pr)
                                                 ;;(filename->module-assoc tgt)
                                                 )))))
                      ;; else no sig, so update :structures
                      (let* ((s-assoc (assoc-in '(:structures :dynamic) pkg))
                             ;; (structures (if s-assoc (append (cadr s-assoc) '(Foo . bar)) '()))
                             )
                        ;; (format #t "~A: ~A~%" (red "STructures") structures)
                        (alist-update-in! pkg `(:structures :dynamic)
                                        (lambda (old)
                                          (format #t "module OLD: ~A\n" old)
                                          (if (null? old)
                                              (cons m-name (cdr pr))
                                              ;;(filename->module-assoc tgt)
                                              (append
                                               old
                                               (list (cons m-name (cdr pr)))
                                               ;; structures
                                               ;;(filename->module-assoc tgt)
                                               )))))))
                pkg)
               ;; (if modules-assoc
               ;;     (begin
               ;;       (format #t
               ;;               "modules-assoc (before): ~A\n" modules-assoc)
               ;;       (set-cdr! modules-assoc
               ;;                 (append (list (filename->module-assoc tgt))
               ;;                         (cdr modules-assoc)))
               ;;       (format #t
               ;;               "modules-assoc (after): ~A\n" modules-assoc))
               ;;     ;; else
               ;;     (begin
               ;;       (format #t "initializing modules-assoc\n")
               ;;       (set! modules-assoc (cons :modules (list tgt)))
               ;;       (format #t "modules-assoc (after): ~A\n"
               ;;               modules-assoc)))
               ;; )
               (else
                ;; (format #t ":other: ~A\n" tgt)
                ;; (format #t "files-assoc: ~A\n" files-assoc)
                (alist-update-in! pkg '(:files :dynamic)
                                  (lambda (old)
                                    ;; (format #t "other OLD: ~A\n" old)
                                    ;; (format #t "other tgt: ~A\n" tgt)
                                    (let ((fa (filename->file-assoc tgt))
                                          (tgtstr (if (symbol? tgt)
                                                      (symbol->string tgt)
                                                      tgt)))
                                      ;; (format #t "fa: ~A\n" fa)
                                      ;; (format #t "fa2: ~A\n"old)
                                      (if (null? old)
                                          tgtstr
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
             )) ;; lambda
         tgts) ;; for-each
        ;; (format #t "pkg (after): ~A\n" pkg)
        pkg))))

;; e.g. (cdr (:standard (symbol "\\") legacy_store_builder))
(define (pkg->module-names pkg) ;; seq)
  (let ((modules (assoc-val :modules pkg)))
    (map car modules)))

  ;; (let recur ((assoc :
  ;;             (modnames '()))
  ;;   (if (null? srcfiles)
  ;;       modnames
  ;;       (let ((m (file-name->module-name (car srcfiles))))
  ;;         (if (member m modnames) ;; avoid .ml/.mli dups
  ;;             (recur (cdr srcfiles) modnames)
  ;;             (recur (cdr srcfiles) (cons m modnames)))))))

