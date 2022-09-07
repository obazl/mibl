(define (update-filegroups-table! ws pkg-path tgt pattern)
  (format #t "~A: ~A~%" (magenta "update-filegroups-table!") pkg-path)
  (format #t "~A: ~A~%" (green "tgt") tgt)
  (format #t "~A: ~A~%" (green "pattern") pattern)
  (format #t "~A: ~A~%" (green "ws") ws)
  ;; (format #t "~A: ~A~%" (green "mibl tbl") -mibl-ws-table)

  (let* ((-ws (if (keyword? ws) (assoc-val ws -mibl-ws-table) ws))
         ;; (_ (format #t "~A: ~A~%" (uwhite "-ws") -ws))
         (filegroups (car (assoc-val :filegroups -ws)))
         (_ (format #t "filegroups tbl: ~A\n" filegroups))
         (glob? (string-index pattern (lambda (ch)
                                              (equal? ch #\*)))))

    (format #t "adding ~A~A to filegroups tbl\n" pkg-path tgt)

    (let ((fgroups (hash-table-ref filegroups pkg-path)))
      (format #t "~A: ~A~%" (red "fgroups") fgroups)
      (if fgroups
          (hash-table-set! filegroups pkg-path
                           (append
                            fgroups
                            (list (cons tgt (if glob?
                                                (list (cons :glob pattern))
                                                (list (cons :file pattern)))))))
          ;; else new
          (hash-table-set! filegroups pkg-path
                           (list (cons tgt (if glob?
                                               (list (cons :glob pattern))
                                               (list (cons :file pattern)))))))
      (format #t "updated filegroups tbl: ~A\n" filegroups))))

(define (-module-in-modules? m modules)
  (format #t "~A: ~A~%" (ublue "-module-in-modules?") m)
  (find-if (lambda (mod)
             (string=? (format #f "~A" m)
                     (format #f "~A" (car mod))))
           modules))

(define (-normalize-pkg-files pkg)
  (format #t "~A: ~A~%" (ublue "-normalize-pkg-files") (assoc-val :pkg-path pkg))
  (let ((pkg-modules (assoc-val :modules pkg))
        (pkg-structs (assoc :structures pkg))
        (pkg-sigs (assoc :signatures pkg)))
    (if pkg-modules
        (begin
          (format #t "~A:~%" (cyan "pkg-modules"))
          (for-each (lambda (m)
                      (format #t "  ~A~%" m))
                    pkg-modules))
          (format #t "~A: ~A~%" (cyan "pkg-modules") pkg-modules))

    (if pkg-structs
        (let* ((statics (if-let ((statics (assoc-in '(:structures :static) pkg)))
                                statics '(:static)))
               (dynamics (if-let ((dynamics (assoc-in '(:structures :dynamic) pkg)))
                                 dynamics '(:dynamic))))
          (format #t "~A: ~A~%" (cyan "pstructs, static") statics)
          (format #t "~A: ~A~%" (cyan "pstructs, dynamic") dynamics)
          (if pkg-modules
              (begin ;; twice, once for statics, once for dynamics
                (let ((remainder
                       (filter (lambda (struct)
                                 (format #t "~A: ~A~%" (uwhite "struct") struct)
                                 (if-let ((x (-module-in-modules? (car struct) pkg-modules)))
                                         (begin
                                           (format #t "~A: ~A~%"
                                                   (uwhite "in modules?") x)
                                           #f)
                                         #t))
                               (cdr statics))))
                  (format #t "~A: ~A~%" (ured "structs static remainder") remainder)
                  (if (null? remainder)
                      (dissoc! '(:structures :static) pkg)
                      (set-cdr! statics remainder))
                  )
                ;; (format #t "~A~%" (bgmagenta "dynstructs"))
                (let ((remainder
                       (filter (lambda (struct)
                                 (format #t "~A: ~A~%" (uwhite "struct") struct)
                                 (if-let ((x (-module-in-modules? (car struct) pkg-modules)))
                                         (begin
                                           (format #t "~A: ~A~%"
                                                   (uwhite "in modules?") x)
                                           #f)
                                         #t))
                               (cdr dynamics))))
                (format #t "~A: ~A~%" (ured "structs dyn remainder") remainder)
                (if (null? remainder)
                    (begin
                      ;; (dissoc! '(:structures :dynamic) pkg)
                      )
                    (alist-update-in! pkg `(:structures :dynamic)
                                      (lambda (old) remainder)))
                    ;; (set-cdr! dynamics remainder))
                ))
              )))
    ;; (format #t "~A: ~A~%" (bgmagenta "updated structs") (assoc :structures pkg))
    ;; (if (equal? "compiler/lib" (car (assoc-val :pkg-path pkg)))
    ;;     (error 'STOP "nmani"))

    (if pkg-sigs
        (let* ((_ (format #t "~A: ~A~%" (red "pkg-sigs") pkg-sigs))
               (psigs (cdr pkg-sigs))
               (statics (if-let ((statics (assoc-val :static psigs)))
                                statics '()))
               (dynamics (if-let ((dynamics (assoc-val :dynamic psigs)))
                                 dynamics '())))
          (format #t "~A: ~A~%" (cyan "psigs, static") statics)
          (format #t "~A: ~A~%" (cyan "psigs, dynamic") dynamics)
          (if pkg-modules
              (begin ;; twice, once for statics, once for dynamics
                (let ((remainder
                       (filter (lambda (sig)
                                 (format #t "~A: ~A~%" (uwhite "sig") sig)
                                 (if-let ((x (-module-in-modules? (car sig) pkg-modules)))
                                         (begin
                                           (format #t "~A: ~A~%"
                                                   (uwhite "in modules?") x)
                                           #f)
                                         #t))
                               (concatenate statics dynamics))))
                  (format #t "~A: ~A~%" (ured "sigs remainder") remainder)
                  (if (null? remainder)
                      (dissoc! '(:signatures) pkg)
                      (set-cdr! pkg-sigs remainer)))))))
    pkg))

;; normalize-manifests: one for each aggregate, plus pkg files (:modules,
;; :signatures, :structures) are manifests.

;; task 1: remove items from pkg-structs and pkg-sigs if they are also
;; in pkg-modules. this can happen with lex and yacc files.

;; task 2: re-populate the (:manifest :modules) of aggregates.

;; Stage 1 processing of aggregates resolves module deps with what's
;; avaiable in pkg modules and structs. but some dynamic files could
;; be adeed by a later stanza. So we need to repeat the process after
;; all dynamic files have been added to the pkg file fields.
(define (normalize-manifests! ws)
  (format #t "~A: ~A~%" (bgblue "normalize-manifests") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    (for-each (lambda (kv)
                (let* ((pkg-key (car kv))
                       (pkg (cdr kv))
                       (stanzas (assoc-val :dune (cdr kv))))
                  (format #t "~%~A: ~A~%" (bgcyan "pkg key") pkg-key)

                  (format #t "~A: ~A~%" (bgmagenta ":structures before") (assoc :structures pkg))
                  ;; task 1.
                  (set! pkg (-normalize-pkg-files pkg))
                  (format #t "~A: ~A~%" (bgmagenta ":structures after") (assoc :structures pkg))
                  ;; (set! pkg (-normalize-pkg-files pkg))
                  ;; (format #t "~A: ~A~%" (bgred "normed pkg") pkg)
                  ;; (if (equal? "compiler/lib" (car (assoc-val :pkg-path (cdr kv))))
                  ;;     (error 'STOP "nmani"))

                  ;; for each aggregate stanza, resolve the (modules) fld
                  (if stanzas
                      (for-each (lambda (stanza)
                                  (format #t "~A: ~A~%" (blue "stanza") stanza)
                                  (case (car stanza)
                                    ((:ns-archive
                                      :archive
                                      :ns-library
                                      :library)
                                     (format #t "~A: ~A~%" (ured "aggregate")
                                             (assoc-val :privname (cdr stanza)))
                                     (let* ((stanza-alist (cdr stanza))
                                            (_ (format #t "~A: ~A~%" (ured "stanza-alist") stanza-alist))
                                            (old-manifest (assoc ':manifest stanza-alist))
                                            (mmods (assoc-in '(:manifest :raw) stanza-alist))
                                            (manifest (x-get-manifest pkg #t stanza-alist (cadr mmods))))
                                       (format #t "~A: ~A~%" (ured "structures") (assoc :structures stanza-alist))
                                       (format #t "~A: ~A~%" (uyellow "mmods") mmods)
                                       (format #t "~A: ~A~%" (uyellow "old manifest") old-manifest)
                                       (format #t "~A: ~A~%" (uyellow "manifest") manifest)
                                       (set-cdr! old-manifest (cdr manifest))
                                       ))

                                    ;; rule outputs should already be in pkg files
                                    ;; ((:rule)
                                    ;;  (format #t "~A~%" (ured "rule"))
                                    ;;  (for-each (lambda (o)
                                    ;;              (format #t "~A: ~A~%" (red "OUT") o)
                                    ;;              (let ((tgt (assoc-val :tgt (cdr o))))
                                    ;;                (format #t "~A: ~A~%" (red "tgt") tgt)
                                    ;;                (if (= (fnmatch "*.ml" tgt 0) 0)
                                    ;;                    (update-pkg-files-with-struct! pkg tgt))
                                    ;;                ))
                                    ;;            (assoc-val :outputs (cdr stanza))))
                                    (else)))
                                stanzas)
                          )))
              pkgs)
    (format #t "~A: ~A~%" (bgblue "//normalize-manifests") ws)
      ))

  ;; (let* ((@ws (assoc-val ws -mibl-ws-table))
  ;;        (ws-path (car (assoc-val :path @ws)))
  ;;        (_ (format #t "~A: ~A~%" (blue "ws-path") ws-path))
  ;;        (pkgs (car (assoc-val :pkgs @ws)))
  ;;        (filegroups (car (assoc-val :filegroups @ws))))
  ;;   (format #t "~A: ~A~%" (yellow "fg-table") filegroups)
  ;;   (for-each (lambda (kv)
  ;;               (let* ((fg-path (car kv))
  ;;                      (fg-key (car kv))
  ;;                      (fg-pkg (hash-table-ref pkgs fg-key)))
  ;;                 (format #t "\n~A: ~A~%" (yellow "fg-path") fg-path)
  ;;                 (format #t "~A: ~A~%" (yellow "fg-key") fg-key)
  ;;                 (format #t "~A: ~A~%" (yellow "pkg") fg-pkg)
  ;;                 ;; WARNING: fg-pkg will be #f if it is not in scope
  ;;                 ;; e.g. its a globbed super-dir
  ;;                 (if fg-pkg
  ;;                     (hash-table-set! pkgs fg-key
  ;;                                      (append fg-pkg
  ;;                                              (list
  ;;                                               (cons :filegroups (cdr kv)))))
  ;;                     (hash-table-set! pkgs fg-key
  ;;                                      (list
  ;;                                       `(:ws-path ,ws-path)
  ;;                                       `(:pkg-path ,fg-path)
  ;;                                       `(:realpath
  ;;                                         ,(realpath (string-join (map (lambda (x) (format #f "~A" x))
  ;;                                                                      (list  ws-path fg-path)) "/")
  ;;                                                    '()))
  ;;                                       (cons :filegroups (cdr kv)))))
  ;;                 ;; (for-each (lambda (fg)
  ;;                 ;;             (format #t "~A: ~A~%" (yellow "fg") fg))
  ;;                 ;;           (cdr kv))
  ;;                 )
  ;;               )
  ;;             filegroups)))

(define (add-filegroups-to-pkgs ws)
  (format #t "~A: ~A~%" (blue "-add-filegroups-to-pkgs") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (ws-path (car (assoc-val :path @ws)))
         (_ (format #t "~A: ~A~%" (blue "ws-path") ws-path))
         (pkgs (car (assoc-val :pkgs @ws)))
         (filegroups (car (assoc-val :filegroups @ws))))
    (format #t "~A: ~A~%" (yellow "fg-table") filegroups)
    (for-each (lambda (kv)
                (let* ((fg-path (car kv))
                       (fg-key (car kv))
                       (fg-pkg (hash-table-ref pkgs fg-key)))
                  (format #t "\n~A: ~A~%" (yellow "fg-path") fg-path)
                  (format #t "~A: ~A~%" (yellow "fg-key") fg-key)
                  (format #t "~A: ~A~%" (yellow "pkg") fg-pkg)
                  ;; WARNING: fg-pkg will be #f if it is not in scope
                  ;; e.g. its a globbed super-dir
                  (if fg-pkg
                      (hash-table-set! pkgs fg-key
                                       (append fg-pkg
                                               (list
                                                (cons :filegroups (cdr kv)))))
                      (hash-table-set! pkgs fg-key
                                       (list
                                        `(:ws-path ,ws-path)
                                        `(:pkg-path ,fg-path)
                                        `(:realpath
                                          ,(realpath (string-join (map (lambda (x) (format #f "~A" x))
                                                                       (list  ws-path fg-path)) "/")
                                                     '()))
                                        (cons :filegroups (cdr kv)))))
                  ;; (for-each (lambda (fg)
                  ;;             (format #t "~A: ~A~%" (yellow "fg") fg))
                  ;;           (cdr kv))
                  )
                )
              filegroups)))
    ;;      (pkgs (car (assoc-val :pkgs @ws)))
    ;;      (mpkg-alist (map (lambda (kv)
    ;;                        (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv))))
    ;;                          (hash-table-set! pkgs (car kv) mibl-pkg)
    ;;                          mibl-pkg))
    ;;                      pkgs)))
    ;;     ;; (_ (format #t "~A: ~A~%" (blue "mpkg-alist")
    ;;     ;;            mpkg-alist))
    ;;     ;; (_ (for-each (lambda (k)
    ;;     ;;                (format #t "~A: ~A~%" (blue "pkg") k))
    ;;     ;;              (sort! (hash-table-keys pkgs) string<?)))
    ;; (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist))
    ;; mpkg-alist))


(define (update-tagged-label-list! filename tllist pkg)
  (format #t "~A: ~A ~A~%" (ublue "update-tagged-label-list!")
          filename tllist)
  (let* ((fname (format #f "~A" filename))
         (key (string->keyword fname)))
    (format #t "~A: ~A~%" (uwhite "tllist before") tllist)
    (set-cdr! tllist
              (cons (cons key
                          (list (cons :pkg (car (assoc-val :pkg-path pkg)))
                                (cons :tgt fname)))
                    (cdr tllist)))
    (format #t "~A: ~A~%" (uwhite "tllist after") tllist)
    key))

(define (update-pkg-files-with-struct! pkg tgt)
  (format #t "~A: ~A~%" (ublue "update-pkg-files-with-struct!") tgt)
  (format #t "~A: ~A~%" (blue "pkg") pkg)
  (flush-output-port)
  ;; if we already have corresponding sig, move to :modules
  ;; else update :structures
  (let* ((m-assoc (filename->module-assoc tgt))
         ;; m-assoc == (A (:ml "a.ml"))
         (m-name (car m-assoc))
         (pr (cadr m-assoc))
         (_ (format #t "~A: ~A\n" (red "PR") pr))
         ;; (sigs (assoc-val :signatures pkg))
         ;; (_ (format #t "~A: ~A~%" (cyan "sigs2") sigs))
         (sigs (assoc :signatures pkg))
         (_ (format #t "~A: ~A~%" (cyan "sigs") sigs))
         ;; removes matching sig from :signatures
         (matching-sig (if sigs
                           (if (cdr sigs)
                               (find-module-in-rsrc-list!? m-name tgt sigs)
                               #f)
                           #f))
         (_ (format #t "~A: ~A~%" (cyan "found sig?") matching-sig))
         )

    ;; FIXME: check to see if already in pkg-modules
    (if matching-sig
        (begin ;; we know this file is not in :modules since it was in sigs
          (format #t "~A: ~A~%" (red "updating :modules") m-name)
          (alist-update-in! pkg `(:modules ,m-name)
                            (lambda (old)
                              (format #t "struct Module OLD: ~A\n" old)
                              (format #t "adding: ~A\n" matching-sig)
                              (format #t " pr: ~A\n" pr)
                              (if (null? old)
                                  (cons pr
                                        (list (cons :mli (cdr matching-sig))))
                                  ;;(filename->module-assoc tgt)
                                  (list
                                   old
                                   (cons pr (list (cons :mli (cdr matching-sig))))
                                   ;; (list pr)
                                   ;;(filename->module-assoc tgt)
                                   )))))

        ;; else no sig, so update :structures
        (let* ((_ (format #t "~A: ~A~%" (yellow "updating :structures")
                          m-name))
               (dynamics (assoc-in '(:structures :dynamic) pkg))
               )
          (format #t "~A: ~A~%" (white "(:structures :dynamic) : ") dynamics)
          (alist-update-in! pkg `(:structures :dynamic)
                                (lambda (old)
                                  (format #t "structures OLD: ~A\n" old)
                                  (format #t "adding: ~A\n" pr)
                                  (if (null? old)
                                      (list
                                       (cons m-name (cdr pr)))
                                      ;; (list m-name (cdr pr))
                                      ;;(filename->module-assoc tgt)
                                      (append
                                       old
                                       (list (cons m-name (cdr pr))) ;;)))
                                       ;; (list (cons m-name (cdr pr)))
                                       ;; structures
                                       ;;(filename->module-assoc tgt)
                                       ))))
          )))
  (format #t "~A: ~A~%" (bgblue "pkg w/updated structs") pkg)
  pkg)

(define (-update-pkg-files-with-sig pkg tgt)
  (format #t "~A: ~A~%" (yellow "-update-pkg-files-with-sig") tgt)
  ;; if we already have corresponding struct, move to :modules
  ;; else update :signatures
  (let* ((m-assoc (filename->module-assoc tgt))
         ;; m-assoc == (A (:ml "a.ml"))
         (m-name (car m-assoc))
         (pr (cadr m-assoc))
         (_ (format #t "~A: ~A\n" (red "PR") pr))
         (structs (assoc :structures pkg))
         (_ (format #t "~A: ~A~%" (cyan "structs") structs))
         ;; removes matching struct from :structnatures
         (matching-struct (find-module-in-rsrc-list!? m-name tgt structs))
         (_ (format #t "~A: ~A~%" (cyan "found struct?") matching-struct))
         )
    (if matching-struct
        (begin ;; we know this file is not in :modules since it was in structs
          (format #t "~A: ~A~%" (red "updating :modules") m-name)
          (alist-update-in! pkg `(:modules ,m-name)
                            (lambda (old)
                              (format #t "sig Module OLD: ~A\n" old)
                              (format #t "adding: ~A\n" matching-struct)
                              (format #t " pr: ~A\n" pr)
                              (if (null? old)
                                  (cons pr
                                        (list (cons :mli (cdr matching-struct))))
                                  ;;(filename->module-assoc tgt)
                                  (list
                                   old
                                   (cons pr (list (cons :mli (cdr matching-struct))))
                                   ;; (list pr)
                                   ;;(filename->module-assoc tgt)
                                   )))))
        ;; else no matching struct, so update :signatures
        (let* ((s-assoc (assoc-in '(:signatures :dynamic) pkg))
               ;; (structures (if s-assoc (append (cadr s-assoc) '(Foo . bar)) '()))
               )
          (format #t "~A: ~A~%" (yellow "UPDATING (:signatures :dynamic) : ") s-assoc)
          (alist-update-in! pkg `(:signatures :dynamic)
                            (lambda (old)
                              (format #t "signatures OLD: ~A\n" old)
                              (format #t "adding: ~A\n" pr)
                              (if (null? old)
                                  (list
                                   (cons m-name (cdr pr)))
                                  ;; (list m-name (cdr pr))
                                  ;;(filename->module-assoc tgt)
                                  (append
                                   old
                                   (list (cons m-name `,@(cdr pr))) ;;)))
                                   ;; (list (cons m-name (cdr pr)))
                                   ;; structures
                                   ;;(filename->module-assoc tgt)
                                   )))))))
  )

;; principal-fname: fileame w/o extension
;; updates :modules with generated files
(define (update-pkg-modules-with-module! pkg principal-fname)
  (format #t "~A: ~A~%" (ublue "update-pkg-modules-with-module") principal-fname)
  (let* ((pkg-modules (assoc :modules pkg))
         (_ (format #t "~A: ~A~%" (uyellow "pkg-modules") pkg-modules))
         (m-assoc (list (list (normalize-module-name principal-fname)
                              (cons :ml_ (string->symbol (format #f "~A.ml" principal-fname)))
                              (cons :mli_ (string->symbol (format #f "~A.mli" principal-fname)))))))
    (format #t "~A: ~A~%" (red "updating :modules") m-assoc)
    (if pkg-modules
        (set-cdr! pkg-modules (append (cdr pkg-modules) m-assoc))
        (alist-update-in! pkg `(:modules)
                          (lambda (old)
                            (format #t ":modules OLD: ~A\n" old)
                            (format #t "adding: ~A\n" m-assoc)
                            (if (null? old)
                                m-assoc
                                (append
                                 old m-assoc)))))))
        ;; (set! pkg (append pkg (cons :modules m-assoc))))))

(define (-partition-tgts tgts)
  (format #t "~A: ~A~%" (ublue "-partition-tgts") tgts)
  (let recur ((tgts tgts)
              (module-tgts '())
              (sig-tgts    '())
              (struct-tgts '())
              (file-tgts   '()))
    (if (null? tgts)
        (values module-tgts sig-tgts struct-tgts file-tgts)
        (let* ((tgt (car tgts))
               (principal (principal-name tgt))
               (ext (filename-extension tgt)))
          ;; (format #t "~A: ~A~%" (blue "principal") principal)
          ;; (format #t "~A: ~A (~A)~%" (blue "ext") ext (type-of ext))
          (if ext
              (cond
               ((string=? ext ".mli")
                (let ((ml (string->symbol
                           (string-append principal ".ml"))))
                  (if (member ml struct-tgts)
                      (recur (cdr tgts)
                             ;; (cons
                             ;;  (cons (normalize-module-name principal)
                             ;;        (list (cons :ml_ ml)
                             ;;              (cons :mli_ tgt)))
                             ;;  module-tgts)
                             (cons principal module-tgts)
                             sig-tgts
                             (remove ml struct-tgts) file-tgts)
                      (recur (cdr tgts)
                             module-tgts (cons tgt sig-tgts)
                             struct-tgts file-tgts)
                      )))
               ((string=? ext ".ml")
                (let ((mli (string->symbol
                            (string-append principal ".mli"))))
                  (if (member mli sig-tgts)
                      (recur (cdr tgts)
                             ;; (cons
                             ;;  (cons (normalize-module-name principal)
                             ;;        (list (cons :ml_ tgt)
                             ;;              (cons :mli_ mli)))
                             ;;  module-tgts)
                             (cons principal module-tgts)
                             sig-tgts struct-tgts file-tgts)
                      (recur (cdr tgts)
                             module-tgts sig-tgts
                             (cons tgt struct-tgts) file-tgts)
                      )))
                (else (recur (cdr tgts)
                                  module-tgts sig-tgts
                                  struct-tgts (cons tgt file-tgts))))
              (recur (cdr tgts)
                                  module-tgts sig-tgts
                                  struct-tgts (cons tgt file-tgts)))))))

(define update-pkg-files!
  (let ((+documentation+ "INTERNAL. Add tgts to :modules (or :files etc) fld of pkg."))
    (lambda (pkg tgts)
      (format #t "~%~A: ~A~%" (bgblue "update-pkg-files!") tgts)
      (format #t "  package: ~A\n" pkg)
      ;; (format #t "  targets: ~A\n" tgts)
      ;; tgts may contain ml/mli pairs, so
      ;; step 1: partition tgts into module-tgts, struct-tgts, sig-tgts
      ;; and other-tgts
      (let-values (((module-tgts sig-tgts struct-tgts file-tgts)
                    (-partition-tgts tgts)))
        (format #t "~A: ~A~%" (blue "module-tgts") module-tgts)
        (format #t "~A: ~A~%" (blue "sig-tgts")    sig-tgts)
        (format #t "~A: ~A~%" (blue "struct-tgts") struct-tgts)
        (format #t "~A: ~A~%" (blue "file-tgts")   file-tgts)

        (if (not (null? module-tgts))
            (for-each (lambda (tgt)
                        (update-pkg-modules-with-module! pkg tgt))
                        module-tgts))

        (if (not (null? sig-tgts))
            (for-each (lambda (tgt)
                        (update-pkg-files-with-sig! pkg tgt))
                      sig-tgts))

        (if (not (null? struct-tgts))
            (for-each (lambda (tgt)
                        (update-pkg-files-with-struct! pkg tgt))
                      struct-tgts))

        (if (not (null? file-tgts))
            (for-each (lambda (tgt)
                        (alist-update-in! pkg '(:files :dynamic)
                                          (lambda (old)
                                            ;; (format #t ":files :dynamic OLD: ~A\n" old)
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
                                              ))))
                      file-tgts))))))

      ;; ;; get the alists we might update
      ;; (let* (;;(pkg (car pkg*))
      ;;        (modules-assoc (assoc :modules pkg))
      ;;        ;; (modules (if modules-assoc (cdr modules-assoc) #f))
      ;;        ;; (scripts (if modules-assoc (assoc-val :scripts pkg) #f))
      ;;        ;; (data    (if modules-assoc (assoc-val :data pkg) #f))
      ;;        (files-assoc (if (assoc :files pkg)
      ;;                         (assoc-val :files pkg) #f)))
      ;;   ;; (format #t "~A: ~A~%" (cyan "modules-assoc") modules-assoc)
      ;;   ;; (format #t "~A: ~A~%" (cyan "data") data)
      ;;   ;; (format #t "~A: ~A~%" (cyan "scripts") scripts)
      ;;   ;; (format #t "~A: ~A~%" (cyan "files-assoc") files-assoc)
      ;;   ;; for each tgt, decide its kind: ml/mli, or other
      ;;   ;; then update the pkg fld: :modules, :scripts, :files, :data
      ;;   ;; since we're updating pkg use for-each
      ;;   (for-each
      ;;    (lambda (tgt)
      ;;      ;; (format #t "~A: ~A~%" (red "tgt") tgt)
      ;;      ;; (format #t "~A: ~A~%" (red "tgt str") (format #f "~A" tgt))
      ;;      (if (not (string-index (format #f "~A" tgt)
      ;;                             (lambda (ch) (equal? ch #\/))))
      ;;          (let ((kind (filename->kind (format #f "~A" tgt))))
      ;;            ;; (format #t "~A: ~A~%" (red "kind") kind)
      ;;            (case kind
      ;;              ((:struct)
      ;;               ;; (format #t ":struct tgt: ~A\n" tgt)
      ;;               (update-pkg-files-with-struct! pkg tgt))

      ;;              ((:sig)
      ;;               ;;(format #t ":sig tgt: ~A\n" tgt)
      ;;               (-update-pkg-files-with-sig pkg tgt))

      ;;              (else
      ;;               ;; (format #t ":other: ~A\n" tgt)
      ;;               ;; (format #t "files-assoc: ~A\n" files-assoc)
      ;;               (alist-update-in! pkg '(:files :dynamic)
      ;;                                 (lambda (old)
      ;;                                   ;; (format #t ":files :dynamic OLD: ~A\n" old)
      ;;                                   ;; (format #t "other tgt: ~A\n" tgt)
      ;;                                   (let ((fa (filename->file-assoc tgt))
      ;;                                         (tgtstr (if (symbol? tgt)
      ;;                                                     (symbol->string tgt)
      ;;                                                     tgt)))
      ;;                                     ;; (format #t "fa: ~A\n" fa)
      ;;                                     ;; (format #t "fa2: ~A\n"old)
      ;;                                     (if (null? old)
      ;;                                         (list tgtstr)
      ;;                                         (append old (list tgtstr))
      ;;                                         ;; (if (pair? fa)
      ;;                                         ;;     (cons fa old)
      ;;                                         ;;     (append fa old))
      ;;                                         )
      ;;                                     )))
      ;;               )) ;; case
      ;;            )
      ;;          ;; else '/' found in tgt
      ;;          )) ;; lambda
      ;;    tgts) ;; for-each
      ;;   ;; (format #t "pkg (after): ~A\n" pkg)
      ;;   pkg))))

;; %{bin:foo} etc. Dune uses those prefixes to reference installation
;; locations. Since we do not do any installation, they're just labels
;; to us. E.g. when we process an executable with public name foo, we
;; add 'bin:foo' to the exports table. Any target that uses it (in a
;; rule action for example) will refer to it as 'bin:foo', so we can
;; just look it up to find its Bazel label.

(define (update-exports-table! ws pfx nm pkg-path tgt)
  (format #t "~A: ~A , ~A\n" (ublue "update-exports-table!") pfx nm)
  (format #t "~A: ~A , ~A~%" (uwhite "spec") pkg-path tgt)
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws -mibl-ws-table))))
         (key (case pfx
                ((:exe) (symbol (format #f "bin:~A.exe" nm)))
                ((:bin) (symbol (format #f "bin:~A.exe" nm)))
                ((:lib) (symbol (format #f "lib:~A" nm)))
                ((:libexec) (symbol (format #f "libexec:~A" nm)))
                ((:test) (string->keyword (format #f "~A.exe" nm)))
                ((#f) (string->symbol (format #f "~A" nm)))
                (else nm)))
         (exe (case pfx
                ((:bin :exe) #t)
                (else #f)))
         (pfx-assoc (case pfx
                ((:bin :exe :lib :libexec :test) (list (cons pfx #t)))
                (else '())))
         (spec `(,@pfx-assoc
                 ,(cons :pkg pkg-path)
                 ,(cons :tgt (if exe
                                 (format #f "~A.exe" tgt)
                                 (format #f "~A" tgt))))))
    (format #t "exports tbl: ~A\n" exports)

    (format #t "~A ~A => ~A~%" (bgred "adding to exports") key spec)
    (if exe
        (begin
          (hash-table-set! exports tgt spec)
          (hash-table-set! exports (symbol->keyword tgt) spec)
          (hash-table-set! exports
                           (if (keyword? key) key
                               (symbol->keyword key))
                           spec)
          )
        (begin
          (hash-table-set! exports ;; key
                           (if (keyword? key) key
                               (symbol->keyword key))
                           spec)))
    (if (eq? pfx :test)
        (let ((key (string->symbol
                    (format #f ":exe~A" key))))
          (hash-table-set! exports key
                           spec)))
    (format #t "~A: ~A\n" (uwhite "updated exports tbl") exports)
    ;; (error 'STOP "STOP exports")
    ))

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

                (update-exports-table! ws :FIXME tgt pkg)

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
