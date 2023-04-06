(if *mibl-debug-s7-loads*
    (format #t "loading dune/updaters.scm~%"))

(define (update-filegroups-table! ws client-path pkg-path tgt pattern)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (begin
        (format #t "~A path: ~A~%" (umagenta "update-filegroups-table!") pkg-path)
        (format #t "~A: ~A~%" (green "client") client-path)
        (format #t "~A: ~A~%" (green "tgt") tgt)
        (format #t "~A: ~A~%" (green "pattern") pattern)
        (format #t "~A: ~A~%" (green "ws") ws)
        ;; (format #t "~A: ~A~%" (green "mibl tbl") *mibl-project*)
        ))

  (let* ((-ws (if (keyword? ws) (assoc-val ws *mibl-project*) ws))
         ;; (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (uwhite "-ws") -ws)))
         (filegroups (car (assoc-val :filegroups -ws)))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "filegroups tbl: ~A\n" filegroups)))
         (patt-str (format #f "~A" pattern))
         (glob? (string-index patt-str (lambda (ch) (equal? ch #\*))))
         (fg-name (format #f "glob_~A" (string-replace patt-str "STAR" glob? (+ 1 glob?))))
         )
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (begin
          (format #t "~A: ~A~%" (bgcyan "fg-name") fg-name)
          (format #t "adding ~A~A to filegroups tbl\n" pkg-path tgt)))

    (let ((fgroups (hash-table-ref filegroups pkg-path)))
      (if (or *mibl-debug-s7* *mibl-debug-updaters*)
          (format #t "~A: ~A~%" (red "fgroups") fgroups))
      (if fgroups
          (if-let ((same (assoc fg-name fgroups)))
                  (begin
                    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                        (format #t "~A: ~A~%" (bgred "same") same))
                    (alist-update-in! fgroups (list fg-name :clients)
                                      (lambda (old) (append old (list client-path))))
                    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                        (format #t "~A: ~A~%" (bgblue "updated") filegroups))
                    ;; (error 'STOP "update-fgs")
                    ;; (hash-table-set! filegroups pkg-path
                    ;;                  (append
                    ;;                   fgroups
                    ;;                   (list (cons fg-name
                    ;;                               ;;tgt
                    ;;                               (if glob?
                    ;;                                   `((:glob . ,pattern)
                    ;;                                     (:clients ,client-path))
                    ;;                                   `((:file . ,pattern)))))))
                    )
                  ;; else new entry in existing fg list
                  (hash-table-set! filegroups pkg-path
                                   (append
                                    fgroups
                                    (list (cons fg-name
                                                ;;tgt
                                                (if glob?
                                                    `((:pkg ,pkg-path) ;; redundant, == ht key
                                                      (:glob . ,pattern)
                                                      (:clients ,client-path))
                                                    `((:file . ,pattern))))))))
          ;; else new fg list for pkg
          (hash-table-set! filegroups pkg-path
                           (list (cons fg-name
                                       ;; tgt
                                       (if glob?
                                               `((:pkg ,pkg-path)
                                                 (:glob . ,pattern)
                                                 (:clients ,client-path))
                                               `((:file . ,pattern)))))))
      (if (or *mibl-debug-s7* *mibl-debug-updaters*)
          (format #t "updated filegroups tbl: ~A\n" filegroups))
      )))

;; normalize-rule-deps: in some rules the same resource may end up as both tool and dep, e.g. test.exe in yojson/test/pretty:
;; (rule
;;  (targets test.output)
;;  (deps ./sample.json ./test.exe)
;;  (action
;;   (with-stdout-to
;;    %{targets}
;;    (run ./test.exe))))

;; tasks: remove tool from deps list; fix ::unresolved cmd args
(define (normalize-rule-deps! ws)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (bgblue "normalize-rule-deps!") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))
    (for-each (lambda (kv)
                (let* ((pkg-key (car kv))
                       (pkg (cdr kv))
                       (stanzas (assoc-val :mibl (cdr kv))))
                  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                      (format #t "~A: ~A~%" (bgcyan "pkg") pkg))

                  (if stanzas
                      (for-each (lambda (stanza)
                                  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                      (format #t "~A: ~A~%" (green "stanza") stanza))
                                  (case (car stanza)
                                    ((:rule)
                                     (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                         (format #t "~A: ~A~%" (ured "rule stanza") stanza))
                                     (if (assoc-in '(:deps ::tools) (cdr stanza))
                                         (let* ((stanza-alist (cdr stanza))
                                                (all-deps (assoc-val ':deps stanza-alist))
                                                (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (green "all deps") all-deps)))
                                                (tools (assoc-val '::tools all-deps))
                                                (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (green "tools") tools)))
                                                (deps (cdr all-deps))
                                                (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (green "deps") deps))))
                                           (if tools
                                               (for-each (lambda (tool)
                                                           (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                                               (format #t "~A: ~A~%" (red "tool key") (car tool)))
                                                           ;;FIXME? we expect the entire list to match, e.g.
                                                           ;; (:./test.exe (:pkg . "test/pretty") (:tgt . "test.exe"))
                                                           ;; but what if only the key matches?
                                                           (if (member tool deps)
                                                               (let ((filtered-deps (dissoc (list (car tool)) deps)))
                                                                 (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                                                     (format #t "~A: ~A~%" (green "filtered") filtered-deps))
                                                                 (set-cdr! all-deps filtered-deps))
                                                               (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                                                   (format #t "~A: ~A~%" (bgred "MISS") tool)))
                                                           )
                                                         tools))
                                           (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                               (format #t "~A: ~A~%" (bgred "updated stanza") stanza)
                                               (values))))
                                     ;;FIXME: can there be more than one :actions per :rule?
                                     (if (assoc-in '(:actions :cmd :args) (cdr stanza))
                                         (if *mibl-debug-s7*
                                             (begin
                                               ;;FIXME: write to .mibl/mibl.log
                                               (format #t "FIXME: normalize cmd args\n")
                                               (debug-print-stacktrace))))
                                     )
                                    (else)))
                                stanzas))))
              pkgs)
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (bgblue "//normalize-rule-deps!") ws))
    ))

  ;; (let* ((@ws (assoc-val ws *mibl-project*))
  ;;        (ws-path (car (assoc-val :path @ws)))
  ;;        (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (blue "ws-path") ws-path))
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
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (ublue "add-filegroups-to-pkgs") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (ws-path (assoc-val :path @ws))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (blue "ws-path") ws-path)))
         (pkgs (car (assoc-val :pkgs @ws)))
         (filegroups (car (assoc-val :filegroups @ws))))
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (yellow "fg-table") filegroups))
    (for-each (lambda (kv)
                (let* ((fg-path (car kv))
                       (fg-key (car kv))
                       (fg-pkg (hash-table-ref pkgs fg-key)))
                  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                      (begin
                        (format #t "\n~A: ~A~%" (yellow "fg-path") fg-path)
                        (format #t "~A: ~A~%" (yellow "fg-key") fg-key)
                        (format #t "~A: ~A~%" (yellow "pkg") fg-pkg)))
                  ;; WARNING: fg-pkg will be #f if it is not in scope
                  ;; e.g. its a globbed super-dir
                  (if fg-pkg
                      (hash-table-set! pkgs fg-key
                                       (append fg-pkg
                                               (list
                                                (cons :filegroups (cdr kv)))))
                      (hash-table-set! pkgs fg-key
                                       (list
                                        `(:ws-path . ,ws-path)
                                        `(:pkg-path . ,fg-path)
                                        `(:realpath .
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
    ;;     ;; (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (blue "mpkg-alist")
    ;;     ;;            mpkg-alist)))
    ;;     ;; (_ (for-each (lambda (k)
    ;;     ;;                (format #t "~A: ~A~%" (blue "pkg") k))
    ;;     ;;              (sort! (hash-table-keys pkgs) string<?)))
    ;; (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist))
    ;; mpkg-alist))


(define (update-tagged-label-list! filename tllist pkg)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A ~A~%" (ublue "update-tagged-label-list!")
          filename tllist))
  (let* ((fname (format #f "~A" filename))
         (key (string->keyword fname)))
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (uwhite "tllist before") tllist))
    (set-cdr! tllist
              (cons (cons key
                          (list (cons :pkg (assoc-val :pkg-path pkg))
                                (cons :tgt fname)))
                    (cdr tllist)))
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (uwhite "tllist after") tllist))
    key))

(define (update-pkg-files-with-struct! pkg tgt)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (ublue "update-pkg-files-with-struct!") tgt))
  ;; (format #t "~A: ~A~%" (blue "pkg") pkg)
  (flush-output-port)
  ;; if we already have corresponding sig, move to :modules
  ;; else update :structures
  (let* ((m-assoc (filename->module-assoc tgt))
         ;; m-assoc == (A (:ml "a.ml"))
         (m-name (car m-assoc))
         (pr (cadr m-assoc))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A\n" (red "PR") pr)))
         ;; (sigs (assoc-val :signatures pkg))
         ;; (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (cyan "sigs2") sigs)))
         (sigs (assoc :signatures pkg))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (cyan "sigs") sigs)))
         ;; removes matching sig from :signatures
         (matching-sig (if sigs
                           (if (cdr sigs)
                               (find-module-in-rsrc-list!? m-name tgt sigs)
                               #f)
                           #f))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (cyan "found sig?") matching-sig)))
         )

    ;; FIXME: check to see if already in pkg-modules
    (if matching-sig
        (begin ;; we know this file is not in :modules since it was in sigs
          (if (or *mibl-debug-s7* *mibl-debug-updaters*)
              (format #t "~A: ~A~%" (red "updating :modules") m-name))
          (alist-update-in! pkg `(:modules ,m-name)
                            (lambda (old)
                              (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                  (begin
                                    (format #t "struct Module OLD: ~A\n" old)
                                    (format #t "adding: ~A\n" matching-sig)
                                    (format #t " pr: ~A\n" pr)))
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
        (let* ((_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (yellow "updating :structures")
                          m-name)))
               (dynamics (assoc-in '(:structures :dynamic) pkg))
               )
          (if (or *mibl-debug-s7* *mibl-debug-updaters*)
              (format #t "~A: ~A~%" (white "(:structures :dynamic) : ") dynamics))
          (alist-update-in! pkg `(:structures :dynamic)
                                (lambda (old)
                                  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                      (begin
                                        (format #t "structures OLD: ~A\n" old)
                                        (format #t "adding: ~A\n" pr)))
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
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (bgblue "pkg w/updated structs") pkg))
  pkg)

(define (-update-pkg-files-with-sig! pkg tgt)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (begin
        (format #t "~A: ~A~%" (ublue "-update-pkg-files-with-sig!") tgt)
        (format #t "~A: ~A~%" (ublue "Pkg") pkg)))

  ;; if we already have corresponding struct, move to :modules
  ;; else update :signatures
  (let* ((m-assoc (filename->module-assoc tgt))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                (format #t "~A: ~A\n" (red "m-assoc") m-assoc)))
         ;; m-assoc == (A (:ml "a.ml"))
         (m-name (car m-assoc))
         (pr (cadr m-assoc))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A\n" (red "PR") pr)))
         (structs (assoc :structures pkg))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (cyan "structs") structs)))
         ;; removes matching struct from :structnatures
         (matching-struct (find-module-in-rsrc-list!? m-name tgt structs))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (cyan "found struct?") matching-struct)))
         )
    (if matching-struct
        (begin ;; we know this file is not in :modules since it was in structs
          (if (or *mibl-debug-s7* *mibl-debug-updaters*)
              (format #t "~A: ~A~%" (red "updating :modules") m-name))
          (alist-update-in! pkg `(:modules ,m-name)
                            (lambda (old)
                              (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                  (begin
                                    (format #t "sig Module OLD: ~A\n" old)
                                    (format #t "adding: ~A\n" matching-struct)
                                    (format #t " pr: ~A\n" pr)))
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
          (if (or *mibl-debug-s7* *mibl-debug-updaters*)
              (format #t "~A: ~A~%" (yellow "UPDATING (:signatures :dynamic) : ") s-assoc))
          (alist-update-in! pkg `(:signatures :dynamic)
                            (lambda (old)
                              (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                  (begin
                                    (format #t "signatures OLD: ~A\n" old)
                                    (format #t "m-name: ~A\n" m-name)
                                    (format #t "adding: ~A\n" pr)))
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
                                   )))))))
  )

;; principal-fname: fileame w/o extension
;; updates :modules with generated files
(define (update-pkg-modules-with-module! pkg principal-fname)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (ublue "update-pkg-modules-with-module") principal-fname))
  (let* ((pkg-modules (assoc :modules pkg))
         (_ (if (or *mibl-debug-s7* *mibl-debug-updaters*) (format #t "~A: ~A~%" (uyellow "pkg-modules") pkg-modules)))
         (m-assoc (list (list (normalize-module-name principal-fname)
                              (cons :ml_ (string->symbol (format #f "~A.ml" principal-fname)))
                              (cons :mli_ (string->symbol (format #f "~A.mli" principal-fname)))))))
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (red "updating :modules") m-assoc))
    (if pkg-modules
        (set-cdr! pkg-modules (append (cdr pkg-modules) m-assoc))
        (alist-update-in! pkg `(:modules)
                          (lambda (old)
                            (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                (begin
                                  (format #t ":modules OLD: ~A\n" old)
                                  (format #t "adding: ~A\n" m-assoc)))
                            (if (null? old)
                                m-assoc
                                (append
                                 old m-assoc)))))))
        ;; (set! pkg (append pkg (cons :modules m-assoc))))))

(define (-partition-tgts tgts)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (ublue "-partition-tgts") tgts))
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
      (if (or *mibl-debug-s7* *mibl-debug-updaters*)
          (begin
            (format #t "~%~A: ~A~%" (bgblue "update-pkg-files!") tgts)
            (format #t "  package: ~A\n" (assoc-val :pkg-path pkg))))
      ;; (format #t "  targets: ~A\n" tgts)
      ;; tgts may contain ml/mli pairs, so
      ;; step 1: partition tgts into module-tgts, struct-tgts, sig-tgts
      ;; and other-tgts
      (let-values (((module-tgts sig-tgts struct-tgts file-tgts)
                    (-partition-tgts tgts)))
        ;; (format #t "~A: ~A~%" (blue "module-tgts") module-tgts)
        ;; (format #t "~A: ~A~%" (blue "sig-tgts")    sig-tgts)
        ;; (format #t "~A: ~A~%" (blue "struct-tgts") struct-tgts)
        ;; (format #t "~A: ~A~%" (blue "file-tgts")   file-tgts)

        (if (not (null? module-tgts))
            (for-each (lambda (tgt)
                        (update-pkg-modules-with-module! pkg tgt))
                        module-tgts))

        (if (not (null? sig-tgts))
            (for-each (lambda (tgt)
                        (-update-pkg-files-with-sig! pkg tgt))
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
      ;;               (-update-pkg-files-with-sig! pkg tgt))

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

;; executables: emit keys .bc and .exe, tgt always .exe
;; javascript: emit 2, one with sfx .js, one without

(define (update-exports-table! ws pfx modes nm pkg-path tgt)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (begin
        (format #t "~A\n" (bgblue "update-exports-table!"))
        (format #t "~A: ~A\n" (blue "pfx") pfx)
        (format #t "~A: ~A\n" (blue "modes") modes)
        (format #t "~A: ~A\n" (blue "nm") nm)
        (format #t "~A: ~A (type: ~A)\n" (blue "pkg-path") pkg-path (type-of pkg-path))
        (format #t "~A: ~A\n" (blue "tgt") tgt)
        ;; (debug-print-stacktrace)
        ))
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws *mibl-project*))))
         (key (case pfx
                ((:exe) (symbol (format #f "bin:~A.exe" nm)))
                ((:bin) (symbol (format #f "bin:~A.exe" nm)))
                ((:lib) (symbol (format #f "lib:~A" nm)))
                ((:libexec) (symbol (format #f "libexec:~A" nm)))
                ((:test) (string->keyword (format #f "~A.exe" nm)))
                ;; ((#f) (string->symbol (format #f "~A" nm)))
                (else nm)))
         (exe (case pfx
                ((:bin :exe) #t)
                (else #f)))
         (pfx-assoc (case pfx
                ((:bin :exe :lib :libexec :test) (list (cons pfx #t)))
                (else '())))
         (spec `(,@pfx-assoc
                 ,(cons :pkg (if (symbol? pkg-path)
                                 pkg-path
                                 (string->symbol pkg-path)))
                 ,(cons :tgt (string->symbol
                              (if exe
                                  (format #f "~A.exe" tgt)
                                  (format #f "~A" tgt)))))))
    ;; (format #t "exports tbl: ~A\n" exports)

    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A ~A => ~A~%" (red "adding to exports") key spec))
    (if exe
        (begin
          (hash-table-set! exports tgt spec)
          (hash-table-set! exports (symbol->keyword tgt) spec)
          (case pfx
            ((:exe :bin)
             (hash-table-set! exports
                              (string->keyword (format #f "bin:~A" tgt))
                              spec)

             (hash-table-set! exports
                              (string->symbol (format #f "~A.bc" tgt))
                              spec)
             (hash-table-set! exports
                              (string->keyword (format #f "~A.bc" tgt))
                              spec)
             (hash-table-set! exports
                              (string->keyword (format #f "bin:~A.bc" tgt))
                              spec)
             (if modes
                 (if (member 'js modes)
                     ;;FIXME: make this less hideous
                     (let ((spec `(,@pfx-assoc
                                   ,(cons :pkg (string->symbol pkg-path))
                                   ,(cons :tgt (string->symbol
                                                (if exe
                                                    (format #f "~A.bc.js" tgt)
                                                    (format #f "~A.js" tgt)))))))
                           (hash-table-set! exports
                                            (string->symbol (format #f "~A.bc.js" tgt))
                                            spec)
                           (hash-table-set! exports
                                            (string->keyword (format #f "~A.bc.js" tgt))
                                            spec)
                           (hash-table-set! exports
                                            (string->keyword (format #f "bin:~A.bc.js" tgt))
                                            spec))))

             (hash-table-set! exports
                              (string->symbol (format #f "~A.exe" tgt))
                              spec)
             (hash-table-set! exports
                              (string->keyword (format #f "~A.exe" tgt))
                              spec)
             (hash-table-set! exports
                              (string->keyword (format #f "bin:~A.exe" tgt))
                              spec))
            ;; ((:bin)
            ;;  (hash-table-set! exports
            ;;                   (string->symbol (format #f "~A.bc" tgt))
            ;;                   spec)
            ;;  (hash-table-set! exports
            ;;                   (string->keyword (format #f "bin:~A.bc" tgt))
            ;;                   spec))
            )
          (hash-table-set! exports
                           (if (keyword? key) key
                               (symbol->keyword key))
                           spec)
          )
        ;; exe false
        (begin
          (if (keyword? key)
              (hash-table-set! exports key spec)
              ;;
              ;; both with and without leading ':', just to be sure
              (begin
                (hash-table-set! exports key spec)
                (hash-table-set! exports (symbol->keyword key) spec)))))

    (if (eq? pfx :test)
        (let ((key (string->symbol
                    (format #f ":exe~A" key))))
          (hash-table-set! exports key
                           spec)))
    ;; (format #t "~A: ~A\n" (uwhite "updated exports tbl") exports)
    ;; (error 'STOP "STOP exports")
    ))

(define (update-exports-table-with-targets! ws targets pkg-path)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (magenta "update-exports-table-with-targets!") targets))
  (if targets
      (for-each (lambda (target)
                  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                      (format #t "~A: ~A~%" (red "target") target))
                  (let* ((pkg-tgt (cdr target))
                         (pkg (assoc-val :pkg pkg-tgt))
                         (tgt (assoc-val :tgt pkg-tgt)))
                    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                        (begin
                          (format #t "~A: ~A~%" (magenta "pkg") pkg)
                          (format #t "~A: ~A~%" (magenta "tgt") tgt)))

                    (update-exports-table! ws :FIXME :FIXME-MODES tgt pkg)

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

(define (update-stanza-deps pkg fname mdeps)
  (if (or *mibl-debug-s7* *mibl-debug-updaters*)
      (begin
        (format #t "~A: ~A~%" (ublue "update-stanza-deps") (assoc-val :pkg-path pkg))
        (format #t "~A: ~A~%" (blue "pkg") pkg)
        (format #t "~A: ~A~%" (blue "fname") fname)
        (format #t "~A: ~A~%" (blue "mdeps") mdeps)))
  (let ((mname (filename->module-name fname)))
    (if (or *mibl-debug-s7* *mibl-debug-updaters*)
        (format #t "~A: ~A~%" (blue "mname") mname))
    (for-each (lambda (stanza)
                (case (car stanza)
                  ((:exports-files))
                  (else
                   (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                       (format #t "~A: ~A~%" (ucyan "stanza") stanza))
                   (let ((compile-deps (assoc-in '(:compile :manifest :modules) (cdr stanza))))
                     (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                         (format #t "~A: ~A~%" (cyan "compile-deps (before)") compile-deps))
                     (if compile-deps
                         (if (member mname (cdr compile-deps))
                             (begin
                               (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                   (format #t "~A ~A to :compile :manifest ~A~%" (bgcyan "adding") mdeps compile-deps))
                               (set-cdr! compile-deps
                                         (remove-duplicates ;; don't add if its already there
                                          (append (cdr compile-deps)
                                                  mdeps)))
                               (if (or *mibl-debug-s7* *mibl-debug-updaters*)
                                   (format #t "~A: ~A~%" (cyan "compile-deps (after)") compile-deps))))))
                         )))
                (assoc-val :mibl pkg))))

(if *mibl-debug-s7-loads*
    (format #t "loaded dune/updaters.scm~%"))
