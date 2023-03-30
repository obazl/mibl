;; original version, forces shared prologues.
(define XXXupdate-pkg-prologues!
  (let ((+documentation+ "Update stanza :prologue and pkg :prologues")
        (+signature+ '(update-prologues! prologue pkg stanza-alist)))
    (lambda (prologue pkg stanza-alist)
      (if-let ((pkg-prologues (assoc :prologues (cdr pkg))))
              (begin
                (if *mibl-debug-s7*
                    (format #t "~A: ~A~%" (ublue "found pkg-prologues") pkg-prologues))
                (if (number? (cdr prologue))
                    (begin) ;; won't happen?
                    (if-let ((x (member (cdr prologue) (cdr pkg-prologues)
                                        (lambda (a b)
                                          (if *mibl-debug-s7*
                                              (format #t "~A: a: ~A, b: ~A~%" (red "comparing") a b))
                                          (equal? a (assoc-val :modules (cdr b)))))))
                            (begin
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (ublue "match") x))
                              (set-cdr! prologue (caar x))
                              )
                            ;; else update pkg-prologues
                            (let* ((ct (+ 1 (length (cdr pkg-prologues))))
                                   (opts (if-let ((opts (assoc-val :opts stanza-alist)))
                                                 `((:opts ,@opts)) '()))
                                   (link-opts (if-let ((opts (assoc-val :link-opts stanza-alist)))
                                                      `((:link-opts ,@opts)) '()))
                                   (ocamlc-opts (if-let ((opts (assoc-val :ocamlc-opts stanza-alist)))
                                                        `((:ocamlc-opts ,@opts)) '()))
                                   (ocamlopt-opts (if-let ((opts (assoc-val :ocamlopt-opts stanza-alist)))
                                                          `((:ocamlopt-opts ,@opts)) '()))
                                   (modules (sort! (cdr prologue) sym<?))
                                   (new (list (cons ct
                                                    `((:modules ,@modules)
                                                      ,@opts ,@link-opts ,@ocamlc-opts ,@ocamlopt-opts)))))
                              (if *mibl-debug-s7*
                                  (begin
                                    (format #t "~A: ~A~%" (ublue "mismatch; adding") prologue)
                                    (format #t "~A: ~A~%" (ublue "ct") ct)
                                    (format #t "~A: ~A~%" (ublue "new") new)))
                              (set-cdr! pkg-prologues (append (cdr pkg-prologues) new ))
                              (set-cdr! prologue ct)
                              ))))
              ;;FIXME: prologues must have all the props of the exe from which they are derived:
              ;; deps, opts, link-opts, etc.
              (let* ((opts (if-let ((opts (assoc-val :opts stanza-alist)))
                                   `((:opts ,@opts)) '()))
                     (link-opts (if-let ((lopts (assoc-val :link-opts stanza-alist)))
                                        `((:link-opts ,@lopts)) '()))
                     (ocamlc-opts (if-let ((opts (assoc-val :ocamlc-opts stanza-alist)))
                                          `((:ocamlc-opts ,@opts)) '()))
                     (ocamlopt-opts (if-let ((opts (assoc-val :ocamlopt-opts stanza-alist)))
                                            `((:ocamlopt-opts ,@opts)) '()))
                     (modules (sort! (cdr prologue) sym<?))
                     (new `((:prologues (1 (:modules ,@modules)
                                           ,@opts ,@link-opts ,@ocamlc-opts ,@ocamlopt-opts)))))
                (if *mibl-debug-s7*
                    (format #t "~A: ~A~%" (ublue "adding :prologues to stanzas") prologue))
                (set-cdr! pkg (append (cdr pkg) new))
                (set-cdr! prologue 1))
              ))))


(define (shared-prologue->ns prologues module)
  (if (or *mibl-debug-prologues* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (ublue "shared-prologue->ns for module") module))
  (let ((plog (find-if (lambda (prologue)
                         (format #t "testing plog: ~A\n" prologue)
                         (let ((p-modules (assoc-val :modules (cdr prologue))))
                           (member (car module) p-modules)))
                       (cdr prologues))))
    (if plog
        (let* ((serial (car plog))
               (_ (format #t "plog serial ~A\n" serial))
               (exe (find-then (lambda (stanza)
                                 (case (car stanza)
                                   ((:executable)
                                    (format #t "checking exe stanza ~A\n" stanza)
                                    (let ((prologue-id (assoc-val :prologue (cdr stanza))))
                                      (if (equal? prologue-id serial)
                                          (assoc-val :main (cdr stanza))
                                          #f)))
                                   (else #f)))
                               (assoc-val :mibl pkg))))
          (format #t "exe: ~A\n" exe)
          (if exe
              (format #f "~S_ns" exe))))))

;; module is from :modules or :structures
;; search pkg :prologues for it;
;; if found, find executable for prologue
;; derive ns from executable
(define (derive-exe-ns pkg module)
  (if (or *mibl-debug-prologues* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (ublue "-derive-exe-ns for module") module))
  (if (alist? (cdr module))
      ;; (A (:ml a.ml) (:mli a.mli) (:ml-deps...) (:mli-deps...))
      (begin)
      ;; else (A . a.ml) from :structures
      (if-let ((prologues (assoc-in '(:mibl :shared-prologues) pkg)))
              (shared-prologue->ns prologues module)
              (begin
                (format #t "nonshared prologue\n")
                ;; this module is from :structures
                ;; task: search for it in executables' :prologue
                ;; else in :main deps
                "Foo" ;; (error 'x "X")
                ))))

(define (shared-prologue-contains-module? prologue-key modname pkg)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A ~A\n"
                (ublue "prologue-contains-module?") prologue-key modname)
        (format #t "~A: ~A\n" (yellow "pkg") pkg)))
  (if-let ((prologues (assoc-in '(:mibl :prologues) pkg)))
          (let* ((prologues (cdr prologues))
                 (the-prologue (assoc-val prologue-key prologues))
                 (modules (assoc-val :modules the-prologue))
                 )
            ;; (format #t "~A: ~A\n" (yellow "prologues") prologues)
            ;; (format #t "~A: ~A\n" (yellow "the-prologue") the-prologue)
            ;; (format #t "~A: ~A\n" (yellow "modules") modules)
            (if (member modname modules)
                #t #f))
          ;; should not happen:
          (error 'Missing-prologues "Pkg is missing a :prologues list")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FIXME: obsolete
(define (-normalize-main-deps prologue  main-deps)
  (if (or *mibl-debug-s7* *mibl-debug-prologues*)
      (begin
        (format #t "~A: ~A~%" (bgblue "-normalize-main-deps") main-deps)
        (format #t "~A: ~A~%" (bgblue "prologue") prologue)))
  ;; (fold kons knil lis1 . lists)
  (let ((result (fold (lambda (dep accum)
                        (format #t "folding ~A\n" dep)
                        ;; (if (member dep main-deps)
                        (cons (format #f "X_~A" dep) accum))
                      '() prologue)))
    (format #t "FOLD result: ~A\n" result)
    (error 'xx "XX")))
  ;; (prolog (remove-if list (lambda (x)
  ;;                           ;; (format #t "B plog before: ~A\n" prolog)
  ;;                           ;; (format #t "x: ~A\n" x)
  ;;                           (member x main-deps))
  ;;                    prolog)))

;; normalize-prologues!
;; An executable has a :main and may have a :prologue.
;; This routine makes them mutually independent by
;; removing any deps of :main from the :prologue.
;; E.g. parsetree:
;; (:structures (:static (A a.ml)))
;; (:modules (Main (:ml main.ml A) (:mli main.mli)))
;; (:mibl (:executable (:privname . main) (:pubname . main) (:main . Main)
;;                     (:prologue . 1))
;;        (:prologues (1 (:modules A))))
;; In this case the prologue should be removed since A is in the depgraph of Main.

(define normalize-prologues!
  (let ((+documentation+ "Remove modules in :main's depgraph from prologue.")
        (+signature+ '(normalize-prologues!)))
    (lambda ()
      (if (or *mibl-debug-s7* *mibl-debug-prologues*)
          (format #t "~A~%" (bgblue "normalize-prologues!")))
      (flush-output-port)
      (for-each (lambda (ws-kv)
                  ;; (format #t "ws key: ~A\n" (car ws-kv))
                  (let ((pkgs (car (assoc-val :pkgs (cdr ws-kv)))))
                    (for-each (lambda (pkg-kv)
                                (let* ((stanzas (assoc-val :mibl (cdr pkg-kv)))
                                       (_ (format #t "pkg: ~A\n" (car pkg-kv)))
                                       ;; collect all executable :main modules
                                       (mains (filter-map (lambda (stanza)
                                                            (case (car stanza)
                                                              ((:executable)
                                                               (format #t "~A: ~A\n" (green "stanza") stanza)
                                                               (assoc-val :main (cdr stanza)))
                                                              (else #f)))
                                                          stanzas)))
                                  (if *mibl-debug-prologues* (format #t "~A: ~A\n" (green "mains") mains))
                                  (for-each (lambda (stanza)
                                              (format #t "  stanza: ~A\n" stanza)
                                              (case (car stanza)
                                                ((:executable)
                                                 (if *mibl-debug-prologues*
                                                     (format #t "~A: ~A\n" (green "executable") (assoc-val :main (cdr stanza))))
                                                 (let ((stanza-alist (cdr stanza)))
                                                   (if-let ((prologue (assoc :prologue stanza-alist)))
                                                           (begin
                                                             (if *mibl-debug-prologues*
                                                                 (format #t "~A: ~A\n" (green "  prologue") prologue))
                                                             (let ((prolog (remove-if list (lambda (x) (member x mains))
                                                                                        (cdr prologue))))
                                                               ;; (format #t "A plog after: ~A\n" prolog)
                                                               (let ((main (assoc-val :main stanza-alist)))
                                                                 (let-values (((ml-deps mli-deps) (module->local-deps main (cdr pkg-kv))))
                                                                   ;; (format #t "  ml-deps: ~A\n" ml-deps)
                                                                   ;; (format #t "  mli-deps: ~A\n" mli-deps)
                                                                   (if (or (truthy? ml-deps) (truthy? mli-deps))
                                                                       ;; move :main deps from :prologue to :deps
                                                                       (let* ((main-deps (concatenate ml-deps mli-deps))
                                                                              (_ (format #t "main-deps: ~A\n" main-deps))
                                                                              (prolog (remove-if list (lambda (x)
                                                                                                       ;; (format #t "B plog before: ~A\n" prolog)
                                                                                                       ;; (format #t "x: ~A\n" x)
                                                                                                       (member x main-deps))
                                                                                                prolog)))
                                                                         (format #t "B main-deps after: ~A\n" main-deps)
                                                                         (format #t "B plog after: ~A\n" prolog)

                                                                         ;; to limit the update to this stanza we
                                                                         ;; must first dissoc
                                                                         (dissoc! '(:prologue) stanza-alist)
                                                                         ;; (alist-update-in! (cdr stanza) '(:prologue)
                                                                         ;;                   (lambda (old)
                                                                         ;;                     (format #t "replacing ~A\n" old)
                                                                         ;;                     prolog))

                                                                         (set-cdr! stanza
                                                                                   (append stanza-alist
                                                                                           `((:prologue ,@prolog)
                                                                                           ;;FIXME :deps, :manifest, or :main-deps?
                                                                                             (:main-deps ,@main-deps)))))
                                                                       ;; else no main-deps
                                                                       (set-cdr! prologue prolog)
                                                                       )
                                                                   ))
                                                                 )))
                                                           ;; now if :executable's :prologue is empty, remove it
                                                           ;; WARNING: pathological cases are possible.
                                                           ;; E.g. main1 dep A, main2 dep B; initial prologue is A B
                                                           ;; then what should be the prologues for A and B?
                                                           ;; they could not be shared in this case.
                                                           ;; (format #t "sa: ~A\n" stanza-alist)
                                                           (if-let ((prologue (assoc :prologue stanza-alist)))
                                                                   (begin
                                                                     ;; (format #t "XPL: ~A\n" (cdr prologue))
                                                                     (if (null? (cdr prologue))
                                                                       (dissoc! '(:prologue) stanza-alist))))
                                                           (if *mibl-debug-prologues*
                                                               (format #t "~A: ~A\n" (green "normalized")
                                                                       (assoc :prologue stanza-alist)))
                                                 ))
                                                (else))
                                              (format #t "after stanza: ~A\n" stanza))
                                            stanzas))
                                ;; now remove empty entries from pkg :prologues,
                                ;; and remove pkg :prologues if empty
                                (let ((stanzas (assoc-val :mibl (cdr pkg-kv))))
                                  (dissoc! '(:prologues) stanzas)))
                              pkgs)))
                *mibl-project*)
      ;; (error 'x "X")
      )))

(define share-prologues!  ;; currently not used
  (let ((+documentation+ "Construct :shared-prologues pkg fld.")
        (+signature+ '(normalize-prologues!)))
    (lambda ()
      (if (or *mibl-debug-s7* *mibl-debug-prologues*)
          (format #t "~A~%" (bgblue "normalize-prologues!")))
      (flush-output-port)
      (for-each (lambda (ws-kv)
                  (format #t "ws key: ~A\n" (car ws-kv))
                  (let ((pkgs (car (assoc-val :pkgs (cdr ws-kv)))))
                    (for-each (lambda (pkg-kv)
                                (let ((stanzas (assoc-val :mibl (cdr pkg-kv))))
                                  (format #t "pkg: ~A\n" (car pkg-kv))
                                  (for-each (lambda (stanza)
                                              ;; (format #t "  stanza: ~A\n" stanza)
                                              (case (car stanza)
                                                ((:executable)
                                                 (format #t "  EXECUTABLE: ~A\n" (assoc-val :main (cdr stanza)))
                                                 (let ((stanza-alist (cdr stanza)))
                                                   (if-let ((plog-id (assoc-val :prologue stanza-alist)))
                                                           (begin
                                                             (format #t "  prologue: ~A\n" plog-id)
                                                             (let ((main (assoc-val :main stanza-alist)))
                                                               (let-values (((ml-deps mli-deps) (module->local-deps main (cdr pkg-kv))))
                                                                 (format #t "  ml-deps: ~A\n" ml-deps)
                                                                 (format #t "  mli-deps: ~A\n" mli-deps)
                                                                 (if (or ml-deps mli-deps)
                                                                     ;; get prologue and check for membership
                                                                     (let* ((plogs (assoc-in '(:mibl :prologues) (cdr pkg-kv)))
                                                                            (this-plog (assoc-val plog-id (cdr  plogs)))
                                                                            (plog-modules (assoc :modules this-plog)))
                                                                       (format #t "  this plog modules: ~A\n" plog-modules)
                                                                       (for-each (lambda (m)
                                                                                   (format #t "mdep: ~A\n" m)
                                                                                   (if (member m (cdr plog-modules))
                                                                                       (begin
                                                                                         (format #t "before: ~A\n" plog-modules)
                                                                                         (set-cdr! plog-modules (remove m (cdr plog-modules)))
                                                                                         (format #t "after: ~A\n" plog-modules)
                                                                                         )))
                                                                                 (concatenate ml-deps mli-deps))
                                                                       ;; (error 'a "A")
                                                                       ))
                                                                 )))
                                                           ))
                                                 ;; now if :executable's :prologue is empty, remove it
                                                 ;; WARNING: pathological cases are possible.
                                                 ;; E.g. main1 dep A, main2 dep B; initial prologue is A B
                                                 ;; then what should be the prologues for A and B?
                                                 ;; they could not be shared in this case.
                                                 )
                                                (else)))
                                            stanzas))
                                ;; now remove empty entries from pkg :prologues,
                                ;; and remove pkg :prologues if empty
                                (let ((stanzas (assoc-val :mibl (cdr pkg-kv))))
                                  (dissoc! '(:prologues) stanzas)))
                              pkgs)))
                *mibl-project*)
      ;; (error 'x "X")
      )))

(define update-shared-prologues!
  (let ((+documentation+ "Update stanza :prologue")
        (+signature+ '(update-prologues! prologue pkg stanza-alist)))
    (lambda (prologue pkg stanza-alist)
      (if (or *mibl-debug-s7* *mibl-debug-updaters*)
          (format #t "~A: ~A~%" (bgblue "update-shared-prologues!") prologue))

      (if-let ((pkg-prologues (assoc :prologues (cdr pkg))))
              (begin
                (if *mibl-debug-s7*
                    (format #t "~A: ~A~%" (ublue "found pkg-prologues") pkg-prologues))
                ;; (error 'x "X")
                (if (number? (cdr prologue))
                    (begin) ;; won't happen?
                    (if-let ((x (member (cdr prologue) (cdr pkg-prologues)
                                        (lambda (a b)
                                          (if *mibl-debug-s7*
                                              (format #t "~A: a: ~A, b: ~A~%" (red "comparing") a b))
                                          (equal? a (assoc-val :modules (cdr b)))))))
                            (begin
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (ublue "match") x))
                              (set-cdr! prologue (caar x))
                              )
                            ;; else update pkg-prologues
                            (let* ((ct (+ 1 (length (cdr pkg-prologues))))
                                   (opts (if-let ((opts (assoc-val :opts stanza-alist)))
                                                 `((:opts ,@opts)) '()))
                                   (link-opts (if-let ((opts (assoc-val :link-opts stanza-alist)))
                                                      `((:link-opts ,@opts)) '()))
                                   (ocamlc-opts (if-let ((opts (assoc-val :ocamlc-opts stanza-alist)))
                                                        `((:ocamlc-opts ,@opts)) '()))
                                   (ocamlopt-opts (if-let ((opts (assoc-val :ocamlopt-opts stanza-alist)))
                                                          `((:ocamlopt-opts ,@opts)) '()))
                                   (modules (sort! (cdr prologue) sym<?))
                                   (new (list (cons ct
                                                    `((:modules ,@modules)
                                                      ,@opts ,@link-opts ,@ocamlc-opts ,@ocamlopt-opts)))))
                              (if *mibl-debug-s7*
                                  (begin
                                    (format #t "~A: ~A~%" (ublue "mismatch; adding") prologue)
                                    (format #t "~A: ~A~%" (ublue "ct") ct)
                                    (format #t "~A: ~A~%" (ublue "new") new)))
                              (set-cdr! pkg-prologues (append (cdr pkg-prologues) new ))
                              (set-cdr! prologue ct)
                              ))))
              ;;FIXME: prologues must have all the props of the exe from which they are derived:
              ;; deps, opts, link-opts, etc.
              (let* ((opts (if-let ((opts (assoc-val :opts stanza-alist)))
                                   `((:opts ,@opts)) '()))
                     (link-opts (if-let ((lopts (assoc-val :link-opts stanza-alist)))
                                        `((:link-opts ,@lopts)) '()))
                     (ocamlc-opts (if-let ((opts (assoc-val :ocamlc-opts stanza-alist)))
                                          `((:ocamlc-opts ,@opts)) '()))
                     (ocamlopt-opts (if-let ((opts (assoc-val :ocamlopt-opts stanza-alist)))
                                            `((:ocamlopt-opts ,@opts)) '()))
                     (modules (sort! (cdr prologue) sym<?))
                     (new `((:prologues (1 (:modules ,@modules)
                                           ,@opts ,@link-opts ,@ocamlc-opts ,@ocamlopt-opts)))))
                (if *mibl-debug-s7*
                    (format #t "~A: ~A~%" (ublue "adding :prologues to stanzas") prologue))
                (set-cdr! pkg (append (cdr pkg) new))
                (set-cdr! prologue 1))
              ))))
