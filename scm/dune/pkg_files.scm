(define (-module-in-modules? m modules)
  (if (or *mibl-debug-all* *mibl-debug-updaters*)
      (format #t "~A: ~A~%" (ublue "-module-in-modules?") m))
  (let ((mstr (format #f "~A" m)))
    (find-if (lambda (mod)
               (string=? mstr
                         (format #f "~A" (car mod))))
             modules)))

;; initial parsetree may contain unmatched :sig or :struct files
;; during processing a matching dynamic file may be discovered,
;; giving e.g. foo.ml in :structures (static) and foo.mli in :signatures (:dynamic)
;; or: a foo.mll file and a foo.mli file will be merged into a :modules entry
;; this routine moves them to :modules (Foo (:ml foo.ml) (:mli_ foo.mli))
(define (normalize-pkg-files!)
  (if (or *mibl-debug-all* *mibl-debug-updaters*)
      (format #t "~A~%" (ublue "normalize-pkg-files!")))
  ;; for each pkg in each workspace
  (for-each (lambda (ws-kv)
              (if (or *mibl-debug-all* *mibl-debug-updaters*)
                  (format #t "~A: ~A~%" (green "ws") (car ws-kv)))
              (let ((pkgs (car (assoc-val :pkgs (cdr ws-kv)))))
                (for-each (lambda (pkg-kv)
                            (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                (format #t "~A: ~A~%" (green "pkg") (car pkg-kv)))
                            (let* ((pkg (cdr pkg-kv))
                                   (pkg-modules (assoc-val :modules pkg))
                                   (pkg-structs (assoc :structures pkg))
                                   (pkg-sigs (assoc :signatures pkg)))
                              (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                  (begin
                                    (format #t "~A: ~A~%" (blue "pkg-modules") pkg-modules)
                                    (format #t "~A: ~A~%" (blue "pkg-structs") pkg-structs)
                                    (format #t "~A: ~A~%" (blue "pkg-sigs") pkg-sigs)))
                              ;; (if pkg-modules
                              ;;     (begin
                              ;;       (if (or *mibl-debug-all* *mibl-debug-updaters*)
                              ;;           (format #t "~A:~%" (cyan "pkg-modules")))
                              ;;       (for-each (lambda (m)
                              ;;                   (if (or *mibl-debug-all* *mibl-debug-updaters*)
                              ;;                       (format #t "  ~A~%" m)))
                              ;;                 pkg-modules))
                              ;;     )

                              (if pkg-structs
                                  (let* ((statics (if-let ((statics (assoc-in '(:structures :static) pkg)))
                                                          statics '(:static)))
                                         (dynamics (if-let ((dynamics (assoc-in '(:structures :dynamic) pkg)))
                                                           dynamics '(:dynamic))))
                                    (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                        (begin
                                          (format #t "~A: ~A~%" (cyan "pstructs, static") statics)
                                          (format #t "~A: ~A~%" (cyan "pstructs, dynamic") dynamics)))
                                    (if pkg-modules
                                        (begin ;; twice, once for statics, once for dynamics
                                          (if (truthy? statics)
                                              (let ((remainder
                                                     (filter (lambda (struct)
                                                               (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                   (format #t "~A: ~A~%" (uwhite "struct") struct))
                                                               (if-let ((x (-module-in-modules? (car struct) pkg-modules)))
                                                                       (begin
                                                                         (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                             (format #t "~A: ~A~%" (uwhite "in modules?") x))
                                                                         #f)
                                                                       #t))
                                                             (cdr statics))))
                                                (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                    (format #t "~A: ~A~%" (ured "structs static remainder") remainder))
                                                (if (null? remainder)
                                                    (dissoc! '(:structures :static) pkg)
                                                    (set-cdr! statics remainder))))

                                          (if (truthy? dynamics)
                                              (let ((remainder
                                                     (filter (lambda (struct)
                                                               (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                   (format #t "~A: ~A~%" (uwhite "struct") struct))
                                                               (if-let ((x (-module-in-modules? (car struct) pkg-modules)))
                                                                       (begin
                                                                         (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                             (format #t "~A: ~A~%"
                                                                                     (uwhite "in modules?") x))
                                                                         #f)
                                                                       #t))
                                                             (cdr dynamics))))
                                                (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                    (format #t "~A: ~A~%" (ured "structs dyn remainder") remainder))
                                                (if (null? remainder)
                                                    (begin
                                                      ;; (dissoc! '(:structures :dynamic) pkg)
                                                      )
                                                    (alist-update-in! pkg `(:structures :dynamic)
                                                                      (lambda (old) remainder)))))
                                          ))))
                              ;; (format #t "~A: ~A~%" (bgmagenta "updated structs") (assoc :structures pkg))
                              ;; (if (equal? "compiler/lib" (assoc-val :pkg-path pkg))
                              ;;     (error 'STOP "nmani"))

                              ;; if :signatures item is in :modules, remove it from :signatures
                              (if pkg-sigs
                                  (let* ((statics (if-let ((statics (assoc-in '(:signatures :static) pkg)))
                                                          statics '()))
                                         (dynamics (if-let ((dynamics (assoc-in '(:signatures :dynamic) pkg)))
                                                           dynamics '())))

                                    ;; (let* ((_ (if (or *mibl-debug-all* *mibl-debug-updaters*) (format #t "~A: ~A~%" (red "pkg-sigs") pkg-sigs)))
                                    ;;        (psigs (cdr pkg-sigs))

                                    ;;        (statics (if-let ((statics (assoc-val :static psigs)))
                                    ;;                         statics '()))
                                    ;;        (dynamics (if-let ((dynamics (assoc-val :dynamic psigs)))
                                    ;;                          dynamics '())))
                                    (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                        (begin
                                          (format #t "~A: ~A~%" (cyan "psigs, static") statics)
                                          (format #t "~A: ~A~%" (cyan "psigs, dynamic") dynamics)))

                                    (if pkg-modules
                                        (begin
                                          (if (truthy? statics)
                                              (let ((remainder  ;; :signatures :static
                                                     (filter (lambda (sig)
                                                               (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                   (format #t "~A: ~A~%" (uwhite "sig") sig))
                                                               (if-let ((x (-module-in-modules? (car sig) pkg-modules)))
                                                                       (begin
                                                                         (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                             (format #t "~A: ~A~%"
                                                                                     (uwhite "in modules?") x))
                                                                         #f)
                                                                       #t))
                                                             (cdr statics))))
                                                (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                    (begin
                                                      (format #t "~A: ~A~%" (ured "sig statics") statics)
                                                      (format #t "~A: ~A~%" (ured "sig static remainder") remainder)))
                                                (if (null? remainder)
                                                    (dissoc! '(:signatures :static) pkg)
                                                    (set-cdr! statics remainder))))

                                          (if (truthy? dynamics)
                                              (let ((remainder  ;; :signatures :dynamic
                                                     (filter (lambda (sig)
                                                               (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                   (format #t "~A: ~A~%" (uwhite "sig") sig))
                                                               (if-let ((x (-module-in-modules? (car sig) pkg-modules)))
                                                                       (begin
                                                                         (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                                             (format #t "~A: ~A~%"
                                                                                     (uwhite "in modules?") x))
                                                                         #f)
                                                                       #t))
                                                             (cdr dynamics))))
                                                (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                    (format #t "~A: ~A~%" (ured "sigs dyn remainder") remainder))
                                                (if (null? remainder)
                                                    (dissoc! '(:signatures) pkg)
                                                    (alist-update-in! pkg `(:signatures :dynamic)
                                                                      (lambda (old) remainder)))))))
                                    )
                                  )
                              ))
                          pkgs)))
            *mibl-project*))

;; deps in parsetree may contain vectors like #(Stdlib String),
;; emitted by codept. remove them.
(define (prune-pkg-file-deps!)
  (mibl-trace-entry "prune-pkg-file-deps!" "")
  ;; WARNING: if filtered deps list is null, we need to restore the list to a pair (w/dot)
  ;; e.g. (:ml foo.ml #(Stdlib String)) => (:ml . foo.ml)
  (for-each (lambda (ws-kv)
              ;; (format #t "ws key: ~A\n" (car ws-kv))
              (let ((pkgs (car (assoc-val :pkgs (cdr ws-kv)))))
                (for-each (lambda (pkg-kv)
                            (let* ((pkg (cdr pkg-kv))
                                   (pkg-modules (assoc-val :modules pkg))
                                   (pkg-structs (assoc :structures pkg))
                                   (pkg-sigs (assoc :signatures pkg)))

                              (mibl-trace "pkg-modules" pkg-modules)
                              (mibl-trace "pkg-structs" pkg-structs)
                              (mibl-trace "pkg-sigs" pkg-sigs)

                              (if (truthy? pkg-modules)
                                  (for-each (lambda (module)
                                              (mibl-trace "module" module)
                                              (if-let ((ml-static (assoc :ml (cdr module))))
                                                      (if (list? (cdr ml-static))
                                                          (begin
                                                            (mibl-trace "ml-static" ml-static)
                                                            (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr ml-static))))
                                                              (mibl-trace "filtered deps" filtered)
                                                              (set-cdr! ml-static filtered)
                                                              (mibl-trace "filtered ml" ml-static)
                                                              ))
                                                          ;;(mibl-trace "skipping" module)
                                                          ))
                                              (if-let ((ml-dynamic (assoc :ml_ (cdr module))))
                                                      (if (list? (cdr ml-dynamic))
                                                          (begin
                                                            (mibl-trace "ml-dynamic b" ml-dynamic)
                                                            (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr ml-dynamic))))
                                                              (set-cdr! ml-dynamic filtered)
                                                              ;; (format #t "~A: ~A~%" (uwhite "filtered ml") ml-static)
                                                              ))))
                                              (if-let ((mli-static (assoc :mli (cdr module))))
                                                      (if (list? (cdr mli-static))
                                                          (begin
                                                            (mibl-trace "mli-static" mli-static)
                                                            (let ((filtered (filter (lambda (dep)
                                                                                      (not (vector? dep)))
                                                                                    (cdr mli-static))))
                                                              (begin
                                                                ;; (format #t "FFFF ~A~%" filtered)
                                                                (set-cdr! mli-static filtered)
                                                                ;; (set-cdr! mli-static :FOO)))))
                                                                ;; (format #t "~A: ~A~%" (uwhite "filtered mli") mli-static)
                                                                )))))
                                                              ;; ))))
                                              (if-let ((mli-dynamic (assoc :mli_ (cdr module))))
                                                      (if (list? (cdr mli-dynamic))
                                                          (begin
                                                            (mibl-trace "mli-dynamic" mli-dynamic)
                                                            (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr mli-dynamic))))
                                                              (set-cdr! mli-dynamic filtered)
                                                              ;; (format #t "~A: ~A~%" (uwhite "filtered mli") mli-dynamic)
                                                              ))))
                                              )
                                            pkg-modules))
                              (mibl-trace "pruning pkg structs" "")
                              (if (truthy? pkg-structs)
                                  (let* ((statics (if-let ((statics (assoc-in '(:structures :static) pkg)))
                                                          statics '()))
                                         (dynamics (if-let ((dynamics (assoc-in '(:structures :dynamic) pkg)))
                                                           dynamics '()))
                                         )
                                    (mibl-trace "pkg structs, static" statics)
                                    (mibl-trace "pkg structs, dynamic" dynamics)
                                    (if (truthy? statics)
                                        (for-each (lambda (struct)
                                                    (mibl-trace "sstruct" struct)
                                                    (if (list? (cdr struct))
                                                        (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr struct))))
                                                          (set-cdr! struct filtered)
                                                          ;; (format #t "~A: ~A~%" (uwhite "filtered struct") struct)
                                                          )))
                                                  (cdr statics)))
                                    (if (truthy? dynamics)
                                        (for-each (lambda (struct)
                                                    (mibl-trace "dstruct" struct)
                                                    (if (list? (cdr struct))
                                                        (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr struct))))
                                                          (set-cdr! struct filtered))))
                                                  (cdr dynamics)))
                                    ))

                              (mibl-trace "pruning pkg sigs" "")
                              (if pkg-sigs
                                  (let* ((statics (if-let ((statics (assoc-in '(:signatures :static) pkg)))
                                                          statics '()))
                                         (dynamics (if-let ((dynamics (assoc-in '(:signatures :dynamic) pkg)))
                                                           dynamics '()))
                                         )
                                    (if (truthy? statics)
                                        (for-each (lambda (sig)
                                                    (mibl-trace "sig" sig :test *mibl-debug-updaters*)
                                                    (if (list? (cdr sig))
                                                        (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr sig))))
                                                          (mibl-trace "filtered sigs" filtered :test *mibl-debug-updaters*)
                                                          (set-cdr! sig filtered)
                                                          (mibl-trace "updated sig" filtered :test *mibl-debug-updaters*)
                                                          )))
                                                  (cdr statics))

                                    (if (truthy? dynamics)
                                        (for-each (lambda (sig)
                                                    (if (or *mibl-debug-all* *mibl-debug-updaters*)
                                                        (format #t "~A: ~A~%" (uwhite "sig") sig)))
                                                    (if (list? (cdr sig))
                                                        (let ((filtered (filter (lambda (dep) (not (vector? dep))) (cdr sig))))
                                                          (set-cdr! sig filtered)
                                                          ;; (format #t "~A: ~A~%" (uwhite "filtered sig") sig)
                                                          )))
                                                  (cdr dynamics)))))
                              ))
                          pkgs)))
            *mibl-project*))

(define (detect-ppx-inline f)
  (let ((is-ppx-inline #f)
        (is-ppx-expect #f))
    (call-with-input-file f
      (lambda (file)
        (let loop ((line (read-line file #t)))
          (or is-ppx-inline
              (eof-object? line)
              ;; ppx_inline_test ops: let%test, let%test_unit, let%test_module
	      (let ((pos (or (string-position "let%test_module " line)
                             (string-position "let%test_unit " line)
                             (string-position "let%test " line))))
	        (if pos (set! is-ppx-inline #t))))
	  (loop (read-line file #t)))))
    is-ppx-inline))

(define module->local-deps
  (let ((+documentation+ "Lookup module (normalized name) in pkg (:modules, :structures) and return depslist.")
        (+signature+ '(module->local-deps module pkg)))
    (lambda (module pkg)
      (mibl-trace-entry "module->local-deps" module)
      (let* ((module-tlbl (module-name->tagged-label module pkg)))
        (mibl-trace "module-tlbl" module-tlbl)
        (if (alist? (cdr module-tlbl))
            (let* ((ml-deps (if-let ((deps (assoc-val :ml (cdr module-tlbl))))
                                    (if (list? deps)
                                        (cdr deps)
                                        '())
                                    (if-let ((deps (assoc-val :ml_ (cdr module-tlbl))))
                                            (cdr deps)
                                            '())))
                   (mibl-trace-let "ml-deps" ml-deps)
                   (mli-deps (if-let ((deps (assoc-val :mli (cdr module-tlbl))))
                                     (if (list? deps)
                                         (cdr deps)
                                         '())
                                     (if-let ((deps (assoc-val :mli_ (cdr module-tlbl))))
                                             (cdr deps)
                                             '())))
                   (mibl-trace-let "mli-deps" mli-deps)
                   )
              (mibl-trace "mli-deps 2" mli-deps)
              (values ml-deps mli-deps))
            ;; else must be a struct, (Foo foo.ml Dep1 ...)
            (let ((ml-deps (cddr module-tlbl)))
              (values ml-deps '())))))))

;; (define module->tlbl
;;   (let ((+documentation+ "Lookup module (normalized name) in pkg (:modules, :structures) and return tagged label.")
;;         (+signature+ '(module->tlbl module pkg)))
;;     (lambda (module pkg)
;;       (mibl-trace "module->local-deps" module)
;;       (let* ((module-tlbl (module-name->tagged-label module pkg))
;;              (mibl-trace-let "module tagged lbl" module-tlbl)
;;              )
;;         ))))

;; find module anywhere in local workspace
(define module-name->local-target
  (let ((+documentation+ "Search entire ws for <module> (normalized module name)")
        (+signature+ '(module-name->local-target return module ws-id pkg)))
    (lambda (return module ws-id pkg)
      (mibl-trace-entry "module-name->local-target" module :test *mibl-debug-deps*)
      ;; (format #t "WS: ~A~%" ws-id)
      (let* ((@ws (assoc-val ws-id *mibl-project*))
             (pkgs (car (assoc-val :pkgs @ws)))
             (mpkg-alist (map (lambda (pkg-assoc)
                                ;; pkg-assoc key: pkg path (string)
                                ;; val: assoc-list
                                ;; (format #t "~A: ~A~%" "pkg" (car pkg-assoc))
                                (if-let ((mdep (module-name->tagged-label module (cdr pkg-assoc))))
                                        ;; ((mdep (module-name->tagged-label dep pkg))
                                        (begin
                                          ;; (mibl-trace "FOUND :local dep" mdep :test *mibl-debug-deps*)
                                          (return mdep)))
                                )
                              pkgs)))
        #f))))
        ;; (format #t "MPKG ALIST: ~A~%" mpkg-alist)
        ;; (if (eq? module 'Suba)
        ;;     '(:mod //foo:Suba)
        ;;     #f)))))

(define module-name->tagged-label
  (let ((+documentation+ "Search pkg :modules, :structures, :signatures for <module> (normalized module name)")
        (+signature+ '(module-name->tagged-label module pkg)))
    (lambda (module pkg)
      (mibl-trace-entry "module-name->tagged-label" module :test *mibl-debug-deps*)
      (mibl-trace "pkg" (assoc-val :pkg-path pkg) :test *mibl-debug-deps*)
      (let* ((pkg-modules (if-let ((files (assoc-val :modules pkg)))
                                  files '()))
             (mibl-trace-let "pkg-modules" pkg-modules)
             ;; (m-name (filename->module-name arg))
             ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "m-name") m-name)))
             (entry
              (find-if (lambda (mod)
                         (mibl-trace "mod" mod)
                         (equal? (format #f "~A" module)
                                 (format #f "~A" (car mod))))
                       pkg-modules)))
        (mibl-trace "found pkg-module entry?"  entry :test *mibl-debug-deps*)
        ;; (mibl-trace "pkg" (assoc :pkg-path pkg) :test *mibl-debug-deps*)
        (if entry
            `(:mod (:pkg ,(assoc-val :pkg-path pkg)) ,(car entry))
            ;; else srch pkg-structs
            (let* ((structs-static (if-let ((ss (assoc-in '(:structures :static) pkg))) (cdr ss) '()))
                   (structs-dynamic (if-let ((ss (assoc-in '(:structures :dynamic) pkg))) (cdr ss) '()))
                   (pkg-structs (append structs-static structs-dynamic))
                   (mibl-trace-let "pkg-structs" pkg-structs)
                   (entry
                    (find-if (lambda (struct)
                               ;; (format #t "~A: ~A~%" (white "struct") struct)
                               (equal? (format #f "~A" module)
                                       (format #f "~A" (car struct))))
                             pkg-structs)))
              (mibl-trace "found pkg-struct entry?" entry :test *mibl-debug-deps*)
              (if entry
                  `(:struct (:pkg ,(assoc-val :pkg-path pkg)) ,(car entry))
                  ;; `(:struct ,(car entry))
                  (let* ((sigs-static (if-let ((ss (assoc-in '(:signatures :static) pkg))) (cdr ss) '()))
                         (sigs-dynamic (if-let ((ss (assoc-in '(:signatures :dynamic) pkg))) (cdr ss) '()))
                         (pkg-sigs (append sigs-static sigs-dynamic))
                         (mibl-trace-let "pkg-sigs" pkg-sigs)
                         (entry
                          (find-if (lambda (struct)
                                     ;; (format #t "~A: ~A~%" (white "struct") struct)
                                     (equal? (format #f "~A" module)
                                             (format #f "~A" (car struct))))
                                   pkg-sigs)))
                    (mibl-trace "found pkg-sig entry?" entry :test *mibl-debug-deps*)
                    (if entry
                        `(:sig (:pkg ,(assoc-val :pkg-path pkg)) ,(car entry))
                        ;; `(:sig ,(car entry))
                        entry)))))))))
              ;; entry))))))

(define (-find-m-file-in-pkg-modules arg pkg)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-modules") arg))
  (let* ((pkg-modules (if-let ((files (assoc-val :modules pkg)))
                                files '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-modules") pkg-modules)))
         (m-name (filename->module-name arg))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "m-name") m-name)))
         (file
          (find-if (lambda (mod)
                     ;; (format #t "~A: ~A~%" (white "mod") mod)
                     (equal? (format #f "~A" m-name)
                             (format #f "~A" (car mod))))
                   pkg-modules)))
        (format #t "~A: ~A~%" (white "found file") file)
        file))

;; arg: filename
(define (-find-m-file-in-pkg-structs arg pkg)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-structs") arg))
  (let* ((statics (if-let ((files (assoc-in '(:structures :static) pkg)))
                          (cdr files) '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "statics") statics)))
         (dynamics (if-let ((files (assoc-in '(:structures :dynamic) pkg)))
                           (cdr files) '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "dynamics") dynamics)))
         (structs (concatenate statics dynamics))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "structs") structs)))
         (file
          (find-if (lambda (struct)
                     ;; (format #t "~A: ~A~%" (white "struct") struct)
                     (equal? (format #f "~A" arg)
                             (format #f "~A" (cdr struct))))
                   structs)))
        ;; (format #t "~A: ~A~%" (white "found file") file)
        file))

;; arg: filename
(define (-find-m-file-in-pkg-sigs arg pkg)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-sigs") arg))
  (let* ((pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                              files '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
         (file
          (find-if (lambda (sig)
                     ;; (format #t "~A: ~A~%" (white "sig") sig)
                     (equal? (format #f "~A" arg)
                             (format #f "~A" (cdr sig))))
                   pkg-sigs)))
        ;; (format #t "~A: ~A~%" (white "found file") file)
        file))

;; returns pair from :modules, :structures, or :signatures
;; arg: filename
;; e.g. (:ml foo.ml) or (Foo . foo.ml) or (Foo . foo.mli)
(define (find-m-file-in-pkg arg pkg)
  (if *mibl-debug-all*
      (begin
        (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg") arg)
        (format #t "~A: ~A~%" (blue ":files") (assoc-val :files pkg))
        ;; (format #t "~A: ~A~%" (blue "deps") deps)
        (format #t "~A: ~A~%" (red ":scripts") (assoc-val :scripts pkg))))

  (if-let ((m (-find-m-file-in-pkg-modules arg pkg)))
          m
          (if-let ((m (-find-m-file-in-pkg-structs arg pkg)))
                  m
                  (if-let ((m (-find-m-file-in-pkg-sigs arg pkg)))
                          m))))

(define (is-module-in-pkg m pkg)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (ucyan "is-module-in-pkg") m))
  ;; (format #t "~A: ~A~%" (ucyan "pkg") pkg)
  (let* ((pkg-mods (if-let ((files (assoc-val :modules pkg)))
                             (map car files) '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-mods") pkg-mods)))
         (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                              (let* (;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (uyellow "pkg-structs") structs)))
                                     (statics (if-let ((statics (assoc-val :static structs)))
                                                      statics '()))
                                     ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "struct statics") statics)))
                                     (dynamics (if-let ((dynamics (assoc-val :dynamic structs)))
                                                       dynamics '()))
                                     ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "struct dynamics") dynamics)))
                                     ) ;; gets both :static & :dynamic
                                (map car (concatenate statics dynamics)))
                           '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs)))
         (pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                           (map car (car (map cdr files)))
                           '()))
         ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
         (modules (concatenate pkg-mods pkg-structs pkg-sigs))
         (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "all pkg modules") modules)))
         (answer (member m modules)))
    (if *mibl-debug-all*
        (format #t "~A: ~A~%" (cyan "answer") answer))
    ;; (if (equal? 'Arg m) (error 'STOP "STOP mod in pkg"))
    answer))

;;FIXME: rename
;; may update deps
(define (find-file-in-pkg-files!? arg deps pkg)
  (mibl-trace-entry "find-file-in-pkg-files!?" arg)
  (if *mibl-debug-all*
      (begin
        (format #t "~A: ~A~%" (ublue "find-file-in-pkg-files") arg)
        (format #t "~A: ~A~%" (blue ":files") (assoc-val :files pkg))
        (format #t "~A: ~A~%" (blue "deps") deps)))
  ;; (format #t "~A: ~A~%" (red ":scripts") (assoc-val :scripts pkg))

  (if (assoc-val :files pkg)
      (let* ((pkg-files (if-let ((files (assoc-val :files pkg)))
                                files '()))
             (pkg-files (append
                         (if-let ((statics (assoc-val :static pkg-files)))
                                 statics '())
                         (if-let ((dynamics (assoc-val :dynamic pkg-files)))
                                 dynamics '())))
             (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "pkg-files") pkg-files)))
             (file
              (find-if (lambda (f)
                         (if *mibl-debug-all*
                             (format #t "~A: ~A~%" (white "f") f))
                         (equal? (format #f "~A" arg)
                                 (format #f "~A" f)))
                       pkg-files)))
        (if *mibl-debug-all*
            (format #t "~A: ~A~%" (white "found file") file))
        (if file
            #t
            ;; update deps/targets but no dups
            ;; (let ((key (string->keyword (format #f "~A" arg))))
            ;;   (format #t "~A: ~A~%" (magenta "FOUND ~A in :files") file)
            ;;   (set-cdr! deps (cons (cons key
            ;;                              (list (cons :pkg (assoc-val :pkg-path pkg))
            ;;                                    (cons :tgt arg)))
            ;;                        (cdr deps)))
            ;;   key)

            ;; else not found in :files
            ;; if it looks like a filename, add it?
            (begin
              (if (string-index (format #f "~A" arg)
                                (lambda (ch) (equal? ch #\/)))
                  #f ;; skip if not in this pkg
                  (begin
                    ;; (format #t "~A~%" (ured "XXXX"))
                    (set! pkg (update-pkg-files! pkg (list arg)))
                    #t)))))
      ;; else no :files field; add it
      (begin
        ;; (format #t "~A~%" (ured "YYYYYYYYYYYYYYYY"))
        (set! pkg (update-pkg-files! pkg (list arg)))
        #t #|(string->keyword (format #f "~A" arg))|# )))

;; (define (-codept-pkg-file-deps pkg-path)
;;   (if *mibl-debug-all*
;;       (format #t "~A: ~A~%" (blue "-codept-pkg-file-deps") pkg-path))
;;   (let* ((fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
;;          (cmd (format #f "codept -k -expand-deps -sexp -I ~A ~A/*"
;;                       pkg-path pkg-path))
;;          (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "cmd") cmd))))
;;     (let ((deps (string-trim '(#\newline) (system cmd #t))))
;;       (if *mibl-debug-all*
;;           (format #t "~A: ~A~%" (yellow "codept") deps)))
;;   ;;(set-cdr! struct (cdr struct))
;;     ))

;; OBSOLETE: deps are handled by load-project c impl
;; run ocamldep against all static src files in pkg
;; iterate over output, updating :pkg-modules, :pkg-structures
;; this handles only static srcs, dynamics (ocamllex, ocamlyacc)
;; must be handled separately
;; (define (-ocamldep-pkg-static-file-deps! pkg)
;;   (if *mibl-debug-all*
;;       (format #t "~A: ~A~%" (bgblue "-ocamldep-pkg-static-file-deps") (assoc-val :pkg-path pkg)))
;;           ;; (assoc-val :pkg-path pkg))
;;   (let* ((pkg-path (assoc-val :pkg-path pkg))
;;          (fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
;;          (pkg-mods (if-let ((files (assoc-val :modules pkg)))
;;                              (map car files) '()))
;;          (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-mods") pkg-mods)))
;;          (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
;;                               (let* (;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (uyellow "pkg-structs") structs)))
;;                                      (statics (if-let ((statics (assoc-val :static structs)))
;;                                                       statics '()))
;;                                      ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "struct statics") statics)))
;;                                      (dynamics (if-let ((dynamics (assoc-val :dynamic structs)))
;;                                                        dynamics '()))
;;                                      ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "struct dynamics") dynamics)))
;;                                      ) ;; gets both :static & :dynamic
;;                                 (map car (concatenate statics dynamics)))
;;                            '()))
;;          (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs)))

;;          (pkg-sigs (if-let ((sigs (assoc-val :signatures pkg)))
;;                               (let* (;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (uyellow "pkg-sigs") sigs)))
;;                                      (statics (if-let ((statics (assoc-val :static sigs)))
;;                                                       statics '()))
;;                                      ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "sig statics") statics)))
;;                                      (dynamics (if-let ((dynamics (assoc-val :dynamic sigs)))
;;                                                        dynamics '()))
;;                                      ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (cyan "sig dynamics") dynamics)))
;;                                      ) ;; gets both :static & :dynamic
;;                                 (map car (concatenate statics dynamics)))
;;                            '()))
;;          (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
;;          ;; (pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
;;          ;;                   (map car files)
;;          ;;                   '()))
;;          ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))

;;          (modules (concatenate pkg-mods pkg-structs pkg-sigs))
;;          (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "all pkg modules") modules)))
;;          )
;;     (let* ((cmd (format #f "ocamldep -one-line -modules -I ~A ~A/*"
;;                         pkg-path pkg-path))
;;            (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "cmd") cmd)))
;;            (deps (string-trim '(#\newline) (system cmd #t))) ;; EXECUTE CMD
;;            (deps (string-split deps #\newline)))
;;       (for-each (lambda (dep)
;;                   (if *mibl-debug-all*
;;                       (format #t "~%~A: ~A~%" (uyellow "iter over ocamldeps") dep))
;;                   (let ((segs (string-split dep #\:)))
;;                     (if *mibl-debug-all*
;;                         (format #t "~A: ~A~%" (yellow "segs") segs))
;;                     (if (null? (cdr segs))
;;                         (begin)
;;                         (let* ((fpath (car segs))
;;                                (fname (basename fpath))
;;                                (mname (filename->module-name fname))
;;                                (kind (filename->kind fname))
;;                                (mdeps (string-trim '(#\space) (cadr segs)))
;;                                (mdeps (string-split mdeps #\space))
;;                                (mdeps (map string->symbol mdeps))

;;                                (_ (if *mibl-debug-all* (format #t "~A for ~A: ~A~%" (ugreen "unfiltered mdeps") mname mdeps)))
;;                                ;; do not include file module in deps list
;;                                (mdeps (remove mname mdeps))
;;                                (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (ugreen "unfiltered mdeps excluding self") mdeps)))
;;                                ;; eliminate mdeps not in this pkg
;;                                ;; (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
;;                                (mdeps (filter (lambda (dep) (member dep modules)) mdeps))
;;                                (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (ugreen "filtered mdeps, excluding pkg-externals") mdeps)))
;;                                )
;;                           (if *mibl-debug-all*
;;                               (begin
;;                                 (format #t "~A: ~A~%" (red "mdeps") mdeps)
;;                                 (format #t "~A: ~A~%" (red "pkg (before)") pkg)
;;                                 (format #t "~A: ~A~%" (red "fname") fname)
;;                                 (format #t "~A: ~A~%" (red "mname") mname)))
;;                           ;; (if (string=? "arg.ml" fname) (error 'STOP "STOP ocamldep"))
;;                           (if (truthy? mdeps) ;; (not (null? mdeps))
;;                               (begin
;;                                 (if *mibl-debug-all*
;;                                     (format #t "~A ~A to ~A~%" (bgyellow "updating stanza :deps") mdeps fname))
;;                                 ;; (format #t "~A: ~A~%" (uyellow "in pkg") pkg)
;;                                 (if (assoc-val :mibl pkg)
;;                                     (update-stanza-deps pkg fname mdeps))
;;                                 (if *mibl-debug-all*
;;                                     (format #t "~A: ~A~%" (red "pkg (after)") pkg))
;;                                 ))

;;                           ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
;;                           ;; we retrieve the pkg-dep for fname and add the mdeps to it
;;                           ;; (format #t "~A: ~A~%" (bgyellow "updating pkg file flds") pkg)
;;                           ;; (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
;;                           ;; (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
;;                           ;; (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
;;                           (if (truthy? mdeps) ;; (not (null? mdeps))
;;                               (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
;;                                       (begin
;;                                         (if *mibl-debug-all*
;;                                             (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc))
;;                                         (if (proper-list? m-assoc)
;;                                             ;; its a module entry, form (A (:ml a.ml) (:mli a.mli))
;;                                             (begin ;; if mdeps not empty
;;                                               (set-cdr! m-assoc
;;                                                         (append (cdr m-assoc)
;;                                                                 (list (cons
;;                                                                        (if (eq? kind :struct)
;;                                                                            :ml-deps :mli-deps)
;;                                                                        mdeps))))
;;                                               ;; (format #t "~A: ~A~%" (bgred "m-assoc after") m-assoc)
;;                                               )
;;                                             ;; else its a struct entry, (A a.ml)
;;                                             (begin
;;                                               (if *mibl-debug-all*
;;                                                   (begin
;;                                                     (format #t "~A: ~A~%" (bgred "STRUCT ENTRY") m-assoc)
;;                                                     (format #t "~A: ~A~%" (bgred "adding mdeps") mdeps)))
;;                                               (if (not (null? mdeps))
;;                                                   (set-cdr! m-assoc
;;                                                             (cons (cdr m-assoc)
;;                                                                   mdeps))))))
;;                                       ;;else
;;                                       (format #t "~A: ~A~%" (blue "not found") m-assoc))
;;                               ;; else mdeps is null
;;                               (if *mibl-debug-all*
;;                                   (format #t "~A~%" (uyellow "continue")))
;;                               )))))
;;                 deps))))

;; (define (-resolve-pkg-struct-deps pkg-path pkg-structs)
;;   (if *mibl-debug-all*
;;       (format #t "~A: ~A~%" (blue "-resolve-pkg-struct-deps") pkg-structs))
;;   (let* ((statics (if-let ((statics (assoc-val :static pkg-structs)))
;;                           statics '()))
;;          (dynamics (if-let ((dynamics (assoc-val :dynamics pkg-structs)))
;;                            dynamics '()))
;;          (structs (concatenate statics dynamics)))
;;     ;; (format #t "~A: ~A~%" (green "statics") statics)
;;     ;; (format #t "~A: ~A~%" (green "dynamics") dynamics)
;;     ;; (format #t "~A: ~A~%" (green "structs") structs)
;;     (for-each (lambda (struct)
;;                 ;; (format #t "~A: ~A~%" (green "s") struct)
;;                 (let* ((fname (cdr struct))
;;                        (fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
;;                        (cmd (format #f "codept -k -expand-deps -sexp -I ~A ~A"
;;                                     pkg-path fpath))
;;                        ;; (cmd (format #f "ocamldep -one-line -modules ~A" fpath))
;;                        (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "cmd") cmd)))
;;                        (deps (string-trim '(#\newline) (system cmd #t))))
;;                   ;;(format #t "~A: ~A~%" (yellow "ocamldeps") deps)
;;                   )
;;                   ;;(set-cdr! struct (cdr struct))
;;                   )
;;               structs)))

;; resolve-pkg-file-deps OBSOLETE
;; main task: run ocamldep on each (static) file in the pkg,
;; filter results to exclude anything not in this pkg
;; (since any other deps will be resolved via the exports tbl or opam)
;; then update the pkg file entries and also stanza :deps flds

;; for each pkg file (module, sig, struct),
;;   find the stanza(s) that depend(s) on it
;;   if depending stanza is namespaced aggregate, continue (?)
;;   else ocamldep and update the pkg fld with :deps
;;   if stanza is executable?
;; skip :ocamllex, :ocamlyacc, they're already elaborated
;; add flds (:ml-deps), (:mli-deps)
;; OBSOLETE
;; (define (resolve-pkg-file-deps ws)      
;;   (if *mibl-debug-all*
;;       (format #t "~%~A: ~A~%" (bgred "resolve-pkg-file-deps for ws") ws))
;;   (let* ((@ws (assoc-val ws *mibl-project*))
;;          (pkgs (car (assoc-val :pkgs @ws))))
;;     (if *mibl-debug-all*
;;         (format #t "~A: ~A~%" (uwhite "pkgs") pkgs))
;;     (for-each (lambda (pkg-kv)
;;                 (if *mibl-debug-all*
;;                     (format #t "~A: ~A~%" (uwhite "for pkg") (car pkg-kv)))
;;                 (let* ((pkg-path (car pkg-kv))
;;                        (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
;;                        (pkg (cdr pkg-kv))
;;                        ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (green "pkg") pkg)))
;;                        (pkg-modules (if-let ((ms (assoc-val :modules pkg)))
;;                                             ms '()))
;;                        (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
;;                                             structs '()))
;;                        (pkg-sigs (if-let ((sigs (assoc-val :signatures pkg)))
;;                                          (car sigs) '())))
;;                   ;; (format #t "~A: ~A~%" (green "pkg-modules") pkg-modules)
;;                   ;; (format #t "~A: ~A~%" (green "pkg-structs") pkg-structs)
;;                   ;; (format #t "~A: ~A~%" (green "pkg-sigs") pkg-sigs)

;;                   ;; (-resolve-pkg-struct-deps pkg-path pkg-structs)))
;;                   ;; (-resolve-pkg-sig-deps pkg-sigs)
;;                   ;; (-codept-pkg-file-deps pkg-path)))

;;                   (if pkg
;;                       (begin
;;                         ;; (-ocamldep-pkg-static-file-deps! pkg)
;;                         ;; (format #t "~A: ~A~%" (bgred "pkg after sfd") pkg)
;;                         ;; (-ocamldep-pkg-dynamic-file-deps! pkg)
;;                         ;; (format #t "~A: ~A~%" (bgred "pkg after dfd") pkg)
;;                         ;;FIXME: handle sigs
;;                         )
;;                       (error 'STOP
;;                              (format #f "STOP ~A" pkgs)))
;;                   ))
;;               ;; now delete any files generated by ocamllex or ocamlyacc
;;               ;; (for-each (lambda (stanza)
;;               ;;             (if (equal? :ocamlyacc (car stanza))
;;               ;;                 (for-each (lambda (yacc)
;;               ;;                             ;; delete the .ml, .mli files
;;               ;;                             )
;;               ;;                           (cdr stanza))
;;               ;;                 ))
;;               ;;           (assoc-val :mibl pkg))
;;               pkgs)))
