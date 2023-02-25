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

;; arg:  normalized module name
(define (find-module-in-pkg module pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "find-module-in-pkg") module))
  (let* ((pkg-modules (if-let ((files (assoc-val :modules pkg)))
                                files '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-modules") pkg-modules)))
         ;; (m-name (filename->module-name arg))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "m-name") m-name)))
         (entry
          (find-if (lambda (mod)
                     ;; (format #t "~A: ~A~%" (white "mod") mod)
                     (equal? (format #f "~A" module)
                             (format #f "~A" (car mod))))
                   pkg-modules)))
        ;; (format #t "~A: ~A~%" (white "found entry") file)
    (if entry
        entry
        ;; else srch pkg-structs
        (let* ((structs-static (if-let ((ss (assoc-in '(:structures :static) pkg))) (cdr ss) '()))
               (structs-dynamic (if-let ((ss (assoc-in '(:structures :dynamic) pkg))) (cdr ss) '()))
               (pkg-structs (append structs-static structs-dynamic))
               (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs)))
               ;; (m-name (filename->module-name arg))
               ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "m-name") m-name)))
               (entry
                (find-if (lambda (struct)
                           ;; (format #t "~A: ~A~%" (white "mod") mod)
                           (equal? (format #f "~A" module)
                                   (format #f "~A" (car struct))))
                         pkg-structs)))
          entry))))

(define (-find-m-file-in-pkg-modules arg pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-modules") arg))
  (let* ((pkg-modules (if-let ((files (assoc-val :modules pkg)))
                                files '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-modules") pkg-modules)))
         (m-name (filename->module-name arg))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "m-name") m-name)))
         (file
          (find-if (lambda (mod)
                     ;; (format #t "~A: ~A~%" (white "mod") mod)
                     (equal? (format #f "~A" m-name)
                             (format #f "~A" (car mod))))
                   pkg-modules)))
        ;; (format #t "~A: ~A~%" (white "found file") file)
        file))

;; arg: filename
(define (-find-m-file-in-pkg-structs arg pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-structs") arg))
  (let* ((statics (if-let ((files (assoc-in '(:structures :static) pkg)))
                          (cdr files) '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "statics") statics)))
         (dynamics (if-let ((files (assoc-in '(:structures :dynamic) pkg)))
                           (cdr files) '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "dynamics") dynamics)))
         (structs (concatenate statics dynamics))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "structs") structs)))
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
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-sigs") arg))
  (let* ((pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                              files '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
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
  (if *debugging*
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
  (if *debugging*
      (format #t "~A: ~A~%" (ucyan "is-module-in-pkg") m))
  ;; (format #t "~A: ~A~%" (ucyan "pkg") pkg)
  (let* ((pkg-mods (if-let ((files (assoc-val :modules pkg)))
                             (map car files) '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-mods") pkg-mods)))
         (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                              (let* (;; (_ (if *debugging* (format #t "~A: ~A~%" (uyellow "pkg-structs") structs)))
                                     (statics (if-let ((statics (assoc-val :static structs)))
                                                      statics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "struct statics") statics)))
                                     (dynamics (if-let ((dynamics (assoc-val :dynamic structs)))
                                                       dynamics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "struct dynamics") dynamics)))
                                     ) ;; gets both :static & :dynamic
                                (map car (concatenate statics dynamics)))
                           '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs)))
         (pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                           (map car (car (map cdr files)))
                           '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
         (modules (concatenate pkg-mods pkg-structs pkg-sigs))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "all pkg modules") modules)))
         (answer (member m modules)))
    (if *debugging*
        (format #t "~A: ~A~%" (cyan "answer") answer))
    ;; (if (equal? 'Arg m) (error 'STOP "STOP mod in pkg"))
    answer))

;;FIXME: rename
;; may update deps
(define (find-file-in-pkg-files!? arg deps pkg)
  (if *debugging*
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
             (_ (if *debugging* (format #t "~A: ~A~%" (cyan "pkg-files") pkg-files)))
             (file
              (find-if (lambda (f)
                         (if *debugging*
                             (format #t "~A: ~A~%" (white "f") f))
                         (equal? (format #f "~A" arg)
                                 (format #f "~A" f)))
                       pkg-files)))
        (if *debugging*
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

(define (-codept-pkg-file-deps pkg-path)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-codept-pkg-file-deps") pkg-path))
  (let* ((fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
         (cmd (format #f "codept -k -expand-deps -sexp -I ~A ~A/*"
                      pkg-path pkg-path))
         (_ (if *debugging* (format #t "~A: ~A~%" (green "cmd") cmd))))
    (let ((deps (string-trim '(#\newline) (system cmd #t))))
      (if *debugging*
          (format #t "~A: ~A~%" (yellow "codept") deps)))
  ;;(set-cdr! struct (cdr struct))
    ))

;; run ocamldep against all static src files in pkg
;; iterate over output, updating :pkg-modules, :pkg-structures
;; this handles only static srcs, dynamics (ocamllex, ocamlyacc)
;; must be handled separately
(define (-ocamldep-pkg-static-file-deps! pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "-ocamldep-pkg-static-file-deps") (assoc-val :pkg-path pkg)))
          ;; (assoc-val :pkg-path pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
         (pkg-mods (if-let ((files (assoc-val :modules pkg)))
                             (map car files) '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-mods") pkg-mods)))
         (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                              (let* (;; (_ (if *debugging* (format #t "~A: ~A~%" (uyellow "pkg-structs") structs)))
                                     (statics (if-let ((statics (assoc-val :static structs)))
                                                      statics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "struct statics") statics)))
                                     (dynamics (if-let ((dynamics (assoc-val :dynamic structs)))
                                                       dynamics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "struct dynamics") dynamics)))
                                     ) ;; gets both :static & :dynamic
                                (map car (concatenate statics dynamics)))
                           '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs)))

         (pkg-sigs (if-let ((sigs (assoc-val :signatures pkg)))
                              (let* (;; (_ (if *debugging* (format #t "~A: ~A~%" (uyellow "pkg-sigs") sigs)))
                                     (statics (if-let ((statics (assoc-val :static sigs)))
                                                      statics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "sig statics") statics)))
                                     (dynamics (if-let ((dynamics (assoc-val :dynamic sigs)))
                                                       dynamics '()))
                                     ;; (_ (if *debugging* (format #t "~A: ~A~%" (cyan "sig dynamics") dynamics)))
                                     ) ;; gets both :static & :dynamic
                                (map car (concatenate statics dynamics)))
                           '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))
         ;; (pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
         ;;                   (map car files)
         ;;                   '()))
         ;; (_ (if *debugging* (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs)))

         (modules (concatenate pkg-mods pkg-structs pkg-sigs))
         (_ (if *debugging* (format #t "~A: ~A~%" (yellow "all pkg modules") modules)))
         )
    (let* ((cmd (format #f "ocamldep -one-line -modules -I ~A ~A/*"
                        pkg-path pkg-path))
           (_ (if *debugging* (format #t "~A: ~A~%" (green "cmd") cmd)))
           (deps (string-trim '(#\newline) (system cmd #t))) ;; EXECUTE CMD
           (deps (string-split deps #\newline)))
      (for-each (lambda (dep)
                  (if *debugging*
                      (format #t "~%~A: ~A~%" (uyellow "iter over ocamldeps") dep))
                  (let ((segs (string-split dep #\:)))
                    (if *debugging*
                        (format #t "~A: ~A~%" (yellow "segs") segs))
                    (if (null? (cdr segs))
                        (begin)
                        (let* ((fpath (car segs))
                               (fname (basename fpath))
                               (mname (filename->module-name fname))
                               (kind (filename->kind fname))
                               (mdeps (string-trim '(#\space) (cadr segs)))
                               (mdeps (string-split mdeps #\space))
                               (mdeps (map string->symbol mdeps))

                               (_ (if *debugging* (format #t "~A for ~A: ~A~%" (ugreen "unfiltered mdeps") mname mdeps)))
                               ;; do not include file module in deps list
                               (mdeps (remove mname mdeps))
                               (_ (if *debugging* (format #t "~A: ~A~%" (ugreen "unfiltered mdeps excluding self") mdeps)))
                               ;; eliminate mdeps not in this pkg
                               ;; (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                               (mdeps (filter (lambda (dep) (member dep modules)) mdeps))
                               (_ (if *debugging* (format #t "~A: ~A~%" (ugreen "filtered mdeps, excluding pkg-externals") mdeps)))
                               )
                          (if *debugging*
                              (begin
                                (format #t "~A: ~A~%" (red "mdeps") mdeps)
                                (format #t "~A: ~A~%" (red "pkg (before)") pkg)
                                (format #t "~A: ~A~%" (red "fname") fname)
                                (format #t "~A: ~A~%" (red "mname") mname)))
                          ;; (if (string=? "arg.ml" fname) (error 'STOP "STOP ocamldep"))
                          (if (truthy? mdeps) ;; (not (null? mdeps))
                              (begin
                                (if *debugging*
                                    (format #t "~A ~A to ~A~%" (bgyellow "updating stanza :deps") mdeps fname))
                                ;; (format #t "~A: ~A~%" (uyellow "in pkg") pkg)
                                (if (assoc-val :dune pkg)
                                    (update-stanza-deps pkg fname mdeps))
                                (if *debugging*
                                    (format #t "~A: ~A~%" (red "pkg (after)") pkg))
                                ))

                          ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
                          ;; we retrieve the pkg-dep for fname and add the mdeps to it
                          ;; (format #t "~A: ~A~%" (bgyellow "updating pkg file flds") pkg)
                          ;; (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                          ;; (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                          ;; (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
                          (if (truthy? mdeps) ;; (not (null? mdeps))
                              (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                      (begin
                                        (if *debugging*
                                            (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc))
                                        (if (proper-list? m-assoc)
                                            ;; its a module entry, form (A (:ml a.ml) (:mli a.mli))
                                            (begin ;; if mdeps not empty
                                              (set-cdr! m-assoc
                                                        (append (cdr m-assoc)
                                                                (list (cons
                                                                       (if (eq? kind :struct)
                                                                           :ml-deps :mli-deps)
                                                                       mdeps))))
                                              ;; (format #t "~A: ~A~%" (bgred "m-assoc after") m-assoc)
                                              )
                                            ;; else its a struct entry, (A a.ml)
                                            (begin
                                              (if *debugging*
                                                  (begin
                                                    (format #t "~A: ~A~%" (bgred "STRUCT ENTRY") m-assoc)
                                                    (format #t "~A: ~A~%" (bgred "adding mdeps") mdeps)))
                                              (if (not (null? mdeps))
                                                  (set-cdr! m-assoc
                                                            (cons (cdr m-assoc)
                                                                  mdeps))))))
                                      ;;else
                                      (format #t "~A: ~A~%" (blue "not found") m-assoc))
                              ;; else mdeps is null
                              (if *debugging*
                                  (format #t "~A~%" (uyellow "continue")))
                              )))))
                deps))))

(define (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "-ocamlyacc-deps!") pkg-mly-modules))
  (for-each (lambda (mly)
              (if *debugging*
                  (format #t "~A: ~A~%" (blue "mly module") mly))
              (let* ((mly-src (assoc-val :mly (cdr mly)))
                     (principal (format #f "~A" (bname mly-src)))
                     (cp-cmd (format #f "cp -v ~A/~A ~A" pkg-path mly-src *tmp-dir*)))
                (if *debugging*
                    (begin
                      (format #t "~A: ~A~%" (blue "mly-src") mly-src)
                      (format #t "~A: ~A~%" (blue "cp-cmd") cp-cmd)))
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((yacc-cmd
                       (format #f "ocamlyacc ~A/~A" *tmp-dir* mly-src)))
                  (if *debugging*
                      (format #t "~A: ~A~%" (blue "yacc-cmd") yacc-cmd))
                  (system yacc-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "ocamldep -one-line -modules -I ~A ~A/~A.*"
                           pkg-path *tmp-dir* principal))
                         (deps (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (file-deps (string-split deps #\newline)))
                    (if *debugging*
                        (begin
                          (format #t "~A: ~A~%" (blue "ocamldep-cmd") ocamldep-cmd)
                          (format #t "~A: ~A~%" (blue "file-deps") file-deps)))

                    ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
                    ;;         (assoc :modules pkg))
                    ;; (error 'stop "STOP yacc")

                    (for-each (lambda (file-dep)
                                (if *debugging*
                                    (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep))
                                (let ((segs (string-split file-dep #\:)))
                                  ;; (format #t "~A: ~A~%" (yellow "segs") segs)
                                  (if (null? (cdr segs))
                                      (begin)
                                      (let* ((fpath (car segs))
                                             (fname (basename fpath))
                                             (kind (filename->kind fname))
                                             (mdeps (string-trim '(#\space) (cadr segs)))
                                             (mdeps (string-split mdeps #\space))
                                             (mdeps (map string->symbol mdeps))
                                             ;; eliminate mdeps not in this pkg
                                             (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                                             )
                                        (if *debugging*
                                            (begin
                                              (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                              (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                              (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)))
                                        (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                                (begin
                                                  (if *debugging*
                                                      (format #t "~A: ~A~%" (blue "m-assoc before") m-assoc))
                                                  (if (proper-list? m-assoc)
                                                      ;; its a module entry, (A (:ml a.ml) (:mli a.mli))
                                                      (begin
                                                        ;; (format #t "~A: ~A~%" (bgred "FOO") 99)
                                                        (set-cdr! m-assoc
                                                                  (append (cdr m-assoc)
                                                                          (list (cons
                                                                                 (if (eq? kind :struct)
                                                                                     :ml-deps :mli-deps)
                                                                                 mdeps))))
                                                        (if *debugging*
                                                            (format #t "~A: ~A~%" (blue "m-assoc after") m-assoc)))
                                                      ;; else its a struct entry, (A a.ml)
                                                      (begin
                                                        (if *debugging*
                                                            (format #t "~A: ~A~%" (bgred "updating w/struct") m-assoc))
                                                        (set-cdr! m-assoc
                                                                  (cons (cdr m-assoc)
                                                                        mdeps))))
                                                  )
                                                ;;else - should not happen
                                                (error 'stop "STOP yacc")
                                                ;; (update-pkg-modules-with-module! pkg principal-name)
                                                )))))
                              file-deps)
                    ))))
            pkg-mly-modules))

(define (-ocamllex-deps! pkg pkg-path pkg-mll-modules)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "-ocamllex-deps") pkg-mll-modules))
  (for-each (lambda (mll)
              (if *debugging*
                  (format #t "~A: ~A~%" (blue "mll module") mll))
              (let* ((mll-src (cdr mll))
                     (ml-src (format #f "~A.ml" (bname mll-src)))
                     ;;FIXME: if verbose, add '-v' to cmd
                     (cp-cmd (format #f "cp ~A/~A ~A" pkg-path mll-src *tmp-dir*)))
                (if *debugging*
                    (begin
                      (format #t "~A: ~A~%" (blue "mll-src") mll-src)
                      (format #t "~A: ~A~%" (blue "cp-cmd") cp-cmd)))
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((lex-cmd
                       (format #f "ocamllex -q ~A/~A" *tmp-dir* mll-src)))
                  (if *debugging*
                      (format #t "~A: ~A~%" (blue "lex-cmd") lex-cmd))
                  (system lex-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "ocamldep -one-line -modules -I ~A ~A/~A"
                           pkg-path *tmp-dir* ml-src))
                         (deps (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (file-deps (string-split deps #\newline)))
                    (if *debugging*
                        (begin
                          (format #t "~A: ~A~%" (blue "ocamldep-cmd") ocamldep-cmd)
                          (format #t "~A: ~A~%" (blue "file-deps") file-deps)))

                    ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
                    ;;         (assoc :modules pkg))
                    ;; (error 'stop "STOP lex")

                    (for-each (lambda (file-dep)
                                (if *debugging*
                                    (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep))
                                (let ((segs (string-split file-dep #\:)))
                                  ;; (format #t "~A: ~A~%" (yellow "segs") segs)
                                  (if (null? (cdr segs))
                                      (begin)
                                      (let* ((fpath (car segs))
                                             (fname (basename fpath))
                                             (kind (filename->kind fname))
                                             (mdeps (string-trim '(#\space) (cadr segs)))
                                             (mdeps (string-split mdeps #\space))
                                             (mdeps (map string->symbol mdeps))
                                             ;; eliminate mdeps not in this pkg
                                             (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                                             )
                                        (if *debugging*
                                            (begin
                                              (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                              (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                              (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)))
                                        (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                                (begin
                                                  (if *debugging*
                                                      (format #t "~A: ~A~%" (blue "m-assoc before") m-assoc))
                                                  (if (proper-list? m-assoc)
                                                      ;; its a module entry, (A (:ml a.ml) (:mli a.mli))
                                                      (begin
                                                        ;; (format #t "~A: ~A~%" (bgred "FOO") 99)
                                                        (set-cdr! m-assoc
                                                                  (append (cdr m-assoc)
                                                                          (list (cons
                                                                                 (if (eq? kind :struct)
                                                                                     :ml-deps :mli-deps)
                                                                                 mdeps))))
                                                        (if *debugging*
                                                            (format #t "~A: ~A~%" (blue "m-assoc after") m-assoc)))
                                                      ;; else its a struct entry, (A a.ml)
                                                      (begin
                                                        (if *debugging*
                                                            (format #t "~A: ~A~%" (bgred "updating w/struct entry") m-assoc))
                                                        (set-cdr! m-assoc
                                                                  (cons (cdr m-assoc)
                                                                        mdeps))))
                                                  )
                                                ;;else - should not happen
                                                (error 'stop
                                                       (format #f "not found in pkg files: ~A" fname))
                                                ;; (update-pkg-modules-with-module! pkg principal-name)
                                                )))))
                              file-deps)
                    ))))
            pkg-mll-modules))

;; handle :ocamllex, :ocamlyacc, etc.
(define (-ocamldep-pkg-dynamic-file-deps! pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "-ocamldep-pkg-dynamic-file-deps") (assoc-val :pkg-path pkg)))
          ;; (assoc-val :pkg-path pkg))
  (let* ((ws-path (car (assoc-val :ws-path pkg)))
         (pkg-path (car (assoc-val :pkg-path pkg)))
         (pkg-modules (if-let ((modules (assoc-val :modules pkg)))
                              modules '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-modules") pkg-modules)))

         (pkg-static-mll (if-let ((mll-static
                                   (assoc-in '(:ocamllex :static) pkg)))
                                 (cdr mll-static) '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-static-mll") pkg-static-mll)))
         ;; prolly won't have dynamic .mll files, but just in case
         (pkg-dynamic-mll (if-let ((mll-dyn (assoc-in '(:ocamllex :dynamic) pkg)))
                                  (cdr mll-dyn) '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-dynamic-mll") pkg-dynamic-mll)))

         (pkg-mll (concatenate pkg-static-mll pkg-dynamic-mll))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-mll") pkg-mll)))

         (pkg-static-mly (if-let ((mly-static
                                   (assoc-in '(:ocamlyacc :static) pkg)))
                                 mly-static '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-static-mly") pkg-static-mly)))
         (pkg-dynamic-mly (if-let ((mly-dyn
                                    (assoc-in '(:ocamlyacc :dynamic) pkg)))
                                  mly-dyn '()))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-dynamic-mly") pkg-dynamic-mly)))
         (pkg-mly (concatenate pkg-static-mly pkg-dynamic-mly))
         (_ (if *debugging* (format #t "~A: ~A~%" (blue "pkg-mly") pkg-mly)))

         ;; filtered :modules
         (pkg-mly-modules (filter (lambda (m)
                                    ;; (format #t "~A: ~A~%" (blue "m") m)
                                    (assoc :mly (cdr m))) pkg-modules))
         )
    (if *debugging*
        (begin
          (format #t "~A: ~A~%" (blue "filtered mly") pkg-mly-modules)
          (format #t "~A: ~A~%" (blue "ws-path") ws-path)
          (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
          (format #t "~A: ~A~%" (blue "pwd") (pwd))
          (format #t "~A: ~A~%" (blue "tmp-dir") *tmp-dir*)))

    ;; 1. copy mly file to tmp dir (ensure tmp dir empty first?)
    ;; 2. run ocamlyacc on it
    ;; 3. run ocamldep on the generated .ml and .mli files
    ;; 4. update :pkg-modules entries

    (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)

    ;; ditto for ocamllex
    ;; (set! *debugging* #t)
    (-ocamllex-deps! pkg pkg-path pkg-mll)
    ;; (set! *debugging* #f)
    ))

(define (-resolve-pkg-struct-deps pkg-path pkg-structs)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-resolve-pkg-struct-deps") pkg-structs))
  (let* ((statics (if-let ((statics (assoc-val :static pkg-structs)))
                          statics '()))
         (dynamics (if-let ((dynamics (assoc-val :dynamics pkg-structs)))
                           dynamics '()))
         (structs (concatenate statics dynamics)))
    ;; (format #t "~A: ~A~%" (green "statics") statics)
    ;; (format #t "~A: ~A~%" (green "dynamics") dynamics)
    ;; (format #t "~A: ~A~%" (green "structs") structs)
    (for-each (lambda (struct)
                ;; (format #t "~A: ~A~%" (green "s") struct)
                (let* ((fname (cdr struct))
                       (fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
                       (cmd (format #f "codept -k -expand-deps -sexp -I ~A ~A"
                                    pkg-path fpath))
                       ;; (cmd (format #f "ocamldep -one-line -modules ~A" fpath))
                       (_ (if *debugging* (format #t "~A: ~A~%" (green "cmd") cmd)))
                       (deps (string-trim '(#\newline) (system cmd #t))))
                  ;;(format #t "~A: ~A~%" (yellow "ocamldeps") deps)
                  )
                  ;;(set-cdr! struct (cdr struct))
                  )
              structs)))

;; resolve-pkg-file-deps
;; main task: run ocamldep on each (static) file in the pkg,
;; filter results to exclude anything not in this pkg
;; (since any other deps will be resolved via the exports tbl or opam)
;; then update the pkg file entries and also stanza :deps flds

;; for each pkg file (module, sig, struct),
;;   find the stanza that depends on it
;;   if depending stanza is namespaced aggregate, continue (?)
;;   else ocamldep and update the pkg fld with :deps
;;   if stanza is executable?
;; skip :ocamllex, :ocamlyacc, they're already elaborated
;; add flds (:ml-deps), (:mli-deps)
(define (resolve-pkg-file-deps ws)
  (if *debugging*
      (format #t "~%~A: ~A~%" (bgred "resolve-pkg-file-deps for ws") ws))
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    (if *debugging*
        (format #t "~A: ~A~%" (uwhite "pkgs") pkgs))
    (for-each (lambda (pkg-kv)
                (if *debugging*
                    (format #t "~A: ~A~%" (uwhite "for pkg") (car pkg-kv)))
                (let* ((pkg-path (car pkg-kv))
                       (_ (if *debugging* (format #t "~A: ~A~%" (green "pkg-path") pkg-path)))
                       (pkg (cdr pkg-kv))
                       ;; (_ (if *debugging* (format #t "~A: ~A~%" (green "pkg") pkg)))
                       (pkg-modules (if-let ((ms (assoc-val :modules pkg)))
                                            ms '()))
                       (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                                            structs '()))
                       (pkg-sigs (if-let ((sigs (assoc-val :signatures pkg)))
                                         (car sigs) '())))
                  ;; (format #t "~A: ~A~%" (green "pkg-modules") pkg-modules)
                  ;; (format #t "~A: ~A~%" (green "pkg-structs") pkg-structs)
                  ;; (format #t "~A: ~A~%" (green "pkg-sigs") pkg-sigs)

                  ;; (-resolve-pkg-struct-deps pkg-path pkg-structs)))
                  ;; (-resolve-pkg-sig-deps pkg-sigs)
                  ;; (-codept-pkg-file-deps pkg-path)))

                  (if pkg
                      (begin
                        (-ocamldep-pkg-static-file-deps! pkg)
                        ;; (format #t "~A: ~A~%" (bgred "pkg after sfd") pkg)
                        (-ocamldep-pkg-dynamic-file-deps! pkg)
                        ;; (format #t "~A: ~A~%" (bgred "pkg after dfd") pkg)
                        ;;FIXME: handle sigs
                        )
                      (error 'STOP
                             (format #f "STOP ~A" pkgs)))
                  ))
              ;; now delete any files generated by ocamllex or ocamlyacc
              ;; (for-each (lambda (stanza)
              ;;             (if (equal? :ocamlyacc (car stanza))
              ;;                 (for-each (lambda (yacc)
              ;;                             ;; delete the .ml, .mli files
              ;;                             )
              ;;                           (cdr stanza))
              ;;                 ))
              ;;           (assoc-val :dune pkg))
              pkgs)))
