;; arg:  filename
;; returns pair from :modules e.g. (:ml foo.ml) or (:ml_ foo.ml)
;; or (:mli foo.mli) or (:mli_ foo.mli)
(define (-find-m-file-in-pkg-modules arg pkg)
  ;; (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-modules") arg)
  (let* ((pkg-modules (if-let ((files (assoc-val :modules pkg)))
                                files '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "pkg-modules") pkg-modules))
         (m-name (filename->module-name arg))
         ;; (_ (format #t "~A: ~A~%" (yellow "m-name") m-name))
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
  (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-structs") arg)
  (let* ((statics (if-let ((files (assoc-in '(:structures :static) pkg)))
                          (cdr files) '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "statics") statics))
         (dynamics (if-let ((files (assoc-in '(:structures :dynamic) pkg)))
                           (cdr files) '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "dynamics") dynamics))
         (structs (concatenate statics dynamics))
         ;; (_ (format #t "~A: ~A~%" (yellow "structs") structs))
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
  ;; (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg-sigs") arg)
  (let* ((pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                              files '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs))
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
  ;; (format #t "~A: ~A~%" (ublue "find-m-file-in-pkg") arg)
  ;; (format #t "~A: ~A~%" (blue ":files") (assoc-val :files pkg))
  ;; (format #t "~A: ~A~%" (blue "deps") deps)
  ;; (format #t "~A: ~A~%" (red ":scripts") (assoc-val :scripts pkg))

  (if-let ((m (-find-m-file-in-pkg-modules arg pkg)))
          m
          (if-let ((m (-find-m-file-in-pkg-structs arg pkg)))
                  m
                  (if-let ((m (-find-m-file-in-pkg-sigs arg pkg)))
                          m))))

(define (is-module-in-pkg m pkg)
  ;; (format #t "~A: ~A~%" (ucyan "is-module-in-pkg") m)
  ;; (format #t "~A: ~A~%" (ucyan "pkg") pkg)
  (let* ((pkg-mods (if-let ((files (assoc-val :modules pkg)))
                             (map car files) '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "pkg-mods") pkg-mods))
         (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                              (let* (;; (_ (format #t "~A: ~A~%" (uyellow "pkg-structs") structs))
                                     (statics (if-let ((statics (assoc-val :static structs)))
                                                      statics '()))
                                     ;; (_ (format #t "~A: ~A~%" (cyan "struct statics") statics))
                                     (dynamics (if-let ((dynamics (assoc-val :dynamic structs)))
                                                       dynamics '()))
                                     ;; (_ (format #t "~A: ~A~%" (cyan "struct dynamics") dynamics))
                                     ) ;; gets both :static & :dynamic
                                (map car (concatenate statics dynamics)))
                           '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "pkg-structs") pkg-structs))
         (pkg-sigs (if-let ((files (assoc-val :signatures pkg)))
                           (map car (car (map cdr files)))
                           '()))
         ;; (_ (format #t "~A: ~A~%" (yellow "pkg-sigs") pkg-sigs))
         (modules (concatenate pkg-mods pkg-structs pkg-sigs))
         (_ (format #t "~A: ~A~%" (yellow "all pkg modules") modules))
         (_ (format #t "~A: ~A~%" (yellow "all pkg modules X") modules))
         (answer (member m modules)))
    (format #t "~A: ~A~%" (cyan "answer") answer)
    ;; (if (equal? 'Arg m) (error 'STOP "STOP mod in pkg"))
    answer))

;;FIXME: rename
;; may update deps
(define (find-file-in-pkg-files!? arg deps pkg)
  (format #t "~A: ~A~%" (ublue "find-file-in-pkg-files") arg)
  (format #t "~A: ~A~%" (blue ":files") (assoc-val :files pkg))
  (format #t "~A: ~A~%" (blue "deps") deps)
  ;; (format #t "~A: ~A~%" (red ":scripts") (assoc-val :scripts pkg))

  (if (assoc-val :files pkg)
      (let* ((pkg-files (if-let ((files (assoc-val :files pkg)))
                                files '()))
             (pkg-files (append
                         (if-let ((statics (assoc-val :static pkg-files)))
                                 statics '())
                         (if-let ((dynamics (assoc-val :dynamic pkg-files)))
                                 dynamics '())))
             (_ (format #t "~A: ~A~%" (cyan "pkg-files") pkg-files))
             (file
              (find-if (lambda (f)
                         (format #t "~A: ~A~%" (white "f") f)
                         (equal? (format #f "~A" arg)
                                 (format #f "~A" f)))
                       pkg-files)))
        (format #t "~A: ~A~%" (white "found file") file)
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
  (format #t "~A: ~A~%" (blue "-codept-pkg-file-deps") pkg-path)
  (let* ((fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
         (cmd (format #f "codept -k -expand-deps -sexp -I ~A ~A/*"
                      pkg-path pkg-path))
         (_ (format #t "~A: ~A~%" (green "cmd") cmd)))
    (let ((deps (string-trim '(#\newline) (system cmd #t))))
      (format #t "~A: ~A~%" (yellow "codept") deps))
  ;;(set-cdr! struct (cdr struct))
    ))

;; run ocamldep against all static src files in pkg
;; iterate over output, updating :pkg-modules, :pkg-structures
;; this handles only static srcs, dynamics (ocamllex, ocamlyacc)
;; must be handled separately
(define (-ocamldep-pkg-static-file-deps! pkg)
  (format #t "~A: ~A~%" (bgblue "-ocamldep-pkg-static-file-deps") pkg)
          ;; (assoc-val :pkg-path pkg))
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (fpath (format #f "~A/*" pkg-path)) ;; (cdr struct)))
         (cmd (format #f "ocamldep -one-line -modules -I ~A ~A/*"
                      pkg-path pkg-path))
         (_ (format #t "~A: ~A~%" (green "cmd") cmd))
         )
    (let* ((deps (string-trim '(#\newline) (system cmd #t)))
           (deps (string-split deps #\newline)))
      (for-each (lambda (dep)
                  (format #t "~A: ~A~%" (bgyellow "processing ocamldep") dep)
                  (let ((segs (string-split dep #\:)))
                    (format #t "~A: ~A~%" (yellow "segs") segs)
                    (if (null? (cdr segs))
                        (begin)
                        (let* ((fpath (car segs))
                               (fname (basename fpath))
                               (mname (filename->module-name fname))
                               (kind (filename->kind fname))
                               (mdeps (string-trim '(#\space) (cadr segs)))
                               (mdeps (string-split mdeps #\space))
                               (mdeps (map string->symbol mdeps))
                               ;; do not include file module in deps list
                               (mdeps (remove mname mdeps))
                               ;; eliminate mdeps not in this pkg
                               (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
                               )
                          (format #t "~A: ~A~%" (red "mdeps") mdeps)
                          (format #t "~A: ~A~%" (red "pkg") pkg)
                          (format #t "~A: ~A~%" (red "fname") fname)
                          (format #t "~A: ~A~%" (red "mname") mname)
                          ;; (if (string=? "arg.ml" fname) (error 'STOP "STOP ocamldep"))
                          (if (not (null? mdeps))
                              (begin
                                (format #t "~A ~A to ~A~%" (bgyellow "adding mdeps") mdeps fname)
                                (format #t "~A: ~A~%" (uyellow "in pkg") pkg)
                                (update-stanza-deps pkg fname mdeps)
                                ))

                          ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
                          ;; we retrieve the pkg-dep for fname and add the mdeps to it
                          (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                          (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                          (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
                          (if (not (null? mdeps))
                              (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                      (begin
                                        (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc)
                                        (if (proper-list? m-assoc)
                                            ;; its a module entry, (A (:ml a.ml) (:mli a.mli))
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
                                              (format #t "~A: ~A~%" (bgred "STRUCT ENTRY") m-assoc)
                                              (format #t "~A: ~A~%" (bgred "adding mdeps") mdeps)
                                              (if (not (null? mdeps))
                                                  (set-cdr! m-assoc
                                                            (cons (cdr m-assoc)
                                                                  mdeps))))))
                                      ;;else
                                      (format #t "~A: ~A~%" (blue "not found") m-assoc))
                              ;; else mdeps is null
                              )))))
                deps))))

(define (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)
  (format #t "~A: ~A~%" (ublue "-ocamlyacc-deps!") pkg-mly-modules)
  (for-each (lambda (mly)
              (format #t "~A: ~A~%" (blue "mly module") mly)
              (let* ((mly-src (assoc-val :mly (cdr mly)))
                     (principal (format #f "~A" (bname mly-src)))
                     (cp-cmd (format #f "cp -v ~A/~A ~A" pkg-path mly-src *tmp-dir*)))
                (format #t "~A: ~A~%" (blue "mly-src") mly-src)
                (format #t "~A: ~A~%" (blue "cp-cmd") cp-cmd)
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((yacc-cmd
                       (format #f "ocamlyacc ~A/~A" *tmp-dir* mly-src)))
                  (format #t "~A: ~A~%" (blue "yacc-cmd") yacc-cmd)
                  (system yacc-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "ocamldep -one-line -modules -I ~A ~A/~A.*"
                           pkg-path *tmp-dir* principal))
                         (deps (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (file-deps (string-split deps #\newline)))
                    (format #t "~A: ~A~%" (blue "ocamldep-cmd") ocamldep-cmd)
                    (format #t "~A: ~A~%" (blue "file-deps") file-deps)

                    ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
                    ;;         (assoc :modules pkg))
                    ;; (error 'stop "STOP yacc")

                    (for-each (lambda (file-dep)
                                (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep)
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
                                        (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                        (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                        (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
                                        (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                                (begin
                                                  (format #t "~A: ~A~%" (blue "m-assoc before") m-assoc)
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
                                                        (format #t "~A: ~A~%" (blue "m-assoc after") m-assoc))
                                                      ;; else its a struct entry, (A a.ml)
                                                      (begin
                                                        (format #t "~A: ~A~%" (bgred "updating w/struct") m-assoc)
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
  (format #t "~A: ~A~%" (ublue "-ocamllex-deps") pkg-mll-modules)
  (for-each (lambda (mll)
              (format #t "~A: ~A~%" (blue "mll module") mll)
              (let* ((mll-src (cdr mll))
                     (ml-src (format #f "~A.ml" (bname mll-src)))
                     (cp-cmd (format #f "cp -v ~A/~A ~A" pkg-path mll-src *tmp-dir*)))
                (format #t "~A: ~A~%" (blue "mll-src") mll-src)
                (format #t "~A: ~A~%" (blue "cp-cmd") cp-cmd)
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((lex-cmd
                       (format #f "ocamllex ~A/~A" *tmp-dir* mll-src)))
                  (format #t "~A: ~A~%" (blue "lex-cmd") lex-cmd)
                  (system lex-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "ocamldep -one-line -modules -I ~A ~A/~A"
                           pkg-path *tmp-dir* ml-src))
                         (deps (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (file-deps (string-split deps #\newline)))
                    (format #t "~A: ~A~%" (blue "ocamldep-cmd") ocamldep-cmd)
                    (format #t "~A: ~A~%" (blue "file-deps") file-deps)

                    ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
                    ;;         (assoc :modules pkg))
                    ;; (error 'stop "STOP lex")

                    (for-each (lambda (file-dep)
                                (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep)
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
                                        (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
                                        (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
                                        (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)
                                        (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
                                                (begin
                                                  (format #t "~A: ~A~%" (blue "m-assoc before") m-assoc)
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
                                                        (format #t "~A: ~A~%" (blue "m-assoc after") m-assoc))
                                                      ;; else its a struct entry, (A a.ml)
                                                      (begin
                                                        (format #t "~A: ~A~%" (bgred "updating w/struct entry") m-assoc)
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
  (format #t "~A: ~A~%" (bgblue "-ocamldep-pkg-dynamic-file-deps") pkg)
          ;; (assoc-val :pkg-path pkg))
  (let* ((ws-path (car (assoc-val :ws-path pkg)))
         (pkg-path (car (assoc-val :pkg-path pkg)))
         (pkg-modules (if-let ((modules (assoc-val :modules pkg)))
                              modules '()))
         (_ (format #t "~A: ~A~%" (blue "pkg-modules") pkg-modules))

         (pkg-static-mll (if-let ((mll-static
                                   (assoc-in '(:ocamllex :static) pkg)))
                                 (cdr mll-static) '()))
         (_ (format #t "~A: ~A~%" (blue "pkg-static-mll") pkg-static-mll))
         ;; prolly won't have dynamic .mll files, but just in case
         (pkg-dynamic-mll (if-let ((mll-dyn (assoc-in '(:ocamllex :dynamic) pkg)))
                                  (cdr mll-dyn) '()))
         (_ (format #t "~A: ~A~%" (blue "pkg-dynamic-mll") pkg-dynamic-mll))

         (pkg-mll (concatenate pkg-static-mll pkg-dynamic-mll))
         (_ (format #t "~A: ~A~%" (blue "pkg-mll") pkg-mll))

         (pkg-static-mly (if-let ((mly-static
                                   (assoc-in '(:ocamlyacc :static) pkg)))
                                 mly-static '()))
         (_ (format #t "~A: ~A~%" (blue "pkg-static-mly") pkg-static-mly))
         (pkg-dynamic-mly (if-let ((mly-dyn
                                    (assoc-in '(:ocamlyacc :dynamic) pkg)))
                                  mly-dyn '()))
         (_ (format #t "~A: ~A~%" (blue "pkg-dynamic-mly") pkg-dynamic-mly))
         (pkg-mly (concatenate pkg-static-mly pkg-dynamic-mly))
         (_ (format #t "~A: ~A~%" (blue "pkg-mly") pkg-mly))

         ;; filtered :modules
         (pkg-mly-modules (filter (lambda (m)
                                    ;; (format #t "~A: ~A~%" (blue "m") m)
                                    (assoc :mly (cdr m))) pkg-modules))
         )
    (format #t "~A: ~A~%" (blue "filtered mly") pkg-mly-modules)
    (format #t "~A: ~A~%" (blue "ws-path") ws-path)
    (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
    (format #t "~A: ~A~%" (blue "pwd") (pwd))
    (format #t "~A: ~A~%" (blue "tmp-dir") *tmp-dir*)

    ;; 1. copy mly file to tmp dir (ensure tmp dir empty first?)
    ;; 2. run ocamlyacc on it
    ;; 3. run ocamldep on the generated .ml and .mli files
    ;; 4. update :pkg-modules entries

    (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)

    ;; ditto for ocamllex
    (-ocamllex-deps! pkg pkg-path pkg-mll)
    ))

(define (-resolve-pkg-struct-deps pkg-path pkg-structs)
  (format #t "~A: ~A~%" (blue "-resolve-pkg-struct-deps") pkg-structs)
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
                       (_ (format #t "~A: ~A~%" (green "cmd") cmd))
                       (deps (string-trim '(#\newline) (system cmd #t))))
                  ;;(format #t "~A: ~A~%" (yellow "ocamldeps") deps)
                  )
                  ;;(set-cdr! struct (cdr struct))
                  )
              structs)))

;; for each pkg file (module, sig, struct),
;;   find the stanza that depends on it
;;   if depending stanza is namespaced, continue
;;   else ocamldep and update the pkg fld with :deps
;; skip :ocamllex, :ocamlyacc, they're already elaborated
;; add flds (:ml-deps), (:mli-deps)
(define (resolve-pkg-file-deps ws)
  (format #t "~A: ~A~%" (bgred "resolve-pkg-file-deps") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))
    (format #t "~A: ~A~%" (uwhite "pkgs") pkgs)
    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (uwhite "for pkg") kv)
                (let* ((pkg-path (car kv))
                       (_ (format #t "~A: ~A~%" (green "pkg-path") pkg-path))
                       (pkg (cdr kv))
                       (_ (format #t "~A: ~A~%" (green "pkg") pkg))
                       (pkg-modules (if-let ((ms (assoc-val :modules pkg)))
                                            ms '()))
                       (pkg-structs (if-let ((structs (assoc-val :structures pkg)))
                                            structs '()))
                       (pkg-sigs (if-let ((sigs (assoc-val :signatures pkg)))
                                         (car sigs) '())))
                  (format #t "~A: ~A~%" (green "pkg-modules") pkg-modules)
                  (format #t "~A: ~A~%" (green "pkg-structs") pkg-structs)
                  (format #t "~A: ~A~%" (green "pkg-sigs") pkg-sigs)

                  ;; (-resolve-pkg-struct-deps pkg-path pkg-structs)))
                  ;; (-resolve-pkg-sig-deps pkg-sigs)
                  ;; (-codept-pkg-file-deps pkg-path)))

                  (if pkg
                      (begin
                        (-ocamldep-pkg-static-file-deps! pkg)
                        (format #t "~A: ~A~%" (bgred "pkg after sfd") pkg)
                        (-ocamldep-pkg-dynamic-file-deps! pkg)
                        (format #t "~A: ~A~%" (bgred "pkg after dfd") pkg))
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
