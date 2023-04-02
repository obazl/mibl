;; WARNING: runs ocamlyacc, ocamllex, codept

;; example of output of codept:
;; ((version ( 0 11 0 ))
 ;; (dependencies  ;; list of alists
 ;;  (((file /tmp/obazl.lkeefjCgKn/lexer.ml)
 ;;    (deps
 ;;     ((Stdlib Printf) (Stdlib Lexing) (Stdlib Char) (Stdlib Buffer) (Path)
 ;;      (Parser_with_layout) (Parser))))))
;;  (local
;;   (((module (Lexer))
;;     (ml /tmp/obazl.lkeefjCgKn/lexer.ml))))
;;  (unknown
;;   ((Parser) (Parser_with_layout) (Path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: do this in load_project c code
;; (define (yacc-deps pkg principal-name)
;;   (if *mibl-debug-s7*
;;       (format #t "~A: ~A~%" (ublue "yacc-deps") principal-name))
;;   (let ((ws-path (assoc-val :ws-path pkg))
;;         (pkg-path (assoc-val :pkg-path pkg))
;;         (mly-src (format #f "~A.mly" principal-name)))
;;     (if *mibl-debug-s7*
;;         (begin
;;           (format #t "~A: ~A~%" (ublue "ws-path") ws-path)
;;           (format #t "~A: ~A~%" (ublue "pkg-path") pkg-path)
;;           (format #t "~A: ~A~%" (ublue "pwd") (pwd))
;;           (format #t "~A: ~A~%" (ublue "tmp-dir") *mibl-tmp-dir*)))

;;     (system (format #f "cp -v ~A/~A ~A" pkg-path mly-src *mibl-tmp-dir*))
;;     (let ((yacc-cmd
;;            (format #f "ocamlyacc ~A/~A" *mibl-tmp-dir* mly-src)))
;;       (if *mibl-debug-s7*
;;           (format #t "~A: ~A~%" (red "yacc-cmd") yacc-cmd))
;;       (system yacc-cmd)
;;       (let* ((ocamldep-cmd
;;               (format
;;                ;; #f "ocamldep -one-line -modules -I ~A ~A/*"
;;                #f "codept -verbosity info -sexp -k"
;;                pkg-path *mibl-tmp-dir*))
;;              (deps (string-trim '(#\newline) (system ocamldep-cmd #t)))
;;              (file-deps (string-split deps #\newline)))
;;         (if *mibl-debug-s7*
;;             (begin
;;               (format #t "~A: ~A~%" (red "ocamldep-cmd") ocamldep-cmd)
;;               (format #t "~A: ~A~%" (bgblue "file-deps 3") file-deps)))

;;         ;; (format #t "~A: ~A~%" (bgred "pkg-modules")
;;         ;;         (assoc :modules pkg))
;;         ;; (error 'stop "STOP yacc")

;;         (for-each (lambda (file-dep)
;;                     (if *mibl-debug-s7*
;;                         (format #t "~A: ~A~%" (bgyellow "ocamldep") file-dep))
;;                     (let ((segs (string-split file-dep #\:)))
;;                       ;; (format #t "~A: ~A~%" (yellow "segs") segs)
;;                       (if (null? (cdr segs))
;;                           (begin)
;;                           (let* ((fpath (car segs))
;;                                  (fname (basename fpath))
;;                                  (kind (filename->kind fname))
;;                                  (mdeps (string-trim '(#\space) (cadr segs)))
;;                                  (mdeps (string-split mdeps #\space))
;;                                  (mdeps (map string->symbol mdeps))
;;                                  ;; eliminate mdeps not in this pkg
;;                                  (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))
;;                                  )
;;                             (if *mibl-debug-s7*
;;                                 (begin
;;                                   (format #t "~A: ~A~%" (yellow "ocamldep fname") fname)
;;                                   (format #t "~A: ~A~%" (yellow "ocamldep kind") kind)
;;                                   (format #t "~A: ~A~%" (yellow "ocamldep mdeps") mdeps)))
;;                             (if-let ((m-assoc (find-m-file-in-pkg fname pkg)))
;;                                     ;; should not happen
;;                                     (begin
;;                                       (if *mibl-debug-s7*
;;                                           (format #t "~A: ~A~%" (red "m-assoc in pkg") m-assoc))
;;                                       ;; (error 'stop (format #f "lex/yacc output file ~A already exists~%" fname))
;;                                       )
;;                                     ;;else
;;                                     ;; (error 'stop "STOP yacc")
;;                                     (update-pkg-modules-with-module! pkg principal-name)
;;                                 )))))
;;                   file-deps)))
;;     ;; (error 'stop "STOP yacc")
;;   ))

(define (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)
  (mibl-trace "-ocamlyacc-deps!" pkg-mly-modules *mibl-debug-lexyacc*)
  (for-each (lambda (mly-spec)
              (mibl-trace "mly spec" mly-spec *mibl-debug-lexyacc*)
              (let* ((mly-module (car mly-spec))
                     (mly-src (assoc-val :mly (cdr mly-spec)))
                     (principal (format #f "~A" (bname mly-src)))
                     (cp-cmd (format #f "cp ~A/~A ~A" pkg-path mly-src *mibl-tmp-dir*)))
                (mibl-trace "mly-src" mly-src *mibl-debug-lexyacc*)
                (mibl-trace "cp-cmd" cp-cmd *mibl-debug-lexyacc*)
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((yacc-cmd
                       ;; (if *mibl-menhir*
                       ;;     (format #f "menhir ...
                       (format #f "ocamlyacc ~A/~A" *mibl-tmp-dir* mly-src)))
                  (mibl-trace "yacc-cmd" yacc-cmd *mibl-debug-lexyacc*)
                  (system yacc-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "codept -verbosity info -sexp -k ~A/~A.*  2> /dev/null"
                           ;; #f "ocamldep -one-line -modules -I ~A ~A/~A.*"
                           ;; pkg-path
                           *mibl-tmp-dir* principal))
                         (mibl-trace-let "codept cmd" ocamldep-cmd *mibl-debug-lexyacc*)
                         (depstring (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (mibl-trace-let "depstring" depstring *mibl-debug-lexyacc*)
                         (depgraph_port (open-input-string depstring))
                         (depgraph (read depgraph_port)))
                    ;; mly yields 2 deplists, one for .ml one for .mli
                    (let-values (((ml-deps mli-deps)
                                  (let* ((deps-list (assoc-val 'dependencies depgraph))
                                         (deps-alist-list (car deps-list)))
                                    (mibl-trace "mly deps-alist-list" deps-alist-list *mibl-debug-lexyacc*)
                                    (if (truthy? deps-alist-list)
                                        ;; deps-alist-list: ( ((file ...) (deps ...)) ((file ...) (deps ...)) )
                                        (let* ((deps-alist1 (car deps-alist-list))
                                               (deps-f1 (basename (car (assoc-val 'file deps-alist1))))
                                               (deps-ext1 (filename-extension deps-f1))
                                               (deps-d1 (map (lambda (dep)
                                                               (if (> (length dep) 1)
                                                                   (list->vector dep) (car dep)))
                                                             (car (assoc-val 'deps deps-alist1))))
                                               (deps-alist2 (cadr deps-alist-list))
                                               (deps-f2 (basename (car (assoc-val 'file deps-alist2))))
                                               (deps-ext2 (filename-extension deps-f2))
                                               (deps-d2 (map (lambda (dep)
                                                               (if (> (length dep) 1)
                                                                   (list->vector dep) (car dep)))
                                                             (car (assoc-val 'deps deps-alist2))))
                                               )
                                           (if (string=? ".ml" deps-ext1)
                                               (values deps-d1 deps-d2)
                                               (values deps-d2 deps-d1)))
                                        (values '() '())))))
                      (mibl-trace "mly ml-deps" ml-deps *mibl-debug-lexyacc*)
                      (mibl-trace "mly mli-deps" mli-deps *mibl-debug-lexyacc*)
                      (mibl-trace "mly module" mly-module *mibl-debug-lexyacc*)

                      ;; task: find yacc module in pkg files and update its deps
                      (let* ((mly-tlbl (module-name->tagged-label mly-module pkg))
                             (mibl-trace-let "mly tlbl" mly-tlbl *mibl-debug-lexyacc*)
                             ;; mly-tlbl should have :ml_ and :mli_
                             (mly-ml_  (assoc :ml_ (cdr mly-tlbl)))
                             (mly-mli_ (assoc :mli_ (cdr mly-tlbl))))
                        (mibl-trace "mly :ml_"  mly-ml_ *mibl-debug-lexyacc*)
                        (mibl-trace "mly :mli_" mly-mli_ *mibl-debug-lexyacc*)
                        (set-cdr! (cdr mly-ml_)  ml-deps)
                        (set-cdr! (cdr mly-mli_) mli-deps)
                        ))))))
            pkg-mly-modules))

(define (ocamllex-deps! pkg pkg-path pkg-mll-modules)
  (mibl-trace-entry "-ocamllex-deps!" pkg-mll-modules *mibl-debug-lexyacc*)
  ;; pkg-mll-modules comes from (:lex ....)
  ;; task: discover deps, then find corresponding pkg-file and update
  (for-each (lambda (mll-spec)
              (mibl-trace "mll module spec" mll-spec *mibl-debug-lexyacc*)
              (let* ((mll-module (car mll-spec))
                     (mll-src (cdr mll-spec))
                     (ml-src (format #f "~A.ml" (bname mll-src)))
                     ;;FIXME: if verbose, add '-v' to cmd
                     (cp-cmd (format #f "cp ~A/~A ~A" pkg-path mll-src *mibl-tmp-dir*)))
                (mibl-trace "mll-src" mll-src *mibl-debug-lexyacc*)
                (mibl-trace "cp-cmd" cp-cmd *mibl-debug-lexyacc*)
                (system cp-cmd) ;;;;;;;;;;;;;;;; SYS
                (let ((lex-cmd
                       (format #f "ocamllex -q ~A/~A" *mibl-tmp-dir* mll-src)))
                  (mibl-trace "lex-cmd" lex-cmd *mibl-debug-lexyacc*)
                  (system lex-cmd)  ;;;;;;;;;;;;;;;; SYS
                  (let* ((ocamldep-cmd
                          (format
                           #f "codept -verbosity info -sexp -k ~A/~A 2> /dev/null"
                           ;; #f "ocamldep -one-line -modules -I ~A ~A/~A"
                           ;; pkg-path
                           *mibl-tmp-dir* ml-src))
                         (depstring (string-trim '(#\newline) (system ocamldep-cmd #t))) ;;;;;;;;;;;;;;;; SYS
                         (depgraph_port (open-input-string depstring))
                         (depgraph (read depgraph_port))
                         ;; 'dependencies is a list of a list of alists
                         (file-deps (let* ((deps-list (assoc-val 'dependencies depgraph))
                                           (deps-alist (caar deps-list)))
                                      (if (truthy? deps-alist)
                                          (car (assoc-val 'deps deps-alist))
                                          '())))
                         (file-deps (map (lambda (dep)
                                           (if (> (length dep) 1)
                                              (list->vector dep) (car dep)))
                                         file-deps))
                         (mibl-trace-let "parsed codept" file-deps *mibl-debug-lexyacc*)
                         ;; (file-deps (cdr (string-split deps #\newline)))
                         )
                    (mibl-trace "ocamldep-cmd" ocamldep-cmd *mibl-debug-lexyacc*)
                    (mibl-trace "depstring" depstring *mibl-debug-lexyacc*)
                    (mibl-trace "file-deps 2" file-deps *mibl-debug-lexyacc*)
                    (mibl-trace "mll module" mll-module *mibl-debug-lexyacc*)
                    (mibl-trace "pkg" pkg *mibl-debug-lexyacc*)

                    ;; task: find mll module in pkg files and update its deps
                    (let* ((mll-tlbl (module-name->tagged-label mll-module pkg))
                           (mibl-trace-let "mll tlbl" mll-tlbl *mibl-debug-lexyacc*)
                           ;; mll-tlbl should have :ml_ not :ml e.g. (:ml_ lexer.ml)
                           (mll-ml_ (assoc :ml_ (cdr mll-tlbl))))
                      (mibl-trace "mll :ml_" mll-ml_ *mibl-debug-lexyacc*)
                      (set-cdr! (cdr mll-ml_) file-deps)
                      )))))
            pkg-mll-modules))

;; handle :ocamllex, :ocamlyacc, etc.
(define (lexyacc-file-deps!)
  (mibl-trace-entry "lexyacc-file-deps" "" *mibl-debug-lexyacc*)

  ;; for each pkg in each workspace
  (for-each (lambda (ws-kv)
              (mibl-trace "ws" (car ws-kv) *mibl-debug-lexyacc*)
              (let ((pkgs (car (assoc-val :pkgs (cdr ws-kv)))))
                (for-each (lambda (pkg-kv)
                            (mibl-trace "lexyacc-file-deps! pkg" (assoc-val :pkg-path (cdr pkg-kv)) *mibl-debug-lexyacc*)
                            (mibl-trace "pkg.lex" (assoc :lex (cdr pkg-kv)))
                            (mibl-trace "pkg.yacc" (assoc :yacc (cdr pkg-kv)))
                            (if (or (assoc :lex (cdr pkg-kv)) (assoc :yacc (cdr pkg-kv)))
                                (let* ((pkg (cdr pkg-kv))
                                       (ws-path (assoc-val :ws-path pkg))
                                       (pkg-path (assoc-val :pkg-path pkg))
                                       (pkg-modules (if-let ((modules (assoc-val :modules pkg)))
                                                            modules '()))
                                       (mibl-trace-let "pkg-modules" pkg-modules *mibl-debug-lexyacc*)

                                       (pkg-static-mll (if-let ((mll-static
                                                                 (assoc-in '(:lex :static) pkg)))
                                                               (cdr mll-static) '()))
                                       (mibl-trace-let "pkg-static-mll" pkg-static-mll *mibl-debug-lexyacc*)
                                       ;; prolly won't have dynamic .mll files, but just in case
                                       (pkg-dynamic-mll (if-let ((mll-dyn (assoc-in '(:lex :dynamic) pkg)))
                                                                (cdr mll-dyn) '()))
                                       (mibl-trace-let "pkg-dynamic-mll" pkg-dynamic-mll *mibl-debug-lexyacc*)

                                       (pkg-mll (concatenate pkg-static-mll pkg-dynamic-mll))
                                       (mibl-trace-let "pkg-mll" pkg-mll *mibl-debug-lexyacc*)

                                       (pkg-static-mly (if-let ((mly-static
                                                                 (assoc-in '(:yacc :static) pkg)))
                                                               mly-static '()))
                                       (mibl-trace-let "pkg-static-mly" pkg-static-mly *mibl-debug-lexyacc*)
                                       (pkg-dynamic-mly (if-let ((mly-dyn
                                                                  (assoc-in '(:yacc :dynamic) pkg)))
                                                                mly-dyn '()))
                                       (mibl-trace-let "pkg-dynamic-mly" pkg-dynamic-mly *mibl-debug-lexyacc*)
                                       (pkg-mly (concatenate pkg-static-mly pkg-dynamic-mly))
                                       (mibl-trace-let "pkg-mly" pkg-mly *mibl-debug-lexyacc*)

                                       ;; filtered :modules
                                       (pkg-mly-modules (filter (lambda (m)
                                                                  ;; (format #t "~A: ~A~%" (blue "m") m)
                                                                  (assoc :mly (cdr m))) pkg-modules))
                                       )
                                  (mibl-trace "filtered mly" pkg-mly-modules *mibl-debug-lexyacc*)
                                  (mibl-trace "ws-path" ws-path *mibl-debug-lexyacc*)

                                  (if *mibl-debug-s7*
                                      (begin
                                        (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
                                        (format #t "~A: ~A~%" (blue "pwd") (pwd))
                                        (format #t "~A: ~A~%" (blue "tmp-dir") *mibl-tmp-dir*)))

                                  ;; 1. copy mly file to tmp dir (ensure tmp dir empty first?)
                                  ;; 2. run ocamlyacc on it
                                  ;; 3. run ocamldep on the generated .ml and .mli files
                                  ;; 4. update :pkg-modules entries

                                  (-ocamlyacc-deps! pkg pkg-path pkg-mly-modules)

                                  ;; ditto for ocamllex
                                  ;; (set! *mibl-debug-s7* #t)
                                  (ocamllex-deps! pkg pkg-path pkg-mll)
                                  ;; (set! *mibl-debug-s7* #f)
                                  )))
                          pkgs)))
            *mibl-project*))

;; dune ocamllex/ocamlyacc take non-normalized module names
;; e.g. (ocamllex pb_parsing_lexer)
;; from this we produce (:lex (<modulename> . <src>))
;; e.g. (:lex (Pb_parsing_lexer . pb_parsing_lexer.mll))
;; :lex - alist of (Foo . foo.mll) pairs
;; :yacc - alist of (Foo . foo.mly) pairs
;; NB: usually the srcfiles are static, but in principle could be
;; generated by another rule.
(define (lexyacc->mibl tag ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "lexyacc->mibl") stanza))
  (let ((srcs (let recur ((modules (cdr stanza))
                          (result '()))
                (if (null? modules)
                    result
                    (if (pair? (car modules))
                        ;; e.g. (ocamllex (modules lexer_impl))
                        (recur (cdr modules)
                               (concatenate
                                (let recur2 ((modules2 (car modules))
                                             (result2 '()))
                                  (if (null? modules2)
                                      result2
                                      (if (equal? 'modules (car modules2))
                                          (recur2 (cdr modules2) result2)
                                          (recur2 (cdr modules2)
                                                  (cons (car modules2) result2)))))
                                result))
                        ;; else (ocamllex foo bar ...)
                        (recur (cdr modules)
                               (cons (car modules) result)))))))
    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (uwhite "lex/yacc srcs") srcs))
    ;; tasks:
    ;; a) verify srcfile exists?
    ;; b) update pkg-modules and/or pkg-structures
    (let* ((ext (case tag ((:lex) "mll")
                      ((:yacc) "mly")
                      (else (error 'fixme
                                   (format #t "~A: ~A~%" (bgred "unrecognized lex/yacc tag") tag)))))
           (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "ext") ext)))
           (resolved (map (lambda (principal-fname)
                            ;;FIXME: verify file is in pkg :ocamllex
                            (let* ((mllfile (format #f "~A.~A" principal-fname ext))
                                   (mlfile (format #f "~A.ml" principal-fname))
                                   (mname (normalize-module-name principal-fname))
                                   (pr (cons mname mllfile)))
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (uwhite "tag") tag))
                              ;; side-effects:
                              (case tag
                                ((:lex)
                                 ;;(update-pkg-files-with-struct! pkg mlfile)
                                 )
                                ((:yacc)
                                 ;; process it, then run ocamldep
                                 ;; (yacc-deps pkg principal-fname)
                                 ;; (error 'stop "STOP mllfile")
                                 )
                                (else
                                 (error 'fixme (format #f "~A" (bgred "unrecognized lex/yacc tag") tag))))
                              pr))
                          srcs)))
    (list (cons tag resolved)))))
