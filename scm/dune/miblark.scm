(define -sh-test-id 0)

;; FIXME: do not convert to sh-test unless runtool is a shell script
(define (X-alias-args->miblark pkg stanza)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "-alias-args->miblark") stanza))
  (if-let ((args (assoc-in '(:actions :cmd :args) (cdr stanza))))
          ;;FIXME assuming one cmd
          (let* ((stanza-alist (cdr stanza))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "stanza-alist") stanza-alist)))
                 (alias (cadr (assoc :alias stanza-alist)))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "alias") alias)))
                 (args (cdr args))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "alias args") args)))
                 (cmd-ct (length (assoc-in* '(:actions :cmd) stanza-alist)))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "cmd ct") cmd-ct)))
                 (deps (if-let ((deps (assoc :deps stanza-alist)))
                               (cdr deps) '()))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "deps") deps)))
                 (outputs (if-let ((outputs (assoc :outputs stanza-alist)))
                               (cdr outputs) '()))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "outputs") outputs)))

                 ;; (tool (format #f "~A" (keyword->symbol (car args))))
                 ;; (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "tool") tool)))

                 ;;FIXME: handle progn (multiple cmds)
                 (tool (assoc-in '(:actions :cmd :tool) stanza-alist))
                 (_ (if *mibl-debugging* (format #t "~A: ~A~%" (uwhite "tool") tool)))
                 ;; (error 'X "STOP miblark tool")

                 )
            ;; (-expand-literal-tool!? (car (assoc-val :pkg-path pkg)) tool deps)
            (set-cdr! stanza
                      (acons :name
                             (format #f "~A_~A" alias -sh-test-id)
                             (cdr stanza)))
            (set-car! stanza :sh-test)
            (set! -sh-test-id (+ 1 -sh-test-id))

            ;; if :args contains executable, mark as :test
            ;; (let ((tool-args (fold (lambda (arg accum)
            ;;                          (format #t "~A: ~A~%" (ucyan "fold arg") arg)
            ;;                          (let ((argstr (format #f "~A" arg)))
            ;;                            (format #t "~A: ~A~%" (uwhite "argstr") argstr)
            ;;                            ;; FIXME what about local sh scripts?
            ;;                            (cond
            ;;                             ((string-prefix? ":bin" argstr) (cons arg accum))
            ;;                             ((string-prefix? ":exe" argstr) (cons arg accum))
            ;;                             ((string-prefix? ":libexec" argstr) (cons arg accum))
            ;;                             ;; lib:<public-library-name>:<file>
            ;;                             ;; lib-private:<library-name>:<file>
            ;;                             ;; libexec-private:<library-name>:<file>
            ;;                             ((= 0 (fnmatch "*.sh" argstr 0)) (cons arg accum))
            ;;                             ((= 0 (fnmatch "*.py" argstr 0)) (cons arg accum))
            ;;                             ((= 0 (fnmatch "*.js" argstr 0)) (cons arg accum))
            ;;                             ;;FIXME others?
            ;;                             (else accum))))
            ;;                        '() args)))
            ;;   (format #t "~A: ~A~%" (ured "found executable tool args") tool-args)
            ;;   ;; (if tool-args
            ;;   ;;     (begin
            ;;   ;;       (format #t "~A: ~A~%" (ured "found executable tool args") tool-args)
            ;;   ;;       (if-let ((deps (assoc :deps (cdr stanza))))
            ;;   ;;               (let ((tool-deps (assoc ::tools (cdr deps))))
            ;;   ;;                 (if tool-deps
            ;;   ;;                     ;; append tools
            ;;   ;;                     (format #t "~A: ~A~%" (ured "tool-deps") tool-deps)
            ;;   ;;                     ;; add ::tools to (:deps ...)
            ;;   ;;                     (let ((_ (if *mibl-debugging* (format #t "~A: ~A~%" (ured "deps") deps)))
            ;;   ;;                           (deps-list (cdr deps))
            ;;   ;;                           (tools (list (cons ::tools tool-args))))
            ;;   ;;                       (set-cdr! deps (append tools deps-list)))
            ;;   ;;                     ))
            ;;   ;;               ;; else no deps in stanza?
            ;;   ;;               (begin))
            ;;   ;;       (set-car! stanza :sh-test)
            ;;   ;;       )
            ;;   ;;     (begin
            ;;   ;;       (format #t "~A: ~A~%" (ured "NO executable tools") tools)))
            ;;   )
            )))

;; rule stanza containing (alias) fld?
;; if alias name contains 'test' convert to :sh-test
(define (-alias->miblark stanza)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "-alias->miblark") stanza))
  (if-let ((alias-assoc (assoc :alias (cdr stanza))))
          (if (assoc-in '(:actions :cmd) (cdr stanza))
              (let ((alias (cadr alias-assoc))
                    (cmd-ct (length (assoc-in* '(:actions :cmd) (cdr stanza))))
                    ;;FIXME assuming one cmd
                    (args (assoc-in '(:actions :cmd :args) (cdr stanza))))
                (if *mibl-debugging*
                    (begin
                      (format #t "~A: ~A~%" (ured "ALIAS") alias)
                      (format #t "~A: ~A~%" (ured "cmd ct") cmd-ct)
                      (format #t "~A: ~A~%" (ured "args") args)))
                ;; if :args contains executable, mark as :test
                (let ((tool-args (fold (lambda (arg accum)
                                         (if *mibl-debugging*
                                             (format #t "~A: ~A~%" (ured "arg") arg))
                                         (let ((argstr (format #f "~A" arg)))
                                           (if *mibl-debugging*
                                               (format #t "~A: ~A~%" (ured "argstr") argstr))
                                           ;; FIXME what about local sh scripts?
                                           (cond
                                            ((string-prefix? ":bin" argstr) (cons arg accum))
                                            ((string-prefix? ":exe" argstr) (cons arg accum))
                                            ((string-prefix? ":libexec" argstr) (cons arg accum))
                                            ;; lib:<public-library-name>:<file>
                                            ;; lib-private:<library-name>:<file>
                                            ;; libexec-private:<library-name>:<file>
                                            ((= 0 (fnmatch "*.sh" argstr 0)) (cons arg accum))
                                            ((= 0 (fnmatch "*.py" argstr 0)) (cons arg accum))
                                            ((= 0 (fnmatch "*.js" argstr 0)) (cons arg accum))
                                            ;;FIXME others?
                                            (else accum))))
                                       '() (cdr args))))
                  (if tool-args
                      (begin
                        (if *mibl-debugging*
                            (format #t "~A: ~A~%" (ured "found executable tool args") tool-args))
                        (if-let ((deps (assoc :deps (cdr stanza))))
                                (let ((tool-deps (assoc ::tools (cdr deps))))
                                  (if tool-deps
                                      ;; append tools
                                      (if *mibl-debugging*
                                          (format #t "~A: ~A~%" (ured "tool-deps") tool-deps))
                                      ;; add ::tools to (:deps ...)
                                      (let ((_ (if *mibl-debugging* (format #t "~A: ~A~%" (ured "deps") deps)))
                                            (deps-list (cdr deps))
                                            (tools (list (cons ::tools tool-args))))
                                        (set-cdr! deps (append tools deps-list)))
                                      ))
                                ;; else no deps in stanza?
                                (begin))
                        (set-cdr! stanza
                                  (acons :name
                                         (format #f "~A_~A" alias -sh-test-id)
                                         (cdr stanza)))
                        (set-car! stanza :sh-test)
                        (set! -sh-test-id (+ 1 -sh-test-id))
                        )
                      (begin
                        (if *mibl-debugging*
                            (format #t "~A: ~A~%" (ured "NO executable tools") tools))
                        (error 'FIXME "alias without run tool")))
                  ))
              ;; else alias with no :actions
              (begin
                (error 'fixme
                       (format #f "~A: ~A~%" (ured "ALIAS w/o actions") stanza))))
          (begin
            (if *mibl-debugging*
                (format #t "~A: ~A~%" (ured "NO ALIAS") stanza))
            #| nop |#)))

;; replace e.g. :rule by :write-file, :ocamlc, :node, etc.
;; depending on action tool
;; :executable by :test if deps include unit test pkg
(define (mibl-pkg->miblark pkg)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "mibl-pkg->miblark") pkg)) ;;(assoc-val :pkg-path pkg))

  (set! -sh-test-id 0)

  (if-let ((dune-pkg (assoc :mibl pkg)))
          (for-each
           (lambda (stanza)
             (if *mibl-debugging*
                 (format #t "~A: ~A~%" (magenta "stanza") stanza))
             ;; first do write-file etc.
             (case (car stanza)
               ((:rule)
                ;; if multiple cmds (progn) do not miblarkize
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (red "cmd ct:")
                        (length (assoc-in* '(:actions :cmd) (cdr stanza)))))
                (if (< (length (assoc-in* '(:actions :cmd) (cdr stanza))) 2)
                    (let ((tool (assoc-in '(:actions :cmd :tool) (cdr stanza))))
                      (if *mibl-debugging*
                          (format #t "~A: ~A~%" (green "tool") tool))
                      (if tool
                          (let ((tool (cadr tool)))
                            (if *mibl-debugging*
                                (format #t "~A: ~A~%" (green "tool") tool))
                            (case tool
                              ((:write-file) ;;FIXME: what if we have write-file in an alias rule?
                               (if *mibl-debugging*
                                   (format #t "~A: ~A~%" (red "miblarking") stanza))
                               (set-car! stanza :write-file))
                              ((:cppo) (set-car! stanza :cppo))

                              ((::ocamlc)
                               (if-let ((deps (assoc :deps (cdr stanza))))
                                       (set-car! deps :srcs))
                               (set-car! stanza :ocamlc))

                              ((::diff) (set-car! stanza :diff))
                              ((::node) (set-car! stanza :node))
                              (else ;; nop
                               '())))
                          ))))

               ((:ns-archive)
                ;; convert to archive if only one submodule
                ;; AND submodule name = ns name
                (if (= (length (cdr (assoc-in '(:manifest :modules) (cdr stanza))))
                       1)
                    (let ((mname (cdr (assoc-in '(:manifest :modules) (cdr stanza))))
                          (ns (assoc-val :ns (cdr stanza))))
                      (if (equal? mname ns)
                          (begin
                            (set-car! stanza :archive)
                            (set-cdr! stanza (dissoc '(:ns) (cdr stanza))))))))

               ((:executable)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (uwhite "miblarkizing executable") (car stanza)))
                (let* ((stanza-alist (cdr stanza))
                       (compile-deps (assoc-in '(:compile :deps :resolved) stanza-alist))
                       (exec-lib (assoc :exec-lib stanza-alist)))
                  (if *mibl-debugging*
                      (begin
                        (format #t "~A: ~A~%" (uwhite "compile-deps") compile-deps)
                        (format #t "~A: ~A~%" (uwhite "exec-lib") exec-lib)))
                  (if compile-deps
                      (let ((test? (find-if (lambda (dep)
                                              (member dep unit-test-pkgs))
                                            (cdr compile-deps))))
                        (if test? (set-car! stanza :test) #f)))
                  (if (truthy? exec-lib)
                      (if-let ((pkg-exec-libs (assoc :exec-libs (cdr dune-pkg))))
                              (begin
                                (if *mibl-debugging*
                                    (format #t "~A: ~A~%" (ublue "found pkg-exec-libs") pkg-exec-libs))
                                (if (number? (cdr exec-lib))
                                    (begin) ;; already done
                                    (if (member (cdr exec-lib) (cdr pkg-exec-libs)
                                                (lambda (a b)
                                                  (if *mibl-debugging*
                                                      (format #t "~A: a: ~A, b: ~A~%" (red "comparing") a b))
                                                  (equal? a (assoc-val :modules (cdr b)))))
                                        (if *mibl-debugging*
                                            (format #t "~A: ~A~%" (ublue "match") exec-lib))
                                        (let* ((ct (length (cdr pkg-exec-libs)))
                                               (new (list (cons (+ 1 ct)
                                                                `((:modules ,@(cdr exec-lib)))))))
                                          (if *mibl-debugging*
                                              (format #t "~A: ~A~%" (ublue "mismatch; adding") exec-lib))
                                          (set-cdr! pkg-exec-libs (append (cdr pkg-exec-libs) new))))))
                              (let* ((opts (if-let ((opts (assoc-val :opts stanza-alist)))
                                                  `((:opts ,@opts)) '()))
                                     (new `((:exec-libs (1 (:modules ,@(cdr exec-lib)) ,@opts)))))
                                (if *mibl-debugging*
                                    (format #t "~A: ~A~%" (ublue "adding :exec-libs to stanzas") exec-lib))
                                (set-cdr! dune-pkg (append (cdr dune-pkg) new))
                                (set-cdr! exec-lib 1))
                                ))
                  ))
               (else
                ))
             ;; aliases
             ;; (if (alist? (cdr stanza))
             ;;     (if (assoc :alias (cdr stanza))
             ;;         (-alias-args->miblark pkg stanza)))
             )
           (cdr dune-pkg))
      ;; else no dune file
      ))

(define (miblarkize ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (blue "miblarkizing") kv))
                ;; dir may have dune-project but no dune file:
                (if (not (null? (cdr kv)))
                    (mibl-pkg->miblark (cdr kv)))
                )
              pkgs)))

