(define (-alias-args->miblark pkg stanza)
  (format #t "~A: ~A~%" (ublue "-alias-args->miblark") stanza)
  (if-let ((args (assoc-in '(:actions :cmd :args) (cdr stanza))))
          ;;FIXME assuming one cmd
          (let* ((stanza-alist (cdr stanza))
                 (_ (format #t "~A: ~A~%" (uwhite "stanza-alist") stanza-alist))
                 (alias (cadr (assoc :alias stanza-alist)))
                 (_ (format #t "~A: ~A~%" (uwhite "alias") alias))
                 (args (cdr args))
                 (_ (format #t "~A: ~A~%" (uwhite "alias args") args))
                 (cmd-ct (length (assoc-in* '(:actions :cmd) stanza-alist)))
                 (_ (format #t "~A: ~A~%" (uwhite "cmd ct") cmd-ct))
                 (deps (if-let ((deps (assoc :deps stanza-alist)))
                               (cdr deps) '()))
                 (_ (format #t "~A: ~A~%" (uwhite "deps") deps))
                 (outputs (if-let ((outputs (assoc :outputs stanza-alist)))
                               (cdr outputs) '()))
                 (_ (format #t "~A: ~A~%" (uwhite "outputs") outputs))

                 ;; (tool (format #f "~A" (keyword->symbol (car args))))
                 ;; (_ (format #t "~A: ~A~%" (uwhite "tool") tool))

                 ;;FIXME: handle progn (multiple cmds)
                 (tool (assoc-in '(:actions :cmd :tool) stanza-alist))
                 (_ (format #t "~A: ~A~%" (uwhite "tool") tool))
                 ;; (error 'X "STOP miblark tool")

                 )
            ;; (-expand-literal-tool!? (car (assoc-val :pkg-path pkg)) tool deps)
            (set-car! stanza :sh-test)

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
            ;;   ;;                     (let ((_ (format #t "~A: ~A~%" (ured "deps") deps))
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

(define (-alias->miblark stanza)
  (format #t "~A: ~A~%" (ublue "-alias->miblark") stanza)
  (if-let ((alias-assoc (assoc :alias (cdr stanza))))
          (if (assoc-in '(:actions :cmd) (cdr stanza))
              (let ((alias (cadr alias-assoc))
                    (cmd-ct (length (assoc-in* '(:actions :cmd) (cdr stanza))))
                    ;;FIXME assuming one cmd
                    (args (assoc-in '(:actions :cmd :args) (cdr stanza))))
                (format #t "~A: ~A~%" (ured "ALIAS") alias)
                (format #t "~A: ~A~%" (ured "cmd ct") cmd-ct)
                (format #t "~A: ~A~%" (ured "args") args)
                ;; if :args contains executable, mark as :test
                (let ((tool-args (fold (lambda (arg accum)
                                         (format #t "~A: ~A~%" (ured "arg") arg)
                                         (let ((argstr (format #f "~A" arg)))
                                           (format #t "~A: ~A~%" (ured "argstr") argstr)
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
                        (format #t "~A: ~A~%" (ured "found executable tool args") tool-args)
                        (if-let ((deps (assoc :deps (cdr stanza))))
                                (let ((tool-deps (assoc ::tools (cdr deps))))
                                  (if tool-deps
                                      ;; append tools
                                      (format #t "~A: ~A~%" (ured "tool-deps") tool-deps)
                                      ;; add ::tools to (:deps ...)
                                      (let ((_ (format #t "~A: ~A~%" (ured "deps") deps))
                                            (deps-list (cdr deps))
                                            (tools (list (cons ::tools tool-args))))
                                        (set-cdr! deps (append tools deps-list)))
                                      ))
                                ;; else no deps in stanza?
                                (begin))
                        (set-car! stanza :sh-test)
                        )
                      (begin
                        (format #t "~A: ~A~%" (ured "NO executable tools") tools)))
                  ))
              ;; else alias with no :actions
              (begin
                (error 'fixme
                       (format #f "~A: ~A~%" (ured "ALIAS w/o actions") stanza))))
          (begin
            (format #t "~A: ~A~%" (ured "NO ALIAS") stanza)
            #| nop |#)))

;; replace e.g. :rule by :write-file
;; :executable by :test if deps include unit test pkg
(define (mibl-pkg->miblark pkg)
  (format #t "~A: ~A~%" (blue "mibl-pkg->miblark") pkg) ;;(assoc-val :pkg-path pkg))

  (if (assoc :dune pkg)
      (for-each
       (lambda (stanza)
         (format #t "~A: ~A~%" (magenta "stanza") stanza)
         ;; first do write-file etc.
         (case (car stanza)
           ((:rule)
            ;; if multiple cmds (progn) do not miblarkize
            (format #t "~A: ~A~%" (red "cmd ct:")
                    (length (assoc-in* '(:actions :cmd) (cdr stanza))))
            (if (< (length (assoc-in* '(:actions :cmd) (cdr stanza))) 2)
                (let ((tool (assoc-in '(:actions :cmd :tool) (cdr stanza))))
                  (format #t "~A: ~A~%" (green "tool") tool)
                  (if tool
                      (let ((tool (cadr tool)))
                        (format #t "~A: ~A~%" (green "tool") tool)
                        (case tool
                          ((:write-file) ;;FIXME: what if we have write-file in an alias rule?
                           (format #t "~A: ~A~%" (red "miblarking") stanza)
                           ;; (format #t "~A: ~A~%" (white "pkg before") pkg)
                           (set-car! stanza :write-file)
                           ;; (format #t "~A: ~A~%" (white "pkg after") pkg)
                           )

                          (else ;; nop
                           '())))
                      ))))
           ((:executable)
            (format #t "~A: ~A~%" (uwhite "miblarkizing executable") stanza)
            (let* ((stanza-alist (cdr stanza))
                   (compile-deps (assoc-in '(:compile :deps :resolved) stanza-alist)))
              (format #t "~A: ~A~%" (uwhite "compile-deps") compile-deps)
              (if compile-deps
                  (let ((test? (find-if (lambda (dep)
                                          (member dep unit-test-pkgs))
                                        (cdr compile-deps))))
                    (if test? (set-car! stanza :test) #f))
                  #f)))
           (else
            ))
         ;; aliases
         (if (alist? (cdr stanza))
             (if (assoc :alias (cdr stanza))
                 (-alias-args->miblark pkg stanza)))
         )
       (assoc-val :dune pkg))
      ;; else
      ))
