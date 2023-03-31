(define -sh-test-id 0)

;; FIXME: do not convert to sh-test unless runtool is a shell script
(define (X-alias-args->miblark pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-alias-args->miblark") stanza))
  (if-let ((args (assoc-in '(:actions :cmd :args) (cdr stanza))))
          ;;FIXME assuming one cmd
          (let* ((stanza-alist (cdr stanza))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "stanza-alist") stanza-alist)))
                 (alias (cadr (assoc :alias stanza-alist)))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "alias") alias)))
                 (args (cdr args))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "alias args") args)))
                 (cmd-ct (length (assoc-in* '(:actions :cmd) stanza-alist)))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "cmd ct") cmd-ct)))
                 (deps (if-let ((deps (assoc :deps stanza-alist)))
                               (cdr deps) '()))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "deps") deps)))
                 (outputs (if-let ((outputs (assoc :outputs stanza-alist)))
                               (cdr outputs) '()))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "outputs") outputs)))

                 ;; (tool (format #f "~A" (keyword->symbol (car args))))
                 ;; (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "tool") tool)))

                 ;;FIXME: handle progn (multiple cmds)
                 (tool (assoc-in '(:actions :cmd :tool) stanza-alist))
                 (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "tool") tool)))
                 ;; (error 'X "STOP miblark tool")

                 )
            ;; (-expand-literal-tool!? (assoc-val :pkg-path pkg) tool deps)
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
            ;;   ;;                     (let ((_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "deps") deps)))
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
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-alias->miblark") stanza))
  (if-let ((alias-assoc (assoc :alias (cdr stanza))))
          (if (assoc-in '(:actions :cmd) (cdr stanza))
              (let ((alias (cadr alias-assoc))
                    (cmd-ct (length (assoc-in* '(:actions :cmd) (cdr stanza))))
                    ;;FIXME assuming one cmd
                    (args (assoc-in '(:actions :cmd :args) (cdr stanza))))
                (if *mibl-debug-s7*
                    (begin
                      (format #t "~A: ~A~%" (ured "ALIAS") alias)
                      (format #t "~A: ~A~%" (ured "cmd ct") cmd-ct)
                      (format #t "~A: ~A~%" (ured "args") args)))
                ;; if :args contains executable, mark as :test
                (let ((tool-args (fold (lambda (arg accum)
                                         (if *mibl-debug-s7*
                                             (format #t "~A: ~A~%" (ured "arg") arg))
                                         (let ((argstr (format #f "~A" arg)))
                                           (if *mibl-debug-s7*
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
                        (if *mibl-debug-s7*
                            (format #t "~A: ~A~%" (ured "found executable tool args") tool-args))
                        (if-let ((deps (assoc :deps (cdr stanza))))
                                (let ((tool-deps (assoc ::tools (cdr deps))))
                                  (if tool-deps
                                      ;; append tools
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (ured "tool-deps") tool-deps))
                                      ;; add ::tools to (:deps ...)
                                      (let ((_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "deps") deps)))
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
                        (if *mibl-debug-s7*
                            (format #t "~A: ~A~%" (ured "NO executable tools") tools))
                        (error 'FIXME "alias without run tool")))
                  ))
              ;; else alias with no :actions
              (begin
                (error 'fixme
                       (format #f "~A: ~A~%" (ured "ALIAS w/o actions") stanza))))
          (begin
            (if *mibl-debug-s7*
                (format #t "~A: ~A~%" (ured "NO ALIAS") stanza))
            #| nop |#)))


