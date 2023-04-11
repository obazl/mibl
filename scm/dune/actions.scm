(if *mibl-debug-s7-loads*
    (format #t "loading dune/dune_actions.scm~%"))

(provide 'actions.scm)

(load "dune/action_run.scm")
(load "dune/action_shell.scm")

(define (normalize-action-chdir-dsl ws pkg action-alist tools targets deps)
  (mibl-trace-entry "normalize-action-chdir-dsl" action-alist)
  (let* ((action-assoc action-alist)
         (mibl-trace-let "action-assoc" action-assoc :test *mibl-debug-action-directives*)
         (ctx (if (eq? '%{workspace_root} (cadr action-assoc))
                  :ws-root
                  `(:fixme ,(cadr action-assoc))))
         (mibl-trace-let "ctx" ctx :test *mibl-debug-action-directives*)
         (subaction-list (caddr action-assoc))
         (mibl-trace-let "subaction-list" subaction-list :test *mibl-debug-action-directives*)
         (subaction (car subaction-list))
         (mibl-trace-let "subaction" subaction :test *mibl-debug-action-directives*)
         (cmd (if-let ((cmd-fn (assoc-val subaction dune-action-cmds-no-dsl)))
                      (let ((cmd-list (apply (car cmd-fn)
                                             ;; (ws pkg action action-list tools targets deps)
                                             (list ws pkg
                                                   subaction subaction-list
                                                   tools targets deps))))
                        cmd-list)
                      (if-let ((cmd-fn (assoc-val subaction dune-action-cmds-dsl)))
                              (let ((cmd-list (apply (car cmd-fn)
                                                     (list ws pkg ;; subaction
                                                           subaction-list
                                                           tools targets deps))))
                                cmd-list)
                              (begin
                                (format #t "UNHANDLED SUBACTION: ~A\n" subaction)
                                stanza)))))
    (mibl-trace "chdir cmd" cmd :test *mibl-debug-action-directives*)
    (append cmd
          `((:ctx ,ctx)))))

;; file ops:  cat, cmp, copy, copy#, diff, diff?
(define (normalize-action-file-op ws pkg action action-list tools targets deps)
  (if :test *mibl-debug-action-directives*
      (begin
        (format #t "~A\n" (ublue "normalize-action-file-op"))
        (format #t "~A: ~A\n" (blue "pkg") pkg)
        (format #t "~A: ~A\n" (blue "ws") ws)
        (format #t "  ~A: ~A\n" (blue "action") action)
        (format #t "  ~A: ~A\n" (blue "action-list") action-list)
        (format #t "  ~A: ~A\n" (blue "targets") targets)
        (format #t "  ~A: ~A\n" (blue "deps") deps)
        (format #t "  ~A: ~A\n" (green "deps (dune)") (assoc-in '(dune rule deps) pkg))))
  (let* ((tool (string->keyword (format #f "~A" action)))
         (mibl-trace-let "tool" tool)
         ;; build-in cmds get double-colon kw, e.g. ::cat
         (action-args (cdr action-list)) ;; (assoc-val action action-list))
         (_ (if :test *mibl-debug-action-directives*
                (format #t "action-args: ~A\n" action-args)))
         (args (expand-cmd-args* ws action-args pkg targets deps))
                               ;; ;; action-list
                               ;; '()))
         (_ (if :test *mibl-debug-action-directives* (format #t "expanded args: ~A\n" args))))
    (set-cdr! tools `((,tool . :shell-tool)))
    `((:cmd
       (:tool ,tool)
       (:args ,@args)))))

(define (normalize-action-ignore-outputs-dsl action stanza)
  (if :test *mibl-debug-action-directives*
      (format #t "NORMALIZE-ACTION-OUTPUTS-DSL ~A\n" action))
  '()
  )

(define (normalize-action-ignore-stderr-dsl action stanza)
  (if :test *mibl-debug-action-directives*
      (format #t "NORMALIZE-ACTION-STDERR-DSL ~A\n" action))
  '()
  )

(define (normalize-action-ignore-stdout-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-STDOUT-DSL ~A\n" action))
  '()
  )

(define (normalize-action-no-infer-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-NO-INFOR-DSL ~A\n" action))
  '()
  )

(define (normalize-action-no-infer-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-NO-INFOR-DSL ~A\n" action))
  '()
  )

(define (normalize-action-pipe-outputs-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-PIPE-OUTPUTS-DSL ~A\n" action))
  (error 'NOTYET
         (format #f "not implemented: normalize-action-pipe-outputs-dsl")))

(define (normalize-action-pipe-stderr-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-PIPE-STDERR-DSL ~A\n" action))
  (error 'NOTYET
         (format #f "not implemented: normalize-action-pipe-stderr-dsl")))

;; yojson/bench/dune:
;; (rule
;;  (alias bench-generic-sexp)
;;  (deps bench.json)
;;  (action
;;   (pipe-stdout
;;    (run ./bench.exe generic -sexp)
;;    (run ./conversions.exe -- generic))))
;; jsoo compiler/tests-io/dune
;; (with-stdout-to
;;  %{target}
;;  (pipe-stdout
;;   (run printf "echo \\226\\152\\160")
;;   (run node %{dep:./cat.bc.js})))

(define (normalize-action-pipe-stdout-dsl ws pkg action-alist targets deps)
  (if :test *mibl-debug-action-directives*
      (begin
        (format #t "~A: ~A~%" (ublue "normalize-action-pipe-stdout-dsl") action-alist)
        (format #t "~A: ~A~%" (green "pkg") pkg)
        (format #t "~A: ~A~%" (green "targets") targets)
        (format #t "~A: ~A~%" (green "deps") deps)))
  (error 'x "unimplemented: normalize-action-pipe-stdout-dsl")
  (let* ((progn-items (cdar action-alist))
         (_ (if :test *mibl-debug-action-directives* (format #t "progn-items: ~A\n" progn-items)))

         ;; (args (assoc-val 'write-file action-alist))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "args: ~A\n" args)))

         ;; (output (car args))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (white "output") output)))

         ;; (target (if (null? targets)
         ;;             output
         ;;             ;; get tagged-label from targets, for output
         ;;             (if-let ((t (-find-item-in-targets output targets)))
         ;;                     t
         ;;                     (-infer-output! output targets pkg))))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (red "target") target)))

         (progns (map (lambda (item)
                        (-handle-progn-item item ws pkg targets deps))
                      progn-items)))
  ;; (let recur ((progn-list progn-items)
  ;;             (cmd-list '()))
  ;;   ;; (format #t "progn cmdlist ~A\n" cmd-list)
  ;;   (if (null? progn-list)
  ;;       cmd-list
  ;;       (let* ((progn (car progn-list))
  ;;              (action (car progn))
  ;;              (_ (if :test *mibl-debug-action-directives* (format #t "progn action: ~A\n" action)))
  ;;              (args (cdr progn))
  ;;              (_ (if :test *mibl-debug-action-directives* (format #t "args: ~A\n" args)))
  ;;              (cmd (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
  ;;                           (let ((cmd-list (apply (car cmd-fn)
  ;;                                                  (list ws pkg
  ;;                                                        action
  ;;                                                        (list progn)
  ;;                                                        targets deps))))
  ;;                             cmd-list)
  ;;                           (if-let ((cmd-fn (assoc-val action
  ;;                                                       dune-action-cmds-dsl)))
  ;;                                   (let ((cmd-list (apply (car cmd-fn)
  ;;                                                          (list ws pkg
  ;;                                                                (list progn)
  ;;                                                                ;;(list action-alist)
  ;;                                                                targets deps))))
  ;;                                     cmd-list)
  ;;                                   (begin
  ;;                                     (format #t "UNHANDLED PROGN ACTION: ~A\n"
  ;;                                             action)
  ;;                                     stanza)))))
  ;;         (recur (cdr progn-list) (append cmd-list cmd)))))

    (if :test *mibl-debug-action-directives*
        (format #t "~A: ~A~%" (cyan "progns") progns))
    (list (cons :progn progns))))

(define (-handle-progn-item item ws pkg targets deps) ;;FIXME: add tools arg
  (if :test *mibl-debug-action-directives*
      (begin
        (format #t "~A: ~A~%" (blue "-handle-progn-item") item)
        (format #t "~A: ~A~%" (white "targets") targets)
        (format #t "~A: ~A~%" (blue "deps") deps)))

  (let* ((progn item) ;; (car progn-list))
         (action (car progn))
         (_ (if :test *mibl-debug-action-directives* (format #t "progn action: ~A\n" action)))
         (args (cdr progn))
         (_ (if :test *mibl-debug-action-directives* (format #t "args: ~A\n" args)))
         (cmd (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
                      (let ((cmd-list (apply (car cmd-fn)
                                             (list ws pkg
                                                   action (list progn)
                                                   targets deps))))
                        cmd-list)
                      (if-let ((cmd-fn (assoc-val action
                                                  dune-action-cmds-dsl)))
                              (let ((cmd-list (apply (car cmd-fn)
                                                     (list ws pkg
                                                           (list progn)
                                                           ;;(list action-alist)
                                                           tools targets deps))))
                                (if :test *mibl-debug-action-directives*
                                    (format #t "~A: ~A~%" (bggreen "progn cmd-list") cmd-list))
                                (error 'X "STOP cmd-list 2")
                                cmd-list)
                              (begin
                                (format #t "UNHANDLED PROGN ACTION: ~A\n"
                                        action)
                                stanza)))))
    (if :test *mibl-debug-action-directives*
        (format #t "~A: ~A~%" (red "PROGN ITEM") cmd))
    ;; (error 'fixme "tmp")
    (car cmd)))

(define (normalize-action-progn-dsl ws pkg action-alist targets deps)
  (if :test *mibl-debug-action-directives*
      (begin
        (format #t "~A: ~A\n" (ublue "normalize-action-progn-dsl") action-alist)
        (format #t "~A: ~A~%" (green "pkg") pkg)
        (format #t "~A: ~A~%" (green "targets") targets)
        (format #t "~A: ~A~%" (green "deps") deps)))

  (let* ((progn-items (cdar action-alist))
         (_ (if :test *mibl-debug-action-directives* (format #t "progn-items: ~A\n" progn-items)))

         ;; (args (assoc-val 'write-file action-alist))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "args: ~A\n" args)))

         ;; (output (car args))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (white "output") output)))

         ;; (target (if (null? targets)
         ;;             output
         ;;             ;; get tagged-label from targets, for output
         ;;             (if-let ((t (-find-item-in-targets output targets)))
         ;;                     t
         ;;                     (-infer-output! output targets pkg))))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (red "target") target)))

         (progns (map (lambda (item)
                        (-handle-progn-item item ws pkg targets deps))
                      progn-items)))
  ;; (let recur ((progn-list progn-items)
  ;;             (cmd-list '()))
  ;;   ;; (format #t "progn cmdlist ~A\n" cmd-list)
  ;;   (if (null? progn-list)
  ;;       cmd-list
  ;;       (let* ((progn (car progn-list))
  ;;              (action (car progn))
  ;;              (_ (if :test *mibl-debug-action-directives* (format #t "progn action: ~A\n" action)))
  ;;              (args (cdr progn))
  ;;              (_ (if :test *mibl-debug-action-directives* (format #t "args: ~A\n" args)))
  ;;              (cmd (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
  ;;                           (let ((cmd-list (apply (car cmd-fn)
  ;;                                                  (list ws pkg
  ;;                                                        action
  ;;                                                        (list progn)
  ;;                                                        targets deps))))
  ;;                             cmd-list)
  ;;                           (if-let ((cmd-fn (assoc-val action
  ;;                                                       dune-action-cmds-dsl)))
  ;;                                   (let ((cmd-list (apply (car cmd-fn)
  ;;                                                          (list ws pkg
  ;;                                                                (list progn)
  ;;                                                                ;;(list action-alist)
  ;;                                                                targets deps))))
  ;;                                     cmd-list)
  ;;                                   (begin
  ;;                                     (format #t "UNHANDLED PROGN ACTION: ~A\n"
  ;;                                             action)
  ;;                                     stanza)))))
  ;;         (recur (cdr progn-list) (append cmd-list cmd)))))

    (if :test *mibl-debug-action-directives*
        (format #t "~A: ~A~%" (cyan "progns") progns))
    (list (cons :progn progns))))

;; called for (action (run ...) ...)
;; just remove 'run' and recur on dune-action-cmds-...
(define (normalize-action-run-dsl ws pkg run-list tools targets deps)
  (mibl-trace-entry "normalize-action-run-dsl" run-list)
  ;; run-list: ((run foo.sh arg1 arg2 ...))
  ;; but maybe: ((run (foo.sh arg1 arg2 ...)))
  (let* ((run-dsl run-list)
         (action-list (cdr run-dsl))
         (mibl-trace-let "action-list" action-list :test *mibl-debug-action-directives*)
         (action (car action-list))
         (mibl-trace-let "action" action :test *mibl-debug-action-directives*)
         (action-args (cdr action-list))
         (mibl-trace-let "action-args" action-args :test *mibl-debug-action-directives*))

    (mibl-trace "processing" "")
    (if (equal? '%{deps} action)
        (if (null? action-args)
            (begin
              (if :test *mibl-debug-action-directives*
                  (format #t "~A: ~A~%" (ured "special case: (run %{deps}), deps") deps))
              (let ((tool-dep (cadr deps))
                    (arg-deps (cddr deps)))
                (if :test *mibl-debug-action-directives*
                    (begin
                      (format #t "~A: ~A~%" (ured "tool-dep") tool-dep)
                      (format #t "~A: ~A~%" (ured "arg-deps") arg-deps)))
                `((:cmd (:tool ,(car tool-dep)) (:args ,@(map car arg-deps))))
                ;; (error 'STOP "${deps}")
                ))
            (error 'FIXME
                   "run-dsl %{deps} action w/null args: ~A~%" action))

        ;;case: action == node

        ;; action not %{deps}

        (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
                (let ((cmd-list (apply (car cmd-fn)
                                       (list ws pkg action action-list tools targets deps))))
                                          ;; ws pkg action action-list tools targets deps
                  cmd-list)
                (if-let ((cmd-fn (assoc-val action dune-action-cmds-dsl)))
                        (let ((cmd-list (apply (car cmd-fn)
                                               (list ws pkg
                                                     action-list tools targets deps))))
                          (if :test *mibl-debug-action-directives*
                              (format #t "~A: ~A~%" (bggreen "cmd-list 1") cmd-list))
                          (error 'X "STOP cmd-list 1")
                          cmd-list)
                        (let* ((_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (yellow "adhoc action") action)))
                               (args (expand-cmd-list ws pkg run-dsl tools targets deps))
                               ;; (args (expand-cmd-args* action-args pkg targets deps))
                               (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A\n" (yellow "expanded args") args))))
                          ;; (error 'X "STOP xargs")
                          ;; `((:cmd (:tool ,action)
                          ;;         (:args ,@args)))
                          (list (cons :cmd args))))))))

    ;; (case action
    ;;   ((write-file)
    ;;    (normalize-action-write-file ws pkg action action-list targets deps))
    ;;   ((progn)
    ;;    (normalize-action-progn-dsl ws pkg action-list targets deps))
    ;;   (else ((_ (error 'fixme "run-dsl fixme")))))))

(define (normalize-action-setenv-dsl action stanza)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-SETENV-DSL ~A\n" action))
  '()
  )

;; (define (normalize-action-with-outputs-to-dsl ws pkg action-alist targets deps)
(define (normalize-action-with-accepted-exit-codes-dsl
           ws pkg action-alist tools targets deps)
           ;; item ws pkg targets deps)
  (if :test *mibl-debug-action-directives*
      (format #t "~A: ~A\n"
          (ublue "normalize-action-with-accepted-exit-codes-dsl")
          action-alist))
  ;; e.g. jsoo compiler/tests-jsoo/bin/dune
  ;; (action
  ;;  (with-accepted-exit-codes
  ;;   2
  ;;   (with-outputs-to
  ;;    %{target}
  ;;    (run node %{dep:error1.bc.js}))))
  (let* ((action-assoc (car action-alist))
         (_ (if :test *mibl-debug-action-directives* (format #t "action-assoc: ~A\n" action-assoc)))
         (action (car action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "action: ~A\n" action)))
         (arg1 (cadr action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "arg1: ~A\n" arg1)))
         (subaction-alist (caddr action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction-alist: ~A\n" subaction-alist)))
         (subaction (car subaction-alist))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction: ~A\n" subaction)))
         (cmd (if-let ((cmd-fn (assoc-val subaction
                                          dune-action-cmds-no-dsl)))
                      (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-no-dsl\n")))
                            (cmd-list (apply (car cmd-fn)
                                             (list ws pkg
                                                   subaction subaction-alist
                                                   tools targets deps))))
                        cmd-list)
                      (if-let ((cmd-fn (assoc-val subaction
                                                  dune-action-cmds-dsl)))
                              (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-dsl\n")))
                                    (cmd-list (apply (car cmd-fn)
                                                     (list ws pkg
                                                           subaction-alist
                                                           tools targets deps))))
                                ;; (format #t "~A: ~A~%" (bggreen "w/output cmd-list") cmd-list)
                                ;; (error 'X "STOP cmd-list 3")
                                cmd-list)
                              (begin
                                (format #t "UNHANDLED WST ACTION: ~A\n"
                                        subaction)
                                stanza))
                      )))
    (append cmd
            `((:stdout ,(string->keyword
                         (format #f "~A" arg1)))))))

;; with-stdout-to, with-stderr-to
;; 1st arg: often %{target}, may also be a filename
(define (normalize-action-with-outputs-to-dsl ws pkg action-alist tools targets deps)
  (mibl-trace-entry "normalize-action-with-outputs-to-dsl" action-alist :test *mibl-debug-action-directives*)
  ;; action-list e.g. (with-outputs-to %{target} (cmd ...))
  (let* ((action-assoc action-alist)
         (_ (if :test *mibl-debug-action-directives* (format #t "action-assoc: ~A\n" action-assoc)))
         (action (car action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "action: ~A\n" action)))
         (stdio-kw (if (eq? 'with-stdout-to action)
                       :stdout
                       (if (eq? 'with-stderr-to action)
                           :stderr
                           :stdio)))
         (std-fd (let ((fd (cadr action-assoc)))
                   (if (eq? '%{target} fd)
                       :output
                       `(:fixme ,fd))))
         (_ (if :test *mibl-debug-action-directives* (format #t "std-fd: ~A\n" std-fd)))
         (subaction-list (caddr action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction-list: ~A\n" subaction-list)))
         (subaction (car subaction-list))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction: ~A\n" subaction)))
         (cmd (if-let ((cmd-fn (assoc-val subaction
                                          dune-action-cmds-no-dsl)))
                      (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-no-dsl\n")))
                            (cmd-list (apply (car cmd-fn)
                                             (list ws pkg
                                                   subaction subaction-list
                                                   tools targets deps))))
                        cmd-list)
                      (if-let ((cmd-fn (assoc-val subaction
                                                  dune-action-cmds-dsl)))
                              (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-dsl\n")))
                                    (cmd-list (apply (car cmd-fn)
                                                     (list ws pkg
                                                           subaction-list
                                                           tools targets deps))))
                                ;; (format #t "~A: ~A~%" (bggreen "w/output cmd-list") cmd-list)
                                ;; (error 'X "STOP cmd-list 3")
                                cmd-list)
                              (begin
                                (format #t "UNHANDLED WST ACTION: ~A\n"
                                        subaction)
                                stanza))
                      )))
    (append cmd
            `((,stdio-kw ,std-fd)))))

(define (normalize-action-with-stderr-to-dsl ws pkg action-alist tools targets deps)
  (if :test *mibl-debug-action-directives* (format #t "NORMALIZE-ACTION-WITH-STDERR-TO-DSL ~A\n" action))_
  '()
  )

(define (normalize-action-with-stdin-from-dsl ws pkg action-alist tools targets deps)
  (mibl-trace-entry "normalize-action-with-stdin-from-dsl" action-alist :test *mibl-debug-action-directives*)
  ;; `((:unresolved ,action-alist)))
  (let* ((action-assoc action-alist)
         (_ (if :test *mibl-debug-action-directives* (format #t "action-assoc: ~A\n" action-assoc)))
         (action (car action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "action: ~A\n" action)))
         (arg1 (cadr action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "arg1: ~A\n" arg1)))
         (subaction-alist (caddr action-assoc))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction-alist: ~A\n" subaction-alist)))
         (subaction (car subaction-alist))
         (_ (if :test *mibl-debug-action-directives* (format #t "subaction: ~A\n" subaction)))
         (cmd (if-let ((cmd-fn (assoc-val subaction
                                          dune-action-cmds-no-dsl)))
                      (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-no-dsl\n")))
                            (cmd-list (apply (car cmd-fn)
                                             (list ws pkg
                                                   subaction subaction-alist
                                                   tools targets deps))))
                        cmd-list)
                      (if-let ((cmd-fn (assoc-val subaction
                                                  dune-action-cmds-dsl)))
                              (let ((_ (if :test *mibl-debug-action-directives* (format #t "found cmd-dsl\n")))
                                    (cmd-list (apply (car cmd-fn)
                                                     (list ws pkg
                                                           ;; subaction
                                                           subaction-alist
                                                           tools targets deps))))
                                ;; (format #t "~A: ~A~%" (bggreen "w/output cmd-list") cmd-list)
                                ;; (error 'X "STOP cmd-list 3")
                                cmd-list)
                              (begin
                                (format #t "UNHANDLED WST ACTION: ~A\n"
                                        subaction)
                                stanza))
                      )))
    (append cmd
            `((:stdin ,(string->keyword
                         (format #f "~A" arg1)))))))

(define (-find-item-in-targets item targets)
  (if :test *mibl-debug-action-directives*
      (format #t "~A: ~A in ~A~%" (blue "-find-item-in-targets") item targets))
  (let* ((items (cdr targets))
         (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (red "items") items)))
         (lbl (find-if (lambda (-item)
                         (if :test *mibl-debug-action-directives*
                             (format #t "~A: ~A~%" (white "-item") -item))
                         (let* ((key (car -item))
                                (label (cdr -item))
                                (tgt (cadr label)))
                           (if :test *mibl-debug-action-directives*
                               (format #t "~A: ~A~%" (white "tgt") tgt))
                           (equal? (format #f "~A" item) (cdr tgt))))
                       items)))
    (if :test *mibl-debug-action-directives*
        (format #t "~A: ~A~%" (red "found label") lbl))
    (if lbl (car lbl) #f)))

(define (normalize-action-write-file ws pkg action action-list tools targets deps)
  (mibl-trace-entry "normalize-action-write-file" action :test *mibl-debug-action-directives*)
  (mibl-trace "pkg" (assoc-val :pkg-path pkg) :test *mibl-debug-action-directives*)
  (mibl-trace "action-list" action-list :test *mibl-debug-action-directives*)
  (mibl-trace "targets" targets :test *mibl-debug-action-directives*)
  (mibl-trace "deps" deps :test *mibl-debug-action-directives*)

  (let* ((args (cdr action-list)) ;; (assoc-val 'write-file action-list))
         (mibl-trace-let "args" args :test *mibl-debug-action-directives*)

         (output (car args))
         (mibl-trace-let "output" output :test *mibl-debug-action-directives*)

         ;; (file (car args))
         ;; (_ (if :test *mibl-debug-action-directives* (format #t "file: ~A\n" file)))
         (content `(:content ,(cadr args)))
         (mibl-trace-let "content" content :test *mibl-debug-action-directives*)

         (target (if (null? targets)
                     output
                     ;; get tagged-label from targets, for output
                     (if-let ((t (-find-item-in-targets output targets)))
                             t
                             (-infer-output! output targets pkg))))
         (_ (if :test *mibl-debug-action-directives* (format #t "~A: ~A~%" (red "target") target)))
         )

    ;; update exports table with outfile
    (update-exports-table! ws
                           :FIXME-TAG ;; tag
                           :FIXME-MODES ;; modes
                           target ;; name
                           (assoc-val :pkg-path pkg)
                           target)

    (set-cdr! tools `((:write-file . :unresolved)))
    `(;;  (:output ,@targets)
      (:cmd
       (:tool :write-file)
       ,(cons :args (list target content))))))

    ;; `(:write-file
    ;;   (:out ,outfile)
    ;;   (:content ,(car str))
    ;;   ,(if deps `(:deps ,@(cdr deps)) '(:deps ()))
    ;;   (:raw ,stanza))))

;; expand-deps
;; (:name <dependencies>) available as %{name} in actions.
;; (file <filename>) or simply <filename>
;; (alias <alias-name>): e.g. (alias src/runtest)
;; (alias_rec <alias-name>): recursively in all children directories wherever it is defined. For instance: (alias_rec src/runtest) might depend on (alias src/runtest), (alias src/foo/bar/runtest), …
;; (glob_files <glob>)
;; (source_tree <dir>): depend on all srcs in the subtree at root <dir>
;; (universe): depend on everything in the universe; this is only for dependencies in the installed world, you must still specify all dependencies that come from the workspace.
;; (package <pkg>) depend on all files installed by <package>, as well as on the transitive package dependencies of <package>. This can be used to test a command against the files that will be installed
;; (env_var <var>): depend on the value of the environment variable <var>. If this variable becomes set, becomes unset, or changes value, the target will be rebuilt.
;; (sandbox <config>): require a particular sandboxing configuration. <config> can be one (or many) of:
;; always: the action requires a clean environment.
;; none: the action must run in the build directory.
;; preserve_file_kind: the action needs the files it reads to look like normal files (so dune won’t use symlinks for sandboxing

;; ((chdir %{workspace_root} (run %{libexec:tezos-protocol-environment-packer:s_packer} "sigs" %{deps})))

;; dsl expr will usually contain vars of form %{foo}. such vars may be
;; defined in some other package, e.g.
;; %{libexec:tezos-protocol-environment-packer:s_packer} or locally,
;; within the 'rule' stanza itself. There are (at least) two kinds of such
;; 'local' vars: field names (e.g. field 'deps' => %{deps}),
;; and vars defined within the 'deps' fld, e.g.
;; (:src_dir <filename>) => ${src_dir}

;; external vars will be resolved by emitter (after normalization).

;; output: ((:tool %{libexec:tezos-protocol-environment-packer:s_packer})
;;          (:args ("sigs" %{deps}))
;;          (:vars ((:deps ...)
;;                  (:src_dir TEZOS_PROTOCOL)
;;                  (:other_var ...))))

(define (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
  (if :test *mibl-debug-action-directives*
      (format #t "~A: ~A\n" (ublue "NORMALIZE-CMD-DSL-UNIVERSE") dsl))
  ;; special case: using 'universe' dep and passing e.g. unix.cma
  ;; e.g.
  ;; (rule
  ;;  (targets generated_git_info.ml)
  ;;  (deps (universe) (:script get-git-info.mlt))
  ;;  (action (with-stdout-to %{targets} (run %{ocaml} unix.cma %{script}))))

  ;; 'universe' means: "depend on everything in the universe. This is
  ;; for cases where dependencies are too hard to specify."

  ;; atm we punt and make the developer deal with this by hand
  `((:tool #f)
    (:deps '())
    (:args '())
    (:universe #t) ;; emitter does sth special when it finds this
    (:raw ,dsl))
  )

;;FIXME: for tools use sh to see if executable, not file-exists?
(define resolve-string-arg
  ;; to resolve argstrings with multiple %{} vars we need to loop/recur
  (lambda (pkg-path arg vars)
    (if :test *mibl-debug-action-directives*
        (begin
          (format #t "~A: ~A\n" (ublue "resolve-string-arg")arg)
          (format #t " vars: ~A\n" vars)))
    (if (or (equal? 'bash arg) (equal? "bash" arg))
        'bash
        ;; else scan arg to see if it contains %{} vars, replace if found
        (if-let ((beg-delim (string-contains arg "%{")))
            (let* ((tok-pfx (substring arg (+ beg-delim 2)))
                   (end-delim (string-index tok-pfx
                                            (lambda (ch)
                                              (char=? ch #\}))))
                   (tok (string-take tok-pfx end-delim))
                   (key (string->symbol (string-append ":" tok))))
              (if :test *mibl-debug-action-directives*
                  (begin
                    (format #t "substituting in '~A'\n" arg)
                    (format #t "tok-pfx: ~A\n" tok-pfx)
                    (format #t "tok: ~A\n" tok)
                    (format #t "key: ~A\n" key)))
              (if-let ((val-assoc (assoc key vars)))
                      (let* ((val (cadr val-assoc))
                             (subst (case (car val)
                                      ((:_srcfile)
                                       (let ((r (string-append
                                                 "//"
                                                 (cadr val)
                                                 ":" (caddr val))))
                                         (if :test *mibl-debug-action-directives*
                                             (format #t "_srcfile: ~A\n" r))
                                         r))

                                      ((:_genfile)
                                       (string-append "//"
                                                      (cadr val)
                                                      ":" (caddr val)))
                                      (else
                                       )))
                             ;; (locn (list :_location subst))
                             (locn (string-append "$(location "
                                                  subst ")"))
                             (new (string-replace arg locn
                                                  beg-delim
                                                  (+ beg-delim end-delim
                                                     3 ;; %, {, and }
                                                     ))))
                        (if :test *mibl-debug-action-directives*
                            (format #t "NEW: ~A\n" new))
                        (resolve-string-arg pkg-path new vars))
                      ;; else not a var key
                      arg))
            (let ((arg (normalize-toolname pkg-path arg)))
              (if (file-exists? arg)
                  (list :_srcfile
                        (dirname arg)
                        (basename arg))
                  (if (string-index arg (lambda (ch) (equal ch #\space)))
                      arg
                      (list :_genfile
                            (dirname arg) (basename arg)))))))))

(define (normalize-tool-tag tag)
  (let ((tag-str (if (symbol? tag) (symbol->string tag) tag)))
    (if (string-prefix? "%{" tag-str)
        (string->symbol
         (string-append ":" (substring tag-str 2 (- (length tag-str) 1))))
        tag)))

;; (define (normalize-progn-action pkg stanza-alist)
;;   (format #t "NORMALIZE-PROGN-ACTION: ~A\n" stanza-alist)
;;   #t)

;; (define (Xnormalize-progn-action pkg-path action stanza srcfiles)
;;   (format #t "NORMALIZE-PROGN-ACTION: ~A\n" action)
;;   ;; tezos: (action progn), (action (progn)), (action (progn (...) ...))
;;   ;; missing but maybe possible: (action progn ...)

;;   ;; empty progn: (action (progn)) - forces eval of rule for side-effects?

;;   ;; examples:
;;   ;; (rule (alias runtest)
;;   ;;       (package tezos-protocol-004-Pt24m4xi)
;;   ;;       (deps (alias runtest_sandbox))
;;   ;;       (action (progn)))  <<== null action?
;;   ;; "Building this alias means building the targets of
;;   ;; this rule." Presumably that means deps too.
;;   ;; "The typical use of the alias stanza is to define tests:
;;   ;; (rule (alias   runtest)
;;   ;;       (action (run %{exe:my-test-program.exe} blah)))
;;   ;; "[T]o define a test a pair of alias and
;;   ;; executable stanzas are required."

;;   ;; more common:
;;   ;; (progn
;;   ;;  (bash "touch .depend")
;;   ;;  (run make %{targets} COMPUTE_DEPS=false)
;;   ;;  (bash "rm .depend")))))))

;;   ;; (let* ((rule-alist (cdr stanza))
;;   ;;        (stanza-type (if (assoc 'alias rule-alist) :alias-cmd :run-cmd))

;;   (let* ((rule-alist (cdr stanza))
;;          (alias (assoc 'alias rule-alist))

;;          (progn (cdadr action))
;;          (stanza-type (if (null? progn) :null-cmd :run-cmd))

;;          (_ (if :test *mibl-debug-action-directives* (format #t "progn: ~A\n" progn)))
;;          (deps (assoc 'deps rule-alist))
;;          (_ (if :test *mibl-debug-action-directives* (format #t "deps: ~A\n" deps))))

;;     (let-values (((filedeps vars env-vars universe aliases unresolved)
;;                   (expand-deps pkg-path
;;                                #f
;;                                #f
;;                                deps
;;                                srcfiles)))
;;       (format #t "r filedeps: ~A\n" filedeps)
;;       (format #t "r vars: ~A\n" vars)
;;       (format #t "r env-vars: ~A\n" env-vars)
;;       (format #t "r universe: ~A\n" universe)
;;       (format #t "r aliases: ~A\n" aliases)
;;       (format #t "r unresolved: ~A\n" unresolved)

;;       ;; (for-each (lambda (filedep)
;;       ;;             (format #t "~A\n" filedep))
;;       ;;           (reverse filedeps))
;;       ;; '())))

;;       (let* (
;;              (result `((:pkg ,pkg-path)
;;                        (:raw ,stanza)))
;;              (result (if (null? vars)
;;                          result
;;                          (acons :vars vars result)))
;;              (result (if (null? filedeps)
;;                          result
;;                          (acons :deps filedeps result)))
;;              ;; (result (if (null? outfile)
;;              ;;             result
;;              ;;             (acons (:out outfile) result)))
;;              (result (if-let ((alias (assoc 'alias rule-alist)))
;;                              (acons :alias (last alias) result)
;;                              result)))
;;         `(,stanza-type ,result)))))

    ;; (if (> (length dsl) 1)
    ;;     (format #t "normalize-cmd-dsl ct: ~A\n" (length dsl)))

    ;; (let-values (((filedeps vars env-vars universe aliases unresolved)
    ;;               (expand-rule-deps pkg-path
    ;;                            #f ;; tool-tag
    ;;                            tool
    ;;                            (assoc 'deps rule-alist)
    ;;                            srcfiles)))
    ;; (let ((action-deps (expan
    ;;   (format #t "filedeps: ~A\n" filedeps)
    ;;   (format #t "vars: ~A\n" vars)
    ;;   (format #t "env-vars: ~A\n" env-vars)
    ;;   (format #t "universe: ~A\n" universe)
    ;;   (format #t "aliases: ~A\n" aliases)
    ;;   (format #t "unresolved: ~A\n" unresolved)

      ;; (let ((cmd (if universe
      ;;                (normalize-cmd-dsl-universe pkg-path dsl filedeps vars)
      ;;                (normalize-cmd-dsl pkg-path
      ;;                                   target targets
      ;;                                   dsl
      ;;                                   (if filedeps (reverse filedeps)
      ;;                                       '())
      ;;                                   vars))))

      ;;   (format #t "wso cmd: ~A\n" cmd)
      ;;   ;; (format #t "xDSL: ~A\n" dsl)
      ;;   ;; (let ((deps (assoc ':deps cmd)))
      ;;   ;;   (for-each (lambda (dep)
      ;;   ;;               (format #t "D: ~A\n" dep))
      ;;   ;;             (cadr deps)))

      ;;   `(:rule ;; with-stdout-to
      ;;     (:out ,outfile)
      ;;     (:cmd ,cmd)
      ;;     (:vars ,vars)
      ;;     ;; ,(if (null? filedeps) '() `(:filedeps ,@(cdr filedeps)))
      ;;     (:raw ,stanza)))
    ;; )
    ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dune-action-cmds-no-dsl
  ;; primary cmds that do NOT take a DSL argument
  ;; args: ws pkg subaction subaction-alist targets deps
  `((bash       ,normalize-action-shell-cmd) ;; (bash <cmd>)
    (cat        ,normalize-action-file-op)  ;; (cat <file>)
    (cmp        ,normalize-action-file-op) ;; (cmp <file1> <file2>)
    (copy       ,normalize-action-file-op) ;; (copy <src> <dst>)
    (copy#      ,normalize-action-file-op) ;; copy & add header line
    (diff       ,normalize-action-file-op) ;; (diff <file1> <file2>)
    (diff?      ,normalize-action-file-op) ;; (diff? <file1> <file2>)
    (echo       ,normalize-action-echo-cmd) ;; (echo <string>)
    (system     ,normalize-action-shell-cmd) ;; (system <cmd>) uses sh
    (write-file ,normalize-action-write-file) ;; (write-file <file> <string>)
    ))

(define dune-action-cmds-dsl ;; i.e. cmd 'directives'
  ;; primary cmds that DO take a DSL argument
  ;; args ws pkg action-list targets deps
  `((chdir ,normalize-action-chdir-dsl) ;; (chdir <dir> <DSL>)
    ;; (ignore-<outputs> <DSL>)
    (ignore-outputs ,normalize-action-ignore-outputs-dsl)
    (ignore-stderr ,normalize-action-ignore-stderr-dsl)
    (ignore-stdout ,normalize-action-ignore-stdout-dsl)
    (no-infer ,normalize-action-no-infer-dsl) ;; (no-infer <DSL>)
    ;; (pipe-<outputs> <DSL> <DSL> <DSL>...)
    (pipe-outputs ,normalize-action-pipe-outputs-dsl)
    (pipe-stderr ,normalize-action-pipe-stderr-dsl)
    (pipe-stdout ,normalize-action-pipe-stdout-dsl)
    (progn ,normalize-action-progn-dsl)
    (run ,normalize-action-run-dsl)
    (setenv ,normalize-action-setenv-dsl)     ;; (setenv <var> <value> <DSL>)
                                       ;; NB: (run env FOO=bar ...)
    ;; (with-accepted-exit-codes <pred> <DSL>)
    (with-accepted-exit-codes ,normalize-action-with-accepted-exit-codes-dsl)
    ;; (with-<outputs>-to <file> <DSL>)
    (with-outputs-to ,normalize-action-with-outputs-to-dsl)
    (with-stderr-to ,normalize-action-with-stderr-to-dsl)
    (with-stdout-to ,normalize-action-with-outputs-to-dsl)
    ;; ,normalize-action-with-stdout-to-dsl)
    ;; (with-stdin-from <file> <DSL>)
    (with-stdin-from ,normalize-action-with-stdin-from-dsl)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; called by normalize-action-rule
;; converts (action ...) to mibl list
(define (rule-action:normalize ws pkg stanza-alist tools targets deps) ;; rule stanza
  (if (or :test *mibl-debug-s7* :test *mibl-debug-action-directives*)
      (begin
        (format #t "~A: ~A\n" (ublue "rule-action:normalize") stanza-alist)
        (format #t "~A: ~A~%" (white "targets") targets)
        (format #t "~A: ~A~%" (white "deps") deps)))
  (let* ((action-assoc (assoc 'action stanza-alist))
         (action-alist (car (assoc-val 'action stanza-alist)))
         (_ (if :test *mibl-debug-action-directives* (format #t "  action alist: ~A\n" action-alist)))
         (action (if (pair? (car action-alist)) ;; e.g. (action (tool ...))
                     (caar action-alist)
                     ;; else (action tool ...) - illegal but may happen?
                     (car action-alist)))
         )
    (if :test *mibl-debug-action-directives*
        (format #t "  action cmd: ~A\n" action))

    (if-let ((cmd-fn (assoc-val action dune-action-cmds-no-dsl)))
            (let ((cmd-list (apply (car cmd-fn)
                                   (list ws pkg
                                         action action-alist
                                         tools targets deps))))
              cmd-list)
            (if-let ((cmd-fn (assoc-val action dune-action-cmds-dsl)))
                    (let ((cmd-list (apply (car cmd-fn)
                                           (list ws pkg
                                                 action-alist tools targets deps))))
                      ;; (if :test *mibl-debug-action-directives*
                      ;;     (format #t "~A: ~A~%" (bggreen "normalized cmd-list") cmd-list))
                      ;; (error 'X "STOP cmd-list 4")
                      cmd-list)
                    (begin
                      (format #t "~A: ~A~%" (red "UNHANDLED ACTION:") action)
                      stanza)))))

;; action-list: (bash foo ...) or (bash "foo ...")
;; if arg is a string, we have a problem - no reliable way to parse
;; out the tool, or anything other than pctvars. In some cases the string
;; will be an entire shell script, with function definitions etc.
(define (destructure-bash-cmd action-list)
  (mibl-trace-entry "destructure-bash-cmd" action-list :color red :test *mibl-debug-action-directives*)
  (let* ((subaction-list (cdr action-list)))
    (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-directives*)
    (if (string? (car subaction-list))
        ;; FIXME: what if subaction-list is list of strings?
        (let* ((segs (string-split (car subaction-list) #\space))
               (xlines (map dsl:string->lines subaction-list)))
          (mibl-trace "segs" segs :test *mibl-debug-action-directives*)
          (mibl-trace "expanded lines" xlines :test *mibl-debug-action-directives*)
          `((:shell . bash) (:cmd-lines ,@xlines))) ;; subaction-list)))
        (let ((subcmd (car subaction-list))
              (subargs (cdr subaction-list)))
          (mibl-trace "bash subcmd" subcmd :test *mibl-debug-action-directives*)
          (mibl-trace "bash subargs" subargs :test *mibl-debug-action-directives*)
          (let ((sargs (map dsl:string->lines subargs)))
            (mibl-trace "bash sargs" sargs :test *mibl-debug-action-directives*)
            `((:shell . bash) (:tool . ,subcmd) (:args ,@sargs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (destructure-progn-cmd action-list)
  (mibl-trace-entry "destructure-progn-cmd" action-list :test *mibl-debug-action-directives*)
  (let ((result
         (map (lambda (action)
                (mibl-trace "progn action" action :test *mibl-debug-action-directives*)
                (dsl:action->mibl (list action))
                )
              (list action-list))))
    (mibl-trace "progn result" result :test *mibl-debug-action-directives*)
    (error 'progn "PROGN")
    (let* ((subaction-list (cdr action-list)))
      (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-directives*)
      (if (string? (car subaction-list))
          ;; FIXME: what if subaction-list is list of strings?
          (let* ((segs (string-split (car subaction-list) #\space))
                 (xlines (map dsl:string->lines subaction-list)))
            (mibl-trace "segs" segs :test *mibl-debug-action-directives*)
            (mibl-trace "expanded lines" xlines :test *mibl-debug-action-directives*)
            `((:shell . bash) (:cmd-lines ,@xlines))) ;; subaction-list)))
          (let ((subcmd (car subaction-list))
                (subargs (cdr subaction-list)))
            (mibl-trace "bash subcmd" subcmd :test *mibl-debug-action-directives*)
            (mibl-trace "bash subargs" subargs :test *mibl-debug-action-directives*)
            (let ((sargs (map dsl:string->lines subargs)))
              (mibl-trace "bash sargs" sargs :test *mibl-debug-action-directives*)
              `((:shell . bash) (:tool . ,subcmd) (:args ,@sargs))))))))
