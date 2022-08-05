(define (pct-var->keyword v)
  (format #t "~A: ~A~%" (blue "pct-var->var") v)
  (let* ((v (format #f "~A" v))
         (len (length v))
         (vstr (substring v 2 (- len 1)))
         (kw (string->keyword vstr)))
    (format #t "vstr ~A\n" vstr)
    (format #t "kw ~A\n" kw)
    kw))

;;;;;;;;;;;;;;;;;;
(define (parse-subcmd dsl subcmd cmd-list)
  (format #t "parse-subcmd: ~A\n\t~A\n\t~A\n" dsl subcmd cmd-list)
  (if (null? dsl)
      subcmd
      (if (pair? (car dsl))
          (let ((subcmd (parse-subcmd (car dsl) '() '())))
            (format #t "SUBCMD: ~A\n" subcmd)
            (parse-action-dsl (cdr dsl) initial-cmd
                              (append cmd-list subcmd)))
          (parse-subcmd (cdr dsl) (append subcmd (list (car dsl)))
                        cmd-list))))

(define (cmd-list->cmd-alist dsl)
  (format #t "CMD-LIST->CMD-ALIST: ~A\n" dsl)
  (let recur ((dsl dsl)
              (cmd-alist '()))
    (format #t "recur dsl: ~A\n" dsl)
    (format #t "recur cmds: ~A\n" cmd-alist)
    (if (null? dsl)
        cmd-alist
      (if (pair? (car dsl))
          (recur (cdr dsl) (append cmd-alist (list (car dsl))))

          ;; gather leading atoms into a sublist
          ;; .e.g. (a b c (d e)) => ((a b c) (d e))
          (let-values (((subcmd trailing)
                        (let* subrecur ((subdsl dsl)
                                        (subcmd '()))
                          (format #t "subrecur subdsl: ~A\n" subdsl)
                          (format #t "subrecur cmds: ~A\n" subcmd)
                          (if (null? subdsl)
                              (values subcmd subdsl)
                              (if (pair? (car subdsl))
                                  (values subcmd subdsl)
                                  (subrecur (cdr subdsl)
                                            (append subcmd (list (car subdsl)))))))))
            (recur trailing (append (list subcmd) cmd-alist)))))))


          ;; (let* ((subcmd (parse-subcmd (car dsl) '() '())))
          ;;   (format #t "xSUBCMD: ~A\n" subcmd)
          ;;   (format #t " (initial cmd: ~A)\n" initial-cmd)
          ;;   (cmd-list->cmd-alist
          ;;    (cdr dsl) initial-cmd (append cmd-list subcmd)))
          ;;   ;; (parse-action-dsl (cdr dsl) initial-cmd (append cmd-list subcmd)))
          ;; ;; car must be an action verb like chdir
          ;; ;; this can only happen for the initial cmd
          ;; ;; task: accumulate initial-cmd
          ;; (cmd-list->cmd-alist
          ;;    (cdr dsl) initial-cmd (append cmd-list subcmd)))))
          ;; ;; (parse-action-dsl (cdr dsl) (append initial-cmd (list (car dsl)))
          ;; ;;                   cmd-list))))

;; (define (parse-cmd-list action-alist targets deps)
;;   (format #t "PARSE-CMD-LIST: ~A\n" action-alist)
;;   (format #t "  TARGETS: ~A\n" targets)
;;   (format #t "  DEPS: ~A\n" deps)
;;   ;; we don't validate dune files
;;   ;; (if (> (length targets) 1)
;;   ;;     (error 'bad-targets-fld
;;   ;;            "'targets' fld of 'with-stdout-to' takes 1 arg\n"))
;;   (let* (         ;; ASSUMPTION: dsl is 2nd and last arg to 'with-outputs-to'
;;          ;; dsl may contain embedded actions, e.g. 'chdir', 'setenv', etc.

;;          ;; action may be a sequence of subactions, e.g.
;;          ;; (chdir %{foo} (run ${bar} ...))
;;          (cmd-list (parse-action-dsl dsl '() '()))
;;          (_ (format #t "cmd-list: ~A\n" cmd-list))
;;          )
;;     cmd-list))
