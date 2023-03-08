(define (analyze-libdeps libdeps)
  (if *debugging*
      (format #t "~A: ~A\n" (ublue "analyze-libdeps") libdeps))
  ;; libdeps: raw dune (libraries) fld cdr
  (let recur ((raw libdeps) ;; 'libraries' fld
              (directs '()) ;; directs == public_name, listed in (libraries)
              (selects '())
              ;; (modules '()) ;; not needed?
              )
    (if *debugging*
        (format #t "~A: ~A~%" (uwhite "recurring on") raw))
    (if (null? raw)
        (let* ((seldeps (map (lambda (s)
                               (if *debugging*
                                   (format #t "select: ~A\n" s))
                               (let ((seldeps (assoc-val :deps s))) seldeps))
                             selects))
               (seldeps (fold (lambda (x accum)
                             (if (member x accum)
                                 accum
                                 (cons x accum)))
                           '() seldeps))
               ;; (_ (if *debugging* (format #t "FOLDED: ~A\n" seldeps)))
               (conditionals (map (lambda (s)
                                    (if *debugging*
                                        (format #t "S: ~A\n" s))
                                    (let ((conditionals (alist-delete
                                                         '(:deps) s)))
                                      conditionals))
                                  selects)))
          (if *debugging*
              (begin
                (format #t "~A: ~A~%" (blue "select seldeps") seldeps)
                (format #t "~A: ~A\n" (blue "select conditionals") conditionals)
                (format #t "~A: ~A\n" (blue "directs") directs)))
          ;; (let* ((dirdeps (if (null? directs) '()
          ;;                     (list (cons :remote directs))))
          ;;        (dirdeps (if (null? seldeps) dirdeps
          ;;                     (list (cons :select seldeps)))))
            (values (if (null? directs)
                        '() (cons :remote (reverse directs)))
                    (if (null? seldeps) ;; dirdeps
                        '() seldeps)
                    conditionals
                    ;; modules
                    ))
        (if (pair? (car raw))
            ;; e.g. (select ...)
            (case (caar raw)
              ((select)
            ;; (if (equal? (caar raw) 'select)
               (let ((the-selects (analyze-select (car raw))))
                 (if *debugging*
                     (format #t "~A: ~A~%" (cyan "the-selects") the-selects))
                 (update-selects-list! the-selects)
                 (recur (cdr raw)
                        directs
                        (append selects (list the-selects))
                        ;; (cons (car raw) selects)
                        ;; (libdep->module-name modules)
                        )))
              ((re_export)
               (if *debugging*
                   (format #t "~A: ~A~%" (bgyellow "re_export") raw))
               (recur (cdr raw)
                      (cons (cadar raw) directs)
                       selects
                       ;;(libdep->module-name modules)
                       ))
              (else
               (error 'bad-lib-dep
                      (format #f "~A: ~A~%"
                              (bgred
                               "unrecognizde fld embedded as pair dep")
                              raw)
                      )))

            ;; else (car raw) not a pair
            (if (equal? 'libraries (car raw))
                ;; skip the initial fld name "libraries"
                (recur (cdr raw)
                       directs selects
                       ;;(libdep->module-name modules)
                       )
                (recur (cdr raw)
                       (cons (car raw) directs)
                       selects
                       ;;(libdep->module-name modules)
                       ))))))

