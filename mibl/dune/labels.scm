
(define (-fixup-deps! ws stanza)
  (format #t "~A: ~A\n" (blue "-fixup-deps") stanza)
  (let* ((exports (car (assoc-val 'exports ws)))
         (stanza-alist (cdr stanza)))
    (case (car stanza)
      ((:ns-archive)
       (let ((deps (assoc-val :deps stanza-alist)))
         (format #t "ns-archive deps: ~A~%" deps)))

      ((:executable)
       ;; FIXME: also handle :dynamic
       (let ((deps (assoc-in '(:compile :deps :fixed) stanza-alist)))
         (format #t "exec deps: ~A~%" deps)
         (if (not (null? deps))
             (let ((new (map (lambda (dep)
                               (format #t "dep: ~A\n" dep)
                               (let ((exp (hash-table-ref exports dep)))
                                 (format #t "val: ~A\n" exp)
                                 (format #f "//~A:~A" exp dep)))
                             (cdr deps))))
               (set-cdr! deps new)))
               ))
      (else
       (error 'unhandled (format #f "fixme: ~A\n" (car stanza)))))))

(define resolve-labels
  (let ((+documentation+ "Map dune target references to bazel labels")
        (+signature+ '(resolve-labels workspace)))
    (lambda (ws)
      (format #t "~A for ws: ~A\n" (blue "resolve-labels")
              (assoc-val 'name ws))
      (let ((pkgs (car (assoc-val 'pkgs ws)))
            (exports (car (assoc-val 'exports ws))))
        (format #t "resolving labels for pkgs: ~A\n"
                (hash-table-keys pkgs))
        (format #t "exports: ~A\n" exports)
        (for-each (lambda (kv)
                    (format #t "pkg path: ~A~%" (car kv))
                    (format #t "pkg: ~A~%" (cdr kv))
                    (if-let ((stanzas (assoc-val :dune (cdr kv))))
                            (for-each (lambda (stanza)
                                        (-fixup-deps! ws stanza)
                                        (format #t "stanza: ~A~%" stanza))
                                      stanzas))
                    )
                  pkgs)
        ))))

