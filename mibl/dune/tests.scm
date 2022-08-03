;; (define (normalize-stanza-test pkg-path srcfiles stanza)

(define (dune-test->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (blue "dune-test->mibl") stanza)

  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (_ (format #t "~A: ~A~%" (white "pkg-path") pkg-path))
         (stanza-alist (cdr stanza))
         (privname (assoc 'name stanza-alist)))
    (let ((t (list :test ;; (car stanza)
                   (remove
                    '()
                    (map (lambda (fld-assoc)
                           (case (car fld-assoc)
                             ((name) (normalize-stanza-fld-name
                                      pkg-path privname stanza-alist))
                             ((public_name) '())
                             ;; ((name) (normalize-stanza-fld-name (cadr fld-assoc)))
                             ((flags) (normalize-stanza-fld-flags (cadr fld-assoc)))
                             ((foreign_stubs)
                              (normalize-stanza-fld-foreign_stubs (cdr fld-assoc)))
                             (else fld-assoc)))
                         (cdr stanza))))))
      (format #t "~A: ~A~%" (red "mibl t stanza") t)
      t)))

(define (normalize-stanza-tests pkg-path ocaml-srcs stanza)
  ;; (display (format #f "dir: ~A" pfx)) (newline)
  ;; (display (format #f "normalize-stanza-tests: ~A" stanza)) (newline)
  (dune-executables->mibl :tests
                                pkg-path ocaml-srcs stanza))
