(format #t "loading dune/predicates.scm\n")

(define (structfile? s)
  (let ((f (if (symbol? s) (symbol->string s) s)))
    (eq? 0 (fnmatch "*.ml" f 0))))

(define (sigfile? s)
  (let ((f (if (symbol? s) (symbol->string s) s)))
    (eq? 0 (fnmatch "*.mli" f 0))))

(format #t "loaded dune/predicates.scm\n")
