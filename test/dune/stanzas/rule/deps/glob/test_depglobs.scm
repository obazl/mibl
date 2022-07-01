(load "dune/dune_actions.scm")

(define glob-patterns
  ;; quoted and unquoted
  '(("*.ml" ("a.ml" "b.ml"))
    (*.ml ("a.ml" "b.ml"))
    ("*.ml{,i}" ("b.mli" "b.ml" "a.ml" "a.mli"))
    (*.ml{,i} ("b.mli" "b.ml" "a.ml" "a.mli"))
    ;; relative paths
    (../*.ml{,i} ("dune/stanzas/rule/deps/globtest.mli"
                    "dune/stanzas/rule/deps/globtest.ml"))
    ("../*.ml{,i}" ("dune/stanzas/rule/deps/globtest.mli"
                    "dune/stanzas/rule/deps/globtest.ml"))
    ("../relative/*.ml{,i}"
     ("dune/stanzas/rule/deps/relative/r.ml"
         "dune/stanzas/rule/deps/relative/q.mli"
         "dune/stanzas/rule/deps/relative/p.mli"
         "dune/stanzas/rule/deps/relative/p.ml"))
    (../relative/*.ml{,i}
     ("dune/stanzas/rule/deps/relative/r.ml"
         "dune/stanzas/rule/deps/relative/q.mli"
         "dune/stanzas/rule/deps/relative/p.mli"
         "dune/stanzas/rule/deps/relative/p.ml"))
    ;; ((glob_files ../*.ml{,i}))))
    ))

(define name-deps
  `(,vector
    (a.ml a.mli)
    (a.ml a.mli (glob_files ../*.ml{,i}))
    (../foo.ml ../globtest.ml ../relative/p.ml)))

(define mixed-deps
  `(,vector
    (a.ml a.mli (glob_files ../*.ml{,i}))
    (../foo.ml ../globtest.ml ../relative/p.ml)))

(define paths
  '((:ws-path "/Users/gar/obazl/mibl/test")
    (:pkg-path "dune/stanzas/rule/deps/glob")
    (:realpath "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/glob")))

(define targets '(targets foo bar))

(define action
  '(action (run %{<} %{targets})))

(define rule
  `(rule ,targets
         (deps ,(vector-ref glob-deps 1))
         ,action))

(define pkg
  `(pkg
    ,(append
      paths
      `((:dune-stanzas
         (rule ,targets
               (deps ,(vector-ref glob-deps 5))
               ,action))))))

(define (subset? l1 l2)
  (or (null? l1)
      (and (member  (car l1) l2)
           (subset? (cdr l1) l2))))

(define (set-equal? l1 l2)
  (and (subset? l1 l2)
       (subset? l2 l1)))

(define ESC #o033)
(define CCRED "[0;31m")

(begin
  (load "dune.scm")
  (let* ((paths '((:ws-path "/Users/gar/obazl/mibl/test")
                  (:pkg-path "dune/stanzas/rule/deps/glob")
                  (:realpath
                   "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/glob")))

         (rs (map (lambda (pattern)
                    (format #t "PATTERN: ~A\n" pattern)
                    (let* ((p (car pattern))
                           (stanza-alist `((targets foo bar)
                                           ;; INSERT PATTERN HERE:
                                           (deps (glob_files ,p))
                                           (action (run %{<} %{targets}))))
                           (result (expand-action-deps paths stanza-alist)))
                      (format #t "RES: ~A" result)
                      (format #t "EXP: ~A" (cadr pattern))
                      (set-equal? (assoc-val ':_ result)
                                  (cadr pattern))))
                  ;; result))
                  glob-patterns)))
    rs))
