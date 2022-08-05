;; https://dune.readthedocs.io/en/stable/concepts.html#user-actions
(define dune-dsl-cmds
  ;; all except 'run' and 'dynamic-run'
  '(bash cat chdir cmp copy copy# diff diff? echo ignore-outputs-to
ignore-stderr-to ignore-stdout-to no-infer pipe-outputs pipe-stderr
pipe-stdout progn setenv system with-accepted-exit-codes
with-outputs-to with-stderr-to with-stdin-from with-stdout-to
write-file))

(define shell-tools
  '(cat cp copy)) ;; etc

(define shell-tool-kws
  '(::cat ::cp ::copy)) ;; etc

(define shell-tool-map
  '((cat ::cat)
    (cp ::cp)
    (copy ::cp)
    ;; etc
    ))

