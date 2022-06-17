(load "dune/dune_actions.scm")

(begin
  (load "dune.scm")
  (let* ((pkg '((:ws-path "/Users/gar/obazl/mibl/test")
                (:pkg-path "dune/stanzas/rule/deps/glob")
                (:realpath "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/glob")
                (:dune-stanzas
                 (rule (targets foo bar)
                       ;; (deps (glob_files "*.ml"))
                       ;; (deps (glob_files *.ml{,i}))
                       ;; (deps (glob_files ../*.ml{,i}))
                       ;; (deps (glob_files ../relative/*.ml{,i}))
                       (deps (glob_files *.ml)
                             (glob_files ../relative/*.ml{,i}))
                       ;; (deps (glob_files ../*.ml{,i})
                       ;;       (glob_files ../relative/*.ml{,i}))

                       ;; (deps (glob_files *.ml{,i})
                       ;;       (glob_files ../*.ml{,i})
                       ;;       ;; foo.ml
                       ;;       (glob_files ../relative/*.ml{,i}))

                       ;; (deps a.ml a.mli)
                       ;; (deps a.ml a.mli (glob_files ../*.ml{,i}))

                       ;; (deps ../foo.ml ../globtest.ml ../relative/p.ml)

                       (action (run %{<} %{targets}))))))
         (stanza (cdar (assoc-val :dune-stanzas pkg))))
    (expand-action-deps pkg stanza)))
