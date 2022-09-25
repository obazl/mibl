;; https://dune.readthedocs.io/en/stable/dune-files.html#alias

;; The alias stanza adds dependencies to an alias or specifies an
;; action to run to construct the alias.

;; The syntax is as follows:

;; (alias
;;  (name    <alias-name>)
;;  (deps    <deps-conf list>)
;;  <optional-fields>)

;; <action>, an action for constructing the alias. See the User
;; Actions section for more details. Note that this is removed in Dune
;; 2.0, so users must port their code to use the rule stanza with the
;; alias field instead.

(define (dune-alias->mibl ws pkg stanza)
  (format #t "~A: ~A\n" (blue "dune-alias->mibl") pkg)
  (format #t "~A~%" (red "WARNING: unimplemented: alias stanzas"))
  ;; (format #t "~A: ~A\n" (white "stanza") stanza)
  ;;(list (cons ':alias (cdr stanza)))
  (-alias->miblark stanza)
  )
