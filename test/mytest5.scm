(define pkg
  (let* ((_   (load "dune.scm"))
         (arg "dune/stanzas/rule/action/mixed/five")
         (pkgs (mibl-load-project arg))
         (pkg (hash-table-ref pkgs arg))
         (normed-pkg (dune-pkg->mibl pkg)))
    normed-pkg))
