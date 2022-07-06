(define pkg
  (let* ((_   (load "dune.scm"))
         (arg "dune/stanzas/rule/action/chdir")
         (pkgs (load-dune arg))
         (pkg (hash-table-ref pkgs arg))
         (normed-pkg (dune-pkg->mibl pkg)))
    normed-pkg))
