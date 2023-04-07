(if *mibl-debug-s7-loads*
    (format #t "loading dune/version.scm\n"))

;; dune has a complicated method of interpreting %{version:<package>}

;; Case: cppo
;; (rule
;;  (targets cppo_version.ml)
;;  (action
;;   (with-stdout-to
;;    %{targets}
;;    (echo "let cppo_version = \"%{version:cppo}\""))))

;; Package version: https://dune.readthedocs.io/en/stable/advanced-topics.html#package-version

;; dune-build-info library: https://dune.readthedocs.io/en/stable/dune-libs.html#build-info

;; algorithm:
;; 1. check for version fld of package stanza of same dunefile
;; 2. check for toplevel version fld of dune-project file
;; 3. run vcs cmd, e.g. `git describe --always --dirty --abbrev=7`

;; To convert to bazel: the vcs check must be obtained via Bazel's
;; stamping mechanism?

(define (handle-dune-version-pct-var ws pkg pct-var)
  (mibl-trace-entry "handle-dune-version-pct-var" pct-var)
  "testversion-0.0.0"
  )


