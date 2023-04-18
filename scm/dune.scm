(if *mibl-debug-s7-loads*
    (format #t "loading dune.scm~%"))

(provide 'dune.scm)

(autoload 'expanders.scm "dune/expanders.scm")

(unless (provided? 'alist.scm)
  (load "alist.scm"))
;; (load "srfi.scm")
;;(load "libc/regex.scm")
(load "string.scm")
;; (load "utils.scm")

(autoload 'mibl:parsetree->mibl "libmibl.scm")

(load "dune/conditionals.scm")
(load "dune/aggregates.scm")
(load "dune/constants.scm")
(load "dune/debug.scm")
(load "dune/deps.scm")
(load "dune/dune_api.scm")

;; (autoload 'dsl:action->mibl "dune/action_dsl.scm")
;; (autoload 'parse-pct-var "dune/action_dsl.scm")
(load  "dune/action_dsl.scm")
(load  "dune/action_directives.scm")
(load "dune/action_deps.scm")

;;(load "dune/action_dsl.scm")
(load "dune/action_shell.scm")

(load "dune/dune_utils.scm")
;; (load "dune/dune_actions.scm")
;; (load "dune/dune_ingest.scm")
;; (load "dune/dune_normalize.scm")
;; (load "dune/dune_stanza_executable.scm")
(load "dune/dune_stanza_fields.scm")
;; (load "dune/dune_stanza_library.scm")
(load "dune/rules.scm")
(load "dune/dune_stanzas.scm")
;; (load "dune/lookup_tables.scm")

(load "dune/file_exports.scm")

(load "dune/cc.scm")
(load "dune/executables.scm")
;; (load "dune/expanders.scm")
(load "dune/fields.scm")
(load "dune/flags.scm")
(load "dune/lib_deps.scm")
(load "dune/libraries.scm")
(load "dune/modules.scm")
(load "dune/opam.scm")
(load "dune/pkg_api.scm")
(load "dune/predicates.scm")
(load "dune/preprocessing.scm")
(load "dune/prologues.scm")
;; (load "dune/actions_with_output_to.scm")
(load "dune/user_actions.scm")
(load "dune/aliases.scm")
(load "dune/resolvers.scm")
(load "dune/shared_deps.scm")
(load "dune/shared_opts.scm")
(load "dune/shared_ppx.scm")
(load "dune/updaters.scm")
(load "dune/tests.scm")
(load "dune/pkg_files.scm")
(load "dune/install.scm")
(load "dune/lexyacc.scm")
(load "dune/menhir.scm")
(load "dune/version.scm")

(load "dune/ppx_inline_tests.scm")

(load "mibl_pp.scm")

(provide 'dune.scm)

(if *mibl-debug-s7-loads*
    (format #t "loaded dune.scm~%"))
