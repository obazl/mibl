;; (format #t "mibl: dune.scm loading...") (newline)

(load "alist.scm")
;; (load "srfi.scm")
(load "string.scm")
;; (load "utils.scm")

(load "dune/dune_utils.scm")
(load "dune/dune_action_deps.scm")
(load "dune/dune_actions.scm")
(load "dune/dune_ingest.scm")
(load "dune/dune_normalize.scm")
(load "dune/dune_stanza_executable.scm")
(load "dune/dune_stanza_fields.scm")
(load "dune/dune_stanza_library.scm")
(load "dune/dune_stanza_rule.scm")
(load "dune/dune_stanzas.scm")
(load "dune/lookup_tables.scm")

(load "dune/fields.scm")
(load "dune/flags.scm")
(load "dune/modules.scm")
(load "dune/normalize.scm")
(load "dune/pkg_api.scm")

(load "dune/actions_with_output_to.scm")

;; (format #t "loaded mibl/dune.scm") (newline)
