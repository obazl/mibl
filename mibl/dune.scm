;; (format #t "mibl: dune.scm loading...") (newline)

(load "alist.scm")
;; (load "srfi.scm")
(load "libc/regex.scm")
(load "string.scm")
;; (load "utils.scm")

(load "dune/constants.scm")
(load "dune/dune_api.scm")
(load "dune/actions.scm")
(load "dune/shell_actions.scm")

(load "dune/dune_utils.scm")
(load "dune/dune_action_deps.scm")
;; (load "dune/dune_actions.scm")
(load "dune/dune_ingest.scm")
;; (load "dune/dune_normalize.scm")
;; (load "dune/dune_stanza_executable.scm")
(load "dune/dune_stanza_fields.scm")
;; (load "dune/dune_stanza_library.scm")
(load "dune/rules.scm")
(load "dune/dune_stanzas.scm")
(load "dune/lookup_tables.scm")

(load "dune/executables.scm")
(load "dune/expanders.scm")
(load "dune/fields.scm")
(load "dune/flags.scm")
(load "dune/labels.scm")
(load "dune/libraries.scm")
(load "dune/modules.scm")
(load "dune/pkg_api.scm")
(load "dune/predicates.scm")
(load "dune/ppx.scm")
;; (load "dune/actions_with_output_to.scm")
(load "dune/emit.scm")
(load "dune/miblark.scm")
(load "dune/aliases.scm")
(load "dune/updaters.scm")
(load "dune/tests.scm")
(load "dune/pkg_files.scm")
(load "dune/install.scm")

;; (format #t "loaded mibl/dune.scm") (newline)
