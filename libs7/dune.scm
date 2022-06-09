(format #t "Oibl: dune.scm loading...") (newline)

(load "alist.scm")
(load "srfi.scm")
(load "string.scm")
(load "utils.scm")

(load "dune/dune_utils.scm")
(load "dune/dune_actions.scm")
(load "dune/dune_ingest.scm")
(load "dune/dune_normalize.scm")
(load "dune/dune_stanza_executable.scm")
(load "dune/dune_stanza_fields.scm")
(load "dune/dune_stanza_library.scm")
(load "dune/dune_stanza_rule.scm")
(load "dune/dune_stanzas.scm")
(load "dune/lookup_tables.scm")

(display "loaded oibl: dune.scm") (newline)
