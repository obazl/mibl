(if *mibl-debug-s7-loads*
    (format #t "loading libdune.scm~%"))

(provide 'dune.scm)

(autoload 'expanders.scm "dune/expanders.scm")

(autoload 'mibl:parsetree->mibl "libmibl.scm")

(load "dune/conditionals.scm")
(load "dune/aggregates.scm")
(load "dune/constants.scm")
;; (load "dune/debug.scm")
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

;; expand dunefile to miblx (mibl, expanded)
(define (dune->miblx ws)
  (mibl-trace-entry "dune->miblx" ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (pkg-assoc)
                            ;; key: pkg path (string)
                            ;; val: assoc-list
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr pkg-assoc))
                            (if (assoc 'dune (cdr pkg-assoc))

                                (let ((mibl-pkg (dune-pkg->miblx :@ (cdr pkg-assoc))))
                                  ;; (format #t "~A: ~A~%" (red "dune->mibld") mibl-pkg)
                                  (hash-table-set! pkgs (car pkg-assoc) mibl-pkg)
                                  mibl-pkg)
                                (begin
                                  ;; (format #t "~A: ~A~%" (red "dune->mibl: no dune file") pkg-assoc)
                                  (cdr pkg-assoc))
                                ))
                         pkgs)))
        ;; (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist)
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (if *mibl-debug-all*
        (format #t "~A: ~A~%" (blue "mibl pkg ct") (length mpkg-alist)))
    mpkg-alist))

(provide 'dune.scm)

(if *mibl-debug-s7-loads*
    (format #t "loaded dune.scm~%"))
