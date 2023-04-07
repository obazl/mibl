(if *mibl-debug-s7-loads*
    (format #t "loading dune/pct_vars.scm\n"))

;; string args to e.g. echo may contain percent vars.
;; e.g. (echo "let cppo_version = \"%{version:cppo}\"")

;; this routine must parse the pct vars in arg s.
;; if the pct vars can be resolved to strings we return a string;
;; otherwise we return a list of substrings with pct vars intermixed.
;; e.g. (:string "let cppo_version = \"" (% (:version . cppo)) "\"")
;; client can then resolve and join.

(define (string-parse-pct-vars s)
  (mibl-trace-entry "string-parse-pct-vars" s)
  ;; 1. parse out the pct-vars
  ;; 2. resolve them if possible
  ;; 3. construct result
  '((:string "let test_var = \"" (:% (:FIXME . TEST)) "\"")))

