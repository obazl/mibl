;; if a tgt depends on a static file in other pkg, we need to
;; validate, and we need to make sure that file is accessible either
;; by defining a filegroup for it or by using 'exports_files'.

;; globbed deps handled separately via filegroups

;; if it depends on a dynamically generated file, it should be
;; resolved via the exports table, in another step.

;; to update pkg we must update the hash table
(define (-update-pkg-file-exports! pkgs-ht pkg lbl-path lbl-tgt)
  (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
      (format #t "~A: ~S~%" (ublue "-update-file-exports!") pkg))

  ;; We check for the matching static file; if we find it we add it to
  ;; :exported-files. If not do nothing - assume it is dynamically
  ;; generated and the label will resolve at build time.

  (let ((static-files (assoc-in '(:files :static) (cdr pkg)))
        ;; TODO: handle .ml/.mli and other files as exports
        ;; We expect this to be rare so we'll add support when we find a case.
        ;; (static-structs (assoc-in '(:structs :static) (cdr pkg)))
        ;; (static-sigs (assoc-in '(:structs :static) (cdr pkg)))
        )
    ;; (format #t "~A: ~S~%" (green "static files:") static-files)
    (if (and static-files (member lbl-tgt static-files))
        (begin
          ;; (format #t "~A: ~S~%" (green "HIT") lbl-tgt)
          (if-let ((mibl (assoc-val :mibl (cdr pkg))))
                  (begin
                    ;; update :mibl, add or augment :exported-files stanza
                    )
                  (begin
                    ;; (format #t "~A: ~S~%" (red "pkg before") pkg)
                    ;; (format #t "~A: ~S~%" (red "pkg ht before") (hash-table-ref pkgs-ht (car pkg)))
                    ;; (set-cdr! pkg ... ; NO: won't update the hash-table entry
                    (hash-table-set! pkgs-ht
                                     (car pkg)
                                     (append (cdr pkg)
                                             `((:exported-files ,lbl-tgt))))
                    ;; (format #t "~A: ~S~%" (red "pkg after") pkg)
                    ;; (format #t "~A: ~S~%" (red "pkg ht after") (hash-table-ref pkgs-ht (car pkg)))
                    ))
          )
        ;; if no static files, or no matching static, do nothing
        )
    ))

;; tagged label fmt: (<kw> (:pkg . "path") (:tgt . "string"))
;; e.g.  (:< (:pkg . "subtool/sub") (:tgt . "gen.sh"))
(define (-resolve-tagged-label tlbl this-pkg pkgs-ht)
  (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "-resolve-tagged-label") tlbl))
  (let ((lbl-path (assoc-val :pkg (cdr tlbl)))
        (lbl-tgt  (assoc-val :tgt (cdr tlbl)))
        (this-pkg-path (assoc-val :pkg-path this-pkg)))
    ;; (format #t "~A: ~S~%" (green "lbl-path") lbl-path)
    ;; (format #t "~A: ~S~%" (green "lbl-tgt") lbl-tgt)
    ;; (format #t "~A: ~S~%" (green "this-pkg-path") this-pkg-path)
    (if (equal? lbl-path this-pkg-path)
        (begin
          ;; tgt is pkg-local, skip
          ;; (format #t "SKIPPING\n")
          )
        ;; search other pkgs
        ;; WARNING: for-each over hashtable converts entries to kv pairs;
        ;; set-cdr! on the kv pair does NOT update the hash table.
        (for-each (lambda (pkg)
                    (if (equal? (car pkg) this-pkg-path)
                        (begin) ;; skip this-pkg
                        (begin
                          ;; (format #t "~A: ~S~%" (green "searching pkg") (car pkg))
                          (if (equal? (car pkg) lbl-path)
                              (-update-pkg-file-exports! pkgs-ht pkg lbl-path lbl-tgt))
                          ;; (format #t "~A: ~S~%" (red "pkg after") pkg)
                          ;; (format #t "~A: ~S~%" (red "ht pkg after") (hash-table-ref pkgs-ht (car pkg)))
                          )))
                  pkgs-ht))
    )
  )

;; stanza e.g.
;; (:rule (:deps (::tools (:< (:pkg . "./") (:tgt . "gen.sh"))))
;;        (::outputs (:foo.txt (:pkg . "supertool") (:tgt . "foo.txt")))
;;        (:actions (:cmd (:tool :<) (:args ::outputs))))
;; ws-pkgs: hash table, key: :pkg-path (string)
(define (-resolve-rule-file-exports! stanza this-pkg ws-pkgs)
  (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A\n"
                (ublue "resolve-rule-file-exports!") stanza)
        (format #t "~A: ~A~%" (green "this pkg") this-pkg)
        (format #t "~A: ~A~%" (green "pkgs") ws-pkgs)))
  (let ((deps (assoc-val :deps (cdr stanza)))
        (outputs (assoc-val ::outputs (cdr stanza))))
    (if (truthy? deps)
        (for-each (lambda (dep)
                    ;; (format #t "~A: ~A~%" (green "DEP") dep)
                    (if (equal? (car dep) ::tools)
                        (for-each (lambda (tool)
                                    (-resolve-tagged-label tool this-pkg ws-pkgs)
                                    )
                                  (cdr dep))
                        (begin
                          ;; (format #t "~A: ~A~%" (green "NONTOOL") dep)
                          (-resolve-tagged-label dep this-pkg ws-pkgs)
                          )))
                  deps)))
  ;; (format #t "~A~%" (red "pkgs after"))
  ;; (mibl-pretty-print ws-pkgs)
  ;; (newline)
  )

(define resolve-file-exports!
  (let ((+documentation+ "Resolve deps on static files in other pkgs.")
        (+signature+ '(resolve-filegroups! workspace-id)))
    (lambda (ws-id)
      (let ((ws (assoc-val ws-id *mibl-project*)))
        (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
            (format #t "~%~A for ws: ~A\n"
                    (bgblue "resolve-file-exports!") (assoc :name ws)))
        ;; (assoc-val 'name ws))
        (let* ((pkgs (car (assoc-val :pkgs ws)))
               ;; (_ (if (or *mibl-debug-file-exports* *mibl-debug-s7*) (format #t "PKGS: ~A\n" pkgs)))
               (exports (car (assoc-val :exports ws))))
          ;; (format #t "resolving filegroups for pkgs: ~A\n" (hash-table-keys pkgs))
          ;; (format #t "exports: ~A\n" exports)

          ;; if we find a dep on //foo/gen.sh, add fld :exported-files
          ;; to the foo pkg exporting gen.sh.

          (for-each (lambda (pkg-kv)
                      (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (green "resolving file exports for pkg") (car pkg-kv)))
                      ;; (format #t "pkg: ~A~%" (cdr pkg-kv))
                      (let ((pkg (cdr pkg-kv)))
                        (if-let ((stanzas (assoc-val :mibl (cdr pkg-kv))))
                                (for-each (lambda (stanza)
                                            (if (or *mibl-debug-file-exports* *mibl-debug-s7*)
                                                (format #t "~A: ~A~%" (green "stanza") stanza))
                                            (case (car stanza)
                                             ((:rule) (-resolve-rule-file-exports! stanza pkg pkgs))
                                             ((else) (format #t "~A  ~:A~%" (red "OTHER") (car stanza)))
                                              ))
                                          stanzas)))
                      )
                    pkgs)
          )
        ;; (format #t "RESOLVED: ~A~%" ws)
        ))))
