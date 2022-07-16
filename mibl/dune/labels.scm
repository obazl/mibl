;; %{bin:foo} etc. Dune uses those prefixes to reference installation
;; locations. Since we do not do any installation, they're just labels
;; to us. E.g. when we process an executable with public name foo, we
;; add 'bin:foo' to the exports table. Any target that uses it (in a
;; rule action for example) will refer to it as 'bin:foo', so we can
;; just look it up to find its Bazel label.

(define (update-exports-table! ws tag name pkg-path)
  (format #t "~A: ~A -> ~A\n" (blue "update-exports-table!") name pkg-path)
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws -mibl-ws-table))))
         (key (case tag
                ((:bin) (symbol (format #f "bin:~A" name)))
                ((:lib) (symbol (format #f "lib:~A" name)))
                ((:libexec) (symbol (format #f "libexec:~A" name)))
                (else name)))
         (spec `((,tag #t)
                 (:pkg ,pkg-path)
                 (:tgt ,(format #f "~A" name)))))
    (format #t "hidden exports tbl: ~A\n" exports)

    (format #t "adding ~A to exports tbl\n" name)
    (hash-table-set! exports key spec)))
;;(car (assoc-val :pkg-path pkg)))))

(define (-fixup-deps! ws stanza)
  (format #t "~A: ~A\n" (blue "-fixup-deps!") stanza)
  (let* ((exports (car (assoc-val :exports ws)))
         (stanza-alist (cdr stanza)))
    (format #t "fixup hidden exports tbl: ~A\n" exports)
    (case (car stanza)
      ((:ns-archive)
       (let ((deps (assoc-val :deps stanza-alist)))
         (format #t "ns-archive deps: ~A~%" deps)))

      ((:executable)
       ;; FIXME: also handle :dynamic
       (let ((deps (assoc-in '(:compile :deps :fixed) stanza-alist)))
         (format #t "exec deps: ~A~%" deps)
         (if (not (null? deps))
             (let ((new (map (lambda (dep)
                               (format #t "dep: ~A\n" dep)
                               (let ((exp (hash-table-ref exports dep)))
                                 (format #t "val: ~A\n" exp)
                                 (format #f "//~A:~A" exp dep)))
                             (cdr deps))))
               (set-cdr! deps new)))
               ))

      ((:rule)
       ;; tool and deps
       (let ((targets (assoc-val :targets stanza-alist))
             (deps (assoc-val :deps stanza-alist))
             (action (assoc-val :action stanza-alist))
             (tool (assoc-in '(:action :cmd :tool) stanza-alist)))
         (format #t "rule action: ~A~%" action)
         (format #t "rule tool: ~A~%" tool)
         (format #t "rule targets: ~A~%" targets)
         (format #t "rule deps: ~A~%" deps)
         (let* ((tool-label (hash-table-ref exports (cadr tool)))
                (_ (format #t "tool-label: ~A~%" tool-label))
                (pkg (car (assoc-val :pkg tool-label)))
                (tgt (car (assoc-val :tgt tool-label)))
                (label (format #f "//~A:~A" pkg tgt)))
           (format #t "tool-label: ~A\n" tool-label)
           (set-cdr! tool (list label))
           '())))

      (else
       (error 'unhandled (format #f "-fixup-deps!: ~A\n" (car stanza)))))))

(define resolve-labels
  (let ((+documentation+ "Map dune target references to bazel labels using exports table.")
        (+signature+ '(resolve-labels workspace)))
    (lambda (ws)
      (format #t "~A for ws: ~A\n" (blue "resolve-labels") ws)
              ;; (assoc-val 'name ws))
      (let* ((pkgs (car (assoc-val :pkgs ws)))
             (_ (format #t "PKGS: ~A\n" pkgs))
             (exports (car (assoc-val :exports ws))))
        (format #t "resolving labels for pkgs: ~A\n"
                (hash-table-keys pkgs))
        (format #t "exports: ~A\n" exports)
        (for-each (lambda (kv)
                    (format #t "pkg path: ~A~%" (car kv))
                    (format #t "pkg: ~A~%" (cdr kv))
                    (if-let ((stanzas (assoc-val :dune (cdr kv))))
                            (for-each (lambda (stanza)
                                        (-fixup-deps! ws stanza)
                                        (format #t "stanza: ~A~%" stanza))
                                      stanzas))
                    )
                  pkgs)
        ))))

