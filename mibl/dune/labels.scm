;; %{bin:foo} etc. Dune uses those prefixes to reference installation
;; locations. Since we do not do any installation, they're just labels
;; to us. E.g. when we process an executable with public name foo, we
;; add 'bin:foo' to the exports table. Any target that uses it (in a
;; rule action for example) will refer to it as 'bin:foo', so we can
;; just look it up to find its Bazel label.

(define (update-exports-table! ws tag name pkg-path)
  (format #t "~A: ~A -> ~A\n" (magenta "update-exports-table!") name pkg-path)
  (let* ((exports (car (assoc-val :exports
                                  (assoc-val ws -mibl-ws-table))))
         (key (case tag
                ((:bin) (symbol (format #f "bin:~A" name)))
                ((:lib) (symbol (format #f "lib:~A" name)))
                ((:libexec) (symbol (format #f "libexec:~A" name)))
                (else name)))
         (tag (case tag
                ((:bin :lib :libexec) (list (cons tag #t)))
                (else '())))
         (spec `(,@tag
                 (:pkg ,pkg-path)
                 (:tgt ,(format #f "~A" name)))))
    (format #t "hidden exports tbl: ~A\n" exports)

    (format #t "adding ~A to exports tbl\n" name)
    (hash-table-set! exports key spec)
    (format #t "updated exports tbl: ~A\n" exports)))

(define (update-exports-table-with-targets! ws targets pkg-path)
  (format #t "~A: ~A~%" (magenta "update-exports-table-with-targets!") targets)
  (if targets
      (for-each (lambda (target)
              (format #t "~A: ~A~%" (red "target") target)
              (let* ((pkg-tgt (cdr target))
                     (pkg (assoc-val :pkg pkg-tgt))
                     (tgt (assoc-val :tgt pkg-tgt)))
                (format #t "~A: ~A~%" (magenta "pkg") pkg)
                (format #t "~A: ~A~%" (magenta "tgt") tgt)

                (update-exports-table! ws :_ tgt pkg)

               ;; (case (car target)
                ;;   ((::)
                ;;    (update-exports-table! ws :_ (cadr target) pkg-path))

                ;;   ((:_)
                ;;    (error 'fixme "unhandled :_ target"))

                ;;   (else
                ;;    (if (list? (cadr target))
                ;;        ;; (:foo.sh (:pkg "foo/bar") (:tgt "foo.sh"))
                ;;        (error 'fixme (format #f "~A" "unhandled  target"))
                ;;        ;; else (:foo.sh "foo.sh")
                ;;        (update-exports-table! ws :_ (cadr target) pkg-path)))
                ;;   )
                ))
            (cdr targets))))

(define (update-filegroups-table! ws pkg-path tgt pattern)
  (format #t "~A: ~A~%" (magenta "update-filegroups-table!") pkg-path)
  (format #t "~A: ~A~%" (green "tgt") tgt)
  (format #t "~A: ~A~%" (green "pattern") pattern)

  (let* ((filegroups (car (assoc-val :filegroups
                                     (assoc-val ws -mibl-ws-table))))
         (glob? (string-index pattern (lambda (ch)
                                              (equal? ch #\*)))))

    (format #t "hidden filegroups tbl: ~A\n" filegroups)
    (format #t "adding ~A:~A to filegroups tbl\n" pkg-path tgt)

    (let ((fgroups (hash-table-ref filegroups pkg-path)))
      (format #t "~A: ~A~%" (red "fgroups") fgroups)
      (if fgroups
          (hash-table-set! filegroups pkg-path
                           (append
                            fgroups
                            (list (cons tgt (if glob?
                                                (list (cons :glob pattern))
                                                pattern)))))
          ;; else new
          (hash-table-set! filegroups pkg-path
                           (list (cons tgt (if glob?
                                               (list (cons :glob pattern))
                                               pattern)))))
      (format #t "updated filegroups tbl: ~A\n" filegroups))))

(define (-fixup-progn-cmd! ws c targets deps)
  (format #t "~A: ~A\n" (blue "-fixup-progn-cmd!") c)
  c)

(define (-fixup-stanza! ws stanza)
  (format #t "~A: ~A\n" (blue "-fixup-stanza!") stanza)
  (let* ((exports (car (assoc-val :exports ws)))
         (stanza-alist (cdr stanza)))
    (format #t "~A: ~A\n" (green "fixup hidden exports tbl") exports)
    (case (car stanza)
      ((:ns-archive)
       (format #t "~A~%" (magenta "fixup :ns-archive"))
       (let ((deps (assoc-val :deps stanza-alist)))
         (format #t "ns-archive deps: ~A~%" deps)))

      ((:library)
       (format #t "~A~%" (magenta "fixup :library"))
       (let ((deps (assoc-val :deps stanza-alist)))
         (format #t "ns-archive deps: ~A~%" deps)))

      ((:executable)
       (format #t "~A~%" (magenta "fixup :executable"))
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
       (format #t "~A: ~A~%" (magenta "fixup :rule") stanza-alist)
       (let* ((targets (assoc-val :outputs stanza-alist))
              (_ (format #t "targets: ~A~%" targets))
              (deps (if-let ((deps (assoc-val :deps stanza-alist)))
                            (if (null? deps) '() (car deps))
                            '()))
              (_ (format #t "deps: ~A~%" deps))
              (action (if-let ((action (assoc-val :actions stanza-alist)))
                              action
                              (if-let ((action
                                        (assoc-val :progn stanza-alist)))
                                      action
                                      (error 'bad-action "unexpected action in :rule"))))
              (_ (format #t "action: ~A~%" action))
              (tool (assoc-in '(:actions :cmd :tool) stanza-alist)))
         (format #t "Tool: ~A~%" tool)
         (format #t "Action: ~A~%" action)
         (format #t "stanza-alist: ~A~%" stanza-alist)

         ;; :actions is always a list of cmd; for progn, more than one
         (if (assoc :actions stanza-alist)
             (begin
               (for-each (lambda (c)
                           (format #t "PROGN cmd: ~A~%" c)
                           (-fixup-progn-cmd! ws c targets deps))
                         action))
             ;; else? actions always have a :cmd?
             (begin
               (format #t "rule action: ~A~%" action)
               (format #t "rule tool: ~A~%" tool)
               (format #t "rule targets: ~A~%" targets)
               (format #t "rule deps: ~A~%" deps)
               (error 'unhandled action "unhandled action")
               ;; (if-let ((tool-label (hash-table-ref exports (cadr tool))))
               ;;         (let* ((_ (format #t "tool-label: ~A~%" tool-label))
               ;;                (pkg (car (assoc-val :pkg tool-label)))
               ;;                (tgt (car (assoc-val :tgt tool-label)))
               ;;                (label (format #f "//~A:~A" pkg tgt))
               ;;                (_ (format #t "tool-label: ~A\n" tool-label)))
               ;;           (set-cdr! tool (list label)))
               ;;         ;; FIXME: handle deps
               ;;         '())
               ))))

      (else
       (error 'fixme (format #f "-fixup-stanza! unhandled stanza: ~A\n" (car stanza)))))))

;; updates stanzas
(define resolve-labels!
  (let ((+documentation+ "Map dune target references to bazel labels using exports table.")
        (+signature+ '(resolve-labels workspace)))
    (lambda (ws)
      (format #t "~A for ws: ~A\n" (blue "resolve-labels") ws)
              ;; (assoc-val 'name ws))
      (let* ((pkgs (car (assoc-val :pkgs ws)))
             ;; (_ (format #t "PKGS: ~A\n" pkgs))
             (exports (car (assoc-val :exports ws))))
        ;; (format #t "resolving labels for pkgs: ~A\n" (hash-table-keys pkgs))
        ;; (format #t "exports: ~A\n" exports)
        (for-each (lambda (kv)
                    (format #t "resolving pkg: ~A~%" (car kv))
                    ;; (format #t "pkg: ~A~%" (cdr kv))
                    (if-let ((stanzas (assoc-val :dune (cdr kv))))
                            (for-each (lambda (stanza)
                                        (-fixup-stanza! ws stanza)
                                        (format #t "stanza: ~A~%" stanza))
                                      stanzas))
                    )
                  pkgs)
        ))))

