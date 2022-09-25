(define (-fixup-progn-cmd! ws c targets deps)
  (format #t "~A: ~A\n" (ublue "-fixup-progn-cmd!") c)
  c)

(define (-module->filename m pkg)
  (format #t "~A: ~A~%" (blue "-module->filename") m)
  (let ((pkg-modules (assoc-val :modules pkg))
        (pkg-structs (assoc-val :structures pkg)))
    (format #t "~A: ~A~%" (white "pkg-modules") pkg-modules)
    (format #t "~A: ~A~%" (white "pkg-structs") pkg-structs))
  "mytest.ml")

;; (define (-resolve-module-deps m stanza pkg)
;;   (format #t "~A: ~A~%" (blue "-resolve-module-deps") m)
;;   (format #t "~A: ~A~%" (white "stanza") stanza)
;;   (let* ((m-fname (-module->filename m pkg))
;;          (pkg-path (car (assoc-val :pkg-path pkg)))
;;          (cmd (format #f "ocamldep -modules ~A/~A" pkg-path m-fname))
;;          (_ (format #t "~A: ~A~%" (green "cmd") cmd))
;;          (ocamldeps
;;           (system cmd #t)))
;;     (format #t "~A: '~A'~%" (red "ocamldeps") ocamldeps)
;;     ;; search pkg-local modules for dep
;;     ;; else search exports table for dep
;;     ;; else assume opam
;;     (list m '(:deps (:foo :bar)))))

(define (-fixup-std-dep-form ws pkg dep exports)
  (format #t "~A: ~A~%" (ublue "-fixup-std-dep-form") dep)
  (case (cdr dep)
    ((::unresolved ::opam-pkg)
     ;; (if (eq? ::import (last dep))
     (let ((exp (hash-table-ref exports
                                (car dep))))
       (format #t "~A: ~A~%" (ured "XP") exp)
       (if exp
           (let* ((pkg (assoc-val :pkg exp))
                  (_ (format #t "~A: ~A~%" (ured "pkg") pkg))
                  (tgt (assoc-val :tgt exp))
                  (_ (format #t "~A: ~A~%" (ured "tgt") tgt)))
             (cons (car dep) exp))
           ;; else try std ocaml pkgs
           (begin
             (format #t "~A: ~A~%" (bgred "ocaml-std-pkgs") ocaml-std-pkgs)
             (format #t "~A: ~A~%" (bgred "dep key") (car dep))
             (if-let ((x (assoc-val (car dep) ocaml-std-pkgs)))
                     ;; (format #f "@ocaml//lib/~A" (car dep))
                     (cons (car dep)
                           `((:ws . "@ocaml")
                             (:pkg .
                                   ,(format #f "lib/~A"
                                            (keyword->symbol (car dep))))
                             (:tgt . ,(keyword->symbol (car dep)))))
                     dep)))))
    ;; ((::fixme)
    ;;  ;; side-effect: update filegroups table
    ;;  ;; FIXME: instead, add :fg dep?
    ;;  (format #t "~A: ~A~%" (yellow "export keys")
    ;;          (hash-table-keys exports))
    ;;  (format #t "~A: ~A~%" (yellow "exports")
    ;;          exports)
    ;;  (let* ((exp (hash-table-ref exports
    ;;                              (car dep)))
    ;;         (_ (format #t "~A: ~A~%" (yellow "exp") exp))
    ;;         (-pkg (assoc-val :pkg exp))
    ;;         (_ (format #t "~A: ~A~%" (yellow "pkg") -pkg))
    ;;         (pkg-path (assoc-val :pkg-path pkg))
    ;;         )
    ;;    (update-filegroups-table! ;; ws pkg-path tgt pattern
    ;;     ws pkg-path ;; (car (assoc-val :name ws))
    ;;     -pkg ::all "*")
    ;;    (cons (car dep)
    ;;          (list (car exp)
    ;;                (cons :tgt "__all__")))))
    (else dep)))

(define ocaml-std-pkgs
  '((bigarray . bigarray)
    (compiler-libs . compiler-libs)
    (compiler-libs.common . compiler-libs/common)
    (compiler-libs.bytecomp . compiler-libs/bytecomp)
    (compiler-libs.toplevel . compiler-libs/toplevel)
    (dynlink . dynlink)
    (num . num/core)
    (ocamldoc . ocamldoc)
    (stdlib . stdlib)
    (:stdlib . stdlib)
    (str . str)
    (threads . threads)
    (unix . unix)))

(define (-fixup-dep-sym ws-id dep pkg-path exports)
  (format #t "~A: ~A~%" (bgblue "-fixup-dep-sym") dep)
  ;; possible keys for dep 'foo:
  ;; 'foo, :foo, :lib:foo, :exe:foo, etc.
  ;; assume ref is to lib, so search for :lib:foo

  ;; builtin ocaml pkgs
  (if-let ((x (assoc-val dep ocaml-std-pkgs)))
          (string->symbol (format #f "@ocaml//~A" x))
          ;; try :lib:foo
          (let* ((key (string->keyword
                       (format #f "lib:~A" dep)))
                 (_ (format #t "~A: ~A~%" (uwhite "trying") key))
                 (resolved (hash-table-ref exports key)))
            (if resolved
                (let* ((pkg (assoc-val :pkg resolved))
                       (_ (format #t "~A: ~A~%" (ured "pkg") pkg))
                       (tgt (assoc-val :tgt resolved))
                       (_ (format #t "~A: ~A~%" (ured "tgt") tgt)))
                  ;;(cons dep resolved)
                  ;;(string->symbol (format #f "BBBB//~A:~A" pkg tgt)))
                  (if (equal? pkg pkg-path)
                      (string->symbol (format #f ":~A" tgt))
                      (string->symbol (format #f "//~A:~A" pkg tgt))))
                ;; else :foo
                (let* ((key (string->keyword
                             (format #f "~A" dep)))
                       (_ (format #t "~A: ~A~%" (uwhite "trying2") key))
                       ;; (_ (format #t "~A: ~A~%" (bgblue "exports") exports))
                       ;; (_ (debug-print-exports-table ws-id))
                       (resolved (hash-table-ref exports key)))
                  (if resolved
                      (let* ((pkg (assoc-val :pkg resolved))
                             (_ (format #t "~A: ~A~%" (ured "pkg") pkg))
                             (tgt (assoc-val :tgt resolved))
                             (_ (format #t "~A: ~A~%" (ured "tgt") tgt)))
                        (if (equal? pkg pkg-path)
                            (string->symbol (format #f ":~A" tgt))
                            (string->symbol (format #f "//~A:~A" pkg tgt))))
                      ;; else 'foo
                      (let* ((key (string->symbol
                                   (format #f "~A" dep)))
                             (_ (format #t "~A: ~A~%" (uwhite "trying3") key))
                             (resolved (hash-table-ref exports key)))
                        (if resolved
                            (let* ((pkg (assoc-val :pkg resolved))
                                   (_ (format #t "~A: ~A~%" (ured "pkg") pkg))
                                   (tgt (assoc-val :tgt resolved))
                                   (_ (format #t "~A: ~A~%" (ured "tgt") tgt)))
                              (cons dep resolved))
                            (let ((segs (string-split (format #f "~A" key) ".")))
                              (format #t "~A: ~A~%" (ured "unresolved; assume opam") key)
                              (if (= 1 (length segs))
                                  (string->symbol (format #f "@~A//lib/~A" dep dep))
                                  (string->symbol (format #f "@~A//lib/~{~A~^/~}" (car segs) (cdr segs)))))))))))))

(define (-fixup-conditionals! ws pkg stanza)

  (format #t "~A: ~A\n" (bgblue "-fixup-conditionals!") stanza)
  (if (not (eq? :menhir (car stanza)))
      (if-let ((conditionals (if-let ((dc
                                       (assoc-in '(:deps :conditionals)
                                                 (cdr stanza))))
                                     dc #f)))
              (if (truthy? conditionals)
                  (begin
                    (format #t "~A: ~A~%" (blue "conditionals") (cdr conditionals))
                    (for-each (lambda (conditional)
                                (format #t "~A: ~A~%" (bgblue "conditional")
                                        conditional)

                                ;;FIXME: selectors are always external pkgs (i.e. with '@')?
                                ;; even for proj libs?
                                (for-each (lambda (selector)
                                            (format #t "~A: ~A~%" (bgblue "selector")
                                                    selector)
                                            (let ((resolution (find-in-exports ws (car selector))))
                                              (format #t "~A: ~A~%" (bgblue "selector resolution") resolution)
                                              (set-cdr! selector
                                                        (list
                                                         (cdr selector)
                                                         (if resolution
                                                             (string-append
                                                              (format #f "//~A"
                                                                      (assoc-val :pkg resolution))
                                                              ":"
                                                              (format #f "~A"
                                                                      (assoc-val :tgt resolution)))
                                                             (string->symbol
                                                              (format #f "@~A//lib/~A"
                                                                      (car selector) (car selector)))))))
                                            ;; (set-car! selector (format #f "//bzl/import:~A" (car selector)))
                                            )
                                          (assoc-val :selectors conditional)))
                              (cdr conditionals))))
              )))

;; FIXME: rename
(define (-fixup-stanza! ws-id pkg stanza)
  (format #t "~A: ~A\n" (bgblue "-fixup-stanza!") stanza)
  (case (car stanza)
    ((:install) (values))
    (else (let* ((ws (assoc-val ws-id -mibl-ws-table))
                 (exports (car (assoc-val :exports ws)))
                 (pkg-path (car (assoc-val :pkg-path pkg)))
                 (stanza-alist (cdr stanza)))
            ;; (debug-print-exports-table ws-id)

            (-fixup-conditionals! ws-id pkg stanza)

            (case (car stanza)

              ((:executable :test)
               (format #t "~A: ~A~%" (ublue "fixup") (car stanza))
               ;; FIXME: also handle :dynamic
               (let (;; (modules (assoc-in '(:compile :manifest :modules) stanza-alist))
                     (compile-deps (assoc-in '(:deps :remote) stanza-alist))
                     (stanza-deps (assoc-in '(:deps :remote) stanza-alist)))
                 ;; (format #t "x compile modules: ~A~%" modules)
                 (format #t "x compile deps: ~A~%" compile-deps)
                 (format #t "x stanza deps: ~A~%" stanza-deps)
                 (if compile-deps ;; (not (null? compile-deps))
                     (begin
                       (format #t "~A: ~A~%" (ured "resolving dep labels 1") compile-deps)
                       (let* ((exports (car (assoc-val :exports ws)))
                              (ppx (if-let ((ppx (assoc-val :ppx stanza-alist)))
                                           ppx #f))
                              (_ (format #t "~A: ~A~%" (ublue "ppx") ppx))
                              (fixdeps
                               (map (lambda (dep)
                                      (format #t "~A: ~A~%" (uwhite "fixup dep A") dep)
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws-id pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg-path exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized :archive dep type") dep)))))
                                    (cdr compile-deps))))
                         (format #t "~A: ~A~%" (ured "fixed-up compile-deps") fixdeps)
                         (set-cdr! compile-deps fixdeps)
                         (set-car! compile-deps :resolved)))

                     ;; else no compile-deps
                     )
                     ;; (let ((new (map (lambda (dep)
                     ;;                   ;; (format #t "~A: ~A\n" (uyellow "dep") dep)
                     ;;                   (let ((exp (hash-table-ref exports dep)))
                     ;;                     ;; (format #t "~A: ~A\n" (uyellow "ht val") exp)
                     ;;                     (if exp
                     ;;                         (string->symbol (format #f "//~A:~A" exp dep))
                     ;;                         ;; assume opam label:
                     ;;                         (string->symbol (format #f "@~A//:~A" dep dep)))))
                     ;;                 (cdr deps))))
                     ;;   (set-cdr! deps new)
                     ;;   (set-car! deps :resolved))
                 ;; (if modules
                 ;;     (let ((new (map (lambda (m)
                 ;;                       (format #t "module: ~A\n" m)
                 ;;                       (let ((exp (hash-table-ref exports m)))
                 ;;                         (format #t "importing: ~A\n" exp)
                 ;;                         (if exp
                 ;;                             (format #f "//~A:~A" exp m)
                 ;;                             (-resolve-module-deps m stanza pkg))))
                 ;;                     (cdr modules))))
                 ;;       (set-cdr! modules new)))
                 ;; (error 'fixme "STOP labels")
                 ))

              ((:rule)
               (format #t "~A: ~A~%" (ublue "fixup :rule") stanza-alist)
               (let* ((targets (assoc-val :outputs stanza-alist))
                      (_ (format #t "targets: ~A~%" targets))
                      (deps (if-let ((deps (assoc :deps stanza-alist)))
                                    ;; (if (null? deps) '() (car deps))
                                    deps
                                    #f))
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

                 ;; fixup-deps
                 ;; (if-let ((deps (if-let ((deps (assoc :deps stanza-alist)))
                 ;;                     ;; (if (null? deps) '() (car deps))
                 ;;                     deps #f)))
                 (if deps
                     (begin
                       (format #t "~A: ~A~%" (ured "resolving dep labels 2") deps)
                       (let ((exports (car (assoc-val :exports ws)))
                             (fixdeps
                              (map (lambda (dep)
                                     (format #t "~A: ~A~%" (uwhite "fixup dep B") dep)
                                     (if (eq? (car dep) ::tools)
                                         (begin
                                           (format #t "~A: ~A~%" (bgred "::TOOLS") (caadr dep))
                                           (cond
                                            ((eq? ::unresolved (cdadr dep))
                                             (begin
                                               (format #t "~A~%" (bgred "IMPORT TOOL"))
                                               (format #t "~A~%" (red "export keys"))
                                               (for-each (lambda (k)
                                                           (format #t "~A: ~A~%" (ured "key") k))
                                                         (sort! (hash-table-keys exports) sym<?))
                                               (if-let ((import (hash-table-ref exports (caadr dep))))
                                                       (begin
                                                         (format #t "~A: ~A~%" (bgred "importing") import)
                                                         (list ::tools
                                                               (cons (caadr dep)
                                                                           (list (assoc :pkg import)
                                                                                 (assoc :tgt import))
                                                                           ;; (format #f "//~A:~A"
                                                                           ;;         (assoc-val :pkg import)
                                                                           ;;         (assoc-val :tgt import))
                                                                           ))
                                                         )
                                                       (begin
                                                         (format #t "~A: ~A~%" (red "no import for tool") (caadr dep))
                                                         ;; (error 'STOP "STOP no import")
                                                         ;; assume (rashly) that form is e.g. :tools/version/gen/gen.exe
                                                         (let* ((kw (caadr dep))
                                                                (t (keyword->symbol kw))
                                                                (path (dirname t)))
                                                           `(::tools
                                                             (,kw
                                                              ,(cons :pkg (if (string=? "./" path)
                                                                              pkg-path path))
                                                              ,(cons :tgt (basename t))))
                                                           )))))
                                            ((eq? ::unresolved (cdadr dep))
                                             )
                                           ;; else treat it just like a std dep
                                           (else (-fixup-std-dep-form ws-id pkg dep exports))))
                                         ;; else std dep form: (:foo (:pkg...)(:tgt...))
                                         (-fixup-std-dep-form ws-id pkg dep exports)))
                                   (cdr deps))))
                         (format #t "~A: ~A~%" (ured "fixed-up deps") fixdeps)
                         (set-cdr! deps fixdeps))))
                 ;; (format #t "~A: ~A~%" (ured "reset deps") deps)

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

              ((:archive :ns-archive :library :ns-library)
               (format #t "~A: ~A~%" (blue "fixup") (car stanza))
               ;; (let ((deps (assoc-val :deps stanza-alist)))
               ;;   (format #t "archive deps: ~A~%" deps)))
               (let* ((deps (if-let ((deps (assoc-in '(:deps :remote) stanza-alist)))
                                    deps #f))
                      (_ (format #t "~A: ~A~%" (blue "deps") deps))
                      (ppx (if-let ((ppx (assoc-val :ppx stanza-alist)))
                                   ppx #f))
                      (_ (format #t "~A: ~A~%" (ublue "ppx") ppx))
                      )
                 (if deps
                     (begin
                       (format #t "~A: ~A~%" (ured "resolving dep labels 3") deps)
                       (let ((exports (car (assoc-val :exports ws)))
                             (fixdeps
                              (map (lambda (dep)
                                     (format #t "~A: ~A~%" (uwhite "fixup dep") dep)
                                     (cond
                                      ((list? dep)
                                       ;; std dep form: (:foo (:pkg...)(:tgt...))
                                       (-fixup-std-dep-form ws pkg dep exports))
                                      ((symbol? dep)
                                       (-fixup-dep-sym ws-id dep pkg-path exports))
                                      (else (error 'fixme
                                                   (format #f "~A: ~A~%" (bgred "unrecognized :archive dep type") dep)))))
                                   (cdr deps))))
                         (format #t "~A: ~A~%" (ured "fixed-up deps") fixdeps)
                         (set-cdr! deps fixdeps)
                         (set-car! deps :resolved))))
                 (if ppx
                     (begin
                       (format #t "~A: ~A~%" (ured "resolving ppx") ppx)
                       (let* ((exports (car (assoc-val :exports ws)))
                              (ppx-deps (assoc :manifest ppx))
                              (_ (format #t "~A: ~A~%" (bgred "ppx-deps") ppx-deps))
                              (fixppx
                               (map (lambda (dep)
                                      (format #t "~A: ~A~%" (uwhite "fixup ppx-dep") dep)
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg-path exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized ppx-dep type") dep)))))
                                    (cdr ppx-deps))))
                         (format #t "~A: ~A~%" (ured "fixed-up ppx-deps") fixppx)
                         (set-cdr! ppx-deps fixppx)
                         ;; (set-car! ppx-deps :resolved)
                         ;; (error 'STOP "stop ppx")
                         )))))

              ;; ((:library)
              ;;  (format #t "~A~%" (ublue "fixup :library"))
              ;;  (let* ((deps (if-let ((deps (assoc-in '(:deps :remote) stanza-alist)))
              ;;                       deps #f))
              ;;         (_ (format #t "deps: ~A~%" deps))
              ;;         )
              ;;    (if deps
              ;;        (begin
              ;;          (format #t "~A: ~A~%" (ured "resolving dep labels") deps)
              ;;          (let ((exports (car (assoc-val :exports ws)))
              ;;                (fixdeps
              ;;                 (map (lambda (dep)
              ;;                        (format #t "~A: ~A~%" (uwhite "fixup dep") dep)
              ;;                        (cond
              ;;                         ((list? dep)
              ;;                          ;; std dep form: (:foo (:pkg...)(:tgt...))
              ;;                          (-fixup-std-dep-form ws pkg dep exports))
              ;;                         ((symbol? dep)
              ;;                          (-fixup-dep-sym ws-id dep pkg-path exports))
              ;;                         (else (error 'fixme
              ;;                                      (format #f "~A: ~A~%" (bgred "unrecognized :archive dep type") dep)))))
              ;;                      (cdr deps))))
              ;;            (format #t "~A: ~A~%" (ured "fixed-up deps") fixdeps)
              ;;            (set-cdr! deps fixdeps)
              ;;            (set-car! deps :resolved))))))

              ;;  (format #t "~A~%" (magenta "fixup :library"))
              ;;  (let ((manifest (assoc-val :manifest stanza-alist))
              ;;        (deps (assoc-val :deps stanza-alist)))
              ;;    (format #t "library deps: ~A~%" deps)))

              ((:ocamllex :ocamlyacc :menhir :env :testsuite :tuareg :alias) (values))

              (else
               (error 'fixme
                      (format #t "~A: ~A~%" (bgred "unhandled fixup stanza") stanza))))))))

;; updates stanzas. initial conditions:
;; * manifest deps derived from static file inventory
(define resolve-labels!
  (let ((+documentation+ "Map dune target references to bazel labels using exports table.")
        (+signature+ '(resolve-labels! workspace)))
    (lambda (ws-id)
      (let ((ws (assoc-val ws-id -mibl-ws-table)))
        (format #t "~%~A for ws: ~A\n"
                (bgred "resolve-labels!") (assoc :name ws))
        ;; (assoc-val 'name ws))
        (let* ((pkgs (car (assoc-val :pkgs ws)))
               ;; (_ (format #t "PKGS: ~A\n" pkgs))
               (exports (car (assoc-val :exports ws))))
          ;; (format #t "resolving labels for pkgs: ~A\n" (hash-table-keys pkgs))
          ;; (format #t "exports: ~A\n" exports)
          (for-each (lambda (kv)
                      (format #t "~A: ~A~%" (ublue "resolving pkg") (car kv))
                      ;; (format #t "pkg: ~A~%" (cdr kv))
                      (let ((pkg (cdr kv)))
                        (if-let ((stanzas (assoc-val :dune (cdr kv))))
                                (for-each (lambda (stanza)
                                            (-fixup-stanza! ws-id pkg stanza)
                                            (format #t "stanza: ~A~%" stanza))
                                          stanzas)))
                      )
                    pkgs)
          )))))

