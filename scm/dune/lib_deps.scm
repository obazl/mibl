(define (-fixup-progn-cmd! ws c targets deps)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-progn-cmd!") c))
  c)

(define (-module->filename m pkg)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (blue "-module->filename") m))
  (let ((pkg-modules (assoc-val :modules pkg))
        (pkg-structs (assoc-val :structures pkg)))
    (if *mibl-debug-s7*
        (begin
          (format #t "~A: ~A~%" (white "pkg-modules") pkg-modules)
          (format #t "~A: ~A~%" (white "pkg-structs") pkg-structs))))
  "mytest.ml")

;; (define (-resolve-module-deps m stanza pkg)
;;   (format #t "~A: ~A~%" (blue "-resolve-module-deps") m)
;;   (format #t "~A: ~A~%" (white "stanza") stanza)
;;   (let* ((m-fname (-module->filename m pkg))
;;          (pkg-path (assoc-val :pkg-path pkg))
;;          (cmd (format #f "ocamldep -modules ~A/~A" pkg-path m-fname))
;;          (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (green "cmd") cmd)))
;;          (ocamldeps
;;           (system cmd #t)))
;;     (format #t "~A: '~A'~%" (red "ocamldeps") ocamldeps)
;;     ;; search pkg-local modules for dep
;;     ;; else search exports table for dep
;;     ;; else assume opam
;;     (list m '(:deps (:foo :bar)))))

(define (-fixup-std-dep-form ws pkg dep exports)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-fixup-std-dep-form") dep))
  (if (equal? (car dep) ::tools)
      dep ;; validate :pkg fld???
      (case (cdr dep)
        ((::unresolved ::opam-pkg)
         ;; (if (eq? ::import (last dep))
         (let ((exp (hash-table-ref exports
                                    (car dep))))
           (if *mibl-debug-s7*
               (format #t "~A: ~A~%" (ured "XP") exp))
           (if exp
               (let* ((pkg (assoc-val :pkg exp))
                      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "pkg") pkg)))
                      (tgt (assoc-val :tgt exp))
                      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "tgt") tgt))))
                 (cons (car dep) exp))
               ;; else try std ocaml pkgs
               (begin
                 (if *mibl-debug-s7*
                     (begin
                       (format #t "~A: ~A~%" (bgred "ocaml-std-pkgs") ocaml-std-pkgs)
                       (format #t "~A: ~A~%" (bgred "dep key") (car dep))))
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
        ;;         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (yellow "exp") exp)))
        ;;         (-pkg (assoc-val :pkg exp))
        ;;         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (yellow "pkg") -pkg)))
        ;;         (pkg-path (assoc-val :pkg-path pkg))
        ;;         )
        ;;    (update-filegroups-table! ;; ws pkg-path tgt pattern
        ;;     ws pkg-path ;; (car (assoc-val :name ws))
        ;;     -pkg ::all "*")
        ;;    (cons (car dep)
        ;;          (list (car exp)
        ;;                (cons :tgt "__all__")))))
        (else dep))))

(define ocaml-std-pkgs
  '((bigarray . bigarray)
    (compiler-libs . compiler-libs)
    (compiler-libs.common . compiler-libs/common)
    (compiler-libs.bytecomp . compiler-libs/bytecomp)
    (compiler-libs.toplevel . compiler-libs/toplevel)
    (dynlink . dynlink)
    (num . num/core)
    (ocamldoc . ocamldoc)
    ;; (stdlib . stdlib)
    ;; (:stdlib . stdlib)
    (str . str)
    (threads . threads)
    (unix . unix)))

;; a dep sym, e.g. (:deps (:remote bigarray parsexp sexplib0)) may be:
;; :here (current pkg), :local (other pkg in ws), :remote (other bazel ws)
;; :builtin (e.g. bigarray, num), or :opam

;; Dune's (libraries ...) only lists pkgs, :here should not
;; occur. But we also use this routine to resolve deps in pkg-files,
;; so we must check for :here deps.

;; Support for multiple wss in one project not yet supported, so we
;; also do not check for :remote.
(define (-fixup-dep-sym ws-id dep pkg exports)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-fixup-dep-sym") dep))
  ;; possible keys for dep 'foo:
  ;; 'foo, :foo, :lib:foo, :exe:foo, etc.
  ;; assume ref is to lib, so search for :lib:foo

  (if-let ((x (assoc-val dep ocaml-std-pkgs)))
          ;; builtin ocaml pkgs - distributed with but not automatically handled by compiler
          (begin
            (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "builtin") x))
            ;;(string->symbol (format #f "@ocaml//~A" x))
            `(:builtin . ,x)
            )
          ;; else try :here
          (if-let ((module-tlbl (find-module-in-pkg dep pkg)))
                  (begin
                    (format #t "FOUND :here dep: ~A\n" module-tlbl)
                    `(:here . ,dep))

                  ;; else try :local by lookup in exports tbl
                  ;; requires one lookup per possible key
                  ;; first lookup, :lib:foo
                  (let* ((pkg-path (assoc-val :pkg-path pkg))
                         (key (string->keyword (format #f "lib:~A" dep)))
                         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "trying 1") key)))
                         (resolved (hash-table-ref exports key)))
                    (if resolved
                        (let* ((pkg (assoc-val :pkg resolved))
                               (_ (if *mibl-debug-s7*
                                      (begin
                                        (format #t "~A: ~A~%" (ured "pkg") pkg)
                                        (format #t "~A: ~A~%" (ured "this pkg-path") pkg-path))))
                               (tgt (assoc-val :tgt resolved))
                               (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "tgt") tgt))))
                          ;; (if (equal? pkg pkg-path)
                          ;;     (string->symbol (format #f ":~A" tgt))
                          ;;     (string->symbol (format #f "//~A:~A" pkg tgt)))
                          `(:local (:path . ,pkg) (:tgt . ,tgt))
                          )
                        ;; else second lookup, :foo
                        (let* ((key (string->keyword
                                     (format #f "~A" dep)))
                               (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "trying2") key)))
                               ;; (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (bgblue "exports") exports)))
                               ;; (_ (mibl-debug-print-exports-table ws-id))
                               (resolved (hash-table-ref exports key)))
                          (if resolved
                              (let* ((pkg (assoc-val :pkg resolved))
                                     (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "pkg") pkg)))
                                     (tgt (assoc-val :tgt resolved))
                                     (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "tgt") tgt))))
                                ;; (if (equal? pkg pkg-path)
                                ;;     (string->symbol (format #f ":~A" tgt))
                                ;;     (string->symbol (format #f "//~A:~A" pkg tgt)))
                                `(:local (:path . ,pkg) (:tgt . ,tgt)))
                              ;; else third lookup, 'foo
                              (let* ((key (string->symbol
                                           (format #f "~A" dep)))
                                     (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "trying3") key)))
                                     (resolved (hash-table-ref exports key)))
                                (if resolved
                                    (let* ((pkg (assoc-val :pkg resolved))
                                           (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "pkg") pkg)))
                                           (tgt (assoc-val :tgt resolved))
                                           (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ured "tgt") tgt))))
                                      ;; (cons dep resolved)
                                      `(:local (:path . ,pkg) (:tgt . ,tgt)))
                                    ;; else not in exports tbl
                                    (let ((segs (string-split (format #f "~A" key) ".")))
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (red "unresolved; assume opam") key))
                                      `(:opam . ,key)
                                      ;; (if (= 1 (length segs))
                                      ;;     (string->symbol (format #f "@~A//lib/~A" dep dep))
                                      ;;     (string->symbol (format #f "@~A//lib/~{~A~^/~}" (car segs) (cdr segs))))
                                      ))))))))))

(define (-fixup-conditionals! ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-conditionals!") stanza))
  (if (not (member (car stanza) '(:diff :menhir)))
      (if-let ((conditionals (if-let ((dc
                                       (assoc-in '(:deps :conditionals)
                                                 (cdr stanza))))
                                     dc #f)))
              (if (truthy? conditionals)
                  (begin
                    (if *mibl-debug-s7*
                        (format #t "~A: ~A~%" (blue "conditionals") (cdr conditionals)))
                    (for-each (lambda (conditional)
                                (if *mibl-debug-s7*
                                    (format #t "~A: ~A~%" (bgblue "conditional")
                                            conditional))

                                ;;FIXME: selectors are always external pkgs (i.e. with '@')?
                                ;; even for proj libs?
                                (for-each (lambda (selector)
                                            (if *mibl-debug-s7*
                                                (format #t "~A: ~A~%" (bgblue "selector")
                                                        selector))
                                            (let ((resolution (find-in-exports ws (car selector))))
                                              (if *mibl-debug-s7*
                                                  (format #t "~A: ~A~%" (bgblue "selector resolution") resolution))
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
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-stanza!") stanza))
  (case (car stanza)
    ((:install) (values))
    (else (let* ((ws (assoc-val ws-id *mibl-project*))
                 (exports (car (assoc-val :exports ws)))
                 (pkg-path (assoc-val :pkg-path pkg))
                 (stanza-alist (cdr stanza)))
            ;; (mibl-debug-print-exports-table ws-id)

            (-fixup-conditionals! ws-id pkg stanza)

            (case (car stanza)

              ((:executable :test)
               (if *mibl-debug-s7*
                   (format #t "~A: ~A~%" (ublue "x fixup") (car stanza)))
               ;; FIXME: also handle :dynamic
               (let* (;; (modules (assoc-in '(:compile :manifest :modules) stanza-alist))
                     (compile-deps (assoc-in '(:deps :remote) stanza-alist))
                     (stanza-deps (assoc-in '(:deps :remote) stanza-alist))

                      (ppx (if-let ((ppx (assoc-val :ppx stanza-alist)))
                                   ppx #f))
                      (ppxex (if-let ((ppxes (assoc-val :ppxes stanza-alist)))
                                     ppxes #f))
                      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ublue "ppx") ppx)))
                      (ppx-codeps (if-let ((ppx-codeps (assoc
                                                        :ppx-codeps stanza-alist)))
                                          ppx-codeps #f))
                     )
                 ;; (format #t "x compile modules: ~A~%" modules)
                 (if *mibl-debug-s7*
                     (begin
                       (format #t "x compile deps: ~A~%" compile-deps)
                       (format #t "x stanza deps: ~A~%" stanza-deps)))
                 (if compile-deps ;; (not (null? compile-deps))
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ured "resolving dep labels 1") compile-deps))
                       (let* ((exports (car (assoc-val :exports ws)))
                              (ppx (if-let ((ppx (assoc-val :ppx stanza-alist)))
                                           ppx #f))
                              (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ublue "ppx") ppx)))
                              (fixdeps
                               (map (lambda (dep)
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (uwhite "fixup dep A") dep))
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws-id pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized :archive dep type") dep)))))
                                    (cdr compile-deps))))
                         (if *mibl-debug-s7*
                             (format #t "~A: ~A~%" (ured "fixed-up compile-deps") fixdeps))
                         (set-cdr! compile-deps fixdeps)
                         (set-car! compile-deps :Resolved)))

                     ;; else no compile-deps
                     )

                 (if ppx
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ured "resolving ppx") ppx))
                       (let* ((exports (car (assoc-val :exports ws)))
                              (ppx-deps (assoc :manifest ppx))
                              (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (bgred "ppx-deps") ppx-deps)))
                              (fixppx
                               (map (lambda (dep)
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (uwhite "fixup ppx-dep") dep))
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized ppx-dep type") dep)))))
                                    (cdr ppx-deps))))
                         (if *mibl-debug-s7*
                             (format #t "~A: ~A~%" (ured "fixed-up ppx-deps") fixppx))
                         (set-cdr! ppx-deps fixppx)
                         ;; (set-car! ppx-deps :resolved)
                         ;; (error 'STOP "stop ppx")
                         )))
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

              ;; ((:diff)
              ;;  (error 'FIXME
              ;;         (format #f "unhandled labels :diff" )))

              ((:rule :diff :node :ocamlc
                      :bindiff-test :diff-test
                      :write-file)
               (if *mibl-debug-s7*
                   (format #t "~A: ~A, ~A~%" (ublue "fixup") (car stanza) stanza-alist))
               (let* ((targets (assoc-val :outputs stanza-alist))
                      (_ (if *mibl-debug-s7* (format #t "targets: ~A~%" targets)))
                      (deps (if-let ((deps (assoc :deps stanza-alist)))
                                    ;; (if (null? deps) '() (car deps))
                                    deps
                                    #f))
                      (_ (if *mibl-debug-s7* (format #t "deps: ~A~%" deps)))
                      (action (if-let ((action (assoc-val :actions stanza-alist)))
                                      action
                                      (if-let ((action
                                                (assoc-val :progn stanza-alist)))
                                              action
                                              (error 'bad-action "unexpected action in :rule"))))
                      (_ (if *mibl-debug-s7* (format #t "action: ~A~%" action)))
                      (tool (assoc-in '(:actions :cmd :tool) stanza-alist)))
                 (if *mibl-debug-s7*
                     (begin
                       (format #t "Tool: ~A~%" tool)
                       (format #t "Action: ~A~%" action)
                       (format #t "stanza-alist: ~A~%" stanza-alist)))

                 ;; fixup-deps
                 ;; (if-let ((deps (if-let ((deps (assoc :deps stanza-alist)))
                 ;;                     ;; (if (null? deps) '() (car deps))
                 ;;                     deps #f)))
                 (if deps
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ured "resolving dep labels 2") deps))
                       (let ((exports (car (assoc-val :exports ws)))
                             (fixdeps
                              (map (lambda (dep)
                                     (if *mibl-debug-s7*
                                         (format #t "~A: ~A~%" (uwhite "fixup dep B") dep))
                                     (if (eq? (car dep) ::tools)
                                         (begin
                                           (if *mibl-debug-s7*
                                               (format #t "~A: ~A~%" (bgred "::TOOLS") (caadr dep)))
                                           (cond
                                            ((eq? ::unresolved (cdadr dep))
                                             (let* ((t (format #f "~A" (caadr dep)))
                                                    (t (if (string-prefix? ":exe:" t)
                                                           (string->symbol (string-append ":bin:" (string-drop t 5)))
                                                           (caadr dep))))
                                               (if *mibl-debug-s7*
                                                   (begin
                                                     (format #t "~A~%" (bgred "IMPORT TOOL"))
                                                     (format #t "~A~%" (red "export keys"))))
                                               ;; if tool = :exe:..., replace exe with bin before lookup
                                               ;; (for-each (lambda (k)
                                               ;;             (format #t "~A: ~A~%" (ured "key") k))
                                               ;;           (sort! (hash-table-keys exports) sym<?))
                                               (if-let ((import (hash-table-ref exports t)))
                                                       (begin
                                                         (if *mibl-debug-s7*
                                                             (format #t "~A: ~A~%" (bgred "importing") import))
                                                         (list ::tools
                                                               (cons (caadr dep) ;; use original :exe:, :bin: just for lookup
                                                                           (list (assoc :pkg import)
                                                                                 (assoc :tgt import))
                                                                           ;; (format #f "//~A:~A"
                                                                           ;;         (assoc-val :pkg import)
                                                                           ;;         (assoc-val :tgt import))
                                                                           ))
                                                         )
                                                       (begin
                                                         (if *mibl-debug-s7*
                                                             (format #t "~A: ~A~%" (red "no import for tool") t))
                                                         ;; (error 'STOP "STOP no import")
                                                         ;; assume (rashly) that form is e.g. :tools/version/gen/gen.exe
                                                         (let* ((kw t) ;; (caadr dep))
                                                                (t (keyword->symbol kw))
                                                                (path (dirname t)))
                                                           `(::tools
                                                             (,kw
                                                              ,(cons :pkg (if (string=? "./" path)
                                                                              pkg-path path
                                                                              ;; (if (string=? "::wsroot" path)
                                                                              ;;     pkg-path
                                                                              ;;     path)
                                                                              ))
                                                              ;; ,(cons :pkg (if (string=? "./" path)
                                                              ;;                 pkg-path path))
                                                              ,(cons :tgt (basename t))))
                                                           )))))
                                            ((eq? ::unresolved (cdadr dep))
                                             ;;FIXME
                                             )
                                           ;; else treat it just like a std dep
                                           (else (-fixup-std-dep-form ws-id pkg dep exports))))
                                         ;; else std dep form: (:foo (:pkg...)(:tgt...))
                                         (-fixup-std-dep-form ws-id pkg dep exports)))
                                   (cdr deps))))
                             (if *mibl-debug-s7*
                                 (format #t "~A: ~A~%" (ured "fixed-up deps") fixdeps))
                         (set-cdr! deps fixdeps))))
                 ;; (format #t "~A: ~A~%" (ured "reset deps") deps)

                 ;; :actions is always a list of cmd; for progn, more than one
                 (if (assoc :actions stanza-alist)
                     (begin
                       (for-each (lambda (c)
                                   (if *mibl-debug-s7*
                                       (format #t "PROGN cmd: ~A~%" c))
                                   (-fixup-progn-cmd! ws c targets deps))
                                 action))
                     ;; else? actions always have a :cmd?
                     (begin
                       (if *mibl-debug-s7*
                           (begin
                             (format #t "rule action: ~A~%" action)
                             (format #t "rule tool: ~A~%" tool)
                             (format #t "rule targets: ~A~%" targets)
                             (format #t "rule deps: ~A~%" deps)
                             (error 'unhandled action "unhandled action")
                             ))
                       ;; (if-let ((tool-label (hash-table-ref exports (cadr tool))))
                       ;;         (let* ((_ (if *mibl-debug-s7* (format #t "tool-label: ~A~%" tool-label)))
                       ;;                (pkg (car (assoc-val :pkg tool-label)))
                       ;;                (tgt (car (assoc-val :tgt tool-label)))
                       ;;                (label (format #f "//~A:~A" pkg tgt))
                       ;;                (_ (if *mibl-debug-s7* (format #t "tool-label: ~A\n" tool-label))))
                       ;;           (set-cdr! tool (list label)))
                       ;;         ;; FIXME: handle deps
                       ;;         '())
                       ))))

              ((:archive :ns-archive :library :ns-library)
               (if *mibl-debug-s7*
                   (format #t "~A: ~A~%" (blue "aggregate fixup") (car stanza)))
               ;; (let ((deps (assoc-val :deps stanza-alist)))
               ;;   (format #t "archive deps: ~A~%" deps)))
               (let* ((deps (if-let ((deps (assoc-in '(:deps :remote) stanza-alist)))
                                    deps #f))
                      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (blue "deps") deps)))
                      (ppx (if-let ((ppx (assoc-val :ppx stanza-alist)))
                                   ppx #f))
                      (ppxex (if-let ((ppxes (assoc-val :ppxes stanza-alist)))
                                     ppxes #f))
                      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (ublue "ppx") ppx)))
                      (ppx-codeps (if-let ((ppx-codeps (assoc
                                                        :ppx-codeps stanza-alist)))
                                          ppx-codeps #f))
                      )
                 (if deps
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ugreen "resolving libdeps 3") deps))
                       (let ((exports (car (assoc-val :exports ws)))
                             (fixdeps
                              (map (lambda (dep)
                                     (if *mibl-debug-s7*
                                         (format #t "~A: ~A~%" (green "resolving dep") dep))
                                     (cond
                                      ((list? dep) ;; std dep form: (:foo (:pkg...)(:tgt...))
                                       (-fixup-std-dep-form ws pkg dep exports))
                                      ((symbol? dep) ;; e.g. (:deps (:remote bigarray parsexp sexplib0))
                                       (-fixup-dep-sym ws-id dep pkg exports))
                                      (else (error 'fixme
                                                   (format #f "~A: ~A~%" (bgred "unrecognized :archive dep type") dep)))))
                                   (cdr deps))))
                         (if *mibl-debug-s7*
                             (format #t "~A: ~A~%" (ured "fixed-up deps") fixdeps))
                         (set-cdr! deps fixdeps)
                         (set-car! deps :resolved))))
                 (if ppx
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ured "resolving ppx") ppx))
                       (let* ((exports (car (assoc-val :exports ws)))
                              (ppx-deps (assoc :manifest ppx))
                              (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (bgred "ppx-deps") ppx-deps)))
                              (fixppx
                               (map (lambda (dep)
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (uwhite "fixup ppx-dep") dep))
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized ppx-dep type") dep)))))
                                    (cdr ppx-deps))))
                         (if *mibl-debug-s7*
                             (format #t "~A: ~A~%" (ured "fixed-up ppx-deps") fixppx))
                         (set-cdr! ppx-deps fixppx)
                         ;; (set-car! ppx-deps :resolved)
                         ;; (error 'STOP "stop ppx")
                         )))
                 (if ppx-codeps
                     (begin
                       (if *mibl-debug-s7*
                           (format #t "~A: ~A~%" (ured "resolving ppx-codeps") ppx-codeps))
                       (let* ((exports (car (assoc-val :exports ws)))
                              ;; (ppx-deps (assoc :manifest ppx))
                              ;; (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (bgred "ppx-deps") ppx-deps)))
                              (fixppx
                               (map (lambda (dep)
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (uwhite "fixup ppx-dep") dep))
                                      (cond
                                       ((list? dep)
                                        ;; std dep form: (:foo (:pkg...)(:tgt...))
                                        (-fixup-std-dep-form ws pkg dep exports))
                                       ((symbol? dep)
                                        (-fixup-dep-sym ws-id dep pkg exports))
                                       (else (error 'fixme
                                                    (format #f "~A: ~A~%" (bgred "unrecognized ppx-dep type") dep)))))
                                    (cdr ppx-codeps))))
                         (if *mibl-debug-s7*
                             (format #t "~A: ~A~%" (ured "fixed-up ppx-codeps") fixppx))
                         (set-cdr! ppx-codeps fixppx)
                         ;; (set-car! ppx-deps :resolved)
                         ;; (error 'STOP "stop ppx")
                         )))
                 ))

              ((:cppo)
               (let ((deps (assoc :deps (cdr stanza))))
                 (if *mibl-debug-s7*
                     (format #t "~A: ~A~%" (red "deps") deps))
                 (set-cdr! deps (dissoc '(::tools) (cdr deps)))
                 (if *mibl-debug-s7*
                     (format #t "~A: ~A~%" (red "deps after") deps)))
               )

              ;; ((:library)
              ;;  (format #t "~A~%" (ublue "fixup :library"))
              ;;  (let* ((deps (if-let ((deps (assoc-in '(:deps :remote) stanza-alist)))
              ;;                       deps #f))
              ;;         (_ (if *mibl-debug-s7* (format #t "deps: ~A~%" deps)))
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
              ;;                          (-fixup-dep-sym ws-id dep pkg exports))
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

              ((:lex :yacc :menhir :env
                          :prologues :testsuite :tuareg :alias)
               (values))

              (else
               (error 'fixme
                      (format #t "~A: ~A~%" (bgred "unhandled fixup stanza") stanza))))))))

(define (-fixup-module-deps! ws pkg module-spec)
  ;; module-spec: (A (ml: a.ml ...) (:mli a.mli ...))
  ;; (or :ml_, :mli_)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-module-deps!") module-spec))
  (let* ((exports (car (assoc-val :exports ws)))
         (ml-deps (if-let ((mldeps (assoc-val :ml (cdr module-spec))))
                               mldeps
                               (assoc-val :ml_ (cdr module-spec)))))
    (format #t "~A: ~A\n" (ublue "ml-deps") ml-deps)
    (let ((newdeps (map (lambda (dep)
                          (-fixup-dep-sym :@ dep pkg exports))
                        (cdr ml-deps))))
      (format #t "~A: ~A\n" (ublue "newdeps") newdeps)
      (set-cdr! ml-deps newdeps))))

(define (-fixup-struct-deps! ws-id pkg struct)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-struct-deps!") struct))
  )

(define (-fixup-sig-deps! ws-id pkg sig)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "-fixup-sig-deps!") sig))
  )

;; updates stanzas/pkg files. initial conditions:
;; * :deps derived from (libraries) fld of (library) stanza
;; * module deps listed in :modules, :structures, :signatures
;; result:
;;     :deps - module names replaced by bazel labels
;;     pkg file deps: opam pkg modules removed
;; e.g. sexplib0 =>  @sexplib0//lib/sexplib0
;; method: try to resolve each module locally
;; if that fails, assume it is an opam pkg
(define normalize-lib-deps!
  (let ((+documentation+ "Map dune target references to bazel labels using exports table.")
        (+signature+ '(normalize-lib-deps! workspace-id)))
    (lambda (ws-id)
      (let ((ws (assoc-val ws-id *mibl-project*)))
        (if *mibl-debug-s7*
            (format #t "~%~A for ws: ~A\n"
                (bgred "normalize-lib-deps!") ws)) ;;(assoc :name ws)))
        ;; (assoc-val 'name ws))
        (let* ((pkgs (car (assoc-val :pkgs ws)))
               ;; (_ (if *mibl-debug-s7* (format #t "PKGS: ~A\n" pkgs)))
               (exports (car (assoc-val :exports ws))))
          ;; (format #t "resolving labels for pkgs: ~A\n" (hash-table-keys pkgs))
          ;; (format #t "exports: ~A\n" exports)
          (for-each (lambda (pkg-kv)
                      (if *mibl-debug-s7*
                          (format #t "~A: ~A~%" (ublue "resolving pkg") pkg-kv))
                      ;; (format #t "pkg: ~A~%" (cdr pkg-kv))
                      (let ((pkg (cdr pkg-kv)))
                        (if-let ((stanzas (assoc-val :mibl (cdr pkg-kv))))
                                (for-each (lambda (stanza)
                                            (-fixup-stanza! ws-id pkg stanza)
                                            (if *mibl-debug-s7*
                                                (format #t "stanza: ~A~%" stanza)))
                                          stanzas))
                        (if-let ((modules (assoc-val :modules (cdr pkg-kv))))
                                (for-each (lambda (module)
                                            (-fixup-module-deps! ws pkg module))
                                          modules))
                        (if-let ((structs (assoc-val :structures (cdr pkg-kv))))
                                (for-each (lambda (struct)
                                            (-fixup-struct-deps! ws-id pkg struct))
                                          structs))
                        (if-let ((sigs (assoc-val :signatures (cdr pkg-kv))))
                                (for-each (lambda (sig)
                                            (-fixup-sig-deps! ws-id pkg sig))
                                          sigs))
                        )
                      )
                    pkgs)
          )))))
