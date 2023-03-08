;; SELECT in 'libraries' field of lib stanzas:
;; alternative deps:
;; https://dune.readthedocs.io/en/stable/concepts.html#alternative-dependencies
;; e.g. tezos/src/lib_signer_backends/unix:
;; (libraries ocplib-endian.bigstring
;;            ... etc. ...
;;            (select ledger.ml from
;;              (ledgerwallet-tezos -> ledger.available.ml)
;;              (-> ledger.none.ml)))
;;  Why is a .ml file in a 'libraries' field? Why not in the 'modules'
;;  field? In this example, srcs include ledger.mli but not ledger.ml,
;;  and (modules :standard). Evidently the modules list would not
;;  include Ledger, since it lacks ledger.ml. But the 'libraries'
;;  field indicates a dependency on _some_ ledger.ml file. So
;;  implicitly, before the 'modules' field is resolved, the
;;  dependencies are resolved, resulting in generation (by copy) of
;;  ledger.ml, so the Ledger will be included in the 'modules' roster.
;;  Furthermore, the protasis in (foo -> bar.ml) _is_ a library,
;;  so (evidently) if it is found ("installed"?) then it is included
;;  in the libdeps list.

;; js_of_ocaml/compiler/lib/dune:
 ;; (libraries
 ;;  ...
 ;;  (select
 ;;   source_map_io.ml
 ;;   from
 ;;   (yojson -> source_map_io.yojson.ml)
 ;;   (-> source_map_io.unsupported.ml)))

;;  As a side-effect, evaluating each (a -> b) clause makes a a
;;  dependency; but a fake dependency, whose only purpose is to
;;  trigger a file copy to induce a module src dep.

;; In other words, these select clauses do NOT express a lib
;; dependency on a lib/archive/module; rather they express a (sub)
;; modules dependency on a source file (and as a side effect, a lib
;; dep on the protasis of the winning clause).

;; translation to mibl: such selects do not belong in :deps. In
;; starlark they will be 'select' for a source attibute.

;; (libraries a b c.d a.b.c ...)
;; uncommon: "alternative deps"
;; see https://dune.readthedocs.io/en/stable/concepts.html#alternative-deps
;; example from tezos:
;; (libraries a b c
;;            (select void_for_linking-genesis from
;;              (tezos-client-genesis -> void_for_linking-genesis.empty)
;;              (-> void_for_linking-genesis.empty))
;;              ...
;;            (select void_for_linking-demo-counter from
;;              (tezos-client-demo-counter -> void_for_linking-demo-counter.empty)
;;              (-> void_for_linking-demo-counter.empty)))

;; also:  (libraries (re_export foo))

(define (update-selects-list! conditional)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (ublue "update-selects-list!") conditional))
  (for-each (lambda (selector)
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (blue "selector") selector))
              (set! *select-protases*
                    (cons (car selector) *select-protases*)))
            (assoc-val :selectors conditional)))

;; FIXME: update pkg files
(define (update-pkg-conditionals! pkg conditional)
  (if *mibl-debugging*
      (format #t "~A: ~A\n" (ublue "update-pkg-conditionals!") conditional))
  ;; conditional:  ((:target . a.ml) (:selectors (<lib> . a_b.ml)) (:default a_default.ml))
  (let* ((ctarget (assoc-val :target conditional))
         (modules (if-let ((ms (assoc-val :modules pkg)))
                          ms
                          (begin
                            (set-cdr! pkg
                                      (append (cdr pkg)
                                              '((:modules))))
                            (assoc-val :modules (cdr pkg)))))
         ;;FIXME: dynamics too
         (sigs (assoc-in '(:signatures :static) pkg))
         (sigs-dynamic (assoc-in '(:signatures :dynamic) pkg))
         (structs (assoc-in '(:structures :static) pkg))
         (structs-dynamic (assoc-in '(:structures :dynamic) pkg)))
    (if *mibl-debugging*
        (begin
          (format #t "~A: ~A\n" (uwhite "pkg") pkg)
          (format #t "~A: ~A\n" (uwhite "cond target") ctarget)
          (format #t "~A: ~A\n" (uwhite "pkg modules") modules)
          (format #t "~A: ~A\n" (uwhite "pkg sigs (static)") sigs)
          (format #t "~A: ~A\n" (uwhite "pkg sigs (dynamic)") sigs-dynamic)
          (format #t "~A: ~A\n" (uwhite "pkg structs (static)") structs)
          (format #t "~A: ~A\n" (uwhite "pkg structs (dynamic)") structs-dynamic)))

    ;; remove apodoses from :structures, :signatures
    ;; NB: a dune trick is to use an empty file as apodosis of one
    ;; conditional and as the default. This serves to select the
    ;; dependency (the protatis) when it's available, without adding
    ;; a module to the ns.  So we need to uniquify the apodoses.
    (let* ((apodoses (cons
                      (assoc-val :default conditional)
                      (map cdr (assoc-val :selectors conditional))))
           (apodoses (remove-duplicates apodoses)))
      (if *mibl-debugging*
          (begin
            (format #t "~A: ~A\n" (ucyan "removing apodoses") apodoses)
            (format #t "~A: ~A~%" (ucyan "... from pkg-structs") structs)))
      ;; first structs
      (for-each (lambda (apo)
                  (if *mibl-debugging*
                      (format #t "~A: ~A~%" (uwhite "testing structs") apo))
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr structs))))
                    (if *mibl-debugging*
                        (format #t "matched? ~A~%" match))
                    (if match
                        (set-cdr! structs (dissoc! (list (car match))
                                                   (cdr structs))))
                    ))
                apodoses)
      (if *mibl-debugging*
          (begin
          (format #t "~A: ~A~%" (uwhite "updated pkg-structs") structs)
          (format #t "~A: ~A~%" (ucyan "... from pkg-sigs") sigs)))

      (if sigs
          (for-each (lambda (apo)
                      (if *mibl-debugging*
                          (format #t "~A: ~A~%" (uwhite "testing sigs") apo))
                      (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                            (cdr sigs))))
                        (if *mibl-debugging*  (format #t "matched? ~A~%" match))
                        (if match
                            ;; FIXME: also add to :ignore
                            (set-cdr! sigs (dissoc! (list (car match))
                                                    (cdr sigs))))
                        ))
                    apodoses))
      (if *mibl-debugging*
          (format #t "~A: ~A~%" (uwhite "updated pkg-sigs") sigs))
      )

    ;; now update :modules if ctarget matchs a sig or struct
    ;; if no match, update :structures or :signatures
    (if (eq? (fnmatch "*.ml" (format #f "~A" ctarget) 0) 0)
        (if (truthy? sigs)
            (begin
              (if *mibl-debugging*
                  (begin
                    (format #t "~A: ~A~%" (ucyan "maybe migrating sig to :modules for select tgt") ctarget)
                    (format #t "~A: ~A~%" (white "sigs") sigs)))
              ;; (if (and (structfile? ctarget) sigs)
              (let* ((sigtarget (string->symbol (format #f "~Ai" ctarget)))
                     (match (find-if (lambda (e)
                                       ;; (format #t "e: ~A\n" e)
                                       (eq? sigtarget (cdr e)))
                                     (cdr sigs))))
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (white "sigmatch?") match))
                (if match
                    (let ((newmod (cons (car match)
                                        (list (cons :ml_ ctarget)
                                              (cons :mli (cdr match))))))
                      (if *mibl-debugging* (format #t "matching sig: ~A\n" newmod))
                      (set-cdr! modules (append (cdr modules) (list newmod)))
                      (if *mibl-debugging*
                          (format #t "~A: ~A\n" (cyan "upated pkg modules") modules))
                      (set-cdr! sigs (dissoc! match (cdr sigs)))
                      (if *mibl-debugging*
                          (format #t "~A: ~A\n" (cyan "upated pkg sigs") sigs)))
                    ;; else update :structures
                    ;;FIXME: what if conditional target already exists?
                    (if (truthy? structs)
                        (begin
                          (set-cdr! structs (dissoc! match (cdr sigs)))
                          (if *mibl-debugging*
                              (format #t "~A: ~A\n" (cyan "upated pkg sigs") sigs)))
                        ;; else create new :structures fld
                        )
                    ))
              )
            ;; else no sigs
            ;; assumption: ctarget is NOT already in structs
            (if (truthy? structs-dynamic)
                (begin
                  (if *mibl-debugging*
                      (format #t "~A: ~A~%" (cyan "updating (:structures :dynamic)") structs-dynamic))
                  (set-cdr! structs-dynamic (append (cdr structs) `(,(cons (filename->module-name ctarget) ctarget))))
                  (if *mibl-debugging*
                      (format #t "~A: ~A\n" (cyan "upated pkg structs") structs)))
                ;; else create new :structures fld
                (begin
                  (if *mibl-debugging*
                      (format #t "~A~%" (cyan "adding (:structures :dynamic)")))
                  (alist-update-in! pkg '(:structures :dynamic)
                                    (lambda (old)
                                      `(,(cons (filename->module-name ctarget) ctarget)))))
            )))

    (if (and (eq? (fnmatch "*.mli" (format #f "~A" ctarget) 0) 0)
             (truthy? structs))
        (begin
          (if *mibl-debugging*
              (begin
                (format #t "~A: ~A~%" (ucyan "maybe migrating struct to :modules for select tgt") ctarget)
                (format #t "~A: ~A~%" (red "ctarget") ctarget)
                (format #t "~A: ~A~%" (red "structs") structs)))
          (let* ((structtarget (string->symbol
                                (string-drop-right (format #f "~A" ctarget)
                                                   1)))
                 (_ (if *mibl-debugging* (format #t "structtarget: ~A\n" structtarget)))
                 (match (find-if (lambda (e)
                                   (format #t "e: ~A\n" e)
                                   (eq? structtarget (cdr e)))
                                 (cdr structs))))
            (if *mibl-debugging*
                (format #t "~A: ~A~%" (bgred "structmatch?") match))
            (if match
                (let ((newmod (cons (car match)
                                    (list (cons :ml (cdr match))
                                          (cons :mli_ ctarget)))))
                  (if *mibl-debugging*
                      (format #t "matching struct: ~A\n" newmod))
                  (set-cdr! modules (append (cdr modules) (list newmod)))
                  (set-cdr! structs (dissoc! match (cdr structs))))))
          (if *mibl-debugging*
              (format #t "~A~%" (cyan "updated pkg-modules")))
          ))

    ;;FIXME: maybe add to :structures or :signatures

    ;; (for-each (lambda (m) (format #t "\t~A~%" m)) modules)
    pkg))

(define (analyze-select select) ;; directs selects)
  (if *mibl-debugging*
      (format #t "~A: ~A\n" (ublue "analyze-select") select))
  ;; e.g. (select foo.ml from (bar -> baz.ml) (-> default.ml))
  ;; see normalize-lib-select in dune_stanzas.scm
  ;; FIXME: extract module dep from select sexp and add to directs
  (let* ((target (cadr select))
         (selectors (cdddr select))
         (default (cadr (last selectors)))
         (selectors (but-last selectors)))
    (if *mibl-debugging*
        (begin
          (format #t "select target: ~A\n" target)
          (format #t "selectors : ~A\n" selectors)
          (format #t "default : ~A\n" default)))
    (let ((clauses (map (lambda (selector)
                          (if *mibl-debugging*
                              (format #t "selector: ~A\n" selector))
                          (let ((protasis (car selector))
                                (apodosis (caddr selector)))
                            (list
                             `(:dep ,protasis)
                             `(:clause ,(cons protasis apodosis)))))
                        selectors)))
      (if *mibl-debugging*
          (format #t "clauses: ~A\n" clauses))
      `((:target . ,target)
        ,(cons ':deps (map (lambda (c) (cadar c)) clauses))
        ,(cons ':selectors (apply
                            append
                            (map (lambda (c) (assoc-val :clause c))
                                 clauses)))
        (:default . ,default)))))

