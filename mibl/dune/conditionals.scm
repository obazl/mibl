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
  (format #t "~A: ~A~%" (ublue "update-selects-list!") conditional)
  (for-each (lambda (selector)
              (format #t "~A: ~A~%" (blue "selector") selector)
              (set! *select-protases*
                    (cons (car selector) *select-protases*)))
            (assoc-val :selectors conditional)))

(define (update-pkg-conditionals! pkg conditional)
  (format #t "~A: ~A\n" (ublue "update-pkg-conditionals!") conditional)
  (let* ((ctarget (car (assoc-val :target conditional)))
         (modules (if-let ((ms (assoc-val :modules pkg)))
                          ms
                          (begin
                            (set-cdr! pkg
                                      (append (cdr pkg)
                                              '((:modules))))
                            (assoc-val :modules (cdr pkg)))))
         ;;FIXME: dynamics too
         (sigs (assoc-in '(:signatures :static) pkg))
         (structs (assoc-in '(:structures :static) pkg)))
    (format #t "~A: ~A\n" (uwhite "pkg") pkg)
    (format #t "~A: ~A\n" (uwhite "cond target") ctarget)
    (format #t "~A: ~A\n" (uwhite "pkg modules") modules)
    (format #t "~A: ~A\n" (uwhite "pkg sigs") sigs)
    (format #t "~A: ~A\n" (uwhite "pkg structs") structs)

    ;; remove apodoses from :structures, :signatures
    (format #t "~A: ~A~%" (red "removing apodoses from pkg-structs") structs)
    (let ((apodoses (cons
                     (car (assoc-val :default conditional))
                     (map cdr (assoc-val :selectors conditional)))))
      (format #t "~A: ~A\n" (uwhite "collected apodoses") apodoses)
      ;; first structs
      (for-each (lambda (apo)
                  (format #t "~A: ~A~%" (uwhite "testing structs") apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr structs))))
                    (format #t "matched? ~A~%" match)
                    (if match
                        (set-cdr! structs (dissoc! (list (car match))
                                                   (cdr structs))))
                    ))
                apodoses)
      (format #t "~A: ~A~%" (cyan "updated pkg-structs") structs)
      (format #t "~A: ~A~%" (red "removing apodoses from pkg-sigs") sigs)
      (for-each (lambda (apo)
                  (format #t "~A: ~A~%" (uwhite "testing sigs") apo)
                  (let ((match (find-if (lambda (e) (eq? apo (cdr e)))
                                        (cdr sigs))))
                    (format #t "matched? ~A~%" match)
                    (if match
                        (set-cdr! sigs (dissoc! (list (car match))
                                                (cdr sigs))))
                    ))
                apodoses)
      (format #t "~A: ~A~%" (cyan "updated pkg-sigs") sigs)
      )

    (format #t "~A: ~A~%" (red "updating :modules for select tgt") ctarget)
    (if (structfile? ctarget)
        (let* ((sigtarget (string->symbol (format #f "~Ai" ctarget)))
               (match (find-if (lambda (e)
                                 ;; (format #t "e: ~A\n" e)
                                 (eq? sigtarget (cdr e)))
                               (cdr sigs))))
          ;; fixme: (if match ...
          (let ((newmod (cons (car match)
                              (list (cons :ml_ ctarget)
                                    (cons :mli (cdr match))))))
            (format #t "matching sig: ~A\n" newmod)
            (set-cdr! modules (append (cdr modules) (list newmod)))
            (set-cdr! sigs (dissoc! match (cdr sigs))))
          (format #t "~A: ~A\n" (uwhite "upd pkg modules") modules)
          ))

    (if (sigfile? ctarget)
        (let* ((structtarget (string->symbol
                              (string-drop-right (format #f "~A" ctarget)
                                                 1)))
               (_ (format #t "structtarget: ~A\n" structtarget))
               (match (find-if (lambda (e)
                                 (format #t "e: ~A\n" e)
                                 (eq? structtarget (cdr e)))
                               (cdr structs))))
          ;; (format #t "match: ~A\n" match)
          (let ((newmod (cons (car match)
                              (list (cons :ml (cdr match))
                                    (cons :mli_ ctarget)))))
            (format #t "matching struct: ~A\n" newmod)
            (set-cdr! modules (append (cdr modules) (list newmod)))
            (set-cdr! structs (dissoc! match (cdr structs))))
          ))
    ;; (format #t "~A~%" (cyan "updated pkg-modules"))
    ;; (for-each (lambda (m) (format #t "\t~A~%" m)) modules)
    pkg))

(define (analyze-select select) ;; directs selects)
  (format #t "~A: ~A\n" (ublue "analyze-select") select)
  ;; e.g. (select foo.ml from (bar -> baz.ml) (-> default.ml))
  ;; see normalize-lib-select in dune_stanzas.scm
  ;; FIXME: extract module dep from select sexp and add to directs
  (let* ((target (cadr select))
         (selectors (cdddr select))
         (default (cadr (last selectors)))
         (selectors (but-last selectors)))
    (format #t "select target: ~A\n" target)
    (format #t "selectors : ~A\n" selectors)
    (format #t "default : ~A\n" default)
    (let ((clauses (map (lambda (selector)
                          (format #t "selector: ~A\n" selector)
                          (let ((protasis (car selector))
                                (apodosis (caddr selector)))
                            (list
                             `(:dep ,protasis)
                             `(:clause ,(cons protasis apodosis)))))
                        selectors)))
      (format #t "clauses: ~A\n" clauses)
      `((:target ,target)
        ,(cons ':deps (map (lambda (c) (cadar c)) clauses))
        ,(cons ':selectors (apply
                            append
                            (map (lambda (c) (assoc-val :clause c))
                                 clauses)))
        (:default ,default)))))

