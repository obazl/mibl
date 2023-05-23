;; https://dune.readthedocs.io/en/stable/dune-files.html#alias

;; The alias stanza adds dependencies to an alias or specifies an
;; action to run to construct the alias.

;; The syntax is as follows:

;; (alias
;;  (name    <alias-name>)
;;  (deps    <deps-conf list>)
;;  <optional-fields>)

;; <action>, an action for constructing the alias. See the User
;; Actions section for more details. Note that this is removed in Dune
;; 2.0, so users must port their code to use the rule stanza with the
;; alias field instead.

;; WARNING: evidently it was not removed. Plenty of packages still
;; use dune 1.0, e.g.
;; yojson/test/pretty:
;; (alias
;;  (name runtest)
;;  (action
;;   (diff test.expected test.output)))

(define (dune-alias->mibl ws pkg stanza)
  (if (or *mibl-debug-alias* *mibl-debug-all*)
      (format #t "~A: ~A\n" (blue "dune-alias->mibl") stanza))
  ;; (if-let ((alias-assoc (assoc :alias (cdr stanza))))
  (if (assoc-in '(:actions :cmd) (cdr stanza))
      (let ((alias (cadr alias-assoc))
            (cmd-ct (length (assoc-in* '(:actions :cmd) (cdr stanza))))
            ;;FIXME assuming one cmd
            (args (assoc-in '(:actions :cmd :args) (cdr stanza))))
        (if (or *mibl-debug-alias* *mibl-debug-all*)
            (begin
              (format #t "~A: ~A~%" (ured "ALIAS") alias)
              (format #t "~A: ~A~%" (ured "cmd ct") cmd-ct)
              (format #t "~A: ~A~%" (ured "args") args)))
        ;; if :args contains executable, mark as :test
        (let ((tool-args (fold (lambda (arg accum)
                                 (if (or *mibl-debug-alias* *mibl-debug-all*)
                                     (format #t "~A: ~A~%" (ured "arg") arg))
                                 (let ((argstr (format #f "~A" arg)))
                                   (if (or *mibl-debug-alias* *mibl-debug-all*)
                                       (format #t "~A: ~A~%" (ured "argstr") argstr))
                                   ;; FIXME what about local sh scripts?
                                   (cond
                                    ((string-prefix? ":bin" argstr) (cons arg accum))
                                    ((string-prefix? ":exe" argstr) (cons arg accum))
                                    ((string-prefix? ":libexec" argstr) (cons arg accum))
                                    ;; lib:<public-library-name>:<file>
                                    ;; lib-private:<library-name>:<file>
                                    ;; libexec-private:<library-name>:<file>
                                    ((= 0 (libc:fnmatch  "*.sh" argstr 0)) (cons arg accum))
                                    ((= 0 (libc:fnmatch  "*.py" argstr 0)) (cons arg accum))
                                    ((= 0 (libc:fnmatch  "*.js" argstr 0)) (cons arg accum))
                                    ;;FIXME others?
                                    (else accum))))
                               '() (cdr args))))
          (if tool-args
              (begin
                (if (or *mibl-debug-alias* *mibl-debug-all*)
                    (format #t "~A: ~A~%" (ured "found executable tool args") tool-args))
                (if-let ((deps (assoc :deps (cdr stanza))))
                        (let ((tool-deps (assoc ::tools (cdr deps))))
                          (if tool-deps
                              ;; append tools
                              (if (or *mibl-debug-alias* *mibl-debug-all*)
                                  (format #t "~A: ~A~%" (ured "tool-deps") tool-deps))
                              ;; add ::tools to (:deps ...)
                              (let ((_ (if (or *mibl-debug-alias* *mibl-debug-all*) (format #t "~A: ~A~%" (ured "deps") deps)))
                                    (deps-list (cdr deps))
                                    (tools (list (cons ::tools tool-args))))
                                (set-cdr! deps (append tools deps-list)))
                              ))
                        ;; else no deps in stanza?
                        (begin))
                (set-cdr! stanza
                          (acons :name
                                 (format #f "~A_~A" alias -sh-test-id)
                                 (cdr stanza)))
                (set-car! stanza :sh-test)
                (set! -sh-test-id (+ 1 -sh-test-id))
                )
              (begin
                (if (or *mibl-debug-alias* *mibl-debug-all*)
                    (format #t "~A: ~A~%" (ured "NO executable tools") tools))
                (error 'FIXME "alias without run tool")))
          ))
      ;; else alias with no :actions
      (begin
        ;; ignore?
        (list `(:alias ,@(cdr stanza)))
        ;; (error 'fixme
        ;;        (format #f "~A: ~A~%" (ured "ALIAS w/o actions") stanza))
        )))
          ;; (begin
          ;;   (format #t "~A: ~A~%" (ured "NO ALIAS") stanza)
          ;;   #| nop |#)))
