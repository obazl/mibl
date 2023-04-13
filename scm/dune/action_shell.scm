(define shell-tools
  '(cat cp copy diff echo)) ;; etc

(define shell-tool-kws
  '(::cat ::cp ::copy ::diff ::echo)) ;; etc

(define shell-tool-map
  '((cat ::cat)
    (cp ::cp)
    (copy ::cp)
    (diff ::diff)
    (echo ::echo)
    ;; etc
    ))

;; (define (-expand-bash-tool tool pkg-path stanza)
;;   (format #t "~A: ~A~%" (ublue "-expand-bash-tool") tool)
;;   (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
;;   (format #t "~A: ~A~%" (blue "stanza") stanza)

;;   (if (member (if (string? tool) (string->symbol tool) tool)
;;               shell-tools)
;;       #f
;;       (let* ((key (string->keyword tool)))
;;         ;; search :deps
;;         (let* ((deps (assoc-val :deps stanza))
;;                ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "deps") deps)))
;;                (match (find-if (lambda (dep)
;;                                  ;; (format #t "~A: ~A~%" (yellow "dep") dep)
;;                                  (eq? key (car dep)))
;;                                deps)))
;;           (format #t "~A: ~A~%" (yellow "match") match)
;;           (if match
;;               (let* ((lbl (cdr match))
;;                      (pkg (assoc-val :pkg lbl))
;;                      ;; (_ (if *mibl-debug-all* (format #t "~A: ~A~%" (yellow "pkg") pkg)))
;;                      (pkg (if (equal? pkg-path pkg) "" pkg))
;;                      (tgt (if-let ((t (assoc-val :tgt lbl)))
;;                                   (format #f "~A:~A" pkg t)
;;                                   (error 'fixme "bash tool has :tgts"))))
;;                 (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
;;                 tgt)
;;               arg)))))

;; (system "...script..."), (bash "...script..."), (echo ...) etc.
;; ops: bash, echo, system
(define (normalize-action-echo-cmd ws pkg action action-list tools targets deps)
  (mibl-trace-entry "normalize-action-echo-cmd" action)
  (mibl-trace "action-list" action-list)
  (let* ((tool (string->keyword (format #f "~A" action)))
         (mibl-trace-let "tool" tool)
         (action-args (cdr action-list))
         (mibl-trace-let "action-args" action-args)
         (echo-string (string-parse-pct-vars action-args))
         )
    (mibl-trace "echo-string" echo-string)
    (set-cdr! tools `((,tool . :shell-tool)))
    `((:cmd
       (:tool ,tool)
       (:args ,@echo-string)
       ))))

(define (normalize-action-shell-cmd ws pkg action action-list tools targets deps)
  ;; FIXME: shell cmd args may include filename literals; find way to expand?
  ;; FIXME: may include ${target}
  ;; FIXME: in general: expand all '${}' in args
  (if *mibl-debug-all*
      (begin
        (format #t "~A: ~A\n" (ublue "normalize-action-shell-cmd") action)
        (format #t "~A: ~A~%" (white "action-list") action-list)
        (format #t "~A: ~A~%" (white "action-args") (cdr action-list))))

  ;; search deps for tool, if not found, add it to (:deps ::tool)?

  (let* ((tool (string->keyword (format #f "~A" action)))
         ;; (tool (if (eq? action 'system) 'sh action))
         (mibl-trace-let "tool" tool)
         (action-args (cdr action-list))
         (mibl-trace-let "action-args" action-args)
         ;; (destructured-args (destructure-dsl-string action-args))
         (expanded-args (expand-cmd-args* ws action-args pkg targets deps))
         (mibl-trace-let "expanded-args" expanded-args)
         )
    (set-cdr! tools `((,tool . :shell-tool)))
    `((:cmd
       (:tool ,tool)
       (:args ,@expanded-args)))))

