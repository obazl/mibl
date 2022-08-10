(define shell-tools
  '(cat cp copy)) ;; etc

(define shell-tool-kws
  '(::cat ::cp ::copy)) ;; etc

(define shell-tool-map
  '((cat ::cat)
    (cp ::cp)
    (copy ::cp)
    ;; etc
    ))

(define (-expand-bash-tool tool pkg-path stanza)
  (format #t "~A: ~A~%" (ublue "-expand-bash-tool") tool)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (blue "stanza") stanza)

  (if (member (if (string? tool) (string->symbol tool) tool)
              shell-tools)
      #f
      (let* ((key (string->keyword tool)))
        ;; search :deps
        (let* ((deps (assoc-val :deps stanza))
               ;; (_ (format #t "~A: ~A~%" (yellow "deps") deps))
               (match (find-if (lambda (dep)
                                 ;; (format #t "~A: ~A~%" (yellow "dep") dep)
                                 (eq? key (car dep)))
                               deps)))
          (format #t "~A: ~A~%" (yellow "match") match)
          (if match
              (let* ((lbl (cdr match))
                     (pkg (assoc-val :pkg lbl))
                     ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                     (pkg (if (equal? pkg-path pkg) "" pkg))
                     (tgt (if-let ((t (assoc-val :tgt lbl)))
                                  (format #f "~A:~A" pkg t)
                                  (error 'fixme "bash tool has :tgts"))))
                (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
                tgt)
              arg)))))

;; (system "...script..."), (bash "...script...")
(define (normalize-action-shell-cmd ws pkg action action-list targets deps)
  ;; FIXME: shell cmd args may include filename literals; find way to expand?
  ;; FIXME: may include ${target}
  ;; FIXME: in general: expand all '${}' in args
  (format #t "~A: ~A\n" (ublue "normalize-action-shell-cmd") action)
  (format #t "~A: ~A~%" (white "action-list") action-list)
  (format #t "~A: ~A~%" (white "action-args") (cdr action-list))

  ;; search deps for tool, if not found, add it to (:deps ::tool)?

  (let* ((tool (if (eq? action 'system) 'sh action))
         (_ (format #t "~A: ~A~%" (white "tool") tool))
         (action-args (cdr action-list))
         (_ (format #t "~A: ~A~%" (white "action-args") action-args))
         (expanded-args (expand-cmd-args* action-args pkg targets deps))
         )
    (format #t "~A: ~A~%" (uwhite "expanded-args") expanded-args)
    `((:cmd
       (:tool ,tool)
       (:args ,@expanded-args)))))
;)
