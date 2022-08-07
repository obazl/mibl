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

;; (system "...script..."), (bash "...script...")
(define (normalize-action-shell-cmd ws pkg action action-list targets deps)
  ;; FIXME: shell cmd args may include filename literals; find way to expand?
  ;; FIXME: may include ${target}
  ;; FIXME: in general: expand all '${}' in args
  (format #t "~A: ~A\n" (blue "normalize-action-shell-cmd") action)
  (format #t "~A: ~A~%" (blue "action-list") action-list)
  (let* ((tool (if (eq? action 'system) 'sh action))
         (k (case action
              ((bash) :bash)
              ((system) :sh)
              (else :script)))
         (_ (format #t "k: ~A~%" k))
         (action-args (cdr action-list)))
    `((:cmd
       (:tool ,tool)
       (:args ,@action-args)))))
