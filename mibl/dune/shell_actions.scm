

;; (system "...script..."), (bash "...script...")
(define (normalize-action-shell-cmd ws pkg action action-alist targets deps)
  ;; FIXME: shell cmd args may include filename literals; find way to expand?
  ;; FIXME: may include ${target}
  ;; FIXME: in general: expand all '${}' in args
  (format #t "~A: ~A\n" (blue "normalize-action-shell-cmd") action)
  (let* ((tool (if (eq? action 'system) 'sh action))
         (k (case action
              ((bash) :bash)
              ((system) :sh)
              (else :script)))
         (_ (format #t "k: ~A~%" k))
         (action-args (assoc-val action action-alist)))
    `((:cmd
       (:tool ,tool)
       (:args (,k ,@action-args))))))
