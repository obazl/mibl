(define (foreign-archives->mibl fld)
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "foreign-archives->mibl") fld))
  `(:cc-archives ,fld))

(define (foreign-stubs->mibl name fld)
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "foreign-stubs->mibl") fld))
  (let ((lang (car (assoc 'language (cdr fld))))
        (res (map (lambda (fld)
                    (case (car fld)
                      ((language)
                       (case (cadr fld)
                         ((c) '(:cc #t))
                         (else fld)))
                      ((names) `(:srcs ,@(cdr fld)))
                      ((flags) `(:flags ,@(cdr fld)))
                      ((include_dirs) `(:includes ,@(cdr fld)))
                      ((extra_deps) `(:extra-deps ,@(cdr fld)))

                      ;; obsolete but still in the wild:
                      ((c_names) `(:c-srcs ,@(cdr fld)))
                      ((c_flags) `(:c-flags ,@(cdr fld)))
                      ((cxx_names) `(:cxx-srcs ,@(cdr fld)))
                      ((cxx_flags) `(:cxx-flags ,@(cdr fld)))

                      (else
                       (error 'FIXM
                              (format
                               #f "unrecognize foreign-stubs fld" fld)))))
                  (cdr fld))))
    (if (assoc :cc res)
        `(:cc-stubs (:name . ,name) ,@(dissoc '(:cc) res))
        `(:foreign-stubs (:name . ,name) ,res))))

(define (c_names->mibl stanza-name fld)
  (if *debugging*
      (format #t "~A: ~A~%" (bgblue "c_names->mibl") fld))
  `(:cc-stubs (:name ,stanza-name) (:c-srcs ,fld)))
  ;; (let ((lang (car (assoc 'language (cdr fld))))
  ;;       (res (map (lambda (fld)
  ;;                   (case (car fld)
  ;;                     ((language)
  ;;                      (case (cadr fld)
  ;;                        ((c) '(:cc #t))
  ;;                        (else fld)))
  ;;                     ((names) `(:srcs ,@(cdr fld)))
  ;;                     ((flags) `(:flags ,@(cdr fld)))
  ;;                     ((include_dirs) `(:includes ,@(cdr fld)))
  ;;                     ((extra_deps) `(:extra-deps ,@(cdr fld)))

  ;;                     ;; obsolete but still in the wild:
  ;;                     ((c_names) `(:c-srcs ,@(cdr fld)))
  ;;                     ((c_flags) `(:c-flags ,@(cdr fld)))
  ;;                     ((cxx_names) `(:cxx-srcs ,@(cdr fld)))
  ;;                     ((cxx_flags) `(:cxx-flags ,@(cdr fld)))

  ;;                     (else
  ;;                      (error 'FIXM
  ;;                             (format
  ;;                              #f "unrecognize foreign-stubs fld" fld)))))
  ;;                 (cdr fld))))
  ;;   (if (assoc :cc res)
  ;;       `(:cc-stubs (:name . ,name) ,@(dissoc '(:cc) res))
  ;;       `(:foreign-stubs (:name . ,name) ,res))))
