(define (dune-install->mibl ws pkg stanza)
  (format #t "~A: ~A~%" (ublue "dune-install->mibl") stanza)
  ;; write to exports table
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (section (if-let ((section (assoc-val 'section (cdr stanza))))
                          (car section) #f))
         (_ (format #t "~A: ~A~%" (white "section") section))
         (pkg (if-let ((pkg (assoc-val 'package (cdr stanza))))
                      (car pkg) #f))
         (_ (format #t "~A: ~A~%" (white "pkg") pkg))
         (files (assoc-val 'files (cdr stanza)))
         )
    (format #t "~A: ~A~%" (white "files") files)
    (let ((stanzas
           (flatten (map (lambda (file)
                           (if (list? file)
                               (if (equal? 'as (cadr file))
                                   (let* ((fsrc (car file))
                                          (fsrc-str (format #f "~A" fsrc))
                                          (fdst (last file))
                                          (key (string-join (map symbol->string
                                                                 (list section pkg fdst))
                                                            ":"))
                                          (fdst-str (format #f "~A" fdst))
                                          (tgt-pkg (format #f "~A/~A" pkg-path
                                                           (dirname fdst-str))))
                                     ;; key is only for dune lookups
                                     ;; FIXME: verify that src file exists? no, it could be genruled

                                     ;; if (dirname fdst) does not
                                     ;; exist we will to genrule it so
                                     ;; it is a target in this pkg,
                                     ;; then add to exports so other
                                     ;; pkgs can refer to it
                                     ;; e.g. as :lib:foo:<file>
                                     (if (file-exists? (dirname fdst-str))
                                         (update-filegroups-table! ws tgt-pkg (string->keyword fsrc) fsrc)
                                         (let ((key (string-join (map symbol->string
                                                                      (list section pkg fdst))
                                                                 ":")))
                                           (update-exports-table! ws (string->symbol key) fdst-str pkg-path)
                                           (update-exports-table! ws (string->symbol key) fdst-str pkg-path))
                                         )
                                     ;; if dest is different pkg do NOT map it here
                                     (format #t "~A: ~A~%" (uwhite "as-list") key)
                                     ;;(string->symbol fsrc) ;; (list (cons ':export fsrc))
                                     (vector fsrc fdst))
                                   ;; else list w/o 'as'
                                   (let ((fs
                                          (map (lambda (f)
                                                 (let ((key (string-join (map symbol->string
                                                                              (list section pkg f))
                                                                         ":")))
                                                   (format #t "~A: ~A~%" (uwhite "install files list w/o as") key)
                                                   f ;; (cons ':export f)
                                                   ))
                                               file)))
                                     (format #t "~A: ~A~%" (ured "files w/o as") fs)
                                     fs))
                               (let ((key (string-join (map symbol->string
                                                            (list section pkg file))
                                                       ":")))
                                 (format #t "~A: ~A~%" (uwhite "not list") key)
                                 ;;(list (cons ':export file))
                                 file
                                 )))
                         files))))
      (format #t "~A: ~A~%" (ured "exporting files") stanzas)
      (list (cons :exports-files stanzas))
      )
    ))
    ;;      (ftarget (last files))
    ;;      (key (string-join (map symbol->string
    ;;                             (list section pkg ftarget))
    ;;                        ":"))
    ;;      (_ (format #t "~A: ~A~%" (white "key") key))
    ;;      )
    ;; (list (cons ':install (cdr stanza)))))
