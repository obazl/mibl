(define (-analyze-files filelist)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-analyze-files") filelist))
  (let recur ((files filelist)
              (singletons '())
              (renames '()))
    (if (null? files)
        (values singletons renames)
        (if (list? (car files))
            (recur (cdr files) singletons
                   ;; drop 'as' from (foo as bar)
                   (cons `(,(caar files) . ,(last (car files))) renames))
            (recur (cdr files) (cons (car files) singletons) renames)))))

;; (install
;;  (section lib)
;;  (package js_of_ocaml-compiler)
;;  (files
;;   bigarray.js ...))
;; becomes
;; (:exports-files (:opam-ws . js_of_ocaml-compiler) (:pkg . lib)
;;     (:singletons a b c) (:aliases (x .y)))

(define (dune-install->mibl ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (bgblue "dune-install->mibl") stanza))
  ;; write to exports table
  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (section (if-let ((section (assoc-val 'section (cdr stanza))))
                          (car section) #f))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (white "section") section)))
         (pkg (if-let ((pkg (assoc-val 'package (cdr stanza))))
                      (car pkg) #f))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (white "pkg") pkg)))
         (files (assoc-val 'files (cdr stanza)))
         )
    (let-values (((singletons aliases) (-analyze-files (assoc-val 'files (cdr stanza)))))
      (if *mibl-debug-s7*
          (begin
            (format #t "~A: ~A~%" (white "singletons") singletons)
            (format #t "~A: ~A~%" (white "aliases") aliases)))

      ;; to be used to write filegroups in this pkg,
      ;; and aliases in the corresponding opam-ws pkg
      (list `(:install (:opam-ws . ,pkg) (:opam-pkg . ,section)
                        (:singletons ,@singletons) (:aliases ,aliases)))
      ;; (error 'STOP "stop install files")
      )))

      ;; write to opam global table
      ;; key: package
      ;; val:  alias label,  name label
      ;; e.g. in compiler/bin-js_of_ocaml
      ;; (install (section lib) (package js_of_ocaml-compiler)
      ;;   (files runtime.js))
      ;; becomes
      ;; (js_of_ocaml-compiler
      ;;   (lib/runtime.js ((:pkg compiler/bin-js_of_ocaml-compiler
      ;;                    (:tgt runtime.js))))
      ;; within the opam pkg build file, the cdr will be namespaced
      ;; with @???

      ;; (let ((stanzas
      ;;        (flatten (map (lambda (file)
      ;;                        (if (list? file)
      ;;                            (if (equal? 'as (cadr file))
      ;;                                (let* ((fsrc (car file))
      ;;                                       (fsrc-str (format #f "~A" fsrc))
      ;;                                       (fdst (last file))
      ;;                                       (key (string-join (map symbol->string
      ;;                                                              (list section pkg fdst))
      ;;                                                         ":"))
      ;;                                       (fdst-str (format #f "~A" fdst))
      ;;                                       (tgt-pkg (format #f "~A/~A" pkg-path
      ;;                                                        (dirname fdst-str))))
      ;;                                  ;; key is only for dune lookups
      ;;                                  ;; FIXME: verify that src file exists? no, it could be genruled

      ;;                                  ;; if (dirname fdst) does not
      ;;                                  ;; exist we will to genrule it so
      ;;                                  ;; it is a target in this pkg,
      ;;                                  ;; then add to exports so other
      ;;                                  ;; pkgs can refer to it
      ;;                                  ;; e.g. as :lib:foo:<file>
      ;;                                  (if (file-exists? (dirname fdst-str))
      ;;                                      (update-filegroups-table! ws tgt-pkg (string->keyword (format #f "~A" fsrc)) fsrc)
      ;;                                      (let ((key (string-join (map symbol->string
      ;;                                                                   (list section pkg fdst))
      ;;                                                              ":")))
      ;;                                        (update-exports-table! ws
      ;;                                                               :FIXME ;; tag
      ;;                                                               fdst-str ;; name
      ;;                                                               pkg-path (string->symbol key))
      ;;                                        (update-exports-table! ws
      ;;                                                               :FIXME ;; tag
      ;;                                                               fdst-str ;; name
      ;;                                                               pkg-path (string->symbol key)))
      ;;                                      )
      ;;                                  ;; if dest is different pkg do NOT map it here
      ;;                                  (format #t "~A: ~A~%" (uwhite "as-list") key)
      ;;                                  ;;(string->symbol fsrc) ;; (list (cons ':export fsrc))
      ;;                                  (vector fsrc fdst))
      ;;                                ;; else list w/o 'as'
      ;;                                (let ((fs
      ;;                                       (map (lambda (f)
      ;;                                              (let ((key (string-join (map symbol->string
      ;;                                                                           (list section pkg f))
      ;;                                                                      ":")))
      ;;                                                (format #t "~A: ~A~%" (uwhite "install files list w/o as") key)
      ;;                                                f ;; (cons ':export f)
      ;;                                                ))
      ;;                                            file)))
      ;;                                  (format #t "~A: ~A~%" (ured "files w/o as") fs)
      ;;                                  fs))
      ;;                            (let ((key (string-join (map symbol->string
      ;;                                                         (list section pkg file))
      ;;                                                    ":")))
      ;;                              (format #t "~A: ~A~%" (uwhite "not list") key)
      ;;                              ;;(list (cons ':export file))
      ;;                              file
      ;;                              ))
      ;;                        )
      ;;                      files))))
      ;;   (format #t "~A: ~A~%" (ured "exporting files") stanzas)
      ;;   (if-let ((files (assoc 'files (cdr stanza))))
      ;;           (if (equal? (car files) 'main.exe)
      ;;               (error 'STOP "stop install")))
      ;;   (list (cons :exports-files stanzas))
      ;;   )
      ;; )))
    ;;      (ftarget (last files))
    ;;      (key (string-join (map symbol->string
    ;;                             (list section pkg ftarget))
    ;;                        ":"))
    ;;      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (white "key") key)))
    ;;      )
    ;; (list (cons ':install (cdr stanza)))))
