;; (display "pkg_api.scm") (newline)

(define (find-in-filegroups triple)
  ;; triple: (:key (:pkg . foo) (:tgt . bar))
  ;; or (::glob (:pkg . foo) (:glob . "bar.*"))
  triple
  )

;; return normalized path relative to ws-root
;; FIXME: returns #f for non-existent paths
;; use ->canonical-path instead
(define (normalize-pkg-path path ws-root)
  (if *mibl-debug-all*
      (format #t "~A: ~A~%" (blue "normalize-pkg-path") path))
  ;; path is not in pkg-path dir
  ;; (let ((rp ((*libc* 'realpath) path '())))
  (let ((rp (libc:realpath path '())))
    (if *mibl-debug-all*
        (begin
          (format #t "f rel path: ~A\n" path)
          (format #t "f realpath: ~A\n" rp)
          (format #t "ws root: ~A\n" ws-root)
          (format #t "pfx?: ~A\n" (string-prefix? ws-root rp))))
    (if (string-prefix? ws-root rp)
        (string-drop rp (+ 1 (string-length ws-root)))
        "problem")))

;;FIXME: put this in file_utils.scm?
(define filename->file-assoc
  (let ((+documentation+ "For now just stringify and pair with ext."))
    (lambda (filename)
      (let* ((fname (if (symbol? filename) (symbol->string filename)
                        filename))
             (ext (filename-extension fname))
             (pname (principal-name fname)))
        (cons (string->keyword (if ext (string-drop ext 1) ":file"))
              fname)))))

(define (filename->kind filename)
  (let* ((fname (format #f "~A" filename))
         (ext (filename-extension fname)))
    (if ext
        (cond
         ((string=? ext ".mli") :sig)
         ((string=? ext ".ml") :struct)

         ((string=? ext ".sh") :script)
         ((string=? ext ".js") :script)
         ((string=? ext ".py") :script)

         ((string=? ext ".c") :src)
         ((string=? ext ".h") :src)
         ((string=? ext ".cxx") :src)
         ((string=? ext ".rs") :src)

         ((string=? ext ".dat") :data)
         ((string=? ext ".json") :data)
         (else :file))
        #<undefined>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rsrc-list: :signatures or :structures
;; may remove item from list
(define (find-module-in-rsrc-list!? m-name tgt rsrc-list)
  (if *mibl-debug-all*
      (begin
        (format #t "~A: ~A~%" (blue "find-module-in-rsrc-list!?") m-name)
        (format #t "~A: ~A~%" (white "tgt") tgt)
        (format #t "~A: ~A~%" (white "singletons") rsrc-list)))
  (if rsrc-list
      (let* ((rsrc-alists (cdr rsrc-list))
             (statics (assoc :static rsrc-alists))
             (dynamics (assoc :dynamic rsrc-alists))
             (fileset (if statics statics
                          (if dynamics dynamics
                              '()))))
        (if *mibl-debug-all*
            (begin
              (format #t "~A: ~A~%" (white "statics") statics)
              (format #t "~A: ~A~%" (white "dynamics") dynamics)))

        (let ((match (find-if (lambda (sig)
                                (if *mibl-debug-all*
                                    (format #t "~A: ~A~%" (green "sig") sig))
                                (equal? (format #f "~A" m-name)
                                        (format #f "~A" (car sig))))
                              (cdr fileset))))
          (if *mibl-debug-all*
              (format #t "~A: ~A~%" (red "MATCH") match))
          (if match
              ;; remove from pkg files, then return
              (begin
                (if *mibl-debug-all*
                    (format #t "~A: ~A from: ~A~%" (red "removing") match fileset))
                (let ((new (dissoc (list (car match)) (cdr fileset))))
                  (if *mibl-debug-all*
                      (format #t "~A: ~A~%" (red "new fs") new))
                  (set-cdr! fileset new)
                  match))

              (let ((match (find-if (lambda (sig)
                                      (if *mibl-debug-all*
                                          (format #t "dynamic ~A: ~A~%" (green "sig") sig))
                                      #f)
                                    (if dynamics
                                        (cdr dynamics)
                                        '()))))
                (if match
                    #f
                    #f)))))
      #f))

(define (-remove-from-pkg-files kind sig pkg)
  (if *mibl-debug-all*
      (format #t "~A: ~A, ~A~%" (blue "-remove-from-pkg-files") kind sig))
  ;; (format #t "~A: ~A~%" (blue "pkg") pkg)
  (let ((fileset (assoc kind pkg)))
    (if *mibl-debug-all*
        (format #t "~A: ~A~%" (cyan "fileset") fileset)))
  )

;; e.g. (cdr (:standard (symbol "\\") legacy_store_builder))
(define (pkg->module-names pkg) ;; seq)
  (let ((modules (assoc-val :modules pkg)))
    (map car modules)))

  ;; (let recur ((assoc :
  ;;             (modnames '()))
  ;;   (if (null? srcfiles)
  ;;       modnames
  ;;       (let ((m (filename->module-name (car srcfiles))))
  ;;         (if (member m modnames) ;; avoid .ml/.mli dups
  ;;             (recur (cdr srcfiles) modnames)
  ;;             (recur (cdr srcfiles) (cons m modnames)))))))

(define (pkg->sigs pkg)
  (let* ((statics (if-let ((structs (assoc-in
                                     '(:signatures :static) pkg)))
                          (cdr structs) '()))
         (dynamics (if-let ((structs (assoc-in
                                      '(:signatures :dynamic) pkg)))
                           (cdr structs) '()))
         (sigs (concatenate statics dynamics)))
    sigs))

(define (pkg->structs pkg)
  (let* ((statics (if-let ((structs (assoc-in
                                     '(:structures :static) pkg)))
                          (cdr structs) '()))
         (dynamics (if-let ((structs (assoc-in
                                      '(:structures :dynamic) pkg)))
                           (cdr structs) '()))
         (structs (concatenate statics dynamics)))
    structs))
