(if *mibl-debug-s7-loads*
    (format #t "loading dune/action_dsl.scm\n"))

(set! *mibl-debug-action-dsl* #t)

;; variables (pct-vars) - see https://dune.readthedocs.io/en/stable/concepts.html#variables

;; string args to e.g. echo may contain percent vars.
;; e.g. (echo "let cppo_version = \"%{version:cppo}\"")

;; this routine must parse the pct vars in arg s.
;; if the pct vars can be resolved to strings we return a string;
;; otherwise we return a list of substrings with pct vars intermixed.
;; e.g. (:string "let cppo_version = \"" (% (:version . cppo)) "\"")
;; client can then resolve and join.

(define (string-parse-pct-vars s) ;;FIXME obsolete, use dsl:string->lines
  (mibl-trace-entry "string-parse-pct-vars" s :test *mibl-debug-action-dsl*)
  ;; 1. parse out the pct-vars
  ;; 2. resolve them if possible
  ;; 3. construct result
  '((:string "let test_var = \"" (:% (:FIXME . PCTVARS)) "\"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pct-regex "(%{[^}]+})")
(define rgx (regex.make))
(define rx (regcomp rgx pct-regex REG_EXTENDED))
(define match-ct 2)

;; input: string or sym of form %{<str>}
;; only called if string known to be a pct var
;; if unsure call dsl:expand-word
;; returns (:input  pfx sfx) or (:input  str)
(define (parse-pct-var arg)
  (mibl-trace-entry "parse-pct-var" arg :test *mibl-debug-action-dsl*)
  (mibl-trace "arg type" (type-of arg) :test *mibl-debug-action-dsl*)
  (case arg
    ((%{target} %{targets}) ::outputs)
    ((%{deps}) ::inputs)
    (else
     (let ((arg-str (format #f "~A" arg)))
       (cond
        ((string=? "%{targets}" arg-str) ::outputs)
        ((string=? "%{target}" arg-str) ::outputs)
        ((string=? "%{deps}" arg-str) ::inputs)
        (else
         (if (string-prefix? "%{" arg-str)
           (let* ((len (length arg-str))
                  (vstr (substring arg-str 2 (- len 1)))
                  (sym (string->symbol vstr)))
             (mibl-trace "vstr" vstr :test *mibl-debug-action-dsl*)
             (mibl-trace "sym" sym :test *mibl-debug-action-dsl*)
             (let ((splits (string-split vstr #\:)))
               (mibl-trace "splits" splits :test *mibl-debug-action-dsl*)
               (if (equal? (list vstr) splits) ;; singleton e.g. %{foo}
                   (if (string? arg)
                       (if (member sym '(target targets))
                           ::outputs
                           `(:string (:input  . ,sym)))
                       (if (member sym '(target targets))
                           ::outputs
                           `(:input  . ,sym)))
                   ;; else dyad e.g. %{foo:bar}; won't every be %{target}, %{targets}
                   (if (string? arg)
                       `(:string (:input  ,(symbol (car splits))
                                          ,(string->symbol (string-join (cdr splits) ":"))))
                       `(:input  ,(symbol (car splits))
                                 ,(string->symbol (string-join (cdr splits) ":")))))))
           ;; else not a pct-var
           arg)))))))

(define (dsl:expand-word arg)
  (mibl-trace-entry "dsl:expand-word" arg :test *mibl-debug-action-dsl*)
  (mibl-trace "arg type" (type-of arg) :test *mibl-debug-action-dsl*)
  (let* ((arg-type (type-of arg))
         (arg-str (format #f "~A" arg)))
    (let recur ((s arg-str)
                (segs '()))
      (mibl-trace "recur on string" s :test *mibl-debug-action-dsl*)
      (mibl-trace "len string" (length s) :test *mibl-debug-action-dsl*)
      (mibl-trace "segs" segs :test *mibl-debug-action-dsl*)
      (if (= 0 (length s))
          (case arg-type
            ((string?)
             (mibl-trace "STRING arg" arg :test *mibl-debug-action-dsl*)
             ;; remove nested :string tags
             (let ((x (map (lambda (seg)
                             (if (list? seg)
                                 (if (eq? :string (car seg))
                                     (cadr seg)
                                     seg)
                                 seg))
                           segs)))
               (mibl-trace "expanded w" x :test *mibl-debug-action-dsl*)
               (if (= 1 (length x)) (car x)
                   (if (eq? :string (car x)) x `(:string ,@x)))))
            ((symbol?)
             (mibl-trace "SYM arg" arg :test *mibl-debug-action-dsl*)
             `(:string ,@segs))
            ((integer? number?)
             (mibl-trace "NUM arg" arg :test *mibl-debug-action-dsl*)
             (if (< (length segs) 2)
                 (car segs) segs))
            (else
             (error 'Bad-pctvar-arg
                    (format #f "Bad pctvar arg type: ~A, ~A"
                            arg (type-of arg)))))

          (let ((slen (length s))
                (res (regexec rgx s match-ct 0)))
            (mibl-trace "slen" slen :test *mibl-debug-action-dsl*)
            (mibl-trace "rgx res" res :test *mibl-debug-action-dsl*)
            (if (int-vector? res)
                ;; so, eo: start, end offset
                ;; rm: rgx match offsets over whole
                ;; match: matched subexp offsets
                (let* ((rm-so (int-vector-ref res 0))
                       (rm-eo (int-vector-ref res 1))
                       (match-so (int-vector-ref res 2))
                       (match-eo (int-vector-ref res 3)))
                  (if (and (eq? 0 rm-so) (eq? 0 match-so)
                           (eq? slen rm-eo) (eq? slen match-eo))
                      (let* ((match (substring s match-so match-eo))
                             (pct (parse-pct-var match))
                             )
                        (mibl-trace "match" match :test *mibl-debug-action-dsl*)
                        (mibl-trace "expanded pct" pct :test *mibl-debug-action-dsl*)
                        (mibl-trace "NO PFX/SFX" pct :test *mibl-debug-action-dsl*)
                        ;; dsl-str type is string?, so parse-pct-var
                        ;; will return (:string (:input  ...)). remove :string.
                        ;; nothing left to recur on so return directly
                        (if (truthy? segs)
                            (append segs (if (list? pct) (cadr pct) pct))
                            (if (list? pct) (cadr pct) pct)))
                      ;; else we have a pfx and/or sfx, so we need to recur
                      (let* ((pfx  (substring s 0 rm-so))
                             (sfx  (substring s rm-eo))
                             (match (substring s match-so match-eo))
                             (pct (parse-pct-var match)))
                        (mibl-trace "pfx" pfx :test *mibl-debug-action-dsl*)
                        (mibl-trace "match" match :test *mibl-debug-action-dsl*)
                        (mibl-trace "pct" pct :test *mibl-debug-action-dsl*)
                        (mibl-trace "sfx" sfx :test *mibl-debug-action-dsl*)
                        ;; (error 'b "b")
                        (recur sfx (append segs (if (> match-so 0)
                                                    (list pfx pct)
                                                    pct))))))
                ;; else no match: exit directly?
                (recur "" (append segs (list s)))))))))

(define (pct-var->keyword v)
  (if :test *mibl-debug-s7*
      (format #t "~A: ~A~%" (blue "pct-var->var") v))
  (let* ((v (format #f "~A" v))
         (len (length v))
         ;; remove %{ and }
         (vstr (substring v 2 (- len 1)))
         (kw (string->keyword vstr)))
    (if :test *mibl-debug-s7*
        (begin
          (format #t "vstr ~A\n" vstr)
          (format #t "kw ~A\n" kw)))
    kw))

;; examples:
;;  char *s = "\
;; \ndiff -u  --label test.output --label test-partitions.output test.output test-partitions.output > diff-with-without-partitions || true\
;; \n" ;

;; (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
;; =>  ((:input  ocaml-config c_compiler) -c -I (:input  ocaml-config standard_library) -o ::outputs (:input  . deps))


;; input: string w/o newlines
;; output: list of words
(define (dsl:line->words dsl-line)
  (mibl-trace-entry "dsl:line->words" dsl-line :test *mibl-debug-action-dsl*)
  (mibl-trace "dsl-line type" (type-of dsl-line) :test *mibl-debug-action-dsl*)
  (let* ((slen (length dsl-line))
         (str (string-trim '(#\space #\tab) dsl-line))
         (words (string-split str #\space)))
    (mibl-trace "words" words :test *mibl-debug-action-dsl*)
    ;; (let ((x (map parse-pct-var words)))
    (let ((x (map dsl:expand-word words)))
      (mibl-trace "Expanded words" x :test *mibl-debug-action-dsl*)
      ;;(error 'words "words")
      `(:line ,@x))))

;; ))
;;      (else
;;       (error 'Bad-dsl-str-arg
;;              (format #f "bad dsl-str arg: ~A, ~A" dsl-str (type-of dsl-str))))))

;; input: string
;; output: list; input split on newlines, pctvars expanded
;;(define (destructure-dsl-string dsl-str)
(define (dsl:string->lines dsl-str)
  (mibl-trace-entry "dsl:string->lines" dsl-str :test *mibl-debug-action-dsl*)
  (mibl-trace "dsl-str type" (type-of dsl-str) :test *mibl-debug-action-dsl*)
  (case (type-of dsl-str)
    ((integer? number?) dsl-str)
    ((symbol?) (parse-pct-var dsl-str))
    ((string?)
     (let* ((lines (string-split dsl-str #\newline))
            (mibl-trace-let "lines" lines :test *mibl-debug-action-dsl*)
            (expanded-lines (map dsl:line->words lines)))
       (mibl-trace "expanded lines" expanded-lines :test *mibl-debug-action-dsl*)
       (mibl-trace "xxxxxxxxxxxxxxxx"  *mibl-debug-action-dsl*  :test *mibl-debug-action-dsl*)
       (if (> (length expanded-lines) 1)
           expanded-lines
           (car expanded-lines))))))

;; directives take string args
(define (destructure-directive-arg dsl-arg)
  (mibl-trace-entry "destructure-directive-arg" dsl-arg :test *mibl-debug-action-dsl*)
  (mibl-trace "dsl-arg type" (type-of dsl-arg) :test *mibl-debug-action-dsl*)
  (case (type-of dsl-arg)
    ((string?)
     (let* ((str (string-trim '(#\space #\newline) dsl-arg)))
       ;; may have embedded pctvar, e.g. "foo/%{bar}"
       (dsl:string->lines str)))
    ((symbol?) (parse-pct-var dsl-arg))
    ((number?) dsl-arg)
    (else
     (error 'Bad-dsl-arg "Bad dsl-arg"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION: (dsl:action->mibl action) -> list
;; destructures but does not expand; e.g. %{foo} -> (:input . foo) w/o resolving
;; action grammar: (action <dsl>)
;; usually (action (cmd ...)) but may be (action cmd ...)
;; cmds are of 2 kinds:
;;   direct tools (e.g. cat, echo) parameterized by file or string
;;       e.g. cat, diff, echo
;;   'directives' that wrap a direct cmd; parameterized by dsl exp
;;       e.g. run, with-stdout-to, etc.

;; so actions are inductively defined, sorta. base cases are those
;; cmds that do not take a dsl arg: the direct shell cmds, bash,
;; system, run, dynamic-run.

;; action examples:
;; (action (echo "Hello world"))
;; (action (cmp a.ml b.ml))
;; (action (bash "# shell script here..."))
;; (action (with-stdout-to %{targets} (cat %{deps})))
;; (action (chdir %{workspace_root}
;;                (run %{bin:tezos-embedded-protocol-packer}
;;                     "%{src_dir}"
;;                     "demo_counter"
;;                     >
;;                     %{target})))

;; input: (cdr (action (cmd ..)))
(define (dsl:action->mibl action)
  (mibl-trace-entry "dsl:action->mibl" action :color bgblue :test *mibl-debug-action-dsl*)
  (flush-output-port)
  (let* ((action-list (car action))
         (mibl-trace-let "action-list" action-list :test *mibl-debug-action-dsl*)
         (primary-cmd (if (pair? (car action-list))
                          (caar action-list)
                          ;; else (action tool ...) - illegal but may happen?
                          (car action-list)))
         (mibl-trace-let "primary cmd" primary-cmd :test *mibl-debug-action-dsl*))
    (case primary-cmd
      ;; base cases - do not take a dsl arg
      ((cat cmp copy copy# diff diff?) ;; etc.
       (begin
         (mibl-trace "direct cmd" primary-cmd :test *mibl-debug-action-dsl*)
         (let* ((-args (cdr action-list))
                (args (map dsl:string->lines -args)))
           (mibl-trace "args" args :test *mibl-debug-action-dsl*)
           `((:shell . sh) (:tool . ,(symbol->keyword primary-cmd)) (:args ,@args)))))

      ((chdir) `((:shell . sh) (:tool . :chdir) (:args)))

      ((echo) `((:shell . sh) (:tool . :echo) (:args ,@(cdr action-list))))

      ;; inductive cases
      ((bash)
       ;; HEURISTIC: first word of cmd string is tool
       (let* ((bash-cmd (destructure-shell-cmd action-list :shell 'bash))
              (bash-lines (assoc-val :cmd-lines bash-cmd)))
         (if (truthy? bash-lines)
             (let* ((fst (car bash-lines))
                    ;; skip over :line key
                    (bash-tool (cadr fst)))
               (mibl-trace "bash-cmd" bash-cmd :test #t :color red)
               (mibl-trace "bash-lines" bash-lines :test #t :color red)
               (mibl-trace "bash-fst" fst :test #t :color red)
               (mibl-trace "bash-tool" bash-tool :test #t :color red)
               `((:tool . ,(if (list? bash-tool)
                               (list bash-tool)
                               bash-tool))
                 ,@bash-cmd))
             bash-cmd)))

      ((system)
       ;; same as bash, but with sh
       ;; HEURISTIC: first word of cmd string is tool
       (let* ((system-cmd (destructure-shell-cmd action-list :shell 'sh))
              (system-lines (assoc-val :cmd-lines system-cmd)))
         (if (truthy? system-lines)
             (let* ((fst (car system-lines))
                    ;; skip over :line key
                    (system-tool (cadr fst)))
               (mibl-trace "system-cmd" system-cmd :test #t :color red)
               (mibl-trace "system-lines" system-lines :test #t :color red)
               (mibl-trace "system-fst" fst :test #t :color red)
               (mibl-trace "system-tool" system-tool :test #t :color red)
               `((:tool . ,(if (list? system-tool)
                               (list system-tool)
                               system-tool))
                 ,@system-cmd))
             system-cmd)))

      ((run)
       (if (eq? 'bash (car (cdr action-list)))
           (destructure-shell-cmd (cdr action-list) :shell 'bash)
           (let* ((-args (cdr action-list))
                  (args (map dsl:string->lines -args))
                  (secondary-cmd (list (car args)))
                  (secondary-args (cdr args)))
             (mibl-trace "run args" -args :test *mibl-debug-action-dsl*)
             (mibl-trace "destructured run args" args :test *mibl-debug-action-dsl*)
             (mibl-trace "secondary cmd" secondary-cmd :test *mibl-debug-action-dsl*)
             (mibl-trace "secondary args" secondary-args :test *mibl-debug-action-dsl*)
             ;; (error 'x "X")
             `((:tool . ,secondary-cmd) (:args ,@secondary-args)))))

      ((progn)
       (let ((progn-result (fold (lambda (action accum)
                                   (let ((cmds (dsl:action->mibl (list action))))
                                     (if (eq? :cmds (car cmds))
                                         (append accum (cadr cmds))
                                         (append accum `((:cmd ,@cmds))))))
                                 '() (cdr action-list))))
         (mibl-trace "progn-result" progn-result :test *mibl-debug-action-dsl*)
         (for-each (lambda (progn)
                     (mibl-trace "progn" progn :test *mibl-debug-action-dsl*)
                     #f)
                   progn-result)
         `(:cmds ,progn-result)))

      ((ignore-outputs)
       ;;,normalize-action-ignore-outputs-dsl
       )
      ((ignore-stderr)
       ;;,normalize-action-ignore-stderr-dsl
       )
      ((ignore-stdout)
       ;; ,normalize-action-ignore-stdout-dsl
       )
      ((no-infer) ;; (no-infer <DSL>)
       ;;,normalize-action-no-infer-dsl
       )
      ;; (pipe-<outputs> <DSL> <DSL> <DSL>...)
      ((pipe-outputs)
       ;; ,normalize-action-pipe-outputs-dsl
       )
      ((pipe-stderr)
       ;; ,normalize-action-pipe-stderr-dsl
       )
      ((pipe-stdout)
       ;; ,normalize-action-pipe-stdout-dsl
       )
      ((setenv)     ;; (setenv <var> <value> <DSL>)
       ;;,normalize-action-setenv-dsl
       )
      ;; NB: (run env FOO=bar ...)
      ;; (with-accepted-exit-codes <pred> <DSL>)
      ((with-accepted-exit-codes)
       ;; ,normalize-action-with-accepted-exit-codes-dsl
       )
      ;; (with-<outputs>-to <file> <DSL>)
      ((with-outputs-to)
       ;; ,normalize-action-with-outputs-to-dsl
       )

      ((with-stderr-to)
       (let* ((stderr (let ((arg (cadr action-list)))
                        (mibl-trace "arg" arg :test *mibl-debug-action-dsl*)
                        (mibl-trace "arg t" (type-of arg) :test *mibl-debug-action-dsl*)
                        (destructure-directive-arg arg)))
              (stderr (if (list? stderr)
                          (if (eq? :line (car stderr)) (cadr stderr) stderr)
                          stderr))
              (subaction-list (cddr action-list)))
         (mibl-trace "Stderr" stderr :test *mibl-debug-action-dsl*)
         (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-dsl*)
         ;; recur
         (let ((subber (dsl:action->mibl subaction-list)))
           (mibl-trace "subber" subber :test *mibl-debug-action-dsl*)
           ;; always make stderr fname a sym?
           `((:stderr ,(if (list? stderr)
                           (if (string? (car stderr))
                               (car stderr)
                               stderr)
                           stderr))
             (:cmd ,@subber)))))

      ((with-stdout-to) ;; (with-stdout-to "%{targets}" (cat "%{deps}"))
       (let* ((stdout (let ((fd (cadr action-list)))
                        (mibl-trace "fd" fd :test *mibl-debug-action-dsl*)
                        (mibl-trace "fd t" (type-of fd) :test *mibl-debug-action-dsl*)
                        (destructure-directive-arg fd)))
              (stdout (if (list? stdout)
                          (if (eq? :line (car stdout)) (cadr stdout) stdout)
                          stdout))
              (mibl-trace-let "STDOUT" stdout :test *mibl-debug-action-dsl*)
              (subaction-list (cddr action-list))
              (secondary-cmd (car subaction-list)))
         ;; subaction-list:  ((cat "%{deps}"))
         (mibl-trace "Stdout" stdout :test *mibl-debug-action-dsl*)
         (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-dsl*)
         ;; recur
         (let ((subber (dsl:action->mibl subaction-list)))
           (mibl-trace "subber" subber :test *mibl-debug-action-dsl*)
           ;; always make stdout fname a sym
           `((:stdout ,stdout)
             ,(if (eq? 'progn (caar subaction-list))
                  subber
                  `(:cmd ,@subber))))))

      ((with-stdin-from);; (with-stdin-from <file> <DSL>)
       ;; ,normalize-action-with-stdin-from-dsl
       )

      ((write-file)
       (let* ((output (let ((fd (cadr action-list)))
                        (mibl-trace "fd" fd :test *mibl-debug-action-dsl*)
                        (mibl-trace "fd t" (type-of fd) :test *mibl-debug-action-dsl*)
                        (destructure-directive-arg fd)))
              (output (if (list? output)
                          (if (eq? :line (car output)) (cadr output) output)
                          output))
              (mibl-trace-let "OUTPUT" output :test *mibl-debug-action-dsl*)
              (subaction-list (cddr action-list))
              (content (car subaction-list)))
         (mibl-trace "Output" output :test *mibl-debug-action-dsl*)
         (mibl-trace "content" content :test *mibl-debug-action-dsl*)
         `(:cmd (:tool :write-file)
                (:args ,output ,content))))

      (else (error 'Missing-cmd-directive "Missing cmd directive")))
          ))
