(if *mibl-debug-s7-loads*
    (format #t "loading dune/action_dsl.scm\n"))


(provide 'action-dsl.scm)
;; (provide 'dsl:action->mibl)
;;(provide 'dune/action_dsl.scm)

;; (set! *mibl-debug-action-dsl* #t)

;; (if (not *libc*)
;;     (varlet (rootlet 'regex.make (*libc* 'regex.make)
;;                      'regcomp (*libc* 'regcomp)
;;                      'regexec (*libc* 'regexec)
;;                      'REG_EXTENDED (*libc* 'REG_EXTENDED))
;;             ;; etc.
;;             ))

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
(define pct-regex "(%\\{[^}]+\\})")
(define rgx (libc:regex.make))
(libc:regcomp rgx pct-regex libc:REG_EXTENDED)
;; (define rgx ((*libc* 'regex.make)))
;; ((*libc* 'regcomp) rgx pct-regex (*libc* 'REG_EXTENDED))

;; (define rgx (regex.make))
;; (define rx (regcomp rgx pct-regex REG_EXTENDED))
(define match-ct 2)

;; input: string or sym of form %{<str>}
;; only called if string known to be a pct var
;; if unsure call dsl:expand-word
;; returns (%  pfx sfx) or (%  str)
(define (parse-pct-var arg)
  (mibl-trace-entry "parse-pct-var" arg :color red :test *mibl-debug-action-dsl*)
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
                           `(:string (%  . ,sym)))
                       (if (member sym '(target targets))
                           ::outputs
                           `(%  . ,sym)))
                   ;; else dyad e.g. %{foo:bar}; won't every be %{target}, %{targets}
                   (if (string? arg)
                       `(:string (%  ,(symbol (car splits))
                                          ,(string->symbol (string-join (cdr splits) ":"))))
                       `(%  ,(symbol (car splits))
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
                (res (libc:regexec rgx s match-ct 0)))
                ;; (res ((*libc* 'regexec) rgx s match-ct 0)))
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
                        ;; will return (:string (%  ...)). remove :string.
                        ;; nothing left to recur on so return directly
                        (if (truthy? segs)
                            (append segs (if (list? pct) (cadr pct) pct))
                            pct))
                            ;; (if (list? pct) (cadr pct) pct)))
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
  (if :test *mibl-debug-all*
      (format #t "~A: ~A~%" (blue "pct-var->var") v))
  (let* ((v (format #f "~A" v))
         (len (length v))
         ;; remove %{ and }
         (vstr (substring v 2 (- len 1)))
         (kw (string->keyword vstr)))
    (if :test *mibl-debug-all*
        (begin
          (format #t "vstr ~A\n" vstr)
          (format #t "kw ~A\n" kw)))
    kw))

;; examples:
;;  char *s = "\
;; \ndiff -u  --label test.output --label test-partitions.output test.output test-partitions.output > diff-with-without-partitions || true\
;; \n" ;

;; (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
;; =>  ((%  ocaml-config c_compiler) -c -I (%  ocaml-config standard_library) -o ::outputs (%  . deps))


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
;; destructures but does not expand; e.g. %{foo} -> (% . foo) w/o resolving
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
(define* (dsl:action->mibl action (ctx #f))
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
      ((cat cmp copy copy# diff diff?) ;; etc.
       ;; base cases - do not take a dsl arg, no shell specified
       ;; (i.e. build engine may use spawn etc.)
       (begin
         (mibl-trace "direct cmd" primary-cmd :test *mibl-debug-action-dsl*)
         (let* ((-args (cdr action-list))
                (args (map dsl:string->lines -args))
                ;; if args are strings we get (:line "arg")
                ;; remove :lines key
                (args (map (lambda (arg)
                             (if (list? arg)
                                 (if (eq? :line (car arg))
                                     (symbol (cadr arg))
                                     arg)
                                 arg))
                           args))
                )
           (mibl-trace "base args" args :test *mibl-debug-action-dsl*)
           `((:cmd
              (:tool . ,(symbol->keyword primary-cmd))
              (:shell . #<unspecified>)
              (:args ,@args))))))

      ((chdir) `((:cmd (:tool . :chdir) (:shell . #<unspecified>) (:args))))

      ((echo) `((:cmd (:tool . :echo) (:shell . #<unspecified>) (:args ,@(cdr action-list)))))

      ;; inductive cases
      ((bash)
       ;; HEURISTIC: first word of cmd string is tool
       (let* ((bash-cmd (destructure-shell-cmd action-list :shell 'bash))
              (bash-lines (assoc-val :cmd-lines bash-cmd)))
         (mibl-trace "bash-lines" bash-lines :test #f)
         (if (truthy? bash-lines)
             (let* ((fst (car bash-lines))
                    ;; skip over :line key
                    (bash-tool (cdr fst))
                    (bash-tool (if (eq? :string (car bash-tool))
                                   (car bash-tool)
                                   ;; (begin
                                   ;;   (if (> (length (cdr bash-tool)) 1)
                                   ;;       bash-tool
                                   ;;       (cdr bash-tool)))
                                   (car bash-tool))))
               (mibl-trace "bash-cmd" bash-cmd :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "bash-lines" bash-lines :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "bash-fst" fst :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "bash-tool" bash-tool :test *mibl-debug-action-dsl* :color red)
               (let ((tool (if (pair? bash-tool)
                               (if (eq? :string (car bash-tool))
                                   (if (> (length (cdr bash-tool)) 1)
                                       bash-tool
                                       (cadr bash-tool))
                                   bash-tool)
                               bash-tool)))
                 `((:tool . ,(if (list? tool)
                                 (list tool)
                                 tool))
                   ,@bash-cmd)))
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
               (mibl-trace "system-cmd" system-cmd :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "system-lines" system-lines :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "system-fst" fst :test *mibl-debug-action-dsl* :color red)
               (mibl-trace "system-tool" system-tool :test *mibl-debug-action-dsl* :color red)
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
             (mibl-trace "run secondary cmd" secondary-cmd :test *mibl-debug-action-dsl*)
             (mibl-trace "run secondary args" secondary-args :test *mibl-debug-action-dsl*)
             `((:cmd (:tool . ,secondary-cmd) (:args ,@secondary-args))))))

      ((progn)
       (mibl-trace "progn" action-list :color red :test *mibl-debug-action-dsl*)
       (let ((progn-result (fold (lambda (action accum)
                                   (mibl-trace "progn action" action :color green :test *mibl-debug-action-dsl*)
                                   (let ((cmds (dsl:action->mibl (list action) :ctx 'progn)))
                                     (mibl-trace "progn cmds" cmds :color green :test *mibl-debug-action-dsl*)
                                     ;; nested progn produces ((:cmds (:cmd ...)))
                                     (if (eq? :cmds (car cmds))
                                         (append accum (cadr cmds))
                                         ;; nested run produces ((:cmd (:tool ...)))
                                         (if (eq? :cmd (caar cmds))
                                             ;; remove :cmd
                                             ;; (append accum (cons (cdar cmds) (cdr cmds)))
                                             (append accum cmds) ;; `((:cmd ,@cmds)))
                                             ;; else cmds = ((:tool ...))
                                             (append accum cmds ;; `((:cmd ,@cmds))
                                                     )))))
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
             ;;(:cmd ,@subber)
             subber
             ))))

      ((with-stdin-from);; (with-stdin-from <file> <DSL>)
       ;; same as with-stdout-to?
       (let* ((stdin (let ((fd (cadr action-list)))
                        (mibl-trace "fd" fd :test *mibl-debug-action-dsl*)
                        (mibl-trace "fd t" (type-of fd) :test *mibl-debug-action-dsl*)
                        (destructure-directive-arg fd)))
              (stdin (if (list? stdin)
                          (if (eq? :line (car stdin)) (cadr stdin) stdin)
                          stdin))
              (mibl-trace-let "STDIN" stdin :test *mibl-debug-action-dsl*)
              (subaction-list (cddr action-list))
              (secondary-cmd (car subaction-list)))
         ;; subaction-list:  ((cat "%{deps}"))
         (mibl-trace "Stdin" stdin :test *mibl-debug-action-dsl*)
         (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-dsl*)
         ;; recur
         (let* ((subcmd (dsl:action->mibl subaction-list))
                (mibl-trace-let "stdin subcmd" subcmd
                                :test *mibl-debug-action-dsl*)
                (cmd (case (caar subcmd) ;; subaction-list)
                       ;; deal with subcmds
                       ((progn) subcmd)
                       ((:cmd) subcmd)
                       ((:stdout)
                        ;; move :stdout out of subcmd
                        (cons (car subcmd)
                              (cdr subcmd)))
                       ;; no subcmds
                       (else
                        ;;`(:cmd ,@subcmd)
                        subcmd))))
           (mibl-trace "stdin cmd" cmd :test *mibl-debug-action-dsl*)
           `((:stdin ,stdin) ,@subcmd))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((with-stdout-to) ;; (with-stdout-to "%{targets}" (cat "%{deps}"))
       (let* ((stdout (let ((fd (cadr action-list)))
                        (mibl-trace "fd" fd :test *mibl-debug-action-dsl*)
                        (mibl-trace "fd t" (type-of fd) :test *mibl-debug-action-dsl*)
                        (destructure-directive-arg fd)))
              (mibl-trace-let "stdout a" stdout
                              :test *mibl-debug-action-dsl*)
              (stdout (if (list? stdout)
                          (case (car stdout)
                            ((:line)
                             ;; quoted string produce (:line (:string "foo"))?
                             ;; quoted pctvars to with-stdout-to produce (:line (:string (% . foo)))
                             ;; remove :line, and :string if it contains no pfx/sfx
                             (case (type-of (cadr stdout))
                             ((string?) (cadr stdout))
                             ;; NB: (type-of '(a b)) == (type-of '(a . b)) == pair?
                             ;; (proper-list? '(a b)) => #t
                             ;; (proper-list? '(a . b)) => #f
                             ((pair?)
                              (if (eq? '% (caadr stdout))
                                  (cadr stdout) ;; (:line (% . foo)) => (% . foo)
                                  (let ((noline (cdr stdout)))
                                        (mibl-trace "noline" noline :test *mibl-debug-action-dsl*)
                                        ;; noline: (:string ...), (% ...)?
                                        (if (list? (car noline))
                                            ;; e.g. (:line (:string (% . foo)))
                                            ;; with-stdout-to can dequote since no lists
                                            (let ((mibl-trace-let "XXXXXXXXXXXXXXXX" noline :test *mibl-debug-action-dsl*)
                                                  (nostr (car noline)))
                                              (mibl-trace "AAAAAAAAAAAAAAAA" nostr :test *mibl-debug-action-dsl*)
                                              (if (eq? :string (car nostr))
                                                  (if (pair? (cadr nostr))
                                                      (if (> (length (cdr nostr)) 1)
                                                          nostr ;; e.g. (:string (% . foo) "/bar")
                                                          (cadr nostr) ;; e.g. (:string (% . foo)) - drop :string
                                                          )
                                                      ;; else maybe (:string "foo/" (% . output))
                                                      ;; retain :string
                                                      nostr)
                                                  ;; (let ((pr (cdr nostr)))
                                                  ;;   (mibl-trace "BBBBBBBBBBBBBBBB" pr :test *mibl-debug-action-dsl*)
                                                  ;;   ;; pr == ((% . foo)) or "foo"
                                                  ;;   (if (eq? :input (car pr))
                                                  ;;       `((% . ,(cdr pr)))
                                                  ;;       pr))
                                                  nostr))
                                            ;; else e.g. (:line "out.txt")
                                            (car noline)))))
                             (else (error 'dbg
                                          (format #f "stdout ~A, ~A" (cadr stdout) (type-of (cadr stdout)))))))

                            ((:input) `(% . ,(cdr stdout)))
                            ((:string)
                             ;; with-stdout-to can dequote since no lists
                             (let ((pr (cadr stdout)))
                               ;; pr == (% . foo)??
                               (if (eq? :input (car pr))
                                   `((% . ,(cdr pr)))
                                   pr)))
                            (else stdout))
                          stdout))
              (mibl-trace-let "stdout b" stdout
                              :test *mibl-debug-action-dsl*)
              (subaction-list (cddr action-list))
              (secondary-cmd (car subaction-list)))
         ;; subaction-list:  ((cat "%{deps}"))
         (mibl-trace "Stdout" stdout :test *mibl-debug-action-dsl*)
         (mibl-trace "subaction-list" subaction-list :test *mibl-debug-action-dsl*)
         ;; recur
         (let* ((subcmd (dsl:action->mibl subaction-list)))
           (mibl-trace "stdout subcmd" subcmd
                       :test *mibl-debug-action-dsl*)
           ;; subcmd may be (:cmd ) or (:cmds )
           (if (eq? :cmds (car subcmd))
               `((:stdout ,stdout) ,subcmd)
               (let ((cmd (case (caar subcmd)
                            ;; deal with subcmds
                            ((progn) subcmd)
                            ((:cmd) subcmd)
                            ((:stdin)
                             ;; move :stdin out of subcmd
                             (cons (car subcmd)
                                   (cdr subcmd)))
                            ;; no subcmds
                            (else
                             ;;`((:cmd ,@subcmd))
                             subcmd
                             ))))
                    (mibl-trace "stdout cmd" cmd :test *mibl-debug-action-dsl*)
                    `((:stdout ,stdout)
                      ,@cmd))))))

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
         `((:cmd (:tool :write-file)
                (:args ,output ,content)))))

      (else (error 'Missing-cmd-directive "Missing cmd directive")))
          ))
