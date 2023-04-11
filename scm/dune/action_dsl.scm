(if *mibl-debug-s7-loads*
    (format #t "loading dune/action_dsl.scm\n"))

(set! *mibl-debug-s7* #t)

;; variables (pct-vars) - see https://dune.readthedocs.io/en/stable/concepts.html#variables

;; string args to e.g. echo may contain percent vars.
;; e.g. (echo "let cppo_version = \"%{version:cppo}\"")

;; this routine must parse the pct vars in arg s.
;; if the pct vars can be resolved to strings we return a string;
;; otherwise we return a list of substrings with pct vars intermixed.
;; e.g. (:string "let cppo_version = \"" (% (:version . cppo)) "\"")
;; client can then resolve and join.

(define (string-parse-pct-vars s) ;;FIXME obsolete, use dsl:string->lines
  (mibl-trace-entry "string-parse-pct-vars" s)
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
;; returns (% pfx sfx) or (% str)
(define (parse-pct-var arg)
  (mibl-trace-entry "parse-pct-var" arg)
  (mibl-trace "arg type" (type-of arg))
  (let ((arg-str (format #f "~A" arg)))
    (if (string-prefix? "%{" arg-str)
        (let* ((len (length arg-str))
               (vstr (substring arg-str 2 (- len 1)))
               (sym (string->symbol vstr)))
          (mibl-trace "vstr" vstr)
          (mibl-trace "sym" sym)
          (let ((splits (string-split vstr #\:)))
            (mibl-trace "splits" splits)
            (if (equal? (list vstr) splits)
                (if (string? arg)
                    `(:string (% . ,sym))
                    `(% . ,sym))
                (if (string? arg)
                    `(:string (% ,(symbol (car splits))
                                 ,(string->symbol (string-join (cdr splits) ":"))))
                    `(% ,(symbol (car splits))
                        ,(string->symbol (string-join (cdr splits) ":"))))
                )))
        ;; else not a pct-var
        arg)))

(define (dsl:expand-word arg)
  (mibl-trace-entry "dsl:expand-word" arg)
  (mibl-trace "arg type" (type-of arg))
  (let* ((arg-type (type-of arg))
         (arg-str (format #f "~A" arg)))
    (let recur ((s arg-str)
                (segs '()))
      (mibl-trace "recur on string" s)
      (mibl-trace "len string" (length s))
      (mibl-trace "segs" segs)
      (if (= 0 (length s))
          (case arg-type
            ((string?)
             (mibl-trace "STRING arg" arg)
             (let ((x (map (lambda (seg)
                             (if (pair? seg)
                                 (if (eq? :string (car seg))
                                     (cadr seg)
                                     seg)
                                 seg))
                           segs)))
               (mibl-trace "X" x)
               (if (= 1 (length x)) (car x) `(:string ,@x))))
            ((symbol?)
             (mibl-trace "SYM arg" arg)
             `(:string ,@segs))
            ((integer? number?)
             (mibl-trace "NUM arg" arg)
             (if (< (length segs) 2)
                 (car segs) segs))
            (else
             (error 'Bad-pctvar-arg
                    (format #f "Bad pctvar arg type: ~A, ~A"
                            arg (type-of arg)))))

          (let ((slen (length s))
                (res (regexec rgx s match-ct 0)))
            (mibl-trace "slen" slen)
            (mibl-trace "rgx res" res)
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
                        (mibl-trace "match" match)
                        (mibl-trace "expanded" pct)
                        (mibl-trace "NO PFX/SFX" pct)
                        ;; dsl-str type is string?, so parse-pct-var
                        ;; will return (:string (% ...)). remove :string.
                        ;; nothing left to recur on so return directly
                        (if (truthy? segs)
                            (append segs (cadr pct))
                            (cadr pct)))
                      ;; else we have a pfx and/or sfx, so we need to recur
                      (let* ((pfx  (substring s 0 rm-so))
                             (sfx  (substring s rm-eo))
                             (match (substring s match-so match-eo))
                             (pct (parse-pct-var match)))
                        (mibl-trace "pfx" pfx)
                        (mibl-trace "match" match)
                        (mibl-trace "pct" pct)
                        (mibl-trace "sfx" sfx)
                        ;; (error 'b "b")
                        (recur sfx (append segs (if (> match-so 0)
                                                    (list pfx pct)
                                                    pct))))))
                ;; else no match: exit directly?
                (recur "" (append segs (list s)))))))))

(define (pct-var->keyword v)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (blue "pct-var->var") v))
  (let* ((v (format #f "~A" v))
         (len (length v))
         ;; remove %{ and }
         (vstr (substring v 2 (- len 1)))
         (kw (string->keyword vstr)))
    (if *mibl-debug-s7*
        (begin
          (format #t "vstr ~A\n" vstr)
          (format #t "kw ~A\n" kw)))
    kw))

;; examples:
;;  char *s = "\
;; \ndiff -u  --label test.output --label test-partitions.output test.output test-partitions.output > diff-with-without-partitions || true\
;; \n" ;

;; (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
;; =>  ((% ocaml-config c_compiler) -c -I (% ocaml-config standard_library) -o (% . targets) (% . deps))


;; input: string w/o newlines
;; output: list of words
(define (dsl:line->words dsl-line)
  (mibl-trace-entry "dsl:line->words" dsl-line)
  (mibl-trace "dsl-line type" (type-of dsl-line))
  (let* ((slen (length dsl-line))
         (str (string-trim '(#\space #\tab) dsl-line))
         (words (string-split str #\space)))
    (mibl-trace "words" words)
    ;; (let ((x (map parse-pct-var words)))
    (let ((x (map dsl:expand-word words)))
      (mibl-trace "expanded words" x)
      ;;(error 'words "words")
      x)))

;; ))
;;      (else
;;       (error 'Bad-dsl-str-arg
;;              (format #f "bad dsl-str arg: ~A, ~A" dsl-str (type-of dsl-str))))))

;; input: string
;; output: list; input split on newlines, pctvars expanded
;;(define (destructure-dsl-string dsl-str)
(define (dsl:string->lines dsl-str)
  (mibl-trace-entry "dsl:string->lines" dsl-str)
  (mibl-trace "dsl-str type" (type-of dsl-str))
  (case (type-of dsl-str)
    ((integer? number?) dsl-str)
    ((symbol?) (parse-pct-var dsl-str))
    ((string?)
     (let* ((lines (string-split dsl-str #\newline))
            (expanded-words (map dsl:line->words lines)))
       (mibl-trace "lines" lines)
       (mibl-trace "expanded words" expanded-words)
       (car expanded-words)))))

;; directives take string args
(define (destructure-directive-arg dsl-arg)
  (mibl-trace-entry "destructure-directive-arg" dsl-arg)
  (mibl-trace "dsl-arg type" (type-of dsl-arg))
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
;; FUNCTION: (destructure-rule-action action) -> list
;; destructures but does not expand; e.g. %{targets} -> (% . targets) w/o resolving
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
(define (destructure-rule-action action)
  (mibl-trace-entry "destructure-rule-action" action)
  (flush-output-port)
  (let* ((action-list (car action))
         (mibl-trace-let "action-list" action-list)
         (primary-cmd (if (pair? (car action-list))
                          (caar action-list)
                          ;; else (action tool ...) - illegal but may happen?
                          (car action-list)))
         (mibl-trace-let "primary cmd" primary-cmd))
    (if (member primary-cmd `(cat cmp copy copy# diff diff? echo)) ;; etc.
                                  ;; bash system run dynamic-run))
        (begin
          (mibl-trace "direct cmd" primary-cmd)
          (let* ((-args (cdr action-list))
                 (args (map dsl:string->lines -args)))
            (mibl-trace "args" args)
            `((:shell . sh) (:tool . ,primary-cmd) (:args ,@args))))
        (begin
          (mibl-trace "cmd directive" primary-cmd)
          (case primary-cmd
            ((bash)
             (destructure-bash-cmd action-list)
             )

            ((run)
             (if (eq? 'bash (car (cdr action-list)))
                 (destructure-bash-cmd (cdr action-list))
                 (let* ((-args (cdr action-list))
                        (args (map dsl:string->lines -args))
                        (secondary-cmd (list (car args)))
                        (secondary-args (cdr args)))
                   (mibl-trace "run args" -args)
                   (mibl-trace "destructured run args" args)
                   (mibl-trace "secondary cmd" secondary-cmd)
                   (mibl-trace "secondary args" secondary-args)
                   ;; (error 'x "X")
                   `((:tool . ,secondary-cmd) (:args ,@secondary-args)))))

            ((system)
             )
            ((chdir) ;; (chdir <dir> <DSL>)
             ;;,normalize-action-chdir-dsl
             )
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
            ((progn)
             ;; ,normalize-action-progn-dsl
             )
            ((run)
             ;; ,normalize-action-run-dsl
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
                              (mibl-trace "arg" arg)
                              (mibl-trace "arg t" (type-of arg))
                              (destructure-directive-arg arg)))
                    (mibl-trace-let "STDERR" stderr)
                    (subaction-list (cddr action-list)))
               (mibl-trace "Stderr" stderr)
               (mibl-trace "subaction-list" subaction-list)
               ;; recur
               (let ((subber (destructure-rule-action subaction-list)))
                 (mibl-trace "subber" subber)
                 ;; always make stdout fname a sym
                 `((:stderr ,stderr)
                   (:cmd ,@subber)))))

            ((with-stdout-to) ;; (with-stdout-to "%{targets}" (cat "%{deps}"))
             (let* ((stdout (let ((fd (cadr action-list)))
                              ;; fd: %{targets}
                              (mibl-trace "fd" fd)
                              (mibl-trace "fd t" (type-of fd))
                              (destructure-directive-arg fd))
                            ;; (if (eq? '%{target} fd)
                            ;;     :output
                            ;;     `(:fixme ,fd))
                            )
                    (mibl-trace-let "STDOUT" stdout)
                    ;; (stdout (case (type-of stdout)
                    ;;           ((symbol?) stdout)
                    ;;           ((string?) stdout)
                    ;;           ((pair? list?)
                    ;;            (if (eq? :string (car stdout))
                    ;;                (cadr stdout)
                    ;;                stdout))
                    ;;           (else
                    ;;            (error 'Bad-stdout
                    ;;                        (format #f "bad stdout: ~A (~A)\n" stdout (type-of stdout))))))
                    (subaction-list (cddr action-list)))
               ;; subaction-list:  ((cat "%{deps}"))
               (mibl-trace "Stdout" stdout)
               (mibl-trace "subaction-list" subaction-list)
               ;; recur
               (let ((subber (destructure-rule-action subaction-list)))
                 (mibl-trace "subber" subber)
                 ;; always make stdout fname a sym
                 `((:stdout ,stdout)
                   (:cmd ,@subber)))))
            ((with-stdin-from);; (with-stdin-from <file> <DSL>)
             ;; ,normalize-action-with-stdin-from-dsl
             )
            (else
             (error 'Missing-cmd-directive "Missing cmd directive")))
          ))))
