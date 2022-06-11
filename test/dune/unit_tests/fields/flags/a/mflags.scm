(load "dune.scm")

;; flags only
(normalize-stanza-fld-flags '(flags (:standard -nopervasives -nostdlib)))
(normalize-stanza-fld-flags '(flags (-nopervasives :standard -nostdlib)))
(normalize-stanza-fld-flags '(flags (-nopervasives -nostdlib :standard)))
(normalize-stanza-fld-flags '(flags (-nopervasives -nostdlib)))
;; whitespace
(normalize-stanza-fld-flags '(flags (  :standard -nopervasives -nostdlib)))
(normalize-stanza-fld-flags '(flags (   -nopervasives :standard -nostdlib)))
;; no list
(normalize-stanza-fld-flags '(flags :standard -nopervasives -nostdlib))
(normalize-stanza-fld-flags '(flags    :standard -nopervasives -nostdlib))
(normalize-stanza-fld-flags '(flags  -nopervasives :standard -nostdlib))

;; options only
(normalize-stanza-fld-flags '(flags (:standard -foo a -bar b)))
(normalize-stanza-fld-flags '(flags (-foo a -bar b)))
;; string opts
(normalize-stanza-fld-flags '(flags (:standard -foo "a" -bar "b")))
(normalize-stanza-fld-flags '(flags (-foo "a" -bar "b")))
;; int opts only
(normalize-stanza-fld-flags '(flags (:standard -foo 1 -bar 2)))
(normalize-stanza-fld-flags '(flags (-foo 1 -bar 2)))
;; float opts only
(normalize-stanza-fld-flags '(flags (:standard -foo 1.0 -bar 2.3)))
(normalize-stanza-fld-flags '(flags (-foo 1.0 -bar 2.3)))

;; opens only
(normalize-stanza-fld-flags '(flags (:standard -open amod -open bmod)))
(normalize-stanza-fld-flags '(flags (-open amod -open bmod)))

;; flags + options
(normalize-stanza-fld-flags
 '(flags (:standard -foo a -bar b -nopervasives -nostdlib)))

(normalize-stanza-fld-flags
 '(flags (-foo a -bar b -nopervasives -nostdlib)))

;; flags + opens
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -open bmod -nopervasives -nostdlib)))
(normalize-stanza-fld-flags
 '(flags (-open amod -open bmod -nopervasives -nostdlib)))

;; opens + options
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -open bmod -foo a -bar b)))
;; reordered
(normalize-stanza-fld-flags
 '(flags (:standard -foo a -bar b -open amod -open bmod)))
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -foo a -bar b -open bmod)))
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -foo a -open bmod -bar b)))
;; no :standard
(normalize-stanza-fld-flags
 '(flags (-open amod -open bmod -foo a -bar b)))
(normalize-stanza-fld-flags
 '(flags (-open amod -foo a -open bmod -bar b)))

;; opens + flags
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -open bmod -flaga -flagb)))
;; reordered
(normalize-stanza-fld-flags
 '(flags (:standard -flaga -flagb -open amod -open bmod)))
(normalize-stanza-fld-flags
 '(flags (:standard -flaga -open amod -flagb -open bmod)))
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -flaga -flagb -open bmod)))
;; no :standard
(normalize-stanza-fld-flags
 '(flags (-open amod -open bmod -flaga -flagb)))
(normalize-stanza-fld-flags
 '(flags (-open amod -flaga -open bmod -flagb)))

;; opens + options + flags
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -open bmod -foo a -bar b -flaga -flagb)))
(normalize-stanza-fld-flags
 '(flags (:standard -open amod -foo a -open bmod -bar b -flaga -flagb)))
(normalize-stanza-fld-flags
 '(flags (:standard -flaga -open amod -foo a -open bmod -bar b -flagb)))
;; nolist
(normalize-stanza-fld-flags
 '(flags :standard -open amod -open bmod -foo a -bar b -flaga -flagb))
(normalize-stanza-fld-flags
 '(flags :standard -open amod -foo a -open bmod -bar b -flaga -flagb))
(normalize-stanza-fld-flags
 '(flags :standard -flaga -open amod -foo a -open bmod -bar b -flagb))
;; no :standard
(normalize-stanza-fld-flags
 '(flags (-open amod -open bmod -foo a -bar b -flaga -flagb)))
(normalize-stanza-fld-flags
 '(flags (-open amod -foo a -open bmod -bar b -flaga -flagb)))
(normalize-stanza-fld-flags
 '(flags (-flaga -open amod -foo a -open bmod -bar b -flagb)))
;; nolist
(normalize-stanza-fld-flags
 '(flags -open amod -open bmod -foo a -bar b -flaga -flagb))
(normalize-stanza-fld-flags
 '(flags -open amod -foo a -open bmod -bar b -flaga -flagb))
(normalize-stanza-fld-flags
 '(flags -flaga -open amod -foo a -open bmod -bar b -flagb))

;; other stuff
(normalize-stanza-fld-flags
 '(flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67)))

(normalize-stanza-fld-flags
 '(flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67
                    -warn-error +a)))

(normalize-stanza-fld-flags
 '(flags -w "+a-4-6-7-9-29-40..42-44-45-48-60-67"
         -flaga
         -warn-error "+a"
         -flagb))

(normalize-stanza-fld-flags
 '(flags (:standard -open amod -open bmod
                    -nopervasives -nostdlib
                    -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67
                    -warn-error +a)))
(normalize-stanza-fld-flags
 '(flags ( -open amod -open bmod
                    -nopervasives -nostdlib
                    -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67
                    -warn-error +a)))


