;; from tezos

(:standard)

(flags (:standard -open Tezos_stdlib)))
(:standard -open Tezos_base__TzPervasives)))
(flags (:standard -open Tezos_base__TzPervasives)))
(library_flags (:standard -linkall))
(library
 (name foo)
 (public_name foo)
 (libraries a b c)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib
                   -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67
                   -warn-error +a
                   -open Pervasives
                   -open Error_monad))
 (modules Misc Storage_description))

(libraries foo
           (library_flags (:standard -linkall))
           (flags (:standard -open Tezos_base__TzPervasives
                   -open Tezos_shell_services
                   -open Tezos_client_base
                   -open Tezos_protocol_genesis_carthagenet
                   -open Tezos_client_commands)))

(library_flags (:standard -linkall))
(flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48-60-67))
(modules (:standard \ alpha_commands_registration))
(modules (:standard))
(modules (:standard \ legacy_store_builder))
(:standard)
(flags :standard)
(modules (:standard) \ Plugin_registerer)
(ocamlopt_flags (:standard -p -ccopt -no-pie))
(modules (:standard bip39_english))
(modules :standard \ gen)
