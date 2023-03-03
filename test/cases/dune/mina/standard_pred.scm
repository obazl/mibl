(flags :standard -short-paths)
(flags :standard -short-paths -warn-error -58)
(modules (:standard \ proof_of_stake_fuzzer))
(modules :standard \ camlsnark_linker_flags_gen camlsnark_ctypes_stubs)
(c_library_flags :standard -lgroth16_gpu_prover_stubs -L/usr/local/cuda/lib64 -lcudart -lomp -lstdc++)
(c_library_flags :standard -lstdc++ -lpthread)
(c_library_flags (:standard (:include flags.sexp)))
(:standard \ -pedantic)
(modules (:standard \ gpu no_gpu))
;;(dirs :standard \\ node_modules)"

(modules :standard \ main)
(dirs :standard \ async_kernel digestif)
(flags (:standard -short-paths -cclib -ljemalloc -w @a-4-29-40-41-42-44-45-48-58-59-60))))
