## GENERATED FILE ##

load("@rules_ocaml//build:rules.bzl",
     "ocaml_binary",
     "ocaml_exec_module",
     "ocaml_library",
     "ocaml_module",
     "ocaml_ns_library",
     "ocaml_ns_resolver",
     "ocaml_signature")

##################
# namespaces all exec modules
ocaml_ns_resolver(
    name     = "Exe_ns",
    ns       = "Exe_ns",
    manifest = ["A", "B", "Main1", "Main2"]
)

##############
ocaml_binary(
    name     = "main1.exe",
    prologue = [":libPrologue_main1"],
    main     = ":Main1",
    visibility = ["//visibility:public"],
)

ocaml_exec_module(
    name          = "Main1",
    ns_resolver   = ":Exe_ns",
    struct        = "main1.ml",
    sig           = ":Main1_cmi",
    deps          = [":A"],
)

ocaml_signature(
    name          = "Main1_cmi",
    src           = "main1.mli",
)

ocaml_library(
    name     = "libPrologue_main1",
    manifest = [":B"]
)

##############
ocaml_binary(
    name     = "main2.exe",
    prologue = [":libPrologue_main2"],
    main     = ":Main2",
    visibility = ["//visibility:public"],
)

ocaml_exec_module(
    name          = "Main2",
    ns_resolver   = ":Exe_ns",
    struct        = "main2.ml",
    sig           = ":Main2_cmi",
)

ocaml_signature(
    name          = "Main2_cmi",
    src           = "main2.mli",
)

ocaml_library(
    name     = "libPrologue_main2",
    manifest = [":A", ":B"]
)

######################## Modules & Signatures ########################
ocaml_module(
    ns_resolver   = ":Exe_ns",
    name          = "A",
    struct        = "a.ml",
)

ocaml_module(
    ns_resolver   = ":Exe_ns",
    name          = "B",
    struct        = "b.ml",
)

