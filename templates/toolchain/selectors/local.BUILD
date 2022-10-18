# generated file - DO NOT EDIT

load("@rules_ocaml//toolchain:adapter.bzl", "toolchain_selector")

exports_files(glob(["*.bazel"]))

##########
toolchain_selector(
    name           = "default", # sys>sys
    toolchain      = "@ocaml//toolchain/adapters/local:syssys",
    visibility     = ["//visibility:public"],
)

##########
toolchain_selector(
    name           = "syssys",
    toolchain      = "@ocaml//toolchain/adapters/local:syssys",
    build_host_constraints  = ["@ocaml//platforms:sys?"],
    target_host_constraints  = ["@ocaml//platforms:sys?"],
    visibility     = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "sysvm",
    toolchain               = "@ocaml//toolchain/adapters/local:sysvm",
    build_host_constraints  = ["@ocaml//platforms:sys?"],
    target_host_constraints  = ["@ocaml//platforms:vm?"],
    visibility              = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "vmsys",
    toolchain               = "@ocaml//toolchain/adapters/local:vmsys",
    build_host_constraints    = ["@ocaml//platforms:vm?"],
    target_host_constraints  = ["@ocaml//platforms:sys?"],
    visibility              = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "vmvm",
    toolchain               = "@ocaml//toolchain/adapters/local:vmvm",
    build_host_constraints  = ["@ocaml//platforms:vm?"],
    target_host_constraints  = ["@ocaml//platforms:vm?"],
    visibility              = ["//visibility:public"],
)
