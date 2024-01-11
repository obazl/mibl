# load("//:BUILD.bzl", "LIBLOGC_VERSION")

BASE_SRCS = []

BASE_DEPS = ["@liblogc//lib:logc"]

BASE_INCLUDE_PATHS = [
    "-Iconfig",
    # "-Iexternal/mibl/config",
    # "-Iexternal/liblogc~{}/src".format(LIBLOGC_VERSION)
]

BASE_COPTS = [
    "-x", "c",
    "-Wall",
    "-Wextra",
    # GCC:
    "-Werror", # turn all warnings into errors
    "-Wfatal-errors", # stop on first error
] + select({
    "//config/host/build:macos?": [
        "-std=c11",
        "-Werror=pedantic", # not needed with -Werror?
        "-Wpedantic", # same as -pedantic, strict ISO C and ISO C++ warnings
        "-pedantic-errors",
        "-Wno-gnu-statement-expression",
        # "-Werror=pedantic",
        # "-Wno-gnu",
        # "-Wno-format-pedantic",
    ],
    "//config/host/build:linux?": [
        "-std=gnu11",
        "-fPIC",
        # "-Wl,--no-undefined",
    ],
    "//conditions:default": ["-std=c11"],
})

BASE_LINKOPTS = select({
    "//config/host/build:linux?": ["-rdynamic", "-ldl"],
    "//config/host/build:macos?": [], ## "-ldl"],
    "//conditions:default": []
})

BASE_DEFINES = select({
    "//config/host/build:macos?": ["DSO_EXT=\\\".dylib\\\""],
    "//config/host/build:linux?": [
        "DSO_EXT=\\\".so\\\"",
        # "_XOPEN_SOURCE=500", # strdup
        "_POSIX_C_SOURCE=200809L", # strdup, strndup since glibc 2.10
        "_DEFAULT_SOURCE",    # dirent macros
        "_GNU_SOURCE"         # dlsym RTLD macros
    ],
    "//conditions:default":   ["DSO_EXT=\\\".so\\\""]
}) + select({
        "//config/s7:debug?": ["S7_DEVBUILD"],
        "//conditions:default":   []
}) + select({
    "//config/profile:dev?": ["DEVBUILD"],
    "//conditions:default": []
}) + select({
    "//config/debug:trace?": ["TRACING"],
    "//conditions:default": []
## FIXME: libs7 config not global
# }) + select({
#     "@mustachios_s7//syntax:alt?": ["ALT_SYNTAX"],
#     "//conditions:default": []
# }) + select({
#     # "@libs7//config/clibs/link:runtime?": ["CLIBS_LINK_RUNTIME"],
#     "//conditions:default":   []
})

