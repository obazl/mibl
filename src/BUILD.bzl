CMD_FLAGS = [
    "-std=c11",
    "-pedantic-errors",
] + select({
    "//bzl/host:linux": [
        "-D_POSIX_C_SOURCE=200809L", ## strdup, strndup
        "-D_DEFAULT_SOURCE"],        ## DT_ constants from dirent.h
    "//conditions:default":   []
})

