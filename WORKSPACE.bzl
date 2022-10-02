load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

def cc_fetch_repos():

    maybe(
        git_repository,
        name = "libs7",
        remote = "https://github.com/obazl/libs7",
        branch = "main"
        # commit = "bb528f3edac6c00953010e28d51e4a52da7555aa",
        # shallow_since = "1618495335 -0500"
    )

    maybe(
        git_repository,
        name = "rules_cc",
        remote = "https://github.com/bazelbuild/rules_cc",
        commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
        shallow_since = "1605101351 -0800"
        # branch = "master"
    )

    ## logc vendored
#     maybe(
#         http_archive,
#         name = "logc",
#         url  = "https://github.com/rxi/log.c/archive/refs/heads/master.zip",
#         sha256 = "4839147fb0674bcfb4b3ede3d1db055546d12315d2f7592862293dfd1c065f83",
#         strip_prefix = "log.c-master",
#         build_file_content = """
# cc_library(
#     name  = 'logc',
#     linkstatic = 1,
#     alwayslink = 1,
#     srcs  = ['src/log.c'],
#     hdrs = ['src/log.h'],
#     copts = [
#         '-std=c11',
#         # '-g',
#     ],
#     local_defines = ['LOG_USE_COLOR'],
#     visibility = ['//visibility:public']
# )
# """,
#         # sha256 = "33a5690733c5cc2ede39cb62ebf89e751f2448e27f20c8b2fbbc7d136b166804",
#     )

    ######
    maybe(
        http_archive,
        name = "libre2c",
        urls = [
            "https://github.com/skvadrik/re2c/archive/refs/tags/2.1.1.zip"
        ],
        strip_prefix = "re2c-2.1.1",
        sha256 = "080931d214943ea021fa9360a4694e824674e5c0f2e880153e8cb41982453aa6",
        build_file = "//external/re2c:BUILD.bazel"
        # build_file_content = all_content,
        # workspace_file_content = "workspace( name = \"opam-re2c\" )"
    )

    ######
    maybe(
        git_repository,
        name = "makeheaders",
        remote = "https://github.com/obazl/makeheaders",
        # branch = "main"
        commit = "bb528f3edac6c00953010e28d51e4a52da7555aa",
        shallow_since = "1618495335 -0500"
        # http_archive,
        # name = "makeheaders",
        # urls = [
        #     "https://github.com/obazl/makeheaders/archive/57bae6cc7e88783b060acf711bc21f99d8380ca5.tar.gz"
        # ],
        # strip_prefix = "makeheaders-57bae6cc7e88783b060acf711bc21f99d8380ca5",
        # sha256 = "83ef79d69c02348efd1f52a85fa10e9bd57333e89a23c2fe66a9b298c055d164"
    )

    ######
    maybe(
        http_archive,
        name = "uthash",
        build_file_content = """
filegroup(name = "include", srcs = glob(["include/*.h"]), visibility = ["//visibility:public"])
    """,
        urls = [
            "https://github.com/troydhanson/uthash/archive/refs/tags/v2.3.0.tar.gz"
        ],
        strip_prefix = "uthash-2.3.0",
        sha256 = "e10382ab75518bad8319eb922ad04f907cb20cccb451a3aa980c9d005e661acc"
    )

    ######
    maybe(
        http_archive,
        name = "libinih",
        # build_file_content = "exports_files(['ini.c', 'ini.h'])",
    build_file_content = """
filegroup(name = "srcs", srcs = ["ini.c", "ini.h"], visibility = ["//visibility:public"])
filegroup(name = "hdrs", srcs = ["ini.h"], visibility = ["//visibility:public"])""",
        urls = [
            "https://github.com/benhoyt/inih/archive/cb55f57d87ae840bd0f65dbe6bd22fa021a873a7.tar.gz"
        ],
        strip_prefix = "inih-cb55f57d87ae840bd0f65dbe6bd22fa021a873a7",
        sha256 = "26d05999033eef9e3abca2d4dbf3dc2e4a24335df51231b6faa093be06bb19d7"
    )

    # Unit testing framework for C
    # http://www.throwtheswitch.org/unity
    maybe(
        http_archive,
        name = "unity",
        urls = [
            "https://github.com/ThrowTheSwitch/Unity/archive/refs/tags/v2.5.2.zip",
        ],
        strip_prefix = "v2.5.2",
        build_file_content = all_content,
        workspace_file_content = "workspace( name = \"unity\" )"
    )

