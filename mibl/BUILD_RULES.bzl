## original src: https://github.com/bazelbuild/bazel-skylib/blob/main/rules/native_binary.bzl
# Copyright 2019 The Bazel Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""native_binary() and native_test() rule implementations.

These rules let you wrap a pre-built binary or script in a conventional binary
and test rule respectively. They fulfill the same goal as sh_binary and sh_test
do, but they run the wrapped binary directly, instead of through Bash, so they
don't depend on Bash and work with --shell_executable="".
"""

def _run_mibl_impl_rule(ctx):
    print("ARGS: %s", ctx.attr.args)
    out = ctx.actions.declare_file(ctx.attr.out)
    ctx.actions.symlink(
        target_file = ctx.executable.src,
        output = out,
        is_executable = True,
    )
    runfiles = ctx.runfiles(files = ctx.files.data)
    print("RUNFILES: %s" % runfiles)
    # Bazel 4.x LTS does not support `merge_all`.
    # TODO: remove `merge` branch once we drop support for Bazel 4.x.
    if hasattr(runfiles, "merge_all"):
        print("XXXXXXXXXXXXXXXX****************")
        runfiles = runfiles.merge_all([
            d[DefaultInfo].default_runfiles
            for d in ctx.attr.data + [ctx.attr.src]
        ])
    else:
        print("ZZZZZZZZZZZZZZZZ****************")
        for d in ctx.attr.data:
            runfiles = runfiles.merge(d[DefaultInfo].default_runfiles)
        runfiles = runfiles.merge(ctx.attr.src[DefaultInfo].default_runfiles)

    return DefaultInfo(
        executable = out,
        files = depset([out]),
        runfiles = runfiles,
    )

_ATTRS = {
    "src": attr.label(
        executable = True,
        # This must be used instead of `allow_single_file` because otherwise a
        # target with multiple default outputs (e.g. py_binary) would not be
        # allowed.
        allow_files = True,
        mandatory = True,
        cfg = "target",
        doc = "path of the pre-built executable",
    ),
    "data": attr.label_list(
        allow_files = True,
        doc = "data dependencies. See" +
              " https://bazel.build/reference/be/common-definitions#typical.data",
    ),
    # "out" is attr.string instead of attr.output, so that it is select()'able.
    "out": attr.string(mandatory = True, doc = "An output name for the copy of the binary"),
}

run_mibl = rule(
    implementation = _run_mibl_impl_rule,
    attrs = _ATTRS,
    executable = True,
    doc = """
Wraps a pre-built binary or script with a binary rule.

You can "bazel run" this rule like any other binary rule, and use it as a tool
in genrule.tools for example. You can also augment the binary with runfiles.
""",
)
