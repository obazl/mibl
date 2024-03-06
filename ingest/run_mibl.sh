#!/bin/bash

# Bazel aliases cannot pass args. We use this script to allow
# different targets to run mibl without recompiling it for each.

# set -x

# echo "ARG 0 $0"
# echo "ARG rest: $@"

# echo "RUNFILES_DIR: $RUNFILES_DIR"
# echo "RUNFILES_MANIFEST_FILE: $RUNFILES_MANIFEST_FILE"
# echo "runfiles: $0.runfiles"

# printenv

# --- begin runfiles.bash initialization v3 ---
# Copy-pasted from the Bazel Bash runfiles library v3.
set -uo pipefail; set +e; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
    source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
    source "$0.runfiles/$f" 2>/dev/null || \
    source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
    source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
    { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v3 ---

# echo "ARGS: $@"

# echo "PWD: $PWD"

# echo "X: $(rlocation mibl/mibl/mibl)"

# set -x

"$(rlocation mibl/mibl/mibl)" "$@"


