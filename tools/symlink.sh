#!/bin/sh

## For local development. Symlinks the scheme files for mibl and libs7
## to XDG_HOME_DATA so that they can be edited within their repos.
## Copies the executable (repl -> mibl) and libc_s7.so instead of
## symlinking, since they are located after build in a Bazel-controled
## directory. Assumes that `https://github.com/obazl/libs7` is cloned
## to the local system.

## Usage: $ bazel build //repl && .tools/symlink.sh

mkdir -vp $HOME/.local/share/mibl
mkdir -vp $HOME/.local/share/mibl/dune
mkdir -vp $HOME/.local/share/mibl/meta
mkdir -vp $HOME/.local/share/mibl/opam
mkdir -vp $HOME/.local/share/mibl/s7

echo "Linking scripts"
## PWD assumed to be $HOME/obazl/mibl
ln -sfv $PWD/mibl/*scm $HOME/.local/share/mibl
ln -sfv $PWD/mibl/dune/*scm $HOME/.local/share/mibl/dune
ln -sfv $PWD/mibl/meta/*scm $HOME/.local/share/mibl/meta
ln -sfv $PWD/mibl/opam/*scm $HOME/.local/share/mibl/opam

ln -sfv $HOME/obazl/libs7/libs7/*scm $HOME/.local/share/mibl
ln -sfv $HOME/obazl/libs7/libs7/s7/*scm $HOME/.local/share/mibl/s7

# ln -sfv $HOME/obazl/mibl/mibl/*scm $HOME/.local/share/mibl
# ln -sfv $HOME/obazl/mibl/mibl/s7/*scm $HOME/.local/share/mibl/s7

## copy the executable and lib (do not symlink from Bazel directories)

# BAZEL_BIN=bazel-
BAZEL_BIN=".bazel/"

# DSO_EXT=".so"
DSO_EXT=".dylib"

cp -fv `realpath ${BAZEL_BIN}bin/repl/repl` $HOME/.local/bin/mibl
chmod u+rwx $HOME/.local/bin/mibl

cp -fv `realpath ${BAZEL_BIN}bin/external/libs7/src/libc_s7${DSO_EXT}` $HOME/.local/share/mibl
chmod u+rwx $HOME/.local/share/mibl/libc_s7${DSO_EXT}
