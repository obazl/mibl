#!/bin/sh

mkdir -vp $HOME/.local/share/libs7
mkdir -vp $HOME/.local/share/libs7/dune
mkdir -vp $HOME/.local/share/libs7/meta
mkdir -vp $HOME/.local/share/libs7/opam
mkdir -vp $HOME/.local/share/libs7/s7

ln -sfv $PWD/libs7/*scm $HOME/.local/share/libs7
ln -sfv $PWD/libs7/dune/*scm $HOME/.local/share/libs7/dune
ln -sfv $PWD/libs7/meta/*scm $HOME/.local/share/libs7/meta
ln -sfv $PWD/libs7/opam/*scm $HOME/.local/share/libs7/opam

ln -sfv $HOME/obazl/libs7/libs7/*scm $HOME/.local/share/libs7
ln -sfv $HOME/obazl/libs7/libs7/s7/*scm $HOME/.local/share/libs7/s7

## don't forget the executble and lib
cp -fv `realpath bazel-bin/repl/repl` $HOME/.local/bin/mibl
chmod u+rwx $HOME/.local/bin/mibl
cp -fv `realpath bazel-bin/external/libs7/src/libc_s7.so` $HOME/.local/share/libs7
