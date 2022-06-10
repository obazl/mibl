#!/bin/sh

mkdir -vp $HOME/.local/share/mibl
mkdir -vp $HOME/.local/share/mibl/dune
mkdir -vp $HOME/.local/share/mibl/meta
mkdir -vp $HOME/.local/share/mibl/opam
mkdir -vp $HOME/.local/share/mibl/s7

echo "Linking scripts"
ln -sfv $PWD/mibl/*scm $HOME/.local/share/mibl
ln -sfv $PWD/mibl/dune/*scm $HOME/.local/share/mibl/dune
ln -sfv $PWD/mibl/meta/*scm $HOME/.local/share/mibl/meta
ln -sfv $PWD/mibl/opam/*scm $HOME/.local/share/mibl/opam

ln -sfv $HOME/obazl/mibl/mibl/*scm $HOME/.local/share/mibl
ln -sfv $HOME/obazl/mibl/mibl/s7/*scm $HOME/.local/share/mibl/s7

## don't forget the executble and lib
cp -fv `realpath bazel-bin/repl/repl` $HOME/.local/bin/mibl
chmod u+rwx $HOME/.local/bin/mibl

cp -fv `realpath bazel-bin/external/libs7/src/libc_s7.so` $HOME/.local/share/mibl
