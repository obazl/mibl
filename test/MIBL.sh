#!/bin/sh

TGTS=" \
test/dune/deps/tagged \
test/dune/executable/main_dyad/mwe1 \
test/dune/executable/main_dyad/mwe2 \
test/dune/rules/shell/cmp/case01 \
test/dune/rules/shell/cmp/case02 \
"

for TGT in ${TGTS}
do
    bazel --bazelrc .config/runmibl.bazelrc run emit:mibl -- -w ${TGT} --emit-parsetree --no-miblrc
    echo
done
