## run this to generate PARSTREE and PROJECT files for all test/dune cases.

BAZELRC=--bazelrc .config/runmibl.bazelrc
QUIET=
#--quiet

all: execs sh

DEPS_TESTS= \
test/dune/deps/tagged

deps:
	@cd .. && \
	for tgt in $(DEPS_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
	done

################################################################
EXEC_TESTS= \
test/dune/executable/flags \
test/dune/executable/main_dyad/mwe1 \
test/dune/executable/main_dyad/mwe2

execs:
	@cd .. && \
	for tgt in $(EXEC_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
	done

################################################################
SHELL_TESTS=\
test/dune/rules/shell/bash/simple \
test/dune/rules/shell/cat/case01 \
test/dune/rules/shell/chdir/case01 \
test/dune/rules/shell/cmp/case01 \
test/dune/rules/shell/cmp/case02 \
test/dune/rules/shell/copy/case01 \
test/dune/rules/shell/diff/case01 \
test/dune/rules/shell/echo/case01 \
test/dune/rules/shell/system/case01

sh:
	@cd .. && \
	for tgt in $(SHELL_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

################################################################
STDIO_TESTS=\
test/dune/rules/stdio/with \
test/dune/rules/stdout/ignore \
test/dune/rules/stdout/pipe \
test/dune/rules/stdout/with/bash \
test/dune/rules/stdout/with/cat \
test/dune/rules/stdout/with/echo

stdio:
	@cd .. && \
	for tgt in $(STDOUT_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

