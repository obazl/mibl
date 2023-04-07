## run this to generate PARSTREE and PROJECT files for all test/dune cases.

BAZELRC=--bazelrc .config/runmibl.bazelrc
QUIET=
#--quiet

all: execs sh

DEPS_TESTS= \
test/dune/deps/dynamic/case01 \
test/dune/deps/glob/cwd/tagged/case01 \
test/dune/deps/glob/cwd/untagged/case01 \
test/dune/deps/glob/sibling/case01 \
test/dune/deps/glob/sub/case01 \
test/dune/deps/glob/sub/untagged/case01 \
test/dune/deps/glob/super \
test/dune/deps/inferred/case01 \
test/dune/deps/literals/cwd/tagged/case01 \
test/dune/deps/literals/cwd/tagged/case02 \
test/dune/deps/literals/cwd/tagged/case03 \
test/dune/deps/literals/cwd/untagged/case01 \
test/dune/deps/literals/cwd/untagged/case02 \
test/dune/deps/literals/cwd/untagged/case03 \
test/dune/deps/literals/sibling/case01 \
test/dune/deps/literals/sub/case01 \
test/dune/deps/literals/super/case01 \
test/dune/deps/tagged/cwd/case01 \
test/dune/deps/tagged/cwd/case02 \
test/dune/deps/tagged/subtool/case01 \
test/dune/deps/tagged/supertool/case01

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
test/dune/rules/shell/redirects/case01 \
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
	for tgt in $(STDIO_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

################################################################
RULE_TESTS=\
test/dune/rules/inferred/io/case01 \
test/dune/rules/inferred/outputs/case01 \
test/dune/rules/inferred/outputs/case02 \

## test/dune/rules/complex/case01 \

rules:
	@cd .. && \
	for tgt in $(RULE_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

