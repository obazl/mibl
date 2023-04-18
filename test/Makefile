## run this to generate PARSTREE and PROJECT files for all test/dune cases.

BAZELRC=--bazelrc .config/runmibl.bazelrc
QUIET=

##--quiet

default:
	echo "Targets: all, deps, execs, dsl_base, dsl_inductive, rules"

.PHONY: case
case:
	@cd .. && \
	bazel $(BAZELRC) run //emit:mibl -- -w $(w) --emit-parsetree $(QUIET) ;

 # --no-miblrc $(QUIET) ;

.PHONY: all
all: deps execs dsl_base dsl_inductive rules

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
test/dune/executable/flags/compile/common \
test/dune/executable/main_dyad/mwe1 \
test/dune/executable/main_dyad/mwe2

# test/dune/executable/flags/compile/ocamlc \
# test/dune/executable/flags/compile/ocamlopt \
# test/dune/executable/flags/link/case01 \

.PHONY: execs
execs:
	@cd .. && \
	for tgt in $(EXEC_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
	done

################################################################
DSL_BASE=\
test/dune/action_dsl/base_cases/bash/case01 \
test/dune/action_dsl/base_cases/cat/case01 \
test/dune/action_dsl/base_cases/cmp/case01 \
test/dune/action_dsl/base_cases/cmp/case02 \
test/dune/action_dsl/base_cases/copy/case01 \
test/dune/action_dsl/base_cases/diff/case01 \
test/dune/action_dsl/base_cases/echo/case01 \
test/dune/action_dsl/base_cases/system/case01

dsl_base:
	@cd .. && \
	for tgt in $(DSL_BASE) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

################################################################
DSL_INDUCTIVE=\
test/dune/action_dsl/inductive/chdir/case01 \
test/dune/action_dsl/inductive/redirects/case01 \
test/dune/action_dsl/inductive/run/case01 \
test/dune/action_dsl/inductive/stderr/with/bash \
test/dune/action_dsl/inductive/stderr/with/echo \
test/dune/action_dsl/inductive/stdin/with/bash \
test/dune/action_dsl/inductive/stdin/with/echo \
test/dune/action_dsl/inductive/stdout/with/bash \
test/dune/action_dsl/inductive/stdout/with/echo \
test/dune/action_dsl/inductive/stdx/with/case01

# test/dune/action_dsl/inductive/stderr/with/cat \
# test/dune/action_dsl/inductive/stdout/with/cat \
# test/dune/action_dsl/inductive/stdx/with/case01 \

# test/dune/action_dsl/inductive/stdin/with/case01 \
# test/dune/action_dsl/inductive/stderr/with/case01 \
# test/dune/action_dsl/inductive/stdio/ignore/case01 \
# test/dune/action_dsl/inductive/stdio/pipe/case01 \
# test/dune/action_dsl/inductive/stdout/ignore/case01 \
# test/dune/action_dsl/inductive/stdout/pipe/case01 \

dsl_inductive:
	@cd .. && \
	for tgt in $(DSL_INDUCTIVE) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done


################################################################
RULE_TESTS=\
test/dune/rules/inferred/io/case01 \
test/dune/rules/inferred/outputs/case01 \
test/dune/rules/inferred/outputs/case02 \

## test/dune/rules/complex/case01 \

.PHONY: rules
rules:
	@cd .. && \
	for tgt in $(RULE_TESTS) ; do \
		bazel $(BAZELRC) run //emit:mibl -- -w $$tgt --emit-parsetree --no-miblrc $(QUIET) ; \
		echo ; \
	done

