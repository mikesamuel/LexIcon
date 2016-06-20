ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CAML:=$(realpath ${ROOT_DIR}/src/main/caml)
TARGET:=$(realpath ${ROOT_DIR}/target)
TEST_FILES:=$(realpath ${ROOT_DIR}/src/test/resources)

help:
	@echo "executables - build all the executables"
	@echo "runtests    - runs the tests"
	@echo "ocamldoc    - builds documentation"
	@echo "benchmark   - runs the benchmarks"
	@echo "clean       - gets rid of generated files"
	@echo "most        - builds all executables and docs, and runs tests"
	@echo "all         - most + benchmark from clean"

most: executables runtests ocamldoc

all: clean most benchmark

executables: ${CAML}/AllTests.native ${CAML}/RunBenchmarks.native ${CAML}/DeadWood.native ${CAML}/Compile.native
	cd ${CAML} && ocaml make.ml

${CAML}/%.native: ${CAML}/*.ml ${CAML}/*.mli
	cd ${CAML} && ocaml make.ml $(notdir $@)

runtests: ${TARGET}/runtests.make.timestamp ${TARGET}/test-
${TARGET}/runtests.make.timestamp: ${CAML}/AllTests.native
	mkdir -p ${TARGET}/test-outputs
# Always test AllTests tests all tests
	cd ${CAML} && ls *Test.ml | perl -pe 's/\.ml//' | sort > "$$TMPDIR"/test_files
	cd ${ROOT_DIR}
	cat ${CAML}/AllTests.ml | perl -ne 'print if s/^module \w+ = //' \
	  > "$$TMPDIR"/tested_modules
	diff -uw "$$TMPDIR"/test_files "$$TMPDIR"/tested_modules
	${CAML}/run_test AllTests.native
	touch ${TARGET}/runtests.make.timestamp
# Dump files under test-files that were not accessed.
	@find ${TEST_FILES} -type f -atime +5m | egrep -v \
	  '~$$|/benchmarks/|/\.gitignore$$|visualize_output_buffer\.html$$|\.dot\.svg$$|/cksums$$' \
	  > "$$TMPDIR"/unused-test-files.txt
	@if [ -s "$$TMPDIR"/unused-test-files.txt ]; then \
	  echo "Unused test-files"; \
	  echo "================="; \
	  cat "$$TMPDIR"/unused-test-files.txt; \
	fi

ocamldoc: ${TARGET}/ocamldoc.make.timestamp
${TARGET}/ocamldoc.make.timestamp: ${CAML}/*.ml ${CAML}/*.mli executables
	./run_ocamldoc.sh && touch ocamldoc.make.timestamp

benchmark: ${CAML}/RunBenchmarks.native
	${CAML}/RunBenchmarks.native

clean:
	cd ${CAML} && ocamlbuild -clean
	cd ${ROOT_DIR} && rm -rf ${TARGET}/*
