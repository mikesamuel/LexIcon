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

executables: AllTests.native RunBenchmarks.native DeadWood.native Compile.native
	ocaml make.ml 

%.native: *.ml *.mli
	ocaml make.ml $@

runtests: runtests.make.timestamp
runtests.make.timestamp: AllTests.native
# Always test AllTests tests all tests
	ls *Test.ml | perl -pe 's/\.ml//' | sort > /tmp/test_files \
	&& \
	cat AllTests.ml | perl -ne 'print if s/^module \w+ = //' \
	  > /tmp/tested_modules \
	&& \
	diff -uw /tmp/test_files /tmp/tested_modules \
	&& \
	./run_test AllTests.native \
	&& \
	touch runtests.make.timestamp
# Dump files under test-files that were not accessed.
	@find test-files -type f -atime +5m | egrep -v \
	  '~$$|/benchmarks/|/\.gitignore$$|visualize_output_buffer\.html$$|\.dot\.svg$$|/cksums$$' \
	  > "$$TEMP"/unused-test-files.txt
	@if [ -s "$$TEMP"/unused-test-files.txt ]; then \
	  echo "Unused test-files"; \
	  echo "================="; \
	  cat "$$TEMP"/unused-test-files.txt; \
	fi

ocamldoc: ocamldoc.make.timestamp
ocamldoc.make.timestamp: *.ml *.mli executables
	./run_ocamldoc.sh && touch ocamldoc.make.timestamp

benchmark: RunBenchmarks.native
	./RunBenchmarks.native

clean:
	ocamlbuild -clean
	rm -rf test-outputs/* *.make.timestamp
