C = clang
LEX = /usr/local/Cellar/flex/2.6.0/bin/flex
YACC = /usr/local/Cellar/bison/3.0.4/bin/bison

LEX_FLAGS = -oout/lex.yy.c
YACC_FLAGS = -d -oout/cmm.tab.c -Wno-other
C_FLAGS = -ll -ly -g -Wno-implicit-function-declaration -Isrc -Iout
C_DEBUG_FLAGS = -DDEBUG

.PHONY: help
.DEFAULT_GOAL := help

build: out/cmmFront ## Compiles all the files for the executable

clean: ## Cleans the directory from the intermediate files
	-rm -rf out
	-rm *.out

test: build ## Runs all the tests from the test suite
	./cmm samplePrograms/s1.cmm
	./cmm samplePrograms/s2.cmm
	./cmm samplePrograms/s3.cmm
	-./cmm samplePrograms/s4.cmm
	-./cmm samplePrograms/s5.cmm
	-./cmm samplePrograms/s6.cmm
	./cmm samplePrograms/s7.cmm
	./a.out < samplePrograms/s7.in
	-rm a.out

out/:
	mkdir out

out/cmmFront: out/lex.yy.c out/cmm.tab.c
	$(C) $(C_FLAGS) -o out/cmmFront out/lex.yy.c out/cmm.tab.c src/cmm.c src/cmm_types.c

out/lex.yy.c: out/ src/cmm.l
	$(LEX) $(LEX_FLAGS) src/cmm.l

out/cmm.tab.c: out/ src/cmm.y
	$(YACC) $(YACC_FLAGS) src/cmm.y

help: ## Display this help text
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
