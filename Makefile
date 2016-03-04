LEX = /usr/local/Cellar/flex/2.6.0/bin/flex
YACC = /usr/local/Cellar/bison/3.0.4/bin/bison
LEX_FLAGS =
YACC_FLAGS = -d -Wno-other
C = clang
C_FLAGS = -ll -ly -g -Wno-implicit-function-declaration
C_DEBUG_FLAGS = -D DEBUG

.PHONY: help
.DEFAULT_GOAL := help

build: cmm ## Compiles all the files for the executable

clean: ## Cleans the directory from the intermediate files
	-rm cmm lex.yy.c cmm.tab.c cmm.tab.h
	-rm -rf cmm.dSYM

test: build ## Runs all the tests from the test suite
	./cmm samplePrograms/s1.cmm
	./cmm samplePrograms/s2.cmm
	./cmm samplePrograms/s3.cmm
	-./cmm samplePrograms/s4.cmm
	-./cmm samplePrograms/s5.cmm
	-./cmm samplePrograms/s6.cmm

cmm: lex.yy.c cmm.tab.c
	$(C) $(C_FLAGS) $(C_DEBUG_FLAGS) -o cmm lex.yy.c cmm.tab.c cmm.c

lex.yy.c: cmm.l
	$(LEX) $(LEX_FLAGS) cmm.l

cmm.tab.c: cmm.y
	$(YACC) $(YACC_FLAGS) cmm.y

help: ## Display this help text
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
