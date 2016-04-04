C = clang
LEX = /usr/local/Cellar/flex/2.6.0/bin/flex
YACC = /usr/local/Cellar/bison/3.0.4/bin/bison
LLVM_AS = /usr/local/Cellar/llvm/3.6.2/bin/llvm-as
LLVM_LINK = /usr/local/Cellar/llvm/3.6.2/bin/llvm-link
LLVM_OPT = /usr/local/Cellar/llvm/3.6.2/bin/opt
LLVM_LLI = /usr/local/Cellar/llvm/3.6.2/bin/lli
LLVM_LLC = /usr/local/Cellar/llvm/3.6.2/bin/llc

LEX_FLAGS =
YACC_FLAGS = -d -Wno-other
C_FLAGS = -ll -ly -g -Wno-implicit-function-declaration
C_DEBUG_FLAGS = -D DEBUG
LLVM_LINK_FLAGS = -o main.bc
LLVM_OPT_FLAGS = -std-link-opts
LLVM_LLC_FLAGS = -filetype=obj

.PHONY: help
.DEFAULT_GOAL := help

build: cmm ## Compiles all the files for the executable

clean: ## Cleans the directory from the intermediate files
	-rm cmm lex.yy.c cmm.tab.c cmm.tab.h
	-rm program.ll program.bc runtime.bc main.bc main_opt.bc main_opt.o a.out
	-rm -rf cmm.dSYM

test: build ## Runs all the tests from the test suite
	./cmm samplePrograms/s1.cmm
	./cmm samplePrograms/s2.cmm
	./cmm samplePrograms/s3.cmm
	-./cmm samplePrograms/s4.cmm
	-./cmm samplePrograms/s5.cmm
	-./cmm samplePrograms/s6.cmm
	./cmm samplePrograms/s7.cmm
	$(LLVM_AS) program.ll
	$(LLVM_AS) runtime.ll
	$(LLVM_LINK) program.bc runtime.bc $(LLVM_LINK_FLAGS)
	$(LLVM_OPT) $(LLVM_OPT_FLAGS) main.bc > main_opt.bc
	$(LLVM_LLC) $(LLVM_LLC_FLAGS) main_opt.bc
	$(C) main_opt.o
	./a.out < samplePrograms/s7.in

cmm: lex.yy.c cmm.tab.c
	$(C) $(C_FLAGS) $(C_DEBUG_FLAGS) -o cmm lex.yy.c cmm.tab.c cmm.c cmm_types.c

lex.yy.c: cmm.l
	$(LEX) $(LEX_FLAGS) cmm.l

cmm.tab.c: cmm.y
	$(YACC) $(YACC_FLAGS) cmm.y

help: ## Display this help text
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
