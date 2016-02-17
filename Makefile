LEX = /usr/local/Cellar/flex/2.6.0/bin/flex
YACC = /usr/local/Cellar/bison/3.0.4/bin/bison
LEX_FLAGS =
YACC_FLAGS = -d
C = clang
C_FLAGS = -ll -ly -g -Wno-implicit-function-declaration

all: cmm

clean:
	rm cmm lex.yy.c cmm.tab.c cmm.tab.h
	rm -rf cmm.dSYM

test: all
	./cmm < s1.cmm
	./cmm < s2.cmm
	./cmm < s3.cmm
	-./cmm < s4.cmm
	-./cmm < s5.cmm

cmm: lex.yy.c cmm.tab.c
	$(C) $(C_FLAGS) -o cmm lex.yy.c cmm.tab.c

lex.yy.c: cmm.l
	$(LEX) $(LEX_FLAGS) cmm.l

cmm.tab.c: cmm.y
	$(YACC) $(YACC_FLAGS) cmm.y
