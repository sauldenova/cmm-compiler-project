LEX = flex
YACC = bison
LEX_FLAGS =
YACC_FLAGS = -d
C = clang
C_FLAGS = -ll -ly

all: cmm

clean:
	rm cmm lex.yy.c cmm.tab.c cmm.tab.h

cmm: lex.yy.c cmm.tab.c
	$(C) $(C_FLAGS) -o cmm lex.yy.c cmm.tab.c

lex.yy.c: cmm.l
	$(LEX) $(LEX_FLAGS) cmm.l

cmm.tab.c: cmm.y
	$(YACC) $(YACC_FLAGS) cmm.y
