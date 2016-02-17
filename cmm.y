%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cmm.h"
%}

%define parse.lac full
%define parse.error verbose

%union {
    t_typeexpr typeexpr;
    double d;
    int i;
    int b;
    char t;
    char* id;
    char* s;
}

%token <i> INTCONST
%token <d> DOUBLECONST
%token <i> BOOLCONST
%token <s> STRINGCONST
%token VOID INT DOUBLE BOOL STRING WHILE FOR IF ELSE RETURN PRINT READINT READLINE
%token ADD SUB MUL DIV MOD ASSIGN
%token LESS LESSEQ GREATER GREATEREQ EQUAL NEQUAL
%token AND OR NOT
%token SEMICOLON COLON
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token LBRACE RBRACE
%token <id> IDENTIFIER

%left COLON
%left ASSIGN
%left OR
%left AND
%left EQUAL NEQUAL
%left LESS LESSEQ GREATER GREATEREQ
%left ADD SUB
%left MUL DIV MOD
%left LPAREN LBRACKET
%nonassoc NOT
%nonassoc "then"
%nonassoc ELSE

%type <t> type
%type <typeexpr> expr
%type <typeexpr> call
%type <typeexpr> lValue
%type <typeexpr> constant
%type <typeexpr> optExpr

%%

start : decl { }
      | start decl { }
      ;

decl : declVariable { }
     | declFunction { }
     ;

declVariable : variable SEMICOLON { }
             ;

variable : type IDENTIFIER { place = lookup($2); place->te.t = $1; }
         ;

type : INT { $$ = 'I'; }
     | DOUBLE { $$ = 'D'; }
     | BOOL { $$ = 'B'; }
     | STRING LBRACKET INTCONST RBRACKET { $$ = 'S'; }
     | type LBRACKET INTCONST RBRACKET { $$ = 'A'; }
     ;

declFunction : type IDENTIFIER LPAREN formals RPAREN block { place = lookup($2); place->te.t = $1; }
             | VOID IDENTIFIER LPAREN formals RPAREN block { place = lookup($2); place->te.t = 'V'; }
             ;

formals : /* empty */ { }
         | formalsDefEnd { }
         ;

formalsDefEnd : variable { }
              | formalsDef variable { }
              ;

formalsDef : variable COLON { }
           | formalsDef variable COLON { }
           ;

block : LBRACE blockDef RBRACE { }
      ;

blockDef : /* empty */ { }
         | blockDef declVariable { }
         | blockDef instr { }
         ;

optExpr : /* empty */ { }
        | expr { $$.t = $1.t; }
        ;

instr : SEMICOLON { }
      | expr SEMICOLON { }
      | instrIf { }
      | instrWhile { }
      | instrFor { }
      | instrReturn { }
      | instrPrint { }
      | block { }
      ;

instrIf : IF LPAREN expr RPAREN instr instrElse { if($3.t != 'B') yyerror("Non-compatible types: if"); }
        ;

instrElse : /* empty */ { } %prec "then"
          | ELSE instr { }
          ;

instrWhile : WHILE LPAREN expr RPAREN instr { if($3.t != 'B') yyerror("Non-compatible types: while"); }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr { if($5.t != 'B') yyerror("Non-compatible types"); }
         ;

instrReturn : RETURN optExpr SEMICOLON { }
            ;

instrPrint : PRINT LPAREN instrPrintDefEnd RPAREN SEMICOLON { }
           ;

instrPrintDefEnd : expr { }
                 | instrPrintDef expr { }
                 ;

instrPrintDef : expr COLON { }
              | instrPrintDef expr COLON { }
              ;

expr : lValue ASSIGN expr { if($$.t != $3.t) yyerror("Non-compatible types: ="); else $$.t = $3.t; }
     | constant { $$.t = $1.t; }
     | lValue { }
     | call { $$.t = $1.t; }
     | LPAREN expr RPAREN { $$.t = $2.t; }
     | expr ADD expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Non-compatible types: +"); }
     | expr SUB expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Non-compatible types: -"); }
     | expr MUL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Non-compatible types: *"); }
     | expr DIV expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Non-compatible types: /"); }
     | expr MOD expr { if($1.t == 'I' && $3.t == 'I') $$.t = $1.t; else yyerror("Non-compatible types: %"); }
     | expr LESS expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: <"); }
     | expr LESSEQ expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: <="); }
     | expr GREATER expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: >"); }
     | expr GREATEREQ expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: >="); }
     | expr EQUAL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: =="); }
     | expr NEQUAL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Non-compatible types: !="); }
     | expr AND expr { if($1.t == 'B' && $3.t == 'B') $$.t = 'B'; else yyerror("Non-compatible types: &&"); }
     | expr OR expr { if($1.t == 'B' && $3.t == 'B') $$.t = 'B'; else yyerror("Non-compatible types: ||"); }
     | NOT expr { if($2.t == 'B') $$.t = 'B'; else yyerror("Non-compatible types: !"); }
     | READINT LPAREN RPAREN { $$.t = 'I'; }
     | READLINE LPAREN RPAREN { $$.t = 'S'; }
     ;

lValue : IDENTIFIER { $$.t = lookup($1)->te.t; }
       | expr LBRACKET expr RBRACKET { }
       ;

call : IDENTIFIER LPAREN reals RPAREN { $$.t = lookup($1)->te.t; }
     ;

reals : /* empty */ { }
      | realsDefEnd { }
      ;

realsDefEnd : expr { }
            | realsDef expr { }
            ;

realsDef : expr COLON { }
         | realsDef expr COLON { }
         ;

constant : sign DOUBLECONST { $$.t = 'D'; }
         | sign INTCONST { $$.t = 'I'; }
         | BOOLCONST { $$.t = 'B'; }
         | STRINGCONST { $$.t = 'S'; }
         ;

sign : /* empty */ { }
     | SUB { }
     ;

%%

int areNumeric(char type1, char type2) {
    return ((type1 == 'I' || type1 == 'D') &&
            type1 == type2);
}

static unsigned symhash(char *sym) {
    unsigned int hash = 0;
    unsigned c;

    while((c = *sym++)) {
        hash = hash * 9 ^ c;
    }

    return hash;
}

int nnew, nold;
int nprobe;
t_symbol* lookup(char* sym) {
    t_symbol *sp = &symtab[symhash(sym)%NHASH];
    int scount = NHASH; /* how many have we looked at */

    while(--scount >= 0) {
        nprobe++;
        if(sp->n && !strcmp(sp->n, sym)) {
            nold++;
            return sp;
        }

        if(!sp->n) { /* new entry */
            nnew++;
            sp->n = strdup(sym);
            return sp;
        }

        if(++sp >= symtab+NHASH) sp = symtab; /* try the next entry */
    }

    fprintf(stderr, "Symbol table overflow\n");
    abort(); /* tried them all, table is full */
}

void printSymbolTable() {
}

int main(int argc, char **argv)
{
    yyparse();
    printf("Expression accepted\nPrinting symbols table\n");
    printSymbolTable();
    return 0;
}

int yyerror(char *s)
{
    fprintf(stderr,"Error: %s\n", s);
    exit(1);
}
