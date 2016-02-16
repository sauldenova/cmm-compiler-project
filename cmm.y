%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cmm.h"
%}

%debug
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
%token COMMENT LLONGCOMMENT RLONGCOMMENT
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

variable : type IDENTIFIER { place = lookup($2); place->t = $1; }
         ;

type : INT { $$ = 'I'; }
     | DOUBLE { $$ = 'D'; }
     | BOOL { $$ = 'B'; }
     | STRING LBRACKET INTCONST RBRACKET { $$ = 'S'; }
     | type LBRACKET INTCONST RBRACKET { $$ = 'A'; }
     ;

declFunction : type IDENTIFIER LPAREN formals RPAREN block { place = lookup($2); place->t = $1; }
             | VOID IDENTIFIER LPAREN formals RPAREN block { place = lookup($2); place->t = 'V'; }
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

instrIf : IF LPAREN expr RPAREN instr instrElse { if($3.t != 'B') yyerror("Tipos incompatibles: if"); }
        ;

instrElse : /* empty */ { }
          | ELSE instr { }
          ;

instrWhile : WHILE LPAREN expr RPAREN instr { if($3.t != 'B') yyerror("Tipos incompatibles: while"); }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr { if($5.t != 'B') yyerror("Tipos incompatibles"); }
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

expr : lValue ASSIGN expr { $$.t = $3.t; }
     | constant { $$.t = $1.t; }
     | lValue { }
     | call { $$.t = $1.t; }
     | LPAREN expr RPAREN { $$.t = $2.t; }
     | expr ADD expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Tipos incompatibles: +"); }
     | expr SUB expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Tipos incompatibles: -"); }
     | expr MUL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Tipos incompatibles: *"); }
     | expr DIV expr { if(areNumeric($1.t, $3.t) != 0) $$.t = $1.t; else yyerror("Tipos incompatibles: /"); }
     | expr MOD expr { if($1.t == 'I' && $3.t == 'I') $$.t = $1.t; else yyerror("Tipos incompatibles: %"); }
     | expr LESS expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: <"); }
     | expr LESSEQ expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: <="); }
     | expr GREATER expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: >"); }
     | expr GREATEREQ expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: >="); }
     | expr EQUAL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: =="); }
     | expr NEQUAL expr { if(areNumeric($1.t, $3.t) != 0) $$.t = 'B'; else yyerror("Tipos incompatibles: !="); }
     | expr AND expr { if($1.t == 'B' && $3.t == 'B') $$.t = 'B'; else yyerror("Tipos incompatibles: &&"); }
     | expr OR expr { if($1.t == 'B' && $3.t == 'B') $$.t = 'B'; else yyerror("Tipos incompatibles: ||"); }
     | NOT expr { if($2.t == 'B') $$.t = 'B'; else yyerror("Tipos incompatibles: !"); }
     | READINT LPAREN RPAREN { $$.t = 'I'; }
     | READLINE LPAREN RPAREN { $$.t = 'S'; }
     ;

lValue : IDENTIFIER { $$.t = lookup($1)->t; }
       | expr LBRACKET expr RBRACKET { }
       ;

call : IDENTIFIER LPAREN reals RPAREN { $$.t = lookup($1)->t; }
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

    fputs("symbol table overflow\n", stderr);
    abort(); /* tried them all, table is full */
}

int main(int argc, char **argv)
{
    yyparse();
    printf("Expresion aceptada \n");
    return 0;
}

int yyerror(char *s)
{
    fprintf(stderr,"error: %s\n", s);
    exit(1);
}
