%{
#include <stdio.h>

#include "cmm.h"

extern FILE *yyin;
%}

%define parse.lac full
%define parse.error verbose

%union {
    struct t_symbol symbol;
    char* id;
    char t;
}

%token INTCONST DOUBLECONST BOOLCONST STRINGCONST
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
%nonassoc "usub"
%nonassoc "uadd"

%type <t> type expr call constant optExpr
%type <symbol> lValue

%%

start : decl
      | start decl
      ;

decl : declVariable
     | declFunction
     ;

declVariable : variable SEMICOLON
             ;

variable : type IDENTIFIER {
               place = createSymbol($2);
               place->t = $1;
           }
         ;

type : INT {
           $$ = 'I';
       }
     | DOUBLE {
           $$ = 'D';
       }
     | BOOL {
           $$ = 'B';
       }
     | STRING LBRACKET INTCONST RBRACKET {
           $$ = 'S';
       }
     | type LBRACKET INTCONST RBRACKET {
           $$ = 'A';
       }
     ;

declFunction : type IDENTIFIER scopeStart LPAREN formals RPAREN block scopeEnd {
                   place = createSymbol($2);
                   place->t = $1;
               }
             | VOID IDENTIFIER scopeStart LPAREN formals RPAREN block scopeEnd {
                   place = createSymbol($2);
                   place->t = 'V';
               }
             ;

formals : %empty
         | formalsDefEnd
         ;

formalsDefEnd : variable
              | formalsDef variable
              ;

formalsDef : variable COLON
           | formalsDef variable COLON
           ;

block : LBRACE blockDef RBRACE
      ;

blockDef : %empty
         | blockDef declVariable
         | blockDef instr
         ;

optExpr : %empty
        | expr {
              $$ = $1;
          }
        ;

instr : SEMICOLON
      | expr SEMICOLON
      | scopeStart instrIf scopeEnd
      | scopeStart instrWhile scopeEnd
      | scopeStart instrFor scopeEnd
      | instrReturn
      | instrPrint
      | block
      ;

instrIf : IF LPAREN expr RPAREN instr instrElse {
              if ($3 != 'B') {
                  yyerror("Non-compatible types: if");
              }
          }
        ;

instrElse : %empty %prec "then"
          | ELSE instr
          ;

instrWhile : WHILE LPAREN expr RPAREN instr {
                 if ($3 != 'B') {
                     yyerror("Non-compatible types: while");
                 }
             }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr {
               if ($5 != 'B') {
                   yyerror("Non-compatible types");
               }
           }
         ;

instrReturn : RETURN optExpr SEMICOLON
            ;

instrPrint : PRINT LPAREN instrPrintDefEnd RPAREN SEMICOLON
           ;

instrPrintDefEnd : expr
                 | instrPrintDef expr
                 ;

instrPrintDef : expr COLON
              | instrPrintDef expr COLON
              ;

expr : lValue ASSIGN expr {
           if ($1.t != $3) {
               yyerror("Non-compatible types: =");
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           $$ = $1.t;
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr + expr");
           }
       }
     | expr SUB expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr - expr");
           }
       }
     | expr MUL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr * expr");
           }
       }
     | expr DIV expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr / expr");
           }
       }
     | expr MOD expr {
           if ($1 == 'I' && $3 == 'I') {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr %% expr");
           }
       }
     | expr LESS expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr < expr");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr <= expr");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr > expr");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr >= expr");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr == expr");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr != expr");
           }
       }
     | expr AND expr {
           if ($1 == 'B' && $3 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr && expr");
           }
       }
     | expr OR expr {
           if ($1 == 'B' && $3 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: expr || expr");
           }
       }
     | NOT expr {
           if ($2 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: ! expr");
           }
       }
     | ADD expr %prec "uadd" {
           if ($2 == 'I' || $2 == 'D') {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: + expr");
           }
       }
     | SUB expr %prec "usub" {
           if ($2 == 'I' || $2 == 'D') {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: - expr");
           }
       }
     | READINT LPAREN RPAREN {
           $$ = 'I';
       }
     | READLINE LPAREN RPAREN {
           $$ = 'S';
       }
     ;

lValue : IDENTIFIER {
             $$ = *lookup($1);
         }
       | IDENTIFIER LBRACKET expr RBRACKET {
             $$ = *lookup($1);
         }
       ;

call : IDENTIFIER LPAREN reals RPAREN {
           $$ = lookup($1)->t;
       }
     ;

reals : %empty
      | realsDefEnd
      ;

realsDefEnd : expr
            | realsDef expr
            ;

realsDef : expr COLON
         | realsDef expr COLON
         ;

constant : DOUBLECONST {
               $$ = 'D';
           }
         | INTCONST {
               $$ = 'I';
           }
         | BOOLCONST {
               $$ = 'B';
           }
         | STRINGCONST {
               $$ = 'S';
           }
         ;

scopeStart : %empty {
                 pushSymbolTable();
             }
           ;

scopeEnd : %empty {
               popSymbolTable();
           }
         ;

%%

int main(int argc, char **argv) {
    if(argc > 1) {
        if(!(yyin = fopen(argv[1], "r"))) {
            perror(argv[1]);
            return (1);
        }
    }

    initializeSymbolTable();

    yyparse();
    if (hasError != 0) {
        return 1;
    } else {
        printf("Expression accepted\nPrinting symbols table\n");
        printSymbolTable();
        return 0;
    }
}

int yyerror(char *s) {
    fprintf(stderr,"%d> Error: %s\n", lineNumber, s);
    hasError = 1;
    return 0;
}
