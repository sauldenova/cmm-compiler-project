%{
#include <stdio.h>

#include "cmm.h"
#include "cmm_types.h"

extern FILE *yyin;
%}

%define parse.lac full
%define parse.error verbose

%union {
    struct t_symbol* symbol;
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
           $$ = INT_TYPE;
       }
     | DOUBLE {
           $$ = DOUBLE_TYPE;
       }
     | BOOL {
           $$ = BOOL_TYPE;
       }
     | STRING LBRACKET INTCONST RBRACKET {
           $$ = STRING_TYPE;
       }
     | type LBRACKET INTCONST RBRACKET {
           $$ = $1 + START_ARRAY_TYPE;
       }
     ;

declFunction : declFunctionType scopeStart LPAREN formals RPAREN block scopeEnd;

declFunctionType : type IDENTIFIER {
                       place = createSymbol($2);
                       place->t = $1 + START_FUNCTION_TYPE;
                       currentFunction = place;
                   }
                 | VOID IDENTIFIER {
                       place = createSymbol($2);
                       place->t = VOID_FUNCTION_TYPE;
                       currentFunction = place;
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

optExpr : %empty {
              $$ = VOID_TYPE;
          }
        | expr {
              $$ = $1;
          }
        ;

instr : SEMICOLON
      | expr SEMICOLON
      | instrIf
      | scopeStart instrWhile scopeEnd
      | scopeStart instrFor scopeEnd
      | instrReturn
      | instrPrint
      | scopeStart block scopeEnd
      ;

instrIf : IF LPAREN expr RPAREN scopeStart instr scopeEnd instrElse {
              if ($3 != BOOL_TYPE) {
                  yyerror("Non-compatible types: if");
              }
          }
        ;

instrElse : %empty %prec "then"
          | ELSE scopeStart instr scopeEnd
          ;

instrWhile : WHILE LPAREN expr RPAREN instr {
                 if ($3 != BOOL_TYPE) {
                     yyerror("Non-compatible types: while");
                 }
             }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr {
               if ($5 != BOOL_TYPE) {
                   yyerror("Non-compatible types");
               }
           }
         ;

instrReturn : RETURN optExpr SEMICOLON {
                  if ($2 != currentFunction->t - START_FUNCTION_TYPE) {
                      yyerror("Invalid return function type");
                  }
              }
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
           if ($1 != NULL && $1->t != $3) {
               yyerror("Non-compatible types: =");
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           $$ = ($1 == NULL ? '\0' : $1->t);
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1, $3)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr + expr");
           }
       }
     | expr SUB expr {
           if (areNumeric($1, $3)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr - expr");
           }
       }
     | expr MUL expr {
           if (areNumeric($1, $3)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr * expr");
           }
       }
     | expr DIV expr {
           if (areNumeric($1, $3)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr / expr");
           }
       }
     | expr MOD expr {
           if ($1 == INT_TYPE && $3 == INT_TYPE) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr %% expr");
           }
       }
     | expr LESS expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr < expr");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr <= expr");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr > expr");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr >= expr");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr == expr");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1, $3)) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr != expr");
           }
       }
     | expr AND expr {
           if ($1 == BOOL_TYPE && $3 == BOOL_TYPE) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr && expr");
           }
       }
     | expr OR expr {
           if ($1 == BOOL_TYPE && $3 == BOOL_TYPE) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr || expr");
           }
       }
     | NOT expr {
           if ($2 == BOOL_TYPE) {
               $$ = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: ! expr");
           }
       }
     | ADD expr %prec "uadd" {
           if ($2 == INT_TYPE || $2 == DOUBLE_TYPE) {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: + expr");
           }
       }
     | SUB expr %prec "usub" {
           if ($2 == INT_TYPE || $2 == DOUBLE_TYPE) {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: - expr");
           }
       }
     | READINT LPAREN RPAREN {
           $$ = INT_TYPE;
       }
     | READLINE LPAREN RPAREN {
           $$ = STRING_TYPE;
       }
     ;

lValue : IDENTIFIER {
             place = lookup($1);
             $$ = (place == NULL ? NULL : place);
         }
       | IDENTIFIER LBRACKET expr RBRACKET {
             place = lookup($1);
             $$ = (place == NULL ? NULL : place);
         }
       ;

call : IDENTIFIER LPAREN reals RPAREN {
           place = lookup($1);
           $$ = (place == NULL ? INVALID_TYPE : place->t - START_FUNCTION_TYPE);
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
               $$ = DOUBLE_TYPE;
           }
         | INTCONST {
               $$ = INT_TYPE;
           }
         | BOOLCONST {
               $$ = BOOL_TYPE;
           }
         | STRINGCONST {
               $$ = STRING_TYPE;
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
