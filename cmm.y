%{
#include <stdio.h>

#include "cmm.h"
#include "cmm_types.h"

extern FILE *yyin;
%}

%define parse.lac full
%define parse.error verbose

%union {
    struct t_instr* instr;
    struct t_symbol* symbol;
    struct t_arguments_list* args;
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

%type <t> type baseType declFunctionType
%type <instr> expr optExpr call constant variable
%type <symbol> lValue
%type <args> reals realsDef realsDefEnd formals formalsDef formalsDefEnd

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
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = $1;
           }
         ;

type : baseType {
           $$ = $1;
       }
     | STRING LBRACKET INTCONST RBRACKET {
           $$ = STRING_TYPE;
       }
     | baseType LBRACKET INTCONST RBRACKET {
           $$ = $1 + START_ARRAY_TYPE;
       }
     ;

baseType : INT {
               $$ = INT_TYPE;
           }
         | DOUBLE {
               $$ = DOUBLE_TYPE;
           }
         | BOOL {
               $$ = BOOL_TYPE;
           }
         ;

declFunction : declFunctionType scopeStart LPAREN formals RPAREN scopeStart block scopeEnd scopeEnd {
                   currentFunction->arguments = $4;
               }
             ;

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

formals : %empty {
              $$ = NULL;
          }
        | formalsDefEnd {
              $$ = $1;
          }
        ;

formalsDefEnd : variable {
                    struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                    arguments->next = NULL;
                    arguments->type = $1->type;
                    $$ = arguments;
                }
              | formalsDef variable {
                    struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                    arguments->next = $1;
                    arguments->type = $2->type;
                    $$ = arguments;
                }
              ;

formalsDef : variable COLON {
                 struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                 arguments->next = NULL;
                 arguments->type = $1->type;
                 $$ = arguments;
             }
           | formalsDef variable COLON {
                 struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                 arguments->next = $1;
                 arguments->type = $2->type;
                 $$ = arguments;
             }
           ;

block : LBRACE blockDef RBRACE
      ;

blockDef : %empty
         | blockDef declVariable
         | blockDef instr
         ;

optExpr : %empty {
              $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
              $$->type = VOID_TYPE;
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
              if ($3->type != BOOL_TYPE) {
                  yyerror("Non-compatible types: if");
              }
          }
        ;

instrElse : %empty %prec "then"
          | ELSE scopeStart instr scopeEnd
          ;

instrWhile : WHILE LPAREN expr RPAREN instr {
                 if ($3->type != BOOL_TYPE) {
                     yyerror("Non-compatible types: while");
                 }
             }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr {
               if ($5->type != BOOL_TYPE) {
                   yyerror("Non-compatible types");
               }
           }
         ;

instrReturn : RETURN optExpr SEMICOLON {
                  if ($2->type != currentFunction->t - START_FUNCTION_TYPE) {
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
           if ($1 != NULL && $1->t != $3->type) {
               yyerror("Non-compatible types: =");
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
           $$->type = ($1 == NULL ? '\0' : $1->t);
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr + expr");
           }
       }
     | expr SUB expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr - expr");
           }
       }
     | expr MUL expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr * expr");
           }
       }
     | expr DIV expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr / expr");
           }
       }
     | expr MOD expr {
           if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: expr %% expr");
           }
       }
     | expr LESS expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr < expr");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr <= expr");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr > expr");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr >= expr");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr == expr");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1->type, $3->type)) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr != expr");
           }
       }
     | expr AND expr {
           if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr && expr");
           }
       }
     | expr OR expr {
           if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: expr || expr");
           }
       }
     | NOT expr {
           if ($2->type == BOOL_TYPE) {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           } else {
               yyerror("Non-compatible types: ! expr");
           }
       }
     | ADD expr %prec "uadd" {
           if ($2->type == INT_TYPE || $2->type == DOUBLE_TYPE) {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: + expr");
           }
       }
     | SUB expr %prec "usub" {
           if ($2->type == INT_TYPE || $2->type == DOUBLE_TYPE) {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: - expr");
           }
       }
     | READINT LPAREN RPAREN {
           $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
           $$->type = INT_TYPE;
       }
     | READLINE LPAREN RPAREN {
           $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
           $$->type = STRING_TYPE;
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
           if (!verifyArguments(place->arguments, $3)) {
               char str[100];
               sprintf(str, "Incorrect argument types for call to %s", $1);
               yyerror(&str);
           }
           $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
           $$->type = (place == NULL ? INVALID_TYPE : place->t - START_FUNCTION_TYPE);
       }
     ;

reals : %empty {
            $$ = NULL;
        }
      | realsDefEnd {
            $$ = $1;
        }
      ;

realsDefEnd : expr {
                  struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                  arguments->next = NULL;
                  arguments->type = $1->type;
                  $$ = arguments;
              }
            | realsDef expr {
                  struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
                  arguments->next = $1;
                  arguments->type = $2->type;
                  $$ = arguments;
              }
            ;

realsDef : expr COLON {
               struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
               arguments->next = NULL;
               arguments->type = $1->type;
               $$ = arguments;
           }
         | realsDef expr COLON {
               struct t_arguments_list* arguments = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list*));
               arguments->next = $1;
               arguments->type = $2->type;
               $$ = arguments;
           }
         ;

constant : DOUBLECONST {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = DOUBLE_TYPE;
           }
         | INTCONST {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = INT_TYPE;
           }
         | BOOLCONST {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = BOOL_TYPE;
           }
         | STRINGCONST {
               $$ = (struct t_instr*)malloc(sizeof(struct t_instr*));
               $$->type = STRING_TYPE;
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
    fprintf(stderr,"%d> Error: %s\n", lineNumber + 1, s);
    hasError = 1;
    return 0;
}
