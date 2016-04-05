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
    char* val;
    char t;
}

%token <val> INTCONST DOUBLECONST BOOLCONST STRINGCONST
%token VOID INT DOUBLE BOOL STRING WHILE FOR IF ELSE RETURN PRINTINT PRINTSTRING PRINTDOUBLE READINT READLINE READDOUBLE
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

%type <t> type baseType
%type <instr> expr optExpr call constant variable reals realsDef realsDefEnd formals formalsDef formalsDefEnd declFunctionType
%type <symbol> lValue

%%

start : decl
      | start decl
      ;

decl : declVariable
     | declFunction
     ;

declVariable : variable SEMICOLON {
                   sprintf(str, "\t\t%%%s = alloca %s", $1->addr, transformType($1->type));
                   emit(str);
             }
             ;

variable : type IDENTIFIER {
               place = createSymbol($2);
               place->t = $1;

               $$ = allocateInstr();
               $$->type = $1;
               $$->addr = $2;
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

declFunction : declFunctionHead scopeStart block scopeEnd scopeEnd functionEnd {
                   if(currentFunction->t != VOID_FUNCTION_TYPE && currentFunction->returnCount == 0) {
                       sprintf(str, "The function %s needs at least one return", currentFunction->n);
                       yyerror(str);
                   }
               }
             ;

declFunctionHead : declFunctionType scopeStart LPAREN formals RPAREN {
                       currentFunction->arguments = $4->args;
                       sprintf(str, "define %s @%s(%s) {", transformType($1->type), $1->addr, $4->addr);
                       emit(str);
                       emit("entry:");

                       char* varName = allocateString(50);
                       char* type = allocateString(10);
                       char* past = $4->addr;
                       char* curr = $4->addr;
                       while (1) {
                           curr = strchr(curr + 1, ',');

                           char* typeStart = past;
                           char* typeEnd = strchr(typeStart, ' ');
                           if (typeEnd == NULL) {
                               break;
                           }
                           strncpy(type, typeStart, typeEnd - typeStart);

                           char* varStart = strstr(past, "p__") + 3;
                           char* varEnd = strchr(varStart, ' ');
                           if (varEnd == NULL) {
                               strcpy(varName, varStart);
                           } else {
                               strncpy(varName, varStart, varEnd - varStart);
                           }

                           sprintf(str, "\t\t%%%s = alloca %s", varName, type);
                           emit(str);

                           sprintf(str, "\t\tstore %s %%__p__%s , %s* %%%s", type, varName, type, varName);
                           emit(str);

                           if (curr == NULL) {
                               break;
                           }

                           past = curr + 1;
                       }
                 }
                 ;

declFunctionType : type IDENTIFIER {
                       place = createSymbol($2);
                       place->t = $1 + START_FUNCTION_TYPE;
                       currentFunction = place;

                       $$ = allocateInstr();
                       $$->type = $1 + START_FUNCTION_TYPE;
                       $$->addr = strdup($2);
                   }
                 | VOID IDENTIFIER {
                       place = createSymbol($2);
                       place->t = VOID_FUNCTION_TYPE;
                       currentFunction = place;

                       $$ = allocateInstr();
                       $$->type = VOID_FUNCTION_TYPE;
                       $$->addr = strdup($2);
                   }
                 ;

functionEnd : %empty {
                  emit("}");
                  emit("");
            }

formals : %empty {
              $$ = allocateInstr();
              $$->type = VOID_TYPE;
              $$->addr = "";
          }
        | formalsDefEnd {
              $$ = $1;
          }
        ;

formalsDefEnd : variable {
                    struct t_arguments_list* arguments = allocateArgumentsList();
                    arguments->next = NULL;
                    arguments->type = $1->type;

                    char* addr = allocateString(strlen($1->addr) + 50);
                    strcat(addr, transformType($1->type));
                    strcat(addr, " %__p__");
                    strcat(addr, $1->addr);

                    $$ = allocateInstr();
                    $$->args = arguments;
                    $$->addr = addr;
                }
              | formalsDef variable {
                    struct t_arguments_list* arguments = allocateArgumentsList();
                    arguments->next = $1->args;
                    arguments->type = $2->type;

                    char* addr = allocateString(strlen($1->addr) + strlen($2->addr) + 50);
                    strcat(addr, $1->addr);;
                    strcat(addr, " , ");
                    strcat(addr, transformType($1->type));
                    strcat(addr, " %__p__");
                    strcat(addr, $2->addr);

                    $$ = allocateInstr();
                    $$->args = arguments;
                    $$->addr = addr;
                }
              ;

formalsDef : variable COLON {
                 struct t_arguments_list* arguments = allocateArgumentsList();
                 arguments->next = NULL;
                 arguments->type = $1->type;

                 char* addr = allocateString(strlen($1->addr) + 50);
                 strcat(addr, transformType($1->type));
                 strcat(addr, " %__p__");
                 strcat(addr, $1->addr);

                 $$ = allocateInstr();
                 $$->args = arguments;
                 $$->addr = addr;
             }
           | formalsDef variable COLON {
                 struct t_arguments_list* arguments = allocateArgumentsList();
                 arguments->next = $1->args;
                 arguments->type = $2->type;

                 char* addr = allocateString(strlen($1->addr) + strlen($2->addr) + 50);
                 strcat(addr, $1->addr);;
                 strcat(addr, " , ");
                 strcat(addr, transformType($1->type));
                 strcat(addr, " %__p__");
                 strcat(addr, $2->addr);

                 $$ = allocateInstr();
                 $$->args = arguments;
                 $$->addr = addr;
             }
           ;

block : LBRACE blockDef RBRACE
      ;

blockDef : %empty
         | blockDef declVariable
         | blockDef instr
         ;

optExpr : %empty {
              $$ = allocateInstr();
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

instrIf : IF LPAREN instrIfCond RPAREN scopeStart instr scopeEnd instrIfEnd %prec "then" {
          }
        | IF LPAREN instrIfCond RPAREN scopeStart instr scopeEnd ELSE instrIfElse scopeStart instr scopeEnd instrIfElseEnd {
          }
        ;

instrIfCond : expr {
                  if ($1->type != BOOL_TYPE) {
                      yyerror("Non-compatible types: if");
                  } else {
                      label1 = createLabel();
                      label2 = createLabel();
                      endLabel = createLabel();
                      sprintf(str, "\t\tbr i1 %s, label %%%s, label %%%s", $1->addr, label1, label2);
                      emit(str);
                      sprintf(str, "%s:", label1);
                      emit(str);
                  }
              }
            ;

instrIfElse : %empty {
                  sprintf(str, "\t\tbr label %%%s", endLabel);
                  emit(str);
                  sprintf(str, "%s:", label2);
                  emit(str);
              }
            ;

instrIfEnd : %empty {
                 sprintf(str, "\t\tbr label %%%s", label2);
                 emit(str);
                 sprintf(str, "%s:", label2);
                 emit(str);
             }
           ;

instrIfElseEnd : %empty {
                     sprintf(str, "\t\tbr label %%%s", endLabel);
                     emit(str);
                     sprintf(str, "%s:", endLabel);
                     emit(str);
                 }
               ;

instrWhile : instrWhileStart WHILE LPAREN instrWhileCond RPAREN instr instrWhileEnd {
             }
           ;

instrWhileStart : %empty {
                      startLabel = createLabel();
                      label1 = createLabel();
                      endLabel = createLabel();
                      sprintf(str, "\t\tbr label %%%s", startLabel);
                      emit(str);
                      sprintf(str, "%s:", startLabel);
                      emit(str);
                  }
                ;

instrWhileCond : expr {
                     if ($1->type != BOOL_TYPE) {
                         yyerror("Non-compatible types: while");
                     } else {
                         sprintf(str, "\t\tbr i1 %s , label %%%s , label %%%s", $1->addr, label1, endLabel);
                         emit(str);
                         sprintf(str, "%s:", label1);
                         emit(str);
                     }
                 }
               ;

instrWhileEnd : %empty {
                    sprintf(str, "\t\tbr label %%%s", startLabel);
                    emit(str);
                    sprintf(str, "%s:", endLabel);
                    emit(str);
                }
              ;

instrFor : FOR LPAREN optExpr instrForStart SEMICOLON instrForCond SEMICOLON optExpr RPAREN instrForLabel instr instrForEnd {
           }
         ;

instrForStart : %empty {
                    startLabel = createLabel();
                    label1 = createLabel();
                    label2 = createLabel();
                    endLabel = createLabel();
                    sprintf(str, "\t\tbr label %%%s", startLabel);
                    emit(str);
                    sprintf(str, "%s:", startLabel);
                    emit(str);
                }
              ;

instrForCond : expr {
                   if ($1->type != BOOL_TYPE) {
                       yyerror("Non-compatible types: for");
                   } else {
                       sprintf(str, "\t\tbr i1 %s , label %%%s , label %%%s", $1->addr, label1, endLabel);
                       emit(str);
                       sprintf(str, "%s:", label2);
                       emit(str);
                   }
               }
             ;

instrForLabel : %empty {
                    sprintf(str, "\t\tbr label %%%s", startLabel);
                    emit(str);
                    sprintf(str, "%s:", label1);
                    emit(str);
                }
              ;

instrForEnd : %empty {
                  sprintf(str, "\t\tbr label %%%s", label2);
                  emit(str);
                  sprintf(str, "%s:", endLabel);
                  emit(str);
              }
            ;

instrReturn : RETURN optExpr SEMICOLON {
                  if ($2->type != currentFunction->t - START_FUNCTION_TYPE) {
                      yyerror("Invalid return function type");
                  } else {
                      currentFunction->returnCount++;

                      if ($2->type == VOID_TYPE) {
                          sprintf(str, "\t\tret void");
                      } else {
                          sprintf(str, "\t\tret %s %s", transformType($2->type), $2->addr);
                      }
                      emit(str);
                  }
              }
            ;

instrPrint : PRINTDOUBLE LPAREN expr RPAREN SEMICOLON {
                 if ($3->type != DOUBLE_TYPE) {
                     yyerror("Non-double arguments in printDouble expression");
                 } else {
                     sprintf(str, "\t\tcall void @printDouble(double %s)", $3->addr);
                     emit(str);
                 }
             }
           | PRINTINT LPAREN expr RPAREN SEMICOLON {
                 if ($3->type != INT_TYPE) {
                     yyerror("Non-int arguments in printInt expression");
                 } else {
                     sprintf(str, "\t\tcall void @printInt(i32 %s)", $3->addr);
                     emit(str);
                 }
             }
           | PRINTSTRING LPAREN expr RPAREN SEMICOLON {
                 if ($3->type != STRING_TYPE) {
                     yyerror("Non-string arguments in printString expression");
                 } else {
                     sprintf(str, "\t\tcall void @printString(i8* %s)", $3->addr);
                     emit(str);
                 }
             }
           ;

expr : lValue ASSIGN expr {
           if ($1 != NULL) {
               if ($1->t != $3->type) {
                   yyerror("Non-compatible types: =");
               } else {
                   const char* type = transformType($1->t);
                   sprintf(str, "\t\tstore %s %s , %s* %%%s", type, $3->addr, type, $1->n);
                   emit(str);
                   $$ = $3;
               }
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           char* temp = createTemporal();
           sprintf(str, "\t\t%s = load %s* %%%s", temp, transformType($1->t), $1->n);
           emit(str);

           $$ = allocateInstr();
           $$->type = ($1 == NULL ? '\0' : $1->t);
           $$->addr = temp;
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = add i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fadd double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr + expr");
           }
       }
     | expr SUB expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = sub i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fsub double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr - expr");
           }
       }
     | expr MUL expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = mul i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fmul double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr * expr");
           }
       }
     | expr DIV expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = sdiv i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fdiv double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr / expr");
           }
       }
     | expr MOD expr {
           if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "\t\t%s = srem i32 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr  expr");
           }
       }
     | expr LESS expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp slt i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp olt double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr < expr");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp sle i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp ole double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr <= expr");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp sgt i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp ogt double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr > expr");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp sge i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp oge double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr >= expr");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp eq i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp oeq double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr == expr");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1->type, $3->type)) {
               char* temp = createTemporal();

               if ($1->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = icmp ne i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "\t\t%s = fcmp one double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr != expr");
           }
       }
     | expr AND expr {
           if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "\t\t%s = and i1 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr && expr");
           }
       }
     | expr OR expr {
           if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "\t\t%s = or i1 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr || expr");
           }
       }
     | NOT expr {
           if ($2->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "\t\t%s = xor i1 %s, 1", temp, $2->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = temp;
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
               char* temp = createTemporal();

               if ($2->type == INT_TYPE) {
                   sprintf(str, "\t\t%s = sub i32 0, %s", temp, $2->addr);
               } else {
                   sprintf(str, "\t\t%s = fsub double -0.0, %s", temp, $2->addr);
               }
               emit(str);

               $$ = allocateInstr();
               $$->type = $2->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: - expr");
           }
       }
     | READINT LPAREN RPAREN {
           char* temp = createTemporal();

           sprintf(str, "\t\t%s = call i32 @readInt()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type = INT_TYPE;
           $$->addr = temp;
       }
     | READDOUBLE LPAREN RPAREN {
           char* temp = createTemporal();

           sprintf(str, "\t\t%s = call i32 @readDouble()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type = DOUBLE_TYPE;
           $$->addr = temp;
       }
     | READLINE LPAREN RPAREN {
           char* temp = createTemporal();

           sprintf(str, "\t\t%s = call i32 @readLine()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type = STRING_TYPE;
           $$->addr = temp;
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
           $$ = allocateInstr();
           if (!verifyArguments(place->arguments, $3->args)) {
               sprintf(str, "Incorrect argument types for call to %s", $1);
               yyerror(&str);
           } else {
               char* temp = createTemporal();
               sprintf(str, "\t\t%s = call %s @%s(%s)", temp, transformType(place->t), $1, $3->addr);
               emit(str);
               $$->addr = temp;
           }

           $$->type = (place == NULL ? INVALID_TYPE : place->t - START_FUNCTION_TYPE);
       }
     ;

reals : %empty {
            $$ = allocateInstr();
            $$->type = VOID_TYPE;
            $$->addr = "";
        }
      | realsDefEnd {
            $$ = $1;
        }
      ;

realsDefEnd : expr {
                  struct t_arguments_list* arguments = allocateArgumentsList();
                  arguments->next = NULL;
                  arguments->type = $1->type;

                  char* addr = allocateString(strlen($1->addr) + 50);
                  strcat(addr, transformType($1->type));
                  strcat(addr, " ");
                  strcat(addr, $1->addr);

                  $$ = allocateInstr();
                  $$->args = arguments;
                  $$->addr = addr;
              }
            | realsDef expr {
                  struct t_arguments_list* arguments = allocateArgumentsList();
                  arguments->next = $1->args;
                  arguments->type = $2->type;

                  char* addr = allocateString(strlen($1->addr) + strlen($2->addr) + 50);
                  strcat(addr, $1->addr);
                  strcat(addr, " , ");
                  strcat(addr, transformType($2->type));
                  strcat(addr, " ");
                  strcat(addr, $2->addr);

                  $$ = allocateInstr();
                  $$->args = arguments;
                  $$->addr = addr;
              }
            ;

realsDef : expr COLON {
               struct t_arguments_list* arguments = allocateArgumentsList();
               arguments->next = NULL;
               arguments->type = $1->type;

               char* addr = allocateString(strlen($1->addr) + 50);
               strcat(addr, transformType($1->type));
               strcat(addr, " ");
               strcat(addr, $1->addr);

               $$ = allocateInstr();
               $$->args = arguments;
               $$->addr = addr;
           }
         | realsDef expr COLON {
               struct t_arguments_list* arguments = allocateArgumentsList();
               arguments->next = $1->args;
               arguments->type = $2->type;

               char* addr = allocateString(strlen($1->addr) + strlen($2->addr) + 50);
               strcat(addr, $1->addr);
               strcat(addr, " , ");
               strcat(addr, transformType($2->type));
               strcat(addr, " ");
               strcat(addr, $2->addr);

               $$ = allocateInstr();
               $$->args = arguments;
               $$->addr = addr;
           }
         ;

constant : DOUBLECONST {
               $$ = allocateInstr();
               $$->type = DOUBLE_TYPE;
               $$->addr = $1;
           }
         | INTCONST {
               $$ = allocateInstr();
               $$->type = INT_TYPE;
               $$->addr = $1;
           }
         | BOOLCONST {
               $$ = allocateInstr();
               $$->type = BOOL_TYPE;
               $$->addr = $1;
           }
         | STRINGCONST {
               $$ = allocateInstr();
               $$->type = STRING_TYPE;
               $$->addr = $1;
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
    emitHeader();

    yyparse();
    if (hasError) {
        return 1;
    } else {
        #ifdef _DEBUG
        printf("Expression accepted\n");
        printf("Printing symbols table\n");
        printSymbolTable();
        #endif //_DEBUG

        FILE* input = fopen("program.ll", "w");

        writeCodeToFile(input);

        fclose(input);

        return 0;
    }
}

int yyerror(char *s) {
    fprintf(stderr,"%d> Error: %s\n", lineNumber + 1, s);
    hasError = 1;
    return 0;
}
