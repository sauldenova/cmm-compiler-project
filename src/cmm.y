%{
#include <stdio.h>

#include "cmm.h"
#include "cmm_types.h"

extern FILE *yyin;
void generatePrints(struct t_instr* instr);
%}

%define parse.lac full
%define parse.error verbose

%union {
    struct t_instr* instr;
    struct t_symbol* symbol;
    struct t_arguments_list* args;
    struct t_type* type;
    char* id;
    char* val;
}

%token <val> INTCONST DOUBLECONST BOOLCONST STRINGCONST
%token VOID INT DOUBLE BOOL STRING WHILE FOR IF ELSE RETURN PRINT READINT READLINE READDOUBLE
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

%type <type> type baseType
%type <instr> expr optExpr call constant variable reals realsDef realsDefEnd formals formalsDef formalsDefEnd declFunctionType
%type <symbol> lValue

%%

start : decl
      | start decl
      ;

decl : declGlobalVariable
     | declFunction
     ;

declGlobalVariable : variable SEMICOLON {
                         if ($1->type->type == STRING_TYPE) {
                             sprintf(str,
                                     "%s = global [%d x %s] zeroinitializer",
                                     $1->addr,
                                     $1->type->size,
                                     transformArrayType($1->type));

                             emit(str);
                         } else if (isTypeArray($1->type)) {
                             const char* arrayType = transformArrayType($1->type);

                             sprintf(str,
                                     "%s = global [%d x %s] zeroinitializer",
                                     $1->addr,
                                     $1->type->size,
                                     arrayType);

                             emit(str);
                         } else {
                             sprintf(str,
                                     "%s = global %s 0",
                                     $1->addr,
                                     transformType($1->type));

                             emit(str);
                         }
                     }
                   ;

declVariable : variable SEMICOLON {
                   if (isTypeArray($1->type)) {
                       sprintf(str, "  %s = alloca [%d x %s]", $1->addr, $1->type->size, transformArrayType($1->type));
                       emit(str);
                   } else {
                       sprintf(str, "  %s = alloca %s", $1->addr, transformType($1->type));
                       emit(str);
                   }
               }
             ;

variable : type IDENTIFIER {
               place = createSymbol($2);
               free(place->type);
               place->type = copyType($1);

               $$ = allocateInstr();
               free($$->type);
               $$->type = copyType($1);
               $$->addr = place->internalName;
           }
         ;

type : baseType {
           $$ = $1;
       }
     | STRING LBRACKET INTCONST RBRACKET {
           $$ = allocateType();
           $$->type = STRING_TYPE;
           $$->size = atoi($3) + 1;
       }
     | baseType LBRACKET INTCONST RBRACKET {
           $$ = allocateType();
           $$->type = $1->type + START_ARRAY_TYPE;
           $$->size = atoi($3);
           free($1);
       }
     ;

baseType : INT {
               $$ = allocateType();
               $$->type = INT_TYPE;
           }
         | DOUBLE {
               $$ = allocateType();
               $$->type = DOUBLE_TYPE;
           }
         | BOOL {
               $$ = allocateType();
               $$->type = BOOL_TYPE;
           }
         ;

declFunction : declFunctionHead scopeStart block scopeEnd scopeEnd functionEnd {
                   if (currentFunction->type->type != VOID_FUNCTION_TYPE && currentFunction->returnCount == 0) {
                       sprintf(str, "The function %s needs at least one return", currentFunction->name);
                       yyerror(str);
                   }
                   if (currentFunction->returnCount > 1) {
                       sprintf(str, "The function %s can only have one return", currentFunction->name);
                       yyerror(str);
                   }
               }
             ;

declFunctionHead : declFunctionType scopeStart LPAREN formals RPAREN {
                       emit("");
                       temporalCount = 1;
                       currentFunction->arguments = $4->args;
                       sprintf(str, "define %s %s(%s) #0 {", transformType($1->type), $1->addr, $4->addr);
                       emit(str);

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

                           sprintf(str, "  %%%s = alloca %s", varName, type);
                           emit(str);

                           sprintf(str, "  store %s %%__p__%s , %s* %%%s", type, varName, type, varName);
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
                       place->type->type = $1->type + START_FUNCTION_TYPE;
                       place->type->size = $1->size;
                       place->returnCount = 0;
                       currentFunction = place;

                       $$ = allocateInstr();
                       $$->type->type = $1->type + START_FUNCTION_TYPE;
                       $$->type->size = $1->size;
                       $$->addr = strdup(place->internalName);
                   }
                 | VOID IDENTIFIER {
                       place = createSymbol($2);
                       place->type->type = VOID_FUNCTION_TYPE;
                       place->returnCount = 0;
                       currentFunction = place;

                       $$ = allocateInstr();
                       $$->type->type = VOID_FUNCTION_TYPE;
                       $$->addr = strdup(place->internalName);
                   }
                 ;

functionEnd : %empty {
                  emit("}");
            }

formals : %empty {
              $$ = allocateInstr();
              $$->type->type = VOID_TYPE;
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
                    strcat(addr, $1->addr + 1);

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
                    strcat(addr, $2->addr + 1);

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
                 strcat(addr, $1->addr + 1);

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
                 strcat(addr, $2->addr + 1);

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
              $$->type->type = VOID_TYPE;
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

instrIf : IF LPAREN instrIfCond RPAREN scopeStart instr scopeEnd instrIfEnd
        | IF LPAREN instrIfCond RPAREN scopeStart instr scopeEnd instrIfElse scopeStart instr scopeEnd instrIfElseEnd
        ;

instrIfCond : expr {
                  if ($1->type->type != BOOL_TYPE) {
                      yyerror("Non-compatible types: if");
                  } else {
                      labelStack[labelStackPointer].label1 = createTemporal();
                      labelStackLOC[labelStackPointer].label1 = nextLOC;

                      sprintf(str, "  br i1 %s, label %s, label ", $1->addr, labelStack[labelStackPointer].label1);
                      emit(str);

                      emit("");

                      sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label1 + 1);
                      emit(str);
                  }
              }
            ;

instrIfElse : ELSE {
                  labelStack[labelStackPointer].label2 = createTemporal();
                  labelStackLOC[labelStackPointer].label2 = nextLOC;

                  sprintf(str, "  br label ");
                  emit(str);

                  emit("");

                  sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label2 + 1);
                  emit(str);

                  sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label1]);
                  sprintf(str + strlen(str), "%s", labelStack[labelStackPointer].label2);
                  resultingCode[labelStackLOC[labelStackPointer].label1] = strdup(str);
              }
            ;

instrIfEnd : %empty %prec "then" {
                 labelStack[labelStackPointer].label2 = createTemporal();

                 sprintf(str, "  br label %s", labelStack[labelStackPointer].label2);
                 emit(str);

                 emit("");

                 sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label2 + 1);
                 emit(str);

                 sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label1]);
                 sprintf(str + strlen(str), "%s", labelStack[labelStackPointer].label2);
                 resultingCode[labelStackLOC[labelStackPointer].label1] = strdup(str);
             }
           ;

instrIfElseEnd : %empty {
                     labelStack[labelStackPointer].endLabel = createTemporal();

                     sprintf(str, "  br label %s", labelStack[labelStackPointer].endLabel);
                     emit(str);

                     emit("");

                     sprintf(str, "; <label>:%s", labelStack[labelStackPointer].endLabel + 1);
                     emit(str);

                     sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label2]);
                     sprintf(str + strlen(str), "%s", labelStack[labelStackPointer].endLabel);
                     resultingCode[labelStackLOC[labelStackPointer].label2] = strdup(str);
                 }
               ;

instrWhile : instrWhileStart WHILE LPAREN instrWhileCond RPAREN instr instrWhileEnd
           ;

instrWhileStart : %empty {
                      labelStack[labelStackPointer].startLabel = createTemporal();

                      sprintf(str, "  br label %s", labelStack[labelStackPointer].startLabel);
                      emit(str);

                      emit("");

                      sprintf(str, "; <label>:%s", labelStack[labelStackPointer].startLabel + 1);
                      emit(str);
                  }
                ;

instrWhileCond : expr {
                     if ($1->type->type != BOOL_TYPE) {
                         yyerror("Non-compatible types: while");
                     } else {
                         labelStack[labelStackPointer].label1 = createTemporal();
                         labelStackLOC[labelStackPointer].label1 = nextLOC;

                         sprintf(str, "  br i1 %s , label %s , label ", $1->addr, labelStack[labelStackPointer].label1);
                         emit(str);

                         emit("");

                         sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label1 + 1);
                         emit(str);
                     }
                 }
               ;

instrWhileEnd : %empty {
                    labelStack[labelStackPointer].endLabel = createTemporal();

                    sprintf(str, "  br label %s", labelStack[labelStackPointer].startLabel);
                    emit(str);

                    emit("");

                    sprintf(str, "; <label>:%s", labelStack[labelStackPointer].endLabel + 1);
                    emit(str);

                    sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label1]);
                    sprintf(str + strlen(str), "%s", labelStack[labelStackPointer].endLabel);
                    resultingCode[labelStackLOC[labelStackPointer].label1] = strdup(str);
                }
              ;

instrFor : FOR LPAREN optExpr instrForStart SEMICOLON instrForCond SEMICOLON optExpr RPAREN instrForLabel instr instrForEnd
         ;

instrForStart : %empty {
                    labelStack[labelStackPointer].startLabel = createTemporal();

                    sprintf(str, "  br label %s", labelStack[labelStackPointer].startLabel);
                    emit(str);

                    emit("");

                    sprintf(str, "; <label>:%s", labelStack[labelStackPointer].startLabel);
                    emit(str);
                }
              ;

instrForCond : expr {
                   if ($1->type->type != BOOL_TYPE) {
                       yyerror("Non-compatible types: for");
                   } else {
                       labelStack[labelStackPointer].label2 = createTemporal();
                       labelStackLOC[labelStackPointer].label2 = nextLOC;

                       sprintf(str, "  br i1 %s ", $1->addr);
                       emit(str);

                       emit("");

                       sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label2);
                       emit(str);
                   }
               }
             ;

instrForLabel : %empty {
                    labelStack[labelStackPointer].label1 = createTemporal();

                    sprintf(str, "  br label %s", labelStack[labelStackPointer].startLabel);
                    emit(str);

                    emit("");

                    sprintf(str, "; <label>:%s", labelStack[labelStackPointer].label1);
                    emit(str);

                    sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label2]);
                    sprintf(str + strlen(str), ", label %s", labelStack[labelStackPointer].label1);
                    resultingCode[labelStackLOC[labelStackPointer].label2] = strdup(str);
                }
              ;

instrForEnd : %empty {
                  labelStack[labelStackPointer].endLabel = createTemporal();

                  sprintf(str, "  br label %s", labelStack[labelStackPointer].label2);
                  emit(str);

                  emit("");

                  sprintf(str, "; <label>:%s", labelStack[labelStackPointer].endLabel);
                  emit(str);

                  sprintf(str, "%s", resultingCode[labelStackLOC[labelStackPointer].label2]);
                  sprintf(str + strlen(str), ", label %s", labelStack[labelStackPointer].endLabel);
                  resultingCode[labelStackLOC[labelStackPointer].label2] = strdup(str);
              }
            ;

instrReturn : RETURN optExpr SEMICOLON {
                  if ($2->type->type != currentFunction->type->type - START_FUNCTION_TYPE) {
                      yyerror("Invalid return function type");
                  } else {
                      currentFunction->returnCount++;

                      if ($2->type->type == VOID_TYPE) {
                          sprintf(str, "  ret void");
                      } else {
                          sprintf(str, "  ret %s %s", transformType($2->type), $2->addr);
                      }
                      emit(str);
                  }
              }
            ;

instrPrint : PRINT LPAREN printExprEnd RPAREN SEMICOLON
           ;

printExprEnd : expr {
                   generatePrints($1);
               }
             | printExpr expr {
                   generatePrints($2);
               }
             ;

printExpr : expr COLON {
                generatePrints($1);
            }
          | printExpr expr COLON {
                generatePrints($2);
            }
          ;

expr : lValue ASSIGN expr {
           if ($1 != NULL) {
               if (!canAssign($1->type, $3->type)) {
                   yyerror("Non-compatible types: =");
               } else {
                   if ($1->type->type == STRING_TYPE) {
                       int size = $1->type->size;
                       sprintf(str, "  store [%d x i8] %s , [%d x i8]* %s", size, convertString(size, $3->addr), size, $1->internalName);
                       emit(str);
                   } else {
                       const char* type = transformType($1->type);
                       sprintf(str, "  store %s %s , %s* %s", type, $3->addr, type, $1->internalName);
                       emit(str);
                   }
                   $$ = $3;
               }
           }
       }
     | lValue LBRACKET expr RBRACKET ASSIGN expr {
           if ($3->type->type != INT_TYPE) {
               yyerror("Array index is not an integer value");
           } else if (!isTypeArray($1->type)) {
               yyerror("Trying to access an index on a non-indexable type");
           } else if (!canAssignToArray($1->type, $6->type)) {
               yyerror("Non-compatible types: =");
           } else {
               int size = $1->type->size;
               char* temp = createTemporal();
               const char* arrayType = transformArrayType($1->type);
               sprintf(str, "  %s = getelementptr inbounds [%d x %s], [%d x %s]* %s, i32 0, i32 %s", temp, size, arrayType, size, arrayType, $1->internalName, $3->addr);
               emit(str);
               sprintf(str, "  store %s %s, %s* %s", arrayType, $6->addr, arrayType, temp);
               emit(str);

               $$ = allocateInstr();
               $$->type = copyType($6->type);
               $$->addr = temp;
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           char* temp = createTemporal();
           if ($1->type->type == STRING_TYPE) {
               int size = $1->type->size;
               sprintf(str, "  %s = getelementptr inbounds [%d x i8], [%d x i8]* %s, i32 0, i32 0", temp, size, size, $1->internalName);
               emit(str);
           } else {
               const char* type = transformType($1->type);
               sprintf(str, "  %s = load %s, %s* %s", temp, type, type, $1->internalName);
               emit(str);
           }

           $$ = allocateInstr();
           $$->type = ($1 == NULL ? '\0' : $1->type);
           $$->addr = temp;
       }
     | lValue LBRACKET expr RBRACKET {
           if ($3->type->type != INT_TYPE) {
               yyerror("Array index is not an integer value");
           } else if (!isTypeArray($1->type)) {
               yyerror("Trying to access an index on a non-indexable type");
           } else {
               $$ = allocateInstr();

               int size = $1->type->size;
               const char* arrayType = transformArrayType($1->type);
               char* temp1 = createTemporal();
               sprintf(str, "  %s = getelementptr inbounds [%d x %s], [%d x %s]* %s, i32 0, i32 %s", temp1, size, arrayType, size, arrayType, $1->internalName, $3->addr);
               emit(str);
               char* temp2 = createTemporal();
               sprintf(str, "  %s = load %s, %s* %s", temp2, arrayType, arrayType, temp1);
               emit(str);
               if ($1->type->type == STRING_TYPE) {
                   char* temp3 = createTemporal();
                   sprintf(str, "  %s = sext i8 %s to i32", temp3, temp2);
                   emit(str);

                   $$->type->type = INT_TYPE;
                   $$->type->size = $1->type->size;
                   $$->addr = temp3;
               } else {
                   $$->type->type = $1->type->type - START_ARRAY_TYPE;
                   $$->type->size = $1->type->size;
                   $$->addr = temp2;
               }
           }
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = add i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fadd double %s, %s", temp, $1->addr, $3->addr);
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
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = sub i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fsub double %s, %s", temp, $1->addr, $3->addr);
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
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = mul i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fmul double %s, %s", temp, $1->addr, $3->addr);
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
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = sdiv i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fdiv double %s, %s", temp, $1->addr, $3->addr);
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
           if ($1->type->type == INT_TYPE && $3->type->type == INT_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "  %s = srem i32 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type = $1->type;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr  expr");
           }
       }
     | expr LESS expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp slt i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp olt double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr < expr");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp sle i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp ole double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr <= expr");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp sgt i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp ogt double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr > expr");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp sge i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp oge double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr >= expr");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp eq i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp oeq double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr == expr");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1->type->type, $3->type->type)) {
               char* temp = createTemporal();

               if ($1->type->type == INT_TYPE) {
                   sprintf(str, "  %s = icmp ne i32 %s, %s", temp, $1->addr, $3->addr);
               } else {
                   sprintf(str, "  %s = fcmp one double %s, %s", temp, $1->addr, $3->addr);
               }

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr != expr");
           }
       }
     | expr AND expr {
           if ($1->type->type == BOOL_TYPE && $3->type->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "  %s = and i1 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr && expr");
           }
       }
     | expr OR expr {
           if ($1->type->type == BOOL_TYPE && $3->type->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "  %s = or i1 %s, %s", temp, $1->addr, $3->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: expr || expr");
           }
       }
     | NOT expr {
           if ($2->type->type == BOOL_TYPE) {
               char* temp = createTemporal();

               sprintf(str, "  %s = xor i1 %s, 1", temp, $2->addr);

               emit(str);

               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = temp;
           } else {
               yyerror("Non-compatible types: ! expr");
           }
       }
     | ADD expr %prec "uadd" {
           if ($2->type->type == INT_TYPE || $2->type->type == DOUBLE_TYPE) {
               $$ = $2;
           } else {
               yyerror("Non-compatible types: + expr");
           }
       }
     | SUB expr %prec "usub" {
           if ($2->type->type == INT_TYPE || $2->type->type == DOUBLE_TYPE) {
               char* temp = createTemporal();

               if ($2->type->type == INT_TYPE) {
                   sprintf(str, "  %s = sub i32 0, %s", temp, $2->addr);
               } else {
                   sprintf(str, "  %s = fsub double -0.0, %s", temp, $2->addr);
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

           readUsed[0] = TRUE;
           sprintf(str, "  %s = call i32 @readInt()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type->type = INT_TYPE;
           $$->addr = temp;
       }
     | READDOUBLE LPAREN RPAREN {
           char* temp = createTemporal();

           readUsed[1] = TRUE;
           sprintf(str, "  %s = call i32 @readDouble()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type->type = DOUBLE_TYPE;
           $$->addr = temp;
       }
     | READLINE LPAREN RPAREN {
           char* temp = createTemporal();

           readUsed[2] = TRUE;
           sprintf(str, "  %s = call i32 @readLine()", temp);
           emit(str);

           $$ = allocateInstr();
           $$->type->type = STRING_TYPE;
           $$->addr = temp;
       }
     ;

lValue : IDENTIFIER {
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
               sprintf(str, "  %s = call %s %s(%s)", temp, transformType(place->type), place->internalName, $3->addr);
               emit(str);
               $$->addr = temp;
           }

           $$->type->type = (place == NULL ? INVALID_TYPE : place->type->type - START_FUNCTION_TYPE);
       }
     ;

reals : %empty {
            $$ = allocateInstr();
            $$->type->type = VOID_TYPE;
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
               $$->type->type = DOUBLE_TYPE;
               $$->addr = $1;
           }
         | INTCONST {
               $$ = allocateInstr();
               $$->type->type = INT_TYPE;
               $$->addr = $1;
           }
         | BOOLCONST {
               $$ = allocateInstr();
               $$->type->type = BOOL_TYPE;
               $$->addr = $1;
           }
         | STRINGCONST {
               $$ = allocateInstr();
               $$->type->type = STRING_TYPE;
               $$->type->size = stringLength($1);
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

void generatePrints(struct t_instr* instr) {
    if (instr->type->type == DOUBLE_TYPE) {
        printUsed[1] = TRUE;
        sprintf(str, "  call void @printDouble(double %s)", instr->addr);
        emit(str);
    } else if (instr->type->type == INT_TYPE) {
        printUsed[0] = TRUE;
        sprintf(str, "  call void @printInt(i32 %s)", instr->addr);
        emit(str);
    } else if (instr->type->type == STRING_TYPE) {
        printUsed[2] = TRUE;
        char* addr = instr->addr;
        if (instr->addr[0] != '@' && instr->addr[0] != '%') {
            char* internalName = createStringConstant()->internalName;
            addr = createTemporal();

            sprintf(str,
                    "%s = internal constant [%d x i8] %s",
                    internalName,
                    instr->type->size,
                    convertString(instr->type->size, instr->addr));

            emitConstant(str);

            sprintf(str,
                    "  %s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0",
                    addr,
                    instr->type->size,
                    instr->type->size,
                    internalName);

            emit(str);
        }

        sprintf(str, "  call void @printString(i8* %s)", addr);
        emit(str);
    } else {
        sprintf(str, "Can\'t print type %s", convertType(instr->type));
        yyerror(str);
    }
}

int main(int argc, char **argv) {
    if(argc > 1) {
        if(!(yyin = fopen(argv[1], "r"))) {
            perror(argv[1]);
            return (1);
        }
    }

    initializeSymbolTable();

    yyparse();
    if (hasError) {
        #ifdef DEBUG
        printf("Printing symbols table\n");
        printSymbolTable();
        #endif //DEBUG

        return 1;
    } else {
        #ifdef DEBUG
        printf("Expression accepted\n");
        printf("Printing symbols table\n");
        printSymbolTable();
        #endif //DEBUG

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
