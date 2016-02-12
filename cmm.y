%{
#include <stdio.h>
%}

%token INTCONST DOUBLECONST BOOLCONST STRINGCONST
%token VOID INT DOUBLE BOOL STRING WHILE FOR IF ELSE RETURN PRINT READINT READLINE
%token ADD SUB MUL DIV MOD ASSIGN
%token LESS LESSEQ GREATER GREATEREQ EQUAL NEQUAL
%token AND OR NOT
%token SEMICOLON COLON
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMENT LLONGCOMMENT RLONGCOMMENT
%token IDENTIFIER

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

%%

start : decl { }
      | start decl { }
      ;

decl : declVariable { }
     | declFunction { }
     ;

declVariable : variable SEMICOLON { }
             ;

variable : type IDENTIFIER { }
         ;

type : INT { }
     | DOUBLE { }
     | BOOL { }
     | STRING LBRACKET INTCONST RBRACKET { }
     | type LBRACKET INTCONST RBRACKET { }
     ;

declFunction : type IDENTIFIER LPAREN formals RPAREN block { }
             | VOID IDENTIFIER LPAREN formals RPAREN block { }
             ;

formals : /* empty */ { }
         | formalsDef { }
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
        | expr { }
        ;

instr : SEMICOLON { }
      | expr SEMICOLON {}
      | instrIf { }
      | instrWhile { }
      | instrFor { }
      | instrReturn { }
      | instrPrint { }
      | block { }
      ;

instrIf : IF LPAREN optExpr RPAREN instrElse { }
        ;

instrElse : /* empty */ { }
          | ELSE instr { }
          ;

instrWhile : WHILE LPAREN expr RPAREN instr { }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr { }
         ;

instrReturn : RETURN optExpr SEMICOLON { }
            ;

instrPrint : PRINT LPAREN instrPrintDef RPAREN SEMICOLON { }
           ;

instrPrintDef : expr COLON { }
              | instrPrintDef expr COLON { }
              ;

expr : lValue ASSIGN expr { }
     | constant { }
     | lValue { }
     | call { }
     | LPAREN expr RPAREN { }
     | expr ADD expr { }
     | expr SUB expr { }
     | expr MUL expr { }
     | expr DIV expr { }
     | expr MOD expr { }
     | expr LESS expr { }
     | expr LESSEQ expr { }
     | expr GREATER expr { }
     | expr GREATEREQ expr { }
     | expr EQUAL expr { }
     | expr NEQUAL expr { }
     | expr AND expr { }
     | expr OR expr { }
     | NOT expr { }
     | READINT LPAREN RPAREN { }
     | READLINE LPAREN RPAREN { }
     ;

lValue : IDENTIFIER
       | expr LBRACKET expr RBRACKET { }
       ;

call : IDENTIFIER LPAREN reals RPAREN { }
     ;

reals : realsDef { }
      | /* empty */ { }
      ;

realsDef : expr COLON { }
         | realsDef expr COLON { }
         ;

constant : sign DOUBLECONST { }
         | sign INTCONST { }
         | BOOLCONST { }
         | STRINGCONST { }
         ;

sign : /* empty */ { }
     | SUB { }
     ;

%%

int main(int argc, char **argv)
{
    yyparse();
    printf("Expresion aceptada \n");
    return 0;
}

yyerror(char *s)
{
    fprintf(stderr,"error: %s\n", s);
}
