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
%left OR
%left AND
%left EQUAL NEQUAL
%left LESS LESSEQ GREATER GREATEREQ
%left ADD SUB
%left MUL DIV MOD
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
         | declVariable { }
         | instr { }
         | blockDef declVariable { }
         | blockDef instr { }
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

instrIf : IF LPAREN RPAREN { }
        | IF LPAREN expr RPAREN { }
        | IF LPAREN RPAREN ELSE instr { }
        | IF LPAREN expr RPAREN ELSE instr { }
        ;

instrWhile : WHILE LPAREN expr RPAREN instr { }
           ;

instrFor : FOR LPAREN SEMICOLON expr SEMICOLON RPAREN instr { }
         | FOR LPAREN SEMICOLON expr SEMICOLON expr RPAREN instr { }
         | FOR LPAREN expr SEMICOLON expr SEMICOLON RPAREN instr { }
         | FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN instr { }
         ;

instrReturn : RETURN SEMICOLON { }
            | RETURN expr SEMICOLON { }
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
     | SUB expr { }
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

realsDef : expr { }
         | realsDef expr { }
         ;

constant : DOUBLECONST { }
         | INTCONST { }
         | BOOLCONST { }
         | STRINGCONST { }
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
