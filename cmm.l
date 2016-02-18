%option noyywrap

%{
#include "cmm.h"
#include "cmm.tab.h"
%}

%x C_COMMENT

%%

"/*"                            { BEGIN(C_COMMENT); }
<C_COMMENT>"*/"                 { BEGIN(INITIAL); }
<C_COMMENT>.|\n                 { }
<C_COMMENT><<EOF>>              { yyerror("Unfinished comment"); return(0); }
"//".*"\n"                      { }
"void"                          { return(VOID); }
"int"                           { return(INT); }
"double"                        { return(DOUBLE); }
"bool"                          { return(BOOL); }
"string"                        { return(STRING); }
"while"                         { return(WHILE); }
"for"                           { return(FOR); }
"if"                            { return(IF); }
"else"                          { return(ELSE); }
"return"                        { return(RETURN); }
"print"                         { return(PRINT); }
"readint"                       { return(READINT); }
"readline"                      { return(READLINE); }
"true"                          { yylval.b = 1; return(BOOLCONST); }
"false"                         { yylval.b = 0; return(BOOLCONST); }
"+"                             { return(ADD); }
"-"                             { return(SUB); }
"*"                             { return(MUL); }
"/"                             { return(DIV); }
"%"                             { return(MOD); }
"<"                             { return(LESS); }
"<="                            { return(LESSEQ); }
">"                             { return(GREATER); }
">="                            { return(GREATEREQ); }
"="                             { return(ASSIGN); }
"=="                            { return(EQUAL); }
"!="                            { return(NEQUAL); }
"&&"                            { return(AND); }
"||"                            { return(OR); }
"!"                             { return(NOT); }
";"                             { return(SEMICOLON); }
","                             { return(COLON); }
"["                             { return(LBRACKET); }
"]"                             { return(RBRACKET); }
"("                             { return(LPAREN); }
")"                             { return(RPAREN); }
"{"                             { return(LBRACE); }
"}"                             { return(RBRACE); }
["][^\n"]*["]                   { if (strlen(yytext) > 31) yyerror("Variable length exceeds the limit of 31 characters"); else { yylval.s = strdup(yytext); return(STRINGCONST); } }
["][^\n"]*[\n]                  { yyerror("Unfinished string"); }
[a-zA-Z][a-zA-Z0-9_]*           { yylval.s = strdup(yytext); return(IDENTIFIER); }
[-+]?[0-9]+\.[0-9]+             { yylval.d = atof(yytext); return(DOUBLECONST); }
[-+]?[0-9]+\.[0-9]+E[-+]?[0-9]+ { yylval.d = atof(yytext); return(DOUBLECONST); }
[-+]?[0][xX][0-9A-Fa-f]+        { yylval.i = atoi(yytext); return(INTCONST); }
[-+]?[0-9]+                     { yylval.i = atoi(yytext); return(INTCONST); }
[ \n\t\r]                       { }
.                               { yyerror("Invalid character"); }
<<EOF>>                         { return(0); }
%%
