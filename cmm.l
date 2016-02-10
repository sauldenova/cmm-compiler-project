%{
#include "cmm.tab.h"
%}

%%
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
"true"                          { yylval = 1; return(BOOLCONST); }
"false"                         { yylval = 0; return(BOOLCONST); }
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
"/*"                            { return(LLONGCOMMENT); }
"*/"                            { return(RLONGCOMMENT); }
"//"                            { return(COMMENT); }
[\"][^\n"]*[\"]                 { return(STRING); }
[a-zA-Z][a-zA-Z0-9_]*           { return(IDENTIFIER); }
[0-9]+.[0-9]+                   { yylval = atof(yytext); return(DOUBLECONST); }
[0-9]+.[0-9]+[E+][0-9]+         { yylval = atof(yytext); return(DOUBLECONST); }
[0][xX][0-9A-Fa-f]+             { yylval = atoi(yytext); return(INTCONST); }
[0-9]+                          { yylval = atoi(yytext); return(INTCONST); }
.                               { }
<<EOF>>                         { return(0); }
%%
