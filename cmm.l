%option noyywrap

%{
#include "cmm.h"
#include "cmm.tab.h"
%}

%x C_COMMENT

%%

"/*"                                { BEGIN(C_COMMENT); }
<C_COMMENT>"*/"                     { BEGIN(INITIAL); }
<C_COMMENT>[^\n]                    { }
<C_COMMENT>\n                       { lineNumber++; }
<C_COMMENT><<EOF>>                  { yyerror("Unfinished comment");
                                      return(0);
                                    }
"//".*"\n"                          { lineNumber++; }
"void"                              { return(VOID); }
"int"                               { return(INT); }
"double"                            { return(DOUBLE); }
"bool"                              { return(BOOL); }
"string"                            { return(STRING); }
"while"                             { return(WHILE); }
"for"                               { return(FOR); }
"if"                                { return(IF); }
"else"                              { return(ELSE); }
"return"                            { return(RETURN); }
"print"                             { return(PRINT); }
"readint"                           { return(READINT); }
"readline"                          { return(READLINE); }
"true"                              { return(BOOLCONST); }
"false"                             { return(BOOLCONST); }
"+"                                 { return(ADD); }
"-"                                 { return(SUB); }
"*"                                 { return(MUL); }
"/"                                 { return(DIV); }
"%"                                 { return(MOD); }
"<"                                 { return(LESS); }
"<="                                { return(LESSEQ); }
">"                                 { return(GREATER); }
">="                                { return(GREATEREQ); }
"="                                 { return(ASSIGN); }
"=="                                { return(EQUAL); }
"!="                                { return(NEQUAL); }
"&&"                                { return(AND); }
"||"                                { return(OR); }
"!"                                 { return(NOT); }
";"                                 { return(SEMICOLON); }
","                                 { return(COLON); }
"["                                 { return(LBRACKET); }
"]"                                 { return(RBRACKET); }
"("                                 { return(LPAREN); }
")"                                 { return(RPAREN); }
"{"                                 { return(LBRACE); }
"}"                                 { return(RBRACE); }
["][^\n"]*["]                       { yylval.id = strdup(yytext);
                                      return(STRINGCONST);
                                    }
["][^\n"]*[\n]                      { yyerror("Unfinished string"); lineNumber++; }
[0-9]+\.[0-9]+                      { return(DOUBLECONST); }
[0-9]+[\.]?[0-9]*[eE][-+]?[0-9]+    { return(DOUBLECONST); }
[0][xX][0-9A-Fa-f]+                 { return(INTCONST); }
[0-9]+                              { return(INTCONST); }
[a-zA-Z][a-zA-Z0-9_]*               { if (strlen(yytext) > 31) {
                                          yyerror("Variable length exceeds the limit of 31 characters");
                                      } else {
                                          yylval.id = strdup(yytext);
                                          return(IDENTIFIER);
                                      }
                                    }
[ \r\t]                             { }
\n                                  { lineNumber++; }
.                                   { yyerror("Invalid character"); }
<<EOF>>                             { return(0); }
%%
