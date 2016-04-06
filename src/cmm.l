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
"printString"                       { return(PRINTSTRING); }
"printInt"                          { return(PRINTINT); }
"printDouble"                       { return(PRINTDOUBLE); }
"readInt"                           { return(READINT); }
"readDouble"                        { return(READDOUBLE); }
"readLine"                          { return(READLINE); }
"true"                              { yylval.val = strdup(yytext);
                                      return(BOOLCONST);
                                    }
"false"                             { yylval.val = strdup(yytext);
                                      return(BOOLCONST);
                                    }
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
["][^\n"]*["]                       { str[0] = '\0';
                                      strcat(str, yytext + 1);
                                      int len = strlen(str);
                                      if (str[len - 3] == '\\' && str[len - 2] == '0') {
                                          str[len - 1] = '\0';
                                      } else {
                                          str[len - 1] = '\\';
                                          str[len] = '0';
                                          str[len + 1] = '\0';
                                      }
                                      yylval.id = strdup(str);
                                      return(STRINGCONST);
                                    }
["][^\n"]*[\n]                      { yyerror("Unfinished string");
                                      lineNumber++;
                                    }
[0-9]+\.[0-9]+                      { double f = strtod(yytext, NULL);
                                      sprintf(str, "%#16llx", *( (long long*) &f ));
                                      yylval.val = strdup(str);
                                      return(DOUBLECONST);
                                    }
[0-9]+[\.]?[0-9]*[eE][-+]?[0-9]+    { double f = strtod(yytext, NULL);
                                      sprintf(str, "%#16llx", *( (long long*) &f ));
                                      yylval.val = strdup(str);
                                      return(DOUBLECONST);
                                    }
[0][xX][0-9A-Fa-f]+                 { long int val = strtol(yytext, NULL, 16);
                                      sprintf(str, "%ld", val);
                                      yylval.val = strdup(str);
                                      return(INTCONST);
                                    }
[0-9]+                              { yylval.val = strdup(yytext);
                                      return(INTCONST);
                                    }
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
