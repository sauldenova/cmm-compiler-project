%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cmm.h"
extern FILE *yyin;
%}

%define parse.lac full
%define parse.error verbose

%union {
    struct t_typeexpr typeexpr;
    struct t_symbol symbol;
    double d;
    int i;
    int b;
    char t;
    char* id;
    char* s;
}

%token <i> INTCONST
%token <d> DOUBLECONST
%token <i> BOOLCONST
%token <s> STRINGCONST
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

%type <i> sign
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
               place = lookup($2);
               place->t = $1;
           }
         ;

type : INT {
           $$ = 'I';
       }
     | DOUBLE {
           $$ = 'D';
       }
     | BOOL {
           $$ = 'B';
       }
     | STRING LBRACKET INTCONST RBRACKET {
           $$ = 'S';
       }
     | type LBRACKET INTCONST RBRACKET {
           $$ = 'A';
       }
     ;

declFunction : type IDENTIFIER scopeStart LPAREN formals RPAREN block scopeEnd {
                   place = lookup($2); place->t = $1;
               }
             | VOID IDENTIFIER scopeStart LPAREN formals RPAREN block scopeEnd {
                   place = lookup($2); place->t = 'V';
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

optExpr : %empty
        | expr {
              $$ = $1;
          }
        ;

instr : SEMICOLON
      | expr SEMICOLON
      | scopeStart instrIf scopeEnd
      | scopeStart instrWhile scopeEnd
      | scopeStart instrFor scopeEnd
      | instrReturn
      | instrPrint
      | block
      ;

instrIf : IF LPAREN expr RPAREN instr instrElse {
              if ($3 != 'B') {
                  yyerror("Non-compatible types: if");
              }
          }
        ;

instrElse : %empty %prec "then"
          | ELSE instr
          ;

instrWhile : WHILE LPAREN expr RPAREN instr {
                 if ($3 != 'B') {
                     yyerror("Non-compatible types: while");
                 }
             }
           ;

instrFor : FOR LPAREN optExpr SEMICOLON expr SEMICOLON optExpr RPAREN instr { 
               if ($5 != 'B') {
                   yyerror("Non-compatible types"); 
               }
           }
         ;

instrReturn : RETURN optExpr SEMICOLON
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
           if ($1.t != $3) {
               yyerror("Non-compatible types: =");
           }
       }
     | constant {
           $$ = $1;
       }
     | lValue {
           $$ = $1.t;
       }
     | call {
           $$ = $1;
       }
     | LPAREN expr RPAREN {
           $$ = $2;
       }
     | expr ADD expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: +");
           }
       }
     | expr SUB expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: -");
           }
       }
     | expr MUL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: *");
           }
       }
     | expr DIV expr {
           if (areNumeric($1, $3) != 0) {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: /");
           }
       }
     | expr MOD expr {
           if ($1 == 'I' && $3 == 'I') {
               $$ = $1;
           } else {
               yyerror("Non-compatible types: %");
           }
       }
     | expr LESS expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: <");
           }
       }
     | expr LESSEQ expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: <=");
           }
       }
     | expr GREATER expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: >");
           }
       }
     | expr GREATEREQ expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: >=");
           }
       }
     | expr EQUAL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: ==");
           }
       }
     | expr NEQUAL expr {
           if (areNumeric($1, $3) != 0) {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: !=");
           }
       }
     | expr AND expr {
           if ($1 == 'B' && $3 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: &&");
           }
       }
     | expr OR expr {
           if ($1 == 'B' && $3 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: ||");
           }
       }
     | NOT expr {
           if ($2 == 'B') {
               $$ = 'B';
           } else {
               yyerror("Non-compatible types: !");
           }
       }
     | READINT LPAREN RPAREN {
           $$ = 'I';
       }
     | READLINE LPAREN RPAREN {
           $$ = 'S';
       }
     ;

lValue : IDENTIFIER {
             $$ = *lookup($1);
         }
       | IDENTIFIER LBRACKET expr RBRACKET {
             $$ = *lookup($1);
         }
       ;

call : IDENTIFIER LPAREN reals RPAREN {
           $$ = lookup($1)->t;
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
               $$ = 'D';
           }
         | INTCONST {
               $$ = 'I';
           }
         | BOOLCONST {
               $$ = 'B';
           }
         | STRINGCONST {
               $$ = 'S';
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

int areNumeric(char type1, char type2) {
    if ((type1 == 'I' || type1 == 'D') && type1 == type2) {
        return 1;
    }

    return 0;
}

static unsigned symhash(char *sym) {
    unsigned int hash = 0;
    unsigned c;

    while((c = *sym++)) {
        hash = hash * 9 ^ c;
    }

    return hash;
}

int nnew, nold;
int nprobe;
struct t_symbol* lookup(char* name) {
    struct t_symtab* symtab = currSymTab;
    struct t_symbol_list* symList;
    while (symtab != NULL) {
        symList = symtab->symbols[symhash(name) % NHASH];
        if (symList != NULL) {
            while (symList->next != NULL) {
                if (strcmp(symList->symbol->n, name) == 0) {
                    symList->symbol->count++;
                    return symList->symbol;
                }

                symList = symList->next;
            }

            if (strcmp(symList->symbol->n, name) == 0) {
                symList->symbol->count++;
                return symList->symbol;
            }
        }

        symtab = symtab->parent;
    }

    // Symbol wasn't found, therefore it doesn't exist
    struct t_symbol* sym = malloc(sizeof(struct t_symbol));
    sym->n = strdup(name);
    sym->count = 1;

    struct t_symbol_list* nextSymList = malloc(sizeof(struct t_symbol_list));
    nextSymList->next = NULL;
    nextSymList->symbol = sym;

    if (symList != NULL) {
        symList->next = nextSymList;
    } else {
        currSymTab->symbols[symhash(name) % NHASH] = nextSymList;
    }

    return sym;
}

void pushSymbolTable() {
    // Create and set new symbol table
    struct t_symtab* symTab = malloc(sizeof(struct t_symtab));
    symTab->parent = currSymTab;

    // Add to children list
    struct t_symtab_list* newList = malloc(sizeof(struct t_symtab_list));
    newList->elem = symTab;
    newList->next = currSymTab->children;
    currSymTab->children = newList;

    // Set current symbol table as the new symbol table
    currSymTab = symTab;
}

void popSymbolTable() {
    currSymTab = currSymTab->parent;
}

void _traverseSymbolTable(struct t_symtab* symtab, int depth) {
    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    printf("Symbol table:\n");

    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    for (int i = 0; i < 47; i++) {
        printf("-");
    }
    printf("\n");

    for (int i = 0; i < NHASH; i++) {
        for (struct t_symbol_list *idx = symtab->symbols[i]; idx != NULL; idx = idx->next) {
            for (int i = 0; i < depth; i++) {
                printf("  ");
            }

            printf("|Name: %-16s|Type: %c    |Count: %3d|\n",
                   idx->symbol->n,
                   idx->symbol->t,
                   idx->symbol->count);
        }
    }

    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    for (int i = 0; i < 47; i++) {
        printf("-");
    }
    printf("\n");

    printf("\n");

    for (struct t_symtab_list *idx = symtab->children; idx != NULL; idx = idx->next) {
        _traverseSymbolTable(idx->elem, depth + 1);
    }
}

void printSymbolTable() {
    _traverseSymbolTable(rootSymTab, 0);
}

void initializeSymbolTable() {
    // Create and set new symbol table
    struct t_symtab* symTab = malloc(sizeof(struct t_symtab));
    symTab->parent = NULL;

    // Set the environment variables
    currSymTab = symTab;
    rootSymTab = symTab;
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
    if (hasError != 0) {
        return 1;
    } else {
        printf("Expression accepted\nPrinting symbols table\n");
        printSymbolTable();
        return 0;
    }
}

int yyerror(char *s) {
    fprintf(stderr,"Error: %s\n", s);
    hasError = 1;
    return 0;
}
