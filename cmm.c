#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cmm.h"
#include "cmm_types.h"

int nnew, nold;
int nprobe;
int temporalCount;
int labelCount;
int nextStat;
char* resultingCode[1000];

t_bool areNumeric(char type1, char type2) {
    return ((type1 == INT_TYPE || type1 == DOUBLE_TYPE) && type1 == type2);
}

static unsigned symhash(char *sym) {
    unsigned int hash = 0;
    unsigned c;

    while((c = *sym++)) {
        hash = hash * 9 ^ c;
    }

    return hash;
}

struct t_symbol* createSymbol(char* name) {
    struct t_symbol_list* symList = currSymTab->symbols[symhash(name) % NHASH];
    if (symList != NULL) {
        while (symList->next != NULL) {
            symList = symList->next;
        }
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
    char str[100];
    sprintf(str, "Symbol %s wasn't found", name);
    yyerror(&str);
    return NULL;
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

            printf("|Name: %-16s|Type: %-3s  |Count: %3d|\n",
                   idx->symbol->n,
                   convertType(idx->symbol->t),
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

t_bool verifyArguments(struct t_arguments_list* args1, struct t_arguments_list* args2) {
    while(args1 != NULL && args2 != NULL) {
        if (args1->type != args2->type) {
            return FALSE;
        }
        args1 = args1->next;
        args2 = args2->next;
    }

    return (args1 == NULL && args2 == NULL);
}

void initializeSymbolTable() {
    // Create and set new symbol table
    struct t_symtab* symTab = malloc(sizeof(struct t_symtab));
    symTab->parent = NULL;

    // Set the environment variables
    currSymTab = symTab;
    rootSymTab = symTab;
}

char* createTemporal() {
    char* temporal = malloc(5 * sizeof(char));
    sprintf(temporal, "%%t%d", temporalCount++);
    return temporal;
}

char* createLabel() {
    char* temporal = malloc(7 * sizeof(char));
    sprintf(temporal, "lab%d", labelCount++);
    return temporal;
}

void emit(char* code) {
    resultingCode[nextStat++] = strdup(code);
}

void writeCodeToFile(FILE* outputFile) {
    for(int i = 0; i < nextStat; i++) {
        fprintf(outputFile, "%s\n", resultingCode[i]);
    }
}

