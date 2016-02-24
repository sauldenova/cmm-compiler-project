#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cmm.h"

int nnew, nold;
int nprobe;

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
    yyerror("Symbol wasn't found");
    exit(1);
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

