#ifndef __CMM_H__
#define __CMM_H__

#include <stdio.h>

#include "cmm_types.h"

/**
 * Constant definitions
 */
#define NHASH   10000

/**
 * Forward declarations
 */
struct t_symtab_list;

/**
 * Structure definitions
 */
// Defines an argument list
struct t_arguments_list {
    struct t_arguments_list* next;
    t_type type;
};

struct t_instr {
    t_type type;
    char* addr;
    struct t_arguments_list* args;
};

// Defines a symbol
struct t_symbol {
    char *n;
    char t;
    int count;
    int returnCount;
    struct t_arguments_list* arguments;
};

// Defines a symbol list for the symbol table
struct t_symbol_list {
    struct t_symbol_list* next;
    struct t_symbol* symbol;
};

// Defines a recursive symbol table
struct t_symtab {
    struct t_symtab* parent;
    struct t_symtab_list* children;
    struct t_symbol_list* symbols[NHASH];
};

// Defines a recursive symbol table list
struct t_symtab_list {
    struct t_symtab* elem;
    struct t_symtab_list* next;
};

/**
 * Variables
 */
char str[200];
struct t_symbol* place;
struct t_symbol* currentFunction;
struct t_symtab* currSymTab;
struct t_symtab* rootSymTab;
int lineNumber;
t_bool hasError;
char* startLabel;
char* label1;
char* label2;
char* endLabel;

/**
 * Functions
 */
t_bool areNumeric(char type1, char type2);
struct t_symbol* createSymbol(char* name);
struct t_symbol* lookup(char* name);
struct t_typeexpr assignSymbol(struct t_symbol* sym, struct t_typeexpr expr);
void pushSymbolTable();
void popSymbolTable();
void emit(char* code);
void emitHeader();
void writeCodeToFile(FILE* outputFile);
char* createTemporal();
char* createLabel();
struct t_instr* allocateInstr();
struct t_arguments_list* allocateArgumentsList();
char* allocateString(int size);

#endif // __CMM_H__

