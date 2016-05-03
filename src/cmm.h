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
    struct t_type* type;
};

// Defines an instruction
struct t_instr {
    struct t_type* type;
    char* addr;
    struct t_arguments_list* args;
};

// Defines a symbol
struct t_symbol {
    int count;
    int returnCount;
    char* name;
    char* internalName;
    struct t_type* type;
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

struct t_labels {
    char* startLabel;
    char* label1;
    char* label2;
    char* endLabel;
};

/**
 * Variables
 */
char str[10000];
int labelStackPointer;
struct t_labels labelStack[100];
struct t_symbol* place;
struct t_symbol* currentFunction;
struct t_symtab* currSymTab;
struct t_symtab* rootSymTab;
int lineNumber;
t_bool hasError;
t_bool printUsed[3];
t_bool readUsed[3];

/**
 * Functions
 */
t_bool areNumeric(char type1, char type2);
t_bool canAssign(struct t_type* type1, struct t_type* type2);
t_bool canAssignToArray(struct t_type* type1, struct t_type* type2);
struct t_symbol* createSymbol(char* name);
struct t_symbol* createStringConstant();
struct t_symbol* lookup(char* name);
struct t_typeexpr assignSymbol(struct t_symbol* sym, struct t_typeexpr expr);
void pushSymbolTable();
void popSymbolTable();
void emitConstant(char* code);
void emit(char* code);
void writeCodeToFile(FILE* outputFile);
char* createTemporal();
char* createLabel();
struct t_instr* allocateInstr();
struct t_type* allocateType();
struct t_arguments_list* allocateArgumentsList();
struct t_symbol* allocateSymbol();
struct t_type* copyType(struct t_type* type);
char* allocateString(int size);
char* convertString(int length, char* str);
int stringLength(char* str);

#endif // __CMM_H__

