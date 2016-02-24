#ifndef __CMM_H_
#define __CMM_H_
/**
 * Constant definitions
 */
#define NHASH 10000

/**
 * Forward declarations
 */
struct t_symtab_list;

/**
 * Structure definitions
 */
// Defines a symbol
struct t_symbol {
    char *n;
    char t;
    int count;
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
struct t_symbol* place;
struct t_symtab* currSymTab;
struct t_symtab* rootSymTab;
int hasError;
int lineNumber;

/**
 * Functions
 */
int areNumeric(char type1, char type2);
struct t_symbol* createSymbol(char* name);
struct t_symbol* lookup(char* name);
struct t_typeexpr assignSymbol(struct t_symbol* sym, struct t_typeexpr expr);
void pushSymbolTable();
void popSymbolTable();
#endif

