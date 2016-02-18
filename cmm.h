#ifndef __CMM_H_
#define __CMM_H_
/**
 * Constant definitions
 */
#define NHASH 10000

/**
 * Structure definitions
 */
struct t_typeexpr {
    char t;
    double d;
    int i;
    char* s;
};

struct t_symbol {
    char *n;
    struct t_typeexpr te;
};

struct t_char_list {
    char *elem;
    struct t_char_list *next;
};

/**
 * Variables
 */
struct t_symbol* place;
struct t_symbol symtab[NHASH];
struct t_char_list* symbolList;

/**
 * Functions
 */
int areNumeric(char type1, char type2);
struct t_symbol* lookup(char*);
struct t_typeexpr assignSymbol(struct t_symbol* sym, struct t_typeexpr expr);
#endif

