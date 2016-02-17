/**
 * Constant definitions
 */
#define NHASH 10000

/**
 * Structure definitions
 */
typedef struct {
    char t;
    double d;
    int i;
    char* s;
} t_typeexpr;

typedef struct {
    char *n;
    t_typeexpr te;
} t_symbol;

/**
 * Variables
 */
t_symbol* place;
t_symbol symtab[NHASH];

/**
 * Functions
 */
int areNumeric(char type1, char type2);
t_symbol* lookup(char*);

