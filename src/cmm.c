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

t_bool canAssign(struct t_type* type1, struct t_type* type2) {
    if (type1->type == STRING_TYPE && type2->type == STRING_TYPE) {
        return (type1->size >= type2->size);
    }

    if (isTypeArray(type1) && isTypeArray(type2)) {
        return (type1->type == type2->type && type1->size >= type2->size);
    }

    return (type1->type == type2->type);
}

t_bool canAssignToArray(struct t_type* type1, struct t_type* type2) {
    if (isTypeArray(type1) && !isTypeArray(type2)) {
        return ((type1->type - START_ARRAY_TYPE == type2->type) ||
                (type1->type == STRING_TYPE && type2->type == INT_TYPE));
    }

    return FALSE;
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
    struct t_symbol* sym = allocateSymbol();
    sym->name = strdup(name);
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
                if (strcmp(symList->symbol->name, name) == 0) {
                    symList->symbol->count++;
                    return symList->symbol;
                }

                symList = symList->next;
            }

            if (strcmp(symList->symbol->name, name) == 0) {
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
    labelStackPointer++;
}

void popSymbolTable() {
    currSymTab = currSymTab->parent;
    labelStackPointer--;
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
                   idx->symbol->name,
                   convertType(idx->symbol->type),
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

struct t_instr* allocateInstr() {
    struct t_instr* instr = (struct t_instr*)malloc(sizeof(struct t_instr));
    instr->type = allocateType();
    return instr;
}

struct t_arguments_list* allocateArgumentsList() {
    struct t_arguments_list* args = (struct t_arguments_list*)malloc(sizeof(struct t_arguments_list));
    args->type = allocateType();
    return args;
}

struct t_type* allocateType() {
    return (struct t_type*)malloc(sizeof(struct t_type));
}

struct t_symbol* allocateSymbol() {
    struct t_symbol* symbol = (struct t_symbol*)malloc(sizeof(struct t_symbol));
    symbol->type = allocateType();
    return symbol;
}

char* allocateString(int size) {
    return (char *)malloc(sizeof(char) * size);
}

struct t_type* copyType(struct t_type* type) {
    struct t_type* newType = allocateType();
    newType->type = type->type;
    newType->size = type->size;
    return newType;
}

void appendEscapeCode(char* targetString, char secondCharacter) {
    *targetString = '\\';
    targetString++;
    switch(secondCharacter) {
        case 'a':
            *targetString = '0';
            targetString++;
            *targetString = '7';
            targetString++;
            break;
        case 'b':
            *targetString = '0';
            targetString++;
            *targetString = '8';
            targetString++;
            break;
        case 'f':
            *targetString = '0';
            targetString++;
            *targetString = 'C';
            targetString++;
            break;
        case 'n':
            *targetString = '0';
            targetString++;
            *targetString = 'A';
            targetString++;
            break;
        case 'r':
            *targetString = '0';
            targetString++;
            *targetString = 'D';
            targetString++;
            break;
        case 't':
            *targetString = '0';
            targetString++;
            *targetString = '9';
            targetString++;
            break;
        case 'v':
            *targetString = '0';
            targetString++;
            *targetString = 'B';
            targetString++;
            break;
        case '\\':
            *targetString = '5';
            targetString++;
            *targetString = 'C';
            targetString++;
            break;
        case '\'':
            *targetString = '2';
            targetString++;
            *targetString = '7';
            targetString++;
            break;
        case '\"':
            *targetString = '2';
            targetString++;
            *targetString = '2';
            targetString++;
            break;
        case '\?':
            *targetString = '3';
            targetString++;
            *targetString = 'F';
            targetString++;
            break;
        case '0':
            *targetString = '0';
            targetString++;
            *targetString = '0';
            targetString++;
            break;
    }
}

char* convertString(int length, char* str) {
    char* result = allocateString((3 * length) + 10);
    char* ptr = str;
    char* resPtr = result;
    *resPtr = 'c';
    resPtr++;
    *resPtr = '\"';
    resPtr++;
    while (*ptr) {
        if (*ptr == '\\') {
            ptr++;
            appendEscapeCode(resPtr, *ptr);
            resPtr += 2;
        } else {
            *resPtr = *ptr;
        }

        resPtr++;
        ptr++;
        length--;
    }

    while(length--) {
        *resPtr = '\\';
        resPtr++;
        *resPtr = '0';
        resPtr++;
        *resPtr = '0';
        resPtr++;
    }

    *resPtr = '\"';
    resPtr++;
    *resPtr = '\0';
    resPtr++;

    return result;
}

int stringLength(char* str) {
    int count = 0;
    char* ptr = str;
    while(*ptr) {
        if (*ptr == '\\') {
            ptr++;
        }

        ptr++;
        count++;
    }

    return count;
}

void emitHeader() {
    emit("declare void @printInt(i32)");
    emit("declare void @printDouble(double)");
    emit("declare void @printString(i8*)");
    emit("declare i32 @readInt()");
    emit("declare double @readDouble()");
    emit("declare i8* @readLine()");
}

