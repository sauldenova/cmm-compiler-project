#include <string.h>

#include "cmm.h"
#include "cmm_types.h"

t_bool isTypeArray(struct t_type* type) {
    return (INT_ARRAY_TYPE <= type->type && type->type < INT_FUNCTION_TYPE);
}

t_bool isTypeFunction(struct t_type* type) {
    return (INT_FUNCTION_TYPE <= type->type);
}

const char* convertType(struct t_type* type) {
    switch(type->type) {
        case INT_TYPE:
            return "I";
        case BOOL_TYPE:
            return "B";
        case DOUBLE_TYPE:
            return "D";
        case STRING_TYPE:
            return "S";
        case VOID_TYPE:
            return "V";
        case INT_ARRAY_TYPE:
            return "AI";
        case BOOL_ARRAY_TYPE:
            return "AB";
        case DOUBLE_ARRAY_TYPE:
            return "AD";
        case INT_FUNCTION_TYPE:
            return "FI";
        case BOOL_FUNCTION_TYPE:
            return "FB";
        case DOUBLE_FUNCTION_TYPE:
            return "FD";
        case STRING_FUNCTION_TYPE:
            return "FS";
        case VOID_FUNCTION_TYPE:
            return "FV";
        case INT_ARRAY_FUNCTION_TYPE:
            return "FAI";
        case BOOL_ARRAY_FUNCTION_TYPE:
            return "FAB";
        case DOUBLE_ARRAY_FUNCTION_TYPE:
            return "FAD";
        default:
            return "INV";
    }
}

const char* transformType(struct t_type* type) {
    switch(type->type) {
        case INT_FUNCTION_TYPE:
        case INT_TYPE:
            return "i32";
        case BOOL_FUNCTION_TYPE:
        case BOOL_TYPE:
            return "i1";
        case DOUBLE_FUNCTION_TYPE:
        case DOUBLE_TYPE:
            return "double";
        case STRING_FUNCTION_TYPE:
        case STRING_TYPE:
            return "i8*";
        case VOID_FUNCTION_TYPE:
        case VOID_TYPE:
            return "void";
        default:
            return "";
    }
}

const char* transformArrayType(struct t_type* type) {
    switch(type->type) {
        case INT_ARRAY_FUNCTION_TYPE:
        case INT_ARRAY_TYPE:
            return "i32";
        case BOOL_ARRAY_FUNCTION_TYPE:
        case BOOL_ARRAY_TYPE:
            return "i1";
        case DOUBLE_ARRAY_FUNCTION_TYPE:
        case DOUBLE_ARRAY_TYPE:
            return "double";
        case STRING_FUNCTION_TYPE:
        case STRING_TYPE:
            return "i8";
        default:
            return "";
    }
}
