#include "cmm.h"
#include "cmm_types.h"

t_bool isTypeArray(t_type type) {
    return (INT_ARRAY_TYPE <= type && type < INT_FUNCTION_TYPE);
}
t_bool isTypeFunction(t_type type) {
    return (INT_FUNCTION_TYPE <= type);
}
const char* convertType(t_type type) {
    switch(type) {
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

