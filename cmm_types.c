#include "cmm.h"
#include "cmm_types.h"

cmm_bool isTypeArray(cmm_type type) {
    return (INT_ARRAY_TYPE <= type && type < INT_FUNCTION_TYPE);
}
cmm_bool isTypeFunction(cmm_type type) {
    return (INT_FUNCTION_TYPE <= type);
}

