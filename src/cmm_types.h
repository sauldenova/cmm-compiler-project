#ifndef __CMM_TYPES_H__
#define __CMM_TYPES_H__

typedef char t_bool;

#define TRUE 1
#define FALSE 0

// Type structure definition
struct t_type {
    char type;
    short size;
};

// Define types
#define INT_TYPE                0
#define BOOL_TYPE               1
#define DOUBLE_TYPE             2
#define VOID_TYPE               3

// Define start array number
#define START_ARRAY_TYPE        10

// Define array types
#define INT_ARRAY_TYPE          10
#define BOOL_ARRAY_TYPE         11
#define DOUBLE_ARRAY_TYPE       12
#define STRING_TYPE             13

//Define start function number
#define START_FUNCTION_TYPE      20

// Define function types
#define INT_FUNCTION_TYPE       20
#define BOOL_FUNCTION_TYPE      21
#define DOUBLE_FUNCTION_TYPE    22
#define VOID_FUNCTION_TYPE      23

// Define array function types
#define INT_ARRAY_FUNCTION_TYPE     30
#define BOOL_ARRAY_FUNCTION_TYPE    31
#define DOUBLE_ARRAY_FUNCTION_TYPE  32
#define STRING_FUNCTION_TYPE    33

// Define the invalid type
#define INVALID_TYPE 255

/**
 * Function definitions
 */
t_bool isTypeArray(struct t_type* type);
t_bool isTypeFunction(struct t_type* type);
const char* convertType(struct t_type* type);
const char* transformType(struct t_type* type);
const char* transformArrayType(struct t_type* type);

#endif // __CMM_TYPES_H__
