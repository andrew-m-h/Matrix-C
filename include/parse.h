#ifndef PARSE_H_INCLUDED
#define PARSE_H_INCLUDED

#define NEW_ARG(longCode, shortCode, arg_description, is_mandatory, datatype, arg_code) {.longcode=longCode, .shortcode=shortCode, .description=arg_description, .mandatory=is_mandatory, .type=datatype, .code=arg_code, .value.i=0}

typedef enum {
    INT = 0,
    FLOAT,
    STRING,
    BOOLEAN
} TYPE;

typedef union {
    int i;
    float f;
    const char * s;
    int b;
} Value;

typedef struct {
    const char *longcode;
    const char *shortcode;
    const char *description;
    int mandatory;
    TYPE type;
    int code;
    Value value;
} Argument;

typedef enum {
    PARSE_SUCCESS = 0,
    INT_CAST_ERROR,
    FLOAT_CAST_ERROR,
    STRING_CPY_ERROR,
    MANDATORY_ARG_MISSING,
    INVALID_FLAG,
    PARSE_FAILURE,
} PARSE_ERROR_CODE;

typedef struct {
    PARSE_ERROR_CODE code;
    const char* flag;
} ParseError;

ParseError parse(Argument* argList, int numArgs, char* args[], int argc);

void printParseError(ParseError err);

#endif // PARSE_H_INCLUDED
