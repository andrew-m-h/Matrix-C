
#include "HaskellMatrix.h"
#include "string.h"

int hs_invertD(doubleMatrix * dest, const doubleMatrix * src){
	return invertD(dest, src).code;
}

int hs_invertF(doubleMatrix * dest, const floatMatrix * src){
	return invertF(dest, src).code;
}

int hs_invertI(doubleMatrix * dest, const intMatrix * src){
	return invertI(dest, src).code;
}

void hs_transposeD(doubleMatrix * m){
	transposeD(m);
}

void hs_transposeF(floatMatrix * m){
	transposeF(m);
}

void hs_transposeI(intMatrix * m){
	transposeI(m);
}

int hs_printmD(const doubleMatrix * m){
	return printmD(m).code;
}

int hs_printmF(const floatMatrix * m){
	return printmF(m).code;
}

int hs_printmI(const intMatrix * m){
	return printmI(m).code;
}

double hs_determinantD(const doubleMatrix *m, int row){
	return determinantD(m, row);
}

float hs_determinantF(const floatMatrix *m, int row){
	return determinantF(m, row);
}

int hs_determinantI(const intMatrix *m, int row){
	return determinantI(m, row);
}

int hs_cofactorD(doubleMatrix * dest, const doubleMatrix *a){
	return cofactorD(dest, a).code;
}

int hs_cofactorF(floatMatrix * dest, const floatMatrix *a){
	return cofactorF(dest, a).code;
}

int hs_cofactorI(intMatrix * dest, const intMatrix *a){
	return cofactorI(dest, a).code;
}

int hs_multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b){
	return multiplyD(dest, a, b).code;
}

int hs_multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b){
	return multiplyF(dest, a, b).code;
}

int hs_multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b){
	return multiplyI(dest, a, b).code;
}

double hs_dotProductD(const doubleMatrix *a, const doubleMatrix *b){
	return hs_dotProductD(a, b);
}

float hs_dotProductF(const floatMatrix *a, const floatMatrix *b){
	return hs_dotProductF(a, b);
}

int hs_dotProductI(const intMatrix *a, const intMatrix *b){
	return hs_dotProductI(a, b);
}

int hs_matrix_suite(){
	return matrix_suite();
}

int hs_crossProductD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b){
	return crossProductD(dest, a, b).code;
}

int hs_crossProductF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b){
	return crossProductF(dest, a, b).code;
}

int hs_crossProductI(intMatrix *dest, const intMatrix *a, const intMatrix *b){
	return crossProductI(dest, a, b).code;
}

int hs_paraCofactorD(doubleMatrix * dest, const doubleMatrix *a){
	return paraCofactorD(dest, a).code;
}

int hs_paraCofactorF(floatMatrix * dest, const floatMatrix *a){
	return paraCofactorF(dest, a).code;
}

int hs_paraCofactorI(intMatrix * dest, const intMatrix *a){
	return paraCofactorI(dest, a).code;
}

int hs_stdCofactorD(doubleMatrix * dest, const doubleMatrix *a){
	return stdCofactorD(dest, a).code;
}
int hs_stdCofactorF(floatMatrix * dest, const floatMatrix *a){
	return stdCofactorF(dest, a).code;
}

int hs_stdCofactorI(intMatrix * dest, const intMatrix *a){
	return stdCofactorI(dest, a).code;
}

int hs_stdInvertD(doubleMatrix * dest, const doubleMatrix *a){
	return stdInvertD(dest, a).code;
}

int hs_stdInvertF(doubleMatrix * dest, const floatMatrix *a){
	return stdInvertF(dest, a).code;
}

int hs_stdInvertI(doubleMatrix * dest, const intMatrix *a){
	return stdInvertI(dest, a).code;
}

int hs_stdMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b){
    return stdMultiplyD(dest, a, b).code;
}

int hs_stdMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b){
    return stdMultiplyF(dest, a, b).code;

}

int hs_stdMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b){
    return stdMultiplyI(dest, a, b).code;

}







#define TOOL_CODE_SHORT "-t"
#define TOOL_CODE_LONG "--tool"

#define HELP_CODE_SHORT "-h"
#define HELP_CODE_LONG "--help"

#define NUM_ARGS_INVERT 5
#define NUM_ARGS_TRANSPOSE 5
#define NUM_ARGS_MULTIPLY 7
#define NUM_ARGS_GENERATE 7
#define NUM_ARGS_DETERMINANT 3

enum
{
    I_DIMENSION = 0,
    I_INPUT,
    I_OUTPUT,
    I_PARALLEL,
    I_HELP,
    I_TEST
};

enum
{
    T_WIDTH = 0,
    T_HEIGHT,
    T_INPUT,
    T_OUTPUT,
    T_HELP,
};

enum
{
    M_WIDTH = 0,
    M_HEIGHT,
    M_INPUT_ONE,
    M_INPUT_TWO,
    M_OUTPUT,
    M_PARALLEL,
    M_HELP,
};

enum
{
    G_WIDTH = 0,
    G_HEIGHT,
    G_DATATYPE,
    G_LOWER,
    G_UPPER,
    G_OUTPUT,
    G_HELP
};

enum
{
    D_DIMENSION = 0,
    D_INPUT,
    D_HELP
};

typedef struct
{
    const char* longCode;
    const char* shortCode;
    const char* description;
    int code;
    BOOL mandatory;
    char* value;
} Argument;

Argument argumentsInvert[NUM_ARGS_INVERT] =
{
    {.longCode="--dimension", .shortCode="-d", .description="The dimension, n, of the input nxn matrix", .code=I_DIMENSION, .mandatory=TRUE, .value=NULL},
    {.longCode="--input", .shortCode="-i", .description="The name of the input file that contains the matrix to be inverted", .code=I_INPUT, .mandatory=TRUE, .value=NULL},
    {.longCode="--output", .shortCode="-o", .description="The name of a file, which the inverted matrix will be written to", .code=I_OUTPUT, .mandatory=FALSE, .value=NULL},
    {.longCode="--parallel", .shortCode="-p", .description="Control if program runs in parallel", .code=I_PARALLEL, .mandatory=FALSE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=I_HELP, .mandatory=FALSE, .value=NULL}
};

Argument argumentsTranspose[NUM_ARGS_TRANSPOSE] =
{
    {.longCode="--width", .shortCode="-x", .description="The width, x, of the input x by y matrix", .code=T_WIDTH, .mandatory=TRUE, .value=NULL},
    {.longCode="--height", .shortCode="-y", .description="The height, y, of the input x by y matrix", .code=T_HEIGHT, .mandatory=TRUE, .value=NULL},
    {.longCode="--input", .shortCode="-i", .description="The name of the input file that contains the matrix to be transposed", .code=T_INPUT, .mandatory=TRUE, .value=NULL},
    {.longCode="--output", .shortCode="-o", .description="The name of a file, which the transposed matrix will be written to", .code=T_OUTPUT, .mandatory=FALSE, .value=FALSE},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=T_HELP, .mandatory=FALSE, .value=NULL}
};

Argument argumentsMultiply[NUM_ARGS_MULTIPLY] =
{
    {.longCode="--width", .shortCode="-x", .description="The width, x, of the first, leftmost, input x by y matrix", .code=M_WIDTH, .mandatory=TRUE, .value=NULL},
    {.longCode="--height", .shortCode="-y", .description="The height, y, of the first, leftmost, input y by x matrix", .code=M_HEIGHT, .mandatory=TRUE, .value=NULL},
    {.longCode="--input-one", .shortCode="-i1", .description="The name of the first input file that contains the matrix to be multiplied by the second matrix", .code=M_INPUT_ONE, .mandatory=TRUE, .value=NULL},
    {.longCode="--input-two", .shortCode="-i2", .description="The name of the second input file that contains the matrix to be multiplied by the first matrix", .code=M_INPUT_TWO, .mandatory=TRUE, .value=NULL},
    {.longCode="--output", .shortCode="-o", .description="The name of a file, which the resulting matrix will be written to", .code=M_OUTPUT, .mandatory=FALSE, .value=NULL},
    {.longCode="--parallel", .shortCode="-p", .description="Control if program runs in parallel", .code=M_PARALLEL, .mandatory=FALSE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=M_HELP, .mandatory=FALSE, .value=NULL}
};

Argument argumentsGenerate[NUM_ARGS_GENERATE] =
{
    {.longCode="--width", .shortCode="-x", .description="The width, x, of the output x by y matrix. x must be an integer.", .code=G_WIDTH, .mandatory=TRUE, .value=NULL},
    {.longCode="--height", .shortCode="-y", .description="The height, y, of the output x by y matrix. y must be an integer.", .code=G_HEIGHT, .mandatory=TRUE, .value=NULL},
    {.longCode="--datatype", .shortCode="-d", .description="Define if only integral or floating values should be returned", .code=G_DATATYPE, .mandatory=FALSE, .value=NULL},
    {.longCode="--lower", .shortCode="-l", .description="The lowest value with which to fill the matrix", .code=G_LOWER, .mandatory=FALSE, .value=NULL},
    {.longCode="--upper", .shortCode="-u", .description="The greatest value with which to fill the matrix", .code=G_UPPER, .mandatory=FALSE, .value=NULL},
    {.longCode="--output", .shortCode="-o", .description="The name of a file which the matrix will be written to", .code=G_OUTPUT, .mandatory=FALSE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=G_HELP, .mandatory=FALSE, .value=NULL}
};

Argument argumentsDeterminant[NUM_ARGS_DETERMINANT] =
{
    {.longCode="--dimension", .shortCode="-d", .description="The dimension, n, of the input nxn matrix", .code=D_DIMENSION, .mandatory=TRUE, .value=NULL},
    {.longCode="--input", .shortCode="-i", .description="The name of the input file for which to find the determinant", .code=I_INPUT, .mandatory=TRUE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=I_HELP, .mandatory=FALSE, .value=NULL}
};

void hs_getHelpMessage(char * dest, TOOL tool){
    int lim = 0;
    Argument * args = NULL;
    switch(tool)
    {
    case INVERT :
        lim = NUM_ARGS_INVERT;
        args = argumentsInvert;
        break;
    case TRANSPOSE :
        lim = NUM_ARGS_TRANSPOSE;
        args = argumentsTranspose;
        break;
    case MULTIPLY :
        lim = NUM_ARGS_MULTIPLY;
        args = argumentsMultiply;
        break;
    case GENERATE :
        lim = NUM_ARGS_GENERATE;
        args = argumentsGenerate;
        break;
    case DETERMINANT :
        lim = NUM_ARGS_DETERMINANT;
        args = argumentsDeterminant;
        break;
    case TEST :
        break;
    case NONE :
        strcat(dest, "Tool: Invert\n");
        hs_getHelpMessage(dest, INVERT);
        strcat(dest, "\nTool: Transpose\n");
        hs_getHelpMessage(dest, TRANSPOSE);
        strcat(dest, "\nTool: Multiply\n");
        hs_getHelpMessage(dest, MULTIPLY);
        strcat(dest, "\nTool: Generate\n");
        hs_getHelpMessage(dest, GENERATE);
        strcat(dest, "\nTool: Determinant\n");
        hs_getHelpMessage(dest, DETERMINANT);
        return;
    }
    strcat(dest, "usage $ ./matrix ");
    int i;
    for (i = 0; i < lim; i++)
    {
        if (!args[i].mandatory)
        {
            strcat(dest, "   [");
            strcat(dest, args[i].shortCode);
            strcat(dest, " | ");
            strcat(dest, args[i].longCode);
            strcat(dest, "]  ");
        }
        else
        {
            strcat(dest, "   ");
            strcat(dest, args[i].shortCode);
            strcat(dest, " | ");
            strcat(dest, args[i].longCode);
        }
    }
    switch(tool)
    {
    case INVERT :
        strcat(dest, "   [-t invert | --tool invert]");
        strcat(dest, "\n\t");
        break;
    case TRANSPOSE :
        strcat(dest, "   -t transpose | --tool transpose");
        strcat(dest, "\n\t");
        break;
    case MULTIPLY :
        strcat(dest, "   -t multiply | --tool multiply");
        strcat(dest, "\n\t");
        break;
    case GENERATE :
        strcat(dest, "   -t generate | --tool generate");
        strcat(dest, "\n\t");
        break;
    case DETERMINANT :
        strcat(dest, "   -t determinant | --tool determinant");
        strcat(dest, "\n\t");
        break;
    case TEST :
        return;
    case NONE :
        return;
    }
    for (i = 0; i < lim; i++)
    {
        strcat(dest, args[i].shortCode);
        strcat(dest, ", ");
        strcat(dest, args[i].longCode);
        strcat(dest, ": ");
        strcat(dest, args[i].description);
        strcat(dest, "\n\t");
    }
    strcat(dest, "-t, --tool: choose which tool to utilise, invert, transpose or multiply (default invert)\n");
}
