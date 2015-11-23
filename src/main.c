/*
Created by Andrew M. Hall
This program exports five command line tools that manipulate matrices from files.
Invert:
    This tool inverts a matrix using cofactor expansion. Realistically, 13-14 is the absolute limit, though with more cores, the parallel nature of this
    program means that it can feasibly invert larger matrices.
Transpose:
    This tool transposes a matrix. This is a simple task and can be run on very large matrices in very little time.
Multiply:
    This tool multiplies two matrices from two files. This can be used for large matrices, and uses POSIX threads to allow for arbitrarily large matrices to be multiplied
Generate:
    This tool generates an n x m random matrix. The output datatype can be specified, and upper and lower limits can be adjusted. This is useful for generating test data.
Test:
    This is not really a tool, but a test suit designed to make sure, each version of the library works as desired. This is implemented as a tool in order for it to be accessible to the user.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "matrix.h"
#include "matrix-test.h"
#include "parse.h"

#define HELP_BUFF_LENGTH 10000

#define NUM_ARGS_INVERT 6
#define NUM_ARGS_TRANSPOSE 6
#define NUM_ARGS_MULTIPLY 8
#define NUM_ARGS_GENERATE 8
#define NUM_ARGS_DETERMINANT 4


/*
The layout of this main file, is designed to be easily extensible for adding new tools.
The TOOL enumeration, enumerates all tools used by this module,
The Argument struct, defines an Argument to have certain properties that describe it.
There is an array for each tool, holding the command line arguments. These arrays are created in the same
order as the accompanying ToolArgument enumeration. This allows an argument to be accessed using the enum code.
E.g.
argumentsInvert[I_DIMENSION].value

The help message for each tool is generated according to the descriptions given for each argument. This means simply
writing brief descriptions is enough, to fully document a command line argument as the function will format it nicely.
*/

//These definitions ensure, that common codes are used throughout
#define TOOL_CODE_SHORT "-t"
#define TOOL_CODE_LONG "--tool"

#define HELP_CODE_SHORT "-h"
#define HELP_CODE_LONG "--help"


//The tool enumeration
typedef enum
{
    NONE=0,
    INVERT,
    TRANSPOSE,
    MULTIPLY,
    GENERATE,
    DETERMINANT,
    TEST
} TOOL;

void getHelpMessage(char * dest, TOOL tool);
int invert(int argc, char* argv[]);
int transpose(int argc, char* argv[]);
int multiply(int argc, char* argv[]);
int generate(int argc, char* argv[]);
int determinant(int argc, char* argv[]);
int test(int argc);

int main(int argc, char* argv[])
{

    //The aforementioned generated help message
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, NONE);

    int i;
    for (i = 0; i < argc; i++)
    {
        //Check if a tool is specified, if so, utilise it
        if (!strcmp(argv[i], TOOL_CODE_LONG) || !strcmp(argv[i], TOOL_CODE_SHORT))
        {
            if (i == argc-1)
            {
                puts("--mode must be accompanied by a valid mode\n");
                return ARGUMENT_ERROR;
            }
            if (!strcmp(argv[i+1], "invert"))
            {
                return invert(argc, argv);
            }
            else if(!strcmp(argv[i+1], "transpose"))
            {
                return transpose(argc, argv);
            }
            else if(!strcmp(argv[i+1], "multiply"))
            {
                return multiply(argc, argv);
            }
            else if(!strcmp(argv[i+1], "generate"))
            {
                return generate(argc, argv);
            }
            else if (!strcmp(argv[i+1], "determinant")){
                return determinant(argc, argv);
            }
            else if(!strcmp(argv[i+1], "test"))
            {
                return test(argc);
            }
            else
            {
                puts("--tool must be accompanied by a valid mode\n");
                puts(helpMessage);
                return ARGUMENT_ERROR;
            }
        }
    }

    //If only the --help flag is given, then provide help for all tools
    for (i = 0; i < argc; i++)
    {
        if (!strcmp(argv[i], HELP_CODE_LONG) || !strcmp(argv[i], HELP_CODE_SHORT))
        {
            puts(helpMessage);
            return EXIT_SUCCESS;
        }
    }
    //default tool is invert
    return invert(argc, argv);
}

//These enumerations allow arguments to be accessed using special codes
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


//These arrays store the command line arguments specific to each tool
Argument argumentsInvert[NUM_ARGS_INVERT] =
{
    NEW_ARG("--dimension", "-d", "The dimension, n, of the input nxn matrix", TRUE, INT, I_DIMENSION),
    NEW_ARG("--input", "-i", "The name of the input file that contains the matrix to be inverted", TRUE, STRING, I_INPUT),
    NEW_ARG("--output", "-o", "The name of a file, which the inverted matrix will be written to", FALSE, STRING, I_OUTPUT),
    NEW_ARG("--parallel", "-p", "Control if program runs in parallel", FALSE, BOOLEAN, I_PARALLEL),
    NEW_ARG(HELP_CODE_LONG, HELP_CODE_SHORT, "Display help message", FALSE, BOOLEAN, I_HELP),
    NEW_ARG("--tool", "-t", "choose which tool to utilise, invert, transpose or multiply (default invert)", FALSE, STRING, 0)
};

Argument argumentsTranspose[NUM_ARGS_TRANSPOSE] =
{
    NEW_ARG("--width", "-x", "The width, x, of the input x by y matrix", TRUE, INT, T_WIDTH),
    NEW_ARG("--height", "-y", "The height, y, of the input x by y matrix", TRUE, INT, T_HEIGHT),
    NEW_ARG("--input", "-i", "The name of the input file that contains the matrix to be transposed", TRUE, STRING, T_INPUT),
    NEW_ARG("--output", "-o", "The name of a file, which the transposed matrix will be written to", FALSE, STRING, T_OUTPUT),
    NEW_ARG(HELP_CODE_LONG, HELP_CODE_SHORT, "Display help message", FALSE, BOOLEAN, T_HELP),
    NEW_ARG("--tool", "-t", "choose which tool to utilise, invert, transpose or multiply (default invert)", FALSE, STRING, 0)
};

Argument argumentsMultiply[NUM_ARGS_MULTIPLY] =
{
    NEW_ARG("--width", "-x", "The width, x, of the first, leftmost, input x by y matrix", TRUE, INT, M_WIDTH),
    NEW_ARG("--height", "-y", "The height, y, of the first, leftmost, input y by x matrix", TRUE, INT, M_HEIGHT),
    NEW_ARG("--input-one", "-i1", "The name of the first input file that contains the matrix to be multiplied by the second matrix", TRUE, STRING, M_INPUT_ONE),
    NEW_ARG("--input-two", "-i2", "The name of the second input file that contains the matrix to be multiplied by the first matrix", TRUE, STRING, M_INPUT_TWO),
    NEW_ARG("--output", "-o", "The name of a file, which the resulting matrix will be written to", FALSE, BOOLEAN, M_OUTPUT),
    NEW_ARG("--parallel", "-p", "Control if program runs in parallel", FALSE, BOOLEAN, M_PARALLEL),
    NEW_ARG(HELP_CODE_LONG, HELP_CODE_SHORT, "Display help message", FALSE, BOOLEAN, M_HELP),
    NEW_ARG("--tool", "-t", "choose which tool to utilise, invert, transpose or multiply (default invert)", FALSE, STRING, 0)
};

Argument argumentsGenerate[NUM_ARGS_GENERATE] =
{
    NEW_ARG("--width", "-x", "The width, x, of the output x by y matrix. x must be an integer.", TRUE, INT, G_WIDTH),
    NEW_ARG("--height", "-y", "The height, y, of the output x by y matrix. y must be an integer.", TRUE, INT, G_HEIGHT),
    NEW_ARG("--datatype", "-d", "Define if only integral or floating values should be returned", FALSE, STRING, G_DATATYPE),
    NEW_ARG("--lower", "-l", "The lowest value with which to fill the matrix", FALSE, INT, G_LOWER),
    NEW_ARG("--upper", "-u", "The greatest value with which to fill the matrix", FALSE, INT, G_UPPER),
    NEW_ARG("--output", "-o", "The name of a file which the matrix will be written to", FALSE, STRING, G_OUTPUT),
    NEW_ARG(HELP_CODE_LONG, HELP_CODE_SHORT, "Display help message", FALSE, BOOLEAN, G_HELP),
    NEW_ARG("--tool", "-t", "choose which tool to utilise, invert, transpose or multiply (default invert)", FALSE, STRING, 0)
};

Argument argumentsDeterminant[NUM_ARGS_DETERMINANT] =
{
    NEW_ARG("--dimension", "-d", "The dimension, n, of the input nxn matrix", TRUE, INT, D_DIMENSION),
    NEW_ARG("--input", "-i", "The name of the input file for which to find the determinant", TRUE, STRING, D_INPUT),
    NEW_ARG(HELP_CODE_LONG, HELP_CODE_SHORT, "Display help message", FALSE, BOOLEAN, D_HELP),
    NEW_ARG("--tool", "-t", "choose which tool to utilise, invert, transpose or multiply (default invert)", FALSE, STRING, 0)
};

//Invert tool
int invert(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, INVERT);

    ParseError err = parse(argumentsInvert,NUM_ARGS_INVERT, argv, argc);
    if (err.code != PARSE_SUCCESS){
        printParseError(err);
        return ARGUMENT_ERROR;
    }

    //Hold the dimension of the matrix in question
    int dim = argumentsInvert[I_DIMENSION].value.i;
    if (dim <= 1)
    {
        PRINT_ERROR_CODE(DIMENSION_ERROR);
        return DIMENSION_ERROR;
    }

    //The input file pointer
    FILE * fp = fopen(argumentsInvert[I_INPUT].value.s, "r");
    if (!fp)
    {
        PRINT_ERROR_CODE(FILE_IO_ERROR);
        printf("could not open file: %s\n", argumentsInvert[I_INPUT].value.s);
        return FILE_IO_ERROR;
    }

    //The input data read from the file and cast to a double for more precision
    double * data = (double*)calloc(dim*dim,sizeof(double));

    if (!data)
    {
        PRINT_ERROR_CODE(MEM_ALLOCATION_FAILURE);
        return MEM_ALLOCATION_FAILURE;
    }

    //Read data from file
    float num;
    int count = 0;
    while(count < dim*dim && fscanf(fp, "%f", &num))
    {
        data[count] = (double)num;
        count++;
    }
    fclose(fp);

    //The matrix, m, which shall hold the input data
    doubleMatrix m = DEFAULT_MATRIX;
    MatrixError e = matrixD(&m, data, dim, dim);
    if (e.code != SUCCESS)
    {
        printError(e);
        free(data);
        return e.code;
    }
    free(data);

    /*The --parallel flag dictates if the program should be allowed to run using p-threads
    or instead, just use a single process. The matrix.h library exports two different versions
    of the invert function which allow this specification*/
    BOOL para = argumentsInvert[I_PARALLEL].value.b;

    //If the --output flag is given, then the inverted matrix must be written to the specified output file
    if (argumentsInvert[I_OUTPUT].value.b)
    {
        //Open the file, and execute the required checks
        FILE * out = fopen(argumentsInvert[I_OUTPUT].value.s, "w");
        if (!out)
        {
            PRINT_ERROR_CODE(FILE_IO_ERROR);
            printf("Failed to create file %s\n", argumentsInvert[I_OUTPUT].value.s);
            destroymD(&m);
            return FILE_IO_ERROR;
        }

        //inv will hold the inverted matrix.
        doubleMatrix inv = DEFAULT_MATRIX;
        /*The matrix library uses the matrixNullD function to create nxm matrices filled with 0's
        It is however beholden on the programmer, to take care of the returned matrix errors*/
        e = matrixNullD(&inv, dim, dim);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m);
            return e.code;
        }

        if (para)
        {
            e = invertD(&inv, &m); //invertD is allowed to parallelism, it does not need to if it isn't appropriate
        }
        else
        {
            e = stdInvertD(&inv, &m); //stdInvertD cannot use posix threads
        }

        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m);
            destroymD(&inv);
            return e.code;
        }

        //matrix.h exports a 'to string' function, which nicly formats a matrix and places it into a string buffer
        char * strBuff = (char*)malloc(dim*dim*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &inv, dim*dim*20+1);
        if (e.code != SUCCESS)
        {
            printError(e);
            free(strBuff);
            destroymD(&inv);
            destroymD(&m);
            return e.code;
        }

        //write to file and destroy buffers and inv
        fprintf(out, "%s", strBuff);
        free(strBuff);
        destroymD(&inv);
    }
    else
    {
        //create the inv matrix
        doubleMatrix inv = DEFAULT_MATRIX;
        e = matrixNullD(&inv, dim, dim);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m);
            return e.code;
        }

        //use appropriate invert function (as per --parallel)
        if (para)
        {
            e = invertD(&inv, &m);
        }
        else
        {
            e = stdInvertD(&inv, &m);
        }

        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&inv);
            destroymD(&m);
            return e.code;
        }

        fflush(stdout);

        //matrix.h exports a printmD function, which takes care of printing a matrix to stdout
        e = printmD(&inv);
        if (e.code != SUCCESS)
        {
            printf("\n%s", e.message);
            destroymD(&inv);
            destroymD(&m);
            return e.code;
        }
        destroymD(&inv);
    }

    destroymD(&m);
    return SUCCESS;
}


//Transpose tools
int transpose(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, TRANSPOSE);

    ParseError err = parse(argumentsTranspose, NUM_ARGS_TRANSPOSE, argv, argc);

    if (err.code != PARSE_SUCCESS){
        printParseError(err);
        return ARGUMENT_ERROR;
    }

    //Hold the width and height of the matrix in question
    int width, height;
    //casting command line argument to integer requires checking
    width = argumentsTranspose[T_WIDTH].value.i;
    height = argumentsTranspose[T_HEIGHT].value.i;

    if (width < 1 || height < 1)
    {
        PRINT_ERROR_CODE(DIMENSION_ERROR);
        return ARGUMENT_ERROR;
    }

    //Deal with the input file
    FILE * fp = fopen(argumentsTranspose[T_INPUT].value.s, "r");
    if (!fp)
    {
        PRINT_ERROR_CODE(FILE_IO_ERROR);
        printf("could not open file: %s\n", argumentsTranspose[T_INPUT].value.s);
        return FILE_IO_ERROR;
    }

    //the data stream for reading in data from the file
    double * data = (double*)calloc(width*height,sizeof(double));

    if (!data)
    {
        PRINT_ERROR_CODE(MEM_ALLOCATION_FAILURE);
        return MEM_ALLOCATION_FAILURE;
    }

    //Read data from file
    float num;
    int count = 0;
    while(count < width*height && fscanf(fp, "%f", &num))
    {
        data[count] = (double)num;
        count++;
    }
    fclose(fp);

    //The matrix which will hold the input data
    doubleMatrix m = DEFAULT_MATRIX;
    MatrixError e = matrixD(&m, data, width, height);
    if (e.code != SUCCESS)
    {
        printError(e);
        free(data);
        return SUCCESS;
    }
    free(data);

    //If an output file is specified, then the data must be written there
    if (argumentsTranspose[T_OUTPUT].value.b)
    {
        //Open output file
        FILE * out = fopen(argumentsTranspose[T_OUTPUT].value.s, "w");
        if (!out)
        {
            PRINT_ERROR_CODE(FILE_IO_ERROR);
            printf("Failed to create file %s\n", argumentsTranspose[T_OUTPUT].value.s);
            destroymD(&m);
            return FILE_IO_ERROR;
        }

        //There is no option to parallelise transposition
        transposeD(&m);

        //Use toStringD and write to file
        char * strBuff = (char*)malloc(width*height*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &m, width*height*20+1);
        if (e.code != SUCCESS)
        {
            printError(e);
            free(strBuff);
            destroymD(&m);
            return e.code;
        }

        fprintf(out, "%s", strBuff);
        free(strBuff);
    }
    else
    {
        //Print transposed matrix to output
        transposeD(&m);

        fflush(stdout);

        e = printmD(&m);
        if (e.code != SUCCESS)
        {
            printf("\n%s", e.message);
            destroymD(&m);
            return e.code;
        }
    }

    destroymD(&m);
    return SUCCESS;
}

//Multiply tool
int multiply(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, MULTIPLY);

    ParseError err = parse(argumentsMultiply, NUM_ARGS_INVERT, argv, argc);
    if (err.code != PARSE_SUCCESS){
        printParseError(err);
        return ARGUMENT_ERROR;
    }

    //Hold the dimension of the matrix in question
    int width, height;
    //casting command line argument to integer requires much checking
    width = argumentsMultiply[M_WIDTH].value.i;
    height = argumentsMultiply[M_HEIGHT].value.i;

    if (width < 1 || height < 1)
    {
        PRINT_ERROR_CODE(DIMENSION_ERROR);
        return DIMENSION_ERROR;
    }

    FILE * fp1 = fopen(argumentsMultiply[M_INPUT_ONE].value.s, "r");
    FILE * fp2 = fopen(argumentsMultiply[M_INPUT_TWO].value.s, "r");
    if (!fp1)
    {
        PRINT_ERROR_CODE(FILE_IO_ERROR);
        printf("could not open file: %s\n", argumentsMultiply[M_INPUT_ONE].value.s);
        return FILE_IO_ERROR;
    }
    if (!fp2)
    {
        PRINT_ERROR_CODE(FILE_IO_ERROR);
        printf("could not open file: %s\n", argumentsMultiply[M_INPUT_TWO].value.s);
        return FILE_IO_ERROR;
    }

    double * data1 = (double*)calloc(width*height, sizeof(double));
    double * data2 = (double*)calloc(width*height, sizeof(double));

    if (!data1 || !data2)
    {
        PRINT_ERROR_CODE(MEM_ALLOCATION_FAILURE);
        return MEM_ALLOCATION_FAILURE;
    }

    float num1, num2;
    int count = 0;
    while(count < width*height && fscanf(fp1, "%f", &num1) && fscanf(fp2, "%f", &num2))
    {
        data1[count] = (double)num1;
        data2[count] = (double)num2;
        count++;
    }
    fclose(fp1);
    fclose(fp2);

    doubleMatrix m1 = DEFAULT_MATRIX;
    MatrixError e = matrixD(&m1, data1, width, height);
    if (e.code != SUCCESS)
    {
        printError(e);
        free(data1);
        return e.code;
    }
    free(data1);

    doubleMatrix m2 = DEFAULT_MATRIX;
    e = matrixD(&m2, data2, height, width);
    if (e.code != SUCCESS)
    {
        printError(e);
        free(data2);
        return e.code;
    }
    free(data2);

    BOOL para = argumentsMultiply[M_PARALLEL].value.b;

    if (argumentsMultiply[M_OUTPUT].value.b)
    {
        FILE * out = fopen(argumentsMultiply[M_OUTPUT].value.s, "w");
        if (!out)
        {
            PRINT_ERROR_CODE(FILE_IO_ERROR);
            printf("Failed to create file %s\n", argumentsMultiply[M_OUTPUT].value.s);
            destroymD(&m1);
            destroymD(&m2);
            return FILE_IO_ERROR;
        }
        doubleMatrix mult = DEFAULT_MATRIX;
        e = matrixNullD(&mult, height, height);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m1);
            destroymD(&m2);
            return e.code;
        }

        if (para)
        {
            e = multiplyD(&mult, &m1, &m2);
        }
        else
        {
            e = stdMultiplyD(&mult, &m1, &m2);
        }

        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return e.code;
        }

        char * strBuff = (char*)malloc(height*height*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &mult, height*height*20+1);
        if (e.code != SUCCESS)
        {
            printError(e);
            free(strBuff);
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return e.code;
        }

        fprintf(out, "%s", strBuff);
        free(strBuff);
        destroymD(&mult);
    }
    else
    {
        doubleMatrix mult = DEFAULT_MATRIX;
        e = matrixNullD(&mult, height, height);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m1);
            destroymD(&m2);
            return e.code;
        }

        if (para)
        {
            e = multiplyD(&mult, &m1, &m2);
        }
        else
        {
            e = stdMultiplyD(&mult, &m1, &m2);
        }

        if (e.code != SUCCESS)
        {
            printError(e);
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return e.code;
        }

        fflush(stdout);

        e = printmD(&mult);
        if (e.code != SUCCESS)
        {
            printf("\n%s", e.message);
            destroymD(&mult);
            destroymD(&m1);
            destroymD(&m2);
            return e.code;
        }
        destroymD(&mult);
    }
    destroymD(&m1);
    destroymD(&m2);
    return SUCCESS;
}

//Generate tool
int generate(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, GENERATE);

    ParseError err = parse(argumentsGenerate, NUM_ARGS_GENERATE, argv, argc);
    if (err.code != PARSE_SUCCESS){
        printParseError(err);
        printf("code: %d\n", err.code);
        return ARGUMENT_ERROR;
    }

    if(!argumentsGenerate[G_DATATYPE].value.b)
        argumentsGenerate[G_DATATYPE].value.s = "integer";

    //Hold the dimension of the matrix in question
    int dimX, dimY;
    //casting command line argument to integer requires checking
    dimX = argumentsGenerate[G_WIDTH].value.i;
    dimY = argumentsGenerate[G_HEIGHT].value.i;
    if (dimX <= 1)
    {
        PRINT_ERROR_CODE(DIMENSION_ERROR);
        return DIMENSION_ERROR;
    }
    if (dimY <= 1)
    {
        PRINT_ERROR_CODE(DIMENSION_ERROR);
        return DIMENSION_ERROR;
    }

    int lower, upper;
    if (argumentsGenerate[G_LOWER].value.i)
    {
        lower = argumentsGenerate[G_LOWER].value.i;
    }
    else
    {
        lower = -100;
    }

    if (argumentsGenerate[G_UPPER].value.i)
    {
        upper = argumentsGenerate[G_UPPER].value.i;
    }
    else
    {
        upper = 100;
    }

    if (!strcmp(argumentsGenerate[G_DATATYPE].value.s, "float"))
    {
        floatMatrix m = DEFAULT_MATRIX;
        MatrixError e;
        e = matrixNullF(&m, dimX, dimY);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymF(&m);
            return e.code;
        }

        int x, y;
        time_t t;
        srand((unsigned) time(&t));

        for (y = 0; y < dimY; y++)
        {
            for (x = 0; x < dimX; x++)
            {
                insertAtF(&m, (float)(lower + (rand() % (upper - lower)) + ((float)rand()/(float)RAND_MAX)), x, y);
            }
        }
        if (argumentsGenerate[G_OUTPUT].value.s)
        {
            FILE *fp = fopen(argumentsGenerate[G_OUTPUT].value.s, "w");
            if (!fp)
            {
                PRINT_ERROR_CODE(FILE_IO_ERROR);
                return FILE_IO_ERROR;
            }

            char * strBuff = (char*)malloc(dimX*dimY*20*sizeof(char)+1);
            strBuff[0] = '\0';
            e = toStringF(strBuff, &m, dimX*dimY*20*+1);
            if (e.code != SUCCESS)
            {
                printError(e);
                free(strBuff);
                destroymF(&m);
                return e.code;
            }

            fprintf(fp, "%s", strBuff);
            fclose(fp);
            free(strBuff);
        }
        else
        {
            printmF(&m);
        }
        destroymF(&m);
    }
    else if (!strcmp(argumentsGenerate[G_DATATYPE].value.s, "integer"))
    {
        intMatrix m = DEFAULT_MATRIX;
        MatrixError e;
        e = matrixNullI(&m, dimX, dimY);
        if (e.code != SUCCESS)
        {
            printError(e);
            destroymI(&m);
            return e.code;
        }

        int x, y;
        time_t t;
        srand((unsigned) time(&t));

        for (y = 0; y < dimY; y++)
        {
            for (x = 0; x < dimX; x++)
            {
                insertAtI(&m, lower + (rand() % (upper - lower)), x, y);
            }
        }
        if (argumentsGenerate[G_OUTPUT].value.s)
        {
            FILE *fp = fopen(argumentsGenerate[G_OUTPUT].value.s, "w");
            if (!fp)
            {
                PRINT_ERROR_CODE(FILE_IO_ERROR);
                destroymI(&m);
                return FILE_IO_ERROR;
            }

            char * strBuff = (char*)malloc(dimX*dimY*20*sizeof(char)+1);
            strBuff[0] = '\0';
            e = toStringI(strBuff, &m, dimX*dimY*20*+1);
            if (e.code != SUCCESS)
            {
                printError(e);
                free(strBuff);
                destroymI(&m);
                return e.code;
            }

            fprintf(fp, "%s", strBuff);
            fclose(fp);
            free(strBuff);
        }
        else
        {
            printmI(&m);
        }
        destroymI(&m);
    }
    else
    {
        printf("datatype must be either integer or float, %s was given\n", argumentsGenerate[G_DATATYPE].value.s);
        return ARGUMENT_ERROR;
    }
    return SUCCESS;
}

int determinant(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, DETERMINANT);

    ParseError err = parse(argumentsDeterminant, NUM_ARGS_DETERMINANT, argv, argc);
    if (err.code != PARSE_SUCCESS){
        printParseError(err);
        return ARGUMENT_ERROR;
    }

    int dim = argumentsDeterminant[D_DIMENSION].value.i;
    if (dim <= 1){
        printf("Dimension must be greater than 1, %d given\n", dim);
        return DIMENSION_ERROR;
    }

    FILE * fp = fopen(argumentsDeterminant[D_INPUT].value.s, "r");

    if (!fp){
        PRINT_ERROR_CODE(FILE_IO_ERROR);
        printf("could not open file: %s\n", argumentsDeterminant[D_INPUT].value.s);
        return FILE_IO_ERROR;
    }

    double *data = (double*)calloc(dim*dim, sizeof(double));

    if (!data){
        PRINT_ERROR_CODE(MEM_ALLOCATION_FAILURE);
        return MEM_ALLOCATION_FAILURE;
    }

    float num;
    int count = 0;
    while(count < dim*dim && fscanf(fp, "%f", &num))
    {
        data[count] = (double)num;
        count++;
    }
    fclose(fp);

    doubleMatrix m = DEFAULT_MATRIX;
    MatrixError e;
    e = matrixD(&m, data, dim, dim);

    if (e.code != SUCCESS){
        printError(e);
        return e.code;
    }

    char strBuff[20] = { '\0' };
    doubleToString(strBuff, determinantD(&m, 0));

    printf("%s\n", strBuff);

    return SUCCESS;
}

int test(int argc)
{
    //test does not take any command line arguments
    if (argc > 3)
    {
        puts("test does not take any command line arguments\n");
        return ARGUMENT_ERROR;
    }
    puts("Testing:");
    printf("Tests Passed: %d of %d\n", matrix_suite(), TEST_NUMBER);
    return SUCCESS;
}

void getHelpMessage(char * dest, TOOL tool)
{
    int lim = 0;
    Argument * args=NULL;
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
        getHelpMessage(dest, INVERT);
        strcat(dest, "\nTool: Transpose\n");
        getHelpMessage(dest, TRANSPOSE);
        strcat(dest, "\nTool: Multiply\n");
        getHelpMessage(dest, MULTIPLY);
        strcat(dest, "\nTool: Generate\n");
        getHelpMessage(dest, GENERATE);
        strcat(dest, "\nTool: Determinant\n");
        getHelpMessage(dest, DETERMINANT);
        return;
    }
    strcat(dest, "usage $ ./matrix ");
    int i;
    for (i = 0; i < lim; i++)
    {
        if (!args[i].mandatory)
        {
            strcat(dest, "   [");
            strcat(dest, args[i].shortcode);
            strcat(dest, " | ");
            strcat(dest, args[i].longcode);
            strcat(dest, "]  ");
        }
        else
        {
            strcat(dest, "   ");
            strcat(dest, args[i].shortcode);
            strcat(dest, " | ");
            strcat(dest, args[i].longcode);
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
        strcat(dest, args[i].shortcode);
        strcat(dest, ", ");
        strcat(dest, args[i].longcode);
        strcat(dest, ": ");
        strcat(dest, args[i].description);
        strcat(dest, "\n\t");
    }
}

