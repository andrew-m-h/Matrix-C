/*
Created by Andrew M. Hall
This program exports three command line tools that manipulate matrices from files.
Invert:
    This tool inverts a matrix using cofactor expansion. Realistically, 13-14 is the absolute limit, though with more cores, the parallel nature of this
    program means that it can feasibly invert larger matrices.
Transpose:
    This tool transposes a matrix. This is a simple task and can be run on very large matrices in very little time.
Multiply:
    This tool multiplies two matrices from two files. This can be used for large matrices, and uses POSIX threads to allow for arbitrarily large matrices to be multiplied
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrix-test.h"

#define HELP_BUFF_LENGTH 10000

#define NUM_ARGS_INVERT 5
#define NUM_ARGS_TRANSPOSE 5
#define NUM_ARGS_MULTIPLY 7

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
    MULTIPLY
} TOOL;

void getHelpMessage(char * dest, TOOL tool);
int invert(int argc, char* argv[]);
int transpose(int argc, char* argv[]);
int multiply(int argc, char* argv[]);

int main(int argc, char* argv[])
{
    puts("Testing:");
    printf("Number of tests passed: %d\n\n", matrix_suite());
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
                return EXIT_FAILURE;
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
            else
            {
                puts("--mode must be accompanied by a valid mode\n");
                puts(helpMessage);
                return EXIT_FAILURE;
            }
        }
    }

    //If only the --help flag is given, then provide help for all tools
    for (i = 0; i < argc; i++){
        if (!strcmp(argv[i], HELP_CODE_LONG) || !strcmp(argv[i], HELP_CODE_SHORT)){
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
    M_HELP
};

//The argument struct, used to store information about each argument.
typedef struct
{
    const char* longCode;
    const char* shortCode;
    const char* description;
    int code;
    BOOL mandatory;
    char* value;
} Argument;

//These arrays store the command line arguments specific to each tool
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
    {.longCode="--output", .shortCode="-o", .description="The name of a file, which the transposed matrix will be written to", .code=T_OUTPUT, .mandatory=FALSE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=T_HELP, .mandatory=FALSE, .value=NULL},
};

Argument argumentsMultiply[NUM_ARGS_MULTIPLY] =
{
    {.longCode="--width", .shortCode="-x", .description="The width, x, of the first, leftmost, input x by y matrix", .code=M_WIDTH, .mandatory=TRUE, .value=NULL},
    {.longCode="--height", .shortCode="-y", .description="The height, y, of the first, leftmost, input y by x matrix", .code=M_HEIGHT, .mandatory=TRUE, .value=NULL},
    {.longCode="--input-one", .shortCode="-i1", .description="The name of the first input file that contains the matrix to be multiplied by the second matrix", .code=M_INPUT_ONE, .mandatory=TRUE, .value=NULL},
    {.longCode="--input-two", .shortCode="-i2", .description="The name of the second input file that contains the matrix to be multiplied by the first matrix", .code=M_INPUT_TWO, .mandatory=TRUE, .value=NULL},
    {.longCode="--output", .shortCode="-o", .description="The name of a file, which the resulting matrix will be written to", .code=M_OUTPUT, .mandatory=FALSE, .value=NULL},
    {.longCode="--parallel", .shortCode="-p", .description="Control if program runs in parallel", .code=M_PARALLEL, .mandatory=FALSE, .value=NULL},
    {.longCode=HELP_CODE_LONG, .shortCode=HELP_CODE_SHORT, .description="Display help message", .code=M_HELP, .mandatory=FALSE, .value=NULL},
};

//Invert tool
int invert(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, INVERT);
    int i;
    //Iterate through the command line arguments, they come in pairs, --flag value
    for (i = 1; i < argc; i+=2)
    {
        //Ignore the --mode flag, its already bean dealt with (if present)
        if (!strcmp(argv[i], TOOL_CODE_LONG) || !strcmp(argv[i], TOOL_CODE_SHORT))
            continue;

        //Deal with --help flag being the last argument
        if (i+1 == argc && strcmp(argv[i], argumentsInvert[I_HELP].shortCode) && strcmp(argv[i], argumentsInvert[I_HELP].longCode))
        {
            printf("Incorrect command line arguments, mismatch on %s\n", argv[i]);
            return EXIT_FAILURE;
        }

        /*
        Since C has no map, it is nessecairy to linear search through all possible arguments.
        This is not a problem since the numbers are so small
        */
        int a;
        for (a = 0; a < NUM_ARGS_INVERT; a++)
        {
            if (!strcmp(argv[i], argumentsInvert[a].longCode) || !strcmp(argv[i], argumentsInvert[a].shortCode))
            {
                /*The help flag is a special unary argument. If present, the program should print out
                The help message, and nothing more*/
                if (argumentsInvert[a].code == I_HELP)
                {
                    puts(helpMessage);
                    return EXIT_SUCCESS;
                }
                argumentsInvert[a].value = argv[i+1];
                break;
            } else if (a == NUM_ARGS_TRANSPOSE - 1){
                /*If an invalid flag is passed to a tool, the program should print out a help message,
                and exit in error*/
                printf("Invert does not accept the argument: %s\n", argv[i]);
                puts(helpMessage);
                return EXIT_FAILURE;
            }
        }
    }

    for (i = 0; i < NUM_ARGS_INVERT; i++)
    {
        //With all arguments processed, the program still needs to check if all mandatory arguments have been passed
        //If they have not, the program should return help and exit in error
        if (argumentsInvert[i].value == NULL && argumentsInvert[i].mandatory)
        {
            puts("Must have at least:");
            int a;
            for (a = 0; a < NUM_ARGS_INVERT; a++)
            {
                if (argumentsInvert[a].mandatory)
                {
                    printf("\n\t%s or %s", argumentsInvert[a].shortCode, argumentsInvert[a].longCode);
                }
            }
            printf("\n\t or use the %s or %s flag to display help\n%s\n", HELP_CODE_LONG, HELP_CODE_SHORT, helpMessage);
            return EXIT_FAILURE;
        }
    }

    //Hold the dimension of the matrix in question
    int dim;
    //casting command line argument to integer requires checking
    dim = atoi(argumentsInvert[I_DIMENSION].value);
    if (!dim)
    {
        printf("%s is not a valid dimension, dimension must be an integer\n", argumentsInvert[I_DIMENSION].value);
        return EXIT_FAILURE;
    }
    if (dim <= 1)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        return EXIT_FAILURE;
    }

    //The input file pointer
    FILE * fp = fopen(argumentsInvert[I_INPUT].value, "r");
    if (!fp)
    {
        puts(getErrorMessage(FILE_IO_ERROR));
        printf("could not open file: %s\n", argumentsInvert[I_INPUT].value);
        return EXIT_FAILURE;
    }

    //The input data read from the file and cast to a double for more precision
    double * data = (double*)malloc(dim*dim*sizeof(double));

    if (!data)
    {
        puts(getErrorMessage(MEM_ALLOCATION_FAILURE));
        return EXIT_FAILURE;
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
    if (e != SUCCESS)
    {
        puts(getErrorMessage(e));
        free(data);
        return EXIT_FAILURE;
    }
    free(data);

    /*The --parallel flag dictates if the program should be allowed to run using p-threads
    or instead, just use a single process. The matrix.h library exports two different versions
    of the invert function which allow this specification*/
    BOOL para;
    if (!argumentsInvert[I_PARALLEL].value)
    {
        para = TRUE;
    }
    else if (!strcmp(argumentsInvert[I_PARALLEL].value, "yes"))
    {
        para = TRUE;
    }
    else if (!strcmp(argumentsInvert[I_PARALLEL].value, "no"))
    {
        para = FALSE;
    }
    else
    {
        puts("parallel argument must be either \"yes\" or \"no\"");
        return EXIT_FAILURE;
    }

    //If the --output flag is given, then the inverted matrix must be written to the specified output file
    if (argumentsInvert[I_OUTPUT].value)
    {
        //Open the file, and execute the required checks
        FILE * out = fopen(argumentsInvert[I_OUTPUT].value, "w");
        if (!out)
        {
            puts(getErrorMessage(FILE_IO_ERROR));
            printf("Failed to create file %s\n", argumentsInvert[I_OUTPUT].value);
            destroymD(&m);
            return EXIT_FAILURE;
        }

        //inv will hold the inverted matrix.
        doubleMatrix inv = DEFAULT_MATRIX;
        /*The matrix library uses the matrixNullD function to create nxm matrices filled with 0's
        It is however beholden on the programmer, to take care of the returned matrix errors*/
        e = matrixNullD(&inv, dim, dim);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m);
            return EXIT_FAILURE;
        }

        if (para)
        {
            e = invertD(&inv, &m); //invertD is allowed to parallelism, it does not need to if it isn't appropriate
        }
        else
        {
            e = stdInvertD(&inv, &m); //stdInvertD cannot use posix threads
        }

        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m);
            destroymD(&inv);
            return EXIT_FAILURE;
        }

        //matrix.h exports a 'to string' function, which nicly formats a matrix and places it into a string buffer
        char * strBuff = (char*)malloc(dim*dim*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &inv, dim*dim*20+1);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            free(strBuff);
            destroymD(&inv);
            destroymD(&m);
            return EXIT_FAILURE;
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
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m);
            return EXIT_FAILURE;
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

        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&inv);
            destroymD(&m);
            return EXIT_FAILURE;
        }

        fflush(stdout);

        //matrix.h exports a printmD function, which takes care of printing a matrix to stdout
        e = printmD(&inv);
        if (e != SUCCESS)
        {
            printf("\n%s", getErrorMessage(e));
            destroymD(&inv);
            destroymD(&m);
            return EXIT_FAILURE;
        }
        destroymD(&inv);
    }

    destroymD(&m);
    return EXIT_SUCCESS;
}


//Transpose tools
int transpose(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, TRANSPOSE);
    int i;
    for (i = 1; i < argc; i+=2)
    {
        if (!strcmp(argv[i], TOOL_CODE_LONG) || !strcmp(argv[i], TOOL_CODE_SHORT))
        {
            continue;
        }
        //Deal with --help flag being the last argument
        if (i+1 == argc && strcmp(argv[i], argumentsTranspose[T_HELP].shortCode) && strcmp(argv[i], argumentsTranspose[T_HELP].longCode))
        {
            printf("Incorrect command line arguments, mismatch on %s\n", argv[i]);
            return EXIT_FAILURE;
        }
        //Find if the given flag matches a Transpose flag
        int a;
        for (a = 0; a < NUM_ARGS_TRANSPOSE; a++)
        {
            if (!strcmp(argv[i], argumentsTranspose[a].longCode) || !strcmp(argv[i], argumentsTranspose[a].shortCode))
            {
                //Deal with the --help argument
                if (argumentsTranspose[a].code == T_HELP)
                {
                    puts("This happened\n");
                    puts(helpMessage);
                    return EXIT_SUCCESS;
                }
                argumentsTranspose[a].value = argv[i+1];
                break;
            } else if (a == NUM_ARGS_TRANSPOSE - 1){
                //Deal with invalid argument being passed
                printf("Transpose does not accept the argument: %s\n", argv[i]);
                puts(helpMessage);
                return EXIT_FAILURE;
            }
        }
    }

    for (i = 0; i < NUM_ARGS_TRANSPOSE; i++)
    {
        //Check if all mandatory arguments are given
        if (argumentsTranspose[i].value == NULL && argumentsTranspose[i].mandatory)
        {
            puts("Must have at least:");
            int a;
            for (a = 0; a < NUM_ARGS_INVERT; a++)
            {
                if (argumentsTranspose[a].mandatory)
                {
                    printf("\n\t%s or %s", argumentsTranspose[a].shortCode, argumentsTranspose[a].longCode);
                }
            }
            printf("\n\t or use the %s or %s flag to display help\n%s\n", HELP_CODE_LONG, HELP_CODE_SHORT, helpMessage);
            return EXIT_FAILURE;
        }
    }

    //Hold the width and height of the matrix in question
    int width, height;
    //casting command line argument to integer requires checking
    width = atoi(argumentsTranspose[T_WIDTH].value);
    height = atoi(argumentsTranspose[T_HEIGHT].value);
    if (!width)
    {
        printf("%s is not a valid width, dimension must be an integer\n", argumentsTranspose[T_WIDTH].value);
        return EXIT_FAILURE;
    }
    else if (!height)
    {
        printf("%s is not a valid height, dimension must be an integer\n", argumentsTranspose[T_HEIGHT].value);
        return EXIT_FAILURE;
    }

    if (width < 1 || height < 1)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        return EXIT_FAILURE;
    }

    //Deal with the input file
    FILE * fp = fopen(argumentsTranspose[T_INPUT].value, "r");
    if (!fp)
    {
        puts(getErrorMessage(FILE_IO_ERROR));
        printf("could not open file: %s\n", argumentsTranspose[T_INPUT].value);
        return EXIT_FAILURE;
    }

    //the data stream for reading in data from the file
    double * data = (double*)malloc(width*height*sizeof(double));

    if (!data)
    {
        puts(getErrorMessage(MEM_ALLOCATION_FAILURE));
        return EXIT_FAILURE;
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
    if (e != SUCCESS)
    {
        puts(getErrorMessage(e));
        free(data);
        return EXIT_FAILURE;
    }
    free(data);

    //If an output file is specified, then the data must be written there
    if (argumentsTranspose[T_OUTPUT].value)
    {
        //Open output file
        FILE * out = fopen(argumentsTranspose[T_OUTPUT].value, "w");
        if (!out)
        {
            puts(getErrorMessage(FILE_IO_ERROR));
            printf("Failed to create file %s\n", argumentsTranspose[T_OUTPUT].value);
            destroymD(&m);
            return EXIT_FAILURE;
        }

        //There is no option to parallelise transposition
        transposeD(&m);

        //Use toStringD and write to file
        char * strBuff = (char*)malloc(width*height*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &m, width*height*20+1);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            free(strBuff);
            destroymD(&m);
            return EXIT_FAILURE;
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
        if (e != SUCCESS)
        {
            printf("\n%s", getErrorMessage(e));
            destroymD(&m);
            return EXIT_FAILURE;
        }
    }

    destroymD(&m);
    return EXIT_SUCCESS;
}


//Multiply tool
int multiply(int argc, char* argv[])
{
    char helpMessage[HELP_BUFF_LENGTH] = {'\0'};
    getHelpMessage(helpMessage, MULTIPLY);
    int i;
    for (i = 1; i < argc; i+=2)
    {
        if (!strcmp(argv[i], TOOL_CODE_LONG) || !strcmp(argv[i], TOOL_CODE_SHORT))
        {
            continue;
        }
        //Deal with --help flag being the last argument
        if (i+1 == argc && strcmp(argv[i], argumentsMultiply[M_HELP].shortCode) && strcmp(argv[i], argumentsMultiply[M_HELP].longCode)){
            printf("Incorrect command line arguments, mismatch on %s\n", argv[i]);
            return EXIT_FAILURE;
        }
        int a;
        for (a = 0; a < NUM_ARGS_MULTIPLY; a++)
        {
            if (!strcmp(argv[i], argumentsMultiply[a].longCode) || !strcmp(argv[i], argumentsMultiply[a].shortCode))
            {
                if (argumentsMultiply[a].code == M_HELP)
                {
                    puts(helpMessage);
                    return EXIT_SUCCESS;
                }
                argumentsMultiply[a].value = argv[i+1];
                break;
            } else if (a == NUM_ARGS_TRANSPOSE - 1){
                printf("Invert does not accept the argument: %s\n", argv[i]);
                puts(helpMessage);
                return EXIT_FAILURE;
            }
        }
    }

    for (i = 0; i < NUM_ARGS_MULTIPLY; i++)
    {
        if (argumentsMultiply[i].value == NULL && argumentsMultiply[i].mandatory)
        {
            puts("Must have at least:");
            int a;
            for (a = 0; a < NUM_ARGS_MULTIPLY; a++)
            {
                if (argumentsMultiply[a].mandatory)
                {
                    printf("\n\t%s or %s", argumentsMultiply[a].shortCode, argumentsMultiply[a].longCode);
                }
            }
            printf("\n\t or use the %s or %s flag to display help\n%s\n", HELP_CODE_LONG, HELP_CODE_SHORT, helpMessage);
            return EXIT_FAILURE;
        }
    }

    //Hold the dimension of the matrix in question
    int width, height;
    //casting command line argument to integer requires much checking
    width = atoi(argumentsMultiply[M_WIDTH].value);
    height = atoi(argumentsMultiply[M_HEIGHT].value);
    if (!width)
    {
        printf("%s is not a valid width, width must be an integer\n", argumentsMultiply[M_WIDTH].value);
        return EXIT_FAILURE;
    } else if (!height){
        printf("%s is not a valid height, height must be an integer\n", argumentsMultiply[M_HEIGHT].value);
        return EXIT_FAILURE;
    }

    if (width < 1 || height < 1)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        return EXIT_FAILURE;
    }

    FILE * fp1 = fopen(argumentsMultiply[M_INPUT_ONE].value, "r");
    FILE * fp2 = fopen(argumentsMultiply[M_INPUT_TWO].value, "r");
    if (!fp1)
    {
        puts(getErrorMessage(FILE_IO_ERROR));
        printf("could not open file: %s\n", argumentsMultiply[M_INPUT_ONE].value);
        return EXIT_FAILURE;
    }
    if (!fp2)
    {
        puts(getErrorMessage(FILE_IO_ERROR));
        printf("could not open file: %s\n", argumentsMultiply[M_INPUT_TWO].value);
        return EXIT_FAILURE;
    }

    double * data1 = (double*)malloc(width*height*sizeof(double));
    double * data2 = (double*)malloc(width*height*sizeof(double));

    if (!data1 || !data2)
    {
        puts(getErrorMessage(MEM_ALLOCATION_FAILURE));
        return EXIT_FAILURE;
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
    if (e != SUCCESS)
    {
        puts(getErrorMessage(e));
        free(data1);
        return EXIT_FAILURE;
    }
    free(data1);

    doubleMatrix m2 = DEFAULT_MATRIX;
    e = matrixD(&m2, data2, height, width);
    if (e != SUCCESS)
    {
        puts(getErrorMessage(e));
        free(data2);
        return EXIT_FAILURE;
    }
    free(data2);

    BOOL para;
    if (!argumentsMultiply[M_PARALLEL].value)
    {
        para = TRUE;
    }
    else if (!strcmp(argumentsMultiply[M_PARALLEL].value, "yes"))
    {
        para = TRUE;
    }
    else if (!strcmp(argumentsMultiply[M_PARALLEL].value, "no"))
    {
        para = FALSE;
    }
    else
    {
        puts("parallel argument must be either \"yes\" or \"no\"");
        return EXIT_FAILURE;
    }

    if (argumentsMultiply[M_OUTPUT].value)
    {
        FILE * out = fopen(argumentsMultiply[M_OUTPUT].value, "w");
        if (!out)
        {
            puts(getErrorMessage(FILE_IO_ERROR));
            printf("Failed to create file %s\n", argumentsMultiply[M_OUTPUT].value);
            destroymD(&m1);
            destroymD(&m2);
            return EXIT_FAILURE;
        }
        doubleMatrix mult = DEFAULT_MATRIX;
        e = matrixNullD(&mult, height, height);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m1);
            destroymD(&m2);
            return EXIT_FAILURE;
        }

        if (para)
        {
            e = multiplyD(&mult, &m1, &m2);
        }
        else
        {
            e = stdMultiplyD(&mult, &m1, &m2);
        }

        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return EXIT_FAILURE;
        }

        char * strBuff = (char*)malloc(height*height*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &mult, height*height*20+1);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            free(strBuff);
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return EXIT_FAILURE;
        }

        fprintf(out, "%s", strBuff);
        free(strBuff);
        destroymD(&mult);
    }
    else
    {
        doubleMatrix mult = DEFAULT_MATRIX;
        e = matrixNullD(&mult, height, height);
        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m1);
            destroymD(&m2);
            return EXIT_FAILURE;
        }

        if (para)
        {
            e = multiplyD(&mult, &m1, &m2);
        }
        else
        {
            e = stdMultiplyD(&mult, &m1, &m2);
        }

        if (e != SUCCESS)
        {
            puts(getErrorMessage(e));
            destroymD(&m1);
            destroymD(&m2);
            destroymD(&mult);
            return EXIT_FAILURE;
        }

        fflush(stdout);

        e = printmD(&mult);
        if (e != SUCCESS)
        {
            printf("\n%s", getErrorMessage(e));
            destroymD(&mult);
            destroymD(&m1);
            destroymD(&m2);
            return EXIT_FAILURE;
        }
        destroymD(&mult);
    }
    destroymD(&m1);
    destroymD(&m2);
    return EXIT_SUCCESS;
}

void getHelpMessage(char * dest, TOOL tool)
{
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
    case NONE :
        strcat(dest, "Tool: Invert\n");
        getHelpMessage(dest, INVERT);
        strcat(dest, "\nTool: Transpose\n");
        getHelpMessage(dest, TRANSPOSE);
        strcat(dest, "\nTool: Multiply\n");
        getHelpMessage(dest, MULTIPLY);
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
