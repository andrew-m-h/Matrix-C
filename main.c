/*
Written by Andrew M. Hall
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrixError.h"

#define NUM_ARGS 4

typedef enum{
    DIMENSION = 0,
    INPUT,
    OUTPUT,
    HELP
}ArgCode;

struct Argument{
    const char* longCode;
    const char* shortCode;
    const char* description;
    ArgCode code;
    BOOL mandatory;
    char* value;

}arguments[NUM_ARGS] = {
{.longCode="--dimension", .shortCode="-d", .description="The dimension, n, of the input nxn matrix", .code=DIMENSION, .mandatory=TRUE, .value=NULL},
{.longCode="--input", .shortCode="-i", .description="The name of the input file that contains the matrix to be inverted", .code=INPUT, .mandatory=TRUE, .value=NULL},
{.longCode="--output", .shortCode="-o", .description="The name of a file, which the inverted matrix will be written to", .code=OUTPUT, .mandatory=FALSE, .value=NULL},
{.longCode="--help", .shortCode="-h", .description="Display help message", .code=HELP, .mandatory=FALSE, .value=NULL}
};

void getHelpMessage(char * dest);

int main(int argc, char* argv[]){
    char helpMessage[10000] = {'\0'};
    getHelpMessage(helpMessage);
    int i;
    for (i = 1; i < argc; i+=2){
        //Deal with --help flag being the
        if (i+1 == argc && strcmp(argv[i], arguments[HELP].shortCode) && strcmp(argv[i], arguments[HELP].longCode)){
            printf("Incorrect command line arguments, mismatch on %s\n", argv[i]);
            return EXIT_FAILURE;
        }
        int a;
        for (a = 0; a < NUM_ARGS; a++){
            if (!strcmp(argv[i], arguments[a].longCode) || !strcmp(argv[i], arguments[a].shortCode)){
                if (arguments[a].code == HELP){
                    puts(helpMessage);
                    return EXIT_SUCCESS;
                }
                arguments[a].value = argv[i+1];
            }
        }
    }

    for (i = 0; i < NUM_ARGS; i++){
        if (arguments[i].value == NULL && arguments[i].mandatory){
            puts("Must have at least:");
            int a;
            for (a = 0; a < NUM_ARGS; a++){
                if (arguments[a].mandatory){
                   printf("\n\t%s or %s", arguments[a].shortCode, arguments[a].longCode);
                }
            }
            printf("\n\t or use the -h or --help flag to display help\n%s\n", helpMessage);
            return EXIT_FAILURE;
        }
    }

    //Hold the dimension of the matrix in question
    int dim;
    //casting command line argument to integer requires much checking
    dim = atoi(arguments[DIMENSION].value);
    if (!dim){
        printf("%s is not a valid dimension, dimension must be an integer\n", arguments[DIMENSION].value);
        return EXIT_FAILURE;
    }
    if (dim <= 1){
        puts(getErrorMessage(DIMENSION_ERROR));
        return EXIT_FAILURE;
    }

    FILE * fp = fopen(arguments[INPUT].value, "r");
    if (!fp){
        puts(getErrorMessage(FILE_IO_ERROR));
        printf("could not open file: %s\n", arguments[INPUT].value);
        return EXIT_FAILURE;
    }

    double * data = (double*)malloc(dim*dim*sizeof(double));

    if (!data){
        puts(getErrorMessage(MEM_ALLOCATION_FAILURE));
        return EXIT_FAILURE;
    }

    float num;
    int count = 0;
    while(count < dim*dim && fscanf(fp, "%f", &num)){
        data[count] = (double)num;
        count++;
    }
    fclose(fp);

    doubleMatrix m;
    MatrixError e = matrixD(&m, data, dim, dim);
    if (e != SUCCESS){
        puts(getErrorMessage(e));
        free(data);
        return EXIT_FAILURE;
    }

    free(data);

    if (arguments[OUTPUT].value){
        FILE * out = fopen(arguments[OUTPUT].value, "w");
        if (!out){
            puts(getErrorMessage(FILE_IO_ERROR));
            printf("Failed to create file %s\n", arguments[OUTPUT].value);
            destroymD(&m);
            return EXIT_FAILURE;
        }
        doubleMatrix inv;
        e = matrixNullD(&inv, dim, dim);
        if (e != SUCCESS){
            puts(getErrorMessage(e));
            destroymD(&m);
            return EXIT_FAILURE;
        }
        e = invertD(&inv, &m);
        if (e != SUCCESS){
            puts(getErrorMessage(e));
            destroymD(&m);
            destroymD(&inv);
            return EXIT_FAILURE;
        }

        char * strBuff = (char*)malloc(dim*dim*20*sizeof(char)+1);
        strBuff[0] = '\0';
        e = toStringD(strBuff, &inv, dim*dim*20+1);
        if (e != SUCCESS){
            puts(getErrorMessage(e));
            free(strBuff);
            destroymD(&inv);
            destroymD(&m);
            return EXIT_FAILURE;
        }

        fprintf(out, "%s", strBuff);
        free(strBuff);
        destroymD(&inv);
    } else {
        doubleMatrix inv;
        e = matrixNullD(&inv, dim, dim);
        if (e != SUCCESS){
            puts(getErrorMessage(e));
            destroymD(&m);
            return EXIT_FAILURE;
        }

        e = invertD(&inv, &m);
        if (e != SUCCESS){
            puts(getErrorMessage(e));
            destroymD(&inv);
            destroymD(&m);
            return EXIT_FAILURE;
        }

        fflush(stdout);

        e = printmD(&inv);
        if (e != SUCCESS){
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

void getHelpMessage(char * dest){
    strcat(dest, "usage $ ./matrix-invert-c ");
    int i;
    for (i = 0; i < NUM_ARGS; i++){
        if (!arguments[i].mandatory){
            strcat(dest, "   [");
            strcat(dest, arguments[i].shortCode);
            strcat(dest, " | ");
            strcat(dest, arguments[i].longCode);
            strcat(dest, "]  ");
        } else {
            strcat(dest, "   ");
            strcat(dest, arguments[i].shortCode);
            strcat(dest, " | ");
            strcat(dest, arguments[i].longCode);
        }
    }
    strcat(dest, "\n\t");
    for (i = 0; i < NUM_ARGS; i++){
        strcat(dest, arguments[i].shortCode);
        strcat(dest, ", ");
        strcat(dest, arguments[i].longCode);
        strcat(dest, ": ");
        strcat(dest, arguments[i].description);
        strcat(dest, "\n\t");
    }
    strcat(dest, "\n");
}
