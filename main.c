/*
Written by Andrew M. Hall
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "matrix.h"

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
            return 0;
        }
        int a;
        for (a = 0; a < NUM_ARGS; a++){
            if (!strcmp(argv[i], arguments[a].longCode) || !strcmp(argv[i], arguments[a].shortCode)){
                if (arguments[a].code == HELP){
                    printf("%s\n", helpMessage);
                    return 0;
                }
                arguments[a].value = argv[i+1];
            }
        }
    }

    for (i = 0; i < NUM_ARGS; i++){
        if (arguments[i].value == NULL && arguments[i].mandatory){
            printf("Must have at least:");
            int a;
            for (a = 0; a < NUM_ARGS; a++){
                if (arguments[a].mandatory){
                   printf("\n\t%s or %s", arguments[a].shortCode, arguments[a].longCode);
                }
            }
            printf("\n\t or use the -h or --help flag to display help\n%s\n", helpMessage);
            return 0;
        }
    }

    //Hold the dimension of the matrix in question
    int dim;
    //casting command line argument to integer requires much checking
    dim = atoi(arguments[DIMENSION].value);
    if (!dim){
        printf("%s is not a valid dimension, dimension must be an integer\n", arguments[DIMENSION].value);
        return 0;
    }
    if (dim <= 1){
        printf("The dimension must be greater than 1\n");
        return 0;
    }

    FILE * fp = fopen(arguments[INPUT].value, "r");
    if (!fp){
        printf("could not open file: %s\n", arguments[INPUT].value);
        return 0;
    }

    double * data = (double*)malloc(dim*dim*sizeof(double));

    if (!data){
        printf("failed to allocate dynamic memory of %lu bytes", dim*dim*sizeof(double));
        return 0;
    }

    float num;
    int count = 0;
    while(count < dim*dim && fscanf(fp, "%f", &num)){
        data[count] = (double)num;
        count++;
    }
    fclose(fp);

    doubleMatrix m;
    if (!matrixD(&m, data, dim, dim)){
        printf("Failed to create matrix of dimensions %dx%d\n", dim, dim);
        return 0;
    }

    free(data);

    if (arguments[OUTPUT].value){
        FILE * out = fopen(arguments[OUTPUT].value, "w");
        if (!out){
            printf("Failed to create file %s\n", arguments[OUTPUT].value);
            return 0;
        }
        doubleMatrix inv;
        if (!matrixNullD(&inv, dim, dim)){
            printf("Failed to create matrix of dimensions %dx%d\n", dim, dim);
            return 0;
        }
        if (!invertD(&inv, &m)){
            printf("Could not invert matrix\n");
            return 0;
        }

        char * strBuff = (char*)malloc(dim*dim*20*sizeof(char)+1);
        if (!toStringD(strBuff, &inv, dim*dim*29+1)){
            printf("Failed to represent matrix as string\n");
            return 0;
        }
        fprintf(out, "%s", strBuff);
        free(strBuff);
        destroymD(&inv);
    } else {
        doubleMatrix inv;
        if (!matrixNullD(&inv, dim, dim)){
            printf("Failed to create matrix of dimensions %dx%d\n", dim, dim);
            return 0;
        }

        if (!invertD(&inv, &m)){
            printf("Could not invert matrix\n");
            return 0;
        }

        fflush(stdout);

        printmD(&inv);
        destroymD(&inv);
    }

    destroymD(&m);
    return 0;
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
}
