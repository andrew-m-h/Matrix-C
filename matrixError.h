#ifndef MATRIXERROR_H
#define MATRIXERROR_H

/*
Created by Andrew M. Hall
*/

#include <stdio.h>
#include <stdlib.h>

#define NUM_ERRORS 7
#define GET_ERROR(e) getError(e, __FILE__, __LINE__, __FUNCTION__)
#define PRINT_ERROR_CODE(c) printError(getError(c, __FILE__, __LINE__, __FUNCTION__))

typedef enum
{
    SUCCESS = 0,
    MEM_ALLOCATION_FAILURE,
    MATH_ERROR,
    DIMENSION_ERROR,
    BUFF_SIZE_ERROR,
    FILE_IO_ERROR,
    FAILURE
} MatrixErrorCode;

typedef struct
{
    int line;
    const char* file;
    const char* func;
    const char* message;
    MatrixErrorCode code;
} MatrixError;

MatrixError getError(const MatrixErrorCode e, const char* file, const int line, const char* func);
const char* getErrorMessage(const MatrixErrorCode e);
void printError(const MatrixError e);


#endif // MATRIXERROR_H
