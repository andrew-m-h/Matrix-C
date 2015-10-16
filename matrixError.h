#ifndef MATRIXERROR_H
#define MATRIXERROR_H

/*
Created by Andrew M. Hall
*/

#include <stdio.h>
#include <stdlib.h>

#define NUM_ERRORS 7

typedef enum{
    SUCCESS = 0,
    MEM_ALLOCATION_FAILURE,
    MATH_ERROR,
    DIMENSION_ERROR,
    BUFF_SIZE_ERROR,
    FILE_IO_ERROR,
    FAILURE
} MatrixError;

const char* getErrorMessage(MatrixError e);

#endif // MATRIXERROR_H
