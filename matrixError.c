/*
Created by Andrew M. Hall
*/

#include "matrixError.h"

const char* errors[NUM_ERRORS] = {
    "Matrix Operation Success\n",
    "Failed to allocate sufficient memory to perform matrix operation\n",
    "Math error occurred when performing matrix operation\n\tLikely Cause: Take inverse of matrix with determinant of 0\n",
    "Dimension error occurred when performing matrix operation\n\tLikely Cause: Take inverse, cofactor or determinant of non square matrix\n",
    "Buffer error occurred\n\tLikely Cause: String buffer not of sufficient length to represent matrix\n",
    "Failed to correctly perform fileIO\n",
    "General Matrix Failure occurred\n"
};

const char* getErrorMessage(MatrixError e){
    if (e >= NUM_ERRORS || e < 0){
        puts("Could not get error message for provided error code\n");
        return NULL;
    }
    return errors[e];
}
