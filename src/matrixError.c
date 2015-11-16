/*
Created by Andrew M. Hall
*/

#include "matrixError.h"

const char errors[NUM_ERRORS][200] =
{
    "Matrix Operation Success\n",
    "Failed to allocate sufficient memory to perform matrix operation\n",
    "Math error occurred when performing matrix operation\n\tLikely Cause: Take inverse of matrix with determinant of 0\n",
    "Dimension error occurred when performing matrix operation\n\tLikely Cause: Take inverse, cofactor or determinant of non square matrix\n",
    "Buffer error occurred\n\tLikely Cause: String buffer not of sufficient length to represent matrix\n",
    "Failed to correctly perform fileIO\n",
    "General Matrix Failure occurred\n",
    "Incorrect arguments given to program\n"
};

MatrixError getError(const MatrixErrorCode e, const char* file, const int line, const char* func)
{
    const MatrixError err = {.code=e, .file=file, .line=line, .func=func, .message=getErrorMessage(e)};
    return err;
}

const char* getErrorMessage(MatrixErrorCode e)
{
    if (e >= NUM_ERRORS || e < 0)
    {
        puts("Could not get error message for provided error code\n");
        return NULL;
    }
    return errors[e];
}

void printError(const MatrixError e){
    printf("Matrix Error occurred:\n\tfile: %s\n\tline: %d\n\tfunction: %s\n\t%s", e.file, e.line, e.func, e.message);
}

