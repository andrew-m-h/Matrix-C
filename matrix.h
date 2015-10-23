/*
Created by Andrew M. Hall
This module defines Matrix manipulation functions for Matrices using three numeric data types.
For speed, the POSIX Thread library is used to run certain small aspects in parallel. This parallelism
allows the inverse of a 11x11 matrix to be calculated in reasonable time.
*/

#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>
#include "matrixError.h"

#define DEFAULT_MATRIX {NULL, 0, 0}
#define NTHREADS 40

//Basic Matrix data types. Matrices are implemented on top of a 1D dynamically allocated array.
//All functions will be implemented three times to work on each one of these Matrices.
typedef struct
{
    double* data;
    int width;
    int height;
} doubleMatrix;

typedef struct
{
    float* data;
    int width;
    int height;
} floatMatrix;

typedef struct
{
    int* data;
    int width;
    int height;
} intMatrix;

//Personal preference and used through out this library
typedef int BOOL;

#define TRUE 1
#define FALSE 0

//Matrix constructors
MatrixError matrixD(doubleMatrix * dest, const double * in_data, int in_width, int in_height);
MatrixError matrixF(floatMatrix * dest, const float * in_data, int in_width, int in_height);
MatrixError matrixI(intMatrix * dest, const int * in_data, int in_width, int in_height);

//null Matrix constructors, fills matrix with 0's
MatrixError matrixNullD(doubleMatrix * dest, int in_width, int in_height);
MatrixError matrixNullF(floatMatrix * dest, int in_width, int in_height);
MatrixError matrixNullI(intMatrix * dest, int in_width, int in_height);

MatrixError matrixCopyConsD(doubleMatrix * dest, const doubleMatrix * src);
MatrixError matrixCopyConsF(floatMatrix * dest, const floatMatrix * src);
MatrixError matrixCopyConsI(intMatrix * dest, const intMatrix * src);

//matrix destructor, necessary since dynamic memory is used
void destroymD(doubleMatrix * a);
void destroymF(floatMatrix * a);
void destroymI(intMatrix * a);

//Copy one matrix to another, basically just a call to memcpy with some checks and balances for good measure
MatrixError matrixcpyD(doubleMatrix * dest, const doubleMatrix * src);
MatrixError matrixcpyF(floatMatrix * dest, const floatMatrix * src);
MatrixError matrixcpyI(intMatrix * dest, const intMatrix * src);

//Print matrices to stdout using printf
MatrixError printmD(const doubleMatrix *a);
MatrixError printmF(const floatMatrix *a);
MatrixError printmI(const intMatrix *a);

//Calculate the determinant of a matrix using a given row.
double determinantD(const doubleMatrix *a, int row);
float determinantF(const floatMatrix *a, int row);
int determinantI(const intMatrix *a, int row);

//calculate the determinant of a 2x3 matrix, this is the base case for the determinant calculation
double determinant2x2D(const doubleMatrix *a);
float determinant2x2F(const floatMatrix *a);
int determinant2x2I(const intMatrix *a);

//Calculate the adjoint of a matrix, used to work out an inverse
MatrixError adjointD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError adjointF(floatMatrix * dest, const floatMatrix *a);
MatrixError adjointI(intMatrix * dest, const intMatrix *a);

//Take adjoint of matrix without parallelism
MatrixError stdAdjointD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError stdAdjointF(floatMatrix * dest, const floatMatrix *a);
MatrixError stdAdjointI(intMatrix * dest, const intMatrix *a);

/*This is one of two implementations of the cofactor function. It uses the POSIX Thread library to perform
The calculation in parallel. This provides significant speed boost when used to invert larger matrices*/
MatrixError paraCofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError paraCofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError paraCofactorI(intMatrix * dest, const intMatrix *a);

/*This implementation of cofactor, uses only a single thread, and removes the overhead associated with using
multi-threading for small matrices*/
MatrixError stdCofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError stdCofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError stdCofactorI(intMatrix * dest, const intMatrix *a);

//This cofactor function, dispatches a call to either stdcofactor or paracofactor based on the matrix size
MatrixError cofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError cofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError cofactorI(intMatrix * dest, const intMatrix *a);

//Transpose a matrix
void transposeD(doubleMatrix *a);
void transposeF(floatMatrix *a);
void transposeI(intMatrix *a);

//invert a 2x2 matrix using the standard formula
MatrixError invert2x2D(doubleMatrix * dest, const doubleMatrix *a);
MatrixError invert2x2F(doubleMatrix * dest, const floatMatrix *a);
MatrixError invert2x2I(doubleMatrix * dest, const intMatrix *a);

//invert an nxn matrix using cofactor expansion
MatrixError invertD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError invertF(doubleMatrix * dest, const floatMatrix *a);
MatrixError invertI(doubleMatrix * dest, const intMatrix *a);

//Invert matrix using no parallelism
MatrixError stdInvertD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError stdInvertF(doubleMatrix * dest, const floatMatrix *a);
MatrixError stdInvertI(doubleMatrix * dest, const intMatrix *a);


//add two matrices of the same dimension together
MatrixError addD(doubleMatrix *a, const doubleMatrix *b);
MatrixError addF(floatMatrix *a, const floatMatrix *b);
MatrixError addI(intMatrix *a, const intMatrix *b);

//subtract two matrices of the same dimension
MatrixError subtractD(doubleMatrix *a, const doubleMatrix *b);
MatrixError subtractF(floatMatrix *a, const floatMatrix *b);
MatrixError subtractI(intMatrix *a, const intMatrix *b);

//the unary negative of a matrix
void negativeD(doubleMatrix *a);
void negativeF(floatMatrix *a);
void negativeI(intMatrix *a);

//multiply a matrix by a scalar constant
void scalarMultiplyD(doubleMatrix *a, const double b);
void scalarMultiplyF(floatMatrix *a, const float b);
void scalarMultiplyI(intMatrix *a, const int b);

//The cross product of two matrices in R^3
MatrixError crossProductD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError crossProductF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError crossProductI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//The dot product of two matrices in R^n
double dotProductD(const doubleMatrix *a, const doubleMatrix *b);
float dotProductF(const floatMatrix *a, const floatMatrix *b);
int dotProductI(const intMatrix *a, const intMatrix *b);

//Multiply two matrices of appropriate dimensions
MatrixError multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//Multiply two matrices in parallel, this provides a speed boost with larger matrices
MatrixError paraMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError paraMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError paraMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//Multiply two matrices using a single thread. Used for smaller matrices
MatrixError stdMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError stdMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError stdMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//Compare two matrices element wise
BOOL cmpD(const doubleMatrix *a, const doubleMatrix *b);
BOOL cmpF(const floatMatrix *a, const floatMatrix *b);
BOOL cmpI(const intMatrix *a, const intMatrix *b);

//Convert a matrix to a string representation
MatrixError toStringD(char* dest, const doubleMatrix *a, size_t buffSize);
MatrixError toStringF(char* dest, const floatMatrix *a, size_t buffSize);
MatrixError toStringI(char* dest, const intMatrix *a, size_t buffSize);

//Get the element in a matrix at a specified x, y coordinate (0 indexed)
double atD(const doubleMatrix* a, int x, int y);
float atF(const floatMatrix* a, int x, int y);
int atI(const intMatrix* a, int x, int y);

//Insert element into matrix
MatrixError insertAtD(doubleMatrix* a, double val, int x, int y);
MatrixError insertAtF(floatMatrix* a, float val, int x, int y);
MatrixError insertAtI(intMatrix* a, int val, int x, int y);

//Get the x'th column of a matrix.
MatrixError getColD(doubleMatrix *dest, const doubleMatrix *a, int x);
MatrixError getColF(floatMatrix *dest, const floatMatrix *a, int x);
MatrixError getColI(intMatrix *dest, const intMatrix *a, int x);

//Get the y'th row of a matrix.
MatrixError getRowD(doubleMatrix *dest, const doubleMatrix *a, int y);
MatrixError getRowF(floatMatrix *dest, const floatMatrix *a, int y);
MatrixError getRowI(intMatrix *dest, const intMatrix *a, int y);

#endif
