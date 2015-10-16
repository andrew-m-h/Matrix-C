/*
Written by Andrew M. Hall
*/

#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>
#include "matrixError.h"

#define NTHREADS 40

typedef struct {
    double* data;
    int width;
    int height;
} doubleMatrix;

typedef struct {
    float* data;
    int width;
    int height;
} floatMatrix;

typedef struct {
    int* data;
    int width;
    int height;
} intMatrix;

typedef int BOOL;

#define TRUE 1
#define FALSE 0


//DONE
MatrixError matrixD(doubleMatrix * dest, double * in_data, int in_width, int in_height);
MatrixError matrixF(floatMatrix * dest, float * in_data, int in_width, int in_height);
MatrixError matrixI(intMatrix * dest, int * in_data, int in_width, int in_height);

//DONE
MatrixError matrixNullD(doubleMatrix * dest, int in_width, int in_height);
MatrixError matrixNullF(floatMatrix * dest, int in_width, int in_height);
MatrixError matrixNullI(intMatrix * dest, int in_width, int in_height);

//DONE
void destroymD(doubleMatrix * a);
void destroymF(floatMatrix * a);
void destroymI(intMatrix * a);

//DONE
MatrixError matrixcpyD(doubleMatrix * dest, const doubleMatrix * src);
MatrixError matrixcpyF(floatMatrix * dest, const floatMatrix * src);
MatrixError matrixcpyI(intMatrix * dest, const intMatrix * src);

//DONE
MatrixError printmD(const doubleMatrix *a);
MatrixError printmF(const floatMatrix *a);
MatrixError printmI(const intMatrix *a);

//DONE
double determinantD(const doubleMatrix *a, int row);
float determinantF(const floatMatrix *a, int row);
int determinantI(const intMatrix *a, int row);

//DONE
double determinant2x2(const doubleMatrix *a);
float determinant2x2F(const floatMatrix *a);
int determinant2x2I(const intMatrix *a);

//DONE
MatrixError adjointD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError adjointF(floatMatrix * dest, const floatMatrix *a);
MatrixError adjointI(intMatrix * dest, const intMatrix *a);

//DONE
MatrixError paraCofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError paraCofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError paraCofactorI(intMatrix * dest, const intMatrix *a);

//DONE
MatrixError stdCofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError stdCofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError stdCofactorI(intMatrix * dest, const intMatrix *a);

//DONE
MatrixError CofactorD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError cofactorF(floatMatrix * dest, const floatMatrix *a);
MatrixError cofactorI(intMatrix * dest, const intMatrix *a);

//DONE
void transposeD(doubleMatrix *a);
void transposeF(floatMatrix *a);
void transposeI(intMatrix *a);

//DONE
MatrixError invert2x2D(doubleMatrix * dest, const doubleMatrix *a);
MatrixError invert2x2F(doubleMatrix * dest, const floatMatrix *a);
MatrixError invert2x2I(doubleMatrix * dest, const intMatrix *a);

//DONE
MatrixError invertD(doubleMatrix * dest, const doubleMatrix *a);
MatrixError invertF(doubleMatrix * dest, const floatMatrix *a);
MatrixError invertI(doubleMatrix * dest, const intMatrix *a);

//DONE
MatrixError addD(doubleMatrix *a, const doubleMatrix *b);
MatrixError addF(floatMatrix *a, const floatMatrix *b);
MatrixError addI(intMatrix *a, const intMatrix *b);

//DONE
MatrixError subtractD(doubleMatrix *a, const doubleMatrix *b);
MatrixError subtractF(floatMatrix *a, const floatMatrix *b);
MatrixError subtractI(intMatrix *a, const intMatrix *b);

//DONE
void negativeD(doubleMatrix *a);
void negativeF(floatMatrix *a);
void negativeI(intMatrix *a);

//DONE
void scalarMultiplyD(doubleMatrix *a, const double b);
void scalarMultiplyF(floatMatrix *a, const float b);
void scalarMultiplyI(intMatrix *a, const int b);

MatrixError crossProductD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError crossProductF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError crossProductI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//DONE
double dotProductD(const doubleMatrix *a, const doubleMatrix *b);
float dotProductF(const floatMatrix *a, const floatMatrix *b);
int dotProductI(const intMatrix *a, const intMatrix *b);

//DONE
MatrixError multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

MatrixError paraMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError paraMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError paraMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

MatrixError stdMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
MatrixError stdMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
MatrixError stdMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

//DONE
BOOL cmpD(const doubleMatrix *a, const doubleMatrix *b);
BOOL cmpF(const floatMatrix *a, const floatMatrix *b);
BOOL cmpI(const intMatrix *a, const intMatrix *b);

//DONE
MatrixError toStringD(char* dest, const doubleMatrix *a, size_t buffSize);
MatrixError toStringF(char* dest, const floatMatrix *a, size_t buffSize);
MatrixError toStringI(char* dest, const intMatrix *a, size_t buffSize);

//DONE
double atD(const doubleMatrix* a, int x, int y);
float atF(const floatMatrix* a, int x, int y);
int atI(const intMatrix* a, int x, int y);

//DONE
MatrixError getColD(doubleMatrix *dest, const doubleMatrix *a, int x);
MatrixError getColF(floatMatrix *dest, const floatMatrix *a, int x);
MatrixError getColI(intMatrix *dest, const intMatrix *a, int x);

//DONE
MatrixError getRowD(doubleMatrix *dest, const doubleMatrix *a, int y);
MatrixError getRowF(floatMatrix *dest, const floatMatrix *a, int y);
MatrixError getRowI(intMatrix *dest, const intMatrix *a, int y);

#endif
