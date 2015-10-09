/*
Written by Andrew M. Hall
*/

#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>
#include <pthread.h>

#define TRUE 1
#define FALSE 0

#define NTHREADS 40

typedef struct {
    double* data;
    int width;
    int height;
    int size;
} doubleMatrix;

typedef struct {
    float* data;
    int width;
    int height;
    int size;
} floatMatrix;

typedef struct {
    int* data;
    int width;
    int height;
    int size;
} intMatrix;

typedef int BOOL;

//DONE
BOOL matrixD(doubleMatrix * dest, double * in_data, int in_width, int in_height);
BOOL matrixF(floatMatrix * dest, float * in_data, int in_width, int in_height);
BOOL matrixI(intMatrix * dest, int * in_data, int in_width, int in_height);

//DONE
BOOL matrixNullD(doubleMatrix * dest, int in_width, int in_height);
BOOL matrixNullF(floatMatrix * dest, int in_width, int in_height);
BOOL matrixNullI(intMatrix * dest, int in_width, int in_height);

//DONE
BOOL destroymD(doubleMatrix * a);
BOOL destroymF(floatMatrix * a);
BOOL destroymI(intMatrix * a);

//DONE
BOOL matrixcpyD(doubleMatrix * dest, const doubleMatrix * src);
BOOL matrixcpyF(floatMatrix * dest, const floatMatrix * src);
BOOL matrixcpyI(intMatrix * dest, const intMatrix * src);

//DONE
BOOL printmD(const doubleMatrix *a);
BOOL printmF(const floatMatrix *a);
BOOL printmI(const intMatrix *a);

//DONE
double determinantD(const doubleMatrix *a, int row);
float determinantF(const floatMatrix *a, int row);
int determinantI(const intMatrix *a, int row);

//DONE
double determinant2x2(const doubleMatrix *a);
float determinant2x2F(const floatMatrix *a);
int determinant2x2I(const intMatrix *a);

//DONE
BOOL adjointD(doubleMatrix * dest, const doubleMatrix *a);
BOOL adjointF(floatMatrix * dest, const floatMatrix *a);
BOOL adjointI(intMatrix * dest, const intMatrix *a);

BOOL paraCofactorD(doubleMatrix * dest, const doubleMatrix *a);
BOOL paraCofactorF(floatMatrix * dest, const floatMatrix *a);
BOOL paraCofactorI(intMatrix * dest, const intMatrix *a);

BOOL stdCofactorD(doubleMatrix * dest, const doubleMatrix *a);
BOOL stdCofactorF(floatMatrix * dest, const floatMatrix *a);
BOOL stdCofactorI(intMatrix * dest, const intMatrix *a);

//DONE
BOOL CofactorD(doubleMatrix * dest, const doubleMatrix *a);
BOOL cofactorF(floatMatrix * dest, const floatMatrix *a);
BOOL cofactorI(intMatrix * dest, const intMatrix *a);

//DONE
void transposeD(doubleMatrix *a);
void transposeF(floatMatrix *a);
void transposeI(intMatrix *a);

//DONE
BOOL invert2x2D(doubleMatrix * dest, const doubleMatrix *a);
BOOL invert2x2F(doubleMatrix * dest, const floatMatrix *a);
BOOL invert2x2I(doubleMatrix * dest, const intMatrix *a);

//DONE
BOOL invertD(doubleMatrix * dest, const doubleMatrix *a);
BOOL invertF(doubleMatrix * dest, const floatMatrix *a);
BOOL invertI(doubleMatrix * dest, const intMatrix *a);

//DONE
BOOL addD(doubleMatrix *a, const doubleMatrix *b);
BOOL addF(floatMatrix *a, const floatMatrix *b);
BOOL addI(intMatrix *a, const intMatrix *b);

//DONE
BOOL subtractD(doubleMatrix *a, const doubleMatrix *b);
BOOL subtractF(floatMatrix *a, const floatMatrix *b);
BOOL subtractI(intMatrix *a, const intMatrix *b);

//DONE
void negativeD(doubleMatrix *a);
void negativeF(floatMatrix *a);
void negativeI(intMatrix *a);

//DONE
void scalarMultiplyD(doubleMatrix *a, const double b);
void scalarMultiplyF(floatMatrix *a, const float b);
void scalarMultiplyI(intMatrix *a, const int b);

//TO DO
doubleMatrix multiplyD(const doubleMatrix *a, const doubleMatrix *b);
floatMatrix multiplyF(const floatMatrix *a, const floatMatrix *b);
intMatrix multiplyI(const intMatrix *a, const intMatrix *b);

//DONE
BOOL cmpD(const doubleMatrix *a, const doubleMatrix *b);
BOOL cmpF(const floatMatrix *a, const floatMatrix *b);
BOOL cmpI(const intMatrix *a, const intMatrix *b);

//DONE
BOOL toStringD(char* dest, const doubleMatrix *a, size_t buffSize);
BOOL toStringF(char* dest, const floatMatrix *a, size_t buffSize);
BOOL toStringI(char* dest, const intMatrix *a, size_t buffSize);

//DONE
double atD(const doubleMatrix* a, int x, int y);
float atF(const floatMatrix* a, int x, int y);
int atI(const intMatrix* a, int x, int y);

#endif
