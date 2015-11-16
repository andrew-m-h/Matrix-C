#ifndef HS_MATRIX_H
#define HS_MATRIX_H

#include "matrix.h"
#include "matrixError.h"
#include "matrix-test.h"

MatrixErrorCode hs_invertD(doubleMatrix * dest, const doubleMatrix * src);
MatrixErrorCode hs_invertF(doubleMatrix * dest, const floatMatrix * src);
MatrixErrorCode hs_invertI(doubleMatrix * dest, const intMatrix * src);

void hs_transposeD(doubleMatrix * m);
void hs_transposeF(floatMatrix * m);
void hs_transposeI(intMatrix * m);

int hs_printmD(const doubleMatrix * m);
int hs_printmF(const floatMatrix * m);
int hs_printmI(const intMatrix * m);

double hs_determinantD(const doubleMatrix *m, int row);
float hs_determinantF(const floatMatrix *m, int row);
int hs_determinantI(const intMatrix *m, int row);

int hs_cofactorD(doubleMatrix * dest, const doubleMatrix *a);
int hs_cofactorF(floatMatrix * dest, const floatMatrix *a);
int hs_cofactorI(intMatrix * dest, const intMatrix *a);

double hs_dotProductD(const doubleMatrix *a, const doubleMatrix *b);
float hs_dotProductF(const floatMatrix *a, const floatMatrix *b);
int hs_dotProductI(const intMatrix *a, const intMatrix *b);

int hs_crossProductD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
int hs_crossProductF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
int hs_crossProductI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

int hs_paraCofactorD(doubleMatrix * dest, const doubleMatrix *a);
int hs_paraCofactorF(floatMatrix * dest, const floatMatrix *a);
int hs_paraCofactorI(intMatrix * dest, const intMatrix *a);

int hs_stdCofactorD(doubleMatrix * dest, const doubleMatrix *a);
int hs_stdCofactorF(floatMatrix * dest, const floatMatrix *a);
int hs_stdCofactorI(intMatrix * dest, const intMatrix *a);

int hs_matrix_suite(void);


#endif