#ifndef HS_MATRIX_H
#define HS_MATRIX_H

#include "matrix.h"
#include "matrixError.h"
#include "matrix-test.h"

int hs_invertD(doubleMatrix * dest, const doubleMatrix * src);
int hs_invertF(doubleMatrix * dest, const floatMatrix * src);
int hs_invertI(doubleMatrix * dest, const intMatrix * src);

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

int hs_stdInvertD(doubleMatrix * dest, const doubleMatrix *a);
int hs_stdInvertF(doubleMatrix * dest, const floatMatrix *a);
int hs_stdInvertI(doubleMatrix * dest, const intMatrix *a);

int hs_multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
int hs_multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
int hs_multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

int hs_stdMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b);
int hs_stdMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b);
int hs_stdMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b);

int hs_matrix_suite(void);

typedef enum
{
    INVERT = 0,
    TRANSPOSE,
    MULTIPLY,
    GENERATE,
    DETERMINANT,
    TEST,
    NONE
} TOOL;

void hs_getHelpMessage(char * dest, TOOL tool);

#endif