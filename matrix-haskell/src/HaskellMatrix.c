#include "HaskellMatrix.h"
 
MatrixErrorCode hs_invertD(doubleMatrix * dest, const doubleMatrix * src){
	return invertD(dest, src).code;
}

MatrixErrorCode hs_invertF(doubleMatrix * dest, const floatMatrix * src){
	return invertF(dest, src).code;
}

MatrixErrorCode hs_invertI(doubleMatrix * dest, const intMatrix * src){
	return invertI(dest, src).code;
}

void hs_transposeD(doubleMatrix * m){
	transposeD(m);
}

void hs_transposeF(floatMatrix * m){
	transposeF(m);
}

void hs_transposeI(intMatrix * m){
	transposeI(m);
}

int hs_printmD(const doubleMatrix * m){
	return printmD(m).code;
}

int hs_printmF(const floatMatrix * m){
	return printmF(m).code;
}

int hs_printmI(const intMatrix * m){
	return printmI(m).code;
}

double hs_determinantD(const doubleMatrix *m, int row){
	return determinantD(m, row);
}

float hs_determinantF(const floatMatrix *m, int row){
	return determinantF(m, row);
}

int hs_determinantI(const intMatrix *m, int row){
	return determinantI(m, row);
}

int hs_cofactorD(doubleMatrix * dest, const doubleMatrix *a){
	return cofactorD(dest, a).code;
}

int hs_cofactorF(floatMatrix * dest, const floatMatrix *a){
	return cofactorF(dest, a).code;
}

int hs_cofactorI(intMatrix * dest, const intMatrix *a){
	return cofactorI(dest, a).code;
}

int hs_multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b){
	return multiplyD(dest, a, b).code;
}

int hs_multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b){
	return multiplyF(dest, a, b).code;
}

int hs_multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b){
	return multiplyI(dest, a, b).code;
}

double hs_dotProductD(const doubleMatrix *a, const doubleMatrix *b){
	return hs_dotProductD(a, b);
}

float hs_dotProductF(const floatMatrix *a, const floatMatrix *b){
	return hs_dotProductF(a, b);
}

int hs_dotProductI(const intMatrix *a, const intMatrix *b){
	return hs_dotProductI(a, b);
}


