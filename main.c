/*
Written by Andrew M. Hall
*/

#include <stdio.h>
#include "matrix.h"

int main(){
    int d[10000] = {
3, 	0, 	0, 	3, 	5, 	6, 	12, 	-14, 	8, 	14, 	-6,
15, 	12, 	11, 	5, 	10, 	7, 	2, 	8, 	-4, 	5, 	-14,
13, 	4, 	5, 	4, 	15, 	6, 	3, 	5, 	2, 	9, 	-2,
0, 	11, 	1, 	-5, 	8, 	14, 	6, 	12, 	-6, 	-9, 	12,
7, 	11, 	-13, 	9, 	7, 	8, 	-14, 	10, 	0, 	11, 	4,
-7, 	8, 	4, 	9, 	0, 	15, 	3, 	4, 	2, 	11, 	14,
3, 	-3, 	-3, 	6, 	-7, 	10, 	15, 	-1, 	13, 	-15, 	-3,
10, 	2, 	-13, 	8, 	8, 	13, 	8, 	6, 	14, 	-7, 	-15,
10, 	12, 	3, 	12, 	14, 	9, 	6, 	8, 	-11, 	0, 	15,
9, 	5, 	-12, 	1, 	-11, 	0, 	14, 	-13, 	12, 	0, 	8,
0, 	11, 	2, 	13, 	0, 	14, 	14, 	13, 	2, 	-3, 	2
};

    intMatrix m;
    matrixI(&m, d, 11, 11);

    doubleMatrix inv;
    matrixNullD(&inv, 11, 11);

    invertI(&inv, &m);
    printmD(&inv);
    destroymI(&m);
    destroymD(&inv);
    return 0;
}
