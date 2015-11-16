/*
Created by Andrew M. Hall
*/

#include <string.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrix-test.h"

const double testDataD[] =
{
    -10.5002, 	3.2864, 	11.9539, 	-13.3095, 	-4.5435,
    12.0767, 	7.8707, 	-15.5706, 	3.0786, 	14.557,
    13.8838, 	12.7521, 	-13.3348, 	15.6591, 	15.5848,
    -11.0018, 	-11.6042, 	6.064, 	-0.0162, 	-5.6902,
    -9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068,
    4.6232, 	2.7055, 	-3.7946, 	12.207, 	-4.3025,
    -3.3336, 	1.9573, 	-14.926, 	-3.6502, 	2.0388,
};
const float testDataF[] =
{
    -10.5002, 	3.2864, 	11.9539, 	-13.3095, 	-4.5435,
    12.0767, 	7.8707, 	-15.5706, 	3.0786, 	14.557,
    13.8838, 	12.7521, 	-13.3348, 	15.6591, 	15.5848,
    -11.0018, 	-11.6042, 	6.064, 	-0.0162, 	-5.6902,
    -9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068,
    4.6232, 	2.7055, 	-3.7946, 	12.207, 	-4.3025,
    -3.3336, 	1.9573, 	-14.926, 	-3.6502, 	2.0388,
};
const int testDataI[] =
{
    4, 	1, 	-10, 	5, 	13,
    -5, 	8, 	11, 	5, 	15,
    9, 	4, 	-9, 	9, 	4,
    5, 	-2, 	9, 	-2, 	-5,
    -6, 	5, 	14, 	11, 	5,
    1, 	-10, 	-6, 	6, 	1,
    15, 	0, 	9, 	7, 	-5
};

const double testSquareD[25] =
{
    6, 	-3, 	12, 	-10, 	8,
    2, 	10, 	-5, 	-1, 	12,
    14, 	-13, 	5, 	8, 	10,
    -8, 	14, 	2, 	2, 	-7,
    -1, 	10, 	2, 	-10, 	14
};
const float testSquareF[25] =
{
    6, 	-3, 	12, 	-10, 	8,
    2, 	10, 	-5, 	-1, 	12,
    14, 	-13, 	5, 	8, 	10,
    -8, 	14, 	2, 	2, 	-7,
    -1, 	10, 	2, 	-10, 	14
};
const int testSquareI[25] =
{
    6, 	-3, 	12, 	-10, 	8,
    2, 	10, 	-5, 	-1, 	12,
    14, 	-13, 	5, 	8, 	10,
    -8, 	14, 	2, 	2, 	-7,
    -1, 	10, 	2, 	-10, 	14
};

TEST(test_constructors)
{
    /*
    Assumptions:
        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Constructor Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Constructor Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
   CATCH_ERROR("Constructor Test Failure:", e);

    ASSERT_MSG(d.width == 5,
               "Constructor Test Failure:\n\t matrixD did not create matrix with width 5\n");
    ASSERT_MSG(d.height == 7,
               "Constructor Test Failure:\n\t matrixD did not create matrix with height 7\n");

    ASSERT_MSG(f.width == 5,
               "Constructor Test Failure:\n\t matrixF did not create matrix with width 5\n");
    ASSERT_MSG(f.height == 7,
               "Constructor Test Failure:\n\t matrixF did not create matrix with height 7\n");

    ASSERT_MSG(i.height == 7,
               "Constructor Test Failure:\n\t matrixI did not create matrix with height 7\n");
    ASSERT_MSG(i.width == 5,
               "Constructor Test Failure:\n\t matrixI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++)
    {
        ASSERT_MSG3(testDataD[c] == d.data[c],
                    "Constructor Test Failure:\n\t matrixD did not return correct value at i = %d, %f != %f\n", c, testDataD[c], d.data[c]);

        ASSERT_MSG3(testDataF[c] == f.data[c],
                    "Constructor Test Failure:\n\t matrixF did not return correct value at i = %d, %f != %f\n", c, testDataF[c], f.data[c]);

        ASSERT_MSG3(testDataI[c] == i.data[c],
                    "Constructor Test Failure:\n\t matrixI did not return correct value at i = %d, %d != %d\n", c, testDataI[c], i.data[c]);
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    return TEST_SUCCESS;
}

TEST(test_null_constructors)
{
    /*
    Assumptions:
        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixNullD(&d, 5, 7);
    CATCH_ERROR("Constructor Test Failure:", e);
    e = matrixNullF(&f, 5, 7);
    CATCH_ERROR("Constructor Test Failure:", e);
    e = matrixNullI(&i, 5, 7);
    CATCH_ERROR("Constructor Test Failure:", e);

    ASSERT_MSG(d.width == 5,
               "Null Constructor Test Failure:\n\t matrixNullD did not create matrix with width 5\n");
    ASSERT_MSG(d.height == 7,
               "Null Constructor Test Failure:\n\t matrixNullD did not create matrix with height 7\n");

    ASSERT_MSG(f.width == 5,
               "Null Constructor Test Failure:\n\t matrixNullF did not create matrix with width 5\n");
    ASSERT_MSG(f.height == 7,
               "Null Constructor Test Failure:\n\t matrixNullF did not create matrix with height 7\n");

    ASSERT_MSG(i.height == 7,
               "Null Constructor Test Failure:\n\t matrixNullI did not create matrix with height 7\n");
    ASSERT_MSG(i.width == 5,
               "Null Constructor Test Failure:\n\t matrixNullI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++)
    {
        ASSERT_MSG2(0 == d.data[c],
                    "Null Constructor Test Failure:\n\t matrixNullD did not return correct value at i = %d, 0 != %f\n", c, d.data[c]);

        ASSERT_MSG2(0 == f.data[c],
                    "Null Constructor Test Failure:\n\t matrixNullF did not return correct value at i = %d, 0 != %f\n", c, f.data[c]);

        ASSERT_MSG2(0 == i.data[c],
                    "Null Constructor Test Failure:\n\t matrixNullI did not return correct value at i = %d, 0 != %d\n", c, i.data[c]);
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    return TEST_SUCCESS;
}

TEST(test_copy_constructor)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI
    */

    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix cpyD = DEFAULT_MATRIX;
    floatMatrix cpyF = DEFAULT_MATRIX;
    intMatrix cpyI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Copy Constructor Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Copy Constructor Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Copy Constructor Test Failure:", e);

    e = matrixCopyConsD(&cpyD, &d);
    CATCH_ERROR("Copy Constructor Test Failure:", e);
    e = matrixCopyConsF(&cpyF, &f);
    CATCH_ERROR("Copy Constructor Test Failure:", e);
    e = matrixCopyConsI(&cpyI, &i);
    CATCH_ERROR("Copy Constructor Test Failure:", e);

    ASSERT_MSG(cmpD(&cpyD, &d), "Copy Constructor Test Failure: matrixCopyD failed to create an equivalent matrix\n")
    ASSERT_MSG(cmpF(&cpyF, &f), "Copy Constructor Test Failure: matrixCopyF failed to create an equivalent matrix\n")
    ASSERT_MSG(cmpI(&cpyI, &i), "Copy Constructor Test Failure: matrixCopyI failed to create an equivalent matrix\n")

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&cpyD);
    destroymF(&cpyF);
    destroymI(&cpyI);

    return TEST_SUCCESS;
}

TEST(test_at)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixCopyConsD
        matrixCopyConsF
        matrixCopyCons
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testSquareD, 5, 5);
    CATCH_ERROR("At Test Failure:", e);
    e = matrixF(&f, testSquareF, 5, 5);
    CATCH_ERROR("At Test Failure:", e);
    e = matrixI(&i, testSquareI, 5, 5);
    CATCH_ERROR("At Test Failure:", e);

    ASSERT_MSG1(atD(&d, 3, 4) == -10, "At Test Failure: atD(&d, 3, 4) returned %f instead of the required -10", atD(&d, 3, 4));
    ASSERT_MSG1(atF(&f, 3, 4) == -10, "At Test Failure: atF(&d, 3, 4) returned %f instead of the required -10", atF(&f, 3, 4));
    ASSERT_MSG1(atI(&i, 3, 4) == -10, "At Test Failure: atI(&d, 3, 4) returned %d instead of the required -10", atI(&i, 3, 4));

    doubleMatrix cpyD = DEFAULT_MATRIX;
    floatMatrix cpyF = DEFAULT_MATRIX;
    intMatrix cpyI = DEFAULT_MATRIX;

    e = matrixCopyConsD(&cpyD, &d);
    CATCH_ERROR("At Test Failure:", e);
    e = matrixCopyConsF(&cpyF, &f);
    CATCH_ERROR("At Test Failure:", e);
    e = matrixCopyConsI(&cpyI, &i);
    CATCH_ERROR("At Test Failure:", e);

    e = insertAtD(&cpyD, 42, 3, 4);
    CATCH_ERROR("At Test Failure:", e);
    e = insertAtF(&cpyF, 42, 3, 4);
    CATCH_ERROR("At Test Failure:", e);
    e = insertAtI(&cpyI, 42, 3, 4);
    CATCH_ERROR("At Test Failure:", e);

    ASSERT_MSG1(atD(&cpyD, 3, 4) == 42, "At Test Failure: insertAtD(&tmpD, 3, 4) returned %f instead of the required -10", atD(&cpyD, 3, 4));
    ASSERT_MSG1(atF(&cpyF, 3, 4) == 42, "At Test Failure: insertAtF(&tmpF, 3, 4) returned %f instead of the required -10", atF(&cpyF, 3, 4));
    ASSERT_MSG1(atI(&cpyI, 3, 4) == 42, "At Test Failure: insertAtI(&tmpI, 3, 4) returned %d instead of the required -10", atI(&cpyI, 3, 4));

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&cpyD);
    destroymF(&cpyF);
    destroymI(&cpyI);

    return TEST_SUCCESS;
}

TEST(test_matrix_cpy)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Matrix Copy Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Matrix Copy Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Matrix Copy Test Failure:", e);

    doubleMatrix d_cpy = DEFAULT_MATRIX;
    floatMatrix f_cpy = DEFAULT_MATRIX;
    intMatrix i_cpy = DEFAULT_MATRIX;

    e = matrixCopyConsD(&d_cpy, &d);
    CATCH_ERROR("Matrix Copy Test Failure:", e);
    e = matrixCopyConsF(&f_cpy, &f);
    CATCH_ERROR("Matrix Copy Test Failure:", e);
    e = matrixCopyConsI(&i_cpy, &i);
    CATCH_ERROR("Matrix Copy Test Failure:", e);

    ASSERT_MSG(d_cpy.width == 5,
               "Matrix Copy Test Failure:\n\t matrixcpyD did not create matrix with width 5\n");
    ASSERT_MSG(d_cpy.height == 7,
               "Matrix Copy Test Failure:\n\t matrixcpyD did not create matrix with height 7\n");

    ASSERT_MSG(f_cpy.width == 5,
               "Matrix Copy Test Failure:\n\t matrixcpyF did not create matrix with width 5\n");
    ASSERT_MSG(f_cpy.height == 7,
               "Matrix Copy Test Failure:\n\t matrixcpyF did not create matrix with height 7\n");

    ASSERT_MSG(i_cpy.height == 7,
               "Matrix Copy Test Failure:\n\t matrixcpyI did not create matrix with height 7\n");
    ASSERT_MSG(i_cpy.width == 5,
               "Matrix Copy Test Failure:\n\t matrixcpyI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++)
    {
        ASSERT_MSG3(testDataD[c] == d.data[c],
                    "Matrix Copy Test Failure:\n\t matrixcpyD did not return correct value at i = %d, %f != %f\n", c, d_cpy.data[c], d.data[c]);

        ASSERT_MSG3(testDataF[c] == f.data[c],
                    "Matrix Copy Test Failure:\n\t matrixcpyF did not return correct value at i = %d, %f != %f\n", c, f_cpy.data[c], f.data[c]);

        ASSERT_MSG3(testDataI[c] == i.data[c],
                    "Matrix Copy Test Failure:\n\t matrixcpyI did not return correct value at i = %d, %d != %d\n", c, i_cpy.data[c], i.data[c]);
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&d_cpy);
    destroymF(&f_cpy);
    destroymI(&i_cpy);

    return TEST_SUCCESS;
}

TEST(test_determinant)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    const double detD[] =
    {
        3,  8,
        4,  6
    };
    const float detF[] =
    {
        3,  8,
        4,  6
    };
    const int detI[] =
    {
        3,  8,
        4,  6
    };
    MatrixError e;
    e = matrixD(&d, detD, 2, 2);
    CATCH_ERROR("Determinant Test Failure:", e);
    e = matrixF(&f, detF, 2, 2);
    CATCH_ERROR("Determinant Test Failure:", e);
    e = matrixI(&i, detI, 2, 2);
    CATCH_ERROR("Determinant Test Failure:", e);

    //Test determinant2x2
    double retD = determinant2x2D(&d);
    ASSERT_MSG1(retD == -14, "Determinant Test Failure: determinant2x2D returned %f when it was supposed to return -14\n", retD);
    float retF = determinant2x2F(&f);
    ASSERT_MSG1(retF == -14, "Determinant Test Failure: determinant2x2F returned %f when it was supposed to return -14\n", retF);
    int retI = determinant2x2I(&i);
    ASSERT_MSG1(retI == -14, "Determinant Test Failure: determinant2x2I returned %d when it was supposed to return -14\n", retI);

    e = matrixD(&d, testSquareD, 5, 5);
    CATCH_ERROR("Determinant Test Failure:", e);
    e = matrixF(&f, testSquareF, 5, 5);
    CATCH_ERROR("Determinant Test Failure:", e);
    e = matrixI(&i, testSquareI, 5, 5);
    CATCH_ERROR("Determinant Test Failure:", e);

    retD = determinantD(&d, 0);
    ASSERT_MSG1(retD == -183011, "Determinant Test Failure: determinantD returned %f when it was supposed to return -183011\n", retD);
    retF = determinantF(&f, 0);
    ASSERT_MSG1(retF == -183011, "Determinant Test Failure: determinantF returned %f when it was supposed to return -183011\n", retF);
    retI = determinantI(&i, 0);
    ASSERT_MSG1(retI == -183011, "Determinant Test Failure: determinantI returned %d when it was supposed to return -183011\n", retI);

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    return TEST_SUCCESS;
}

TEST(test_matrix_comparison)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d1 = DEFAULT_MATRIX;
    floatMatrix f1 = DEFAULT_MATRIX;
    intMatrix i1 = DEFAULT_MATRIX;

    doubleMatrix d2 = DEFAULT_MATRIX;
    floatMatrix f2 = DEFAULT_MATRIX;
    intMatrix i2 = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d1, testDataD, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);
    e = matrixD(&d2, testDataD, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);
    e = matrixF(&f1, testDataF, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);
    e = matrixF(&f2, testDataF, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);
    e = matrixI(&i1, testDataI, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);
    e = matrixI(&i2, testDataI, 5, 7);
    CATCH_ERROR("Matrix Comparison Test Failure:", e);

    ASSERT_MSG(cmpD(&d1, &d2), "Matrix Comparison Test Failure: cmpD failed to return true for two equivalent matrices\n");
    ASSERT_MSG(cmpF(&f1, &f2), "Matrix Comparison Test Failure: cmpF failed to return true for two equivalent matrices\n");
    ASSERT_MSG(cmpI(&i1, &i2), "Matrix Comparison Test Failure: cmpI failed to return true for two equivalent matrices\n");

    d2.data[5 * d2.width + 3] = 42;
    ASSERT_MSG(!cmpD(&d1, &d2), "Matrix Comparison Test Failure: cmpD failed to return false for two distinct matrices\n");
    f2.data[5 * f2.width + 3] = 42;
    ASSERT_MSG(!cmpF(&f1, &f2), "Matrix Comparison Test Failure: cmpF failed to return false for two distinct matrices\n");
    i2.data[5 * i2.width + 3] = 42;
    ASSERT_MSG(!cmpI(&i1, &i2), "Matrix Comparison Test Failure: cmpI failed to return false for two distinct matrices\n");

    destroymD(&d1);
    destroymD(&d2);
    destroymF(&f1);
    destroymF(&f2);
    destroymI(&i1);
    destroymI(&i2);

    return TEST_SUCCESS;
}

TEST(test_transpose)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        matrixcpyD
        matrixcpyF
        matrixcpyI

        cmpD
        cmpF
        cmpI

        destroymD
        destroymF
        destroymI
    */
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Transpose Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Transpose Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Transpose Test Failure:", e);

    doubleMatrix tranD = DEFAULT_MATRIX;
    floatMatrix tranF = DEFAULT_MATRIX;
    intMatrix tranI = DEFAULT_MATRIX;

    e = matrixCopyConsD(&tranD, &d);
    CATCH_ERROR("Transpose Test Failure:", e);
    e = matrixCopyConsF(&tranF, &f);
    CATCH_ERROR("Transpose Test Failure:", e);
    e = matrixCopyConsI(&tranI, &i);
    CATCH_ERROR("Transpose Test Failure:", e);

    transposeD(&tranD);
    transposeF(&tranF);
    transposeI(&tranI);

    ASSERT_MSG1(tranD.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranD.width);
    ASSERT_MSG1(tranF.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranF.width);
    ASSERT_MSG1(tranI.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranI.width);

    ASSERT_MSG1(tranD.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranD.height);
    ASSERT_MSG1(tranF.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranF.height);
    ASSERT_MSG1(tranI.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranI.height);

    transposeD(&tranD);
    transposeF(&tranF);
    transposeI(&tranI);

    ASSERT_MSG(cmpD(&d, &tranD), "Transpose Test Failure: transposeD does not obey the law - Transpose(Transpose(A)) != A\n");
    ASSERT_MSG(cmpF(&f, &tranF), "Transpose Test Failure: transposeF does not obey the law - Transpose(Transpose(A)) != A\n");
    ASSERT_MSG(cmpI(&i, &tranI), "Transpose Test Failure: transposeI does not obey the law - Transpose(Transpose(A)) != A\n");

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&tranD);
    destroymF(&tranF);
    destroymI(&tranI);
    return TEST_SUCCESS;
}

TEST(test_cofactor)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        cmpD
        cmpF
        cmpI

        destroymD
        destroymF
        destroymI
    */
    double testCofD[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    float testCofF[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    int testCofI[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix ansD = DEFAULT_MATRIX;
    floatMatrix ansF = DEFAULT_MATRIX;
    intMatrix ansI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testCofD, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixF(&f, testCofF, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixI(&i, testCofI, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);

    double ansDlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};
    float ansFlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};
    int ansIlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};

    doubleMatrix cofD = DEFAULT_MATRIX;
    floatMatrix cofF = DEFAULT_MATRIX;
    intMatrix cofI = DEFAULT_MATRIX;

    e = matrixD(&cofD, ansDlst, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixF(&cofF, ansFlst, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixI(&cofI, ansIlst, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);

    e = matrixNullD(&ansD, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixNullF(&ansF, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixNullI(&ansI, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);

    e = stdCofactorD(&ansD, &d);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = stdCofactorF(&ansF, &f);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = stdCofactorI(&ansI, &i);
    CATCH_ERROR("Cofactor Test Failure:", e);

    ASSERT_MSG(cmpD(&ansD, &cofD), "Cofactor Test Failure: stdCofactorD failed to correctly find cofactor for 3x3 matrix\n");
    ASSERT_MSG(cmpF(&ansF, &cofF), "Cofactor Test Failure: stdCofactorF failed to correctly find cofactor for 3x3 matrix\n");
    ASSERT_MSG(cmpI(&ansI, &cofI), "Cofactor Test Failure: stdCofactorI failed to correctly find cofactor for 3x3 matrix\n");

    e = matrixNullD(&ansD, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixNullF(&ansF, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = matrixNullI(&ansI, 3, 3);
    CATCH_ERROR("Cofactor Test Failure:", e);

    e = paraCofactorD(&ansD, &d);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = paraCofactorF(&ansF, &f);
    CATCH_ERROR("Cofactor Test Failure:", e);
    e = paraCofactorI(&ansI, &i);
    CATCH_ERROR("Cofactor Test Failure:", e);

    ASSERT_MSG(cmpD(&ansD, &cofD), "Cofactor Test Failure: paraCofactorD failed to correctly find cofactor for 3x3 matrix\n");
    ASSERT_MSG(cmpF(&ansF, &cofF), "Cofactor Test Failure: paraCofactorF failed to correctly find cofactor for 3x3 matrix\n");
    ASSERT_MSG(cmpI(&ansI, &cofI), "Cofactor Test Failure: paraCofactorI failed to correctly find cofactor for 3x3 matrix\n");

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&ansD);
    destroymF(&ansF);
    destroymI(&ansI);

    destroymD(&cofD);
    destroymF(&cofF);
    destroymI(&cofI);

    return TEST_SUCCESS;
}

TEST(test_adjoint)
{
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI

        matrixNullD
        matrixNullF
        matrixNullI

        cmpD
        cmpF
        cmpI

        multiplyD
        multiplyF
        multiplyI

        destroymD
        destroymF
        destroymI
    */
    doubleMatrix ansD = DEFAULT_MATRIX;
    floatMatrix ansF = DEFAULT_MATRIX;
    intMatrix ansI = DEFAULT_MATRIX;

    double ansDlst[] = {2, 2, 0, -2, 3, 10, 2, -3, 0};
    float ansFlst[] = {2, 2, 0, -2, 3, 10, 2, -3, 0};
    int ansIlst[] = {2, 2, 0, -2, 3, 10, 2, -3, 0};

    MatrixError e;
    e = matrixD(&ansD, ansDlst, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixF(&ansF, ansFlst, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixI(&ansI, ansIlst, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);

    double testAdjD[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    float testAdjF[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    int testAdjI[9] =
    {
        3,   0,  2,
        2,   0,  -2,
        0,   1,   1
    };

    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix adjD = DEFAULT_MATRIX;
    floatMatrix adjF = DEFAULT_MATRIX;
    intMatrix adjI = DEFAULT_MATRIX;

    e = matrixD(&d, testAdjD, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixF(&f, testAdjF, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixI(&i, testAdjI, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);

    e = matrixNullD(&adjD, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixNullF(&adjF, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = matrixNullI(&adjI, 3, 3);
    CATCH_ERROR("Adjoint Test Failure:", e);

    e = adjointD(&adjD, &d);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = adjointF(&adjF, &f);
    CATCH_ERROR("Adjoint Test Failure:", e);
    e = adjointI(&adjI, &i);
    CATCH_ERROR("Adjoint Test Failure:", e);

    ASSERT_MSG(cmpD(&ansD, &adjD), "Adjoint Test Failure: adjointD failed to correctly find adjoint for 3x3 matrix\n");
    ASSERT_MSG(cmpF(&ansF, &adjF), "Adjoint Test Failure: adjointF failed to correctly find adjoint for 3x3 matrix\n");
    ASSERT_MSG(cmpI(&ansI, &adjI), "Adjoint Test Failure: adjointI failed to correctly find adjoint for 3x3 matrix\n");

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&adjD);
    destroymF(&adjF);
    destroymI(&adjI);

    destroymD(&ansD);
    destroymF(&ansF);
    destroymI(&ansI);

    return TEST_SUCCESS;
}

TEST(test_invert)
{
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testSquareD, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);
    e = matrixF(&f, testSquareF, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);
    e = matrixI(&i, testSquareI, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);

    doubleMatrix ansD = DEFAULT_MATRIX;
    doubleMatrix ansF = DEFAULT_MATRIX;
    doubleMatrix ansI = DEFAULT_MATRIX;

    e = matrixNullD(&ansD, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);
    e = matrixNullD(&ansF, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);
    e = matrixNullD(&ansI, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);


    e = invertD(&ansD, &d);
    CATCH_ERROR("Invert Test Failure:", e);
    e = invertF(&ansF, &f);
    CATCH_ERROR("Invert Test Failure:", e);
    e = invertI(&ansI, &i);
    CATCH_ERROR("Invert Test Failure:", e);

    ASSERT_MSG(cmpD(&ansD, &ansF),
               "Invert Test Failure: invertD does noct produce matrix equivalent matrix to invertF\n");
    ASSERT_MSG(cmpD(&ansF, &ansI),
               "Invert Test Failure: invertF does not produce matrix equivalent matrix to invertI\n");

    doubleMatrix res = DEFAULT_MATRIX;

    e = matrixNullD(&res, 5, 5);
    CATCH_ERROR("Invert Test Failure:", e);

    e = multiplyD(&res, &ansD, &d);
    CATCH_ERROR("Invert Test Failure:", e);

    int x, y;
    for (y = 0; y < 5; y++)
    {
        for (x = 0; x < 5; x++)
        {
            if (x == y)
            {
                ASSERT_MSG3(atD(&res, x, y) - 1 <= 0.0001,
                            "Invert Test Failure: invert does not obey the law Invert(A) * A == I at (%d, %d) %f != 1\n", x, y, atD(&res, x, y));
            }
            else
            {
                ASSERT_MSG3(atD(&res, x, y) <= 0.0001,
                            "Invert Test Failure: invert does not obey the law Invert(A) * A == I at (%d, %d) %f != 0\n", x, y, atD(&res, x, y));
            }
        }
    }
    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&ansD);
    destroymD(&ansF);
    destroymD(&ansI);

    destroymD(&res);

    return TEST_SUCCESS;
}

TEST(test_scalar_multiply)
{
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Scalar Multiplication Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Scalar Multiplication Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Scalar Multiplication Test Failure:", e);

    scalarMultiplyD(&d, 42);
    scalarMultiplyF(&f, 42);
    scalarMultiplyI(&i, 42);

    int x, y;
    for (y = 0; y < 5; y++)
    {
        for (x = 0; x < 5; x++)
        {
            ASSERT_MSG4(testDataD[y * 5 + x]*42 == atD(&d, x, y),
                        "Scalar Multiplication Test Failure: scalarMultiplyD(A) did not multiply (%d, %d) by 42, %f != %f\n", x, y, atD(&d, x, y), testDataD[y * 5 + x]);
            ASSERT_MSG4(testDataF[y * 5 + x]*42 == atF(&f, x, y),
                        "Scalar Multiplication Test Failure: scalarMultiplyF(A) did not multiply (%d, %d) by 42, %f != %f\n", x, y, atF(&f, x, y), testDataF[y * 5 + x]);
            ASSERT_MSG4(testDataI[y * 5 + x]*42 == atI(&i, x, y),
                        "Scalar Multiplication Test Failure: scalarMultiplyD(A) did not multiply (%d, %d) by 42, %d != %d\n", x, y, atI(&i, x, y), testDataI[y * 5 + x]);
        }
    }

    negativeD(&d);
    negativeF(&f);
    negativeI(&i);

    for (y = 0; y < 5; y++)
    {
        for (x = 0; x < 5; x++)
        {
            ASSERT_MSG4(testDataD[y * 5 + x]*-42 == atD(&d, x, y),
                        "Scalar Multiplication Test Failure: negative(A) did not multiply (%d, %d) by -1, %f != %f\n", x, y, atD(&d, x, y), testDataD[y * 5 + x]);
            ASSERT_MSG4(testDataF[y * 5 + x]*-42 == atF(&f, x, y),
                        "Scalar Multiplication Test Failure: scalarMultiplyF(A) did not multiply (%d, %d) by -1, %f != %f\n", x, y, atF(&f, x, y), testDataF[y * 5 + x]);
            ASSERT_MSG4(testDataI[y * 5 + x]*-42 == atI(&i, x, y),
                        "Scalar Multiplication Test Failure: scalarMultiplyD(A) did not multiply (%d, %d) by -1, %d != %d\n", x, y, atI(&i, x, y), testDataI[y * 5 + x]);
        }
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    return TEST_SUCCESS;
}

TEST(test_addition)
{
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix cpyD = DEFAULT_MATRIX;
    floatMatrix cpyF = DEFAULT_MATRIX;
    intMatrix cpyI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);

    e = matrixCopyConsD(&cpyD, &d);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixCopyConsF(&cpyF, &f);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixCopyConsI(&cpyI, &i);
    CATCH_ERROR("Addition Test Failure:", e);

    e = addD(&d, &cpyD);
    CATCH_ERROR("Addition Test Failure:", e);
    e = addF(&f, &cpyF);
    CATCH_ERROR("Addition Test Failure:", e);
    e = addI(&i, &cpyI);
    CATCH_ERROR("Addition Test Failure:", e);

    scalarMultiplyD(&cpyD, 2.0);
    scalarMultiplyF(&cpyF, 2.0);
    scalarMultiplyI(&cpyI, 2);

    ASSERT_MSG(cmpD(&d, &cpyD), "Addition Test Failure: addD does not obey the law A + A == A x 2\n");
    ASSERT_MSG(cmpF(&f, &cpyF), "Addition Test Failure: addF does not obey the law A + A == A x 2\n");
    ASSERT_MSG(cmpI(&i, &cpyI), "Addition Test Failure: addI does not obey the law A + A == A x 2\n");

    subtractD(&d, &cpyD);
    subtractF(&f, &cpyF);
    subtractI(&i, &cpyI);

    e = matrixNullD(&cpyD, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixNullF(&cpyF, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);
    e = matrixNullI(&cpyI, 5, 7);
    CATCH_ERROR("Addition Test Failure:", e);

    ASSERT_MSG(cmpD(&d, &cpyD), "Subtraction Test Failure: subtractD does not obey the law A - A == 0\n");
    ASSERT_MSG(cmpF(&f, &cpyF), "Subtraction Test Failure: subtractF does not obey the law A - A == 0\n");
    ASSERT_MSG(cmpI(&i, &cpyI), "Subtraction Test Failure: subtractI does not obey the law A - A == 0\n");

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&cpyD);
    destroymF(&cpyF);
    destroymI(&cpyI);

    return TEST_SUCCESS;
}

TEST(test_dot_product)
{

    doubleMatrix d1 = DEFAULT_MATRIX;
    floatMatrix f1 = DEFAULT_MATRIX;
    intMatrix i1 = DEFAULT_MATRIX;

    doubleMatrix d2 = DEFAULT_MATRIX;
    floatMatrix f2 = DEFAULT_MATRIX;
    intMatrix i2 = DEFAULT_MATRIX;

    double testVectorD1[3] = {34,12,-3};
    float testVectorF1[3] = {34,12,-3};
    int testVectorI1[3] = {34,12,-3};

    double testVectorD2[3] = {-2,94,4};
    float testVectorF2[3] = {-2,94,4};
    int testVectorI2[3] = {-2,94,4};

    MatrixError e;
    e = matrixD(&d1, testVectorD1, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);
    e = matrixF(&f1, testVectorF1, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);
    e = matrixI(&i1, testVectorI1, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);

    e = matrixD(&d2, testVectorD2, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);
    e = matrixF(&f2, testVectorF2, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);
    e = matrixI(&i2, testVectorI2, 1, 3);
    CATCH_ERROR("Dot Product Test Failure:", e);

    ASSERT_MSG1(dotProductD(&d1, &d2) == 1048,
                "Dot Product Test Failure: dotProductD produced an incorrect dot product {34,12,-3} . {-2,94,4} = 1048, but dotProductD returned %f\n", dotProductD(&d1, &d2));
    ASSERT_MSG1(dotProductF(&f1, &f2) == 1048,
                "Dot Product Test Failure: dotProductF produced an incorrect dot product {34,12,-3} . {-2,94,4} = 1048, but dotProductF returned %f\n", dotProductF(&f1, &f2));
    ASSERT_MSG1(dotProductI(&i1, &i2) == 1048,
                "Dot Product Test Failure: dotProductI produced an incorrect dot product {34,12,-3} . {-2,94,4} = 1048, but dotProductI returned %d\n", dotProductI(&i1, &i2));

    destroymD(&d1);
    destroymF(&f1);
    destroymI(&i1);

    destroymD(&d2);
    destroymF(&f2);
    destroymI(&i2);

    return TEST_SUCCESS;
}

TEST(test_cross_product)
{

    doubleMatrix d1 = DEFAULT_MATRIX;
    floatMatrix f1 = DEFAULT_MATRIX;
    intMatrix i1 = DEFAULT_MATRIX;

    doubleMatrix d2 = DEFAULT_MATRIX;
    floatMatrix f2 = DEFAULT_MATRIX;
    intMatrix i2 = DEFAULT_MATRIX;

    doubleMatrix ansD = DEFAULT_MATRIX;
    floatMatrix ansF = DEFAULT_MATRIX;
    intMatrix ansI = DEFAULT_MATRIX;

    double testVectorD1[3] = {34,12,-3};
    float testVectorF1[3] = {34,12,-3};
    int testVectorI1[3] = {34,12,-3};

    double testVectorD2[3] = {-2,94,4};
    float testVectorF2[3] = {-2,94,4};
    int testVectorI2[3] = {-2,94,4};

    double answerVectorD[3] = {330, -130, 3220};
    float answerVectorF[3] = {330, -130, 3220};
    int answerVectorI[3] = {330, -130, 3220};

    MatrixError e;
    e = matrixD(&d1, testVectorD1, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixF(&f1, testVectorF1, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixI(&i1, testVectorI1, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);

    e = matrixD(&d2, testVectorD2, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixF(&f2, testVectorF2, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixI(&i2, testVectorI2, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);

    e = matrixNullD(&ansD, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixNullF(&ansF, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = matrixNullI(&ansI, 1, 3);
    CATCH_ERROR("Cross Product Test Failure:", e);

    e = crossProductD(&ansD, &d1, &d2);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = crossProductF(&ansF, &f1, &f2);
    CATCH_ERROR("Cross Product Test Failure:", e);
    e = crossProductI(&ansI, &i1, &i2);
    CATCH_ERROR("Cross Product Test Failure:", e);

    int y;
    for (y = 0; y < 3; y++)
    {
        ASSERT_MSG2(atD(&ansD, 0, y) == answerVectorD[y],
                    "Cross Product Test Failure: crossProductD produced an incorrect cross product {34,12,-3} x {-2,94,4} = {330, -130, 3220}, but crossProductD returned %f at element %d\n", atD(&ansD, 0, y), y);
        ASSERT_MSG2(atF(&ansF, 0, y) == answerVectorF[y],
                    "Cross Product Test Failure: crossProductF produced an incorrect cross product {34,12,-3} x {-2,94,4} = {330, -130, 3220}, but crossProductF returned %f at element %d\n", atF(&ansF, 0, y), y);
        ASSERT_MSG2(atI(&ansI, 0, y) == answerVectorI[y],
                    "Cross Product Test Failure: crossProductI produced an incorrect cross product {34,12,-3} x {-2,94,4} = {330, -130, 3220}, but crossProductI returned %d at element %d\n", atI(&ansI, 0, y), y);
    }

    destroymD(&d1);
    destroymF(&f1);
    destroymI(&i1);

    destroymD(&d2);
    destroymF(&f2);
    destroymI(&i2);

    destroymD(&ansD);
    destroymF(&ansF);
    destroymI(&ansI);

    return TEST_SUCCESS;
}

TEST(test_matrix_multiply)
{
    doubleMatrix d1 = DEFAULT_MATRIX;
    floatMatrix f1 = DEFAULT_MATRIX;
    intMatrix i1 = DEFAULT_MATRIX;

    doubleMatrix d2 = DEFAULT_MATRIX;
    floatMatrix f2 = DEFAULT_MATRIX;
    intMatrix i2 = DEFAULT_MATRIX;

    double testD1[6] =
    {
        1, 2, 3,
        4, 5, 6
    };
    float testF1[6] =
    {
        1, 2, 3,
        4, 5, 6
    };
    int testI1[6] =
    {
        1, 2, 3,
        4, 5, 6
    };

    double testD2[6] =
    {
        7, 8,
        9, 10,
        11, 12
    };
    float testF2[6] =
    {
        7, 8,
        9, 10,
        11, 12
    };
    int testI2[6] =
    {
        7, 8,
        9, 10,
        11, 12
    };

    MatrixError e;
    e = matrixD(&d1, testD1, 3, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixF(&f1, testF1, 3, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixI(&i1, testI1, 3, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    e = matrixD(&d2, testD2, 2, 3);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixF(&f2, testF2, 2, 3);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixI(&i2, testI2, 2, 3);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    double ansD[4] =
    {
        58, 64,
        139, 154
    };
    float ansF[4] =
    {
        58, 64,
        139, 154
    };
    int ansI[4] =
    {
        58, 64,
        139, 154
    };

    doubleMatrix mulD = DEFAULT_MATRIX;
    floatMatrix mulF = DEFAULT_MATRIX;
    intMatrix mulI = DEFAULT_MATRIX;

    e = matrixNullD(&mulD, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixNullF(&mulF, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixNullI(&mulI, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    e = paraMultiplyD(&mulD, &d1, &d2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = paraMultiplyF(&mulF, &f1, &f2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = paraMultiplyI(&mulI, &i1, &i2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    int x, y;
    for (y = 0; y < 2; y++)
    {
        for (x = 0; x < 2; x++)
        {
            ASSERT_MSG4(ansD[y*2 + x] == atD(&mulD, x, y),
                        "Matrix Multiply Test Failure: paraMultiplyD did not correctly multiply matrices, element (%d, %d) was incorrect, %f != %f",
                        x, y, ansD[y*2 + x], atD(&mulD, x, y));
            ASSERT_MSG4(ansF[y*2 + x] == atF(&mulF, x, y),
                        "Matrix Multiply Test Failure: paraMultiplyF did not correctly multiply matrices, element (%d, %d) was incorrect, %f != %f",
                        x, y, ansF[y*2 + x], atF(&mulF, x, y));
            ASSERT_MSG4(ansI[y*2 + x] == atI(&mulI, x, y),
                        "Matrix Multiply Test Failure: paraMultiplyI did not correctly multiply matrices, element (%d, %d) was incorrect, %d != %d",
                        x, y, ansI[y*2 + x], atI(&mulI, x, y));
        }
    }

    e = matrixNullD(&mulD, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixNullF(&mulF, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = matrixNullI(&mulI, 2, 2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    e = stdMultiplyD(&mulD, &d1, &d2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = stdMultiplyF(&mulF, &f1, &f2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);
    e = stdMultiplyI(&mulI, &i1, &i2);
    CATCH_ERROR("Matrix Multiply Test Failure:", e);

    for (y = 0; y < 2; y++)
    {
        for (x = 0; x < 2; x++)
        {
            ASSERT_MSG4(ansD[y*2 + x] == atD(&mulD, x, y),
                        "Matrix Multiply Test Failure: stdMultiplyD did not correctly multiply matrices, element (%d, %d) was incorrect, %f != %f",
                        x, y, ansD[y*2 + x], atD(&mulD, x, y));
            ASSERT_MSG4(ansF[y*2 + x] == atF(&mulF, x, y),
                        "Matrix Multiply Test Failure: stdMultiplyF did not correctly multiply matrices, element (%d, %d) was incorrect, %f != %f",
                        x, y, ansF[y*2 + x], atF(&mulF, x, y));
            ASSERT_MSG4(ansI[y*2 + x] == atI(&mulI, x, y),
                        "Matrix Multiply Test Failure: stdMultiplyI did not correctly multiply matrices, element (%d, %d) was incorrect, %d != %d",
                        x, y, ansI[y*2 + x], atI(&mulI, x, y));
        }
    }

    destroymD(&d1);
    destroymF(&f1);
    destroymI(&i1);

    destroymD(&d2);
    destroymF(&f2);
    destroymI(&i2);

    destroymD(&mulD);
    destroymF(&mulF);
    destroymI(&mulI);

    return TEST_SUCCESS;
}

TEST(test_get_row)
{
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix rowD = DEFAULT_MATRIX;
    floatMatrix rowF = DEFAULT_MATRIX;
    intMatrix rowI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Get Row Test Failure:", e);

    e = matrixNullD(&rowD, 5, 1);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = matrixNullF(&rowF, 5, 1);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = matrixNullI(&rowI, 5, 1);
    CATCH_ERROR("Get Row Test Failure:", e);

    double rowListD[5] = {-9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068};
    float rowListF[5] = {-9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068};
    int rowListI[5] = { -6, 5, 14, 11, 5,};

    e = getRowD(&rowD, &d, 4);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = getRowF(&rowF, &f, 4);
    CATCH_ERROR("Get Row Test Failure:", e);
    e = getRowI(&rowI, &i, 4);
    CATCH_ERROR("Get Row Test Failure:", e);

    int x;
    for (x = 0; x < 5; x++){
        ASSERT_MSG3(rowListD[x] == atD(&rowD, x, 0),
                    "Get Row Test Failure: getRowD returned incorrect row element at (%d, 1), %f != %f\n", x, rowListD[x], atD(&rowD, x, 0));
        ASSERT_MSG3(rowListF[x] == atF(&rowF, x, 0),
                    "Get Row Test Failure: getRowF returned incorrect row element at (%d, 1), %f != %f\n", x, rowListF[x], atF(&rowF, x, 0));
        ASSERT_MSG3(rowListI[x] == atI(&rowI, x, 0),
                    "Get Row Test Failure: getRowI returned incorrect row element at (%d, 1), %d != %d\n", x, rowListI[x], atI(&rowI, x, 0));
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&rowD);
    destroymF(&rowF);
    destroymI(&rowI);

    return TEST_SUCCESS;
}

TEST(test_get_col)
{
    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix colD = DEFAULT_MATRIX;
    floatMatrix colF = DEFAULT_MATRIX;
    intMatrix colI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = matrixF(&f, testDataF, 5, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = matrixI(&i, testDataI, 5, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);

    e = matrixNullD(&colD, 1, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = matrixNullF(&colF, 1, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = matrixNullI(&colI, 1, 7);
    CATCH_ERROR("Get Colum Test Failure:", e);

    double colListD[7] = {-4.5435, 14.557, 15.5848, -5.6902, 7.068, -4.3025, 2.0388};
    float colListF[7] = {-4.5435, 14.557, 15.5848, -5.6902, 7.068, -4.3025, 2.0388};
    int colListI[7] = { 13, 15, 4, -5, 5, 1, -5 };

    e = getColD(&colD, &d, 4);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = getColF(&colF, &f, 4);
    CATCH_ERROR("Get Colum Test Failure:", e);
    e = getColI(&colI, &i, 4);
    CATCH_ERROR("Get Colum Test Failure:", e);

    int y;
    for (y = 0; y < 7; y++){
        ASSERT_MSG3(colListD[y] == atD(&colD, y, 0),
                    "Get Colum Test Failure: getColD returned incorrect row element at (1, %d), %f != %f\n", y, colListD[y], atD(&colD, y, 0));
        ASSERT_MSG3(colListF[y] == atF(&colF, y, 0),
                    "Get Colum Test Failure: getColF returned incorrect row element at (1, %d), %f != %f\n", y, colListF[y], atF(&colF, y, 0));
        ASSERT_MSG3(colListI[y] == atI(&colI, y, 0),
                    "Get Colum Test Failure: getColI returned incorrect row element at (1, %d), %d != %d\n", y, colListI[y], atI(&colI, y, 0));
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&colD);
    destroymF(&colF);
    destroymI(&colI);

    return TEST_SUCCESS;
}

TEST(matrix_suite)
{
    int count_passed = 0;
    RUN_TEST("Constructors", test_constructors(), count_passed);
    RUN_TEST("Null Constructors", test_null_constructors(), count_passed);
    RUN_TEST("Copy Constructors", test_copy_constructor(), count_passed);
    RUN_TEST("At", test_at(), count_passed);
    RUN_TEST("Matrix Copy", test_matrix_cpy(), count_passed);
    RUN_TEST("Determinant", test_determinant(), count_passed);
    RUN_TEST("Matrix Comparison", test_matrix_comparison(), count_passed);
    RUN_TEST("Matrix Transpose", test_transpose(), count_passed);

    RUN_TEST("Matrix Cofactor", test_cofactor(), count_passed);

    RUN_TEST("Adjoint", test_adjoint(), count_passed);
    RUN_TEST("Invert", test_invert(), count_passed);
    RUN_TEST("Scalar Multiplication", test_scalar_multiply(), count_passed);
    RUN_TEST("Addition", test_addition(), count_passed);
    RUN_TEST("Dot Product", test_dot_product(), count_passed);
    RUN_TEST("Cross Product", test_cross_product(), count_passed);
    RUN_TEST("Matrix Multiplication", test_matrix_multiply(), count_passed);
    RUN_TEST("Get Row", test_get_row(), count_passed);
    RUN_TEST("Get Colum", test_get_col(), count_passed);

    return count_passed;
}
