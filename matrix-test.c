/*
Created by Andrew M. Hall
*/

#include <string.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrix-test.h"

    const double testDataD[] = {
-10.5002, 	3.2864, 	11.9539, 	-13.3095, 	-4.5435,
12.0767, 	7.8707, 	-15.5706, 	3.0786, 	14.557,
13.8838, 	12.7521, 	-13.3348, 	15.6591, 	15.5848,
-11.0018, 	-11.6042, 	6.064, 	-0.0162, 	-5.6902,
-9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068,
4.6232, 	2.7055, 	-3.7946, 	12.207, 	-4.3025,
-3.3336, 	1.9573, 	-14.926, 	-3.6502, 	2.0388,
};
    const float testDataF[] = {
-10.5002, 	3.2864, 	11.9539, 	-13.3095, 	-4.5435,
12.0767, 	7.8707, 	-15.5706, 	3.0786, 	14.557,
13.8838, 	12.7521, 	-13.3348, 	15.6591, 	15.5848,
-11.0018, 	-11.6042, 	6.064, 	-0.0162, 	-5.6902,
-9.1597, 	6.5219, 	7.0286, 	10.0581, 	7.068,
4.6232, 	2.7055, 	-3.7946, 	12.207, 	-4.3025,
-3.3336, 	1.9573, 	-14.926, 	-3.6502, 	2.0388,
};
    const int testDataI[] = {
4, 	1, 	-10, 	5, 	13,
-5, 	8, 	11, 	5, 	15,
9, 	4, 	-9, 	9, 	4,
5, 	-2, 	9, 	-2, 	-5,
-6, 	5, 	14, 	11, 	5,
1, 	-10, 	-6, 	6, 	1,
15, 	0, 	9, 	7, 	-5
};

    const double testSquareD[25] = {
6, 	-3, 	12, 	-10, 	8,
2, 	10, 	-5, 	-1, 	12,
14, 	-13, 	5, 	8, 	10,
-8, 	14, 	2, 	2, 	-7,
-1, 	10, 	2, 	-10, 	14
};
    const float testSquareF[25] = {
6, 	-3, 	12, 	-10, 	8,
2, 	10, 	-5, 	-1, 	12,
14, 	-13, 	5, 	8, 	10,
-8, 	14, 	2, 	2, 	-7,
-1, 	10, 	2, 	-10, 	14
};
    const int testSquareI[25] = {
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
    assert_msg1(e == SUCCESS, "Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Constructor Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(d.width == 5,
        "Constructor Test Failure:\n\t matrixD did not create matrix with width 5\n");
    assert_msg(d.height == 7,
        "Constructor Test Failure:\n\t matrixD did not create matrix with height 7\n");

    assert_msg(f.width == 5,
        "Constructor Test Failure:\n\t matrixF did not create matrix with width 5\n");
    assert_msg(f.height == 7,
        "Constructor Test Failure:\n\t matrixF did not create matrix with height 7\n");

    assert_msg(i.height == 7,
        "Constructor Test Failure:\n\t matrixI did not create matrix with height 7\n");
    assert_msg(i.width == 5,
        "Constructor Test Failure:\n\t matrixI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++){
        assert_msg3(testDataD[c] == d.data[c],
            "Constructor Test Failure:\n\t matrixD did not return correct value at i = %d, %f != %f\n", c, testDataD[c], d.data[c]);

        assert_msg3(testDataF[c] == f.data[c],
            "Constructor Test Failure:\n\t matrixF did not return correct value at i = %d, %f != %f\n", c, testDataF[c], f.data[c]);

        assert_msg3(testDataI[c] == i.data[c],
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
    assert_msg1(e == SUCCESS, "Null Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullF(&f, 5, 7);
    assert_msg1(e == SUCCESS, "Null Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullI(&i, 5, 7);
    assert_msg1(e == SUCCESS, "Null Constructor Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(d.width == 5,
        "Null Constructor Test Failure:\n\t matrixNullD did not create matrix with width 5\n");
    assert_msg(d.height == 7,
        "Null Constructor Test Failure:\n\t matrixNullD did not create matrix with height 7\n");

    assert_msg(f.width == 5,
        "Null Constructor Test Failure:\n\t matrixNullF did not create matrix with width 5\n");
    assert_msg(f.height == 7,
        "Null Constructor Test Failure:\n\t matrixNullF did not create matrix with height 7\n");

    assert_msg(i.height == 7,
        "Null Constructor Test Failure:\n\t matrixNullI did not create matrix with height 7\n");
    assert_msg(i.width == 5,
        "Null Constructor Test Failure:\n\t matrixNullI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++){
        assert_msg2(0 == d.data[c],
            "Null Constructor Test Failure:\n\t matrixNullD did not return correct value at i = %d, 0 != %f\n", c, d.data[c]);

        assert_msg2(0 == f.data[c],
            "Null Constructor Test Failure:\n\t matrixNullF did not return correct value at i = %d, 0 != %f\n", c, f.data[c]);

        assert_msg2(0 == i.data[c],
            "Null Constructor Test Failure:\n\t matrixNullI did not return correct value at i = %d, 0 != %d\n", c, i.data[c]);
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    return TEST_SUCCESS;
}

TEST(test_copy_constructor){
    /*
    Assumptions:
        matrixD
        matrixF
        matrixI
    */

    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixD(&d, testDataD, 5, 7);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));

    doubleMatrix cpyD = DEFAULT_MATRIX;
    floatMatrix cpyF = DEFAULT_MATRIX;
    intMatrix cpyI = DEFAULT_MATRIX;

    e = matrixCopyConsD(&cpyD, &d);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsF(&cpyF, &f);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsI(&cpyI, &i);
    assert_msg1(e == SUCCESS, "Copy Constructor Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(cmpD(&cpyD, &d), "Copy Constructor Test Failure: matrixCopyD failed to create an equivalent matrix\n")
    assert_msg(cmpF(&cpyF, &f), "Copy Constructor Test Failure: matrixCopyF failed to create an equivalent matrix\n")
    assert_msg(cmpI(&cpyI, &i), "Copy Constructor Test Failure: matrixCopyI failed to create an equivalent matrix\n")

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
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testSquareF, 5, 5);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testSquareI, 5, 5);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg1(atD(&d, 3, 4) == -10, "At Test Failure: atD(&d, 3, 4) returned %f instead of the required -10", atD(&d, 3, 4));
    assert_msg1(atF(&f, 3, 4) == -10, "At Test Failure: atF(&d, 3, 4) returned %f instead of the required -10", atF(&f, 3, 4));
    assert_msg1(atI(&i, 3, 4) == -10, "At Test Failure: atI(&d, 3, 4) returned %d instead of the required -10", atI(&i, 3, 4));

    doubleMatrix cpyD = DEFAULT_MATRIX;
    floatMatrix cpyF = DEFAULT_MATRIX;
    intMatrix cpyI = DEFAULT_MATRIX;

    e = matrixCopyConsD(&cpyD, &d);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsF(&cpyF, &f);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsI(&cpyI, &i);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));

    e = insertAtD(&cpyD, 42, 3, 4);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = insertAtF(&cpyF, 42, 3, 4);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));
    e = insertAtI(&cpyI, 42, 3, 4);
    assert_msg1(e == SUCCESS, "At Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg1(atD(&cpyD, 3, 4) == 42, "At Test Failure: insertAtD(&tmpD, 3, 4) returned %f instead of the required -10", atD(&cpyD, 3, 4));
    assert_msg1(atF(&cpyF, 3, 4) == 42, "At Test Failure: insertAtF(&tmpF, 3, 4) returned %f instead of the required -10", atF(&cpyF, 3, 4));
    assert_msg1(atI(&cpyI, 3, 4) == 42, "At Test Failure: insertAtI(&tmpI, 3, 4) returned %d instead of the required -10", atI(&cpyI, 3, 4));

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
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));

    doubleMatrix d_cpy = DEFAULT_MATRIX;
    floatMatrix f_cpy = DEFAULT_MATRIX;
    intMatrix i_cpy = DEFAULT_MATRIX;

    e = matrixCopyConsD(&d_cpy, &d);
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsF(&f_cpy, &f);
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsI(&i_cpy, &i);
    assert_msg1(e == SUCCESS, "Matrix Copy Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(d_cpy.width == 5,
        "Matrix Copy Test Failure:\n\t matrixcpyD did not create matrix with width 5\n");
    assert_msg(d_cpy.height == 7,
        "Matrix Copy Test Failure:\n\t matrixcpyD did not create matrix with height 7\n");

    assert_msg(f_cpy.width == 5,
        "Matrix Copy Test Failure:\n\t matrixcpyF did not create matrix with width 5\n");
    assert_msg(f_cpy.height == 7,
        "Matrix Copy Test Failure:\n\t matrixcpyF did not create matrix with height 7\n");

    assert_msg(i_cpy.height == 7,
        "Matrix Copy Test Failure:\n\t matrixcpyI did not create matrix with height 7\n");
    assert_msg(i_cpy.width == 5,
        "Matrix Copy Test Failure:\n\t matrixcpyI did not create matrix with width 5\n");

    int c;
    for (c = 0; c < 5 * 7; c++){
        assert_msg3(testDataD[c] == d.data[c],
            "Matrix Copy Test Failure:\n\t matrixcpyD did not return correct value at i = %d, %f != %f\n", c, d_cpy.data[c], d.data[c]);

        assert_msg3(testDataF[c] == f.data[c],
            "Matrix Copy Test Failure:\n\t matrixcpyF did not return correct value at i = %d, %f != %f\n", c, f_cpy.data[c], f.data[c]);

        assert_msg3(testDataI[c] == i.data[c],
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

    const double detD[] = {
3,  8,
4,  6
};
    const float detF[] = {
3,  8,
4,  6
};
    const int detI[] = {
3,  8,
4,  6
};
    MatrixError e;
    e = matrixD(&d, detD, 2, 2);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, detF, 2, 2);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, detI, 2, 2);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));

    //Test determinant2x2
    double retD = determinant2x2D(&d);
    assert_msg1(retD == -14, "Determinant Test Failure: determinant2x2D returned %f when it was supposed to return -14\n", retD);
    float retF = determinant2x2F(&f);
    assert_msg1(retF == -14, "Determinant Test Failure: determinant2x2F returned %f when it was supposed to return -14\n", retF);
    int retI = determinant2x2I(&i);
    assert_msg1(retI == -14, "Determinant Test Failure: determinant2x2I returned %d when it was supposed to return -14\n", retI);

    e = matrixD(&d, testSquareD, 5, 5);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testSquareF, 5, 5);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testSquareI, 5, 5);
    assert_msg1(e == SUCCESS, "Determinant Test Failure:\n\t%s", getErrorMessage(e));

    retD = determinantD(&d, 0);
    assert_msg1(retD == -183011, "Determinant Test Failure: determinantD returned %f when it was supposed to return -183011\n", retD);
    retF = determinantF(&f, 0);
    assert_msg1(retF == -183011, "Determinant Test Failure: determinantF returned %f when it was supposed to return -183011\n", retF);
    retI = determinantI(&i, 0);
    assert_msg1(retI == -183011, "Determinant Test Failure: determinantI returned %d when it was supposed to return -183011\n", retI);

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
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixD(&d2, testDataD, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f1, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f2, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i1, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i2, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Matrix Comparison Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(cmpD(&d1, &d2), "Matrix Comparison Test Failure: cmpD failed to return true for two equivalent matrices\n");
    assert_msg(cmpF(&f1, &f2), "Matrix Comparison Test Failure: cmpF failed to return true for two equivalent matrices\n");
    assert_msg(cmpI(&i1, &i2), "Matrix Comparison Test Failure: cmpI failed to return true for two equivalent matrices\n");

    d2.data[5 * d2.width + 3] = 42;
    assert_msg(!cmpD(&d1, &d2), "Matrix Comparison Test Failure: cmpD failed to return false for two distinct matrices\n");
    f2.data[5 * f2.width + 3] = 42;
    assert_msg(!cmpF(&f1, &f2), "Matrix Comparison Test Failure: cmpF failed to return false for two distinct matrices\n");
    i2.data[5 * i2.width + 3] = 42;
    assert_msg(!cmpI(&i1, &i2), "Matrix Comparison Test Failure: cmpI failed to return false for two distinct matrices\n");

    destroymD(&d1);
    destroymD(&d2);
    destroymF(&f1);
    destroymF(&f2);
    destroymI(&i1);
    destroymI(&i2);

    return TEST_SUCCESS;
}

TEST(test_transpose){
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
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testDataF, 5, 7);
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testDataI, 5, 7);
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));

    doubleMatrix tranD = DEFAULT_MATRIX;
    floatMatrix tranF = DEFAULT_MATRIX;
    intMatrix tranI = DEFAULT_MATRIX;

    e = matrixCopyConsD(&tranD, &d);
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsF(&tranF, &f);
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixCopyConsI(&tranI, &i);
    assert_msg1(e == SUCCESS, "Transpose Test Failure:\n\t%s", getErrorMessage(e));

    transposeD(&tranD);
    transposeF(&tranF);
    transposeI(&tranI);

    assert_msg1(tranD.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranD.width);
    assert_msg1(tranF.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranF.width);
    assert_msg1(tranI.width == 7, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 7\n", tranI.width);

    assert_msg1(tranD.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranD.height);
    assert_msg1(tranF.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranF.height);
    assert_msg1(tranI.height == 5, "Transpose Test Failure: Transposed matrix had width of %d, the width was supposed to be 5\n", tranI.height);

    transposeD(&tranD);
    transposeF(&tranF);
    transposeI(&tranI);

    assert_msg(cmpD(&d, &tranD), "Transpose Test Failure: transposeD does not obey the law - Transpose(Transpose(A)) != A\n");
    assert_msg(cmpF(&f, &tranF), "Transpose Test Failure: transposeF does not obey the law - Transpose(Transpose(A)) != A\n");
    assert_msg(cmpI(&i, &tranI), "Transpose Test Failure: transposeI does not obey the law - Transpose(Transpose(A)) != A\n");

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
    double testCofD[9] = {
3,   0,  2,
2,   0,  -2,
0,   1,   1
};

    float testCofF[9] = {
3,   0,  2,
2,   0,  -2,
0,   1,   1
};

    int testCofI[9] = {
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
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testCofF, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testCofI, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));

    e = matrixNullD(&ansD, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullF(&ansF, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullI(&ansI, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));

    double ansDlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};
    float ansFlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};
    int ansIlst[] = {2, -2, 2, 2, 3, -3, 0, 10, 0};

    doubleMatrix cofD = DEFAULT_MATRIX;
    floatMatrix cofF = DEFAULT_MATRIX;
    intMatrix cofI = DEFAULT_MATRIX;

    e = matrixD(&cofD, ansDlst, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&cofF, ansFlst, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&cofI, ansIlst, 3, 3);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));

    e = cofactorD(&ansD, &d);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = cofactorF(&ansF, &f);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));
    e = cofactorI(&ansI, &i);
    assert_msg1(e == SUCCESS, "Cofactor Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(cmpD(&ansD, &cofD), "Cofactor Test Failure: cofactorD failed to correctly find cofactor for 3x3 matrix\n");
    assert_msg(cmpF(&ansF, &cofF), "Cofactor Test Failure: cofactorF failed to correctly find cofactor for 3x3 matrix\n");
    assert_msg(cmpI(&ansI, &cofI), "Cofactor Test Failure: cofactorI failed to correctly find cofactor for 3x3 matrix\n");

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

TEST(test_adjoint){
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
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&ansF, ansFlst, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&ansI, ansIlst, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));

        double testAdjD[9] = {
3,   0,  2,
2,   0,  -2,
0,   1,   1
};

    float testAdjF[9] = {
3,   0,  2,
2,   0,  -2,
0,   1,   1
};

    int testAdjI[9] = {
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
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testAdjF, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testAdjI, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));

    e = matrixNullD(&adjD, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullF(&adjF, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullI(&adjI, 3, 3);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));

    e = adjointD(&adjD, &d);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = adjointF(&adjF, &f);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));
    e = adjointI(&adjI, &i);
    assert_msg1(e == SUCCESS, "Adjoint Test Failure:\n\t%s", getErrorMessage(e));

    assert_msg(cmpD(&ansD, &adjD), "Adjoint Test Failure: adjointD failed to correctly find adjoint for 3x3 matrix\n");
    assert_msg(cmpF(&ansF, &adjF), "Adjoint Test Failure: adjointF failed to correctly find adjoint for 3x3 matrix\n");
    assert_msg(cmpI(&ansI, &adjI), "Adjoint Test Failure: adjointI failed to correctly find adjoint for 3x3 matrix\n");

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

TEST(test_invert){
    doubleMatrix ansD = DEFAULT_MATRIX;
    floatMatrix ansF = DEFAULT_MATRIX;
    intMatrix ansI = DEFAULT_MATRIX;

    MatrixError e;
    e = matrixNullD(&ansD, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullF(&ansF, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullI(&ansI, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));

    doubleMatrix d = DEFAULT_MATRIX;
    floatMatrix f = DEFAULT_MATRIX;
    intMatrix i = DEFAULT_MATRIX;

    doubleMatrix tmpD = DEFAULT_MATRIX;
    floatMatrix tmpF = DEFAULT_MATRIX;
    intMatrix tmpI = DEFAULT_MATRIX;

    e = matrixD(&d, testSquareD, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixF(&f, testSquareF, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixI(&i, testSquareI, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));

    e = matrixNullD(&tmpD, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullF(&tmpF, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = matrixNullI(&tmpI, 5, 5);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));

    e = invertD(&tmpD, &d);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = adjointF(&tmpF, &f);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = adjointI(&tmpI, &i);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));

    e = multiplyD(&ansD, &d, &tmpD);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = multiplyF(&ansF, &f, &tmpF);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));
    e = multiplyI(&ansI, &i, &tmpI);
    assert_msg1(e == SUCCESS, "Invert Test Failure:\n\t%s", getErrorMessage(e));

    int x, y;
    for (y = 0; y < 5; y++){
        for (x = 0; x < 5; x++){
            if (x == y){
                assert_msg(atD(&ansD, x, y) -1 <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
                assert_msg(atD(&ansD, x, y) -1 <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
                assert_msg(atD(&ansD, x, y) -1 <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
            } else {
                assert_msg(atD(&ansD, x, y) <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
                assert_msg(atD(&ansD, x, y) <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
                assert_msg(atD(&ansD, x, y) <= 0.001, "Invert Test Failure: invertD does not obey the rule: inverse(A) . A == I\n");
            }
        }
    }

    destroymD(&d);
    destroymF(&f);
    destroymI(&i);

    destroymD(&tmpD);
    destroymF(&tmpF);
    destroymI(&tmpI);

    destroymD(&ansD);
    destroymF(&ansF);
    destroymI(&ansI);

    return TEST_SUCCESS;
}

TEST(matrix_suite){
    int count_passed = 0;
    puts("Testing Constructors...");
    count_passed += !test_constructors();
    puts("Testing Null Constructors...");
    count_passed += !test_null_constructors();
    puts("Testing Copy Constructors...");
    count_passed += !test_copy_constructor();
    puts("Testing At...");
    count_passed += !test_at();
    puts("Testing Matrix Copy...");
    count_passed += !test_matrix_cpy();
    puts("Testing Determinant...");
    count_passed += !test_determinant();
    puts("Testing Matrix Comparison...");
    count_passed += !test_matrix_comparison();
    puts("Testing Transpose...");
    count_passed += !test_transpose();
    puts("Testing Cofactor...");
    count_passed += !test_cofactor();
    puts("Testing Adjoint...");
    count_passed += !test_adjoint();
    puts("Testing Invert...");
    count_passed += !test_invert();

    return count_passed;
}
