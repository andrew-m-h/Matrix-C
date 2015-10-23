#ifndef MATRIX_TEST_H_INCLUDED
#define MATRIX_TEST_H_INCLUDED

/*
Created by Andrew M. Hall
*/

#define TEST(name) TestResult name(void)
#define assert_msg(cond, msg) if (!(cond)){printf(msg);return TEST_FAILURE;}
#define assert_msg1(cond, msg, a1) if (!(cond)){printf(msg, a1);return TEST_FAILURE;}
#define assert_msg2(cond, msg, a1, a2) if (!(cond)){printf(msg, a1, a2);return TEST_FAILURE;}
#define assert_msg3(cond, msg, a1, a2, a3) if (!(cond)){printf(msg, a1, a2, a3);return TEST_FAILURE;}
#define assert_msg4(cond, msg, a1, a2, a3, a4) if (!(cond)){printf(msg, a1, a2, a3, a4);return TEST_FAILURE;}
#define assert_msg5(cond, msg, a1, a2, a3, a4, a5) if (!(cond)){printf(msg, a1, a2, a3, a4, a5);return TEST_FAILURE;}

typedef enum {
    TEST_SUCCESS = 0,
    TEST_FAILURE
} TestResult;

TestResult matrix_suite(void);

#endif // MATRIX_TEST_H_INCLUDED
