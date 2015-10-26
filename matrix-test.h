#ifndef MATRIX_TEST_H_INCLUDED
#define MATRIX_TEST_H_INCLUDED

/*
Created by Andrew M. Hall
*/

#define TEST_NUMBER 18

#define TEST(name) TestResult name(void)
#define CATCH_ERROR(msg, e) if(e.code != SUCCESS){puts(msg); printError(e); return TEST_FAILURE;}
#define ASSERT_MSG(cond, msg) if (!(cond)){printf(msg);return TEST_FAILURE;}
#define ASSERT_MSG1(cond, msg, a1) if (!(cond)){printf(msg, a1);return TEST_FAILURE;}
#define ASSERT_MSG2(cond, msg, a1, a2) if (!(cond)){printf(msg, a1, a2);return TEST_FAILURE;}
#define ASSERT_MSG3(cond, msg, a1, a2, a3) if (!(cond)){printf(msg, a1, a2, a3);return TEST_FAILURE;}
#define ASSERT_MSG4(cond, msg, a1, a2, a3, a4) if (!(cond)){printf(msg, a1, a2, a3, a4);return TEST_FAILURE;}
#define ASSERT_MSG5(cond, msg, a1, a2, a3, a4, a5) if (!(cond)){printf(msg, a1, a2, a3, a4, a5);return TEST_FAILURE;}
#define ASSERT_MSG6(cond, msg, a1, a2, a3, a4, a5, a6) if (!(cond)){printf(msg, a1, a2, a3, a4, a5, a6);return TEST_FAILURE;}

#define RUN_TEST(name, proc, count) printf("Testing %s ... ", name); if ((proc) == TEST_SUCCESS){count++;puts("Passed!");}

typedef enum
{
    TEST_SUCCESS = 0,
    TEST_FAILURE
} TestResult;

TestResult matrix_suite(void);

#endif // MATRIX_TEST_H_INCLUDED
