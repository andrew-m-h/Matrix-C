/*
Created by Andrew M. Hall
*/

#include "matrix.h"
#include <string.h>
#include <stdio.h>
#include <pthread.h>

//PThread - cofactor
typedef struct
{
    doubleMatrix * dest;
    const doubleMatrix * a;
    int y;
} cofactorArgD;

typedef struct
{
    floatMatrix * dest;
    const floatMatrix * a;
    int y;
} cofactorArgF;

typedef struct
{
    intMatrix * dest;
    const intMatrix * a;
    int y;
} cofactorArgI;

//PThread - multiply
typedef struct
{
    doubleMatrix *dest;
    const doubleMatrix *tmpRow;
    const doubleMatrix *b;
    int y;
} multiplyArgD;

typedef struct
{
    floatMatrix *dest;
    floatMatrix *tmpRow;
    const floatMatrix *b;
    int y;
} multiplyArgF;

typedef struct
{
    intMatrix *dest;
    const intMatrix *tmpRow;
    const intMatrix *b;
    int y;
} multiplyArgI;

MatrixError matrixD(doubleMatrix * dest, const double * in_data, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    double *ptr = (double*)malloc(in_height*in_width*sizeof(double));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    if (!memcpy(ptr, in_data, in_width*in_height*sizeof(double)))
        return FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixF(floatMatrix * dest, const float * in_data, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    float *ptr = (float*)malloc(in_width*in_height*sizeof(float));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    if (!memcpy(ptr, in_data, in_width*in_height*sizeof(float)))
        return FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixI(intMatrix * dest, const int * in_data, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    int *ptr = (int*)malloc(in_width*in_height*sizeof(int));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    if (!memcpy(ptr, in_data, in_width*in_height*sizeof(int)))
        return FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixNullD(doubleMatrix * dest, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    double *ptr = (double*)calloc(in_width*in_height, sizeof(double));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixNullF(floatMatrix * dest, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    float *ptr = (float*)calloc(in_width*in_height, sizeof(float));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixNullI(intMatrix * dest, int in_width, int in_height)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    int *ptr = (int*)calloc(in_width*in_height, sizeof(int));
    if (!ptr)
        return MEM_ALLOCATION_FAILURE;

    dest->data = ptr;
    dest->height=in_height;
    dest->width=in_width;
    return SUCCESS;
}

MatrixError matrixCopyConsD(doubleMatrix * dest, const doubleMatrix * src)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    MatrixError e;
    e = matrixNullD(dest, src->width, src->height);
    if (e != SUCCESS)
        return e;
    e = matrixcpyD(dest, src);
    return e;
}

MatrixError matrixCopyConsF(floatMatrix * dest, const floatMatrix * src)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    MatrixError e;
    e = matrixNullF(dest, src->width, src->height);
    if (e != SUCCESS)
        return e;
    e = matrixcpyF(dest, src);
    return e;
}

MatrixError matrixCopyConsI(intMatrix * dest, const intMatrix * src)
{
    if (dest->data){
        free(dest->data);
        dest->width = 0;
        dest->height = 0;
    }
    MatrixError e;
    e = matrixNullI(dest, src->width, src->height);
    if (e != SUCCESS)
        return e;
    e = matrixcpyI(dest, src);
    return e;
}

void destroymD(doubleMatrix * a)
{
    if (!a)
        return;
    if (a->data)
    {
        free(a->data);
    }
    a = NULL;
}

void destroymF(floatMatrix * a)
{
    if (!a)
        return;
    if (a->data)
    {
        free(a->data);
    }
    a = NULL;
}

void destroymI(intMatrix * a)
{
    if (!a)
        return;
    if (a->data)
    {
        free(a->data);
    }
    a = NULL;
}

MatrixError matrixcpyD(doubleMatrix * dest, const doubleMatrix * src)
{
    if (dest->width != src->width || dest->height != src->height)
        return DIMENSION_ERROR;

    if (!memcpy(dest->data, src->data, src->width*src->height*sizeof(double)))
        return FAILURE;
    return SUCCESS;
}

MatrixError matrixcpyF(floatMatrix * dest, const floatMatrix * src)
{
    if (dest->width != src->width || dest->height != src->height)
        return DIMENSION_ERROR;

    if (!memcpy(dest->data, src->data, src->width*src->height*sizeof(float)))
        return FAILURE;
    return SUCCESS;
}

MatrixError matrixcpyI(intMatrix * dest, const intMatrix * src)
{
    if (dest->width != src->width || dest->height != src->height)
        return DIMENSION_ERROR;

    if (!memcpy(dest->data, src->data, src->width*src->height*sizeof(int)))
        return FAILURE;
    return SUCCESS;
}

MatrixError printmD(const doubleMatrix *a)
{
    int mul = 10;
    size_t buff_length = a->width * a->height * mul + 1;
    char * strBuff = (char*)malloc(buff_length*sizeof(char));
    if (!strBuff)
        return MEM_ALLOCATION_FAILURE;
    strBuff[0] = '\0';

    int count = 0;
    MatrixError e = toStringD(strBuff, a, buff_length);
    while (e != SUCCESS && count <= 10)
    {
        mul += 2;
        buff_length = a->width * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)calloc(buff_length, sizeof(char));
        if (!strBuff)
            return MEM_ALLOCATION_FAILURE;
        count++;
        e = toStringD(strBuff, a, buff_length);
    }
    if (count == 11)
    {
        free(strBuff);
        return e;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return SUCCESS;
}

MatrixError printmF(const floatMatrix *a)
{
    int mul = 10;
    size_t buff_length = a->width * a->height * mul + 1;
    char * strBuff = (char*)malloc(buff_length*sizeof(char));
    if (!strBuff)
        return MEM_ALLOCATION_FAILURE;
    strBuff[0] = '\0';

    int count = 0;
    MatrixError e = toStringF(strBuff, a, buff_length);
    while (e != SUCCESS && count <= 5)
    {
        mul += 2;
        buff_length = a->width * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)calloc(buff_length, sizeof(char));
        if (!strBuff)
            return MEM_ALLOCATION_FAILURE;
        count++;
        e = toStringF(strBuff, a, buff_length);
    }
    if (count == 6)
    {
        free(strBuff);
        return e;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return SUCCESS;
}

MatrixError printmI(const intMatrix *a)
{
    int mul = 10;
    size_t buff_length = a->width * a->height * mul + 1;
    char * strBuff = (char*)malloc(buff_length*sizeof(char));
    if (!strBuff)
        return MEM_ALLOCATION_FAILURE;
    strBuff[0] = '\0';

    int count = 0;
    MatrixError e = toStringI(strBuff, a, buff_length);
    while (e != SUCCESS && count <= 5)
    {
        mul += 2;
        buff_length = a->width * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)malloc(buff_length*sizeof(char));
        if (!strBuff)
            return MEM_ALLOCATION_FAILURE;
        strBuff[0] = '\0';

        count++;
        e = toStringI(strBuff, a, buff_length);
    }
    if (count == 6)
    {
        free(strBuff);
        return e;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return SUCCESS;
}

double atD(const doubleMatrix* a, int x, int y)
{
    return a->data[y * a->width + x];
}

float atF(const floatMatrix* a, int x, int y)
{
    return a->data[y * a->width + x];
}

int atI(const intMatrix* a, int x, int y)
{
    return a->data[y * a->width + x];
}

MatrixError insertAtD(doubleMatrix *a, double val, int x, int y){
    if (x < 0 || x >= a->width || y < 0 || y >= a->height)
        return DIMENSION_ERROR;
    a->data[y * a->width + x] = val;
    return SUCCESS;
}

MatrixError insertAtF(floatMatrix *a, float val, int x, int y){
    if (x < 0 || x >= a->width || y < 0 || y >= a->height)
        return DIMENSION_ERROR;
    a->data[y * a->width + x] = val;
    return SUCCESS;
}

MatrixError insertAtI(intMatrix *a, int val, int x, int y){
    if (x < 0 || x >= a->width || y < 0 || y >= a->height)
        return DIMENSION_ERROR;
    a->data[y * a->width + x] = val;
    return SUCCESS;
}

MatrixError addD(doubleMatrix *a, const doubleMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] += b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}

MatrixError addF(floatMatrix *a, const floatMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] += b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}

MatrixError addI(intMatrix *a, const intMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] += b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}

MatrixError subtractD(doubleMatrix *a, const doubleMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] -= b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}
MatrixError subtractF(floatMatrix *a, const floatMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] -= b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}
MatrixError subtractI(intMatrix *a, const intMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return DIMENSION_ERROR;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] -= b->data[y * b->width + x];
        }
    }
    return SUCCESS;
}

void negativeD(doubleMatrix *a)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= (-1);
        }
    }
}

void negativeF(floatMatrix *a)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= (-1);
        }
    }
}

void negativeI(intMatrix *a)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= (-1);
        }
    }
}

void scalarMultiplyD(doubleMatrix * a, const double b)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= b;
        }
    }
}

void scalarMultiplyF(floatMatrix *a, const float b)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= b;
        }
    }
}

void scalarMultiplyI(intMatrix *a, const int b)
{
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            a->data[y * a->width + x] *= b;
        }
    }
}

BOOL cmpD(const doubleMatrix *a, const doubleMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return FALSE;

    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            if (a->data[y * a->width + x] != b->data[y * b->width + x])
                return FALSE;
        }
    }
    return TRUE;
}

BOOL cmpF(const floatMatrix *a, const floatMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            if (a->data[y * a->width + x] != b->data[y * b->width + x])
                return FALSE;
        }
    }
    return TRUE;
}

BOOL cmpI(const intMatrix *a, const intMatrix *b)
{
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            if (a->data[y * a->width + x] != b->data[y * b->width + x])
                return FALSE;
        }
    }
    return TRUE;
}

void transposeD(doubleMatrix *a)
{
    doubleMatrix tmp = DEFAULT_MATRIX;
    matrixNullD(&tmp, a->height, a->width);
    int y, x;
    for (x = 0; x < a->width; x++){
        for (y = 0; y < a->height; y++){
            insertAtD(&tmp, atD(a, x, y), y, x);
        }
    }
    matrixD(a, tmp.data, tmp.width, tmp.height);
    destroymD(&tmp);
}

void transposeF(floatMatrix *a)
{
    floatMatrix tmp = DEFAULT_MATRIX;
    matrixNullF(&tmp, a->height, a->width);
    int y, x;
    for (x = 0; x < a->width; x++){
        for (y = 0; y < a->height; y++){
            insertAtF(&tmp, atF(a, x, y), y, x);
        }
    }
    matrixF(a, tmp.data, tmp.width, tmp.height);
    destroymF(&tmp);
}

void transposeI(intMatrix *a)
{
    intMatrix tmp = DEFAULT_MATRIX;
    matrixNullI(&tmp, a->height, a->width);
    int y, x;
    for (x = 0; x < a->width; x++){
        for (y = 0; y < a->height; y++){
            insertAtI(&tmp, atI(a, x, y), y, x);
        }
    }
    matrixI(a, tmp.data, tmp.width, tmp.height);
    destroymI(&tmp);
}

void doubleToString(char *dest, double num)
{
    sprintf(dest, "%f", num);
    size_t len = strlen(dest);
    int i;
    for (i = len - 1; i > 0; i--)
    {
        if (dest[i] == '.'){
            dest[i] = ' ';
            break;
        }
        else if (dest[i] != '0')
        {
            break;
        }
        else
        {
            dest[i] = ' ';
        }
    }
}

MatrixError toStringD(char* dest, const doubleMatrix *a, size_t buffsize)
{
    int y, x;
    size_t length = buffsize - 1;
    if (!dest)
    {
        printf("Destination buffer was null pointer\n");
        return BUFF_SIZE_ERROR;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            char strBuff[50] = {'\0'};
            doubleToString(strBuff, atD(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l)
            {
                length -= l;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
            strncat(dest, "\t", length);
            if (length >= 2)
            {
                length -= 2;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2)
        {
            length -= 2;
        }
        else
        {
            return BUFF_SIZE_ERROR;
        }
    }
    return SUCCESS;
}

void floatToString(char *dest, float num)
{
    sprintf(dest, "%f", num);
    size_t len = strlen(dest);
    int i;
    for (i = len - 1; i > 0; i--)
    {
        if (dest[i] == '.'){
            dest[i] = ' ';
            break;
        }
        else if (dest[i] != '0')
        {
            break;
        }
        else
        {
            dest[i] = ' ';
        }
    }
}

MatrixError toStringF(char* dest, const floatMatrix *a, size_t buffsize)
{
    int y, x;
    size_t length = buffsize - 1;
    if (!dest)
    {
        printf("Destination buffer was null pointer\n");
        return BUFF_SIZE_ERROR;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            char strBuff[50] = {'\0'};
            floatToString(strBuff, atF(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l)
            {
                length -= l;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
            strncat(dest, "\t", length);
            if (length >= 2)
            {
                length -= 2;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2)
        {
            length -= 2;
        }
        else
        {
            return BUFF_SIZE_ERROR;
        }
    }
    return SUCCESS;
}

MatrixError toStringI(char* dest, const intMatrix *a, size_t buffsize)
{
    int y, x;
    size_t length = buffsize - 1;
    if (!dest)
    {
        printf("Destination buffer was null pointer\n");
        return BUFF_SIZE_ERROR;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++)
    {
        for (x = 0; x < a->width; x++)
        {
            char strBuff[50] = {'\0'};
            sprintf(strBuff, "%d", atI(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l)
            {
                length -= l;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
            strncat(dest, "\t", length);
            if (length >= 2)
            {
                length -= 2;
            }
            else
            {
                return BUFF_SIZE_ERROR;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2)
        {
            length -= 2;
        }
        else
        {
            return BUFF_SIZE_ERROR;
        }
    }
    return SUCCESS;
}

double determinant2x2D(const doubleMatrix *a)
{
    if (a->height != 2 || a->width != 2)
    {
        puts(getErrorMessage(MATH_ERROR));
        exit(EXIT_FAILURE);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

float determinant2x2F(const floatMatrix *a)
{
    if (a->height != 2 || a->width != 2)
    {
        puts(getErrorMessage(MATH_ERROR));
        exit(EXIT_FAILURE);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

int determinant2x2I(const intMatrix *a)
{
    if (a->height != 2 || a->width != 2)
    {
        puts(getErrorMessage(MATH_ERROR));
        exit(EXIT_FAILURE);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

double determinantD(const doubleMatrix *a, int row)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
    {
        puts(getErrorMessage(MATH_ERROR));
        exit(EXIT_FAILURE);
    }
    if (row < 0 || row > w)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        exit(EXIT_FAILURE);
    }

    double output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2)
    {
        return determinant2x2D(a);
    }
    else
    {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++)
        {
            int ycount = 0;

            doubleMatrix tmp = DEFAULT_MATRIX;
            MatrixError e = matrixNullD(&tmp, w-1, h-1);
            if (e != SUCCESS)
            {
                puts(getErrorMessage(e));
                exit(EXIT_FAILURE);
            }
            int y;
            for (y = 0; y < h; y++)
            {
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++)
                    {
                        if (x != tmpX)
                        {
                            tmp.data[ycount * tmp.width + (xcount++)] = a->data[y * a->width + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantD(&tmp,0) * (a->data[row * a->width + tmpX]))) : -((determinantD(&tmp,0) * (a->data[row * a->width + tmpX])));
            s = !s;
            destroymD(&tmp);
        }
    }
    return output;
}

float determinantF(const floatMatrix *a, int row)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
    {
        puts(getErrorMessage(MATH_ERROR));
        exit(EXIT_FAILURE);
    }
    if (row < 0 || row > w)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        exit(EXIT_FAILURE);
    }

    float output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2)
    {
        return determinant2x2F(a);
    }
    else
    {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++)
        {
            int ycount = 0;

            floatMatrix tmp = DEFAULT_MATRIX;
            MatrixError e = matrixNullF(&tmp, w-1, h-1);
            if (e != SUCCESS)
            {
                puts(getErrorMessage(e));
                exit(EXIT_FAILURE);
            }

            int y;
            for (y = 0; y < h; y++)
            {
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++)
                    {
                        if (x != tmpX)
                        {
                            tmp.data[ycount * tmp.width + (xcount++)] = a->data[y * a->width + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantF(&tmp,0) * (a->data[row * a->width + tmpX]))) : -((determinantF(&tmp,0) * (a->data[row * a->width + tmpX])));
            s = !s;
            destroymF(&tmp);
        }
    }
    return output;
}

int determinantI(const intMatrix *a, int row)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        exit(EXIT_FAILURE);
    }
    if (row < 0 || row > w)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
        exit(EXIT_FAILURE);
    }

    int output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2)
    {
        return determinant2x2I(a);
    }
    else
    {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++)
        {
            int ycount = 0;

            intMatrix tmp = DEFAULT_MATRIX;
            MatrixError e = matrixNullI(&tmp, w-1, h-1);
            if (e != SUCCESS)
            {
                puts(getErrorMessage(e));
                exit(EXIT_FAILURE);
            }

            int y;
            for (y = 0; y < h; y++)
            {
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++)
                    {
                        if (x != tmpX)
                        {
                            tmp.data[ycount * tmp.width + (xcount++)] = a->data[y * a->width + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantI(&tmp,0) * (a->data[row * a->width + tmpX]))) : -((determinantI(&tmp,0) * (a->data[row * a->width + tmpX])));
            s = !s;
            destroymI(&tmp);
        }
    }
    return output;
}

MatrixError cofactorD(doubleMatrix * dest, const doubleMatrix *a)
{
    if (a->width > 8 && a->width <= NTHREADS)
    {
        return paraCofactorD(dest, a);
    }
    else
    {
        return stdCofactorD(dest, a);
    }
}

MatrixError cofactorF(floatMatrix * dest, const floatMatrix *a)
{
    if (a->width > 8 && a->width <= NTHREADS)
    {
        return paraCofactorF(dest, a);
    }
    else
    {
        return stdCofactorF(dest, a);
    }
}

MatrixError cofactorI(intMatrix * dest, const intMatrix *a)
{
    if (a->width > 8 && a->width <= NTHREADS)
    {
        return paraCofactorI(dest, a);
    }
    else
    {
        return stdCofactorI(dest, a);
    }
}

MatrixError stdCofactorD(doubleMatrix * dest, const doubleMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
        return DIMENSION_ERROR;

    if (dest->height != h || dest->width != w)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < h; y++)
    {
        int x;
        for (x = 0; x < w; x++)
        {
            doubleMatrix det = DEFAULT_MATRIX;
            MatrixError e = matrixNullD(&det, w-1, h-1);
            if (e != SUCCESS)
                return e;

            int ycount = 0;
            int ydet;
            for (ydet = 0; ydet < h; ydet++)
            {
                int xcount = 0;
                if (ydet != y)
                {
                    int xdet;
                    for (xdet = 0; xdet < w; xdet++)
                    {
                        if (xdet != x)
                        {
                            //insertAtD(&det, atD(a, xdet, ydet), xcount++, ycount);
                            det.data[ycount * det.width + (xcount++)] = atD(a, xdet, ydet);
                        }
                    }
                    ycount++;
                }
            }

            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->width + x] = determinantD(&det, 0);
            }
            else
            {
                dest->data[y * dest->width + x] = -determinantD(&det, 0);
            }
            destroymD(&det);
        }
    }
    return SUCCESS;
}

void * cofactorThreadD(void * m)
{
    int s = ((cofactorArgD *)m)->dest->width;
    int x;
    for (x = 0; x < s; x++)
    {
        doubleMatrix det = DEFAULT_MATRIX;
        MatrixError e = matrixNullD(&det, s-1, s-1);
        if (e != SUCCESS)
            pthread_exit((void*)e);

        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++)
        {
            int xcount = 0;
            if (ydet != ((cofactorArgD *)m)->y)
            {
                int xdet;
                for (xdet = 0; xdet < s; xdet++)
                {
                    if (xdet != x)
                    {
                        det.data[ycount * det.width + (xcount++)] = atD(((cofactorArgD *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgD *)m)->y + x) % 2 == 0)
        {
            ((cofactorArgD *)m)->dest->data[((cofactorArgD *)m)->y * s + x] = determinantD(&det, 0);
        }
        else
        {
            ((cofactorArgD *)m)->dest->data[((cofactorArgD *)m)->y * s + x] = -determinantD(&det, 0);
        }
        destroymD(&det);
    }
    pthread_exit(NULL);
}

MatrixError paraCofactorD(doubleMatrix * dest, const doubleMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
        return DIMENSION_ERROR;

    if (dest->height != h || dest->width != w)
        return DIMENSION_ERROR;

    pthread_t threads[NTHREADS];
    cofactorArgD thread_args[NTHREADS];

    int rc;
    int y;
    for (y = 0; y < h; y++)
    {
        cofactorArgD arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, cofactorThreadD, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }
    for (y = 0; y < h; y++)
    {
        pthread_join(threads[y], NULL);
        if (rc)
            return (MatrixError)rc;
    }
    return SUCCESS;
}

MatrixError stdCofactorF(floatMatrix * dest, const floatMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
        return DIMENSION_ERROR;

    if (dest->height != h || dest->width != w)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < h; y++)
    {
        int x;
        for (x = 0; x < w; x++)
        {
            floatMatrix det = DEFAULT_MATRIX;
            MatrixError e = matrixNullF(&det, w-1, h-1);
            if (e != SUCCESS)
                return e;

            int ycount = 0;
            int ydet;
            for (ydet = 0; ydet < h; ydet++)
            {
                int xcount = 0;
                if (ydet != y)
                {
                    int xdet;
                    for (xdet = 0; xdet < w; xdet++)
                    {
                        if (xdet != x)
                        {
                            det.data[ycount * det.width + (xcount++)] = a->data[ydet * a->width + xdet];
                        }
                    }
                    ycount++;
                }
            }
            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->width + x] = determinantF(&det, 0);
            }
            else
            {
                dest->data[y * dest->width + x] = -determinantF(&det, 0);
            }
            destroymF(&det);
        }
    }
    return SUCCESS;
}


void * cofactorThreadF(void * m)
{
    int s = ((cofactorArgF *)m)->dest->width;
    int x;
    for (x = 0; x < s; x++)
    {
        floatMatrix det = DEFAULT_MATRIX;
        MatrixError e = matrixNullF(&det, s-1, s-1);
        if (e != SUCCESS)
            pthread_exit((void*)e);
        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++)
        {
            int xcount = 0;
            if (ydet != ((cofactorArgF *)m)->y)
            {
                int xdet;
                for (xdet = 0; xdet < s; xdet++)
                {
                    if (xdet != x)
                    {
                        det.data[ycount * det.width + (xcount++)] = atF(((cofactorArgF *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgF *)m)->y + x) % 2 == 0)
        {
            ((cofactorArgF *)m)->dest->data[((cofactorArgF *)m)->y * s + x] = determinantF(&det, 0);
        }
        else
        {
            ((cofactorArgF *)m)->dest->data[((cofactorArgF *)m)->y * s + x] = -determinantF(&det, 0);
        }
        destroymF(&det);
    }
    pthread_exit(NULL);
}

MatrixError paraCofactorF(floatMatrix * dest, const floatMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
    {
        return DIMENSION_ERROR;
    }
    if (dest->height != h || dest->width != w)
    {
        return DIMENSION_ERROR;
    }

    pthread_t threads[NTHREADS];
    cofactorArgF thread_args[NTHREADS];

    int rc;
    int y;
    for (y = 0; y < h; y++)
    {
        cofactorArgF arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, cofactorThreadF, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }
    for (y = 0; y < h; y++)
    {
        pthread_join(threads[y], NULL);
        if (rc)
            return (MatrixError)rc;
    }
    return SUCCESS;
}

MatrixError stdCofactorI(intMatrix * dest, const intMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
        return DIMENSION_ERROR;

    if (dest->height != h || dest->width != w)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < h; y++)
    {
        int x;
        for (x = 0; x < w; x++)
        {
            intMatrix det = DEFAULT_MATRIX;
            MatrixError e = matrixNullI(&det, w-1, h-1);
            if (e != SUCCESS)
                return e;

            int ycount = 0;
            int ydet;
            for (ydet = 0; ydet < h; ydet++)
            {
                int xcount = 0;
                if (ydet != y)
                {
                    int xdet;
                    for (xdet = 0; xdet < w; xdet++)
                    {
                        if (xdet != x)
                        {
                            det.data[ycount * det.width + (xcount++)] = a->data[ydet * a->width + xdet];
                        }
                    }
                    ycount++;
                }
            }
            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->width + x] = determinantI(&det, 0);
            }
            else
            {
                dest->data[y * dest->width + x] = -determinantI(&det, 0);
            }
            destroymI(&det);
        }
    }
    return SUCCESS;
}

void * cofactorThreadI(void * m)
{
    int s = ((cofactorArgI *)m)->dest->width;
    int x;
    for (x = 0; x < s; x++)
    {
        intMatrix det = DEFAULT_MATRIX;
        MatrixError e = matrixNullI(&det, s-1, s-1);
        if (e != SUCCESS)
            pthread_exit((void*)e);

        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++)
        {
            int xcount = 0;
            if (ydet != ((cofactorArgI *)m)->y)
            {
                int xdet;
                for (xdet = 0; xdet < s; xdet++)
                {
                    if (xdet != x)
                    {
                        det.data[ycount * det.width + (xcount++)] = atI(((cofactorArgI *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgI *)m)->y + x) % 2 == 0)
        {
            ((cofactorArgI *)m)->dest->data[((cofactorArgI *)m)->y * s + x] = determinantI(&det, 0);
        }
        else
        {
            ((cofactorArgI *)m)->dest->data[((cofactorArgI *)m)->y * s + x] = -determinantI(&det, 0);
        }
        destroymI(&det);
    }
    pthread_exit(NULL);
}

MatrixError paraCofactorI(intMatrix * dest, const intMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (h != w)
        return DIMENSION_ERROR;

    if (dest->height != h || dest->width != w)
        return DIMENSION_ERROR;

    pthread_t threads[NTHREADS];
    cofactorArgI thread_args[NTHREADS];

    int rc;
    int y;
    for (y = 0; y < h; y++)
    {
        cofactorArgI arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, cofactorThreadI, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }
    for (y = 0; y < h; y++)
    {
        rc = pthread_join(threads[y], NULL);
        if (rc)
            return (MatrixError)rc;
    }
    return SUCCESS;
}

MatrixError adjointD(doubleMatrix * dest, const doubleMatrix *a)
{
    MatrixError e = cofactorD(dest, a);
    if (e == SUCCESS)
    {
        transposeD(dest);
        return SUCCESS;
    }
    return e;
}

MatrixError adjointF(floatMatrix * dest, const floatMatrix *a)
{
    MatrixError e = cofactorF(dest, a);
    if (e == SUCCESS)
    {
        transposeF(dest);
        return SUCCESS;
    }
    return e;
}


MatrixError adjointI(intMatrix * dest, const intMatrix *a)
{
    MatrixError e = cofactorI(dest, a);
    if (e == SUCCESS)
    {
        transposeI(dest);
        return SUCCESS;
    }
    return e;
}

MatrixError stdAdjointD(doubleMatrix * dest, const doubleMatrix *a)
{
    MatrixError e = stdCofactorD(dest, a);
    if (e == SUCCESS)
    {
        transposeD(dest);
        return SUCCESS;
    }
    return e;
}

MatrixError stdAdjointF(floatMatrix * dest, const floatMatrix *a)
{
    MatrixError e = stdCofactorF(dest, a);
    if (e == SUCCESS)
    {
        transposeF(dest);
        return SUCCESS;
    }
    return e;
}

MatrixError stdAdjointI(intMatrix * dest, const intMatrix *a)
{
    MatrixError e = stdCofactorI(dest, a);
    if (e == SUCCESS)
    {
        transposeI(dest);
        return SUCCESS;
    }
    return e;
}

MatrixError invert2x2D(doubleMatrix * dest, const doubleMatrix *a)
{
    if (a->width != 2 || a->width != 2)
        return DIMENSION_ERROR;

    if (dest->width != 2 || dest->height != 2)
        return DIMENSION_ERROR;

    double det = determinant2x2D(a);
    if (!det)
        return MATH_ERROR;

    double d = 1.0 / det;

    double tmp0 = a->data[0];

    dest->data[0] = a->data[3] * d;
    dest->data[1] = -a->data[1] * d;
    dest->data[2] = -a->data[2] * d;
    dest->data[3] = tmp0 * d;

    return SUCCESS;
}

MatrixError invert2x2F(doubleMatrix * dest, const floatMatrix *a)
{
    if (a->width != 2 || a->width != 2)
        return DIMENSION_ERROR;

    if (dest->width != 2 || dest->height != 2)
        return DIMENSION_ERROR;


    double det = (double)determinant2x2F(a);
    if (!det)
        return MATH_ERROR;

    double d = 1.0 / det;

    double tmp0 = (double) a->data[0];

    dest->data[0] = (double) (a->data[3] * d);
    dest->data[1] = (double) (-a->data[1] * d);
    dest->data[2] = (double) (-a->data[2] * d);
    dest->data[3] = (double) (tmp0 * d);

    return SUCCESS;
}

MatrixError invert2x2I(doubleMatrix * dest, const intMatrix *a)
{
    if (a->width != 2 || a->width != 2)
        return DIMENSION_ERROR;

    if (dest->width != 2 || dest->height != 2)
        return DIMENSION_ERROR;

    double det = (double)determinant2x2I(a);
    if (!det)
        return MATH_ERROR;

    double d = 1.0 / det;

    double tmp0 = (double) a->data[0];

    dest->data[0] = (double) (a->data[3] * d);
    dest->data[1] = (double) (-a->data[1] * d);
    dest->data[2] = (double) (-a->data[2] * d);
    dest->data[3] = (double) (tmp0 * d);

    return SUCCESS;
}

MatrixError invertD(doubleMatrix * dest, const doubleMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2D(dest, a);

    double det = determinantD(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = atD(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    matrixD(&tmpM, dest->data, dest->width, dest->height);

    MatrixError e = adjointD(dest, &tmpM);

    destroymD(&tmpM);

    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}

MatrixError invertF(doubleMatrix * dest, const floatMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2F(dest, a);

    double det = (double)determinantF(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = (double)atF(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    MatrixError e = matrixD(&tmpM, dest->data, dest->width, dest->height);
    if (e != SUCCESS)
        return e;

    e = adjointD(dest, &tmpM);
    destroymD(&tmpM);
    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}

MatrixError invertI(doubleMatrix * dest, const intMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2I(dest, a);

    double det = (double)determinantI(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = (double)atI(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    MatrixError e = matrixD(&tmpM, dest->data, dest->width, dest->height);
    if (e != SUCCESS)
        return e;

    e = adjointD(dest, &tmpM);
    destroymD(&tmpM);
    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}

MatrixError stdInvertD(doubleMatrix * dest, const doubleMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2D(dest, a);

    double det = determinantD(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = atD(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    MatrixError e = matrixD(&tmpM, dest->data, dest->width, dest->height);
    if (e != SUCCESS)
        return e;

    e = stdAdjointD(dest, &tmpM);

    destroymD(&tmpM);
    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}


MatrixError stdInvertF(doubleMatrix * dest, const floatMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2F(dest, a);

    double det = (double)determinantF(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = (double)atF(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    MatrixError e = matrixD(&tmpM, dest->data, dest->width, dest->height);
    if (e != SUCCESS)
        return e;

    e = stdAdjointD(dest, &tmpM);
    destroymD(&tmpM);
    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}


MatrixError stdInvertI(doubleMatrix * dest, const intMatrix *a)
{
    int w = a->width;
    int h = a->height;
    if (w != h)
        return DIMENSION_ERROR;

    if (dest->width != dest->height || dest->data == NULL)
        return DIMENSION_ERROR;

    if (h == 2)
        return invert2x2I(dest, a);

    double det = (double)determinantI(a,0);
    if (!det)
        return MATH_ERROR;

    int y, x;
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
            dest->data[y * dest->width + x] = (double)atI(a, x, y);
        }
    }

    doubleMatrix tmpM = DEFAULT_MATRIX;
    MatrixError e = matrixD(&tmpM, dest->data, dest->width, dest->height);
    if (e != SUCCESS)
        return e;

    e = stdAdjointD(dest, &tmpM);
    destroymD(&tmpM);
    if (e != SUCCESS)
        return e;

    scalarMultiplyD(dest, 1.0/det);
    return SUCCESS;
}

MatrixError getColD(doubleMatrix *dest, const doubleMatrix *a, int x)
{
    if (x < 0 || x >= a->width)
        return FAILURE;
    if (dest->height != a->height || dest->width != 1)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        dest->data[y] = atD(a, x, y);
    }
    return SUCCESS;
}

MatrixError getColF(floatMatrix *dest, const floatMatrix *a, int x)
{
    if (x < 0 || x >= a->width)
        return FAILURE;
    if (dest->height != a->height || dest->width != 1)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        dest->data[y] = atF(a, x, y);
    }
    return SUCCESS;
}

MatrixError getColI(intMatrix *dest, const intMatrix *a, int x)
{
    if (x < 0 || x >= a->width)
        return FAILURE;
    if (dest->height != a->height || dest->width != 1)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        dest->data[y] = atI(a, x, y);
    }
    return SUCCESS;
}

MatrixError getRowD(doubleMatrix *dest, const doubleMatrix *a, int y)
{
    if (y < 0 || y >= a->height)
        return FAILURE;
    if (dest->width != a->width || dest->height != 1)
        return DIMENSION_ERROR;

    memcpy((void*)dest->data, (void*)(a->data+y*a->width), a->width * sizeof(double));
    return SUCCESS;
}

MatrixError getRowF(floatMatrix *dest, const floatMatrix *a, int y)
{
    if (y < 0 || y >= a->height)
        return FAILURE;
    if (dest->width != a->width || dest->height != 1)
        return DIMENSION_ERROR;

    memcpy((void*)dest->data, (void*)(a->data+ y*a->width), a->width * sizeof(float));
    return SUCCESS;
}
MatrixError getRowI(intMatrix *dest, const intMatrix *a, int y)
{
    if (y < 0 || y >= a->height)
        return FAILURE;
    if (dest->width != a->width || dest->height != 1)
        return DIMENSION_ERROR;

    memcpy((void*)dest->data, (void*)(a->data + y*a->width), a->width * sizeof(int));
    return SUCCESS;
}

MatrixError crossProductD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b)
{
    //dest, a & b must be in R^3
    if (a->width != 1 || a->height != 3 || b->width != 1 || b->height != 3 || dest->height != 3 || dest->width != 1)
    {
        return DIMENSION_ERROR;
    }
    dest->data[0] = atD(a, 0, 1) * atD(b, 0, 2) - atD(a, 0, 2) * atD(b, 0, 1);
    dest->data[1] = atD(a, 0, 2) * atD(b, 0, 0) - atD(a, 0, 0) * atD(b, 0, 2);
    dest->data[2] = atD(a, 0, 0) * atD(b, 0, 1) - atD(a, 0, 1) * atD(b, 0, 0);
    return SUCCESS;
}

MatrixError crossProductF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b)
{
    //dest, a & b must be in R^3
    if (a->width != 1 || a->height != 3 || b->width != 1 || b->height != 3 || dest->height != 3 || dest->width != 1)
    {
        return DIMENSION_ERROR;
    }
    dest->data[0] = atF(a, 0, 1) * atF(b, 0, 2) - atF(a, 0, 2) * atF(b, 0, 1);
    dest->data[1] = atF(a, 0, 2) * atF(b, 0, 0) - atF(a, 0, 0) * atF(b, 0, 2);
    dest->data[2] = atF(a, 0, 0) * atF(b, 0, 1) - atF(a, 0, 1) * atF(b, 0, 0);
    return SUCCESS;
}

MatrixError crossProductI(intMatrix *dest, const intMatrix *a, const intMatrix *b)
{
    //dest, a & b must be in R^3
    if (a->width != 1 || a->height != 3 || b->width != 1 || b->height != 3 || dest->height != 3 || dest->width != 1)
    {
        return DIMENSION_ERROR;
    }
    dest->data[0] = atI(a, 0, 1) * atI(b, 0, 2) - atI(a, 0, 2) * atI(b, 0, 1);
    dest->data[1] = atI(a, 0, 2) * atI(b, 0, 0) - atI(a, 0, 0) * atI(b, 0, 2);
    dest->data[2] = atI(a, 0, 0) * atI(b, 0, 1) - atI(a, 0, 1) * atI(b, 0, 0);
    return SUCCESS;
}

double dotProductD(const doubleMatrix *a, const doubleMatrix *b)
{
    //a & b must be in R^n
    if (a->width != 1 || b->width != 1 || a->height != b->height)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
    }

    double output = 0;
    int i;
    for (i = 0; i < a->height; i++)
    {
        output += a->data[i] * b->data[i];
    }
    return output;
}

float dotProductF(const floatMatrix *a, const floatMatrix *b)
{
    //a & b must be in R^n
    if (a->width != 1 || b->width != 1 || a->height != b->height)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
    }

    float output = 0;
    int i;
    for (i = 0; i < a->height; i++)
    {
        output += a->data[i] * b->data[i];
    }
    return output;
}

int dotProductI(const intMatrix *a, const intMatrix *b)
{
    //a & b must be in R^n
    if (a->width != 1 || b->width != 1 || a->height != b->height)
    {
        puts(getErrorMessage(DIMENSION_ERROR));
    }

    int output = 0;
    int i;
    for (i = 0; i < a->height; i++)
    {
        output += a->data[i] * b->data[i];
    }
    return output;
}

MatrixError multiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b)
{
    int size = a->width > a->height ? a->width : a->height;
    if (size > 10 && size <= NTHREADS)
    {
        return paraMultiplyD(dest, a, b);
    }
    else
    {
        return stdMultiplyD(dest, a, b);
    }
}

void * multiplyThreadD(void * arg)
{
    int x;
    for (x = 0; x < ((multiplyArgD*)arg)->b->width; x++)
    {
        doubleMatrix tmpCol = DEFAULT_MATRIX;
        MatrixError e = matrixNullD(&tmpCol, 1, ((multiplyArgD*)arg)->b->height);
        if (e != SUCCESS)
            return (void*)e;
        e = getColD(&tmpCol, ((multiplyArgD*)arg)->b, x);
        if (e != SUCCESS)
        {
            destroymD(&tmpCol);
            return (void*)e;
        }
        double dp = 0;
        int i;
        for (i = 0; i < tmpCol.height; i++)
        {
            dp += ((multiplyArgD*)arg)->tmpRow->data[i] * tmpCol.data[i];
        }
        ((multiplyArgD*)arg)->dest->data[(((multiplyArgD*)arg)->y)*(((multiplyArgD*)arg)->dest->width) + x] = dp;
        destroymD(&tmpCol);
    }
    return NULL;
}

MatrixError paraMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != dest->width)
        return DIMENSION_ERROR;

    pthread_t threads[NTHREADS];
    multiplyArgD thread_args[NTHREADS];
    doubleMatrix rows[NTHREADS] = {DEFAULT_MATRIX};

    int rc;
    int y;
    for (y = 0; y < a->height; y++)
    {
        MatrixError e = matrixNullD(&rows[y], a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowD(&rows[y], a, y);
        if (e != SUCCESS)
        {
            destroymD(&rows[y]);
            return e;
        }
        multiplyArgD arg = {.dest=dest, .tmpRow=&rows[y], .b=b, .y=y};

        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, multiplyThreadD, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }


    for (y = 0; y < a->height; y++)
    {
        rc = pthread_join(threads[y], NULL);
        if (rc)
            return (MatrixError)rc;
        destroymD(&rows[y]);
    }
    return SUCCESS;
}

MatrixError stdMultiplyD(doubleMatrix *dest, const doubleMatrix *a, const doubleMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != dest->width)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        doubleMatrix tmpRow = DEFAULT_MATRIX;
        MatrixError e = matrixNullD(&tmpRow, a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowD(&tmpRow, a, y);
        if (e != SUCCESS)
        {
            destroymD(&tmpRow);
            return e;
        }

        int x;
        for (x = 0; x < b->width; x++)
        {
            doubleMatrix tmpCol = DEFAULT_MATRIX;
            e = matrixNullD(&tmpCol, 1, b->height);
            if (e != SUCCESS)
                return e;

            e = getColD(&tmpCol, b, x);
            if (e != SUCCESS)
            {
                destroymD(&tmpCol);
                return e;
            }

            double dp = 0;
            int i;
            for (i = 0; i < tmpCol.height; i++)
            {
                dp += tmpRow.data[i] * tmpCol.data[i];
            }
            dest->data[y*dest->width + x] = dp;
            destroymD(&tmpCol);
        }
        destroymD(&tmpRow);
    }
    return SUCCESS;
}

MatrixError multiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b)
{
    int size = a->width > a->height ? a->width : a->height;
    if (size > 10 && size <= NTHREADS)
    {
        return paraMultiplyF(dest, a, b);
    }
    else
    {
        return stdMultiplyF(dest, a, b);
    }
}

void * multiplyThreadF(void * arg)
{
    int x;
    for (x = 0; x < ((multiplyArgF*)arg)->b->width; x++)
    {
        floatMatrix tmpCol = DEFAULT_MATRIX;
        MatrixError e = matrixNullF(&tmpCol, 1, ((multiplyArgF*)arg)->b->height);
        if (e != SUCCESS)
            return (void*)e;
        e = getColF(&tmpCol, ((multiplyArgF*)arg)->b, x);
        if (e != SUCCESS)
        {
            destroymF(&tmpCol);
            return (void*)e;
        }
        float dp = 0;
        int i;
        for (i = 0; i < tmpCol.height; i++)
        {
            dp += ((multiplyArgF*)arg)->tmpRow->data[i] * tmpCol.data[i];
        }
        ((multiplyArgF*)arg)->dest->data[(((multiplyArgF*)arg)->y)*(((multiplyArgF*)arg)->dest->width) + x] = dp;
        destroymF(&tmpCol);
    }
    return NULL;
}

MatrixError paraMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != dest->width)
        return DIMENSION_ERROR;

    pthread_t threads[NTHREADS];
    multiplyArgF thread_args[NTHREADS];
    floatMatrix rows[NTHREADS] = {DEFAULT_MATRIX};

    int rc;
    int y;
    for (y = 0; y < a->height; y++)
    {
        MatrixError e = matrixNullF(&rows[y], a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowF(&rows[y], a, y);
        if (e != SUCCESS)
        {
            destroymF(&rows[y]);
            return e;
        }
        multiplyArgF arg = {.dest=dest, .tmpRow=&rows[y], .b=b, .y=y};

        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, multiplyThreadF, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }


    for (y = 0; y < a->height; y++)
    {
        rc = pthread_join(threads[y], NULL);
        destroymF(&rows[y]);
        if (rc)
            return (MatrixError)rc;
    }
    return SUCCESS;
}

MatrixError stdMultiplyF(floatMatrix *dest, const floatMatrix *a, const floatMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != dest->width)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        floatMatrix tmpRow = DEFAULT_MATRIX;
        MatrixError e = matrixNullF(&tmpRow, a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowF(&tmpRow, a, y);
        if (e != SUCCESS)
        {
            destroymF(&tmpRow);
            return e;
        }

        int x;
        for (x = 0; x < b->width; x++)
        {
            floatMatrix tmpCol = DEFAULT_MATRIX;
            e = matrixNullF(&tmpCol, 1, b->height);
            if (e != SUCCESS)
                return e;

            e = getColF(&tmpCol, b, x);
            if (e != SUCCESS)
            {
                destroymF(&tmpCol);
                return e;
            }

            float dp = 0;
            int i;
            for (i = 0; i < tmpCol.height; i++)
            {
                dp += tmpRow.data[i] * tmpCol.data[i];
            }
            dest->data[y*dest->width + x] = dp;
            destroymF(&tmpCol);
        }
        destroymF(&tmpRow);
    }
    return SUCCESS;
}

MatrixError multiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b)
{
    int size = a->width > a->height ? a->width : a->height;
    if (size > 10 && size <= NTHREADS)
    {
        return paraMultiplyI(dest, a, b);
    }
    else
    {
        return stdMultiplyI(dest, a, b);
    }
}

void * multiplyThreadI(void * arg)
{

    int x;
    for (x = 0; x < ((multiplyArgI*)arg)->b->width; x++)
    {
        intMatrix tmpCol = DEFAULT_MATRIX;
        MatrixError e = matrixNullI(&tmpCol, 1, ((multiplyArgI*)arg)->b->height);
        if (e != SUCCESS)
            return (void*)e;
        e = getColI(&tmpCol, ((multiplyArgI*)arg)->b, x);
        if (e != SUCCESS)
        {
            destroymI(&tmpCol);
            return (void*)e;
        }
        int dp = 0;
        int i;
        for (i = 0; i < tmpCol.height; i++)
        {
            dp += ((multiplyArgI*)arg)->tmpRow->data[i] * tmpCol.data[i];
        }
        ((multiplyArgI*)arg)->dest->data[(((multiplyArgI*)arg)->y)*(((multiplyArgI*)arg)->dest->width) + x] = dp;
        destroymI(&tmpCol);
    }
    return NULL;
}

MatrixError paraMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != dest->width)
        return DIMENSION_ERROR;

    pthread_t threads[NTHREADS];
    multiplyArgI thread_args[NTHREADS];
    intMatrix rows[NTHREADS] = {DEFAULT_MATRIX};

    int rc;
    int y;
    for (y = 0; y < a->height; y++)
    {
        MatrixError e = matrixNullI(&rows[y], a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowI(&rows[y], a, y);
        if (e != SUCCESS)
        {
            destroymI(&rows[y]);
            return e;
        }
        multiplyArgI arg = {.dest=dest, .tmpRow=&rows[y], .b=b, .y=y};

        thread_args[y] = arg;
        rc = pthread_create(&threads[y], NULL, multiplyThreadI, (void *) &thread_args[y]);
        if (rc)
            return (MatrixError)rc;
    }


    for (y = 0; y < a->height; y++)
    {
        pthread_join(threads[y], NULL);
        destroymI(&rows[y]);
        if (rc)
            return (MatrixError)rc;
    }
    return SUCCESS;
}

MatrixError stdMultiplyI(intMatrix *dest, const intMatrix *a, const intMatrix *b)
{
    if (a->width != b->height || a->height != b->width || dest->height != a->height || dest->width != b->width)
        return DIMENSION_ERROR;

    int y;
    for (y = 0; y < a->height; y++)
    {
        intMatrix tmpRow = DEFAULT_MATRIX;
        MatrixError e = matrixNullI(&tmpRow, a->width, 1);
        if (e != SUCCESS)
            return e;

        e = getRowI(&tmpRow, a, y);
        if (e != SUCCESS)
        {
            destroymI(&tmpRow);
            return e;
        }

        int x;
        for (x = 0; x < b->width; x++)
        {
            intMatrix tmpCol = DEFAULT_MATRIX;
            e = matrixNullI(&tmpCol, 1, b->height);
            if (e != SUCCESS)
                return e;

            e = getColI(&tmpCol, b, x);
            if (e != SUCCESS)
            {
                destroymI(&tmpCol);
                return e;
            }

            int dp = 0;
            int i;
            for (i = 0; i < tmpCol.height; i++)
            {
                dp += tmpRow.data[i] * tmpCol.data[i];
            }
            dest->data[y*dest->width + x] = dp;
            destroymI(&tmpCol);
        }
        destroymI(&tmpRow);
    }
    return SUCCESS;
}
