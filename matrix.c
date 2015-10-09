/*
Created by Andrew M. Hall
*/

#include "matrix.h"
#include <string.h>
#include <stdio.h>

#define BUFF_LENGTH 2048

//PThread
typedef struct {
    doubleMatrix * dest;
    const doubleMatrix * a;
    int y;
} cofactorArgD;

typedef struct {
    floatMatrix * dest;
    const floatMatrix * a;
    int y;
} cofactorArgF;

typedef struct {
    intMatrix * dest;
    const intMatrix * a;
    int y;
} cofactorArgI;

BOOL matrixD(doubleMatrix * dest, double * in_data, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    double *ptr = (double*)calloc(size*size, sizeof(double));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(double));
        return FALSE;
    }
    int y, x;
    int i = 0;
    for (y = 0; y < in_height; y++){
        for (x = 0; x < in_width; x++){
            ptr[y * size + x] = in_data[i++];
        }
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL matrixF(floatMatrix * dest, float * in_data, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    float *ptr = (float*)calloc(size*size, sizeof(float));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(float));
        return FALSE;
    }
    int y, x;
    int i = 0;
    for (y = 0; y < in_height; y++){
        for (x = 0; x < in_width; x++){
            ptr[y * size + x] = in_data[i++];
        }
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL matrixI(intMatrix * dest, int * in_data, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    int *ptr = (int*)calloc(size*size, sizeof(int));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(int));
        return FALSE;
    }
    int y, x;
    int i = 0;
    for (y = 0; y < in_height; y++){
        for (x = 0; x < in_width; x++){
            ptr[y * size + x] = in_data[i++];
        }
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL matrixNullD(doubleMatrix * dest, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    double *ptr = (double*)calloc(size*size, sizeof(double));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(double));
        return FALSE;
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL matrixNullF(floatMatrix * dest, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    float *ptr = (float*)calloc(size*size, sizeof(float));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(float));
        return FALSE;
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL matrixNullI(intMatrix * dest, int in_width, int in_height){
    int size = in_width > in_height ? in_width : in_height;

    int *ptr = (int*)calloc(size*size, sizeof(int));
    if (ptr == NULL){
        printf("failed to allocate dynamic memory of size %lu bytes\n", in_width*in_height*sizeof(int));
        return FALSE;
    }

    dest->data = ptr; dest->height=in_height; dest->width=in_width; dest->size=size;
    return TRUE;
}

BOOL destroymD(doubleMatrix * a){
    if (a->data == NULL){
        a = NULL;
        return FALSE;
    }
    free(a->data);
    a = NULL;
    return TRUE;
}

BOOL destroymF(floatMatrix * a){
    if (a->data == NULL){
        a = NULL;
        return FALSE;
    }
    free(a->data);
    a = NULL;
    return TRUE;
}

BOOL destroymI(intMatrix * a){
    if (a->data == NULL){
        a = NULL;
        return FALSE;
    }
    free(a->data);
    a = NULL;
    return TRUE;
}

BOOL matrixcpyD(doubleMatrix * dest, const doubleMatrix * src){
    if (dest->width != src->width || dest->width != src->width){
        printf("Destination matrix not formatted for matrixcpy call");
        return FALSE;
    }
    int y, x;
    for (y = 0; y < src->height; y++){
        for (x = 0; x < src->width; x++){
            dest->data[y * dest->size + x] = atD(src, x, y);
        }
    }
    return TRUE;
}

BOOL matrixcpyF(floatMatrix * dest, const floatMatrix * src){
    if (dest->width != src->width || dest->width != src->width){
        printf("Destination matrix not formatted for matrixcpy call");
        return FALSE;
    }
    int y, x;
    for (y = 0; y < src->height; y++){
        for (x = 0; x < src->width; x++){
            dest->data[y * dest->size + x] = atF(src, x, y);
        }
    }
    return TRUE;
}

BOOL matrixcpyI(intMatrix * dest, const intMatrix * src){
    if (dest->width != src->width || dest->width != src->width){
        printf("Destination matrix not formated for matrixcpy call");
        return FALSE;
    }
    int y, x;
    for (y = 0; y < src->height; y++){
        for (x = 0; x < src->width; x++){
            dest->data[y * dest->size + x] = atI(src, x, y);
        }
    }
    return TRUE;
}

BOOL printmD(const doubleMatrix *a){
    int mul = 10;
    size_t buff_length = a->size * a->height * mul + 1;
    char * strBuff = (char*)calloc(buff_length, sizeof(char));

    int count = 0;
    while (!toStringD(strBuff, a, buff_length) && count <= 10){
        mul += 2;
        buff_length = a->size * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)calloc(buff_length, sizeof(char));
        count++;
    }
    if (count == 11){
        printf("could not print full matrix\n");
        return FALSE;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return TRUE;
}

BOOL printmF(const floatMatrix *a){
    int mul = 10;
    size_t buff_length = a->size * a->height * mul + 1;
    char * strBuff = (char*)calloc(buff_length, sizeof(char));

    int count = 0;
    while (!toStringF(strBuff, a, buff_length) && count <= 5){
        mul += 2;
        buff_length = a->size * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)calloc(buff_length, sizeof(char));
        count++;
    }
    if (count == 6){
        printf("could not print full matrix\n");
        return FALSE;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return TRUE;
}

BOOL printmI(const intMatrix *a){
    int mul = 10;
    size_t buff_length = a->size * a->height * mul + 1;
    char * strBuff = (char*)calloc(buff_length, sizeof(char));

    int count = 0;
    while (!toStringI(strBuff, a, buff_length) && count <= 5){
        mul += 2;
        buff_length = a->size * a->height * mul + 1;
        free(strBuff);
        strBuff = (char*)calloc(buff_length, sizeof(char));
        count++;
    }
    if (count == 6){
        printf("could not print full matrix\n");
        return FALSE;
    }

    printf("%s\n", strBuff);
    fflush(stdout);
    free(strBuff);
    return TRUE;
}

double atD(const doubleMatrix* a, int x, int y){
    return a->data[y * a->size + x];
}

float atF(const floatMatrix* a, int x, int y){
    return a->data[y * a->size + x];
}

int atI(const intMatrix* a, int x, int y){
    return a->data[y * a->size + x];
}

BOOL addD(doubleMatrix *a, const doubleMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] += b->data[y * b->size + x];
        }
    }
    return TRUE;
}

BOOL addF(floatMatrix *a, const floatMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] += b->data[y * b->size + x];
        }
    }
    return TRUE;
}

BOOL addI(intMatrix *a, const intMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] += b->data[y * b->size + x];
        }
    }
    return TRUE;
}

BOOL subtractD(doubleMatrix *a, const doubleMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] -= b->data[y * b->size + x];
        }
    }
    return TRUE;
}
BOOL subtractF(floatMatrix *a, const floatMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] -= b->data[y * b->size + x];
        }
    }
    return TRUE;
}
BOOL subtractI(intMatrix *a, const intMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] -= b->data[y * b->size + x];
        }
    }
    return TRUE;
}

void negativeD(doubleMatrix *a){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= (-1);
        }
    }
}

void negativeF(floatMatrix *a){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= (-1);
        }
    }
}

void negativeI(intMatrix *a){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= (-1);
        }
    }
}

void scalarMultiplyD(doubleMatrix * a, const double b){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= b;
        }
    }
}

void scalarMultiplyF(floatMatrix *a, const float b){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= b;
        }
    }
}

void scalarMultiplyI(intMatrix *a, const int b){
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            a->data[y * a->size + x] *= b;
        }
    }
}

BOOL cmpD(const doubleMatrix *a, const doubleMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->height; x++){
            if (a->data[y * a->size + x] != b->data[y * b->size + x])
                return FALSE;
        }
    }
    return TRUE;
}

BOOL cmpF(const floatMatrix *a, const floatMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->height; x++){
            if (a->data[y * a->size + x] != b->data[y * b->size + x])
                return FALSE;
        }
    }
    return TRUE;
}

BOOL cmpI(const intMatrix *a, const intMatrix *b){
    if (a->height != b->height || a->width != b->width)
        return FALSE;
    int y, x;
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->height; x++){
            if (a->data[y * a->size + x] != b->data[y * b->size + x])
                return FALSE;
        }
    }
    return TRUE;
}

void transposeD(doubleMatrix *a){
    int y, x;
    for (y = 0; y < a->size; y++){
        for (x = 0; x < y; x++){
            double tmp = a->data[y*a->size + x];
            a->data[y*a->size + x] = a->data[x*a->size + y];
            a->data[x*a->size + y] = tmp;
        }
    }
    int tmp = a->width;
    a->width = a->height;
    a->height = tmp;
}

void transposeF(floatMatrix *a){
    int y, x;
    for (y = 0; y < a->size; y++){
        for (x = 0; x < y; x++){
            float tmp = a->data[y*a->size + x];
            a->data[y*a->size + x] = a->data[x*a->size + y];
            a->data[x*a->size + y] = tmp;
        }
    }
    int tmp = a->width;
    a->width = a->height;
    a->height = tmp;
}

void transposeI(intMatrix *a){
    int y, x;
    for (y = 0; y < a->size; y++){
        for (x = 0; x < y; x++){
            int tmp = a->data[y*a->size + x];
            a->data[y*a->size + x] = a->data[x*a->size + y];
            a->data[x*a->size + y] = tmp;
        }
    }
    int tmp = a->width;
    a->width = a->height;
    a->height = tmp;
}

BOOL toStringD(char* dest, const doubleMatrix *a, size_t buffSize){
    int y, x;
    size_t length = buffSize - 1;
    if (!dest){
        printf("Destination buffer was null pointer\n");
        return FALSE;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            char strBuff[50] = {'\0'};
            sprintf(strBuff, "%f", atD(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l){
                length -= l;
            } else {
                return FALSE;
            }
            strncat(dest, "\t", length);
            if (length >= 2){
                length -= 2;
            } else {
                return FALSE;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2){
            length -= 2;
        } else {
            return FALSE;
        }
    }
    return TRUE;
}

BOOL toStringF(char* dest, const floatMatrix *a, size_t buffSize){
    int y, x;
    size_t length = buffSize - 1;
    if (!dest){
        printf("Destination buffer was null pointer\n");
        return FALSE;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            char strBuff[50] = {'\0'};
            sprintf(strBuff, "%f", atF(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l){
                length -= l;
            } else {
                return FALSE;
            }
            strncat(dest, "\t", length);
            if (length >= 2){
                length -= 2;
            } else {
                return FALSE;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2){
            length -= 2;
        } else {
            return FALSE;
        }
    }
    return TRUE;
}

BOOL toStringI(char* dest, const intMatrix *a, size_t buffSize){
    int y, x;
    size_t length = buffSize - 1;
    if (!dest){
        printf("Destination buffer was null pointer\n");
        return FALSE;
    }
    dest[0] = '\0';
    for (y = 0; y < a->height; y++){
        for (x = 0; x < a->width; x++){
            char strBuff[50] = {'\0'};
            sprintf(strBuff, "%d", atI(a, x, y));
            strncat(dest, strBuff, length);
            size_t l = strnlen(strBuff, 50);
            if (length >= l){
                length -= l;
            } else {
                return FALSE;
            }
            strncat(dest, "\t", length);
            if (length >= 2){
                length -= 2;
            } else {
                return FALSE;
            }
        }
        strncat(dest, "\n", length);
        if (length >= 2){
            length -= 2;
        } else {
            return FALSE;
        }
    }
    return TRUE;
}

double determinant2x2D(const doubleMatrix *a)
{
    if (a->height != 2 || a->width != 2){
        printf("input matrix to determinant2x2D is not a 2x2 matrix\n");
        exit(1);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

float determinant2x2F(const floatMatrix *a){
    if (a->height != 2 || a->width != 2){
        printf("input matrix to determinant2x2F is not a 2x2 matrix\n");
        exit(1);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

int determinant2x2I(const intMatrix *a){
    if (a->height != 2 || a->width != 2){
        printf("input matrix to determinant2x2I is not a 2x2 matrix\n");
        exit(1);
    }
    return a->data[0] * a->data[3] - a->data[1] * a->data[2];
}

double determinantD(const doubleMatrix *a, int row){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take determinant of non square matrix\n");
        exit(1);
    }
    double output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2){
        return determinant2x2D(a);
    } else {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++){
            int ycount = 0;

            doubleMatrix tmp;
            if (!matrixNullD(&tmp, w-1, h-1)){
                printf("failed to allocate dynamic memory\n");
                exit(1);
            }
            int y;
            for (y = 0; y < h; y++){
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++){
                        if (x != tmpX){
                            tmp.data[ycount * tmp.size + (xcount++)] = a->data[y * a->size + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantD(&tmp,0) * (a->data[row * a->size + tmpX]))) : -((determinantD(&tmp,0) * (a->data[row * a->size + tmpX])));
            s = !s;
            destroymD(&tmp);
        }
    }
    return output;
}

float determinantF(const floatMatrix *a, int row){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take determinant of non square matrix\n");
        exit(1);
    }
    float output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2){
        return determinant2x2F(a);
    } else {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++){
            int ycount = 0;
            floatMatrix tmp;
            if (!matrixNullF(&tmp, w-1, h-1)){
                printf("failed to allocate dynamic memory\n");
                exit(1);
            }
            int y;
            for (y = 0; y < h; y++){
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++){
                        if (x != tmpX){
                            tmp.data[ycount * tmp.size + (xcount++)] = a->data[y * a->size + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantF(&tmp,0) * (a->data[row * a->size + tmpX]))) : -((determinantF(&tmp,0) * (a->data[row * a->size + tmpX])));
            s = !s;
            destroymF(&tmp);
        }
    }
    return output;
}

int determinantI(const intMatrix *a, int row){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take determinant of non square matrix\n");
        exit(1);
    }
    int output = 0;

    BOOL s = row % 2 == 0;
    if (h == 2){
        return determinant2x2I(a);
    } else {
        int tmpX;
        for (tmpX = 0; tmpX < w; tmpX++){
            int ycount = 0;

            intMatrix tmp;
            if (!matrixNullI(&tmp, w-1, h-1)){
                printf("failed to allocate dynamic memory\n");
                exit(1);
            }
            int y;
            for (y = 0; y < h; y++){
                if (y != row)
                {
                    int xcount = 0;
                    int x;
                    for (x = 0; x < w; x++){
                        if (x != tmpX){
                            tmp.data[ycount * tmp.size + (xcount++)] = a->data[y * a->size + x];
                        }
                    }
                    ycount++;
                }
            }
            output += s ? ((determinantI(&tmp,0) * (a->data[row * a->size + tmpX]))) : -((determinantI(&tmp,0) * (a->data[row * a->size + tmpX])));
            s = !s;
            destroymI(&tmp);
        }
    }
    return output;
}

BOOL cofactorD(doubleMatrix * dest, const doubleMatrix *a){
    if (a->size > 8 && a->size <= NTHREADS){
        return paraCofactorD(dest, a);
    } else {
        return stdCofactorD(dest, a);
    }
}

BOOL cofactorI(intMatrix * dest, const intMatrix *a){
    if (a->size > 8 && a->size <= NTHREADS){
        return paraCofactorI(dest, a);
    } else {
        return stdCofactorI(dest, a);
    }
}

BOOL cofactorF(floatMatrix * dest, const floatMatrix *a){
    if (a->size > 8 && a->size <= NTHREADS){
        return paraCofactorF(dest, a);
    } else {
        return stdCofactorF(dest, a);
    }
}

BOOL stdCofactorD(doubleMatrix * dest, const doubleMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formatted correctly for cofactor call\n");
        return FALSE;
    }

    int y;
    for (y = 0; y < h; y++){
        int x;
        for (x = 0; x < w; x++)
        {
            doubleMatrix det;
            matrixNullD(&det, w-1, h-1);
            int ycount = 0;
            int ydet;
            for (ydet = 0; ydet < h; ydet++){
                int xcount = 0;
                if (ydet != y)
                {
                    int xdet;
                    for (xdet = 0; xdet < w; xdet++)
                    {
                        if (xdet != x)
                        {
                            det.data[ycount * det.size + (xcount++)] = atD(a, xdet, ydet);
                        }
                    }
                    ycount++;
                }
            }

            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->size + x] = determinantD(&det, 0);
            }
            else
            {
                dest->data[y * dest->size + x] = -determinantD(&det, 0);
            }
            destroymD(&det);
        }
    }
    return TRUE;
}

void * cofactorThreadD(void * m){
    int s = ((cofactorArgD *)m)->dest->size;
    int x;
    for (x = 0; x < s; x++){
        doubleMatrix det;

        matrixNullD(&det, s-1, s-1);
        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++){
            int xcount = 0;
            if (ydet != ((cofactorArgD *)m)->y){
                int xdet;
                for (xdet = 0; xdet < s; xdet++){
                    if (xdet != x){
                        det.data[ycount * det.size + (xcount++)] = atD(((cofactorArgD *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgD *)m)->y + x) % 2 == 0){
            ((cofactorArgD *)m)->dest->data[((cofactorArgD *)m)->y * s + x] = determinantD(&det, 0);
        }
        else
        {
            ((cofactorArgD *)m)->dest->data[((cofactorArgD *)m)->y * s + x] = -determinantD(&det, 0);
        }
        destroymD(&det);
    }
    return NULL;
}

BOOL paraCofactorD(doubleMatrix * dest, const doubleMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formatted correctly for cofactor call\n");
        return FALSE;
    }

    pthread_t threads[NTHREADS];
    cofactorArgD thread_args[NTHREADS];

    int y;
    for (y = 0; y < h; y++){
        cofactorArgD arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        pthread_create(&threads[y], NULL, cofactorThreadD, (void *) &thread_args[y]);
    }
    for (y = 0; y < h; y++){
        pthread_join(threads[y], NULL);
    }
    return TRUE;
}

BOOL stdCofactorF(floatMatrix * dest, const floatMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formated correctly for cofactor call\n");
        return FALSE;
    }

    int y;
    for (y = 0; y < h; y++)
    {
        int x;
        for (x = 0; x < w; x++)
        {
            floatMatrix det;
            matrixNullF(&det, w-1, h-1);

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
                            det.data[ycount * det.size + (xcount++)] = a->data[ydet * a->size + xdet];
                        }
                    }
                    ycount++;
                }
            }
            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->size + x] = determinantF(&det, 0);
            }
            else
            {
                dest->data[y * dest->size + x] = -determinantF(&det, 0);
            }
            destroymF(&det);
        }
    }
    return TRUE;
}


void * cofactorThreadF(void * m){
    int s = ((cofactorArgF *)m)->dest->size;
    int x;
    for (x = 0; x < s; x++){
        floatMatrix det;
        matrixNullF(&det, s-1, s-1);

        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++){
            int xcount = 0;
            if (ydet != ((cofactorArgF *)m)->y){
                int xdet;
                for (xdet = 0; xdet < s; xdet++){
                    if (xdet != x){
                        det.data[ycount * det.size + (xcount++)] = atF(((cofactorArgF *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgF *)m)->y + x) % 2 == 0){
            ((cofactorArgF *)m)->dest->data[((cofactorArgF *)m)->y * s + x] = determinantF(&det, 0);
        }
        else
        {
            ((cofactorArgF *)m)->dest->data[((cofactorArgF *)m)->y * s + x] = -determinantF(&det, 0);
        }
        destroymF(&det);
    }
    return NULL;
}

BOOL paraCofactorF(floatMatrix * dest, const floatMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formatted correctly for cofactor call\n");
        return FALSE;
    }

    pthread_t threads[NTHREADS];
    cofactorArgF thread_args[NTHREADS];

    int y;
    for (y = 0; y < h; y++){
        cofactorArgF arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        pthread_create(&threads[y], NULL, cofactorThreadF, (void *) &thread_args[y]);
    }
    for (y = 0; y < h; y++){
        pthread_join(threads[y], NULL);
    }
    return TRUE;
}

BOOL stdCofactorI(intMatrix * dest, const intMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formated correctly for cofactor call\n");
        return FALSE;
    }

    int y;
    for (y = 0; y < h; y++)
    {
        int x;
        for (x = 0; x < w; x++)
        {
            int *ptr2 = (int*)calloc((a->size - 1) * (a->size - 1), sizeof(int));
            if (ptr2 == NULL){
                printf("failed to allocate dynamic memory of size %lu bytes\n", sizeof(int)*(a->size - 1) * (a->size - 1));
                return FALSE;
            }

            intMatrix det;
            matrixNullI(&det, w-1, h-1);
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
                            det.data[ycount * det.size + (xcount++)] = a->data[ydet * a->size + xdet];
                        }
                    }
                    ycount++;
                }
            }
            if ((y + x) % 2 == 0)
            {
                dest->data[y * dest->size + x] = determinantI(&det, 0);
            }
            else
            {
                dest->data[y * dest->size + x] = -determinantI(&det, 0);
            }
            destroymI(&det);
        }
    }
    return TRUE;
}

void * cofactorThreadI(void * m){
    int s = ((cofactorArgI *)m)->dest->size;
    int x;
    for (x = 0; x < s; x++){
        intMatrix det;
        matrixNullI(&det, s-1, s-1);

        int ycount = 0;
        int ydet;
        for (ydet = 0; ydet < s; ydet++){
            int xcount = 0;
            if (ydet != ((cofactorArgI *)m)->y){
                int xdet;
                for (xdet = 0; xdet < s; xdet++){
                    if (xdet != x){
                        det.data[ycount * det.size + (xcount++)] = atI(((cofactorArgI *)m)->a, xdet, ydet);
                    }
                }
                ycount++;
            }
        }

        if ((((cofactorArgI *)m)->y + x) % 2 == 0){
            ((cofactorArgI *)m)->dest->data[((cofactorArgI *)m)->y * s + x] = determinantI(&det, 0);
        }
        else
        {
            ((cofactorArgI *)m)->dest->data[((cofactorArgI *)m)->y * s + x] = -determinantI(&det, 0);
        }
        destroymI(&det);
    }
    return NULL;
}

BOOL paraCofactorI(intMatrix * dest, const intMatrix *a){
    int w = a->width;
    int h = a->height;
    if (h != w){
        printf("cannot take cofactor of non square matrix\n");
        return FALSE;
    }
    if (dest->height != h || dest->width != w){
        printf("Destination matrix not formatted correctly for cofactor call\n");
        return FALSE;
    }

    pthread_t threads[NTHREADS];
    cofactorArgI thread_args[NTHREADS];

    int y;
    for (y = 0; y < h; y++){
        cofactorArgI arg = {.dest=dest, .a=a, .y=y};
        thread_args[y] = arg;
        pthread_create(&threads[y], NULL, cofactorThreadI, (void *) &thread_args[y]);
    }
    for (y = 0; y < h; y++){
        pthread_join(threads[y], NULL);
    }
    return TRUE;
}

BOOL adjointD(doubleMatrix * dest, const doubleMatrix *a){
    if (cofactorD(dest, a)){
        transposeD(dest);
        return TRUE;
    }
    return FALSE;
}

BOOL adjointF(floatMatrix * dest, const floatMatrix *a){
    if (cofactorF(dest, a)){
        transposeF(dest);
        return TRUE;
    }
    return FALSE;
}


BOOL adjointI(intMatrix * dest, const intMatrix *a){
    if (cofactorI(dest, a)){
        transposeI(dest);
        return TRUE;
    }
    return FALSE;
}

BOOL invert2x2D(doubleMatrix * dest, const doubleMatrix *a){
    if (a->width != 2 || a->width != 2){
        printf("cannot call invert2x3F on non 2x2 matrix\n");
        return FALSE;
    }
    if (dest->width != 2 || dest->height != 2){
        printf("Destination matrix not formatted propperly for inversion\n");
        return FALSE;
    }

    double det = determinant2x2D(a);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        return FALSE;
    }
    double d = 1.0 / det;

    double tmp0 = a->data[0];

    dest->data[0] = a->data[3] * d;
    dest->data[1] = -a->data[1] * d;
    dest->data[2] = -a->data[2] * d;
    dest->data[3] = tmp0 * d;

    return TRUE;
}

BOOL invert2x2F(doubleMatrix * dest, const floatMatrix *a){
    if (a->width != 2 || a->width != 2){
        printf("cannot call invert2x2F on non 2x2 matrix\n");
        return FALSE;
    }

    if (dest->width != 2 || dest->height != 2){
        printf("Destination matrix not formatted propperly for inversion\n");
        return FALSE;
    }

    double det = (double)determinant2x2F(a);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        return FALSE;
    }
    double d = 1.0 / det;

    double tmp0 = (double) a->data[0];

    dest->data[0] = (double) (a->data[3] * d);
    dest->data[1] = (double) (-a->data[1] * d);
    dest->data[2] = (double) (-a->data[2] * d);
    dest->data[3] = (double) (tmp0 * d);

    return TRUE;
}

BOOL invert2x2I(doubleMatrix * dest, const intMatrix *a){
    if (a->width != 2 || a->width != 2){
        printf("cannot call invert2x2F on non 2x2 matrix\n");
        return FALSE;
    }
    if (dest->width != 2 || dest->height != 2){
        printf("Destination matrix not formatted propperly for inversion\n");
        return FALSE;
    }

    double det = (double)determinant2x2I(a);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        return FALSE;
    }
    double d = 1.0 / det;

    double tmp0 = (double) a->data[0];

    dest->data[0] = (double) (a->data[3] * d);
    dest->data[1] = (double) (-a->data[1] * d);
    dest->data[2] = (double) (-a->data[2] * d);
    dest->data[3] = (double) (tmp0 * d);

    return TRUE;
}

BOOL invertD(doubleMatrix * dest, const doubleMatrix *a){
    int w = a->width;
    int h = a->height;
    if (w != h){
        printf("cannot call invert2x3F on non 2x2 matrix\n");
        exit(1);
    }
    if (dest->width != dest->height || dest->data == NULL){
        printf("Destination matrix not initialised appropriatly for inverting\n");
        return FALSE;
    }
    if (h == 2){
        return invert2x2D(dest, a);
    }

    double det = determinantD(a,0);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        return FALSE;
    }

    int y, x;
    for (y = 0; y < h; y++){
        for (x = 0; x < w; x++){
            dest->data[y * dest->size + x] = atD(a, x, y);
        }
    }

    doubleMatrix tmpM;
    matrixD(&tmpM, dest->data, dest->width, dest->height);

    if (!adjointD(dest, &tmpM))
        return FALSE;

    destroymD(&tmpM);

    scalarMultiplyD(dest, 1.0/det);
    return TRUE;
}

BOOL invertF(doubleMatrix * dest, const floatMatrix *a){
    int w = a->width;
    int h = a->height;
    if (w != h){
        printf("cannot call invert2x2F on non 2x2 matrix\n");
        return FALSE;
    }
    if (dest->width != dest->height || dest->data == NULL){
        printf("Destination matrix not initialised appropriatly for inverting\n");
        return FALSE;
    }
    if (h == 2){
        return invert2x2F(dest, a);
    }

    double det = (double)determinantF(a,0);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        return FALSE;
    }

    int y, x;
    for (y = 0; y < h; y++){
        for (x = 0; x < w; x++){
            dest->data[y * dest->size + x] = (double)atF(a, x, y);
        }
    }

    doubleMatrix tmpM;
    matrixD(&tmpM, dest->data, dest->width, dest->height);

    if (!adjointD(dest, &tmpM))
        return FALSE;

    destroymD(&tmpM);

    scalarMultiplyD(dest, 1.0/det);
    return TRUE;
}

BOOL invertI(doubleMatrix * dest, const intMatrix *a){
    int w = a->width;
    int h = a->height;
    if (w != h){
        printf("cannot call invert2x3F on non 2x2 matrix\n");
        return FALSE;
    }
    if (dest->width != dest->height || dest->data == NULL){
        printf("Destination matrix not initialised appropriatly for inverting\n");
        return FALSE;
    }
    if (h == 2){
        return invert2x2I(dest, a);
    }

    double det = (double)determinantI(a,0);
    if (!det){
        printf("cannot invert matrix with 0 determinant\n");
        exit(1);
    }

    int y, x;
    for (y = 0; y < h; y++){
        for (x = 0; x < w; x++){
            dest->data[y * dest->size + x] = (double)atI(a, x, y);
        }
    }

    doubleMatrix tmpM;
    matrixD(&tmpM, dest->data, dest->width, dest->height);

    if (!adjointD(dest, &tmpM))
        return FALSE;

    destroymD(&tmpM);


    scalarMultiplyD(dest, 1.0/det);
    return TRUE;
}
