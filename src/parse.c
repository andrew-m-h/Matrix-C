/*
Created by Andrew M. Hall
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "parse.h"

#define TRUE 1
#define FALSE 0

ParseError createError(const char* flag, PARSE_ERROR_CODE code){
    ParseError err = {.code=code, .flag=flag};
    return err;
}

ParseError parse(Argument* argList, int numArgs, char* args[], int argc){
    int given[1024] = {FALSE};

    int i = 1; //First argument will be Program name
    int n;
    while (i < argc){
        for (n = 0; n < numArgs; n++){
            if(!strcmp(args[i], argList[n].shortcode) || !strcmp(args[i], argList[n].longcode)){
                int tmpI; float tmpF;
                switch (argList[n].type){
                case INT:
                    if (i == argc-1)
                        return createError(args[i], MANDATORY_ARG_MISSING);
                    tmpI = atoi(args[i+1]);
                    if (tmpI == 0)
                        return createError(args[i+1], INT_CAST_ERROR);
                    argList[n].value.i = tmpI;
                    i += 2;
                    break;
                case FLOAT:
                    if (i == argc-1)
                        return createError(args[i], MANDATORY_ARG_MISSING);
                    tmpF = atof(args[i+1]);
                    if (tmpF == 0)
                        return createError(args[i+1], FLOAT_CAST_ERROR);
                    argList[n].value.f = (float)tmpF;
                    i+=2;
                    break;
                case STRING:
                    if (i == argc-1)
                        return createError(args[i], MANDATORY_ARG_MISSING);
                    argList[n].value.s = args[i+1];
                    i += 2;
                    break;
                case BOOLEAN:
                    argList[n].value.b = TRUE;
                    i++;
                    break;
                default:
                    return createError(args[i], PARSE_FAILURE);
                }

                if (argList[n].mandatory)
                    given[n] = TRUE;
                break;
            }
            if (n == numArgs-1)
                return createError(args[i],INVALID_FLAG);
        }
    }

    //Check mandatory arguments given
    for (n = 0; n < numArgs && n < 1024; n++){
        if (argList[n].mandatory && !given[n])
            return createError(argList[n].longcode, MANDATORY_ARG_MISSING);
    }

    return createError(NULL, PARSE_SUCCESS);
}

void printParseError(ParseError err){
    puts("Error occurred when parsing command line arguments:");
    switch (err.code){
    case INT_CAST_ERROR:
        printf("\tUnable to parse integer from: %s\n", err.flag);
        break;
    case FLOAT_CAST_ERROR:
        printf("\tUnable to parse float from: %s\n", err.flag);
        break;
    case STRING_CPY_ERROR:
        printf("\tUnable to copy string: %s\n", err.flag);
        break;
    case MANDATORY_ARG_MISSING:
        printf("\tAt least one argument was missing: %s\n", err.flag);
        break;
    case INVALID_FLAG:
        printf("\tAn invalid flag was passed: %s\n", err.flag);
        break;
    case PARSE_FAILURE:
        printf("\tA general parse failure occurred: %s\n", err.flag);
        break;
    case PARSE_SUCCESS:
        break;
    }
}
