matrix-invert-c:
	gcc -Wall -s  -O3 -fexceptions -pthread -I. -o matrix main.c matrix.c matrixError.c matrix-test.c
	strip matrix

check-syntax:
	gcc -o -Wall -S ${CHK_SOURCES}
