matrix-invert-c:
	gcc -Wall -s  -O3 -fexceptions -O2 -pthread -I. -o matrix main.c matrix.c matrixError.c
	strip matrix

check-syntax:
	gcc -o -Wall -S ${CHK_SOURCES}
