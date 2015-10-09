matrix-invert-c:
	gcc -Wall -s  -O2 -fexceptions -O2 -pthread -I. -o invert-matrix main.c matrix.c
	strip invert-matrix

check-syntax:
	gcc -o -Wall -S ${CHK_SOURCES}
