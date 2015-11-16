matrix:
	gcc -Wall -s -O3 -fexceptions -pthread -I include/ -o matrix src/main.c src/matrix.c src/matrixError.c src/matrix-test.c
	strip matrix

check-syntax:
	gcc -o -Wall -S ${CHK_SOURCES}

haskell: Main.hs
	mkdir -p Objects
	gcc -c -o Objects/matrix.o src/matrix.c -I include/ -pthread
	gcc -c -o Objects/matrixError.o src/matrixError.c -I include/
	gcc -c -o Objects/HaskellMatrix.o src/HaskellMatrix.c -I include/ -pthread
	gcc -c -o Objects/matrix-test.o src/matrix-test.c -I include/ -pthread
	ghc --make -Wall -O3 -odir Objects -hidir Interface/ -o HaskellMatrix Main.hs Objects/matrix.o Objects/matrixError.o Objects/HaskellMatrix.o Objects/matrix-test.o -lpthread