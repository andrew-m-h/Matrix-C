all:
	mkdir -p Objects
	gcc -c -o Objects/matrix.o src/matrix.c -I include/ -pthread
	gcc -c -o Objects/matrixError.o src/matrixError.c -I include/
	gcc -c -o Objects/HaskellMatrix.o src/HaskellMatrix.c -I include/ -pthread
	ghc --make -Wall -O3 -odir Objects -hidir Interface/ Main.hs Matrix.hs Objects/matrix.o Objects/matrixError.o Objects/HaskellMatrix.o -lpthread