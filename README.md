# Matrix

### Introduction
matrix is a command line tool, allowing one to easily manipulate a matrix using several tools, invert, transpose & multiply. The tool is built ontop of my matrix.h library and this allows inversion and multiplication to utilise POSIX threads to increase performance.

### Usage
The tool reads matricies from a file, and can output an inverted matrix to either a file, or the terminal.

Tool: Invert\
This tool inverts an nxn matrix and outputs the resulting matrix either to stdout or a specified file (--output flag)

    $ matrix -d | --dimension     -i | --input     [-o | --output]     [-p | --parallel]     [-h | --help]     [-t invert | --tool invert]
        Dimension: The dimension flag must be accompanied by an integer representing the dimension of the input nxn matrix
        Input: The input flag, must be accompanied by a path to an inputfile holding a matrix
        Output: The output flag must be accompanied by a file name to where the output will be written, if it is not provided, the matrix will be printed to the screen
        Parallel: The parallel flag must must be accompanied by yes or no, informing matrix if it should use the POSIX threaded version of it's invert function
        Help: This prints a help message for the invert tool\
        Tool: The tool flag is optional if you wish to use the invert tool since invert is the default tool

Tool: Transpose\
This tool transposes an nxm matrix and outputs the resulting matrix either to stdout or a specified file (--output flag)

    $ matrix -x | --width     -y | --height     -i | --input    -t transpose | --tool transpose     [-o | --output]     [-h | --help]
        Width: The width flag must be accompanied by an integer representing the width of the input nxm matrix
        Height: The height flag must be accompanied by an integer representing the height of the input nxm matrix
        Input: The input flag, must be accompanied by a path to an inputfile holding a matrix
        Output: The output flag must be accompanied by a file name to where the output will be written, if it is not provided, the matrix will be printed to the screen
        Help: This prints a help message for the invert tool
        Tool: The tool flag must specify that you wish to use the transpose tool, the default tool is invert

Tool: Multiply\
This tool transposes an nxm matrix and outputs the resulting matrix either to stdout or a specified file (--output flag)

    $ matrix -x | --width     -y | --height     -i1 | --input-one     -i2 | --input-two    -t multiply | --tool multiply     [-p | --parallel]     [-o | --output]     [-h | --help]
        Width: The width flag must be accompanied by an integer representing the width of the first, leftmost, input nxm matrix
        Height: The height flag must be accompanied by an integer representing the height of the first, leftmost, input nxm matrix
        Input-One: The input-one flag, must be accompanied by a path to an input file holding the first matrix to be multiplied
        Input-Two: The input-two flag, must be accompanied by a path to an input file holding the second matrix to be multiplied
        Parallel: The parallel flag must must be accompanied by yes or no, informing matrix if it should use the POSIX threaded version of it's multiply function
        Output: The output flag must be accompanied by a file name to where the output will be written, if it is not provided, the matrix will be printed to the screen
        Help: This prints a help message for the invert tool
        Tool: The tool flag must specify that you wish to use the transpose tool, the default tool is invert

### Input File
The input file represents a stream of numbers, which will be read, left to right, top to bottom into the matrix of given dimension (remembering that only square matricies are invertable). This means that the input file can be a list of space seperated numbers, tab seperated with newlines or any mixture.

e.g. space seperated
1 8 -9 7 5 0 1 0 4 4 0 0 1 2 5 0 0 0 1 -5 0 0 0 0 1

### Limitations
The tool uses the method of cofactor expansion to invert a matrix, this gives exponential time and limits the size of the input matrix to realistically less than 14x14. Internally, the numbers are represenetd as double precision, this leads to the all too common limitations when working with high precisions.

### Installation
There are two ways to compile the project, both use the g++ compiler. There is included a Makefile with the project, a simple call to 

    $ make
 should be enough to compile the project.
 However, for those who wish to edit the source code, a codebocks file is included as well, and can be used to compile, debug and edit the project.
To test the projecct, a test 5x5 matrix is provied in the file "matrix.txt". Run:
 
    $ ./matrix -d 5 -i matrix.txt --took invert
    $ ./matrix -d 5 -i matrix.txt --parallel no -o matrix-output.txt
Remember, if your using code::blocks, copy matrix.txt into the directory of the binary.
The appropriate output from these tests should be printeed to the terminal and written to the file matrix-output.txt:

1 -8 9 7 17
-0 1 -0 -4 -24
0 -0 1 -2 -15
-0 0 -0 1 5
0 -0 0 -0 1	

