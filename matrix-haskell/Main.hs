import Matrix

my_matrix :: [CInt]
my_matrix = [
    8,  4,  10,     -1,     2, 
    -11,    -3,     9,  3,  -5, 
    13,     -14,    0,  -9,     -7, 
    -8,     -12,    3,  -3,     -14, 
    5,  12,     9,  13,     5]

main :: IO()
main =
    withMatrix 5 5 my_matrix $ \mat -> do
        inv <- invertI mat

        _ <- printmI mat
        _ <- printmD inv

        tran <- transposeI mat
        _ <- printmI tran

        freeM tran
        freeM inv
