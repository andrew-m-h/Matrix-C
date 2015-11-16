import Matrix.Matrix

my_matrix :: [CInt]
my_matrix = [
    8,  4,  10,     -1,     2, 
    -11,    -3,     9,  3,  -5, 
    13,     -14,    0,  -9,     -7, 
    -8,     -12,    3,  -3,     -14, 
    5,  12,     9,  13,     5]

main :: IO()
main = do
    _ <- test
    withMatrix (5::Int) 5 my_matrix $ \mat -> do 
        putStrLn "The original Matrix, M:"
        _ <- printm mat

        putStrLn "The Cofactor Matrix of M:"
        cof <- paraCofactor mat 
        putStrLn $ show cof

        putStrLn "The Determinant of M"
        det <- (determinant mat)
        putStrLn $ show (det :: Int) ++ "\n"

        tran <- transpose mat
        putStrLn "M transposed:"
        _ <- printm tran

        inv <- invert mat
        putStrLn "M inverted:"
        _ <- printm inv

        tmp <- toCDoubleMatrix mat
        mult <- multiply tmp inv
        putStrLn "inverse(M) * M:"
        _ <- printm mult
        
        freeM mult
        freeM tmp
        freeM tran
        freeM inv
