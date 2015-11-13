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
        putStrLn "The original Matrix, M:"
        _ <- printmI mat

        tran <- transposeI mat
        putStrLn "M transposed:"
        _ <- printmI tran

        inv <- invertI mat
        putStrLn "M inverted:"
        _ <- printmD inv

        withMatrix 5 5 (map fromIntegral my_matrix) $ \tmp -> do
            mult <- multiplyD tmp inv
            putStrLn "inverse(M) * M:"
            _ <- printmD mult
            freeM mult
            
        freeM tran
        freeM inv
