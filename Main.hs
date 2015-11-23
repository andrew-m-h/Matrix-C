{-#LANGUAGE NoImplicitPrelude, FlexibleInstances#-}

import Foreign.C.Types
import Matrix.Matrix (
  withMatrix,invert, stdInvert, transpose, stdMultiply,
  multiply, determinant, test, printm, freeM)
import Matrix.CommandLine(
  IsArgument(..), Argument(..), Union(..),
  parse, fromMandatoryINT, fromMandatorySTRING,
  fromMandatoryOTHER, fromMandatoryPRESENT
                         )
import Prelude
import System.Environment (getArgs)
import System.Random(randomRIO)

data Tool = Invert
          | Transpose
          | Multiply
          | Generate
          | Determinant
          | Test
          | None
          deriving (Eq, Enum, Show)

instance IsArgument Tool where
    fromOption s = case s of
        "invert" -> Just Invert
        "transpose" -> Just Transpose
        "multiply" -> Just Multiply
        "generate" -> Just Generate
        "determinant" -> Just Determinant
        "test" -> Just Test
        _ -> Nothing

findTool :: Tool -> [String] -> Tool 
findTool t lst = case lst of
  x:y:xs
    | x == "--tool" || x == "-t" -> case fromOption y of
      Nothing -> error $ "unable to parse tool: " ++ y
      Just tool -> findTool tool xs
    | otherwise -> findTool t (y:xs)
  _ -> t

data InvertFlag = 
      I_Dimension
    | I_Input
    | I_Output
    | I_Parallel
    | I_Help
    | I_Tool
    deriving (Enum, Eq, Show)

data TransposeFlag = 
      T_Width
    | T_Height
    | T_Input
    | T_Output
    | T_Help
    | T_Tool
    deriving (Enum, Eq)

data MultiplyFlag = 
      M_Width
    | M_Height
    | M_Input_One
    | M_Input_Two
    | M_Output
    | M_Parallel
    | M_Help
    | M_Tool
    deriving (Enum, Eq)

data GenerateFlag = 
      G_Width
    | G_Height
    | G_DataType
    | G_Lower
    | G_Upper
    | G_Output
    | G_Help
    | G_Tool
    deriving (Enum,  Eq)


data DataType = G_Integer | G_Float
      deriving (Eq)

instance IsArgument DataType where
  fromOption s = case s of
    "integer" -> Just G_Integer
    "float" -> Just G_Float
    _ -> Nothing

type G_Tool = Either DataType Tool


instance IsArgument (Either DataType Tool) where
  fromOption s = case (fromOption s, fromOption s)::(Maybe DataType, Maybe Tool) of
    (Nothing, Just v) -> Just $ Right v
    (Just v, Nothing) -> Just $ Left v
    (Nothing, Nothing) -> Nothing
    (Just a, Just _) -> Just $ Left a

data DeterminantFlag = 
      D_Dimension
    | D_Input
    | D_Help
    | D_Tool
    deriving (Enum,  Eq)

data TestFlag = Test_Tool
                 deriving (Eq, Enum)


invertArguments :: [Argument InvertFlag Tool]
invertArguments = [
  Argument "--dimension" "-d" "The dimension, n, of the input nxn matrix" True (INT Nothing) I_Dimension,
  Argument "--input" "-i" "The name of the input file that contains the matrix to be inverted" True (STRING Nothing) I_Input,
  Argument "--output" "-o" "The name of a file, which the inverted matrix will be written to" False (STRING Nothing) I_Output,
  Argument "--parallel" "-p" "Control if program runs in parallel" False (PRESENT False) I_Parallel,
  Argument "--help" "-h" "Display help message" False (PRESENT False) I_Help,
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" False (OTHER $ Just Invert) I_Tool
  ]

transposeArguments :: [Argument TransposeFlag Tool]
transposeArguments = [
  Argument "--width" "-x" "The width, x, of the input x by y matrix" True (INT Nothing) T_Width,
  Argument "--height" "-y" "The height, y, of the input x by y matrix" True (INT Nothing) T_Height,
  Argument "--input" "-i" "The name of the input file that contains the matrix to be transposed" True (STRING Nothing) T_Input,
  Argument "--output" "-o" "The name of a file, which the transposed matrix will be written to" False (STRING Nothing) T_Output,
  Argument "--help" "-h" "Display help message" False (PRESENT False) T_Help,
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" True (OTHER $ Just  Invert) T_Tool
  ]

multiplyArguments :: [Argument MultiplyFlag Tool]
multiplyArguments = [
  Argument "--width" "-x" "The width, x, of the input x by y matrix" True (INT Nothing) M_Width,
  Argument "--height" "-y" "The height, y, of the input x by y matrix" True (INT Nothing) M_Height,
  Argument "--input-one" "-i1" "The name of the input file that contains the first matrix to be multiplied" True (STRING Nothing) M_Input_One,
  Argument "--input-two" "-i2" "The name of the input file that contains the second matrix to be multiplied" True (STRING Nothing) M_Input_Two,
  Argument "--output" "-o" "The name of a file, which the resulting matrix will be written to" False (STRING Nothing) M_Output,
  Argument "--parallel" "-p" "Determine if the multiplication should be done in parallel or not" False (PRESENT False) M_Parallel,
  Argument "--help" "-h" "Display help message" False (PRESENT False) M_Help,
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" True (OTHER $ Just Invert) M_Tool
  ]

generateArguments :: [Argument GenerateFlag G_Tool]
generateArguments = [
  Argument "--width" "-x" "The width, x, of the output x by y matrix. x must be an integer." True (INT Nothing) G_Width,
  Argument "--height" "-y" "The height, y, of the output x by y matrix. y must be an integer." True (INT Nothing) G_Height,
  Argument "--datatype" "-d" "Define if only integral or floating values should be returned" False (OTHER $ Just $ Left G_Integer) G_DataType,
  Argument "--lower" "-l" "The lowest value with which to fill the matrix" False (INT $ Just (-100)) G_Lower,
  Argument "--upper" "-u" "The greatest value with which to fill the matrix" False (INT $ Just 100) G_Upper,
  Argument "--output" "-o" "The name of a file which the matrix will be written to" False (STRING Nothing) G_Output,
  Argument "--help" "-h" "Display help message" False (PRESENT False) G_Help,
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" True (OTHER $ Just $ Right Invert) G_Tool
  ]

determinantArguments :: [Argument DeterminantFlag Tool]
determinantArguments = [
  Argument "--dimension" "-d" "The dimension, n, of the input nxn matrix" True (INT Nothing) D_Dimension,
  Argument "--input" "-i" "The name of the input file for which to find the determinant" True (STRING Nothing) D_Input,
  Argument "--help" "-h" "Display help message" False (PRESENT False) D_Help,
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" True (OTHER $ Just Invert) D_Tool
  ]

testArguments :: [Argument TestFlag Tool]
testArguments = [
  Argument "--tool" "-t" "choose which tool to utilise, invert, transpose, multiply, generate or determinant (default invert)" True (OTHER $ Just Invert) Test_Tool
  ]

invertTool :: [String] -> IO()
invertTool opts = do
  case parse invertArguments opts of
   Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
   Right [dim, input, output, para, hlp, _] -> do
     let inFile = fromMandatorySTRING $ value input
         dimension = fromMandatoryINT $ value dim
         help = fromMandatoryPRESENT $ value hlp
         parallel = fromMandatoryPRESENT $ value para
     if help then putStrLn "This'll learn ya!" else do
       matrixData <- readFile inFile >>= (return . map read . words) :: IO [CDouble]
       withMatrix dimension dimension matrixData $ \mat -> do
         case value output of
           STRING Nothing -> do
             inv <- if parallel then invert mat else stdInvert mat
             _ <- printm inv
             freeM inv
           STRING (Just outfile) -> do
             inv <- if parallel then invert mat else stdInvert mat
             writeFile outfile $ show inv
             freeM inv
           _ -> error "CommandLine error, argument specified INT was not returd as such"
   Right _ -> error "Unprecedented runtime error occurred, check CommandLine"



transposeTool :: [String] -> IO()
transposeTool opts = do
  case parse transposeArguments opts of
   Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
   Right [wid, hei, inp, out, hlp, _] -> do
     let inFile = fromMandatorySTRING $  value inp
         width = fromMandatoryINT $ value wid
         height = fromMandatoryINT $ value hei
         help = fromMandatoryPRESENT $ value hlp
     if help then putStrLn "This'll learn ya!" else do
       matrixData <- readFile inFile >>= (return . map read . words) :: IO [CDouble]
       withMatrix width height matrixData $ \mat -> do
         tran <- transpose mat
         case value out of
           STRING Nothing -> do
             _ <- printm tran
             freeM tran
           STRING (Just output) -> do
             writeFile output $ show tran
             freeM tran
           _ -> error "CommandLine error, argument specified INT was not returd as such"
   Right _ -> error "Unprecedented runtime error occurred, check CommandLine"

multiplyTool :: [String] -> IO()
multiplyTool opts = do
  case parse multiplyArguments opts of
    Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
    Right [wid, hei, inp1, inp2, out, para, hlp, _] -> do
      let width = fromMandatoryINT $ value wid
          height = fromMandatoryINT $ value hei
          input1 = fromMandatorySTRING $ value inp1
          input2 = fromMandatorySTRING $ value inp2
          parallel = fromMandatoryPRESENT $ value para
          help = fromMandatoryPRESENT $ value hlp
      if help then putStrLn "This'll learn ya!" else do
        matrixData1 <- readFile input1 >>= (return . map read . words) :: IO [CDouble]
        matrixData2 <- readFile input2 >>= (return . map read . words)
        withMatrix width height matrixData1 $ \mat1 ->
          withMatrix height width matrixData2 $ \mat2 -> do
            case value out of
              STRING Nothing -> do
                mult <- if parallel then multiply mat1 mat2 else stdMultiply mat1 mat2
                _ <- printm mult
                freeM mult
              STRING (Just output) -> do
                mult <- if parallel then multiply mat1 mat2 else stdMultiply mat1 mat2
                writeFile output $ show mult
                freeM mult
              _ -> error "CommandLine error, argument specified INT was not returd as such"
    Right _ -> error "Unprecedented runtime error occurred, check CommandLine"

generateTool :: [String] -> IO()
generateTool opts = do
  case parse generateArguments opts of
    Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
    Right [wid, hei, dt, low, up, out, hlp, _] -> do
      let width = fromMandatoryINT $ value wid
          height = fromMandatoryINT $ value hei
          lower = fromMandatoryINT $ value low
          upper = fromMandatoryINT $ value up
          help = fromMandatoryPRESENT $ value hlp
      if help then putStrLn "This'll learn ya!" else do
        case value dt of
          OTHER (Just (Left G_Integer)) -> do
            matrixData <- (mapM (const $ randomRIO (fromIntegral lower, fromIntegral upper)) [1..width*height]) :: IO([CInt])
            withMatrix width height matrixData $ \mat -> do
              case value out of
                STRING Nothing -> printm mat >> return ()
                STRING (Just file) -> writeFile file $ show mat
                _ -> error "CommandLine error, argument specified INT was not returd as such"
          OTHER (Just (Left G_Float)) -> do
             matrixData <- (mapM (const $ randomRIO (fromIntegral lower, fromIntegral upper)) [1..width*height]) :: IO([CFloat])
             withMatrix width height matrixData $ \mat -> do
               case value out of
                 STRING Nothing -> printm mat >> return ()
                 STRING (Just file) -> writeFile file $ show mat
                 _ -> error "CommandLine error, argument specified INT was not returd as such"
          OTHER (Just (Right t)) -> putStrLn $ "Cannot have tool as datatype: " ++ (show t)
          _ -> error "CommandLine error, argument specified OTHER was not returned as such"
    Right _ -> error "Unprecedented runtime error occurred, check CommandLine"


determinantTool :: [String] -> IO()
determinantTool opts = do
  case parse determinantArguments opts of
    Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
    Right [dim, inp, hlp, _] -> do
      let dimension = fromMandatoryINT $ value dim
          input = fromMandatorySTRING $ value inp
          PRESENT help = value hlp
      if help then putStrLn "This'll learn ya!" else do
        matrixData <- readFile input >>= (return . map read . words) :: IO [CDouble]
        withMatrix dimension dimension matrixData $ \mat -> do
          det <- determinant mat :: IO(Double)
          putStrLn $ show det
    Right _ -> error "Unprecedented runtime error occurred, check CommandLine"

testTool :: [String] -> IO()
testTool opts = do
  case parse testArguments opts of
    Left err -> putStrLn "Error occurred when parsing arguments" >> (putStrLn $ "\t" ++ show err)
    Right [tool] -> do
      let toolV = fromMandatoryOTHER $ value tool
      case toolV of
        Test -> do
          count <- test
          putStrLn $ "Passed " ++ (show count) ++ " out of 18 tests"
        _ -> error "Incorrect tool for use in test Tool"
    Right _ -> error "Unprecedented runtime error occurred, check CommandLine"

main :: IO()
main = do 
  opts <- getArgs
  let t = findTool Invert opts
  case t of
    Invert -> invertTool opts
    Transpose -> transposeTool opts
    Multiply -> multiplyTool opts
    Generate -> generateTool opts
    Determinant -> determinantTool opts
    Test -> testTool opts
    None -> return ()
