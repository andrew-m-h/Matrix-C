{-# LANGUAGE NoImplicitPrelude #-}
module HaskellMatrixError (
	createError,
        MatrixError(..),
        ErrorCode(..),
	) where

import Prelude (
  Show(..), Eq(..), Enum(..), String,
  fromIntegral, ($), (++), error)
import Foreign.C.Types (CInt)

data MatrixError = MatrixError {
  message :: String,
  eCode :: MatrixErrorCode,
  eType :: ErrorCode
  } deriving (Eq, Show)

type MatrixErrorCode = CInt

data ErrorCode =
  Success |
  Mem_Allocation_Failure |
  Math_Error |
  Dimension_Error |
  Buff_Size_Error |
  File_IO_Error |
  Failure |
  Argument_Error
  deriving (Eq)

instance Show ErrorCode where
  show code = case code of
    Success -> "Matrix Operation Success\n"
    Mem_Allocation_Failure -> "Failed to allocate sufficient memory to perform matrix operation\n"
    Math_Error -> "Math error occurred when performing matrix operation\n\tLikely Cause: Take inverse of matrix with determinant of 0\n"
    Dimension_Error -> "Dimension error occurred when performing matrix operation\n\tLikely Cause: Take inverse, cofactor or determinant of non square matrix\n"
    Buff_Size_Error -> "Buffer error occurred\n\tLikely Cause: String buffer not of sufficient length to represent matrix\n"
    File_IO_Error -> "Failed to correctly perform fileIO\n"
    Failure -> "General Matrix Failure occurred\n"
    Argument_Error -> "Incorrect arguments given to program\n"

instance Enum ErrorCode where
  fromEnum code = case code of
        Success -> 0
        Mem_Allocation_Failure -> 1
        Math_Error -> 2
        Dimension_Error -> 3
        Buff_Size_Error -> 4
        File_IO_Error -> 5
        Failure -> 6
        Argument_Error -> 7
        
  toEnum code = case code of
        0 -> Success
        1 -> Mem_Allocation_Failure
        2 -> Math_Error
        3 -> Dimension_Error
        4 -> Buff_Size_Error
        5 -> File_IO_Error
        6 -> Failure
        7 -> Argument_Error
        _ -> error $ "Invalid Error Code: " ++ (show code)

createError :: MatrixErrorCode -> MatrixError
createError c = MatrixError (show errCode) c errCode
  where
    errCode = toEnum $ fromIntegral c
