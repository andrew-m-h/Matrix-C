{-# LANGUAGE NoImplicitPrelude, ConstrainedClassMethods, MultiParamTypeClasses #-}

module Matrix.Matrix (
  Matrix(..),

  Vector,
  mkVector,
  vData, vHeight,

  ListMatrix(..),

  CInt,
  CFloat,
  CDouble,

  unCInt,
  unCFloat,
  unCDouble,

  toMatrix,
  toVector,
  toListMatrix,

  toCIntMatrix,
  toCFloatMatrix,
  toCDoubleMatrix,

  toCIntVector,
  toCFloatVector,
  toCDoubleVector,

  transpose,
  invert,
  stdInvert,
  printm,
  cofactor,
  stdCofactor,
  paraCofactor,
  multiply,
  stdMultiply,
  crossProduct,
  determinant,
  dotProduct,

  matrixNull,
  matrix,
  vectorNull,
  vector,

  matrixCpy,
  vectorCpy,
  freeM,
  freeV,

  withMatrix,
  withMatrixNull,

  withVector,
  withVectorNull,
  
  test
              ) where

import Prelude (Integral(..), Monad(..), IO,
  Double, Float, Int)
import Matrix.Types (
  Matrix(..), FromMatrix(..), MatrixType(..),
  Vector'(..), Vector, mkVector,
  ListMatrix(..),
  unCInt, unCFloat, unCDouble,)
import Matrix.MatrixForeign (
  matrix, matrixNull, vector, vectorNull, 
  freeM, freeV, matrixCpy, vectorCpy,
  transposeD, transposeF, transposeI,
  invertD, invertF, invertI,
  stdInvertD, stdInvertF, stdInvertI,
  printmD, printmF, printmI,
  cofactorD, cofactorF, cofactorI,
  stdCofactorD, stdCofactorF, stdCofactorI,
  paraCofactorD, paraCofactorF, paraCofactorI,
  multiplyD, multiplyF, multiplyI,
  stdMultiplyD, stdMultiplyF, stdMultiplyI,
  crossProductD, crossProductF, crossProductI,
  determinantD, determinantF, determinantI,
  dotProductD, dotProductF, dotProductI,
  test
  )
import Matrix.HaskellMatrixError (MatrixError(..))
import Foreign.C.Types(
  CInt, CFloat, CDouble)
import Foreign(Storable(..))

class MatrixFunction t where
  transpose :: (Integral b) => Matrix t b  -> IO(Matrix t b)
  invert :: (Integral b) => Matrix t b -> IO(Matrix CDouble b)
  stdInvert :: (Integral b) => Matrix t b -> IO(Matrix CDouble b)
  printm :: (Integral b) => Matrix t b -> IO(MatrixError)
  cofactor :: (Integral b) => Matrix t b -> IO (Matrix t b)
  stdCofactor :: (Integral b) => Matrix t b -> IO (Matrix t b)
  paraCofactor :: (Integral b) => Matrix t b -> IO (Matrix t b)
  multiply :: (Integral b) => Matrix t b -> Matrix t b -> IO(Matrix t b)
  stdMultiply :: (Integral b) => Matrix t b -> Matrix t b -> IO(Matrix t b)
  crossProduct :: (Integral b) => Vector t b -> Vector t b -> IO(Vector t b)

class MatrixFunctionReturn t r where
  dotProduct :: (Integral b) => Vector t b -> Vector t b -> IO r
  determinant :: (Integral b) => Matrix t b -> IO r

instance MatrixFunction CDouble where
  transpose = transposeD
  invert = invertD
  stdInvert = stdInvertD
  printm = printmD 
  cofactor = cofactorD
  stdCofactor = stdCofactorD
  paraCofactor = paraCofactorD
  multiply = multiplyD
  stdMultiply = stdMultiplyD
  crossProduct = crossProductD

instance MatrixFunctionReturn CDouble Double where
  dotProduct = dotProductD
  determinant = determinantD

instance MatrixFunction CFloat where
  transpose = transposeF
  invert = invertF
  stdInvert = stdInvertF
  printm = printmF
  cofactor = cofactorF
  stdCofactor = stdCofactorF
  paraCofactor = paraCofactorF
  multiply = multiplyF
  stdMultiply = stdMultiplyF
  crossProduct = crossProductF

instance MatrixFunctionReturn CFloat Float where
  dotProduct = dotProductF
  determinant = determinantF

instance MatrixFunction CInt where
  transpose = transposeI
  invert = invertI
  stdInvert = stdInvertI
  printm = printmI
  cofactor = cofactorI
  stdCofactor = stdCofactorI
  paraCofactor = paraCofactorI
  multiply = multiplyI
  stdMultiply = stdMultiplyI
  crossProduct = crossProductI

instance MatrixFunctionReturn CInt Int where
  dotProduct = dotProductI
  determinant = determinantI



withMatrix :: (Storable a, Integral b) => b -> b -> [a] -> (Matrix a b -> IO c) -> IO c
withMatrix  w h d f = do
  mat <- matrix w h d
  ret <- f mat
  freeM mat
  return ret

withMatrixNull :: (Storable a, Integral b) => b -> b -> (Matrix a b -> IO c) -> IO c
withMatrixNull w h f = do
  mat <- matrixNull w h
  ret <- f mat 
  freeM mat
  return ret

withVector :: (Storable a, Integral b) => b -> [a] -> (Vector a b -> IO c) -> IO c
withVector h arr f = do
  vec <- vector h arr
  ret <- f vec 
  freeV vec 
  return ret

withVectorNull :: (Storable a, Integral b) => b -> (Vector a b -> IO c) -> IO c
withVectorNull h f = do
  vec <- vectorNull h 
  ret <- f vec
  freeV vec 
  return ret
