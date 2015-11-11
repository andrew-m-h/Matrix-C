{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude, DeriveGeneric #-}

module Matrix (
  C_Matrix(..),
  Matrix(..),
  
  CDouble,
  CFloat,
  CInt,

  unCInt,
  unCFloat,
  unCDouble,

  transposeD,
  transposeF,
  transposeI,
  
  invertD,
  invertF,
  invertI,

  printmD,
  printmF,
  printmI,

  determinantD,
  determinantF,
  determinantI,

  cofactorD,
  cofactorF,
  cofactorI,

  toCMatrix,
  toMatrix,

  c_matrixNull,
  matrixNull,

  c_matrixCpy,
  matrixCpy,

  c_freeM,
  freeM,

  c_matrix,
  matrix,

  withCMatrix,
  withMatrix,

  withCMatrixNull,
  withMatrixNull
              ) where

import Prelude (
  Show(..), Eq(..), Num(..), Enum(..), Bool(..), String, IO, Double, Float,
  otherwise, error, putStrLn, notElem, take,
  replicate, length, not, takeWhile, fromIntegral,
  (*), (++), ($), (.), (>), (&&))
import HaskellMatrixError (
  createError, MatrixError(..), ErrorCode(..))
import Data.Int (Int32, Int)
import Foreign (
  Storable(..), Ptr,
  peekArray, pokeArray, mallocArray,
  free, alloca, copyArray)
import Foreign.CStorable (CStorable(..))
import Foreign.C.Types (CInt(..), CDouble(..), CFloat(..))
import GHC.Generics (Generic(..))
import Control.Monad (Monad(..), when, liftM)
import System.IO.Unsafe (unsafePerformIO)

data C_Matrix a = C_Matrix {c_mData :: Ptr a, c_width, c_height :: CInt}
                deriving (Generic, Eq)

instance (CStorable a) => CStorable (C_Matrix a)

instance (CStorable a, Storable a) => Storable (C_Matrix a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance (Show a, Storable a) => Show (C_Matrix a) where
  show (C_Matrix p wid hei) = prettyPrint 1 (unsafePerformIO (peekArray (w*h) p))
    where
      w = fromIntegral wid
      h = fromIntegral hei
      prettyPrint :: (Show a) => Int -> [a] -> String
      prettyPrint n lst = case lst of
        x:xs
          | n == w -> (padRight 7 $ round 6 $ show x) ++ "\n" ++ prettyPrint 1 xs
          | otherwise -> (padRight 7 $ round 6 $ show x) ++ "    " ++ prettyPrint (succ n) xs
        [] -> ""


data Matrix a = Matrix {mData :: Ptr a, width, height :: Int}
                deriving (Eq)

instance (Show a, Storable a) => Show (Matrix a) where
  show (Matrix p w h) = prettyPrint 1 (unsafePerformIO (peekArray (w*h) p))
    where
      prettyPrint :: (Show a) => Int -> [a] -> String
      prettyPrint n lst = case lst of
        x:xs
          | n == w -> (padRight 7 $ round 6 $ show x) ++ "\n" ++ prettyPrint 1 xs
          | otherwise -> (padRight 7 $ round 6 $ show x) ++ "    " ++ prettyPrint (succ n) xs
        [] -> ""

round :: Int -> String -> String
round l s
  | '.' `notElem` s = s
  | otherwise = round' False l s
  where
    round' :: Bool -> Int -> String -> String
    round' dot len str = case str of
      "" ->  ""
      "."-> ""
      '.':xs
        | len == 0 -> ""
        | otherwise -> '.':(round' True (pred len) xs)
      '-':xs -> '-':(round' dot len xs)
      x:xs
        | dot && len == 0 -> ""
        | not dot && len == 0 -> takeWhile (/='.') str
        | otherwise -> x:(round' dot (pred len) xs)

padRight :: Int -> String -> String
padRight len str
  | s_len > len = str
  | otherwise = str ++ (replicate (len - s_len) ' ')
  where 
    s_len = length str

foreign import ccall unsafe "HaskellMatrix.h hs_transposeD" c_transposeD :: Ptr (C_Matrix CDouble) -> IO ()
foreign import ccall unsafe "HaskellMatrix.h hs_transposeF" c_transposeF :: Ptr (C_Matrix CFloat) -> IO ()
foreign import ccall unsafe "HaskellMatrix.h hs_transposeI" c_transposeI :: Ptr (C_Matrix CInt) -> IO ()

foreign import ccall "HaskellMatrix.h hs_invertD" c_invertD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_invertF" c_invertF :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CFloat) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_invertI" c_invertI :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CInt) -> IO Int32

foreign import ccall "HaskellMatrix.h hs_printmD" c_printmD :: Ptr (C_Matrix CDouble) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_printmF" c_printmF :: Ptr (C_Matrix CFloat) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_printmI" c_printmI :: Ptr (C_Matrix CInt) -> IO Int32

foreign import ccall "HaskellMatrix.h hs_determinantD" c_determinantD :: Ptr (C_Matrix CDouble) -> IO Double
foreign import ccall "HaskellMatrix.h hs_determinantF" c_determinantF :: Ptr (C_Matrix CFloat) -> IO Float
foreign import ccall "HaskellMatrix.h hs_determinantI" c_determinantI :: Ptr (C_Matrix CInt) -> IO Int32

foreign import ccall "HaskellMatrix.h hs_cofactorD" c_cofactorD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_cofactorF" c_cofactorF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int32
foreign import ccall "HaskellMatrix.h hs_cofactorI" c_cofactorI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int32


unCFloat :: CFloat -> Float
unCFloat (CFloat a) = a

unCDouble :: CDouble -> Double
unCDouble (CDouble a) = a

unCInt :: CInt -> Int 
unCInt (CInt a) = fromIntegral a


toCMatrix :: Matrix a -> C_Matrix a
toCMatrix (Matrix p w h) = C_Matrix p (fromIntegral w) (fromIntegral h)

toMatrix :: C_Matrix a -> Matrix a
toMatrix (C_Matrix p w h) = Matrix p (fromIntegral w) (fromIntegral h)

c_matrixNull :: (Storable a) => CInt -> CInt -> IO(C_Matrix a)
c_matrixNull w h = do
  p <- mallocArray (fromIntegral $ w * h)
  return $ C_Matrix p w h

matrixNull :: (Storable a) => Int -> Int -> IO(Matrix a)
matrixNull w h = do
  p <- mallocArray (w * h)
  return $ Matrix p w h

c_matrix :: (Storable a) => CInt -> CInt -> [a] -> IO(C_Matrix a)
c_matrix  w h d = do
  p <- mallocArray (fromIntegral $ w * h)
  pokeArray p d
  return $ C_Matrix p w h

matrix :: (Storable a) =>  Int -> Int -> [a] -> IO(Matrix a)
matrix  w h d = do
  p <- mallocArray (w * h)
  pokeArray p d
  return $ Matrix p w h

matrixCpy :: (Storable a) => Matrix a -> IO(Matrix a)
matrixCpy (Matrix p w h) = do
  let size = w * h
  newPtr <- mallocArray size
  copyArray newPtr p size
  return $ Matrix newPtr w h

c_matrixCpy :: (Storable a) => C_Matrix a -> IO(C_Matrix a)
c_matrixCpy (C_Matrix p w h) = do
  let size = fromIntegral $ w * h
  newPtr <- mallocArray size 
  copyArray newPtr p size
  return $ C_Matrix newPtr w h

c_freeM :: C_Matrix a -> IO()
c_freeM = free . c_mData

freeM :: Matrix a -> IO()
freeM = free . mData

transposeD :: Matrix CDouble -> IO(Matrix CDouble)
transposeD srcM =
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest $ toCMatrix destM
    c_transposeD dest
    liftM toMatrix (peek dest)

transposeF :: Matrix CFloat -> IO(Matrix CFloat)
transposeF srcM =
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest $ toCMatrix destM
    c_transposeF dest
    liftM toMatrix (peek dest)

transposeI :: Matrix CInt -> IO(Matrix CInt)
transposeI srcM =
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest $ toCMatrix destM
    c_transposeI dest
    liftM toMatrix (peek dest)

invertD :: Matrix CDouble -> IO(Matrix CDouble)
invertD srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)
      
      poke dest $ toCMatrix destM
      poke src $ toCMatrix srcM

      err <- c_invertD dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      liftM toMatrix (peek dest)

invertF :: Matrix CFloat -> IO(Matrix CDouble)
invertF srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)
      
      poke dest $ toCMatrix destM
      poke src $ toCMatrix srcM

      err <- c_invertF dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      liftM toMatrix (peek dest)

invertI :: Matrix CInt -> IO(Matrix CDouble)
invertI srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)
      
      poke dest $ toCMatrix destM
      poke src $ toCMatrix srcM

      err <- c_invertI dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      liftM toMatrix (peek dest)

printmD :: Matrix CDouble -> IO(MatrixError)
printmD mat = do
  alloca $ \p -> do
    poke p $ toCMatrix mat
    code <- c_printmD p
    return $ createError $ fromIntegral code

printmF :: Matrix CFloat -> IO(MatrixError)
printmF mat = do
  alloca $ \p -> do
    poke p $ toCMatrix mat
    code <- c_printmF p
    return $ createError $ fromIntegral code

printmI :: Matrix CInt -> IO(MatrixError)
printmI mat = do
  alloca $ \p -> do
    poke p $ toCMatrix mat
    code <- c_printmI p
    return $ createError $ fromIntegral code

determinantD :: C_Matrix CDouble -> IO(Double)
determinantD mat = 
  alloca $ \p -> do
    poke p mat
    c_determinantD p

determinantF :: C_Matrix CFloat -> IO(Float)
determinantF mat = 
  alloca $ \p -> do
    poke p mat
    c_determinantF p

determinantI :: C_Matrix CInt -> IO(Int32)
determinantI mat = 
  alloca $ \p -> do
    poke p mat
    c_determinantI p

cofactorD :: C_Matrix CDouble -> IO (C_Matrix CDouble)
cofactorD src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- c_cofactorD destPtr srcPtr >>= (return . createError . fromIntegral)

      when (eType err /= Success) (error $ message err)
      peek destPtr

cofactorF :: C_Matrix CFloat -> IO (C_Matrix CFloat)
cofactorF src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- c_cofactorF destPtr srcPtr >>= (return . createError . fromIntegral)

      when (eType err /= Success) (error $ message err)
      peek destPtr

cofactorI :: C_Matrix CInt -> IO (C_Matrix CInt)
cofactorI src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- c_cofactorI destPtr srcPtr >>= (return . createError . fromIntegral)

      when (eType err /= Success) (error $ message err)
      peek destPtr

withCMatrix :: (Storable a) => CInt -> CInt -> [a] -> (C_Matrix a -> IO b) -> IO b
withCMatrix w h d f = do
  mat <- c_matrix w h d
  ret <- f mat
  c_freeM mat
  return ret

withMatrix :: (Storable a) => Int -> Int -> [a] -> (Matrix a -> IO b) -> IO b
withMatrix  w h d f = do
  mat <- matrix w h d
  ret <- f mat
  freeM mat
  return ret

withCMatrixNull :: (Storable a) => CInt -> CInt -> (C_Matrix a -> IO b) -> IO b
withCMatrixNull w h f = do
  mat <- c_matrixNull w h 
  ret <- f mat
  c_freeM mat
  return ret

withMatrixNull :: (Storable a) => Int -> Int -> (Matrix a -> IO b) -> IO b
withMatrixNull w h f = do
  mat <- matrixNull w h
  ret <- f mat 
  freeM mat
  return ret