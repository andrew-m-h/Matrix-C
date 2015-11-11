{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude, DeriveGeneric #-}

module Matrix (
  C_Matrix(..),
  Matrix(..),
  
  CDouble,
  CFloat,
  CInt,

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
              ) where

import Prelude (
  Show(..), Eq(..), Num(..), Enum(..), Bool(..), String, IO, Double, Float,
  fromIntegral, otherwise, error, putStrLn, notElem, take,
  replicate, length, not, takeWhile,
  (*), (++), ($), (.), (>), (&&), )
import HaskellMatrixError (
  createError, MatrixError(..), ErrorCode(..))
import Data.Int (Int32, Int)
import Foreign (
  Storable(..), Ptr,
  peekArray, pokeArray, mallocArray,
  free, alloca, copyArray)
import Foreign.CStorable (CStorable(..))
import Foreign.C.Types (CInt, CDouble, CFloat)
import GHC.Generics (Generic(..))
import Control.Monad (Monad(..), when)
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



toCMatrix :: Matrix a -> C_Matrix a
toCMatrix (Matrix p w h) = C_Matrix p (fromIntegral w) (fromIntegral h)

toMatrix :: C_Matrix a -> Matrix a
toMatrix (C_Matrix p w h) = Matrix p (fromIntegral w) (fromIntegral h)

c_matrixNull :: (Storable a, Num a) => CInt -> CInt -> IO(C_Matrix a)
c_matrixNull w h = do
  p <- mallocArray (fromIntegral $ w * h)
  return $ C_Matrix p w h

matrixNull :: (Storable a, Num a) => Int -> Int -> IO(Matrix a)
matrixNull w h = do
  p <- mallocArray (w * h)
  return $ Matrix p w h

c_matrix :: (Storable a, Num a) => CInt -> CInt -> [a] -> IO(C_Matrix a)
c_matrix  w h d = do
  p <- mallocArray (fromIntegral $ w * h)
  pokeArray p d
  return $ C_Matrix p w h

matrix :: (Storable a, Num a) =>  Int -> Int -> [a] -> IO(Matrix a)
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

transposeD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
transposeD srcM =
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    c_transposeD dest
    peek dest

transposeF :: C_Matrix CFloat -> IO(C_Matrix CFloat)
transposeF srcM =
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    c_transposeF dest
    peek dest

transposeI :: C_Matrix CInt -> IO(C_Matrix CInt)
transposeI srcM =
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    c_transposeI dest
    peek dest

invertD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
invertD srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)
      
      poke dest destM
      poke src srcM

      err <- c_invertD dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

invertF :: C_Matrix CFloat -> IO(C_Matrix CDouble)
invertF srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)
      
      poke dest destM
      poke src srcM

      err <- c_invertF dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

invertI :: C_Matrix CInt -> IO(C_Matrix CDouble)
invertI srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)
      
      poke dest destM
      poke src srcM

      err <- c_invertI dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

printmD :: C_Matrix CDouble -> IO(MatrixError)
printmD mat = do
  alloca $ \p -> do
    poke p mat
    code <- c_printmD p
    return $ createError $ fromIntegral code

printmF :: C_Matrix CFloat -> IO(MatrixError)
printmF mat = do
  alloca $ \p -> do
    poke p mat
    code <- c_printmF p
    return $ createError $ fromIntegral code

printmI :: C_Matrix CInt -> IO(MatrixError)
printmI mat = do
  alloca $ \p -> do
    poke p mat
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