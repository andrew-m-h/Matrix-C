{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude #-}

module Matrix.MatrixForeign (
  matrixNull,
  matrix,
  vectorNull,
  vector,

  matrixCpy,
  vectorCpy,
  freeM,
  freeV,

  transposeD,
  transposeF,
  transposeI,

  invertD,
  invertF,
  invertI,

  stdInvertD,
  stdInvertF,
  stdInvertI,

  printmD,
  printmF,
  printmI,

  determinantD,
  determinantF,
  determinantI,

  cofactorD,
  cofactorF,
  cofactorI,

  stdCofactorD,
  stdCofactorF,
  stdCofactorI,

  paraCofactorD,
  paraCofactorF,
  paraCofactorI,

  multiplyD,
  multiplyF,
  multiplyI,

  stdMultiplyD,
  stdMultiplyF,
  stdMultiplyI,

  dotProductD,
  dotProductF,
  dotProductI,

  crossProductD,
  crossProductF,
  crossProductI,
  test,
	) where

import Prelude (
  Show(..), Eq(..), Num(..), Enum(..), Bool(..), Functor(..), Integral(..),
  String, IO, Double, Float, Maybe(..),
  otherwise, error, notElem, take, map, id, round,
  replicate, length, not, takeWhile, fromIntegral, undefined,
  (*), (++), ($), (.), (>), (&&))
import Matrix.Types (
  Matrix(..), C_Matrix,
  Vector'(..), Vector, C_Vector,
  fromCMatrix, toCMatrix, fromCVector, toCVector
  )
import Matrix.HaskellMatrixError (
  createError, MatrixError(..), ErrorCode(..))
import Data.Int (Int)
import Foreign (
  Storable(..), Ptr,
  pokeArray, mallocArray,
  free, alloca, copyArray)
import Foreign.C.Types (CInt(..), CDouble(..), CFloat(..))
import Control.Monad (Monad(..), when, liftM)

foreign import ccall "HaskellMatrix.h hs_transposeD" hs_transposeD :: Ptr (C_Matrix CDouble) -> IO ()
foreign import ccall "HaskellMatrix.h hs_transposeF" hs_transposeF :: Ptr (C_Matrix CFloat) -> IO ()
foreign import ccall "HaskellMatrix.h hs_transposeI" hs_transposeI :: Ptr (C_Matrix CInt) -> IO ()

foreign import ccall "HaskellMatrix.h hs_invertD" hs_invertD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_invertF" hs_invertF :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_invertI" hs_invertI :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_stdInvertD" hs_stdInvertD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdInvertF" hs_stdInvertF :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdInvertI" hs_stdInvertI :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_printmD" hs_printmD :: Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_printmF" hs_printmF :: Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_printmI" hs_printmI :: Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_determinantD" hs_determinantD :: Ptr (C_Matrix CDouble) -> IO Double
foreign import ccall "HaskellMatrix.h hs_determinantF" hs_determinantF :: Ptr (C_Matrix CFloat) -> IO Float
foreign import ccall "HaskellMatrix.h hs_determinantI" hs_determinantI :: Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_cofactorD" hs_cofactorD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_cofactorF" hs_cofactorF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_cofactorI" hs_cofactorI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_paraCofactorD" hs_paraCofactorD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_paraCofactorF" hs_paraCofactorF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_paraCofactorI" hs_paraCofactorI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_stdCofactorD" hs_stdCofactorD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdCofactorF" hs_stdCofactorF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdCofactorI" hs_stdCofactorI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_multiplyD" hs_multiplyD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_multiplyF" hs_multiplyF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_multiplyI" hs_multiplyI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_dotProductD" hs_dotProductD :: Ptr (C_Vector CDouble) -> Ptr (C_Vector CDouble) -> IO Double
foreign import ccall "HaskellMatrix.h hs_dotProductF" hs_dotProductF :: Ptr (C_Vector CFloat) -> Ptr (C_Vector CFloat) -> IO Float
foreign import ccall "HaskellMatrix.h hs_dotProductI" hs_dotProductI :: Ptr (C_Vector CInt) -> Ptr (C_Vector CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_crossProductD" hs_crossProductD :: Ptr (C_Vector CDouble) -> Ptr (C_Vector CDouble) -> Ptr (C_Vector CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_crossProductF" hs_crossProductF :: Ptr (C_Vector CFloat) -> Ptr (C_Vector CFloat) -> Ptr (C_Vector CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_crossProductI" hs_crossProductI :: Ptr (C_Vector CInt) -> Ptr (C_Vector CInt) -> Ptr (C_Vector CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_stdMultiplyD" hs_stdMultiplyD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdMultiplyF" hs_stdMultiplyF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_stdMultiplyI" hs_stdMultiplyI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_matrix_suite" test :: IO(Int)



matrixNull :: (Storable a, Integral b) => b -> b -> IO(Matrix a b)
matrixNull w h = do
  p <- mallocArray (fromIntegral $ w * h)
  return $ Matrix p w h

vectorNull :: (Storable a, Num b, Integral b) => b -> IO(Vector a b)
vectorNull h = do
  p <- mallocArray $ fromIntegral h 
  return $ Vector' p 1 h

matrix :: (Storable a, Integral b) =>  b -> b -> [a] -> IO(Matrix a b)
matrix  w h d = do
  let size = fromIntegral $ w * h
  p <- mallocArray size
  pokeArray p (take size d)
  return $ Matrix p w h

vector :: (Storable a, Num b, Integral b) => b -> [a] -> IO(Vector a b)
vector h arr = do
  let size = fromIntegral h
  p <- mallocArray size
  pokeArray p (take size arr)
  return $ Vector' p 1 h

matrixCpy :: (Storable a, Integral b) => Matrix a b -> IO(Matrix a b)
matrixCpy (Matrix p w h) = do
  let size = fromIntegral $ w * h
  newPtr <- mallocArray size
  copyArray newPtr p size
  return $ Matrix newPtr w h

vectorCpy :: (Storable a, Integral b) => Vector a b -> IO(Vector a b)
vectorCpy (Vector' p _ h) = do
  let size = fromIntegral h
  newPtr <- mallocArray size
  copyArray newPtr p size
  return $ Vector' newPtr 1 h

freeM :: Matrix a b -> IO()
freeM = free . mData

freeV :: Vector a b -> IO()
freeV = free . vData

c_transposeD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_transposeD srcM = 
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest destM

    hs_transposeD dest

    peek dest

c_transposeF :: C_Matrix CFloat -> IO(C_Matrix CFloat)
c_transposeF srcM = 
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest destM
    hs_transposeF dest
    peek dest

c_transposeI :: C_Matrix CInt -> IO(C_Matrix CInt)
c_transposeI srcM = 
  alloca $ \dest -> do
    destM <- matrixCpy srcM
    poke dest destM
    hs_transposeI dest
    peek dest

transposeD :: (Integral b) => Matrix CDouble b  -> IO(Matrix CDouble b)
transposeD srcM = liftM fromCMatrix (c_transposeD $ toCMatrix srcM)

transposeF :: (Integral b) => Matrix CFloat b -> IO(Matrix CFloat b)
transposeF srcM = liftM fromCMatrix (c_transposeF $ toCMatrix srcM)

transposeI :: (Integral b) => Matrix CInt b -> IO(Matrix CInt b)
transposeI srcM = liftM fromCMatrix (c_transposeI $ toCMatrix srcM)


c_invertD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_invertD srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertD dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_invertF :: C_Matrix CFloat -> IO(C_Matrix CDouble)
c_invertF srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertF dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_invertI :: C_Matrix CInt -> IO(C_Matrix CDouble)
c_invertI srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertI dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

invertD :: (Integral b) => Matrix CDouble b -> IO(Matrix CDouble b)
invertD srcM = liftM fromCMatrix (c_invertD $ toCMatrix srcM)

invertF :: (Integral b) => Matrix CFloat b -> IO(Matrix CDouble b)
invertF srcM = liftM fromCMatrix (c_invertF $ toCMatrix srcM)

invertI :: (Integral b) => Matrix CInt b -> IO(Matrix CDouble b)
invertI srcM = liftM fromCMatrix (c_invertI $ toCMatrix srcM)


c_stdInvertD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_stdInvertD srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_stdInvertD dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_stdInvertF :: C_Matrix CFloat -> IO(C_Matrix CDouble)
c_stdInvertF srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_stdInvertF dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_stdInvertI :: C_Matrix CInt -> IO(C_Matrix CDouble)
c_stdInvertI srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- matrixNull (width srcM) (height srcM)

      poke dest destM
      poke src srcM

      err <- hs_stdInvertI dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

stdInvertD :: (Integral b) => Matrix CDouble b -> IO(Matrix CDouble b)
stdInvertD srcM = liftM fromCMatrix (c_stdInvertD $ toCMatrix srcM)

stdInvertF :: (Integral b) => Matrix CFloat b -> IO(Matrix CDouble b)
stdInvertF srcM = liftM fromCMatrix (c_stdInvertF $ toCMatrix srcM)

stdInvertI :: (Integral b) => Matrix CInt b -> IO(Matrix CDouble b)
stdInvertI srcM = liftM fromCMatrix (c_stdInvertI $ toCMatrix srcM)

c_printmD :: C_Matrix CDouble -> IO(MatrixError)
c_printmD mat = do
  alloca $ \p -> do
    poke p mat
    code <- hs_printmD p
    return $ createError $ fromIntegral code

c_printmF :: C_Matrix CFloat -> IO(MatrixError)
c_printmF mat = do
  alloca $ \p -> do
    poke p mat
    code <- hs_printmF p
    return $ createError $ fromIntegral code

c_printmI :: C_Matrix CInt -> IO(MatrixError)
c_printmI mat = do
  alloca $ \p -> do
    poke p mat
    code <- hs_printmI p
    return $ createError $ fromIntegral code

printmD :: (Integral b) => Matrix CDouble b -> IO(MatrixError)
printmD mat = c_printmD $ toCMatrix mat

printmF :: (Integral b) => Matrix CFloat b -> IO(MatrixError)
printmF mat = c_printmF $ toCMatrix mat

printmI :: (Integral b) => Matrix CInt b -> IO(MatrixError)
printmI mat = c_printmI $ toCMatrix mat

c_determinantD :: C_Matrix CDouble -> IO(Double)
c_determinantD mat = 
  alloca $ \p -> do
    poke p mat
    hs_determinantD p

c_determinantF :: C_Matrix CFloat -> IO(Float)
c_determinantF mat = 
  alloca $ \p -> do
    poke p mat
    hs_determinantF p

c_determinantI :: C_Matrix CInt -> IO(Int)
c_determinantI mat = 
  alloca $ \p -> do
    poke p mat
    hs_determinantI p

determinantD :: (Integral b) => Matrix CDouble b -> IO(Double)
determinantD mat = c_determinantD $ toCMatrix mat

determinantF :: (Integral b) => Matrix CFloat b-> IO(Float)
determinantF mat = c_determinantF $ toCMatrix mat

determinantI :: (Integral b) => Matrix CInt b -> IO(Int)
determinantI mat = c_determinantI $ toCMatrix mat

c_cofactorD :: C_Matrix CDouble -> IO (C_Matrix CDouble)
c_cofactorD src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorD destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_cofactorF :: C_Matrix CFloat -> IO (C_Matrix CFloat)
c_cofactorF src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorF destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_cofactorI :: C_Matrix CInt -> IO (C_Matrix CInt)
c_cofactorI src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorI destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

cofactorD :: (Integral b) => Matrix CDouble b -> IO (Matrix CDouble b)
cofactorD src = liftM fromCMatrix (c_cofactorD $ toCMatrix src)

cofactorF :: (Integral b) => Matrix CFloat b -> IO (Matrix CFloat b)
cofactorF src = liftM fromCMatrix (c_cofactorF $ toCMatrix src)

cofactorI :: (Integral b) => Matrix CInt b -> IO (Matrix CInt b)
cofactorI src = liftM fromCMatrix (c_cofactorI $ toCMatrix src)

c_multiplyD :: C_Matrix CDouble -> C_Matrix CDouble -> IO(C_Matrix CDouble)
c_multiplyD aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_multiplyD dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_multiplyF :: C_Matrix CFloat -> C_Matrix CFloat -> IO(C_Matrix CFloat)
c_multiplyF aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_multiplyF dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_multiplyI :: C_Matrix CInt -> C_Matrix CInt -> IO(C_Matrix CInt)
c_multiplyI aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_multiplyI dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

multiplyD :: (Integral b) => Matrix CDouble b -> Matrix CDouble b -> IO(Matrix CDouble b)
multiplyD aM bM = liftM fromCMatrix (c_multiplyD (toCMatrix aM) (toCMatrix bM))

multiplyF :: (Integral b) => Matrix CFloat b -> Matrix CFloat b -> IO(Matrix CFloat b)
multiplyF aM bM = liftM fromCMatrix (c_multiplyF (toCMatrix aM) (toCMatrix bM))

multiplyI :: (Integral b) => Matrix CInt b -> Matrix CInt b -> IO(Matrix CInt b)
multiplyI aM bM = liftM fromCMatrix (c_multiplyI (toCMatrix aM) (toCMatrix bM))

c_stdMultiplyD :: C_Matrix CDouble -> C_Matrix CDouble -> IO(C_Matrix CDouble)
c_stdMultiplyD aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_stdMultiplyD dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_stdMultiplyF :: C_Matrix CFloat -> C_Matrix CFloat -> IO(C_Matrix CFloat)
c_stdMultiplyF aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_stdMultiplyF dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_stdMultiplyI :: C_Matrix CInt -> C_Matrix CInt -> IO(C_Matrix CInt)
c_stdMultiplyI aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- matrixNull (height aM) (width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_stdMultiplyI dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

stdMultiplyD :: (Integral b) => Matrix CDouble b -> Matrix CDouble b -> IO(Matrix CDouble b)
stdMultiplyD aM bM = liftM fromCMatrix (c_stdMultiplyD (toCMatrix aM) (toCMatrix bM))

stdMultiplyF :: (Integral b) => Matrix CFloat b -> Matrix CFloat b -> IO(Matrix CFloat b)
stdMultiplyF aM bM = liftM fromCMatrix (c_stdMultiplyF (toCMatrix aM) (toCMatrix bM))

stdMultiplyI :: (Integral b) => Matrix CInt b -> Matrix CInt b -> IO(Matrix CInt b)
stdMultiplyI aM bM = liftM fromCMatrix (c_stdMultiplyI (toCMatrix aM) (toCMatrix bM))

c_stdCofactorD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_stdCofactorD src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_stdCofactorD destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_stdCofactorF :: C_Matrix CFloat -> IO(C_Matrix CFloat)
c_stdCofactorF src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_stdCofactorF destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_stdCofactorI :: C_Matrix CInt -> IO(C_Matrix CInt)
c_stdCofactorI src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_stdCofactorI destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

stdCofactorD :: (Integral b) => Matrix CDouble b -> IO (Matrix CDouble b)
stdCofactorD src = liftM fromCMatrix (c_stdCofactorD $ toCMatrix src)

stdCofactorF :: (Integral b) => Matrix CFloat b -> IO (Matrix CFloat b)
stdCofactorF src = liftM fromCMatrix (c_stdCofactorF $ toCMatrix src)

stdCofactorI :: (Integral b) => Matrix CInt b -> IO (Matrix CInt b)
stdCofactorI src = liftM fromCMatrix (c_stdCofactorI $ toCMatrix src)

c_paraCofactorD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_paraCofactorD src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_paraCofactorD destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_paraCofactorF :: C_Matrix CFloat -> IO(C_Matrix CFloat)
c_paraCofactorF src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_paraCofactorF destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_paraCofactorI :: C_Matrix CInt -> IO(C_Matrix CInt)
c_paraCofactorI src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- matrixNull (width src) (height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_paraCofactorI destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

paraCofactorD :: (Integral b) => Matrix CDouble b -> IO (Matrix CDouble b)
paraCofactorD src = liftM fromCMatrix (c_paraCofactorD $ toCMatrix src)

paraCofactorF :: (Integral b) => Matrix CFloat b -> IO (Matrix CFloat b)
paraCofactorF src = liftM fromCMatrix (c_paraCofactorF $ toCMatrix src)

paraCofactorI :: (Integral b) => Matrix CInt b -> IO (Matrix CInt b)
paraCofactorI src = liftM fromCMatrix (c_paraCofactorI $ toCMatrix src)

c_dotProductD :: C_Vector CDouble -> C_Vector CDouble -> IO Double
c_dotProductD aM bM = do
  alloca $ \a -> 
    alloca $ \b -> do
      poke a aM
      poke b bM
      hs_dotProductD a b

c_dotProductF :: C_Vector CFloat -> C_Vector CFloat -> IO Float
c_dotProductF aM bM = do
  alloca $ \a -> 
    alloca $ \b -> do
      poke a aM
      poke b bM
      hs_dotProductF a b

c_dotProductI :: C_Vector CInt -> C_Vector CInt -> IO Int
c_dotProductI aM bM = do
  alloca $ \a -> 
    alloca $ \b -> do
      poke a aM
      poke b bM
      hs_dotProductI a b

dotProductD :: (Integral b) => Vector CDouble b -> Vector CDouble b -> IO Double
dotProductD a b = c_dotProductD (toCVector a) (toCVector b)

dotProductF :: (Integral b) => Vector CFloat b -> Vector CFloat b -> IO Float
dotProductF a b = c_dotProductF (toCVector a) (toCVector b)

dotProductI :: (Integral b) => Vector CInt b -> Vector CInt b -> IO Int
dotProductI a b = c_dotProductI (toCVector a) (toCVector b)

c_crossProductD :: C_Vector CDouble -> C_Vector CDouble -> IO(C_Vector CDouble)
c_crossProductD aM bM =
  alloca $ \dest -> 
    alloca $ \a -> 
      alloca $ \b -> do
        destM <- vectorNull 3
        
        poke dest destM
        poke a aM 
        poke b bM

        err <- (hs_crossProductD dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_crossProductF :: C_Vector CFloat -> C_Vector CFloat -> IO(C_Vector CFloat)
c_crossProductF aM bM =
  alloca $ \dest -> 
    alloca $ \a -> 
      alloca $ \b -> do
        destM <- vectorNull 3
        
        poke dest destM
        poke a aM 
        poke b bM

        err <- (hs_crossProductF dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

c_crossProductI :: C_Vector CInt -> C_Vector CInt -> IO(C_Vector CInt)
c_crossProductI aM bM =
  alloca $ \dest -> 
    alloca $ \a -> 
      alloca $ \b -> do
        destM <- vectorNull 3
        
        poke dest destM
        poke a aM 
        poke b bM

        err <- (hs_crossProductI dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

crossProductD :: (Integral b) => Vector CDouble b -> Vector CDouble b -> IO(Vector CDouble b)
crossProductD a b = liftM fromCVector (c_crossProductD (toCVector a) (toCVector b))

crossProductF :: (Integral b) => Vector CFloat b -> Vector CFloat b -> IO(Vector CFloat b)
crossProductF a b = liftM fromCVector (c_crossProductF (toCVector a) (toCVector b))

crossProductI :: (Integral b) => Vector CInt b-> Vector CInt b -> IO(Vector CInt b)
crossProductI a b = liftM fromCVector (c_crossProductI (toCVector a) (toCVector b))