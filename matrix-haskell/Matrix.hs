{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude, DeriveGeneric, ConstrainedClassMethods #-}

module Matrix (
  FromMatrix(..),

  Matrix(..),
  ListMatrix(..),

  Vector,
  vData, vHeight,

  CInt,
  CFloat,
  CDouble,

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

  multiplyD,
  multiplyF,
  multiplyI,

  dotProductD,
  dotProductF,
  dotProductI,

  matrixNull,
  vectorNull,
  vector,
  matrixCpy,
  freeM,
  freeV,
  matrix,

  listToCMatrix,
  listToMatrix,
  listToCVector,
  listToVector,

  withCMatrix,
  withMatrix,

  withCMatrixNull,
  withMatrixNull,

  withCVector,
  withVector,

  withCVectorNull,
  withVectorNull
              ) where

import Prelude (
  Show(..), Eq(..), Num(..), Enum(..), Bool(..), Functor(..),
  String, IO, Double, Float,
  otherwise, error, putStrLn, notElem, take, map, id,
  replicate, length, not, takeWhile, fromIntegral,
  (*), (++), ($), (.), (>), (&&))
import HaskellMatrixError (
  createError, MatrixError(..), ErrorCode(..))
import Data.Int (Int, Int)
import Foreign (
  Storable(..), Ptr,
  peekArray, pokeArray, mallocArray,
  free, alloca, copyArray)
import Foreign.CStorable (CStorable(..))
import Foreign.C.Types (CInt(..), CDouble(..), CFloat(..))
import GHC.Generics (Generic(..))
import Control.Monad (Monad(..), when, liftM, liftM2)
import System.IO.Unsafe (unsafePerformIO)

{-
data Type =
  I | F | D | CI | CF | CD
  deriving (Eq, Show)

class SType a where
  fromSType :: (CType b) -> a -> b
  getType   :: a -> Type

class CType a where
  fromCType :: (SType b) => a -> b
  getType   :: a -> Type

instance SType Int where
  fromSType = CInt
  getType   = const I

instance SType Float where
  fromSType = CFloat
  getType   = const F 

instance SType Double where
  fromSType = CDouble
  getType   = const D

instance CType CInt where
  fromCType = fromIntegral
  getType   = const CI 

instance CType CFloat where
  fromCType = unCFloat
  getType   = const CF

instance CType CDouble where
  fromCType = unCDouble
  getType   = const CD
-}

class FromMatrix m where
  toVector :: m a -> Vector a
  toCVector :: m a -> C_Vector a
  toMatrix :: m a -> Matrix a
  toCMatrix :: m a -> C_Matrix a
  toListMatrix :: (Storable a) => m a -> IO(ListMatrix a)

{-C_Matrix Holds data passed to the C Functions. It is not designed to be used though
It can be and is exported-}

data C_Matrix a = C_Matrix {c_mData :: Ptr a, c_width, c_height :: CInt}
                deriving (Generic)

instance FromMatrix C_Matrix where
  toCMatrix = id
  toVector m = case m of
    (C_Matrix p 1 h) -> Vector' p 1 (fromIntegral h)
    _ -> error "Vectors must have width 1"
  toCVector m = case m of
    (C_Matrix p 1 h) -> C_Vector' p 1 h
    _ -> error "Vectors must have width 1"
  toMatrix (C_Matrix p w h) = Matrix p (fromIntegral w) (fromIntegral h)
  toListMatrix (C_Matrix p w h) = do
    lst <- peekArray (fromIntegral $ w*h) p
    free p
    return $ ListMatrix lst (fromIntegral w) (fromIntegral h)


instance (CStorable a) => CStorable (C_Matrix a)

instance (CStorable a, Storable a) => Storable (C_Matrix a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance (Show a, Storable a) => Show (C_Matrix a) where
  show (C_Matrix p wid hei) = prettyPrint 1 w (unsafePerformIO (peekArray (w*h) p))
    where
      w = fromIntegral wid
      h = fromIntegral hei

instance (Eq a, Storable a) => Eq (C_Matrix a) where
  (C_Matrix arr1 w1 h1) == (C_Matrix arr2 w2 h2) = (w1 == w2) && (h1 == h2) && (unsafePerformIO $ cmpArr (fromIntegral $ w1 * h1) arr1 arr2)

{- Holds vector data sent to the C functions. This ensures type security -}
data C_Vector' a = C_Vector' {c_vData :: Ptr a, c_vWidth, c_vHeitht :: CInt}
                  deriving (Generic)
type C_Vector a = C_Vector' a

instance FromMatrix C_Vector' where
  toCVector = id
  toVector (C_Vector' p w h) = Vector' p (fromIntegral w) (fromIntegral h)
  toMatrix (C_Vector' p w h) = Matrix p (fromIntegral w) (fromIntegral h)
  toCMatrix (C_Vector' p w h) = C_Matrix p w h
  toListMatrix (C_Vector' p w h) = do
    lst <- peekArray (fromIntegral $ w*h) p
    free p
    return $ ListMatrix lst (fromIntegral w) (fromIntegral h)

instance (Show a, Storable a) => Show (C_Vector' a) where
  show (C_Vector' p _ h) = prettyPrint 1 1 (unsafePerformIO (peekArray (fromIntegral h) p))

instance (Eq a, Storable a) => Eq (C_Vector' a) where
  (C_Vector' arr1 _ h1) == (C_Vector' arr2 _ h2) = (h1 == h2) && (unsafePerformIO $ cmpArr (fromIntegral $ h1) arr1 arr2)

instance (CStorable a) => CStorable (C_Vector' a)

instance (CStorable a, Storable a) => Storable (C_Vector' a) where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

{-
Main form of user interaction. Holds nicer Int types but still contains Ptr.
-}
data Matrix a = Matrix {mData :: Ptr a, width, height :: Int}

instance FromMatrix Matrix where
  toMatrix = id
  toVector m = case m of
    (Matrix p 1 h) -> Vector' p 1 h
    _ -> error "Vectors must have width 1"
  toCVector m = case m of
    (Matrix p 1 h) -> C_Vector' p 1 (fromIntegral h)
    _ -> error "Vectors must have width 1"
  toCMatrix (Matrix p w h) = C_Matrix p (fromIntegral w) (fromIntegral h)
  toListMatrix (Matrix p w h) = do
    lst <- peekArray (w*h) p
    free p
    return $ ListMatrix lst w h

instance (Show a, Storable a) => Show (Matrix a) where
  show (Matrix p w h) = prettyPrint 1 w (unsafePerformIO (peekArray (w*h) p))

instance (Eq a, Storable a) => Eq (Matrix a) where
  (Matrix arr1 w1 h1) == (Matrix arr2 w2 h2) = (w1 == w2) && (h1 == h2) && (unsafePerformIO $ cmpArr (w1 * h1) arr1 arr2)


data Vector' a = Vector' {vData :: Ptr a, vWidth, vHeight :: Int}

type Vector a = Vector' a

instance FromMatrix Vector' where
  toVector = id
  toCVector (Vector' p w h) = C_Vector' p (fromIntegral w) (fromIntegral h)
  toMatrix (Vector' p w h) = Matrix p w h
  toCMatrix (Vector' p w h) = C_Matrix p (fromIntegral w) (fromIntegral h)
  toListMatrix (Vector' p w h) = do
    lst <- peekArray (w*h) p
    free p
    return $ ListMatrix lst w h

instance (Show a, Storable a) => Show (Vector' a) where
  show (Vector' p _ h) = prettyPrint 1 1 (unsafePerformIO (peekArray h p))

instance (Eq a, Storable a) => Eq (Vector' a) where
  (Vector' arr1 _ h1) == (Vector' arr2 _ h2) =(h1 == h2) && (unsafePerformIO $ cmpArr h1 arr1 arr2)

{-
This structure is usable outside the IO() Monad. This should be used for manipulating the matrix without IO
-} 
data ListMatrix a = ListMatrix {lData :: [a], l_width, l_height :: Int}
  deriving (Eq)

instance Functor ListMatrix where
  fmap f (ListMatrix lst w h) = ListMatrix (map f lst) w h

instance (Show a) => Show (ListMatrix a) where
  show (ListMatrix lst w _) = prettyPrint 1 w lst

cmpArr :: (Eq a, Storable a) => Int -> Ptr a -> Ptr a -> IO(Bool)
cmpArr size p1 p2 = liftM2 (==) (peekArray size p1) (peekArray size p2)

prettyPrint :: (Show a) => Int -> Int -> [a] -> String 
prettyPrint n w lst = case lst of
  x:xs
    | n == w -> (padRight 7 $ round 6 $ show x) ++ "\n" ++ prettyPrint 1 w xs
    | otherwise -> (padRight 7 $ round 6 $ show x) ++ "    " ++ prettyPrint (succ n) w xs
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

foreign import ccall "HaskellMatrix.h hs_transposeD" hs_transposeD :: Ptr (C_Matrix CDouble) -> IO ()
foreign import ccall "HaskellMatrix.h hs_transposeF" hs_transposeF :: Ptr (C_Matrix CFloat) -> IO ()
foreign import ccall "HaskellMatrix.h hs_transposeI" hs_transposeI :: Ptr (C_Matrix CInt) -> IO ()

foreign import ccall "HaskellMatrix.h hs_invertD" hs_invertD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_invertF" hs_invertF :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_invertI" hs_invertI :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_printmD" hs_printmD :: Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_printmF" hs_printmF :: Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_printmI" hs_printmI :: Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_determinantD" hs_determinantD :: Ptr (C_Matrix CDouble) -> IO Double
foreign import ccall "HaskellMatrix.h hs_determinantF" hs_determinantF :: Ptr (C_Matrix CFloat) -> IO Float
foreign import ccall "HaskellMatrix.h hs_determinantI" hs_determinantI :: Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_cofactorD" hs_cofactorD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_cofactorF" hs_cofactorF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_cofactorI" hs_cofactorI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_multiplyD" hs_multiplyD :: Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> Ptr (C_Matrix CDouble) -> IO Int
foreign import ccall "HaskellMatrix.h hs_multiplyF" hs_multiplyF :: Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> Ptr (C_Matrix CFloat) -> IO Int
foreign import ccall "HaskellMatrix.h hs_multiplyI" hs_multiplyI :: Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> Ptr (C_Matrix CInt) -> IO Int

foreign import ccall "HaskellMatrix.h hs_dotProductD" hs_dotProductD :: Ptr (C_Vector CDouble) -> Ptr (C_Vector CDouble) -> IO Double
foreign import ccall "HaskellMatrix.h hs_dotProductF" hs_dotProductF :: Ptr (C_Vector CFloat) -> Ptr (C_Vector CFloat) -> IO Float
foreign import ccall "HaskellMatrix.h hs_dotProductI" hs_dotProductI :: Ptr (C_Vector CInt) -> Ptr (C_Vector CInt) -> IO Int

unCFloat :: CFloat -> Float
unCFloat (CFloat a) = a

unCDouble :: CDouble -> Double
unCDouble (CDouble a) = a

unCInt :: CInt -> Int 
unCInt (CInt a) = fromIntegral a

listToMatrix :: (Storable a) => ListMatrix a -> IO(Matrix a)
listToMatrix (ListMatrix lst w h) = matrix w h lst

listToCMatrix :: (Storable a) => ListMatrix a -> IO(C_Matrix a)
listToCMatrix (ListMatrix lst w h) = c_matrix (fromIntegral w) (fromIntegral h) lst

listToVector :: (Storable a) => ListMatrix a -> IO(Vector a)
listToVector list = case list of
  (ListMatrix lst 1 h) -> vector h lst
  _ -> error "Vectors must have width 1"

listToCVector :: (Storable a) => ListMatrix a -> IO(C_Vector a)
listToCVector list = case list of
  (ListMatrix lst 1 h) -> c_vector (fromIntegral h) lst
  _ -> error "Vectors must have width 1"

c_matrixNull :: (Storable a) => CInt -> CInt -> IO(C_Matrix a)
c_matrixNull w h = do
  p <- mallocArray (fromIntegral $ w * h)
  return $ C_Matrix p w h

matrixNull :: (Storable a) => Int -> Int -> IO(Matrix a)
matrixNull w h = do
  p <- mallocArray (w * h)
  return $ Matrix p w h

c_vectorNull :: (Storable a) => CInt -> IO(C_Vector a)
c_vectorNull h = do
  p <- mallocArray $ fromIntegral h 
  return $ C_Vector' p 1 h

vectorNull :: (Storable a) => Int -> IO(Vector a)
vectorNull h = do
  p <- mallocArray h 
  return $ Vector' p 1 h

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

c_vector :: (Storable a) => CInt -> [a] -> IO(C_Vector' a)
c_vector h arr = do
  p <- mallocArray $ fromIntegral h
  pokeArray p arr 
  return $ C_Vector' p 1 h

vector :: (Storable a) => Int -> [a] -> IO(Vector a)
vector h arr = do
  p <- mallocArray h
  pokeArray p arr
  return $ Vector' p 1 h

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

c_freeV :: C_Vector a -> IO()
c_freeV = free . c_vData

freeV :: Vector a -> IO()
freeV = free . vData

c_transposeD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_transposeD srcM = 
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    hs_transposeD dest
    peek dest

c_transposeF :: C_Matrix CFloat -> IO(C_Matrix CFloat)
c_transposeF srcM = 
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    hs_transposeF dest
    peek dest

c_transposeI :: C_Matrix CInt -> IO(C_Matrix CInt)
c_transposeI srcM = 
  alloca $ \dest -> do
    destM <- c_matrixCpy srcM
    poke dest destM
    hs_transposeI dest
    peek dest

transposeD :: Matrix CDouble -> IO(Matrix CDouble)
transposeD srcM = (c_transposeD $ toCMatrix srcM) >>= (return . toMatrix)

transposeF :: Matrix CFloat -> IO(Matrix CFloat)
transposeF srcM = (c_transposeF $ toCMatrix srcM) >>= (return . toMatrix)

transposeI :: Matrix CInt -> IO(Matrix CInt)
transposeI srcM = (c_transposeI $ toCMatrix srcM) >>= (return . toMatrix)

c_invertD :: C_Matrix CDouble -> IO(C_Matrix CDouble)
c_invertD srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertD dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_invertF :: C_Matrix CFloat -> IO(C_Matrix CDouble)
c_invertF srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertF dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

c_invertI :: C_Matrix CInt -> IO(C_Matrix CDouble)
c_invertI srcM =
  alloca $ \dest ->
    alloca $ \src -> do
      destM <- c_matrixNull (c_width srcM) (c_height srcM)

      poke dest destM
      poke src srcM

      err <- hs_invertI dest src >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)

      peek dest

invertD :: Matrix CDouble -> IO(Matrix CDouble)
invertD srcM = (c_invertD $ toCMatrix srcM) >>= (return . toMatrix)

invertF :: Matrix CFloat -> IO(Matrix CDouble)
invertF srcM = (c_invertF $ toCMatrix srcM) >>= (return . toMatrix)

invertI :: Matrix CInt -> IO(Matrix CDouble)
invertI srcM = (c_invertI $ toCMatrix srcM) >>= (return . toMatrix)

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

printmD :: Matrix CDouble -> IO(MatrixError)
printmD mat = c_printmD $ toCMatrix mat

printmF :: Matrix CFloat -> IO(MatrixError)
printmF mat = c_printmF $ toCMatrix mat

printmI :: Matrix CInt -> IO(MatrixError)
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

determinantD :: Matrix CDouble -> IO(Double)
determinantD mat = c_determinantD $ toCMatrix mat

determinantF :: Matrix CFloat -> IO(Float)
determinantF mat = c_determinantF $ toCMatrix mat

determinantI :: Matrix CInt -> IO(Int)
determinantI mat = c_determinantI $ toCMatrix mat

c_cofactorD :: C_Matrix CDouble -> IO (C_Matrix CDouble)
c_cofactorD src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorD destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_cofactorF :: C_Matrix CFloat -> IO (C_Matrix CFloat)
c_cofactorF src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorF destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

c_cofactorI :: C_Matrix CInt -> IO (C_Matrix CInt)
c_cofactorI src = 
  alloca $ \srcPtr -> 
    alloca $ \destPtr -> do
      dest <- c_matrixNull (c_width src) (c_height src)

      poke srcPtr src 
      poke destPtr dest 

      err <- hs_cofactorI destPtr srcPtr >>= (return . createError . fromIntegral)
      when (eType err /= Success) (error $ message err)
      
      peek destPtr

cofactorD :: Matrix CDouble -> IO (Matrix CDouble)
cofactorD src = liftM toMatrix (c_cofactorD $ toCMatrix src)

cofactorF :: Matrix CFloat -> IO (Matrix CFloat)
cofactorF src = liftM toMatrix (c_cofactorF $ toCMatrix src)

cofactorI :: Matrix CInt -> IO (Matrix CInt)
cofactorI src = liftM toMatrix (c_cofactorI $ toCMatrix src)

c_multiplyD :: C_Matrix CDouble -> C_Matrix CDouble -> IO(C_Matrix CDouble)
c_multiplyD aM bM =
  alloca $ \dest ->
    alloca $ \a ->
      alloca $ \b -> do
        destM <- c_matrixNull (c_height aM) (c_width bM)

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
        destM <- c_matrixNull (c_height aM) (c_width bM)

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
        destM <- c_matrixNull (c_height aM) (c_width bM)

        poke dest destM
        poke a aM
        poke b bM

        err <- (hs_multiplyI dest a b) >>= (return . createError . fromIntegral)
        when (eType err /= Success) (error $ message err)

        peek dest

multiplyD :: Matrix CDouble -> Matrix CDouble -> IO(Matrix CDouble)
multiplyD aM bM = liftM toMatrix (c_multiplyD (toCMatrix aM) (toCMatrix bM))

multiplyF :: Matrix CFloat -> Matrix CFloat -> IO(Matrix CFloat)
multiplyF aM bM = liftM toMatrix (c_multiplyF (toCMatrix aM) (toCMatrix bM))

multiplyI :: Matrix CInt -> Matrix CInt -> IO(Matrix CInt)
multiplyI aM bM = liftM toMatrix (c_multiplyI (toCMatrix aM) (toCMatrix bM))

withCMatrix :: (Storable a) => CInt -> CInt -> [a] -> (C_Matrix a -> IO b) -> IO b
withCMatrix w h d f = do
  mat <- c_matrix w h d
  ret <- f mat
  c_freeM mat
  return ret

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

dotProductD :: Vector CDouble -> Vector CDouble -> IO Double
dotProductD a b = c_dotProductD (toCVector a) (toCVector b)

dotProductF :: Vector CFloat -> Vector CFloat -> IO Float
dotProductF a b = c_dotProductF (toCVector a) (toCVector b)

dotProductI :: Vector CInt -> Vector CInt -> IO Int
dotProductI a b = c_dotProductI (toCVector a) (toCVector b)

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

withVector :: (Storable a) => Int -> [a] -> (Vector a -> IO b) -> IO b
withVector h arr f = do
  vec <- vector h arr
  ret <- f vec 
  freeV vec 
  return ret

withCVector :: (Storable a) => CInt -> [a] -> (C_Vector a -> IO b) -> IO b
withCVector h arr f = do 
  vec <- c_vector h arr 
  ret <- f vec
  c_freeV vec
  return ret

withCVectorNull :: (Storable a) => CInt -> (C_Vector a -> IO b) -> IO b
withCVectorNull h f = do
  vec <- c_vectorNull h 
  ret <- f vec
  c_freeV vec 
  return ret

withVectorNull :: (Storable a) => Int -> (Vector a -> IO b) -> IO b
withVectorNull h f = do
  vec <- vectorNull h 
  ret <- f vec
  freeV vec 
  return ret