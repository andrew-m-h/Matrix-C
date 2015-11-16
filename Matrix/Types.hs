{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, ConstrainedClassMethods, MultiParamTypeClasses #-}

module Matrix.Types (
  FromMatrix(..),
  MatrixType(..),

  Matrix(..),
  C_Matrix,

  Vector'(..),
  Vector,
  C_Vector,
  mkVector,

  ListMatrix(..),

  toCMatrix,
  toCVector,
  fromCMatrix,
  fromCVector,

  unCInt,
  unCFloat,
  unCDouble

  ) where

import Prelude (
  Show(..), Eq(..), Num(..), Enum(..), Bool(..), Functor(..), Integral(..),
  String, IO, Double, Float, Maybe(..),
  otherwise, error, putStrLn, notElem, take, map, id, round,
  replicate, length, not, takeWhile, fromIntegral, undefined,
  (*), (++), ($), (.), (>), (&&))
import Data.Int (Int)
import Foreign (
  Storable(..), Ptr,
  peekArray, pokeArray, mallocArray,
  free)
import Foreign.C.Types (CInt(..), CDouble(..), CFloat(..))
import Foreign.CStorable(CStorable(..))
import GHC.Generics (Generic(..))
import Control.Monad (Monad(..), liftM2)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Float(float2Double, double2Float)

class FromMatrix m where
  toVector :: (Integral b, Num b) =>  m a b -> Vector a b
  toMatrix :: (Integral b) => m a b -> Matrix a b
  toListMatrix :: (Storable a, Integral b) => m a b -> IO(ListMatrix a)

class MatrixType t where
  toCIntMatrix :: (Integral a) => Matrix t a -> IO(Matrix CInt a)
  toCIntVector :: (Integral a) => Vector t a -> IO(Vector CInt a)
  toCDoubleMatrix ::(Integral a) =>  Matrix t a -> IO(Matrix CDouble a)
  toCDoubleVector :: (Integral a) => Vector t a -> IO(Vector CDouble a)
  toCFloatMatrix :: (Integral a) => Matrix t a -> IO(Matrix CFloat a)
  toCFloatVector :: (Integral a) => Vector t a -> IO(Vector CFloat a)

instance MatrixType CInt where
  toCIntMatrix = return . id
  toCIntVector = return . id
  
  toCDoubleMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map fromIntegral lst)
      return $ Matrix ptr w h
  toCDoubleVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map fromIntegral lst)
      return $ Vector' ptr 1 h
      
  toCFloatMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map fromIntegral lst)
      return $ Matrix ptr w h   
  toCFloatVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map fromIntegral lst)
      return $ Vector' ptr 1 h

instance MatrixType CDouble where
  toCDoubleMatrix = return . id
  toCDoubleVector = return . id
  
  toCFloatMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map cDouble2cFloat lst)
      return $ Matrix ptr w h
  toCFloatVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map cDouble2cFloat lst)
      return $ Vector' ptr 1 h   
  
  toCIntMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map round lst)
      return $ Matrix ptr w h
  toCIntVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map round lst)
      return $ Vector' ptr 1 h 

instance MatrixType CFloat where
  toCFloatMatrix = return . id
  toCFloatVector = return . id
  
  toCDoubleMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map cFloat2cDouble lst)
      return $ Matrix ptr w h
  toCDoubleVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map cFloat2cDouble lst)
      return $ Vector' ptr 1 h 
      
  toCIntMatrix (Matrix p w h) = do
      let size = fromIntegral $ w * h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map round lst)
      return $ Matrix ptr w h
  toCIntVector (Vector' p _ h) = do
      let size = fromIntegral h
      ptr <- mallocArray size
      lst <- peekArray size p
      pokeArray ptr (map round lst)
      return $ Vector' ptr 1 h

      
cDouble2cFloat :: CDouble -> CFloat
cDouble2cFloat = CFloat . double2Float . unCDouble

cFloat2cDouble :: CFloat -> CDouble
cFloat2cDouble = CDouble . float2Double . unCFloat

{-C_Matrix Holds data passed to the C Functions. It is not designed to be used though
It can be and is exported-}
data Matrix a b = Matrix {mData :: Ptr a, width, height :: b}
                deriving (Generic)
type C_Matrix a = Matrix a CInt


instance FromMatrix Matrix where
  toVector m = case m of
    (Matrix p 1 h) -> Vector' p 1 (fromIntegral h)
    _ -> error "Vectors must have width 1"
  toMatrix = id
  toListMatrix (Matrix p w h) = do
    lst <- peekArray (fromIntegral $ w*h) p
    free p
    return $ ListMatrix lst (fromIntegral w) (fromIntegral h)


instance (CStorable a, CStorable b) => CStorable (Matrix a b)

instance (CStorable a, Storable a, CStorable b, Storable b) => Storable (Matrix a b) where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance (Show a, Storable a, Show b, Num b, Integral b) => Show (Matrix a b) where
  show (Matrix p wid hei) = prettyPrint 1 w (unsafePerformIO (peekArray (w*h) p))
    where
      w = fromIntegral wid
      h = fromIntegral hei

instance (Eq a, Storable a, Eq b, Num b, Integral b) => Eq (Matrix a b) where
  (Matrix arr1 w1 h1) == (Matrix arr2 w2 h2) = (w1 == w2) && (h1 == h2) && (unsafePerformIO $ cmpArr (fromIntegral $ w1 * h1) arr1 arr2)

{- Holds vector data sent to the C functions. This ensures type security -}
data Vector' a b = Vector' {vData :: Ptr a, _unused, vHeight :: b}
                  deriving (Generic)

type Vector a b = Vector' a b
type C_Vector a = Vector' a CInt

instance FromMatrix Vector' where
  toVector = id
  toMatrix (Vector' p w h) = Matrix p (fromIntegral w) (fromIntegral h)
  toListMatrix (Vector' p w h) = do
    lst <- peekArray (fromIntegral $ w*h) p
    free p
    return $ ListMatrix lst (fromIntegral w) (fromIntegral h)

instance (Show a, Storable a, Show b, Num b, Integral b) => Show (Vector' a b) where
  show (Vector' p _ h) = prettyPrint 1 1 (unsafePerformIO (peekArray (fromIntegral h) p))

instance (Eq a, Storable a, Eq b, Num b, Integral b) => Eq (Vector' a b) where
  (Vector' arr1 _ h1) == (Vector' arr2 _ h2) = (h1 == h2) && (unsafePerformIO $ cmpArr (fromIntegral $ h1) arr1 arr2)

instance (CStorable a, CStorable b) => CStorable (Vector' a b)

instance (CStorable a, Storable a, CStorable b, Storable b) => Storable (Vector' a b) where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

mkVector :: (Storable a, Num b, Integral b) => Ptr a -> b -> Vector a b
mkVector p h = Vector' p 1 h

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
    | n == w -> (padRight 7 $ roundS 6 $ show x) ++ "\n" ++ prettyPrint 1 w xs
    | otherwise -> (padRight 7 $ roundS 6 $ show x) ++ "    " ++ prettyPrint (succ n) w xs
  [] -> ""

roundS :: Int -> String -> String
roundS l s
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

toCMatrix :: (Integral b) => Matrix a b -> C_Matrix a
toCMatrix (Matrix p w h) = Matrix p (fromIntegral w) (fromIntegral h)

fromCMatrix :: (Integral b) => C_Matrix a -> Matrix a b
fromCMatrix (Matrix p w h) = Matrix p (fromIntegral w) (fromIntegral h)

toCVector :: (Num b, Integral b) => Vector a b -> C_Vector a 
toCVector (Vector' p _ h) = Vector' p 1 (fromIntegral h)

fromCVector :: (Num b, Integral b) => C_Vector a -> Vector a b
fromCVector (Vector' p _ h) = Vector' p 1 (fromIntegral h)

unCFloat :: CFloat -> Float
unCFloat (CFloat a) = a

unCDouble :: CDouble -> Double
unCDouble (CDouble a) = a

unCInt :: CInt -> Int 
unCInt (CInt a) = fromIntegral a