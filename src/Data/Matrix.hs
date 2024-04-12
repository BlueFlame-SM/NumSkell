{- |
Module      : Data.Matrix
Description : Provides a matrix data type and operations on matrices.

This module provides a matrix data type and operations on matrices. The matrix
data type is parameterized by the number of rows and columns, and the elements
of the matrix are stored in a flat vector. This module provides functions to
create matrices, access elements of matrices, and perform operations on matrices
such as addition, multiplication, and element-wise operations.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Matrix (
    Matrix,
    fromVector,
    fromList,
    withVecAsVec,
    withListAsVec,
    empty,
    singleton,
    replicate,
    idMatrix,
    zipWith,
    matrixMult,
    reshape,
    index,
    set,
    rows,
    cols,
    size,
    toList,
    toVector
) where

import Data.List (intercalate)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import GHC.TypeLits.Singletons ( Natural, SNat )
import Prelude.Singletons
    ( SingI(..),
      withSingI,
      SingKind(fromSing, toSing),
      SomeSing(SomeSing),
      POrd(type (<)),
      PNum(type (*)) )

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))
import qualified Prelude as P

-- | A matrix data type with dimensions specified by type-level naturals.
newtype Matrix (n :: Natural) (m :: Natural) a
    = Matrix {
        -- | Convert a matrix to a flat vector.
        toVector :: Vector a
    }
    deriving (Eq, Ord)

-- | Convert a matrix to a flat list.
--
-- === __Examples__:
--
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> toList m
-- [1,2,3,4]
--
toList :: Matrix n m a -> [a]
toList = V.toList . toVector

-- | Convert a vector to an $n \times m$ matrix.
--
-- If the vector has the wrong number of elements, this function will return
-- 'Nothing'.
--
-- === __Examples__:
--
-- >>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Matrix 2 2 Int)
-- Just [[1 2]
--  [3 4]]
--
-- >>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Matrix 3 3 Int)
-- Nothing
fromVector :: forall n m a . (SingI n, SingI m) => Vector a -> Maybe (Matrix n m a)
fromVector = fromVector_ (sing :: SNat n) (sing :: SNat m)

fromVector_ :: forall n m a . SNat n -> SNat m -> Vector a -> Maybe (Matrix n m a)
fromVector_ n m v
    | n' * m' == V.length v = Just $ Matrix v
    | otherwise             = Nothing
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

-- | Convert a list to an $n \times m$ matrix.
--
-- Same as 'fromVector', but takes a list instead of a vector.
--
fromList :: forall n m a. (SingI n, SingI m) => [a] -> Maybe (Matrix n m a)
fromList l = fromVector (V.fromList l)

withVecAsVec_ :: Vector a -> (forall m. SNat m -> Matrix 1 m a -> b) -> b
withVecAsVec_ v f = case toSing (fromIntegral (V.length v)) of
    SomeSing (s :: SNat q) -> f s (Matrix v)

-- | Use a vector as a $1 \times m$ matrix.
--
-- This function is useful when you have a vector and you want to treat it as a
-- matrix with one row.
--
-- === __Examples__:
--
-- >>> withVecAsVec (V.fromList [1, 2, 3]) (\mat -> toList (mat + pure 1))
-- [2,3,4]
--
withVecAsVec :: Vector a -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withVecAsVec v f = withVecAsVec_ v (\s a -> withSingI s (f a))

-- | Use a list as a $1 \times m$ matrix.
--
-- Same as 'withVecAsVec', but takes a list instead of a vector.
--
-- === __Examples__:
--
-- >>> withListAsVec [1, 2, 3] (\mat -> toList (mat + pure 1))
-- [2,3,4]
--
withListAsVec :: [a] -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withListAsVec l = withVecAsVec (V.fromList l)

-- | Create a $0 \times 0$ matrix.
--
-- Creates an empty matrix. This is useful for creating a matrix that will be
-- filled in later.
--
-- === __Examples__:
--
-- >>> empty :: Matrix 0 0 Int
-- []
--
empty :: Matrix 0 0 a
empty = Matrix V.empty

-- | Create a $1 \times 1$ matrix.
--
-- === __Examples__:
--
-- >>> singleton 1 :: Matrix 1 1 Int
-- [[1]]
--
singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ V.singleton x

-- | Create an $n \times m$ matrix where all elements are the same.
--
-- === __Examples__:
--
-- >>> replicate 1 :: Matrix 2 2 Int
-- [[1 1]
--  [1 1]]
--
replicate :: forall n m a. (SingI n, SingI m) => a -> Matrix n m a
replicate = Matrix . V.replicate (fromIntegral $ fromSing (sing :: SNat n) * fromSing (sing :: SNat m))

-- | Create an $n \times m$ identity matrix.
--
-- === __Examples__:
--
-- >>> idMatrix :: Matrix 2 2 Int
-- [[1 0]
--  [0 1]]
--
-- >>> idMatrix :: Matrix 2 3 Int
-- [[1 0 0]
--  [0 1 0]]
--
idMatrix :: forall n m a. (Num a, SingI n, SingI m) => Matrix n m a
idMatrix = idMatrix_ (sing :: SNat n) (sing :: SNat m)

idMatrix_ :: (Num a) => SNat n -> SNat m -> Matrix n m a
idMatrix_ n m = Matrix $ V.fromList $ [if i == j then 1 else 0 | i <- [1 .. n'] :: [Int], j <- [1 .. m']]
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

-- | Element-wise join of two $n \times m$ matrices.
--
-- === __Examples__:
--
-- >>> zipWith (+) (replicate 1) (replicate 2) :: Matrix 2 2 Int
-- [[3 3]
--  [3 3]]
--
zipWith :: (SingI n, SingI m) => (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
zipWith f (Matrix x) (Matrix y) = Matrix $ V.zipWith f x y

-- | Matrix multiplication.
--
-- Multiplies a $n \times m$ matrix by a $m \times k$ matrix to produce a $n \times k$ matrix.
--
-- === __Examples__:
--
-- >>> let Just m1 = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> let Just m2 = fromList [5, 6, 7, 8] :: Maybe (Matrix 2 2 Int)
-- >>> matrixMult m1 m2
-- [[19 22]
--  [43 50]]
--
matrixMult :: forall n m k a . (SingI n, SingI k, SingI m, Num a)
            => Matrix n m a -> Matrix m k a -> Matrix n k a
matrixMult = matrixMult_ (sing :: SNat n) (sing :: SNat m) (sing :: SNat k)

matrixMult_ :: (Num a) => SNat n -> SNat m -> SNat p -> Matrix n m a -> Matrix m p a -> Matrix n p a
matrixMult_ n' m' p' (Matrix xs) (Matrix ys) = Matrix $ V.generate (n * p) fromIndex
    where n = fromIntegral $ fromSing n'
          m = fromIntegral $ fromSing m'
          p = fromIntegral $ fromSing p'
          productSum i j = let ks = [0 .. m - 1]
                               as   = [xs ! (i * m + k) | k <- ks]
                               bs   = [ys ! (k * p + j) | k <- ks]
                           in P.sum $ P.zipWith (*) as bs
          fromIndex i' = let (i, j) = i' `divMod` p
                         in productSum i j

-- | Reshape a matrix to a matrix with the same size but different dimensions.
--
-- === __Examples__:
--
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> reshape m :: Matrix 4 1 Int
-- [[1]
--  [2]
--  [3]
--  [4]]
--
reshape :: (SingI n, SingI m, SingI k, SingI l, n * m ~ k * l) => Matrix n m a -> Matrix k l a
reshape (Matrix v) = Matrix v

-- | Get the element at the $i$th row and $j$th column of a matrix.
--
-- This function requires the language extension @TypeApplications@ to specify
-- the row and column indices.
--
-- === __Examples__:
--
-- >>> :set -XTypeApplications
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> index @1 @0 m
-- 3
--
-- >>> index @0 @1 m
-- 2
--
-- >>> index @2 @0 m
-- Couldn't match type 'False with 'True arising from a use of `index'
-- In the expression: index @2 @0 m
-- In an equation for `it_ackQu': it_ackQu = index @2 @0 m
--
index :: forall i j n m a . (SingI m, SingI i, SingI j) => ((i < n) ~ True, (j < m) ~ True) => Matrix n m a -> a
index = index_ (sing :: SNat m) (sing :: SNat i) (sing :: SNat j)

index_ :: forall n i m j a . ((i < n) ~ True, (j < m) ~ True) => SNat m -> SNat i -> SNat j -> Matrix n m a -> a
index_ m i j (Matrix v) = v ! fromEnum (fromSing i * fromSing m + fromSing j)

-- | Set the element at the $i$th row and $j$th column of a matrix.
--
-- This function requires the language extension @TypeApplications@ to specify
-- the row and column indices.
--
-- === __Examples__:
--
-- >>> :set -XTypeApplications
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> set @1 @0 5 m
-- [[1 2]
--  [5 4]]
--
set :: forall i j n m a . (SingI n, SingI m, SingI i, SingI j) => ((i < n) ~ True, (j < m) ~ True) => a -> Matrix n m a -> Matrix n m a
set = set_ (sing :: SNat m) (sing :: SNat i) (sing :: SNat j)

set_ :: forall n i m j a . (SingI n, (i < n) ~ True, (j < m) ~ True) => SNat m -> SNat i -> SNat j -> a -> Matrix n m a -> Matrix n m a
set_ m i j x (Matrix v) = Matrix $ v // [(fromEnum (fromSing i * fromSing m + fromSing j), x)]

-- | The number of rows in a matrix.
--
-- === __Examples__:
--
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> rows m
-- 2
--
rows :: forall n m a. SingI n => Matrix n m a -> Int
rows _ = fromEnum $ fromSing (sing :: SNat n)

-- | The number of columns in a matrix.
--
-- === __Examples__:
--
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> cols m
-- 2
--
cols :: forall n m a. SingI m => Matrix n m a -> Int
cols _ = fromEnum $ fromSing (sing :: SNat m)

-- | The number of elements in a matrix.
--
-- === __Examples__:
--
-- >>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
-- >>> size m
-- 4
--
size :: forall n m a. (SingI n, SingI m) => Matrix n m a -> Int
size _ = fromEnum $ fromSing (sing :: SNat n) * fromSing (sing :: SNat m)

showMatrix_ :: (Show a) => SNat n -> SNat m -> Matrix n m a -> String
showMatrix_ sn sm (Matrix v) = "[" ++ intercalate "\n " (map showRow [0 .. n - 1]) ++ "]"
  where
    n = fromEnum $ fromSing sn
    m = fromEnum $ fromSing sm
    showRow i = "[" ++ unwords (V.toList $ show <$> V.slice (i * m) m v) ++ "]"

showMatrix :: forall n m a. (SingI n, SingI m, Show a) => Matrix n m a -> String
showMatrix (Matrix v) = showMatrix_ (sing :: SNat n) (sing :: SNat m) (Matrix v)

instance (SingI n, SingI m, Show a) => Show (Matrix n m a) where
    show = showMatrix

instance (SingI n, SingI m) => Functor (Matrix n m) where
    fmap :: (SingI n, SingI m) => (a -> b) -> Matrix n m a -> Matrix n m b
    fmap f (Matrix xs) = Matrix $ fmap f xs

instance (SingI n, SingI m) => Applicative (Matrix n m) where
    pure :: (SingI n, SingI m) => a -> Matrix n m a
    pure = replicate
    (<*>) :: (SingI n, SingI m) => Matrix n m (a -> b) -> Matrix n m a -> Matrix n m b
    (<*>) = zipWith ($)

instance (SingI n, SingI m, Num x) => Num (Matrix n m x) where
    (+) :: (SingI n, SingI m, Num x) => Matrix n m x -> Matrix n m x -> Matrix n m x
    (+) = zipWith (+)
    (-) :: (SingI n, SingI m, Num x) => Matrix n m x -> Matrix n m x -> Matrix n m x
    (-) = zipWith (-)
    (*) :: (SingI n, SingI m, Num x) => Matrix n m x -> Matrix n m x -> Matrix n m x
    (*) = zipWith (*)
    abs :: (SingI n, SingI m, Num x) => Matrix n m x -> Matrix n m x
    abs = fmap abs
    signum :: (SingI n, SingI m, Num x) => Matrix n m x -> Matrix n m x
    signum = fmap signum
    fromInteger :: (SingI n, SingI m, Num x) => Integer -> Matrix n m x
    fromInteger = pure . fromInteger

instance (SingI n, SingI m) => Foldable (Matrix n m) where
    foldMap :: (SingI n, SingI m, Monoid m1) => (a -> m1) -> Matrix n m a -> m1
    foldMap f (Matrix v) = foldMap f v

instance (SingI n, SingI m) => Traversable (Matrix n m) where
    traverse :: (SingI n, SingI m, Applicative f) => (a -> f b) -> Matrix n m a -> f (Matrix n m b)
    traverse f (Matrix v) = fmap Matrix (traverse f v)

instance (SingI n, SingI m, Semigroup a) => Semigroup (Matrix n m a) where
    (<>) :: (SingI n, SingI m, Semigroup a) => Matrix n m a -> Matrix n m a -> Matrix n m a
    (<>) = zipWith (<>)

instance (SingI n, SingI m, Monoid a) => Monoid (Matrix n m a) where
    mempty :: (SingI n, SingI m, Monoid a) => Matrix n m a
    mempty = replicate mempty
