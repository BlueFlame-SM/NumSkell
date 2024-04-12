{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType #-}

{- |
Module      : Data.Matrix
Description : Provides a matrix data type and operations on matrices.

This module provides a matrix data type and operations on matrices. The matrix
data type is parameterized by the number of rows and columns, and the elements
of the matrix are stored in a flat vector. This module provides functions to
create matrices, access elements of matrices, and perform operations on matrices
such as addition, multiplication, and element-wise operations.
-}
module Data.Matrix (
    -- * Matrix data type
    Matrix,

    -- ** Constructors
    fromVector,
    fromList,
    fromArray,
    withVecAsVec,
    withListAsVec,
    empty,
    singleton,
    replicate,
    idMatrix,

    -- ** Operations
    zipWith,
    matrixMult,
    reshape,
    index,
    set,
    rows,
    cols,
    size,
    toList,
    toVector,

    -- ** Convenience matrix constructors
    mat0x0,
    mat1x1,
    mat1x2,
    mat1x3,
    mat1x4,
    mat1x5,
    mat1x6,
    mat1x7,
    mat1x8,
    mat1x9,
    mat1x10,
    mat1x11,
    mat1x12,
    mat1x13,
    mat1x14,
    mat1x15,
    mat1x16,
    mat1x17,
    mat1x18,
    mat1x19,
    mat1x20,
    mat1x21,
    mat1x22,
    mat1x23,
    mat1x24,
    mat1x25,
    mat1x26,
    mat1x27,
    mat1x28,
    mat1x29,
    mat1x30,
    mat1x31,
    mat1x32,
    mat2x1,
    mat2x2,
    mat2x3,
    mat3x1,
    mat3x2,
    mat3x3,
) where

import Data.List (intercalate)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import GHC.TypeLits.Singletons (Natural, SNat)
import Prelude.Singletons (
    PNum (type (*)),
    POrd (type (<)),
    SingI (..),
    SingKind (fromSing, toSing),
    SomeSing (SomeSing),
    withSingI,
 )

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))
import qualified Prelude as P

import Data.Array (Array)
import qualified Data.Array as A

-- | A matrix data type with dimensions specified by type-level naturals.
newtype Matrix (n :: Natural) (m :: Natural) a = Matrix
    { toVector :: Vector a
    -- ^ Convert a matrix to a flat vector.
    }
    deriving (Eq, Ord)

{- | Convert a matrix to a flat list.

=== __Examples__:

>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> toList m
[1,2,3,4]
-}
toList :: Matrix n m a -> [a]
toList = V.toList . toVector

{- | Convert a vector to an $n \times m$ matrix.

If the vector has the wrong number of elements, this function will return
'Nothing'.

=== __Examples__:

>>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Matrix 2 2 Int)
Just [[1 2]
 [3 4]]

>>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Matrix 3 3 Int)
Nothing
-}
fromVector :: forall n m a. (SingI n, SingI m) => Vector a -> Maybe (Matrix n m a)
fromVector = fromVector_ (sing :: SNat n) (sing :: SNat m)

fromVector_ :: forall n m a. SNat n -> SNat m -> Vector a -> Maybe (Matrix n m a)
fromVector_ n m v
    | n' * m' == V.length v = Just $ Matrix v
    | otherwise = Nothing
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

{- | Convert a list to an $n \times m$ matrix.

Same as 'fromVector', but takes a list instead of a vector.
-}
fromList :: forall n m a. (SingI n, SingI m) => [a] -> Maybe (Matrix n m a)
fromList l = fromVector (V.fromList l)

{- | Convert an Array to an $n \times m$ matrix.

Same as 'fromVector', but takes an Array instead of a vector.
-}
fromArray :: forall n m k a. (n * m ~ k) => Array k a -> Matrix n m a
fromArray = Matrix . A.toVector

{- | Use a vector of vectors as a $n \times m$ matrix, where $m$, where $m$ is
the minimum length of all the nested vectors.

This function is useful when you have a vector of vectors and you want to
treat it as a matrix, and you are okay with the matrix representation being
a truncated version of the vector of vectors.

=== __Exammples__:

>>> withVec (V.fromList [V.fromList [1, 2, 3], V.fromList [4, 5, 6]] (\mat -> toList (mat + pure 1))
[[2, 3, 4] [5, 6, 7]]
-}
withVec :: Vector (Vector a) -> (forall n m. (SingI n, SingI m) => Matrix n m a -> b) -> b
withVec v f = withVec_ v (\s t a -> withSingI s (withSingI t (f a)))

withVec_ :: Vector (Vector a) -> (forall n m. SNat n -> SNat m -> Matrix n m a -> b) -> b
withVec_ v f =
    let
        n = V.length v
        m = V.foldl' min 0 (V.map V.length v)
        truncatedV = foldMap (V.slice 0 m) v
     in
        case (toSing $ fromIntegral n, toSing $ fromIntegral m) of
            (SomeSing (s :: SNat q), SomeSing (t :: SNat r)) -> f s t (Matrix truncatedV)

{- | Use a list of lists as a $n \times m$ matrix, where $m$, where $m$ is the
minimum length of all the nested vectors.

This function is useful when you have a list of lists and you want to treat it
a matrix, and you are okay with the matrix representation being a truncated
version of the list of lists.

=== __Exammples__:

>>> withList ([[1, 2, 3], [4, 5, 6]] (\mat -> toList (mat + pure 1))
[[2, 3, 4] [5, 6, 7]]
-}
withList :: [[a]] -> (forall n m. (SingI n, SingI m) => Matrix n m a -> b) -> b
withList l = withVec (V.fromList (map V.fromList l))

{- | Use a vector as a $1 \times m$ matrix.

This function is useful when you have a vector and you want to treat it as a
matrix with one row.

=== __Examples__:

>>> withVecAsVec (V.fromList [1, 2, 3]) (\mat -> toList (mat + pure 1))
[2,3,4]
-}
withVecAsVec :: Vector a -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withVecAsVec v f = withVecAsVec_ v (\s a -> withSingI s (f a))

withVecAsVec_ :: Vector a -> (forall m. SNat m -> Matrix 1 m a -> b) -> b
withVecAsVec_ v f = case toSing (fromIntegral (V.length v)) of
    SomeSing (s :: SNat q) -> f s (Matrix v)

{- | Use a list as a $1 \times m$ matrix.

Same as 'withVecAsVec', but takes a list instead of a vector.

=== __Examples__:

>>> withListAsVec [1, 2, 3] (\mat -> toList (mat + pure 1))
[2,3,4]
-}
withListAsVec :: [a] -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withListAsVec l = withVecAsVec (V.fromList l)

{- | Create a $0 \times 0$ matrix.

Creates an empty matrix. This is useful for creating a matrix that will be
filled in later.

=== __Examples__:

>>> empty :: Matrix 0 0 Int
[]
-}
empty :: Matrix 0 0 a
empty = Matrix V.empty

{- | Create a $1 \times 1$ matrix.

=== __Examples__:

>>> singleton 1 :: Matrix 1 1 Int
[[1]]
-}
singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ V.singleton x

{- | Create an $n \times m$ matrix where all elements are the same.

=== __Examples__:

>>> replicate 1 :: Matrix 2 2 Int
[[1 1]
 [1 1]]
-}
replicate :: forall n m a. (SingI n, SingI m) => a -> Matrix n m a
replicate = Matrix . V.replicate (fromIntegral $ fromSing (sing :: SNat n) * fromSing (sing :: SNat m))

{- | Create an $n \times m$ identity matrix.

=== __Examples__:

>>> idMatrix :: Matrix 2 2 Int
[[1 0]
 [0 1]]

>>> idMatrix :: Matrix 2 3 Int
[[1 0 0]
 [0 1 0]]
-}
idMatrix :: forall n m a. (Num a, SingI n, SingI m) => Matrix n m a
idMatrix = idMatrix_ (sing :: SNat n) (sing :: SNat m)

idMatrix_ :: (Num a) => SNat n -> SNat m -> Matrix n m a
idMatrix_ n m = Matrix $ V.fromList $ [if i == j then 1 else 0 | i <- [1 .. n'] :: [Int], j <- [1 .. m']]
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

{- | Element-wise join of two $n \times m$ matrices.

=== __Examples__:

>>> zipWith (+) (replicate 1) (replicate 2) :: Matrix 2 2 Int
[[3 3]
 [3 3]]
-}
zipWith :: (SingI n, SingI m) => (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
zipWith f (Matrix x) (Matrix y) = Matrix $ V.zipWith f x y

{- | Matrix multiplication.

Multiplies a $n \times m$ matrix by a $m \times k$ matrix to produce a $n \times k$ matrix.

=== __Examples__:

>>> let Just m1 = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> let Just m2 = fromList [5, 6, 7, 8] :: Maybe (Matrix 2 2 Int)
>>> matrixMult m1 m2
[[19 22]
 [43 50]]
-}
matrixMult ::
    forall n m k a.
    (SingI n, SingI k, SingI m, Num a) =>
    Matrix n m a ->
    Matrix m k a ->
    Matrix n k a
matrixMult = matrixMult_ (sing :: SNat n) (sing :: SNat m) (sing :: SNat k)

matrixMult_ :: (Num a) => SNat n -> SNat m -> SNat p -> Matrix n m a -> Matrix m p a -> Matrix n p a
matrixMult_ n' m' p' (Matrix xs) (Matrix ys) = Matrix $ V.generate (n * p) fromIndex
  where
    n = fromIntegral $ fromSing n'
    m = fromIntegral $ fromSing m'
    p = fromIntegral $ fromSing p'
    productSum i j =
        let ks = [0 .. m - 1]
            as = [xs ! (i * m + k) | k <- ks]
            bs = [ys ! (k * p + j) | k <- ks]
         in P.sum $ P.zipWith (*) as bs
    fromIndex i' =
        let (i, j) = i' `divMod` p
         in productSum i j

{- | Reshape a matrix to a matrix with the same size but different dimensions.

=== __Examples__:

>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> reshape m :: Matrix 4 1 Int
[[1]
 [2]
 [3]
 [4]]
-}
reshape :: (SingI n, SingI m, SingI k, SingI l, n * m ~ k * l) => Matrix n m a -> Matrix k l a
reshape (Matrix v) = Matrix v

{- | Get the element at the $i$th row and $j$th column of a matrix.

This function requires the language extension @TypeApplications@ to specify
the row and column indices.

=== __Examples__:

>>> :set -XTypeApplications
>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> index @1 @0 m
3

>>> index @0 @1 m
2

>>> index @2 @0 m
Couldn't match type 'False with 'True arising from a use of `index'
In the expression: index @2 @0 m
In an equation for `it_ackQu': it_ackQu = index @2 @0 m
-}
index :: forall i j n m a. (SingI m, SingI i, SingI j) => ((i < n) ~ True, (j < m) ~ True) => Matrix n m a -> a
index = index_ (sing :: SNat m) (sing :: SNat i) (sing :: SNat j)

index_ :: forall n i m j a. ((i < n) ~ True, (j < m) ~ True) => SNat m -> SNat i -> SNat j -> Matrix n m a -> a
index_ m i j (Matrix v) = v ! fromEnum (fromSing i * fromSing m + fromSing j)

{- | Set the element at the $i$th row and $j$th column of a matrix.

This function requires the language extension @TypeApplications@ to specify
the row and column indices.

=== __Examples__:

>>> :set -XTypeApplications
>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> set @1 @0 5 m
[[1 2]
 [5 4]]
-}
set :: forall i j n m a. (SingI n, SingI m, SingI i, SingI j) => ((i < n) ~ True, (j < m) ~ True) => a -> Matrix n m a -> Matrix n m a
set = set_ (sing :: SNat m) (sing :: SNat i) (sing :: SNat j)

set_ :: forall n i m j a. (SingI n, (i < n) ~ True, (j < m) ~ True) => SNat m -> SNat i -> SNat j -> a -> Matrix n m a -> Matrix n m a
set_ m i j x (Matrix v) = Matrix $ v // [(fromEnum (fromSing i * fromSing m + fromSing j), x)]

{- | The number of rows in a matrix.

=== __Examples__:

>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> rows m
2
-}
rows :: forall n m a. (SingI n) => Matrix n m a -> Int
rows _ = fromEnum $ fromSing (sing :: SNat n)

{- | The number of columns in a matrix.

=== __Examples__:

>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> cols m
2
-}
cols :: forall n m a. (SingI m) => Matrix n m a -> Int
cols _ = fromEnum $ fromSing (sing :: SNat m)

{- | The number of elements in a matrix.

=== __Examples__:

>>> let Just m = fromList [1, 2, 3, 4] :: Maybe (Matrix 2 2 Int)
>>> size m
4
-}
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

{- | Convenience matrix constructors
Could someday be created by Template Haskell.
-}
mat0x0 :: Matrix 0 0 a
mat1x1 :: a -> Matrix 1 1 a
mat1x2 :: a -> a -> Matrix 1 2 a
mat1x3 :: a -> a -> a -> Matrix 1 3 a
mat1x4 :: a -> a -> a -> a -> Matrix 1 4 a
mat1x5 :: a -> a -> a -> a -> a -> Matrix 1 5 a
mat1x6 :: a -> a -> a -> a -> a -> a -> Matrix 1 6 a
mat1x7 :: a -> a -> a -> a -> a -> a -> a -> Matrix 1 7 a
mat1x8 :: a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 8 a
mat1x9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 9 a
mat1x10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 10 a
mat1x11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 11 a
mat1x12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 12 a
mat1x13 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 13 a
mat1x14 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 14 a
mat1x15 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 15 a
mat1x16 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 16 a
mat1x17 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 17 a
mat1x18 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 18 a
mat1x19 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 19 a
mat1x20 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 20 a
mat1x21 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 21 a
mat1x22 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 22 a
mat1x23 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 23 a
mat1x24 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 24 a
mat1x25 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 25 a
mat1x26 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 26 a
mat1x27 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 27 a
mat1x28 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 28 a
mat1x29 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 29 a
mat1x30 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 30 a
mat1x31 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 31 a
mat1x32 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 1 32 a
mat0x0 = empty

mat1x1 = singleton
mat1x2 x1 x2 = fromArray $ A.arr2 x1 x2
mat1x3 x1 x2 x3 = fromArray $ A.arr3 x1 x2 x3
mat1x4 x1 x2 x3 x4 = fromArray $ A.arr4 x1 x2 x3 x4
mat1x5 x1 x2 x3 x4 x5 = fromArray $ A.arr5 x1 x2 x3 x4 x5
mat1x6 x1 x2 x3 x4 x5 x6 = fromArray $ A.arr6 x1 x2 x3 x4 x5 x6
mat1x7 x1 x2 x3 x4 x5 x6 x7 = fromArray $ A.arr7 x1 x2 x3 x4 x5 x6 x7
mat1x8 x1 x2 x3 x4 x5 x6 x7 x8 = fromArray $ A.arr8 x1 x2 x3 x4 x5 x6 x7 x8
mat1x9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = fromArray $ A.arr9 x1 x2 x3 x4 x5 x6 x7 x8 x9
mat1x10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = fromArray $ A.arr10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
mat1x11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = fromArray $ A.arr11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
mat1x12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = fromArray $ A.arr12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
mat1x13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 = fromArray $ A.arr13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
mat1x14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = fromArray $ A.arr14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
mat1x15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 = fromArray $ A.arr15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
mat1x16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 = fromArray $ A.arr16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
mat1x17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 = fromArray $ A.arr17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
mat1x18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 = fromArray $ A.arr18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18
mat1x19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 = fromArray $ A.arr19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
mat1x20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 = fromArray $ A.arr20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
mat1x21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 = fromArray $ A.arr21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21
mat1x22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 = fromArray $ A.arr22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22
mat1x23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 = fromArray $ A.arr23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23
mat1x24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 = fromArray $ A.arr24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24
mat1x25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 = fromArray $ A.arr25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25
mat1x26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 = fromArray $ A.arr26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
mat1x27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 = fromArray $ A.arr27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27
mat1x28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 = fromArray $ A.arr28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28
mat1x29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 = fromArray $ A.arr29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
mat1x30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 = fromArray $ A.arr30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
mat1x31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 = fromArray $ A.arr31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
mat1x32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 = fromArray $ A.arr32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32

-- Provided for further convenience
mat2x1 :: a -> a -> Matrix 2 1 a
mat2x2 :: a -> a -> a -> a -> Matrix 2 2 a
mat2x3 :: a -> a -> a -> a -> a -> a -> Matrix 2 3 a
mat3x1 :: a -> a -> a -> Matrix 3 1 a
mat3x2 :: a -> a -> a -> a -> a -> a -> Matrix 3 2 a
mat3x3 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix 3 3 a
mat2x1 x1 x2 = fromArray $ A.arr2 x1 x2
mat3x1 x1 x2 x3 = fromArray $ A.arr3 x1 x2 x3
mat2x2 x1 x2 x3 x4 = fromArray $ A.arr4 x1 x2 x3 x4
mat2x3 x1 x2 x3 x4 x5 x6 = fromArray $ A.arr6 x1 x2 x3 x4 x5 x6
mat3x2 x1 x2 x3 x4 x5 x6 = fromArray $ A.arr6 x1 x2 x3 x4 x5 x6
mat3x3 x1 x2 x3 x4 x5 x6 x7 x8 x9 = fromArray $ A.arr9 x1 x2 x3 x4 x5 x6 x7 x8 x9
