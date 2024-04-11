{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Matrix where

import Data.Array(Array)
import qualified Data.Array as A
import Data.List (intercalate)
import Data.Singletons (
    Sing,
    SingI (..),
    SingKind (fromSing, toSing),
    SomeSing (SomeSing),
    withSingI,
 )
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GHC.TypeLits.Singletons
import Prelude.Singletons

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))
import qualified Prelude as P

type Shape = (Int, Int)
type Index = (Int, Int)

-- type Shape = (Natural, Natural)

newtype Matrix (n :: Natural) (m :: Natural) a = Matrix {toVector :: Vector a}
    deriving (Eq, Ord)

empty :: Matrix 0 0 a
empty = Matrix V.empty

singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ V.singleton x

index_ :: (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True) => Sing n -> Sing m -> Matrix n m a -> Proxy i -> Proxy j -> a
index_ n m (Matrix v) i j = v ! (i' * m' + j')
  where
    i' = fromIntegral $ natVal i
    j' = fromIntegral $ natVal j
    m' = fromIntegral $ fromSing m

index :: forall n i m j a. (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True, SingI n, SingI m) => Matrix n m a -> Proxy i -> Proxy j -> a
index = index_ (sing :: Sing n) (sing :: Sing m)

replicate_ :: Sing n -> Sing m -> a -> Matrix n m a
replicate_ n m = Matrix . V.replicate (fromIntegral $ fromSing n * fromSing m)

replicate :: forall n m a. (SingI n, SingI m) => a -> Matrix n m a
replicate = replicate_ (sing :: Sing n) (sing :: Sing m)

zipWith :: (SingI n, SingI m) => (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
zipWith f (Matrix x) (Matrix y) = Matrix $ V.zipWith f x y

instance (SingI n, SingI m) => Functor (Matrix n m) where
    fmap f (Matrix xs) = Matrix $ fmap f xs

instance (SingI n, SingI m) => Applicative (Matrix n m) where
    pure = replicate
    (<*>) = zipWith ($)

matrixMult ::
    forall n m k a.
    (SingI n, SingI k, SingI m, Num a) =>
    Matrix n m a ->
    Matrix m k a ->
    Matrix n k a
matrixMult = matrixMult_ (sing :: Sing n) (sing :: Sing m) (sing :: Sing k)

matrixMult_ :: (Num a) => Sing n -> Sing m -> Sing p -> Matrix n m a -> Matrix m p a -> Matrix n p a
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

instance (SingI n, SingI m, Num x) => Num (Matrix n m x) where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (SingI n, SingI m) => Foldable (Matrix n m) where
    foldMap f (Matrix v) = foldMap f v

instance (SingI n, SingI m) => Traversable (Matrix n m) where
    traverse f (Matrix v) = fmap Matrix (traverse f v)

instance (SingI n, SingI m, Semigroup a) => Semigroup (Matrix n m a) where
    (<>) = zipWith (<>)

instance (SingI n, SingI m, Monoid a) => Monoid (Matrix n m a) where
    mempty = replicate mempty

showMatrix_ :: (Show a) => Sing n -> Sing m -> Matrix n m a -> String
showMatrix_ n m (Matrix v) = "[" ++ intercalate "\n " (map showRow [0 .. n' - 1]) ++ "]"
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m
    showRow i = "[" ++ unwords (V.toList $ show <$> V.slice (i * m') m' v) ++ "]"

showMatrix :: forall n m a. (SingI n, SingI m, Show a) => Matrix n m a -> String
showMatrix (Matrix v) = showMatrix_ (sing :: Sing n) (sing :: Sing m) (Matrix v)

instance (SingI n, SingI m, Show a) => Show (Matrix n m a) where
    show = showMatrix

reshape :: (SingI n, SingI m, SingI k, SingI l, n * m ~ k * l) => Matrix n m a -> Matrix k l a
reshape (Matrix v) = Matrix v

idMatrix_ :: (Num a) => Sing n -> Sing m -> Matrix n m a
idMatrix_ n m = Matrix $ V.fromList $ [if i == j then 1 else 0 | i <- [1 .. n'], j <- [1 .. m']]
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

idMatrix :: forall n m a. (Num a, SingI n, SingI m) => Matrix n m a
idMatrix = idMatrix_ (sing :: Sing n) (sing :: Sing m)

fromVector_ :: Sing n -> Sing m -> Vector a -> Maybe (Matrix n m a)
fromVector_ n m v
    | n' * m' == V.length v = Just $ Matrix v
    | otherwise = Nothing
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

fromVector :: forall n m a. (SingI n, SingI m) => Vector a -> Maybe (Matrix n m a)
fromVector = fromVector_ (sing :: Sing n) (sing :: Sing m)

withVecAsVec_ :: Vector a -> (forall m. Sing m -> Matrix 1 m a -> b) -> b
withVecAsVec_ v f = case toSing (fromIntegral (V.length v)) of
    SomeSing (s :: Sing q) -> f s (Matrix v)

withVecAsVec :: Vector a -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withVecAsVec v f = withVecAsVec_ v (\s a -> withSingI s (f a))

withListAsVec :: [a] -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withListAsVec l = withVecAsVec (V.fromList l)

withVec_ :: Vector (Vector a) -> (forall n m. Sing n -> Sing m -> Matrix n m a -> b) -> b
withVec_ v f =
    let
        n = V.length v
        m = V.foldl' min 0 (V.map V.length v)
        truncatedV = foldMap (V.slice 0 m) v
     in
        case (toSing $ fromIntegral n, toSing $ fromIntegral m) of
            (SomeSing (s :: Sing q), SomeSing (t :: Sing r)) -> f s t (Matrix truncatedV)

withVec :: Vector (Vector a) -> (forall n m. (SingI n, SingI m) => Matrix n m a -> b) -> b
withVec v f = withVec_ v (\s t a -> withSingI s (withSingI t (f a)))

withList :: [[a]] -> (forall n m. (SingI n, SingI m) => Matrix n m a -> b) -> b
withList l = withVec (V.fromList (map V.fromList l))

fromList :: (SingI n, SingI m) => [a] -> Maybe (Matrix n m a)
fromList = fromVector . V.fromList

fromArray_ :: Array n a -> Matrix 1 n a
fromArray_ x = Matrix (A.toVector x)

fromArray :: (k ~ n * m) => Array k a -> Matrix n m a
fromArray x = Matrix (A.toVector x)

-- | Convenience matrix constructors
-- Could someday be created by Template Haskell.
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
mat0x0  = empty
mat1x1  = singleton
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
