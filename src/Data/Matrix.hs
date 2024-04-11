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

module Data.Matrix where

import Data.List (intercalate)
import Data.Singletons
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GHC.TypeLits.Singletons
import Prelude.Singletons

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))
import qualified Prelude as P

newtype Matrix (n :: Natural) (m :: Natural) a = Matrix {toVector :: Vector a}
    deriving (Eq, Ord)

fromVector_ :: Sing n -> Sing m -> Vector a -> Maybe (Matrix n m a)
fromVector_ n m v
    | n' * m' == V.length v = Just $ Matrix v
    | otherwise = Nothing
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m

fromVector :: forall n m a. (SingI n, SingI m) => Vector a -> Maybe (Matrix n m a)
fromVector = fromVector_ (sing :: Sing n) (sing :: Sing m)

fromList :: forall n m a. (SingI n, SingI m) => [a] -> Maybe (Matrix n m a)
fromList l = fromVector (V.fromList l)

withVecAsVec_ :: Vector a -> (forall m. Sing m -> Matrix 1 m a -> b) -> b
withVecAsVec_ v f = case toSing (fromIntegral (V.length v)) of
    SomeSing (s :: Sing q) -> f s (Matrix v)

withVecAsVec :: Vector a -> (forall m. (SingI m) => Matrix 1 m a -> b) -> b
withVecAsVec v f = withVecAsVec_ v (\s a -> withSingI s (f a))

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
replicate = replicate_ (sing :: Sing n) (sing :: Sing m)

replicate_ :: Sing n -> Sing m -> a -> Matrix n m a
replicate_ n m = Matrix . V.replicate (fromIntegral $ fromSing n * fromSing m)

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
idMatrix = idMatrix_ (sing :: Sing n) (sing :: Sing m)

idMatrix_ :: (Num a) => Sing n -> Sing m -> Matrix n m a
idMatrix_ n m = Matrix $ V.fromList $ [if i == j then 1 else 0 | i <- [1 .. n'], j <- [1 .. m']]
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
-- >>> let Just m1 = fromList [[1, 2], [3, 4]] :: Maybe (Matrix 2 2 Int)
-- >>> let Just m2 = fromList [[5, 6], [7, 8]] :: Maybe (Matrix 2 2 Int)
-- >>> matrixMult m1 m2
-- Variable not in scope:
--   fromList :: [[a0_a7oe5[tau:1]]] -> Maybe (Matrix 2 2 Int)
matrixMult :: forall n m k a . (SingI n, SingI k, SingI m, Num a)
            => Matrix n m a -> Matrix m k a -> Matrix n k a
matrixMult = matrixMult_ (sing :: Sing n) (sing :: Sing m) (sing :: Sing k)

matrixMult_ :: (Num a) => Sing n -> Sing m -> Sing p -> Matrix n m a -> Matrix m p a -> Matrix n p a
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

reshape :: (SingI n, SingI m, SingI k, SingI l, n * m ~ k * l) => Matrix n m a -> Matrix k l a
reshape (Matrix v) = Matrix v

index_ :: (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True) => Sing n -> Sing m -> Matrix n m a -> Proxy i -> Proxy j -> a
index_ n m (Matrix v) i j = v ! (i' * m' + j')
  where
    i' = fromIntegral $ natVal i
    j' = fromIntegral $ natVal j
    m' = fromIntegral $ fromSing m

index :: forall n i m j a. (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True, SingI n, SingI m) => Matrix n m a -> Proxy i -> Proxy j -> a
index = index_ (sing :: Sing n) (sing :: Sing m)


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
