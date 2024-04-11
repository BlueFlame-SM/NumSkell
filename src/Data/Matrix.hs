{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Matrix where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List (intercalate)
import Data.Singletons
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

exampleA :: Matrix 2 2 Int
exampleA = Matrix $ V.fromList [1, 2, 3, 4]

exampleB :: Matrix 2 3 Int
exampleB = Matrix $ V.fromList [1, 2, 3, 4, 5, 6]


-- exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))
exampleC :: Matrix 3 2 Int
exampleC = Matrix $ V.fromList [10, 11, 20, 21, 30, 31]

exampleD :: Matrix 2 1 Int
exampleD = Matrix $ V.fromList [2, 3]

-- Should be [[140, 146], [320, 335]]
matrixMultExample :: Matrix 2 2 Int
matrixMultExample = matrixMult exampleB exampleC

matrixMultExample2 :: Matrix 3 1 Int -- Should be [[2*10 + 3*11], [2 * 20 + 3 * 21], [2 * 30 + 3 * 31]]
                                    -- = [[53], [103],[153]]
matrixMultExample2 = matrixMult exampleC exampleD
