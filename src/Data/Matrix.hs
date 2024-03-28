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
import Data.Singletons (
    Sing,
    SingI (..),
    SingKind (fromSing, toSing),
    SomeSing (SomeSing),
    withSingI,
 )
import GHC.TypeLits.Singletons
import Prelude.Singletons
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.List (intercalate)

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))

type Shape = (Int, Int)
type Index = (Int, Int)

-- type Shape = (Natural, Natural)

newtype Matrix (n :: Natural) (m :: Natural) a = Matrix { toVector :: Vector a }
    deriving (Eq, Ord)

empty :: Matrix 0 0 a
empty = Matrix V.empty

singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ V.singleton x

index_ :: (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True) =>  Sing n -> Sing m -> Matrix n m a -> Proxy i -> Proxy j -> a
index_ n m (Matrix v) i j = v ! (i' * m' + j')
  where
    i' = fromIntegral $ natVal i
    j' = fromIntegral $ natVal j
    m' = fromIntegral $ fromSing m

index :: forall n i m j a . (KnownNat i, KnownNat j, (j <= m) ~ True, (i <= n) ~ True, SingI n, SingI m) => Matrix n m a -> Proxy i -> Proxy j -> a
index = index_ (sing :: Sing n) (sing :: Sing m)

replicate_ :: Sing n -> Sing m -> a -> Matrix n m a
replicate_ n m = Matrix . V.replicate (fromIntegral $ fromSing n * fromSing m)

replicate :: forall n m a . (SingI n, SingI m) => a -> Matrix n m a
replicate = replicate_ (sing :: Sing n) (sing :: Sing m)

zipWith :: (SingI n, SingI m) => (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
zipWith f (Matrix x) (Matrix y) = Matrix $ V.zipWith f x y

instance (SingI n, SingI m) => Functor (Matrix n m) where
    fmap f (Matrix xs) = Matrix $ fmap f xs

instance (SingI n, SingI m) => Applicative (Matrix n m) where
    pure = replicate
    (<*>) = zipWith ($)

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
showMatrix_ n m (Matrix v) = "[" ++ intercalate "\n " (map showRow [0..n'-1]) ++ "]"
  where
    n' = fromIntegral $ fromSing n
    m' = fromIntegral $ fromSing m
    showRow i = "[" ++ unwords (V.toList $ show <$> V.slice (i * m') m' v) ++ "]"

showMatrix :: forall n m a . (SingI n, SingI m, Show a) => Matrix n m a -> String
showMatrix (Matrix v) = showMatrix_ (sing :: Sing n) (sing :: Sing m) (Matrix v)

instance (SingI n, SingI m, Show a) => Show (Matrix n m a) where
    show = showMatrix

reshape :: (SingI n, SingI m, SingI k, SingI l, n * m ~ k * l) => Matrix n m a -> Matrix k l a
reshape (Matrix v) = Matrix v

exampleA :: Matrix 2 2 Int
exampleA = Matrix $ V.fromList [1, 2, 3, 4]

exampleB :: Matrix 2 3 Int
exampleB = Matrix $ V.fromList [1, 2, 3, 4, 5, 6]


-- exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))
