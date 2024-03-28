{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
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

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding (replicate, zipWith, (!!))

type Shape = (Int, Int)
type Index = (Int, Int)

-- type Shape = (Natural, Natural)

newtype Matrix n m a = Matrix {unMatrix :: Array m (Array n a)}

data SquareMatrix n a where
    Matrix0 :: forall n a. SquareMatrix n a
    MatrixN :: forall n a. (KnownNat n) => Matrix n n a -> SquareMatrix n a
    MatrixN' :: forall n a. (SingI n) => Matrix n n a -> SquareMatrix n a

empty :: Matrix 0 0 a
empty = Matrix A.empty

singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ A.singleton (A.singleton x)

indexRow ::
    (KnownNat i, (i <= m) ~ 'True) =>
    Matrix n m a ->
    proxy i ->
    Array n a
indexRow = A.index . unMatrix

index ::
    (KnownNat m', KnownNat n', (m' <= m) ~ 'True, (n' <= n) ~ 'True) =>
    Matrix n m a ->
    proxy m' ->
    proxy n' ->
    a
index mat m = A.index (A.index (unMatrix mat) m)

replicate :: (SingI n, SingI m) => a -> Matrix n m a
replicate = Matrix . pure . pure

zipWith :: (SingI n, SingI m) => (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
zipWith f (Matrix x) (Matrix y) = Matrix $ A.zipWith (A.zipWith f) x y

instance (SingI n, SingI m) => Functor (Matrix n m) where
    fmap f (Matrix xs) = Matrix $ fmap (fmap f) xs

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

exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))
