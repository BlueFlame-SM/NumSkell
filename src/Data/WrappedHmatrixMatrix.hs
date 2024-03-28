{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-
 - This wrapper uses the non-static interface for hmatrix, but there is an
 - (experimental?) static interfaces that has "statically checked dimensions",
 - which looks like it does what we would want to do... so at what point even
 - wrap that?
 -
 - See:
 - https://hackage.haskell.org/package/hmatrix-0.20.2/docs/Numeric-LinearAlgebra-Static.html
 -}

module Data.WrappedHmatrixMatrix where

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

newtype Matrix n m a = Matrix { unMatrix :: Array m (Array n a) }

data SquareMatrix n a where
    Matrix0 :: forall n a. SquareMatrix n a
    MatrixN :: forall n a. (KnownNat n) => Matrix n n a -> SquareMatrix n a
    MatrixN' :: forall n a. (SingI n) => Matrix n n a -> SquareMatrix n a

empty :: Matrix 0 0 a
empty = Matrix A.empty

singleton :: a -> Matrix 1 1 a
singleton x = Matrix $ A.singleton (A.singleton x)

-- newtype Matrix (s :: Shape) where
--     Matrix :: (H.Element a, H.Indexable (H.Vector a) a)
--                 => H.Matrix a
--                 -> Matrix s a

-- index ::
-- (KnownNat x, KnownNat y, KnownNat n, KnownNat m,
-- (i <= n) ~ 'True, (j <= m) ~ 'True) =>
-- Matrix n m a -> proxy i -> proxy j -> a
-- index m i j = A.index (A.index m j) i

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
    Matrix f <*> Matrix x = Matrix $ A.zipWith ($) (A.zipWith ($) f x) _

instance (SingI n, SingI m, Num x) => Num (Matrix n m x) where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

-- idMatrix :: forall n a . (KnownNat n) => Proxy n -> a -> a -> SquareMatrix n a
-- idMatrix n = idMatrix' (natVal n)

-- idMatrix' :: Natural -> a -> a -> SquareMatrix n a
-- idMatrix' 0 zero one = Matrix0
-- idMatrix' n zero one = idMatrixHelp2 zero one (idMatrix' (n - 1) zero one)

-- idMatrixHelp2 :: forall n a . a -> a -> SquareMatrix n a -> SquareMatrix (n + 1) a
-- idMatrixHelp2 zero one Matrix0        = MatrixN' $ idMatrixHelp zero one empty
-- idMatrixHelp2 zero one (MatrixN  mat) = undefined $ idMatrixHelp zero one mat
-- idMatrixHelp2 zero one (MatrixN' mat) = undefined $ idMatrixHelp zero one mat

-- idMatrixHelp :: forall n a . (SingI n) => a -> a -> Matrix n n a -> Matrix (n + 1) (n + 1) a
-- idMatrixHelp zero one mat = A.cons fstRow otherRows
--         where fstRow :: Array (n + 1) a
--               fstRow = A.cons one (pure zero)
--               otherRows :: Matrix (n + 1) n a
--               otherRows = fmap (A.cons zero) mat

-- (!?) :: Matrix a -> Index -> Maybe a
-- m@(Matrix (cols, rows) hMatrix) !? i@(x, y)
--     | x < cols && y < rows = Just $ m !! i
--     | otherwise = Nothing

{- Examples -}
exampleA = A.arr3 (A.arr3 1 2 3) (A.arr3 4 5 6) (A.arr3 7 8 9)

exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))

-- b = Matrix (2, 3) $ (2 H.>< 3) [6, 5, 4, 3, 2, 1] :: Matrix H.C
