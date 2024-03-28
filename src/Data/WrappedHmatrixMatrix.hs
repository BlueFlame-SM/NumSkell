{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Singletons
    ( withSingI,
      Sing,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing) )
import GHC.TypeLits.Singletons
import Prelude.Singletons
import Data.Array (Array)
import qualified Data.Array as A

-- import qualified Numeric.LinearAlgebra as H
import Prelude hiding ((!!))

type Shape = (Int, Int)
type Index = (Int, Int)

-- type Shape = (Natural, Natural)

type Matrix n m a = Array m (Array n a)

data AnyMatrix a where
  Matrix0 :: forall a . AnyMatrix a
  MatrixN :: forall n a . KnownNat n => Matrix n n a -> AnyMatrix a
  MatrixN' :: forall n a . SingI n => Matrix n n a -> AnyMatrix a


empty :: Matrix 0 0 a
empty = A.empty

singleton :: a -> Matrix 1 1 a
singleton x = A.singleton (A.singleton x)

-- newtype Matrix (s :: Shape) where
--     Matrix :: (H.Element a, H.Indexable (H.Vector a) a)
--                 => H.Matrix a
--                 -> Matrix s a

-- index :: 
            -- (KnownNat x, KnownNat y, KnownNat n, KnownNat m,
            -- (i <= n) ~ 'True, (j <= m) ~ 'True) =>  
            -- Matrix n m a -> proxy i -> proxy j -> a
-- index m i j = A.index (A.index m j) i

indexRow :: (KnownNat i, (i <= m) ~ 'True) =>
            Matrix n m a -> proxy i -> Array n a
indexRow = A.index

index :: (KnownNat m', KnownNat n', (m' <= m) ~ 'True, (n' <= n) ~ 'True) =>
        Matrix n m a -> proxy m' -> proxy n' -> a
index mat m n = A.index (A.index mat m ) n

replicate :: (SingI n, SingI m) => a -> Matrix n m a
replicate = pure . pure

idMatrix :: forall n a . (KnownNat n) => Proxy n -> a -> a -> AnyMatrix a
idMatrix n = idMatrix' (natVal n)

idMatrix' :: Natural -> a -> a -> AnyMatrix a
idMatrix' 0 zero one = Matrix0
idMatrix' n zero one = idMatrixHelp2 zero one (idMatrix' (n - 1) zero one)

idMatrixHelp2 :: forall n a . a -> a -> AnyMatrix a -> AnyMatrix a
idMatrixHelp2 zero one Matrix0        = MatrixN' $ idMatrixHelp zero one empty
idMatrixHelp2 zero one (MatrixN  mat) = MatrixN' $ idMatrixHelp zero one mat
idMatrixHelp2 zero one (MatrixN' mat) = MatrixN' $ idMatrixHelp zero one mat

idMatrixHelp :: forall n a . (SingI n, KnownNat (n + 1)) => a -> a -> Matrix n n a -> Matrix (n + 1) (n + 1) a
idMatrixHelp zero one mat = A.cons fstRow otherRows
        where fstRow :: Array (n + 1) a
              fstRow = A.cons one (pure zero)
              otherRows :: Matrix (n + 1) n a
              otherRows = fmap (A.cons zero) mat


-- (!?) :: Matrix a -> Index -> Maybe a
-- m@(Matrix (cols, rows) hMatrix) !? i@(x, y)
--     | x < cols && y < rows = Just $ m !! i
--     | otherwise = Nothing

{- Examples -}
exampleA = A.arr3 (A.arr3 1 2 3) (A.arr3 4 5 6) (A.arr3 7 8 9)

exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))
-- b = Matrix (2, 3) $ (2 H.>< 3) [6, 5, 4, 3, 2, 1] :: Matrix H.C
