{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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




-- (!?) :: Matrix a -> Index -> Maybe a
-- m@(Matrix (cols, rows) hMatrix) !? i@(x, y)
--     | x < cols && y < rows = Just $ m !! i
--     | otherwise = Nothing

{- Examples -}
exampleA = A.arr3 (A.arr3 1 2 3) (A.arr3 4 5 6) (A.arr3 7 8 9)

exampleIndexRow = (indexRow exampleA (Proxy :: Proxy 1))
-- b = Matrix (2, 3) $ (2 H.>< 3) [6, 5, 4, 3, 2, 1] :: Matrix H.C
