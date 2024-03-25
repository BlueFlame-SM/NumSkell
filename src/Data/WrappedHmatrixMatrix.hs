{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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

import qualified Numeric.LinearAlgebra as H
import Prelude hiding ((!!))

type Shape = (Int, Int)
type Index = (Int, Int)

data Matrix a where
    Matrix :: (H.Element a, H.Indexable (H.Vector a) a) => {shape :: Shape, hMatrix :: H.Matrix a} -> Matrix a

(!!) :: Matrix a -> Index -> a
Matrix _ hMatrix !! (x, y) = hMatrix H.! y H.! x

(!?) :: Matrix a -> Index -> Maybe a
m@(Matrix (cols, rows) hMatrix) !? i@(x, y)
    | x < cols && y < rows = Just $ m !! i
    | otherwise = Nothing

{- Examples -}
a = Matrix (2, 3) $ (2 H.>< 3) [1, 2, 3, 4, 5, 6] :: Matrix H.R
b = Matrix (2, 3) $ (2 H.>< 3) [6, 5, 4, 3, 2, 1] :: Matrix H.C
