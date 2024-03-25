{-# LANGUAGE GADTs #-}
module Data.NdArray where

import Data.Vector (Vector)
import qualified Data.Vector as V


-- Example NdArray, but I think it's better we just wrap hmatrix

type Shape = [Int]
type Index = [Int]  -- Could be more complex, e.g. range selection

data NdArray a where
  NdArray :: { shape :: Shape, vector :: Vector a } -> NdArray a


convertIndex :: Shape -> Index -> Int
convertIndex sh i = sum $ zipWith (*) i (scanl (*) 1 (tail sh))  -- TODO: Maybe use modulo? error?

checkIndex :: Shape -> Index -> Bool
checkIndex sh i = length sh == length i && and (zipWith (<) i sh)

(!!) :: NdArray a -> Index -> a
NdArray sh v !! i = v V.! i'
  where i' = convertIndex sh i


(!?) :: NdArray a -> Index -> Maybe a
NdArray sh v !? i | checkIndex sh i = Just (v V.! i')
                  | otherwise       = Nothing
  where i' = convertIndex sh i
