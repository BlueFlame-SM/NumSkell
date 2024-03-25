{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array (
  Array,
  toList,
  fromVector,
  fromList,
  withVec,
  withList,
  append,
  split,
  index,
  arr1, arr2, arr3, arr4, arr5, arr6, arr7, arr8
) where

import Data.Singletons
    ( withSingI,
      Sing,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing) )
import GHC.TypeLits.Singletons
import qualified GHC.TypeLits as TL
import Prelude.Singletons

import Data.Vector (Vector, (!))
import qualified Data.Vector as V


newtype Array (n :: Natural) a where
  Array :: { toVector :: Vector a } -> Array n a

instance Show a => Show (Array n a) where
  show = show . toList

instance Eq a => Eq (Array n a) where
  a == b = toVector a == toVector b

instance Ord a => Ord (Array n a) where
  compare a b = compare (toVector a) (toVector b)

toList :: Array n a -> [a]
toList = V.toList . toVector

fromVector_ :: Sing n -> Vector a -> Maybe (Array n a)
fromVector_ s v | V.length v == l = Just (Array v)
                | otherwise       = Nothing
  where l = fromEnum (fromSing s)

fromVector :: SingI n => Vector a -> Maybe (Array n a)
fromVector = fromVector_ sing

fromList :: SingI n => [a] -> Maybe (Array n a)
fromList = fromVector . V.fromList

withVec_ :: Vector a -> (forall n. Sing n -> Array n a -> b) -> b
withVec_ v f = case toSing (fromIntegral (V.length v)) of
  SomeSing (s :: Sing m) -> f s (Array v)

withVec :: Vector a -> (forall n. SingI n => Array n a -> b) -> b
withVec v f = withVec_ v (\s a -> withSingI s (f a))

withList :: [a] -> (forall n. SingI n => Array n a -> b) -> b
withList l = withVec (V.fromList l)

instance Functor (Array n) where
  fmap f (Array v) = Array (fmap f v)

instance SingI n => Applicative (Array n) where
  pure a = Array (V.replicate l a)
    where l = fromEnum (fromSing (sing :: Sing n))
  Array f <*> Array x = Array (V.zipWith ($) f x)

instance SingI n => Monad (Array n) where
  Array v >>= f = Array (V.imap (\i a -> toVector (f a) ! i) v)

instance Foldable (Array n) where
  foldMap f (Array v) = foldMap f v

instance Traversable (Array n) where
  traverse f (Array v) = fmap Array (traverse f v)

instance (Semigroup a, SingI n) => Semigroup (Array n a) where
  Array a <> Array b = Array (V.zipWith (<>) a b)

instance (Monoid a, SingI n) => Monoid (Array n a) where
  mempty = Array (V.replicate l mempty)
    where l = fromEnum (fromSing (sing :: Sing n))

append :: Array n a -> Array m a -> Array (n + m) a
append (Array a) (Array b) = Array (a <> b)

split_ :: Sing n -> Array (n + m) a -> (Array n a, Array m a)
split_ s v = (Array (V.take l (toVector v)), Array (V.drop l (toVector v)))
  where l = fromEnum (fromSing s)

split :: SingI n => Array (n + m) a -> (Array n a, Array m a)
split = split_ sing


index_ :: (KnownNat m, (m <= n) ~ 'True) => Sing m -> Array n a -> a
index_ n v = (toVector v) V.! (fromEnum (fromSing n))

index :: ((m <= n) ~ 'True, KnownNat m)  
        => Array n a -> proxy m -> a
index v m = (toVector v) V.! (toInt m)
  where toInt :: KnownNat n => proxy n -> Int
        toInt = fromInteger .  (TL.natVal)

-- Note: When giving the type of an array, the type
-- smallConstructors
arr1 :: a -> Array 1 a
arr1 a = Array $ V.fromList [a]

arr2 :: a -> a -> Array 2 a
arr2 a b = Array $ V.fromList [a, b]

arr3 :: a -> a -> a -> Array 3 a
arr3 a b c = Array $ V.fromList $ [a, b, c]

arr4 :: a -> a -> a -> a -> Array 4 a
arr4 a b c d = Array $ V.fromList $ [a, b, c, d]

arr5 :: a -> a -> a -> a -> a -> Array 5 a
arr5 a b c d e = Array $ V.fromList $ [a, b, c, d, e]

arr6 :: a -> a -> a -> a -> a -> a -> Array 6 a
arr6 a b c d e f = Array $ V.fromList $ [a, b, c, d, e, f]

arr7 :: a -> a -> a -> a -> a -> a -> a -> Array 7 a
arr7 a b c d e f g = Array $ V.fromList $ [a, b, c, d, e, f, g]

arr8 :: a -> a -> a -> a -> a -> a -> a -> a -> Array 8 a
arr8 a b c d e f g h = Array $ V.fromList $ [a, b, c, d, e, f, g, h]

oneTwoThree :: Num a => Array 3 a
oneTwoThree = Array (V.fromList [1, 2, 3])

testIndex :: Integer
testIndex = index oneTwoThree (Proxy :: Proxy 1)

testIndexFails :: Integer
testIndexFails = index oneTwoThree (Proxy :: Proxy 1)

fourFiveSix :: Num a => Array 3 a
fourFiveSix = Array (V.fromList [4, 5, 6])

testAppend :: Num a => Array 6 a
testAppend = oneTwoThree `append` fourFiveSix

testSplit :: Num a => (Array 2 a, Array 4 a)
testSplit = split (oneTwoThree `append` fourFiveSix)
