{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Array (
  Array,
  toList,
  toVector,
  fromVector,
  fromList,
  withVec,
  withList,
  append,
  split,
  index,
  cons,
  zipWith,
  replicate,
  Fin,
  empty, singleton,
  arr0, arr1, arr2, arr3, arr4, arr5, arr6, arr7, arr8, arr9, arr10, arr11, arr12, arr13, arr14, arr15, arr16, arr17, arr18, arr19, arr20, arr21, arr22, arr23, arr24, arr25, arr26, arr27, arr28, arr29, arr30, arr31, arr32
) where

import Data.Singletons
    ( withSingI,
      Sing,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing) )
import GHC.TypeLits.Singletons
import qualified GHC.TypeLits as TL
import Data.Singletons.TH
import Prelude.Singletons
import Prelude hiding (replicate, zipWith)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Fin (n :: Natural) where
  Top :: Fin n
  Pop :: Fin n -> Fin (n + 1)

deriving instance Show (Fin n)

-- toFin_ :: Sing n -> Int -> Maybe (Fin n)
-- toFin_ s i | 0 == i = Just Top
--            | 0 < i && i < l = Pop <$> toFin_ (sing :: Sing (n - 1)) (i - 1)
--           | otherwise = Nothing
--   where l = fromEnum (fromSing s)


-- toFin_ :: Sing n -> Int -> Maybe (Fin n)
-- toFin_ s i | 0 <= i && i < l = Just (UnsafeFin i)
--            | otherwise = Nothing
--   where l = fromEnum (fromSing s)

-- toFin :: SingI n => Int -> Maybe (Fin n)
-- toFin = toFin_ sing

-- instance SingI n => Num (Fin n) where
--   signum (UnsafeFin 0) = UnsafeFin 0
--   signum _             = UnsafeFin 1
--   abs = id
--   (+) (UnsafeFin a) (UnsafeFin b) = UnsafeFin ((a + b) `mod` l)
--     where l = fromEnum (fromSing (sing :: Sing n))
--   (*) (UnsafeFin a) (UnsafeFin b) = UnsafeFin ((a * b) `mod` l)
--     where l = fromEnum (fromSing (sing :: Sing n))
--   (-) (UnsafeFin a) (UnsafeFin b) = UnsafeFin ((a - b) `mod` l)
--     where l = fromEnum (fromSing (sing :: Sing n))
--   fromInteger n | 0 <= n && n < l = UnsafeFin (fromInteger n)
--                 | otherwise = error "negative Fin"
--                 where l = toInteger $ fromEnum (fromSing (sing :: Sing n))

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

internalTypeLength_ :: Sing n -> Array n a -> Int
internalTypeLength_ x _ = fromEnum $ fromSing x

-- | Gets the length of the array as represented by the type.
-- | This is an internal function.
internalTypeLength :: SingI n => Array n a -> Int
internalTypeLength = internalTypeLength_ sing

-- | Gets the length of the internal vector.
-- | This is an internal function.
internalValueLength :: Array n a -> Int
internalValueLength (Array v) =  V.length v

-- | Gets the length of the array.
-- | This function is equal to `internalValueLength` and should be equivalent to `internalTypeLength`.
length :: Array n a -> Int
length = internalValueLength

instance Functor (Array n) where
    fmap f (Array v) = Array (fmap f v)

instance SingI n => Applicative (Array n) where
  pure :: SingI n => a -> Array n a
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

cons :: SingI n => a -> Array n a -> Array (n + 1) a
cons a (Array v) = Array (V.cons a v)

zipWith :: SingI n => (a -> b -> c) -> Array n a -> Array n b -> Array n c
zipWith f (Array xs) (Array ys) = Array (V.zipWith f xs ys)

split :: SingI n => Array (n + m) a -> (Array n a, Array m a)
split = split_ sing

-- index_ :: Fin n -> Array n a -> a
-- index_ n v = (toVector v) V.! (toInt n)
--   where toInt :: Fin n -> Int
--         toInt Top = 0
--         toInt (Pop n) = 1 + toInt n

-- index2 :: Array n a -> Fin n -> a
-- index2 v n = toVector v V.! unFin n

index :: ((m <= n) ~ 'True, KnownNat m)  
        => Array n a -> proxy m -> a
index v m = toVector v V.! toInt m
  where toInt :: KnownNat n => proxy n -> Int
        toInt = fromInteger .  TL.natVal

replicate :: SingI n => a -> Array n a
replicate = pure


-- | Convenience Array constructors
-- These could be generated with Template Haskell
arr0 :: Array 0 a
arr0 = Array V.empty
empty :: Array 0 a
empty = arr0
arr1 :: a -> Array 1 a
arr1 x1 = Array $ V.fromList [x1] 
singleton :: a -> Array 1 a
singleton = arr1
arr2 :: a -> a -> Array 2 a
arr3 :: a -> a -> a -> Array 3 a
arr4 :: a -> a -> a -> a -> Array 4 a
arr5 :: a -> a -> a -> a -> a -> Array 5 a
arr6 :: a -> a -> a -> a -> a -> a -> Array 6 a
arr7 :: a -> a -> a -> a -> a -> a -> a -> Array 7 a
arr8 :: a -> a -> a -> a -> a -> a -> a -> a -> Array 8 a
arr9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 9 a
arr10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 10 a
arr11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 11 a
arr12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 12 a
arr13 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 13 a
arr14 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 14 a
arr15 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 15 a
arr16 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 16 a
arr17 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 17 a
arr18 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 18 a
arr19 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 19 a
arr20 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 20 a
arr21 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 21 a
arr22 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 22 a
arr23 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 23 a
arr24 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 24 a
arr25 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 25 a
arr26 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 26 a
arr27 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 27 a
arr28 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 28 a
arr29 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 29 a
arr30 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 30 a
arr31 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 31 a
arr32 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Array 32 a
arr2 x1 x2 = Array $ V.fromList [x1, x2]
arr3 x1 x2 x3 = Array . V.fromList $ [x1, x2, x3]
arr4 x1 x2 x3 x4 = Array . V.fromList $ [x1, x2, x3, x4]
arr5 x1 x2 x3 x4 x5 = Array . V.fromList $ [x1, x2, x3, x4, x5]
arr6 x1 x2 x3 x4 x5 x6 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6]
arr7 x1 x2 x3 x4 x5 x6 x7 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7]
arr8 x1 x2 x3 x4 x5 x6 x7 x8 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8]
arr9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9]
arr10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
arr11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]
arr12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]
arr13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]
arr14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14]
arr15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]
arr16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16]
arr17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17]
arr18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18]
arr19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19]
arr20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20]
arr21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21]
arr22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22]
arr23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23]
arr24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24]
arr25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25]
arr26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26]
arr27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27]
arr28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28]
arr29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29]
arr30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30]
arr31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31]
arr32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 = Array . V.fromList $ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32]

