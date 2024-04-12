-- |
-- Module      :  Data.Array
-- Description :  Provides a fixed-size array type with type-level size constraints.
--
-- This module provides a fixed-size array type with type-level size constraints.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Array (
    -- * Array type
    Array,
    -- ** Constructors
    fromVector,
    fromList,
    withVec,
    withList,
    replicate,
    empty,
    singleton,
    -- ** Operations
    append,
    split,
    cons,
    zipWith,
    index,
    length,
    toList,
    toVector,
    -- * Convenience Array constructors
    arr0, arr1, arr2, arr3, arr4, arr5, arr6, arr7, arr8, arr9, arr10, arr11,
    arr12, arr13, arr14, arr15, arr16, arr17, arr18, arr19, arr20, arr21, arr22,
    arr23, arr24, arr25, arr26, arr27, arr28, arr29, arr30, arr31, arr32
) where


import Prelude.Singletons
    ( withSingI,
      Sing,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing),
      POrd(type (<)),
      PNum(type (+)) )
import GHC.TypeLits.Singletons ( Natural, SNat )

import Prelude hiding (replicate, zipWith, length)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- | An array of a specific size.
--
-- An array is a fixed-size collection of elements of the same type.
-- The size of the array is determined by the type-level natural number n.
--
-- To construct an array, use one of 'fromVector', 'fromList', 'withVec', 'withList', 'replicate', or 'singleton'.
newtype Array (n :: Natural) a
    = Array {
        -- | Convert an array to a 'Vector'.
        toVector :: Vector a
    }

-- | Convert an array to a list.
toList :: Array n a -> [a]
toList = V.toList . toVector

fromVector_ :: forall n a . SNat n -> Vector a -> Maybe (Array n a)
fromVector_ sn xs | V.length xs == n = Just (Array xs)
                  | otherwise        = Nothing
  where n = fromEnum (fromSing sn)

-- | Convert a 'Vector' to an array of a specific size.
--
-- The resulting array will have the same length as the input 'Vector',
-- but its size will be constrained to the specified type-level natural number n.
--
-- If the length of the input 'Vector' is not equal to n, 'Nothing' is returned.
--
-- === ___Example:___
-- >>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Array 4 Int)
-- Just [1,2,3,4]
--
-- >>> fromVector (V.fromList [1, 2, 3, 4]) :: Maybe (Array 3 Int)
-- Nothing
fromVector :: forall n a . SingI n
           => Vector a  -- ^ The input 'Vector'
           -> Maybe (Array n a)  -- ^ The resulting array of size n
fromVector = fromVector_ (sing :: SNat n)

-- | Convert a list to an array of a specific size.
--
-- The same as 'fromVector', but takes a list instead of a 'Vector'.
fromList :: forall n a . SingI n
         => [a]  -- ^ The input list
         -> Maybe (Array n a)  -- ^ The resulting array of size n
fromList = fromVector . V.fromList

withVec_ :: forall a b . Vector a -> (forall n. Sing n -> Array n a -> b) -> b
withVec_ v f = case toSing $ fromIntegral $ V.length v of
  SomeSing (s :: Sing m) -> f s (Array v)

-- | Applies a function to an array of any size.
--
-- This function takes a 'Vector' of any size, converts it to an array of the
-- appropriate size, and applies the given function to it. The function is polymorphic
-- in the size of the array. The result of the function should __not__ depend on the size
-- of the array.
--
-- This function is useful when you want to apply a function to an array of any size,
-- but the function itself does not depend on the size of the array.
--
-- === __Example:__
-- >>> withVec (V.fromList [1, 2, 3, 4]) $ \arr -> toList arr
-- [1,2,3,4]
withVec :: Vector a  -- ^ The input 'Vector'
        -> (forall n. SingI n => Array n a -> b)  -- ^ A function that takes an array of any size
        -> b  -- ^ The result of applying the function to the array
withVec v f = withVec_ v (\s a -> withSingI s (f a))

-- | Applies a function to an array of a specific size.
--
-- The same as 'withVec', but it takes a list instead of a 'Vector'.
withList :: [a] -> (forall n. SingI n => Array n a -> b) -> b
withList l = withVec (V.fromList l)

-- | Create an array with all elements initialized to the given value.
--
-- This function is similar to 'replicate' from the 'Data.Vector' module, but it creates an array
-- instead of a 'Vector'. The size of the array is determined by the type-level natural number n.
--
-- === ___Example:___
-- >>> replicate 0 :: Array 4 Int
-- [0,0,0,0]
replicate :: forall n a . SingI n => a -> Array n a
replicate x = Array (V.replicate len x)
  where len = fromIntegral (fromSing (sing :: SNat n))

-- | Gets the length of the array as represented by the type.
length :: forall n a . SingI n => Array n a -> Int
length _ = fromEnum $ fromSing (sing :: SNat n)

-- | Create an empty array.
--
-- This function is similar to 'empty' from the 'Data.Vector' module, but it creates an array
-- instead of a 'Vector'.
--
-- === ___Example:___
-- >>> empty :: Array 0 Int
-- []
empty :: Array 0 a
empty = Array V.empty

-- | Create an array of length 1 with the given value.
--
-- This function is similar to 'singleton' from the 'Data.Vector' module, but it creates an array
-- instead of a 'Vector'.
--
-- === ___Example:___
-- >>> singleton 42 :: Array 1 Int
-- [42]
singleton :: a  -- ^ The value to replicate
          -> Array 1 a  -- ^ The resulting array of length 1
singleton x = Array (V.singleton x)


-- | Concatenates two arrays.
--
-- Given two arrays, `append` concatenates them into a single array.
-- The resulting array has a length equal to the sum of the lengths of the input arrays.
--
-- === __Example:__
-- >>> let Just arr1 = fromList [1, 2, 3] :: Maybe (Array 3 Int)
-- >>> let Just arr2 = fromList [4, 5, 6] :: Maybe (Array 3 Int)
-- >>> append arr1 arr2
-- [1,2,3,4,5,6]
--
-- >>> :t append arr1 arr2
-- append arr1 arr2 :: Array 6 Int
append :: Array n a  -- ^ The first array
       -> Array m a  -- ^ The second array
       -> Array (n + m) a  -- ^ The concatenated array
append (Array a) (Array b) = Array (a <> b)


-- | Splits an array of size (n + m) into two arrays of size n and m, respectively.
--
-- Given an array of size (n + m), `split` splits the array into two arrays.
-- The resulting arrays have lengths n and m, respectively.
--
-- === __Example:__
-- >>> let Just arr = fromList [1, 2, 3, 4, 5, 6] :: Maybe (Array 6 Int)
-- >>> split arr :: (Array 3 Int, Array 3 Int)
-- ([1,2,3],[4,5,6])
--
-- >>> split arr :: (Array 4 Int, Array 2 Int)
-- ([1,2,3,4],[5,6])
split :: forall n m a . SingI n
      => Array (n + m) a  -- ^ The array to split
      -> (Array n a, Array m a)  -- ^ The resulting arrays
split = split_ (sing :: SNat n)

split_ :: Sing n -> Array (n + m) a -> (Array n a, Array m a)
split_ s v = (Array (V.take l (toVector v)), Array (V.drop l (toVector v)))
  where l = fromEnum (fromSing s)

-- | Prepend an element to an array.
--
-- Given an element and an array, `cons` prepends the element to the array.
-- The resulting array has a length one greater than the input array.
--
-- === __Example:__
-- >>> let Just arr = fromList [2, 3, 4] :: Maybe (Array 3 Int)
-- >>> cons 1 arr
-- [1,2,3,4]
--
-- >>> :t cons 1 arr
-- cons 1 arr :: Array 4 Int
cons :: a  -- ^ The element to prepend
     -> Array n a  -- ^ The array to prepend to
     -> Array (n + 1) a  -- ^ The resulting array
cons a (Array v) = Array (V.cons a v)


-- | Elementwise union of two arrays using a binary function.
--
-- Given two arrays and a binary function, `zipWith` applies the function elementwise to the arrays.
-- The resulting array has the same length as the input arrays.
--
-- === __Example:__
-- >>> let Just arr1 = fromList [1, 2, 3] :: Maybe (Array 3 Int)
-- >>> let Just arr2 = fromList [4, 5, 6] :: Maybe (Array 3 Int)
-- >>> zipWith (+) arr1 arr2
-- [5,7,9]
--
-- >>> :t zipWith (==) arr1 arr2
-- zipWith (==) arr1 arr2 :: Array 3 Bool
zipWith :: (a -> b -> c)  -- ^ The binary function
        -> Array n a  -- ^ The first array
        -> Array n b  -- ^ The second array
        -> Array n c  -- ^ The resulting array
zipWith f (Array xs) (Array ys) = Array (V.zipWith f xs ys)

-- | Index into an array using a type-level natural number.
--
-- Given an array and a type-level natural number, `index` returns the element at the specified index.
-- Note that the index must be provided at compile time.
--
-- === __Example:__
-- >>> let Just arr = fromList [1, 2, 3, 4] :: Maybe (Array 4 Int)
-- >>> index (SNat :: SNat 2) arr
-- 3
--
-- >>> index (SNat :: SNat 5) arr
-- Couldn't match type 'False with 'True arising from a use of `index'
-- In the expression: index (SNat :: SNat 5) arr
-- In an equation for `it_apbE3':
--     it_apbE3 = index (SNat :: SNat 5) arr
index :: (i < n) ~ True => SNat i -> Array n a -> a
index i v = toVector v ! fromIntegral (fromSing i)

instance Show a => Show (Array n a) where
    show :: Show a => Array n a -> String
    show = show . toList

instance Eq a => Eq (Array n a) where
    (==) :: Eq a => Array n a -> Array n a -> Bool
    a == b = toVector a == toVector b

instance Ord a => Ord (Array n a) where
    compare :: Ord a => Array n a -> Array n a -> Ordering
    compare a b = compare (toVector a) (toVector b)

instance Functor (Array n) where
    fmap :: (a -> b) -> Array n a -> Array n b
    fmap f (Array v) = Array (fmap f v)

instance SingI n => Applicative (Array n) where
    pure :: SingI n => a -> Array n a
    pure = replicate
    (<*>) :: SingI n => Array n (a -> b) -> Array n a -> Array n b
    Array f <*> Array x = Array (V.zipWith ($) f x)

instance SingI n => Monad (Array n) where
    (>>=) :: SingI n => Array n a -> (a -> Array n b) -> Array n b
    Array v >>= f = Array (V.imap (\i a -> toVector (f a) ! i) v)

instance Foldable (Array n) where
    foldMap :: Monoid m => (a -> m) -> Array n a -> m
    foldMap f (Array v) = foldMap f v

instance Traversable (Array n) where
    traverse f (Array v) = fmap Array (traverse f v)

instance (Semigroup a, SingI n) => Semigroup (Array n a) where
    (<>) :: (Semigroup a, SingI n) => Array n a -> Array n a -> Array n a
    Array a <> Array b = Array (V.zipWith (<>) a b)

instance (Monoid a, SingI n) => Monoid (Array n a) where
    mempty :: (Monoid a, SingI n) => Array n a
    mempty = Array (V.replicate l mempty)
      where l = fromEnum (fromSing (sing :: Sing n))


-- | Convenience Array constructors
-- These could be generated with Template Haskell
arr0 :: Array 0 a
arr1 :: a -> Array 1 a
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
arr0 = empty
arr1 = singleton
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
