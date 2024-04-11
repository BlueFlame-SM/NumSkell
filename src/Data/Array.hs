{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array (
  Array,
  fromVector,
  fromList,
  withVec,
  withList,
  replicate,
  singleton,
  append,
  split,
  cons,
  zipWith,
  index
) where

import Data.Singletons
    ( withSingI,
      Sing,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing) )
import GHC.TypeLits.Singletons ( Natural, Sing, SNat )
import Prelude.Singletons
    ( withSingI,
      SingI(..),
      SingKind(fromSing, toSing),
      SomeSing(SomeSing),
      POrd(type (<)),
      PNum(type (+)) )
import Prelude hiding (replicate, zipWith)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

newtype Array (n :: Natural) a where
  Array :: { toVector :: Vector a } -> Array n a

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
           -> Maybe (Array n a)  -- ^ The resulting array of size 'n'
fromVector = fromVector_ (sing :: SNat n)

-- | Convert a list to an array of a specific size.
--
-- The same as 'fromVector', but takes a list instead of a 'Vector'.
fromList :: forall n a . SingI n
         => [a]  -- ^ The input list
         -> Maybe (Array n a)  -- ^ The resulting array of size 'n'
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
replicate x = let len = fromIntegral (fromSing (sing :: SNat n))
        in  Array (V.replicate len x)

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
  show = show . toList

instance Eq a => Eq (Array n a) where
  a == b = toVector a == toVector b

instance Ord a => Ord (Array n a) where
  compare a b = compare (toVector a) (toVector b)

instance Functor (Array n) where
  fmap f (Array v) = Array (fmap f v)

instance SingI n => Applicative (Array n) where
  pure = replicate
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
