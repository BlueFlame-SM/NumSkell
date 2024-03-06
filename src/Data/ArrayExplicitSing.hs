{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ArrayExplicitSing where

import Data.Singletons
import Data.Nat

import Data.Vector (Vector, (!))
import qualified Data.Vector as V


newtype Array (n :: Nat) a where
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
