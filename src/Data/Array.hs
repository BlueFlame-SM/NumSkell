{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Array where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Kind
import GHC.TypeNats
import Data.Proxy

newtype Array (n :: Nat) (a :: Type) where
  Array :: { toVector :: Vector a } -> Array n a
  deriving (Eq, Ord)

toList :: Array n a -> [a]
toList = V.toList . toVector

fromVector :: forall n a. KnownNat n => Vector a -> Maybe (Array n a)
fromVector v = if V.length v == fromIntegral (natVal (Proxy @n))
                 then Just (Array v)
                 else Nothing

fromList :: forall n a. KnownNat n => [a] -> Maybe (Array n a)
fromList = fromVector . V.fromList

-- TypeApplications doesn't seem to work with Nat:
-- Error: Expected a type, but 'n' has kind 'Nat'
-- withVec :: Vector a -> (forall n. KnownNat n => Array n a -> b) -> b
-- withVec v f = case (someNatVal . fromIntegral . V.length) v of
--     Just (SomeNat (Proxy :: Proxy m)) -> f (Array @m v)
--     Nothing -> error "withVec: impossible"

instance Show a => Show (Array n a) where
  show (Array v) = show v

instance Functor (Array n) where
  fmap f (Array v) = Array (fmap f v)

instance KnownNat n => Applicative (Array n) where
  pure a = Array (V.replicate (fromIntegral (natVal (Proxy @n))) a)
  Array f <*> Array x = Array (V.zipWith ($) f x)

instance (KnownNat n) => Monad (Array n) where
  return = pure
  Array v >>= f = Array (V.imap (\i a -> toVector (f a) ! i) v)

instance Foldable (Array n) where
  foldMap f (Array v) = foldMap f v

instance Traversable (Array n) where
  traverse f (Array v) = fmap Array (traverse f v)

instance (Semigroup a, KnownNat n) => Semigroup (Array n a) where
  Array a <> Array b = Array (V.zipWith (<>) a b)

instance (Monoid a, KnownNat n) => Monoid (Array n a) where
  mempty = Array (V.replicate (fromIntegral (natVal (Proxy @n))) mempty)


append :: forall n m a. Array n a -> Array m a -> Array (n + m) a
append (Array a) (Array b) = Array (a <> b)
