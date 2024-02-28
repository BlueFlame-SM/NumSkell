module Data.Array where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Kind
import GHC.TypeLits
import Data.Proxy

newtype Array (n :: Nat) (a :: Type) where
  Array :: { toVector :: Vector a } -> Array n a

toList :: Array n a -> [a]
toList = V.toList . toVector

fromVector :: forall n a. KnownNat n => Vector a -> Maybe (Array n a)
fromVector v = if V.length v == fromIntegral (natVal (Proxy @n))
                 then Just (Array v)
                 else Nothing

fromList :: forall n a. KnownNat n => [a] -> Maybe (Array n a)
fromList = fromVector . V.fromList

-- withVec :: Vector a -> (forall n. KnownNat n => Array n a -> b) -> b
-- withVec v f = case someNatVal (fromIntegral (V.length v)) of
--     Just (SomeNat (Proxy :: Proxy m)) -> f (Array @m v)

deriving instance Eq a => Eq (Array n a)

instance Show a => Show (Array n a) where
  show (Array v) = show v

instance Functor (Array n) where
  fmap f (Array v) = Array (fmap f v)

instance KnownNat n => Applicative (Array n) where
  pure a = Array (V.replicate (fromIntegral (natVal (Proxy @n))) a)
  Array f <*> Array x = Array (V.zipWith ($) f x)

instance KnownNat n => Monad (Array n) where
  return = pure
  Array v >>= f = Array (V.concatMap (toVector . f) v)

instance Foldable (Array n) where
  foldMap f (Array v) = foldMap f v

instance Traversable (Array n) where
  traverse f (Array v) = fmap Array (traverse f v)
