{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ArrayTest where

import Control.Monad (liftM)
import Data.Array
import Data.Maybe
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Utils

prop_fromList_preservesLength :: [a] -> Bool
prop_fromList_preservesLength xs =
    let
        n = length xs

        anArray :: forall n a. (KnownNat n) => Array n a
        anArray = (fromJust . fromList) xs
     in
        length anArray == length xs

prop_toList_preservesLength :: forall n a. (KnownNat n) => Array n a -> Bool
prop_toList_preservesLength as = (length . toList) as == length as

arrayProps :: TestTree
arrayProps =
    testGroup
        "(ArrayProps)"
        [ QC.testProperty "toList preserves length" prop_toList_preservesLength
        , QC.testProperty "fromList preserves length" prop_fromList_preservesLength
        ]

instance (KnownNat n, QC.Arbitrary a) => QC.Arbitrary (Array n a) where
    arbitrary :: (KnownNat n, Arbitrary a) => Gen (Array n a)
    arbitrary = QC.sized array'
      where
        array' :: Int -> Gen (Array n a)
        array' n = fmap (fromJust . fromList @n) arbitrary
