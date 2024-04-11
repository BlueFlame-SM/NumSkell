{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module ArrayTest where

import Control.Monad (liftM)
import Data.Array (withList, Array)
import qualified Data.Array as A
import qualified Data.Vector as V
import Data.Maybe
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Singletons
import Utils

-- newtype WithList a = MkWithList { runWithList :: forall r . ([a] -> r) -> r }

-- instance Functor WithList where
--     fmap f (MkWithList g) = MkWithList (\k -> g (k . map f))

-- instance Applicative WithList where
--     pure x = MkWithList (\k -> k [x])
--     x <*> y = do a <- x; b <- y; pure (a b)

-- instance Monad WithList where
--     return = pure
    -- MkWithList m >>= f = MkWithList (\)


prop_fromList_preservesLength :: [a] -> Property
prop_fromList_preservesLength xs = withList xs $ \arr 
    -> A.length arr === length xs

prop_fromToList_isId :: (Eq a, Show a) => [a] -> Property
prop_fromToList_isId xs = withList xs $ \arr 
    -> (A.toList arr) === xs 

prop_fromListAppend_equals_appendFromList :: (Eq a, Show a) => [a] -> [a] -> Property
prop_fromListAppend_equals_appendFromList xs' ys' = 
    withList xs' $ \xs -> 
    withList ys' $ \ys -> 
    withList (xs' ++ ys') $ \zs ->
    A.toList (A.append xs ys) === A.toList zs


-- The goal of typeAgrees is to check that the internal length of the vector agrees with the length stored in the type.
-- This property should be checked for all functions that access the internal Array, as these functions bypass the length checking.
typeAgrees :: SingI n => Array n a -> Property
typeAgrees xs = A.internalValueLength xs === A.internalTypeLength xs

prop_withList_typeAgrees :: [a] -> Property
prop_withList_typeAgrees xs' = withList xs' $ \xs
    -> typeAgrees xs

prop_fmap_typeAgrees :: [a] -> Property
prop_fmap_typeAgrees xs' = withList xs' $ \xs 
    -> typeAgrees $ fmap id xs

prop_star_typeAgrees :: [Int] -> Property
prop_star_typeAgrees xs' = withList xs' $ \xs 
    -> typeAgrees $ fmap (+) xs <*> xs

prop_semigroup_typeAgrees :: [String] -> Property
prop_semigroup_typeAgrees xs' = 
    withList xs' $ \xs -> 
    typeAgrees $ xs <> xs

-- KnownNat n, KnownNat m => KnownNat (n + m)

prop_append_typeAgrees :: [a] -> [a] -> Property
prop_append_typeAgrees xs' ys' =
    withList xs' $ \xs ->
    withList ys' $ \ys ->
    A.internalTypeLength xs + A.internalTypeLength ys === length (xs' ++ ys')

arrayProps :: TestTree
arrayProps = testGroup
        "(ArrayProps)"
        [  QC.testProperty "fromList preserves length" (prop_fromList_preservesLength @[Int] )
         , QC.testProperty "from a list and then back to a list changes nothing " (prop_fromToList_isId @[Int] )
         , QC.testProperty "internal lengths are equal after withList" (prop_withList_typeAgrees @[Int] )
         , QC.testProperty "internal lengths are equal after fmap" ( prop_fmap_typeAgrees @[Int] )
         , QC.testProperty "internal lengths are equal after <*>" ( prop_star_typeAgrees )
         , QC.testProperty "internal lengths are equal after <>" ( prop_semigroup_typeAgrees )
         , QC.testProperty "internal lengths are equal after append" ( prop_append_typeAgrees @[Int] )
        ]