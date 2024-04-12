{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module ArrayTest where

import Data.Array (withList, append, split, Array)
import qualified Data.Array as A
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Singletons

prop_fromList_preservesLength :: [a] -> Property
prop_fromList_preservesLength xs =
    withList xs $ \arr ->
    A.length arr === length xs

prop_fromToList_isId :: (Eq a, Show a) => [a] -> Property
prop_fromToList_isId xs =
    withList xs $ \arr ->
    A.toList arr === xs

prop_fromListAppend_equals_appendFromList :: (Eq a, Show a) => [a] -> [a] -> Property
prop_fromListAppend_equals_appendFromList xs' ys' =
    withList xs' $ \xs ->
    withList ys' $ \ys ->
    withList (xs' ++ ys') $ \zs ->
    A.toList (A.append xs ys) === A.toList zs


-- The goal of typeAgrees is to check that the internal length of the vector agrees with the length stored in the type.
-- This property should be checked for all functions that access the internal Array, as these functions bypass the length checking.
typeAgrees :: SingI n => Array n a -> Property
typeAgrees xs = length (A.toList xs) === A.length xs

prop_withList_typeAgrees :: [a] -> Property
prop_withList_typeAgrees xs' = withList xs' $ \xs
    -> typeAgrees xs

{-# ANN prop_fmap_typeAgrees "HLint: ignore Functor law" #-}
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

prop_append_typeAgrees :: [a] -> [a] -> Property
prop_append_typeAgrees xs' ys' =
    withList xs' $ \xs ->
    withList ys' $ \ys ->
    A.length xs + A.length ys === length (xs' ++ ys')

arrayProps :: TestTree
arrayProps = testGroup
        "(ArrayProps)"
        [  QC.testProperty "fromList preserves length" (prop_fromList_preservesLength @[Int] )
         , QC.testProperty "from a list and then back to a list changes nothing " (prop_fromToList_isId @[Int] )
         , QC.testProperty "internal lengths are equal after withList" (prop_withList_typeAgrees @[Int] )
         , QC.testProperty "internal lengths are equal after fmap" ( prop_fmap_typeAgrees @[Int] )
         , QC.testProperty "internal lengths are equal after <*>" prop_star_typeAgrees
         , QC.testProperty "internal lengths are equal after <>" prop_semigroup_typeAgrees
         , QC.testProperty "internal lengths are equal after append" ( prop_append_typeAgrees @[Int] )
        ]

oneTwoThree :: Array 3 Int
oneTwoThree = A.arr3 1 2 3

testIndex :: Int
testIndex = A.index @1 oneTwoThree

fourFiveSix :: Array 3 Int
fourFiveSix = A.arr3 4 5 6

testAppend :: Array 6 Int
testAppend = oneTwoThree `append` fourFiveSix

testSplit :: (Array 2 Int, Array 4 Int)
testSplit = split (oneTwoThree `append` fourFiveSix)

arrayUnitTests :: TestTree
arrayUnitTests = testGroup "(Array unit tests)"
                 [   QC.testProperty "indexing into an array" $ testIndex === 2
                   , QC.testProperty "appending two arrays" $ A.toList testAppend === [1,2,3,4,5,6]
                   , QC.testProperty "splitting an array" $ testSplit === (A.arr2 1 2, A.arr4 3 4 5 6)
                 ]
