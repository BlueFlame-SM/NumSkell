{-# LANGUAGE DataKinds #-}

module Main (main) where

import Data.Array
import Data.Maybe (fromJust)
import GHC.TypeNats

import ArrayTest (arrayProps)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [arrayProps]
