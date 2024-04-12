{-# LANGUAGE DataKinds #-}

module Main (main) where

import ArrayTest (arrayProps, arrayUnitTests)
import MatrixTest (matrixProps, matrixUnitTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [arrayProps, matrixProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [arrayUnitTests, matrixUnitTests]
