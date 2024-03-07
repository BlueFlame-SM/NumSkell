{-# LANGUAGE DataKinds #-}

module Main (main) where

import Data.Array
import Data.Maybe (fromJust)
import GHC.TypeNats

anArray :: (Array 1 Int)
anArray = (fromJust . fromList) [1]

main :: IO ()
main = print anArray
