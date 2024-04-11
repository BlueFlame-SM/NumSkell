{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module MatrixTest where

import qualified Data.Matrix (Matrix)
import qualified Data.Matrix as M
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- -----------------
-- Property tests
-- -----------------
matrixProps :: TestTree
matrixProps = testGroup
        "(matrixProps)"
        [ 
        ]

-- -----------------
-- Unit tests
-- -----------------
exampleA :: Matrix 2 2 Int
exampleA = M.mat2x2 1 2 3 4

exampleB :: Matrix 2 3 Int
exampleB = M.mat2x3 1 2 3 4 5 6

exampleC :: Matrix 3 2 Int
exampleC = M.mat3x2 10 11 20 21 30 31

exampleD :: Matrix 2 1 Int
exampleD = M.mat2x1 2 3

--[[140, 146], [320, 335]]
exampleBxC :: Matrix 2 2 Int
exampleBxC = M.mat2x2 140 146 320 335

exampleCxD :: Matrix 3 1 Int
exampleCxD = M.mat3x1 53 103 153

matrixUnitTests :: TestTree
matrixUnitTests = testGroup
        "(matrixUnitTests)"
        [  QC.testProperty "Matrix multiplication of [[1,2,3][4,5,6] X [[1,2],[3,4],[5,6]] = [[140,145],[320,355]]" $
             M.matrixMult exampleB exampleC === exampleBxC
         , QC.testProperty "Matrix multiplication of [[1,2],[3,4],[5,6]] X [[53], [103], [153]] = [[53], [103],[153]]" $
             M.matrixMult exampleC exampleD === exampleCxD

        ]