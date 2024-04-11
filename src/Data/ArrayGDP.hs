{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.ArrayGDP where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Theory.Named
import Theory.Equality
import Logic.Implicit
import Logic.Proof
import Logic.Classes
import Logic.Propositional
import Data.The


head :: Fact (NonEmpty xs) => (Vector a  ~~ xs) -> a
head = V.head . the


zipWith :: (a -> b -> c) 
        -> (Vector a ~~ Length xs) 
        -> (Vector b ~~ Length xs) 
        -> (Vector c ~~ Length xs)
zipWith f (The xs) (The ys) = defn $ V.zipWith f xs ys

v2'zipWith :: (a -> b -> c) 
        -> (Vector a ~~ Length xs) 
        -> (Vector b ~~ Length ys) 
        -> (Vector c ~~ Length (Min (Length xs) (Length ys)))
v2'zipWith f (The xs) (The ys) = defn $ V.zipWith f xs ys

reverse :: (Vector a ~~ xs) -> (Vector a ~~ Reverse xs)
reverse = defn . V.reverse . the

-- | Functions producing runtime proofs
isNonEmpty :: (Vector a ~~ xs) 
               -> Maybe (Vector a ~~ NonEmpty xs)
isNonEmpty (The v) = if V.null v then Nothing else Just (defn v)

equalLength :: (Vector a ~~ xs) 
               -> (Vector b ~~ ys) 
               -> Maybe (Proof (Length xs == Length ys))
equalLength (The xs) (The ys) = if V.length xs == V.length ys then Just axiom else Nothing

-- | Predicates about the possible shapes of vectors.
newtype NonEmpty xs = NonEmpty Defn
newtype Reverse xs  = Reverse Defn
newtype Length  xs  = Length  Defn

newtype Min a b = Min Defn


-- | Lemmas about the possible shapes of vectors.
revLengthLemma :: Proof (Length (Reverse xs) == Length xs)
revLengthLemma = axiom

equalLengthLemma :: (Vector a ~~ Length xs) -> Proof (Length xs == Length ys) -> (Vector a ~~ Length ys)
equalLengthLemma xs p = defn $ the xs