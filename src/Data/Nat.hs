{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Nat where

import Data.Singletons.Base.TH
import Numeric.Natural

type Nat = Natural

-- This generates everything we need for the Nat kind. Since Nat is an alias for
-- Natural, we don't have issues with type-level literals always being treated
-- as Natural.
$(genSingletons [''Nat])


-- The following is a manual implementation of Nat, but this doesn't work
-- because Haskell treats a type-level literal number as a Natural, even if Nat
-- has instance Num.
-- data Nat = Z | S Nat deriving (Show, Eq, Ord)

-- instance Bounded Nat where
--   minBound = Z
--   maxBound = S maxBound

-- instance Enum Nat where
--   succ = S

--   pred Z     = error "pred: Nat underflow"
--   pred (S n) = n
--   toEnum n | n < 0  = error "toEnum: Nat underflow"
--            | n == 0 = Z
--            | otherwise = S (toEnum (n - 1))
--   fromEnum Z = 0
--   fromEnum (S n) = 1 + fromEnum n

-- instance Num Nat where
--   Z   + n = n
--   S m + n = S (m + n)
--   Z   * _ = Z
--   S m * n = n + (m * n)
--   m   - Z   = m
--   S m - S n = m - n
--   Z   - _   = error "(-): Nat underflow"
--   abs n = n
--   signum Z = Z
--   signum _ = S Z
--   fromInteger n | n < 0 = error "fromInteger: negative"
--                 | n == 0 = Z
--                 | otherwise = S (fromInteger (n - 1))

-- instance Real Nat where
--   toRational = toRational . fromEnum

-- instance Integral Nat where
--   toInteger = toInteger . fromEnum
--   quotRem m n = (fromInteger q, fromInteger r)
--     where (q, r) = quotRem (toInteger m) (toInteger n)

-- $(genSingletons [''Nat])
