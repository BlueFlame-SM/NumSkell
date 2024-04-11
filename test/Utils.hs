module Utils where

(f `preserves` p) x = p (f x) == p x
