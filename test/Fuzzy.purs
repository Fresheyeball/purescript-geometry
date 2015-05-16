module Test.Fuzzy where

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001
