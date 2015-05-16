module Test.Binary where

import Test.QuickCheck
import Debug.Trace

checkCommutative' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -> (a -> a -> a) -> QC Unit
checkCommutative' (==) (*) = do
  trace "Commutative"
  quickCheck commutative

  where

  commutative :: a -> a -> Boolean
  commutative a b = (a * b) == (b * a)

checkCommutative :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -> QC Unit
checkCommutative = checkCommutative' (==)
