module Data.Geometry.Axis.Test where

import Test.QuickCheck
import Test.Monad
import Test.Magma
import Test.Fuzzy
import Control.Comonad (extract)
import Debug.Trace
import Data.Geometry.Axis

instance arbX :: (Arbitrary x) => Arbitrary (X x) where
  arbitrary = X <$> arbitrary

instance arbY :: (Arbitrary y) => Arbitrary (Y y) where
  arbitrary = Y <$> arbitrary

instance coarbX :: (CoArbitrary x) => CoArbitrary (X x) where
  coarbitrary = coarbitrary <<< extract

instance coarbY :: (CoArbitrary y) => CoArbitrary (Y y) where
  coarbitrary = coarbitrary <<< extract

init = do
  trace "Applicative X"
  checkApplicativeInstance (X 0) (X "") (X false)

  trace "Applicative Y"
  checkApplicativeInstance (Y 0) (Y "") (Y false)

  trace "Semiring X"
  checkSemiringInstance' \(X x) (X x') -> x =~= x'

  trace "Semiring Y"
  checkSemiringInstance' \(Y y) (Y y') -> y =~= y'
