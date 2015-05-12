module Data.Geometry.Axis.Test where

import Test.QuickCheck

import Data.Geometry.Axis

instance arbX :: Arbitrary X where
  arbitrary = X <$> arbitrary

instance arbY :: Arbitrary Y where
  arbitrary = Y <$> arbitrary
