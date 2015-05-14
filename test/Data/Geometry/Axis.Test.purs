module Data.Geometry.Axis.Test where

import Test.QuickCheck

import Data.Geometry.Axis

instance arbX :: (Arbitrary x) => Arbitrary (X x) where
  arbitrary = X <$> arbitrary

instance arbY :: (Arbitrary y) => Arbitrary (Y y) where
  arbitrary = Y <$> arbitrary
