module Data.Geometry.Point.Test where

import Control.Comonad

import Control.Biapplicative(bipure)
import Test.Fuzzy
import Test.QuickCheck
import Test.Monad
import Test.Magma
import Test.Bifunctor
import Debug.Trace
import Data.Geometry.Axis.Test
import Data.Geometry.Point
import Data.Geometry.Axis
import Math

instance arbPoint :: (Arbitrary a, Semiring a) => Arbitrary (Point a a) where
  arbitrary = Point <$> arbitrary <*> arbitrary

instance coarbPoint :: (CoArbitrary a, Semiring a) => CoArbitrary (Point a a) where
  coarbitrary (Point (X x) (Y y)) = coarbitrary $ x + y

assert :: forall p. (Testable p) => p -> QC Unit
assert f = quickCheck' 1 f

initt = do
  trace "Point"

  trace "Distance"
  quickCheck $ \x y x' y' ->
    let
      xyPlus x y = foldXY (+) x y zero
      sq :: forall a. (Semiring a) => a -> a
      sq x = x * x
    in distance (Point x y) (Point x' y') == sqrt (sq (x' - x) `xyPlus` sq (y' - y))

  trace "Show"
  let l =               Point (X 1) (Y 2)
  assert $ (show l) == "Point (X 1) (Y 2)"

  trace "Eq"
  let
    checkPointEq :: X Number -> Y Number -> Boolean
    checkPointEq x y = Point x y == Point x y && Point x y /= Point x (y + (Y one))
  quickCheck checkPointEq

  trace "Bifunctor"
  let p = bipure 0 0 :: Point Number Number
  checkBifunctor p

  trace "Semiring"
  checkSemiring' \(Point (X x) (Y y)) (Point (X x') (Y y')) -> x =~= x' && y =~= y'
