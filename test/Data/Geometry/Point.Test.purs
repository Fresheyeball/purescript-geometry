module Data.Geometry.Point.Test where

import Test.QuickCheck
import Test.Classes
import Debug.Trace

import Data.Geometry.Point
import Math

instance arbPoint :: (Arbitrary a) => Arbitrary (Point a) where
  arbitrary = Point <$> arbitrary <*> arbitrary

sq = flip pow 2

initt = trace "Point"
-- do
--
--   let p = pure 0 :: Point Number
--
--   it "functor"     $ checkFunctor p
--   it "applicative" $ checkApplicative p p p
--
--   it "show" let l =               Point 1 2
--     in expect (show l) `toEqual` "Point 1 2"
--
--   it "eq" <<< quickCheck $ \x y ->
--     Point x y == Point x y && Point x y /= Point x (y + 1)
--
--   it          "|+|" <<< quickCheck $ \x y ->
--     (Point x y |+| Point y x) == Point (x + y) (y + x)
--
--   it          "|-|" <<< quickCheck $ \x y ->
--     (Point x y |-| Point y x) == Point (x - y) (y - x)
--
--   it  "|*|" <<< quickCheck $ \x y z ->
--     (z |*| Point x y) == ((*) z <$> Point x y)
--
--   it                  "|@|" <<< quickCheck $ \x y x' y' ->
--     let b = (Point x y |@| Point x' y')
--         a = (*) <$> Point x y <*> Point x' y'
--     in b == getX a + getY a
--
--   it "distance" <<< quickCheck $ \x y x' y' ->
--       distance (Point x y) (Point x' y') == sqrt (sq (x' - x) + sq (y' - y))
