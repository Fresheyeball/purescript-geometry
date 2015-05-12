module Data.Geometry.Rect.Test where

import Data.Geometry
import Data.Geometry.Rect
import Data.Geometry.Point
import Data.Geometry.Point.Test
import Data.Geometry.Line
import Data.Geometry.Size

import Test.QuickCheck
import Test.Classes
import Debug.Trace

instance arbRect :: (Arbitrary a) => Arbitrary (Rect a) where
  arbitrary = Rect <$> arbitrary <*> arbitrary

init = trace "Rect" 
 -- do
 --
 --  it "perimeter" $ quickCheck \x y ->
 --    let r = Rect (Point 0 0) (Point x y) in perimeter r == x * 2 + y * 2
 --
 --  it "area"      $ quickCheck \x y ->
 --    let
 --      r    = Rect (Point 0 0) (Point x y)
 --      r'   = Rect (Point x y) (Point 0 0)
 --      r''  = Rect (Point x 0) (Point 0 y)
 --      r''' = Rect (Point 0 y) (Point x 0)
 --      xy   = x * y
 --    in xy == area r   && xy == area r'
 --    && xy == area r'' && xy == area r'''
 --
 --  it "show" let r =               Rect (Point 1 2) (Point 3 4)
 --    in expect (show r) `toEqual` "Rect (Point 1 2) (Point 3 4)"
 --
 --  it "hasPoints" let
 --      go :: Number -> Number -> Number -> Number -> Boolean
 --      go x y x' y' = let ps = points $ Rect (Point x y) (Point x' y')
 --        in ps == [Point x y, Point x y', Point x' y', Point x' y]
 --    in quickCheck go
 --
 --  it "eq" <<< quickCheck $ \x y -> let r = Rect (Point x y) (Point x y)
 --    in r == r && r /= Rect (Point x y) (Point x (y + 1))
 --
 --  let r = Rect (Point 0 0) (Point 0 0)
 --
 --  it "functor"     $ checkFunctor r
 --  it "applicative" $ checkApplicative r r r
 --
 --  it "size" <<< quickCheck $ \r ->
 --    let r' = r :: Rect Number in area r' == area (size r')
