module Data.Geometry.Line.Test where


import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Point.Test
import Data.Geometry.Line
import Data.Geometry.Size

import Math

import Test.Mocha
import Test.Chai
import Test.QuickCheck
import Test.Classes

mkLine :: Number -> Number -> Number -> Number -> Line Number
mkLine x y x' y' = Line (Point x y) (Point x' y')

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

checkDistance cf y y' = let
    l   = Line (Point 0 y) (Point 0 y')
    l'  = Line (Point y 0) (Point y' 0)
    f x = cf x =~= abs (y - y')
  in f l && f l'

instance arbLine :: (Arbitrary a) => Arbitrary (Line a) where
  arbitrary = Line <$> arbitrary <*> arbitrary

init = describe "Line" do 
  
  it "perimeter" <<< quickCheck <<< checkDistance $ perimeter

  it "size" <<< quickCheck <<< checkDistance $ getHeight <<< size

  it "eq" <<< quickCheck $ \x y x' y' ->
    let l = mkLine x y x' y'
    in  l == l && l /= Line (Point x y) (Point x' (y' + 1))

  let l = mkLine 0 0 0 0

  it "functor"     $ checkFunctor l 
  it "applicative" $ checkApplicative l l l 

  it "area" <<< quickCheck $ \x y x' y' ->
    let l = mkLine x y x' y' in area l == 0

  it "points" <<< quickCheck $ \x y x' y' ->
    let l = mkLine x y x' y' in points l == [Point x y, Point x' y']

  it "show" let l =               Line (Point 1 2) (Point 3 4) 
    in expect (show l) `toEqual` "Line (Point 1 2) (Point 3 4)"

  