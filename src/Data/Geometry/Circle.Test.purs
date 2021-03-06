module Data.Geometry.Circle.Test where

import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Point.Test
import Data.Geometry.Line
import Data.Geometry.Size
import Data.Geometry.Circle

import Math

import Test.Mocha
import Test.Chai
import Test.QuickCheck
import Test.Classes

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

instance arbCircle :: (Arbitrary a) => Arbitrary (Circle a) where
  arbitrary = Circle <$> arbitrary <*> arbitrary

inittt = describe "Circle" do 
  
  it "show" let c =            Circle (Point 0 0) 0 in
    expect (show c) `toEqual` "Circle (Point 0 0) 0"

  it "points" <<< quickCheck $ \x -> points (Circle x 0) == [x]

  it "eq" <<< quickCheck $ \x y r -> let c = Circle (Point x y) r
    in c /= Circle (Point (x + 1) y     )  r      &&
       c /= Circle (Point  x     (y + 1))  r      &&
       c /= Circle (Point  x      y     ) (r + 1) &&
       c == c

  it "area" <<< quickCheck $ \p r -> 
    let 
      c = (Circle p r)
      d = r * 2
      a = area c 
    in a == pi * (r * r) && a == pi * (d * d) / 4

  it         "circumference" <<< quickCheck $ \c ->
    let c' =  circumference c 
    in  c' =~= perimeter c &&
        c' =~= sqrt (4 * pi * (area c))

  let c = pure 0 :: Circle Number

  it "functor"      $ checkFunctor c 
  it "applicative"  $ checkApplicative c c c 
