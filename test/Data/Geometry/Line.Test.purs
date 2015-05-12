module Data.Geometry.Line.Test where

import Data.Geometry
import Data.Geometry.Axis
import Data.Geometry.Point
import Data.Geometry.Point.Test
import Data.Geometry.Line
import Data.Geometry.Size

import Math

import Test.QuickCheck
import Test.Classes
import Debug.Trace

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

checkDistance :: (Line -> Number) -> Number -> Number -> Boolean
checkDistance cf y y' = let
    l   = newLine 0 y 0 y'
    l'  = newLine y 0 y' 0
    f x = cf x =~= abs (y - y')
  in f l && f l'

instance arbLine :: Arbitrary Line where
  arbitrary = Line <$> arbitrary <*> arbitrary

init = do
  trace "Line"

  trace "perimeter"
  quickCheck <<< checkDistance $ perimeter

  trace "size"
  quickCheck <<< checkDistance $ getHeight <<< getSize

  trace "eq"
  quickCheck $ \x y x' y' ->
    let
      l  = newLine x y x' y'
      l' = newLine x y x' (y' + 1)
    in  l == l && l /= l' && l' /= l

  trace "area"
  quickCheck $ \x y x' y' ->
    let l = newLine x y x' y' in area l == 0

  trace "points"
  quickCheck $ \x y x' y' ->
    let l = newLine x y x' y' in points l == [newPoint x y, newPoint x' y']

  trace "show"
  let l =             Line (Point (X 1) (Y 2)) (Point (X 3) (Y 4))
  assert $ show l == "Line (Point (X 1) (Y 2)) (Point (X 3) (Y 4))"
