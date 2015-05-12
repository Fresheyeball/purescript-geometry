module Data.Geometry.Line where

import Data.Geometry
import Data.Geometry.Axis
import Data.Geometry.Point
import Data.Geometry.Size

data Line = Line Point Point

newLine :: Number -> Number -> Number -> Number -> Line
newLine x y x' y' = Line (Point (X x) (Y y)) (Point (X x') (Y y'))

instance perimeterLine :: Perimeter Line where
  perimeter (Line a b) = distance a b

instance getSizeLine :: GetSize Line where
  getSize l = Size { width : 0, height : (perimeter l) }

instance eqLine :: Eq Line where
  (==) (Line a b) (Line a' b') = a == a' && b == b'
  (/=) x y = not $ x == y

liftLine' :: (Number -> Number) -> Line -> Line
liftLine' = liftLine <<< liftPoint

liftLine :: (Point -> Point) -> Line -> Line
liftLine f (Line a b) = Line (f a) (f b)

pureLine :: Number -> Line
pureLine x = Line (purePoint x) (purePoint x)

instance areaLine :: Area Line where
  area _ = 0

instance pointsLine :: Points Line where
  points (Line a b) = [a,b]

instance showLine :: Show Line where
  show (Line a b) = "Line (" ++ show a ++ ") (" ++ show b ++ ")"
