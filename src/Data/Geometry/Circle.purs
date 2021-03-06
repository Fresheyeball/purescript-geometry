module Data.Geometry.Circle where

import Data.Geometry
import Data.Geometry.Point
import Math

data Circle a = Circle (Point a) a 

instance showCircle :: (Show a) => Show (Circle a) where
  show (Circle p r) = "Circle (" ++ show p ++ ") " ++ show r

instance hasPointsCircle :: HasPoints Circle where
  points (Circle p _) = [p]

instance eqCircle :: (Eq a) => Eq (Circle a) where
  (==) (Circle p r) (Circle p' r') = p == p' && r == r'
  (/=) x y = not $ x == y

instance areaCircle :: Area (Circle Number) where
  area (Circle _ r) = pi * r `pow` 2

instance perimeterCircle :: Perimeter (Circle Number) where
  perimeter (Circle _ r) = 2 * pi * r

instance functorCircle :: Functor Circle where
  (<$>) f (Circle p r) = Circle (f <$> p) (f r)

instance applyCircle :: Apply Circle where
  (<*>) (Circle fp fr) (Circle p r) = Circle (fp <*> p) (fr r)

instance applicativeCircle :: Applicative Circle where
  pure x = Circle (pure x) x

circumference :: Circle Number -> Number
circumference = perimeter