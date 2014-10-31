module Data.Geometry.Line where

import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Size

data Line a = Line (Point a) (Point a)

instance perimeterLine :: Perimeter (Line Number) where
  perimeter (Line a b) = distance a b

instance hasSizeLine :: HasSize (Line Number) where
  size l = Size { width : 0, height : (perimeter l) }

instance eqLine :: (Eq a) => Eq (Line a) where
  (==) (Line a b) (Line a' b') = a == a' && b == b'
  (/=) x y = not $ x == y

instance functorLine :: Functor Line where
  (<$>) f (Line a b) = Line (f <$> a) (f <$> b)

instance applyLine :: Apply Line where
  (<*>) (Line fa fb) (Line a b) = Line (fa <*> a) (fb <*> b)

instance applicativeLine :: Applicative Line where 
  pure x = Line (pure x) (pure x)

instance areaLine :: Area (Line Number) where
  area _ = 0

instance hasPointsLine :: HasPoints Line where 
  points (Line a b) = [a,b]

instance showLine :: (Show a) => Show (Line a) where
  show (Line a b) = "Line (" ++ show a ++ ") (" ++ show b ++ ")"
