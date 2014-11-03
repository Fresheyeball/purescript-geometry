module Data.Geometry.Rect where

import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.Size
import Data.Function
import Math

data Rect a = Rect (Point a) (Point a)

instance showRect :: (Show a) => Show (Rect a) where
  show (Rect xy yx) = "Rect (" ++ show xy ++ ") (" ++ show yx ++ ")"

instance hasPointsRect :: HasPoints Rect where
  points (Rect a@(Point x y) b@(Point x' y')) = 
    [a, (Point x y'), b, (Point x' y)]

instance eqRect :: (Eq a) => Eq (Rect a) where
  (==) (Rect a b) (Rect a' b') = a == a' && b == b'
  (/=) x y = not $ x == y

instance functorRect :: Functor Rect where
  (<$>) f (Rect a b) = Rect (f <$> a) (f <$> b)

instance applyRect :: Apply Rect where 
  (<*>) (Rect fa fb) (Rect a b) = Rect (fa <*> a) (fb <*> b)

instance applicative :: Applicative Rect where 
  pure x = Rect (pure x) (pure x)

instance areaRect :: Area (Rect Number) where 
  area (Rect (Point x y) (Point x' y')) = 
    let f a b = abs (a - b) in f x x' * f y y'

instance perimeterRect :: Perimeter (Rect Number) where
  perimeter (Rect (Point x y) (Point x' y')) = 
    let f a b = abs (a - b) * 2 in f x x' + f y y'

instance hasSizeRect :: HasSize (Rect Number) where
  size (Rect (Point x y) (Point x' y')) = let f a b = abs (a - b)
    in newSize (f x x') (f y y')

withP a b (Rect (Point x y) (Point x' y')) = Point (x `a` x') (y `b` y')

getTopLeft     = withP min min 
getTopRight    = withP max min
getBottomRight = withP max max 
getBottomLeft  = withP min max 

withL x y inp = Line (x inp) (y inp) 

getTopLine    = withL getTopLeft    getTopRight
getLeftLine   = withL getTopLeft    getBottomLeft
getBottomLine = withL getBottomLeft getBottomRight
getRightLine  = withL getTopRight   getBottomRight