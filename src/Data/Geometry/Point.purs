module Data.Geometry.Point where

import Math
import Data.Function(on)
import Data.Monoid
import Data.Geometry.Axis

data Point = Point X Y

newPoint :: Number -> Number -> Point
newPoint x y = Point (X x) (Y y)

class Points a where
  points :: a -> [Point]

class Origin a where
  origin :: a -> Point

instance semiringPoint :: Semiring Point where
  (+) = liftPoint2 (+)
  zero = Point zero zero
  (*) = liftPoint2 (*)
  one = Point one one

instance modulosemiPoint :: ModuloSemiring Point where
  (/) = liftPoint2 (/)
  mod = liftPoint2 mod

instance ringPoint :: Ring Point where
  (-) = liftPoint2 (-)

instance divisionRingPoint :: DivisionRing Point

instance getXPoint :: GetX Point where
  getX (Point x _) = x

instance getYPoint :: GetY Point where
  getY (Point _ y) = y

purePoint :: Number -> Point
purePoint x = Point (X x) (Y x)

liftPoint :: (Number -> Number) -> Point -> Point
liftPoint f (Point x y) = Point (liftX f x) (liftY f y)

liftPoint2 :: (Number -> Number -> Number) -> Point -> Point -> Point
liftPoint2 f (Point x y) (Point x' y') =  Point (liftX2 f x x') (liftY2 f y y')

foldrPoint :: (Number -> Number -> Number) -> Number -> Point -> Number
foldrPoint f a (Point x y) = f (runX x) $ f (runY y) a

foldlPoint :: (Number -> Number -> Number) -> Number -> Point -> Number
foldlPoint f a (Point x y) = f (runY y) $ f (runX x) a

(@) :: Point -> Point -> Number
(@) (Point x y) (Point x' y') = case (x * x') of
  (X x'') -> case (y * y') of
    (Y y'') -> x'' + y''

instance showPoint :: Show Point where
  show (Point x y) = "Point (" ++ show x ++ ") (" ++ show y ++ ")"

instance eqPoint :: Eq Point where
  (==) (Point x y) (Point x' y') = x == x' && y == y'
  (/=) x y = not $ x == y

distance :: Point -> Point -> Number
distance p q = sqrt $ l22dist p q

l22dist :: Point -> Point -> Number
l22dist p q = let a = q - p in on (+) sq (runX $ getX a) (runY $ getY a)
  where sq = flip pow 2
