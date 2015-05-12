module Data.Geometry.Point where

--
-- —— Based on https://hackage.haskell.org/package/hgeometry-0.1.1.1/docs/src/Data-Geometry-Point.html ——
--

import Math
import Data.Function(on)
import Data.Monoid
import Data.Geometry.Axis

data Point = Point X Y

class Points a where
  points :: a -> [Point]

class Origin a where
  origin :: a -> Point

instance semiringPoint :: Semiring Point where
  (+) = liftPoint (+)
  zero = Point zero zero
  (*) = liftPoint (*)
  one = Point one one

instance modulosemiPoint :: ModuloSemiring Point where
  (/) = liftPoint (/)
  mod = liftPoint mod

instance ringPoint :: Ring Point where
  (-) = liftPoint (-)

instance divisionRingPoint :: DivisionRing Point

instance getXPoint :: GetX Point where
  getX (Point x _) = x

instance getYPoint :: GetY Point where
  getY (Point _ y) = y

liftPoint :: (Number -> Number -> Number) -> Point -> Point -> Point
liftPoint f (Point x y) (Point x' y') =  Point (liftX2 f x x') (liftY2 f y y')

foldrPoint :: (Number -> Number -> Number) -> Number -> Point -> Number
foldrPoint f a (Point x y) = f (runX x) $ f (runY y) a

foldlPoint :: (Number -> Number -> Number) -> Number -> Point -> Number
foldlPoint f a (Point x y) = f (runY y) $ f (runX x) a

(|@|) :: Point -> Point -> Number
(|@|) (Point x y) (Point x' y') = case (x * x') of
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
