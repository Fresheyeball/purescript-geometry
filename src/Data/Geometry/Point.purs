module Data.Geometry.Point where

--
-- —— Based on https://hackage.haskell.org/package/hgeometry-0.1.1.1/docs/src/Data-Geometry-Point.html ——
--

import Math
import Data.Function(on)
import Data.Monoid

data Point a = Point a a

class HasPoints g where
  points :: forall a. g a -> [Point a]

instance semiringPoints :: (Semiring a) => Semiring (Point a) where
  (+) = inerPoint (+)
  zero = pure zero
  (*) = inerPoint (*)
  one = pure one

inerPoint :: forall a. (a -> a -> a) -> Point a -> Point a -> Point a
inerPoint f (Point x y) (Point x' y') =  Point (x `f` x') (y `f` y')

(|+|) :: Point Number -> Point Number -> Point Number
(|+|) (Point x y) (Point x' y') =  Point (x + x') (y + y')

(|-|) :: Point Number -> Point Number -> Point Number
(|-|) (Point x y) (Point x' y') =  Point (x - x') (y - y')

(|*|) :: Number -> Point Number -> Point Number
(|*|) s (Point x y) = Point (x * s) (y * s)

(|@|) :: Point Number -> Point Number -> Number
(|@|) (Point x y) (Point x' y') = x * x' + y * y'

getX (Point x _) = x
getY (Point _ y) = y

instance functorPoint :: Functor Point where
  (<$>) f (Point x y) = Point (f x) (f y)

instance applyPoint :: Apply Point where
  (<*>) (Point fa fb) (Point a b) = Point (fa a) (fb b)

instance applicativePoint :: Applicative Point where
  pure x = Point x x

instance showPoint :: (Show a) => Show (Point a) where
  show (Point x y) = "Point " ++ show x ++ " " ++ show y

instance eqPoint :: (Eq a) => Eq (Point a) where
  (==) (Point x y) (Point x' y') = x == x' && y == y'
  (/=) x y = not $ x == y

distance :: Point Number -> Point Number -> Number
distance p q = sqrt $ l22dist p q

l22dist :: Point Number -> Point Number -> Number
l22dist p q = let a = q |-| p in on (+) sq (getX a) (getY a)
  where sq = flip pow 2
