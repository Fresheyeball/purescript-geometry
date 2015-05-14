module Data.Geometry.Point where

import Data.Bifunctor
import Control.Comonad
import Control.Biapply
import Control.Biapplicative
import Control.Apply(lift2)
import Math
import Data.Function(on)
import Data.Monoid
import Data.Geometry.Axis

data Point x y = Point (X x) (Y y)

newPoint :: forall x y. x -> y -> Point x y
newPoint x y = Point (X x) (Y y)

class Points a where points :: forall x y. a -> [Point x y]
class Origin a where origin :: forall x y. a ->  Point x y

instance semiringPoint :: (Semiring x, Semiring y) => Semiring (Point x y) where
  (+) = bilift2 (+) (+)
  zero = Point zero zero
  (*) = bilift2 (*) (*)
  one = Point one one

instance modulosemiPoint :: (ModuloSemiring x, ModuloSemiring y) => ModuloSemiring (Point x y) where
  (/) = bilift2 (/) (/)
  mod = bilift2 mod mod

instance ringPoint :: (Ring x, Ring y) => Ring (Point x y) where
  (-) = bilift2 (-) (-)

instance divisionRingPoint :: (DivisionRing x, DivisionRing y) => DivisionRing (Point x y)

instance getXPoint :: GetX (Point x y) x where
  getX (Point x _) = x

instance getYPoint :: GetY (Point x y) y where
  getY (Point _ y) = y

purePoint :: forall a. a -> Point a a
purePoint x = Point (X x) (Y x)

instance bifunctorPoint :: Bifunctor Point where
  bimap f g (Point x y) = Point (f <$> x) (g <$> y)

instance biapplyPoint :: Biapply Point where
  (<<*>>) (Point f g) (Point a b) = Point (f <*> a) (g <*> b)

instance biapplicativePoint :: Biapplicative Point where
  bipure x y = Point (X x) (Y y)

foldrPoint :: forall a. (a -> a -> a) -> a -> Point a a -> a
foldrPoint f a (Point x y) = f (extract x) $ f (extract y) a

foldlPoint :: forall a. (a -> a -> a) -> a -> Point a a -> a
foldlPoint f a (Point x y) = f (extract y) $ f (extract x) a

(@) :: forall a. (Semiring a) => Point a a -> Point a a -> a
(@) (Point x y) (Point x' y') = case (x * x') of
  (X x'') -> case (y * y') of
    (Y y'') -> x'' + y''

instance showPoint :: (Show x, Show y) => Show (Point x y) where
  show (Point x y) = "Point (" ++ show x ++ ") (" ++ show y ++ ")"

instance eqPoint :: (Eq x, Eq y) => Eq (Point x y) where
  (==) (Point x y) (Point x' y') = x == x' && y == y'
  (/=) x y = not $ x == y

distance :: Point Number Number -> Point Number Number -> Number
distance p q = sqrt $ l22dist p q

l22dist :: forall a. (Num a) => Point a a -> Point a a -> a
l22dist p q = let a = q - p in extract $ on (+) sq (getX a) (ySwapX $ getY a)
  where sq x = x * x
