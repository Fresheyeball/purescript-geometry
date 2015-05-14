module Data.Geometry.Axis where

import Control.Apply
import Control.Comonad
import Control.Extend
import Data.Either
import Data.Function
import Data.Foldable
import Data.Traversable

newtype X x = X x
newtype Y y = Y y

class GetX a b where
  getX :: a -> X b
class GetY a b where
  getY :: a -> Y b

xSwapY :: forall a. X a -> Y a
xSwapY = extract >>> Y

ySwapX :: forall a. Y a -> X a
ySwapX = extract >>> X

type Axis a = Either (X a) (Y a)

foldXY :: forall a. (a -> a -> a) -> X a -> Y a -> a -> a
foldXY f (X x) (Y y) = f x <<< f y
foldYX :: forall a. (a -> a -> a) -> Y a -> X a -> a -> a
foldYX f (Y y) (X x) = f y <<< f x

instance functorX :: Functor X where
  (<$>) f (X x) = X $ f x
instance applyX :: Apply X where
  (<*>) (X fx) (X x) = X $ fx x
instance applicativeX :: Applicative X where
  pure = X
instance bindX :: Bind X where
  (>>=) (X x) f = f x
instance monadX :: Monad X

instance semiringX :: (Semiring x) => Semiring (X x) where
  (+) = lift2 (+)
  zero = X zero
  (*) = lift2 (*)
  one = X one
instance moduloSemiringX :: (ModuloSemiring x) => ModuloSemiring (X x) where
  mod = lift2 mod
  (/) = lift2 (/)
instance ringX :: (Ring x) => Ring (X x) where
  (-) = lift2 (-)
instance divisionRightX :: (DivisionRing x) => DivisionRing (X x)
instance numX :: (Num x) => Num (X x)

instance extendX :: Extend X where
  (<<=) f m = X (f m)

instance comonadX :: Comonad X where
  extract (X x) = x

instance foldableX :: Foldable X where
  foldr f z (X x) = f x z
  foldl f z (X x) = f z x
  foldMap f (X x) = f x

instance traversableX :: Traversable X where
  traverse f (X x) = X <$> f x
  sequence (X x) = X <$> x

instance showX :: (Show x) => Show (X x) where
  show = extract >>> show >>> (++) "X "

instance eqX :: (Eq x) => Eq (X x) where
  (==) = (==) `on` extract
  (/=) x y = not $ x == y

instance ordX :: (Ord a) => Ord (X a) where
  compare (X x) (X y) = compare x y

instance functorY :: Functor Y where
  (<$>) f (Y y) = Y $ f y
instance applyY :: Apply Y where
  (<*>) (Y fy) (Y y) = Y $ fy y
instance applicativeY :: Applicative Y where
  pure = Y
instance bindY :: Bind Y where
  (>>=) (Y y) f = f y
instance monadY :: Monad Y

instance semiringY :: (Semiring y) => Semiring (Y y) where
  (+) = lift2 (+)
  zero = Y zero
  (*) = lift2 (*)
  one = Y one
instance moduloSemiringY :: (ModuloSemiring y) => ModuloSemiring (Y y) where
  mod = lift2 mod
  (/) = lift2 (/)
instance ringY :: (Ring y) => Ring (Y y) where
  (-) = lift2 (-)
instance divisionRightY :: (DivisionRing y) => DivisionRing (Y y)
instance numY :: (Num y) => Num (Y y)

instance extendY :: Extend Y where
  (<<=) f m = Y (f m)

instance comonadY :: Comonad Y where
  extract (Y y) = y

instance foldableY :: Foldable Y where
  foldr f z (Y y) = f y z
  foldl f z (Y y) = f z y
  foldMap f (Y y) = f y

instance traversableY :: Traversable Y where
  traverse f (Y y) = Y <$> f y
  sequence (Y y) = Y <$> y

instance showY :: (Show y) => Show (Y y) where
  show = extract >>> show >>> (++) "Y "

instance eqY :: (Eq y) => Eq (Y y) where
  (==) = (==) `on` extract
  (/=) x y = not $ x == y

instance ordY :: (Ord a) => Ord (Y a) where
  compare (Y x) (Y y) = compare x y
