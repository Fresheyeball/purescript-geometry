module Data.Geometry.Axis where

import Data.Either
import Data.Function

newtype X = X Number
newtype Y = Y Number

class GetX a where
  getX :: a -> X

class GetY a where
  getY :: a -> Y

runX :: X -> Number
runX (X x) = x

runY :: Y -> Number
runY (Y y) = y

liftX :: (Number -> Number) -> X -> X
liftX f = X <<< f <<< runX

liftY :: (Number -> Number) -> Y -> Y
liftY f = Y <<< f <<< runY

liftX2 :: (Number -> Number -> Number) -> X -> X -> X
liftX2 f (X x) (X x') = X $ x `f` x'

liftY2 :: (Number -> Number -> Number) -> Y -> Y -> Y
liftY2 f (Y y) (Y y') = Y $ y `f` y'

type Axis = Either X Y

foldXY :: (Number -> Number -> Number) -> X -> Y -> Number
foldXY f (X x) (Y y) = x `f` y

foldYX :: (Number -> Number -> Number) -> Y -> X -> Number
foldYX f = flip $ foldXY f

instance semiringX :: Semiring X where
  (+) = liftX2 (+)
  zero = X 0
  (*) = liftX2 (*)
  one = X 1

instance moduloSemiringX :: ModuloSemiring X where
  mod = liftX2 mod
  (/) = liftX2 (/)

instance ringX :: Ring X where
  (-) = liftX2 (-)

instance divisionRightX :: DivisionRing X
instance numX :: Num X

instance semiringY :: Semiring Y where
  (+) = liftY2 (+)
  zero = Y 0
  (*) = liftY2 (*)
  one = Y 1

instance moduloSemiringY :: ModuloSemiring Y where
  mod = liftY2 mod
  (/) = liftY2 (/)

instance ringY :: Ring Y where
  (-) = liftY2 (-)

instance divisionRightY :: DivisionRing Y
instance numY :: Num Y

instance showX :: Show X where
  show = runX >>> show >>> (++) "X "

instance showY :: Show Y where
  show = runY >>> show >>> (++) "Y "

instance eqX :: Eq X where
  (==) = (==) `on` runX
  (/=) x y = not $ x == y

instance eqY :: Eq Y where
  (==) = (==) `on` runY
  (/=) x y = not $ x == y
