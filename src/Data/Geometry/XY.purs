module Data.Geometry.XY where

import Data.Either
import Data.Function

newtype X = X Number
newtype Y = Y Number

getX :: X -> Number
getX (X x) = x

getY :: Y -> Number
getY (Y y) = y

liftX :: (Number -> Number) -> X -> X
liftX f = X <<< f <<< getX

liftY :: (Number -> Number) -> Y -> Y
liftY f = Y <<< f <<< getY

liftX2 :: (Number -> Number -> Number) -> X -> X -> X
liftX2 f (X x) (X x') = X $ x `f` x'

liftY2 :: (Number -> Number -> Number) -> Y -> Y -> Y
liftY2 f (Y y) (Y y') = Y $ y `f` y'

type Axis = Either X Y

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
