## Geometry
[![Build Status](https://travis-ci.org/Fresheyeball/purescript-geometry.svg?branch=master)](https://travis-ci.org/Fresheyeball/purescript-geometry)
[![Bower version](https://badge.fury.io/bo/purescript-geometry.svg)](http://badge.fury.io/bo/purescript-geometry)
[![Dependency Status](https://www.versioneye.com/user/projects/546fe2ef81010687ac000605/badge.svg?style=flat)](https://www.versioneye.com/user/projects/546fe2ef81010687ac000605)


Simple geometry types and instances.

## Module Data.Geometry

### Type Classes

    class Area g where
      area :: g -> Number

    class Perimeter g where
      perimeter :: g -> Number


## Module Test.Classes

### Values

    checkApplicative :: forall f a b c. (Applicative f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), CoArbitrary a, Arbitrary b, Arbitrary a, Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC Unit

    checkFunctor :: forall f a. (Functor f, Arbitrary a, CoArbitrary a, Arbitrary (f a), Eq (f a)) => f a -> QC Unit

    checkMonad :: forall m a. (Monad m, Arbitrary a, CoArbitrary a, Arbitrary (m a), Eq (m a)) => m a -> QC Unit


## Module Data.Geometry.Circle.Test

### Type Class Instances

    instance arbCircle :: (Arbitrary a) => Arbitrary (Circle a)


### Values

    (=~=) :: Number -> Number -> Boolean


## Module Data.Geometry.Circle

### Types

    data Circle a where
      Circle :: Point a -> a -> Circle


### Type Class Instances

    instance applicativeCircle :: Applicative Circle

    instance applyCircle :: Apply Circle

    instance areaCircle :: Area (Circle Number)

    instance eqCircle :: (Eq a) => Eq (Circle a)

    instance functorCircle :: Functor Circle

    instance hasPointsCircle :: HasPoints Circle

    instance perimeterCircle :: Perimeter (Circle Number)

    instance showCircle :: (Show a) => Show (Circle a)


### Values

    circumference :: Circle Number -> Number


## Module Data.Geometry.Line.Test

### Type Class Instances

    instance arbLine :: (Arbitrary a) => Arbitrary (Line a)


### Values

    (=~=) :: Number -> Number -> Boolean

    mkLine :: Number -> Number -> Number -> Number -> Line Number


## Module Data.Geometry.Line

### Types

    data Line a where
      Line :: Point a -> Point a -> Line


### Type Class Instances

    instance applicativeLine :: Applicative Line

    instance applyLine :: Apply Line

    instance areaLine :: Area (Line Number)

    instance eqLine :: (Eq a) => Eq (Line a)

    instance functorLine :: Functor Line

    instance hasPointsLine :: HasPoints Line

    instance hasSizeLine :: HasSize (Line Number)

    instance perimeterLine :: Perimeter (Line Number)

    instance showLine :: (Show a) => Show (Line a)


## Module Data.Geometry.Point.Test

### Type Class Instances

    instance arbPoint :: (Arbitrary a) => Arbitrary (Point a)


## Module Data.Geometry.Point

### Types

    data Point a where
      Point :: a -> a -> Point


### Type Classes

    class HasPoints g where
      points :: forall a. g a -> [Point a]


### Type Class Instances

    instance applicativePoint :: Applicative Point

    instance applyPoint :: Apply Point

    instance eqPoint :: (Eq a) => Eq (Point a)

    instance functorPoint :: Functor Point

    instance showPoint :: (Show a) => Show (Point a)


### Values

    (|*|) :: Number -> Point Number -> Point Number

    (|+|) :: Point Number -> Point Number -> Point Number

    (|-|) :: Point Number -> Point Number -> Point Number

    (|@|) :: Point Number -> Point Number -> Number

    distance :: Point Number -> Point Number -> Number

    l22dist :: Point Number -> Point Number -> Number


## Module Data.Geometry.Rect.Test

### Type Class Instances

    instance arbRect :: (Arbitrary a) => Arbitrary (Rect a)


## Module Data.Geometry.Rect

### Types

    data Rect a where
      Rect :: Point a -> Point a -> Rect


### Type Class Instances

    instance applicative :: Applicative Rect

    instance applyRect :: Apply Rect

    instance areaRect :: Area (Rect Number)

    instance eqRect :: (Eq a) => Eq (Rect a)

    instance functorRect :: Functor Rect

    instance hasPointsRect :: HasPoints Rect

    instance hasSizeRect :: HasSize (Rect Number)

    instance perimeterRect :: Perimeter (Rect Number)

    instance showRect :: (Show a) => Show (Rect a)


## Module Data.Geometry.Size.Test

### Type Class Instances

    instance arbSize :: (Arbitrary a) => Arbitrary (Size a)


## Module Data.Geometry.Size

### Types

    data Size a where
      Size :: { height :: a, width :: a } -> Size


### Type Classes

    class HasSize a where
      size :: a -> Size Number


### Type Class Instances

    instance applicativeSize :: Applicative Size

    instance applySize :: Apply Size

    instance areaSizes :: Area (Size Number)

    instance eqSize :: (Eq a) => Eq (Size a)

    instance functorSize :: Functor Size

    instance showSize :: (Show a) => Show (Size a)


### Values

    getHeight :: forall a. Size a -> a

    getWidth :: forall a. Size a -> a



