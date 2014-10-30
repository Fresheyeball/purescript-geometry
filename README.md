# Module Documentation

## Module Data.Geometry

### Type Classes

    class Area g where
      area :: g -> Number

    class Perimeter g where
      perimeter :: g -> Number


## Module Data.Geometry.Circle

### Types

    data Circle a where
      Circle :: Point a -> a -> Circle


### Type Class Instances

    instance areaCircle :: Area (Circle Number)

    instance eqCircle :: (Eq a) => Eq (Circle a)

    instance hasPointsCircle :: HasPoints Circle

    instance perimeterCircle :: Perimeter (Circle Number)

    instance showCircle :: (Show a) => Show (Circle a)


### Values

    circumference :: Circle Number -> Number


## Module Data.Geometry.Dimension

### Types

    data Dimension a where
      Dimension :: { height :: a, width :: a } -> Dimension


### Type Classes

    class HasDimension a where
      dimensions :: a -> Dimension Number


### Type Class Instances

    instance areaDimensions :: Area (Dimension Number)

    instance eqDimension :: (Eq a) => Eq (Dimension a)

    instance functorDimension :: Functor Dimension

    instance showDimension :: (Show a) => Show (Dimension a)


## Module Data.Geometry.Line

### Types

    data Line a where
      Line :: Point a -> Point a -> Line


### Type Class Instances

    instance eqLine :: (Eq a) => Eq (Line a)

    instance functorLine :: Functor Line

    instance hasDimension :: HasDimension (Line Number)

    instance hasPointsLine :: HasPoints Line

    instance perimeterLine :: Perimeter (Line Number)

    instance showLine :: (Show a) => Show (Line a)


## Module Data.Geometry.Point

### Types

    data Point a where
      Point :: a -> a -> Point


### Type Classes

    class HasPoints g where
      points :: forall a. g a -> [Point a]


### Type Class Instances

    instance eqPoint :: (Eq a) => Eq (Point a)

    instance functorPoint :: Functor Point

    instance showPoint :: (Show a) => Show (Point a)


### Values

    (|*|) :: Number -> Point Number -> Point Number

    (|+|) :: Point Number -> Point Number -> Point Number

    (|-|) :: Point Number -> Point Number -> Point Number

    (|@|) :: Point Number -> Point Number -> Number

    dist :: Point Number -> Point Number -> Number

    distance :: Point Number -> Point Number -> Number

    l22dist :: Point Number -> Point Number -> Number


## Module Data.Geometry.Rect

### Types

    data Rect a where
      Rect :: Point a -> Point a -> Rect


### Type Class Instances

    instance areaRect :: Area (Rect Number)

    instance eqRect :: (Eq a) => Eq (Rect a)

    instance hasPointsRect :: HasPoints Rect

    instance perimeterRect :: Perimeter (Rect Number)

    instance showRect :: (Show a) => Show (Rect a)



