-- module Data.Geometry.Point.Test where
--
-- import Test.QuickCheck
-- import Test.Classes
-- import Debug.Trace
--
-- import Data.Geometry.Axis.Test
-- import Data.Geometry.Point
-- import Data.Geometry.Axis
-- import Math
--
-- instance arbPoint :: Arbitrary Point where
--   arbitrary = Point <$> arbitrary <*> arbitrary
--
-- assert :: forall p. (Testable p) => p -> QC Unit
-- assert f = quickCheck' 1 f
--
-- sq :: forall a. (Semiring a) => a -> a
-- sq x = x * x
--
-- initt = do
--   trace "Point"
--
--   trace "distance"
--   quickCheck $ \x y x' y' ->
--     distance (Point x y) (Point x' y') == sqrt (sq (x' - x) `foldXY (+)` sq (y' - y))
--
--   trace "show"
--   let l =               Point (X 1) (Y 2)
--   assert $ (show l) == "Point (X 1) (Y 2)"
--
--   trace "eq"
--   quickCheck $ \x y ->
--     Point x y == Point x y && Point x y /= Point x (y + (Y 1))