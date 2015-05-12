module Data.Geometry.Size.Test where

import Data.Geometry
-- import Data.Geometry.Rect
import Data.Geometry.Point
import Data.Geometry.Point.Test
import Data.Geometry.Line
import Data.Geometry.Size

import Test.QuickCheck
import Test.Classes
import Debug.Trace

instance arbSize :: Arbitrary Size where
  arbitrary = newSize <$> arbitrary <*> arbitrary

init = do
  trace "Size"

  trace "getWidth getHeight"
  quickCheck $ \h w ->
    let s = Size {
          height   :  h,      width   :  w } :: Size
    in getHeight s == h && getWidth s == w

  trace "area"
  quickCheck $ \s -> area s == (getWidth s * getHeight s)

  trace "show"
  let s' =               Size { width : 0, height : 0 }
  assert $ (show s') == "Size { width : 0, height : 0 }"

  let
    checkEq :: Size -> Number -> Boolean
    checkEq s a =
      let s' = newSize (getWidth s) (getHeight s + a)
      in  s == s && s /= s' && s' /= s
  trace "eq"
  quickCheck checkEq
