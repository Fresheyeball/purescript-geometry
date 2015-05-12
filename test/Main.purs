module Test.Main where

import Control.Monad.Eff

main = do
  Data.Geometry.Rect.Test.init
  Data.Geometry.Line.Test.init
  Data.Geometry.Point.Test.initt
  Data.Geometry.Circle.Test.inittt
  Data.Geometry.Size.Test.init
