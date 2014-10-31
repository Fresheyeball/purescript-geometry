module Main where

import Control.Apply((*>))

main = Data.Geometry.Rect.Test.init *> Debug.Trace.trace "what"
  -- Data.Geometry.Line.Test.init