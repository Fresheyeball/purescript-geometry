module Data.Geometry.Size where

import Data.Geometry

data Size = Size { width  :: Number
                 , height :: Number }

newSize w h = Size { width : w, height : h }

class GetSize a where
  getSize :: a -> Size

getWidth :: Size -> Number
getWidth (Size { width = w }) = w

getHeight :: Size -> Number
getHeight (Size { height = h }) = h

instance areaSize :: Area Size where
  area (Size { width = w, height = h }) = w * h

instance showSize :: Show Size where
  show (Size { width = w, height = h })
     = "Size { width : " ++ show w ++
           ", height : " ++ show h ++ " }"

mapSize :: (Number -> Number) -> Size -> Size
mapSize f (Size { width =    w,  height =    h  })
         = Size { width : (f w), height : (f h) }

pureSize :: Number -> Size
pureSize x = newSize x x

instance eqSize :: Eq Size where
  (==) (Size { width = w,  height = h  })
       (Size { width = w', height = h' })
    = w == w' && h == h'
  (/=) x y = not $ x == y
