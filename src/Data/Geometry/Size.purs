module Data.Geometry.Size where

import Data.Geometry

data Size a = Size { width  :: a
                   , height :: a }

getWidth :: forall a. Size a -> a 
getWidth (Size { width = w }) = w

getHeight :: forall a. Size a -> a
getHeight (Size { height = h }) = h 

class HasSize a where
  size :: a -> Size Number

instance areaSizes :: Area (Size Number) where
  area (Size { width = w, height = h }) = w * h

instance showSize :: (Show a) => Show (Size a) where
  show (Size { width = w, height = h })
     = "Size { width  : " ++ show w ++ 
            ", height : " ++ show h ++ "}"

instance functorSize :: Functor Size where
  (<$>) f (Size { width = w,     height = h     }) 
         = Size { width : (f w), height : (f h) }

instance eqSize :: (Eq a) => Eq (Size a) where
  (==) (Size { width = w,  height = h  }) 
       (Size { width = w', height = h' }) 
    = w == w' && h == h'
  (/=) x y = not $ x == y