module Camera where

import Rays
import Vectors

import qualified Data.Vector as V (fromList)

data Camera = Camera { focalLength :: Double
                     , width       :: Int
                     , height      :: Int
                     , rays        :: [ Ray ]
                     }
            deriving (Show)


defaultCamera :: Camera
defaultCamera = Camera f w h rs
  where
    w = 800
    h = 600
    f = 2.0
    o = mkPoint [ 0, 0, 0 ]
    rs = map mkRay [ (fromIntegral x, fromIntegral y)
                   | y <- [(h - 1), (h - 2)..0]
                   , x <- [(w - 1), (w - 2)..0]
                   ]
    mkRay (x,y) = Ray o . normalize $ V.fromList [ 1.0, (x / fromIntegral w) - 0.5,(y / fromIntegral h) - 0.5]
