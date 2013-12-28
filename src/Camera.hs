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
defaultCamera = mkCamera 800 600 2.0 (mkPoint [ 0, 0, 0 ])

mkCamera :: Int -> Int -> Double -> Point -> Camera
mkCamera w h f o = Camera f w h rs
  where
    aR = (fromIntegral w) / (fromIntegral h) -- assumes square pixels
    rs = map mkRay [ (fromIntegral x, fromIntegral y)
                   | y <- [(h - 1), (h - 2)..0]
                   , x <- [(w - 1), (w - 2)..0]
                   ]
    mkRay (x,y) = Ray o . normalize $
                  V.fromList [ 1.0
                             , ((x / fromIntegral w) - 0.5) -- scaled to [ -0.5,0.5 ]
                             , ((y / fromIntegral h) - 0.5) / aR -- scaled and aspect adjusted
                             ]
