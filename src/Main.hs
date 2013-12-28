module Main where

import Camera
import Rays
import Surface
import Scene

main :: IO ()
main = writeFile "test1.ppm" ppm
  where
    scene = Scene [ Sphere (mkPoint [2000,    0,    0]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  100,    0]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  200,  200]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  300,  300]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  400,  400]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  500,  500]) 25.0 (255,0,0)
                  ]
    image = renderScene scene defaultCamera
    ppm = makePPM image

makePPM :: Image -> String
makePPM (Image w h colors) =
  concat [ "P3\n"
         , show w, "\n"
         , show h, "\n"
         , "255\n"
         ]
         ++ concatMap showColor colors


showColor :: (Int, Int, Int) -> String
showColor (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
