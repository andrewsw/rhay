module Main where

import Camera
import Rays
import Surface
import Scene

main :: IO ()
main = writeFile "test1.ppm" ppm
  where
    scene = Scene [ Sphere (mkPoint [1000,    0,    0]) 50.0 (1.0, 0.0, 0.0)
                  , Sphere (mkPoint [1000,  100,   50]) 50.0 (1.0, 1.0, 0.0)
                  , Sphere (mkPoint [1000,  200,  100]) 50.0 (1.0, 1.0, 1.0)
                  , Sphere (mkPoint [1000,  300,  150]) 50.0 (1.0, 0.0, 1.0)
                  , Sphere (mkPoint [1000,  400,  200]) 50.0 (0.0, 1.0, 1.0)
                  , Sphere (mkPoint [1000,  500,  250]) 50.0 (0.0, 0.0, 1.0)
                  ] (0.0, 0.0, 0.0) $ mkPoint [1000, -400, 200]
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


showColor :: Color -> String
showColor (r, g, b) = convert r ++ " " ++ convert g ++ " " ++ convert b ++ "\n"
  where
    convert = show . round . (* 255)
