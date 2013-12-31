module Main where

import Camera
import Rays
import Surface
import Scene

main :: IO ()
main = putStrLn "tracing..." >> writeFile "test1.ppm" ppm
  where
    image = renderScene testScene shortCamera
    ppm = makePPM image

testScene :: Scene
testScene = Scene [
  --    Sphere (mkPoint [1000,    0,    0]) 50.0 (1.0, 0.0, 0.0),
      -- Sphere (mkPoint [1000,  150,  -50]) 50.0 (1.0, 1.0, 0.0),
      -- Sphere (mkPoint [1000,  200,  100]) 50.0 (1.0, 1.0, 1.0),
      -- Sphere (mkPoint [1000,  300,  150]) 50.0 (1.0, 0.0, 1.0),
      -- Sphere (mkPoint [1000,  400,  200]) 50.0 (0.0, 1.0, 1.0),
      -- Sphere (mkPoint [1000,  500,  250]) 50.0 (0.0, 0.0, 1.0),
      Plane  (mkPoint [0, 0, 1]) 400.0 (0.5,1.0,0.5),
      Plane  (mkPoint [0, -1, 0]) 400.0 (1.0,0.5,0.5),
--      Plane  (mkPoint [0, 1, 0]) 400.0 (0.5,0.5,1.0),
      Plane  (mkPoint [-1, 0, 0]) 1500.0 (0.5,0.5,0.5),
      Plane (mkPoint [0, 0, -1]) 400.0 (1.0,0.5,1.0)
      ] (0.0, 0.0, 0.0) $ map mkPoint [
--      [100,-200,-350],
      [1000,-300,0]
--      [1000, -40000, 1000],
  --    [5000, 450, 2000]
      ]
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
