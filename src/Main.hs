module Main where

import Rays
import Surface
import Scene
import Data.List (find)
import Data.Maybe

main :: IO ()
main = writeFile "test1.ppm" ppm
  where
    width = 640
    height = 480
    distance = 800
    scene = Scene [ Sphere (mkPoint [2000,    0,    0]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  100,  100]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  200,  200]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  300,  300]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  400,  400]) 25.0 (255,0,0)
                  , Sphere (mkPoint [2000,  500,  500]) 25.0 (255,0,0)
                  ] $ Just (gridSpacing, gridColor)
    maxX = width `div` 2
    minX = -maxX + 1
    maxY = height `div` 2
    minY = -maxY + 1
    points = [ (x,y) | y <- [maxY, maxY-1..minY], x <- [maxX, maxX-1..minX] ]
    colors = renderScene scene distance points
    ppm = makePPM width height colors

gridSpacing = 20
gridColor =(100,100,100)


makePPM :: Int -> Int -> [(Int, Int, Int)] -> String
makePPM width height colors =
  concat [ "P3\n"
         , show width, "\n"
         , show height, "\n"
         , "255\n"
         ]
         ++ concatMap showColor colors


showColor :: (Int, Int, Int) -> String
showColor (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
