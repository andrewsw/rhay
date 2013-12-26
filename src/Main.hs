module Main where

import Rays
import Surface
import Scene
import Data.List (find)
import Data.Maybe

main :: IO ()
main = do
  print $ take 10 points
  print $ map (\(l,r) -> mkDirection $ map fromIntegral [distance,l,r]) $ take 10 points
  print $ map (render scene distance) $ take 10 points
  print $ render scene distance (0,0)
  writeFile "test1.ppm" ppm

  where
    width = 640
    height = 480
    distance = 200
    scene = Scene [ Sphere (mkPoint [2000,   0,   0]) 100.0 (100,0,0)
                  , Sphere (mkPoint [2000, 400, 400]) 100.0 (100,0,0)
                  , Sphere (mkPoint [2000, 800, 800]) 100.0 (100,0,0)
                  , Sphere (mkPoint [2000, 1200, 1200]) 100.0 (100,0,0)
                  , Sphere (mkPoint [2000, 1600, 1600]) 100.0 (100,0,0)
                  , Sphere (mkPoint [2000, 2000, 2000]) 100.0 (100,0,0)
                  ]
    maxX = width `div` 2
    minX = -maxX + 1
    maxY = height `div` 2
    minY = -maxY + 1
    points = [ (x,y) | y <- [maxY, maxY-1..minY], x <- [maxX, maxX-1..minX] ]
    colors = map (render scene distance) points
    ppm = makePPM width height colors

gridSpacing = 20
gridColor =(100,100,100)

render :: Scene -> Int -> (Int, Int) -> (Int, Int, Int)
render _ _ (0,_) = (255,255,255)
render _ _ (_,0) = (255,255,255)
render (Scene ss) d (x, y)  | (x `mod` gridSpacing == 0) || (y `mod` gridSpacing == 0) = gridColor
                            | otherwise = fromJust $ fromMaybe (Just (0,0,0)) $ find (Nothing /=) (map traceIntersections ss)
  where
    traceIntersections :: Surface -> Maybe Color
    traceIntersections s = case intersect s ray of
      Nothing -> Nothing
      Just _  -> Just $ color s
    ray = Ray (mkPoint [0.0,0.0,0.0]) (mkDirection $ map fromIntegral [d,x,y])


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
