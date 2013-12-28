module Scene where

import Surface
import Rays

import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)

type Grid = Maybe (Int, Color)

data Scene = Scene [Surface] Grid

black :: Color
black = (0, 0, 0)

renderScene :: Scene -> Int -> [ (Int, Int) ] -> [ Color ]
renderScene (Scene ss Nothing) d                 = map (renderPoint ss d (const False) (const black))
renderScene (Scene ss (Just (spacing, color))) d = map (renderPoint ss d gridTest gridColor)
  where gridTest (x,y) = (x `mod` spacing == 0) || (y `mod` spacing == 0)
        gridColor _    = color

renderPoint :: [ Surface ] -> Int -> ((Int, Int) -> Bool) -> ((Int, Int) -> Color) -> (Int, Int) -> Color
renderPoint ss d gt gc p@(x, y)
  | gt p = gc p
  | otherwise = fromJust $ fromMaybe (Just (0,0,0)) $ find (Nothing /=) (map traceIntersections ss)
  where
    traceIntersections :: Surface -> Maybe Color
    traceIntersections s = case ray `intersect` s of
      Nothing -> Nothing
      Just _  -> Just $ color s
    ray = Ray (mkPoint [0.0,0.0,0.0]) (mkDirection $ map fromIntegral [d,x,y])

