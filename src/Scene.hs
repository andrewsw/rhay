module Scene where

import Surface
import Rays
import Camera
import Vectors

import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Ord  (comparing)

-- A scene has surfaces, a background color and lights.
data Scene = Scene [ Surface ] Color [ Point ]
             deriving (Show)

data Image = Image Int Int [ Color ]

renderScene :: Scene -> Camera -> Image
renderScene s cam = Image (width cam) (height cam) $ map (renderPoint s) $ rays cam

renderPoint :: Scene -> Ray -> Color
renderPoint (Scene ss bg ls) ray@(Ray o _) = case intersections of
                                                  [] -> bg
                                                  is -> let (p, s) = firstPoint is
                                                        in color s ss ls p
  where
    firstPoint ps = head $ sortBy (comparing (distance o . fst)) ps

    intersections = map fromJust $ filter (Nothing /=) (map (traceIntersections ray) ss)

traceIntersections :: Ray -> Surface -> Maybe (Point, Surface)
traceIntersections ray s = case ray `intersect` s of
      Nothing -> Nothing
      Just p -> Just (p, s)

distance :: Point -> Point -> Double
distance p1 p2 = mag (p2 - p1)

