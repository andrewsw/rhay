module Surface where

import Rays
import Vectors

type Color = (Double, Double, Double)

data Surface = Sphere Point Double Color
               deriving (Show)

center :: Surface -> Point
center (Sphere c _ _) = c

radius :: Surface -> Double
radius (Sphere _ r _) = r

color :: Surface -> Point -> Point -> Color
color (Sphere c _ (r,g,b)) l p = (r * shade, g * shade, b * shade)
    where
      norm = pointsToDirection c p
      lVector = pointsToDirection c l
      shadeFactor = lVector `dot` norm
      shade = if shadeFactor < 0
              then 0.20
              else (0.20 + 0.80 * shadeFactor)

intersect :: Ray -> Surface -> Maybe Point
intersect (Ray o dir) (Sphere c r _) =
    if disc < 0
    then Nothing
    else (Just $ o + ((v - dr) `mult` dir))
          where dr = sqrt disc
                disc = r ^ (2::Int) - ((dot c c) - v ^ (2::Int))
                v = dot c dir


