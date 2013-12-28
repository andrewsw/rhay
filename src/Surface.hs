module Surface where

import Rays
import Vectors

type Color = (Double, Double, Double)

data Surface = Sphere Point Double Color

center :: Surface -> Point
center (Sphere c _ _) = c

radius :: Surface -> Double
radius (Sphere _ r _) = r

color :: Surface -> Color
color (Sphere _ _ cl) = cl

intersect :: Ray -> Surface -> Maybe Point
intersect (Ray o dir) (Sphere c r _) =
    if disc < 0
    then Nothing
    else (Just $ o + ((v - dr) `mult` dir))
          where dr = sqrt disc
                disc = r ^ (2::Int) - ((dot c c) - v ^ (2::Int))
                v = dot c dir


