module Surface where

import Rays
import Vectors

import Data.Maybe (isJust)

type Color = (Double, Double, Double)

data Surface = Sphere Point Double Color
               deriving (Show)

center :: Surface -> Point
center (Sphere c _ _) = c

radius :: Surface -> Double
radius (Sphere _ r _) = r

color :: Surface -> Point -> Point -> [ Surface ] -> Color
color (Sphere c _ (r,g,b)) l p ss = (r * shade, g * shade, b * shade)
    where
      norm = pointsToDirection c p
      lVector = pointsToDirection p l
      shadeFactor = lVector `dot` norm
      shade = if shadeFactor < 0 || (any (isJust . ((Ray p lVector) `intersect`)) ss)
              then 0.20
              else (0.20 + 0.80 * shadeFactor)

intersect :: Ray -> Surface -> Maybe Point
intersect (Ray o dir) (Sphere c r _) =
    if v < 0 || disc < 0
    then Nothing
    else (Just $ o + ((v - dr) `mult` dir))
          where dr = sqrt disc
                disc = r ^ (2::Int) - ((dot oc oc) - v ^ (2::Int))
                v = dot oc dir
                oc = c - o -- gives a vector that is the same direction and magnitude as oc


