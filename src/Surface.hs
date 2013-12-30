module Surface where

import Rays
import Vectors

import Data.Maybe (isJust)

type Color = (Double, Double, Double)

mergeColors :: Color -> Color -> Color
mergeColors (r1, g1, b1) (r2, g2, b2) = (chop (r1 + r2), chop (g1 + g2), chop (b1 + b2))
     where chop x = if x > 1.0 then 1.0 else max 0.0 x

data Surface = Sphere Point Double Color
             | Plane Point Double Color
               deriving (Eq, Show)

center :: Surface -> Point
center (Sphere c _ _) = c

radius :: Surface -> Double
radius (Sphere _ r _) = r

color :: Surface -> [Surface] -> [Point] -> Point -> Color
color (Sphere c _ (r,g,b)) ss ls p =
    foldl mergeColors (0.0, 0.0, 0.0) $ map (\lv -> (r * shade lv, g * shade lv, b * shade lv)) lVectors
    where
      norm = pointsToDirection c p
      lVectors = map (pointsToDirection p) ls
      shade = cosShade p norm ss
color this@(Plane n _ (r,g,b)) ss ls p =
    foldl mergeColors (0.0, 0.0, 0.0) $ map (\lv -> (r * shade lv, g * shade lv, b * shade lv)) lVectors
    where
      norm = n
      lVectors = map (pointsToDirection p) ls
      shade = cosShade p norm $ filter (/= this) ss


cosShade :: Point -> Point -> [Surface] -> Point -> Double
cosShade o norm ss lv = if shadeFactor < 0 || (any (isJust . ((Ray o lv) `intersect`)) ss)
                   then 0.0
                   else shadeFactor
                       where shadeFactor = lv `dot` norm

intersect :: Ray -> Surface -> Maybe Point
intersect (Ray o dir) (Sphere c r _) =
    if v < 0 || disc < 0
    then Nothing
    else (Just $ o + ((v - dr) `mult` dir))
          where dr = sqrt disc
                disc = r ^ (2::Int) - ((dot oc oc) - v ^ (2::Int))
                v = dot oc dir
                oc = c - o -- gives a vector that is the same direction and magnitude as oc
intersect (Ray o dir) (Plane n d _) =
  if abs vd <= 0.0001 || t < 0
  then Nothing
  else (Just $ o + (t `mult` dir))
       where
         vd = n `dot` dir
         t = v0 / vd
         v0 = -((n `dot` o) + d)


applyAmbient :: Color -> Color
applyAmbient = mergeColors (0.2, 0.2, 0.2)
