module Rays where

import Data.Vector
import Vectors

type Point = Vector Double

mkPoint :: [Double] -> Point
mkPoint = fromList

type Direction = Vector Double

mkDirection :: [Double] -> Point
mkDirection = normalize . fromList

pointsToDirection :: Point -> Point -> Direction
pointsToDirection p1 p2 = normalize (p2 - p1)

-- Ray Origin Direction
data Ray = Ray Point Direction
           deriving (Show)

origin :: Ray -> Point
origin (Ray p _) = p

direction :: Ray -> Direction
direction (Ray _ d) = d

distance :: Point -> Point -> Double
distance p1 p2 = mag (p2 - p1)
