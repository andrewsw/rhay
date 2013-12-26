module Rays where

import Data.Vector
import Vectors

type Point = Vector Double

mkPoint :: [Double] -> Point
mkPoint = fromList

type Direction = Vector Double

mkDirection :: [Double] -> Point
mkDirection = normalize . fromList

-- Ray Origin Direction
data Ray = Ray Point Direction

origin :: Ray -> Point
origin (Ray p _) = p

direction :: Ray -> Direction
direction (Ray _ d) = d