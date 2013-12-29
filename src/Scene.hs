module Scene where

import Surface
import Rays
import Camera

import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)

data Scene = Scene [Surface] Color Point

data Image = Image Int Int [ Color ]

renderScene :: Scene -> Camera -> Image
renderScene s cam = Image (width cam) (height cam) $ map (renderPoint s) $ rays cam

renderPoint :: Scene -> Ray -> Color
renderPoint (Scene ss bg l) ray = fromJust $ fromMaybe (Just bg) $ find (Nothing /=) (map traceIntersections ss)
  where
    traceIntersections :: Surface -> Maybe Color
    traceIntersections s = case ray `intersect` s of
      Nothing -> Nothing
      Just p  -> Just $ color s l p

