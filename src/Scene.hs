module Scene where

import Surface
import Rays
import Camera

import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)

data Scene = Scene [Surface]
data Image = Image Int Int [ Color ]

renderScene :: Scene -> Camera -> Image
renderScene (Scene ss) cam = Image (width cam) (height cam) $ map (renderPoint ss) $ rays cam

renderPoint :: [ Surface ] -> Ray -> Color
renderPoint ss ray = fromJust $ fromMaybe (Just (0,0,0)) $ find (Nothing /=) (map traceIntersections ss)
  where
    traceIntersections :: Surface -> Maybe Color
    traceIntersections s = case ray `intersect` s of
      Nothing -> Nothing
      Just _  -> Just $ color s

