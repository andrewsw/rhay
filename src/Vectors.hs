module Vectors where

import Data.Vector as V

-- basic arithmetic manipulation of vectors
instance Num a =>  Num (Vector a) where
  (+) l r = V.zipWith (+) l r
  (*) l r = V.zipWith (*) l r

-- dot product, very useful
dot :: Num a => Vector a -> Vector a -> a
dot l r = V.sum (l * r)

mag :: Floating a => Vector a -> a
mag v = sqrt $ dot v v

normalize :: Floating a => Vector a -> Vector a
normalize v = V.map (/  mag v) v

mult :: Floating a => a -> Vector a -> Vector a
mult k v = V.map (* k) v