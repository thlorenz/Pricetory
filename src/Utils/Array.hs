module Utils.Array (sampleAtInterval) where

import Data.Array

sampleAtInterval :: Int -> Array Int a -> Array Int a
sampleAtInterval interval array = listArray (min, max) $ sample min []
    where min = (fst . bounds) array
          sourceMax = (snd . bounds) array 
          max = sourceMax `div` interval
          sample i xs 
            | i <= sourceMax  = sample (i + interval) $ (array ! i) : xs
            | otherwise = reverse xs

