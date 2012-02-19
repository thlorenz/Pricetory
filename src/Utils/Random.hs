module Utils.Random (generateRandoms) where

import Random (randomR, Random, StdGen)

generateRandoms :: (Random a, Num a) => StdGen -> a -> a -> [b] -> [a]
generateRandoms gen min max = map fst . tail . scanl f (0, gen)
    where f (x, g) _ =  randomR (min, max) g
