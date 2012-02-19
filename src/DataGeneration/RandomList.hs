import Random (randomR, Random, mkStdGen, StdGen)


generateRandomList :: (Random a, Num a) => StdGen -> a -> a -> [b] -> [a]
generateRandomList gen min max = map fst . tail . scanl f (0, gen)
    where f (x, g) _ =  randomR (min, max) g

getRandomIndexes gen max = generateRandomList gen (0 :: Int) max [0..]

main = do
    print $ take 20 $ getRandomIndexes (mkStdGen 200) 5
