import Random (mkStdGen, randomRs)

getRandomIndexes gen max = randomRs (min, max) gen
    where min = (0 :: Int) 

main = do
    print $ take 20 $ getRandomIndexes (mkStdGen 200) 5
