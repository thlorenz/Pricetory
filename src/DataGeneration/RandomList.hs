import Random (mkStdGen)
import Utils.Random (generateRandoms)

getRandomIndexes gen max = generateRandoms gen (0 :: Int) max [0..]

main = do
    print $ take 20 $ getRandomIndexes (mkStdGen 200) 5
