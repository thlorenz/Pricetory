import Random (randomRs, RandomGen, Random, mkStdGen, newStdGen)
import Data.Maybe
import Data.Word

import Contract.Types
import Contract.Symbols

getRandomRateDeltas :: (RandomGen g) => g -> [Rate]
getRandomRateDeltas g = [deltas !! x | x <- indexes]
    where
        indexes = randomRs (0, upperBound) g
        upperBound = (length deltas) - 1 

        -- smaller rate changes occur more often
        deltas :: [Rate]
        deltas = 
                                  (replicate 8 0) ++
            (replicate 5 (-1)) ++ (replicate 5 1) ++
            (replicate 3 (-2)) ++ (replicate 3 2) ++
            (replicate 2 (-3)) ++ (replicate 2 3) ++
            [4, (-4), 5, (-5)]

getTicks :: Tick -> [Rate] -> TimeOffset -> [Tick]
getTicks prevTick rateDeltas timeInterval = x : getTicks x (tail rateDeltas) timeInterval
    where 
        x = getDeltaTick prevTick (head rateDeltas)

        getDeltaTick currentTick rateDelta = 
                  let nextTimeOffset = (+timeInterval) $ timeOffset currentTick
                      nextRate = (+rateDelta) $ rate currentTick
                  in  Tick nextTimeOffset nextRate 

main = do
    let initialRate = 1234500
    let rt = take 50 $ 
             getTicks (Tick 0 initialRate) (getRandomRateDeltas $ mkStdGen 100) 1
    print rt
