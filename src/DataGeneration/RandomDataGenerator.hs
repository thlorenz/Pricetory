import qualified Data.ByteString.Lazy as L

import Random (randomRs, RandomGen, Random, mkStdGen, newStdGen)

import Data.Maybe
import Data.Word
import Data.Binary (encode)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO

import System.Console.CmdArgs

import Contract.Types
import Contract.Symbols (symbolToCode)
import Contract.Protocol (encodeFileHeader, encodeTick)

import qualified DataGeneration.CmdArgs as A

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
    args <- cmdArgs A.arguments
    putStrLn $ "Generating for: " ++ show args ++ "\n"
    startTime <- getCurrentTime

    let fileName = (A.directory args) ++ "/" ++ (A.symbol args) ++ ".bin"

    let header = encodeFileHeader $ 
                 Header (fromJust . symbolToCode $ A.symbol args) 
                        (A.time args) 
                        (A.interval args) 
                        (A.points args)

    let encodedTicks = map encodeTick $ take (fromIntegral $ A.points args) $ 
                       getTicks (Tick (A.time args)  $ A.firstRate args) 
                                (getRandomRateDeltas $ mkStdGen (A.random args))
                                (A.interval args)

    L.writeFile fileName header
    L.appendFile fileName $ L.concat encodedTicks

    endTime <- getCurrentTime

    putStrLn $ "Took: " ++ show (diffUTCTime endTime startTime)
    
