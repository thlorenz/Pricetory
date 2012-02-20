{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.ByteString.Lazy as L

import Random (randomRs, RandomGen, Random, mkStdGen, newStdGen)

import Data.Maybe
import Data.Word
import Data.Binary (encode)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO

import System.Console.CmdArgs

import Contract.Types
import Contract.Symbols (getSymbolCode, eurusd)
import Contract.Protocol (createFileHeader)

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

encodeTick tick = L.append (encode $ timeOffset tick) (encode $ rate tick)

data Arguments = Arguments { symbol    :: Symbol
                           , time      :: TimeOffset
                           , interval  :: TimeInterval
                           , firstRate :: Rate
                           , points    :: Word32
                           , directory :: FilePath
                           } deriving (Show, Data, Typeable)

-- | Defaults to EURUSD starting at 0, 1 tick/sec for a year (31556926s) into data directory
arguments = Arguments 
    { symbol = eurusd    &= typ "Symbol"       &= help "Symbol for which to generate data"
    , time = 0           &= typ "TimeOffset"   &= help "Time of first tick"
    , interval = 1       &= typ "TimeInterval" &= help "Seconds between ticks"
    , firstRate = 13245  &= typ "Rate"         &= help "Rate of first tick"
    , points = defPts    &= typ "Int"          &= help "Number of ticks to generate"
    , directory = defDir &= typ "FilePath"     &= help "Directory into which to output binary file"
    } &= summary "Random Tick Data Generator version 0.0.1"
    where defPts = 100000 
          defDir = "/Users/thlorenz/dev/data/Pricetory"
main = do
    args <- cmdArgs arguments
    putStrLn $ "Generating for: " ++ show args ++ "\n"
    startTime <- getCurrentTime

    let fileName = (directory args) ++ "/" ++ (symbol args) ++ ".bin"

    let header = createFileHeader (getSymbolCode $ symbol args) 
                                  (time args) 
                                  (interval args) 
                                  (points args)

    let encodedTicks = map encodeTick $ take (fromIntegral $ points args) $ 
                       getTicks (Tick (time args)  $ firstRate args) 
                                (getRandomRateDeltas $ mkStdGen 100)
                                (interval args)

    L.writeFile fileName header
    L.appendFile fileName $ L.concat encodedTicks

    endTime <- getCurrentTime

    putStrLn $ "Took: " ++ show (diffUTCTime endTime startTime)
    
