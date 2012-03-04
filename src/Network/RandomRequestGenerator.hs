module Network.RandomRequestGenerator (getRandomRequests) where

import Random (randomR, RandomGen, StdGen, Random, mkStdGen, getStdGen)
import Data.Word
import Control.Monad (liftM)    
import Contract.Types
import Contract.Constants

instruments = 1 :: SymbolCode -- TODO: use 3
maxSeconds = secondsPerDay
oneSecProb = 5
oneMinProb = 40
oneHourProb = 35

entireDataSecs = secondsPerDay * daysPerYear :: TimeOffset

data Bound = Bound 
    { bndInterval :: TimeInterval
    , bndTimeSpan :: TimeInterval
    }

{-  Bounds are setup to reflect the following distribution:

    Interval:       1 sec       1 min       1 hour
    Probability:    5%          40%         35%
    Max Timespan:   15 mins     600 mins    1 year
    Max Read Ticks: 900         600         8750
    Faults to Disk: N           N           Y -}

bounds =  -- (replicate oneSecProb secondsBnds) ++
           (replicate oneMinProb minutesBnds) ++
           (replicate oneHourProb hoursBnds)
    where
        secondsBnds = Bound (1 :: Word32) (15 * secondsPerMinute)
        minutesBnds = Bound secondsPerMinute (600 * secondsPerMinute)
        hoursBnds   = Bound (secondsPerDay * daysPerYear) (secondsPerDay * daysPerYear)

getRandomRequests :: IO [Request]
getRandomRequests = liftM getRequestStream $ getStdGen
    where getRequestStream :: StdGen -> [Request]
          getRequestStream g = let (request, nextGen) = generateRequest g 
                               in request : getRequestStream nextGen

generateRequest :: StdGen -> (Request, StdGen)
generateRequest g = (req, g3)
    where 
        req = Request symCode 
                      startOffset 
                      (startOffset + bndTimeSpan bound) 
                      (bndInterval bound)

        (symCode, g1)     = randSymbolCode g :: (SymbolCode, StdGen)
        (bound,   g2)     = let (index, bg) = randomR (0, length bounds - 1) g1
                            in (bounds !! index, bg)
        (startOffset, g3) = randStartOffset entireDataSecs (bndTimeSpan bound) g2

randSymbolCode :: StdGen -> (SymbolCode, StdGen)
randSymbolCode g = (fromIntegral . fst $ res, snd res)
    where res = randomR (1, fromIntegral instruments) g :: (Int, StdGen)

randTimeInterval :: (TimeInterval, TimeInterval) -> StdGen -> (TimeInterval, StdGen)
randTimeInterval (min, max) g = (fromIntegral . fst $ res, snd res)
    where res = randomR (fromIntegral min, fromIntegral max) g :: (Int, StdGen)

randStartOffset :: TimeOffset -> TimeInterval -> StdGen -> (TimeOffset, StdGen)
randStartOffset eofOffset len g = (fromIntegral . fst $ res, snd res)
    where max = eofOffset - len
          res = randomR (fromIntegral min, fromIntegral max) g :: (Int, StdGen)
          min = 1

main = getStdGen >>= print . generateRequest
