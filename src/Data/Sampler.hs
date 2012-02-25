import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Map as Map

import System.IO
import Data.Word
import Data.Maybe (fromJust)
import Data.Array
import Control.Monad (liftM)

import Contract.Types
import Contract.Constants
import Contract.Protocol (decodeFileHeader, decodeTick)
import Utils.Array (sampleAtInterval)

-- | Converts a list of ByteStrings into a list of Ticks, assuming that interval is 1 sec.
getSecondIntervalData :: [L.ByteString] -> Word32 -> [Tick]
getSecondIntervalData bs points = undefined

decodeTicks :: [L.ByteString] -> [Tick]
decodeTicks = undefined

-- | TODO: handle Nothing which signifies an exception
readHeader ::  Handle -> IO Header
readHeader fh = liftM (fromJust . decodeFileHeader) $ L.hGet fh bytesInHeader
    where bytesInHeader = 5 * word

{- | Samples byte strings from a given file handle
     h          - the file handle
     maxPoint   - the point after which to stop reading (usually the end of the file)
     interval   - interval at which to read points -}
sampleByteStrings :: Handle -> Int -> Int -> Int -> IO (Array Int L.ByteString)
sampleByteStrings h maxPoint interval pointSize = 
    liftM (listArray (0, points - 1)) $ sample points []
    where points = maxPoint `div` interval
          skipSize = pointSize * (interval - 1)
          sample remainingPoints xs = do 
              x <- L.hGet h pointSize
              if remainingPoints >= 0 then do 
                   hSeek h RelativeSeek $ fromIntegral skipSize
                   sample (remainingPoints - 1) (x:xs)
              else return $ reverse xs

getFullSymbolDataPath :: FilePath -> Symbol -> FilePath
getFullSymbolDataPath dataDir symbolName = dataDir ++ "/" ++ symbolName ++ ".bin"

readTickDataAtInterval :: FilePath -> TimeInterval -> IO ((Array Int L.ByteString), Header)
readTickDataAtInterval fullPath interval = do
    h <- openBinaryFile fullPath ReadMode
    hdr <- readHeader h
    sample <- sampleByteStrings h (fromIntegral $ points hdr) (fromIntegral interval) tickSize 
    hClose h
    return (sample, hdr)

getHistoricalTickData :: FilePath -> IO HistoricalTickData
getHistoricalTickData fullPath = do 
    (byMinute, hdr) <- readTickDataAtInterval fullPath secondsPerMinute 
    let byHour = sampleAtInterval (fromIntegral minutesPerHour) byMinute
    let byDay  = sampleAtInterval (fromIntegral hoursPerDay) byHour 
    return $ HistoricalTickData 
                (symbol hdr) 
                (Map.fromList [ (secondsPerMinute, byMinute)
                              , (secondsPerHour, byHour)
                              , (secondsPerDay, byDay)
                              ])

main = do

    tickData <- getHistoricalTickData $ getFullSymbolDataPath dataDir symbolName
    let hourly = (fromJust . Map.lookup secondsPerHour) $ tickDataByInterval tickData
    let ticks = (map decodeTick . elems) hourly
    print ticks

    
    putStrLn "\n--------------\nDone!"

    where dataDir = "/Users/thlorenz/dev/data/Pricetory"
          symbolName = "EURUSD"

-- Reader monad via data-ivar, write-once variables
-- http://hackage.haskell.org/packages/archive/data-ivar/0.30/doc/html/Data-IVar.html
