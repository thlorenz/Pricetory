module Data.Sampler (getWorldOfTickData) where

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
import Contract.Protocol (decodeFileHeader, decodeTick, fileHeaderSize, getFullSymbolDataPath)
import Utils.Array (sampleAtInterval)

-- | TODO: handle Nothing which signifies an exception
readHeader ::  Handle -> IO Header
readHeader fh = liftM (fromJust . decodeFileHeader) $ L.hGet fh fileHeaderSize

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

getWorldOfTickData :: FilePath -> [Symbol] -> IO HistoricalTickDataMap
getWorldOfTickData dataDir = do
    tpls <- mapM (liftM toTuple . getHistoricalTickData . getFullSymbolDataPath dataDir)
    return $ liftM (HistoricalTickDataMap . Map.fromList) $ tpls
    where toTuple htd = (tickDataSymbol htd, htd)

-----------------------
-- ----  Tests  ---- --
-----------------------

debug = do

    tickData <- getHistoricalTickData $ getFullSymbolDataPath dataDir symbolName

    let hourly = (fromJust . Map.lookup secondsPerHour) $ tickDataByInterval tickData
    let daily = (fromJust . Map.lookup secondsPerDay) $ tickDataByInterval tickData

    putStrLn "Hourly Data:" 
    print $ (map decodeTick . elems) hourly
    putStrLn "Daily Data:" 
    print $ (map decodeTick . elems) daily
    
    putStrLn "\n--------------\nDone!"

    where dataDir = "/Users/thlorenz/dev/data/Pricetory"
          symbolName = "EURUSD"
          
main = debug
