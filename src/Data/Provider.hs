module Data.Provider (provide, provideFromFile) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

import Text.Printf

import System.IO (hSeek, Handle, SeekMode(..))

import Data.Array (elems, ixmap, listArray, (!), bounds)
import Data.List (sort)
import Data.Maybe

import Contract.Types
import Contract.Protocol (decodeTick)
import Control.Arrow ((>>>))

import Test.HUnit

provide :: HistoricalTickDataMap -> SymbolCode -> TimeInterval -> TimeOffset -> TimeOffset -> ProvidedTickData 
provide allTickData code fromTime toTime interval = 
    ProvidedTickData fromIndex toIndex key byteStrings
    where 
        fromIndex   =  getIndex fromTime
        toIndex     =  getIndex toTime
        (min, max)  =  bounds matchingTickData 
        key         =  negotiateUp interval (Map.keys tickDataForCode)
        byteStrings =  elems . ixmap (fromIndex, toIndex) id $ matchingTickData

        getIndex = fromIntegral 
                   >>> getIndexForTime startTime interval 
                   >>> validIndex (bounds matchingTickData) 

        startTime = timeOffset . decodeTick . (! 0) $ matchingTickData
        
        matchingTickData = fromJust . Map.lookup key $ tickDataForCode 
        tickDataForCode = tickDataByInterval . getAllHistTickDataForCode $ allTickData
        getAllHistTickDataForCode = fromJust . Map.lookup code . historicalTickDataBySymbol 

provideFromFile :: TimeOffset -> TimeOffset -> Handle -> IO L.ByteString
provideFromFile start end h = do
    hSeek h AbsoluteSeek $ fromIntegral start
    L.hGet h bytesToGet 
    where bytesToGet = fromIntegral (end - start)
    
validIndex :: (Int, Int) -> Int -> Int
validIndex (minIndex, maxIndex) index = max minIndex . min maxIndex $ index 

negotiateUp :: (Ord a) => a -> [a] -> a
negotiateUp desired availables
    | min >= desired = min
    | max <= desired = max
    | otherwise      = closest desired $ sort availables
    where 
        min = minimum availables
        max = maximum availables
        closest d (x:xs) 
          | d <= x    = x
          | otherwise = closest d xs
          
getIndexForTime :: TimeOffset -> TimeInterval -> TimeOffset -> Int
getIndexForTime startTime interval desiredTime = if idx >= 0 then idx else 0
    where idx = offsetFromStart `div` iv 
          offsetFromStart = dt - st
          st = fromIntegral startTime
          dt = fromIntegral desiredTime
          iv = fromIntegral interval

-----------------------
-- ----  Tests  ---- --
-----------------------

negotiateUpTests =
    [ assertEqual "negotiateUp 2 in [1, 2, 3] returns 2" 2 $ negotiateUp 2 [1, 2, 3]
    , assertEqual "negotiateUp 2 in [1, 3, 4] returns 3" 3 $ negotiateUp 2 [1, 3, 4]
    , assertEqual "negotiateUp 0 in [1, 2, 3] returns 1" 1 $ negotiateUp 0 [1, 3, 4]
    , assertEqual "negotiateUp 5 in [1, 2, 3] returns 3" 3 $ negotiateUp 5 [1, 2, 3]
    ]

getIndexForTimeTests = map (\(stm, intrv, dtm, res) -> assertEqual
    (printf "getIndexForTime startTime: %d, interval: %d, desiredTime: %d = %d" 
            stm intrv dtm res) res $ getIndexForTime stm intrv dtm)
    --  startTime, interval, desiredTime, expected     Example
    [ ( 0,         1,        2,           2       ) -- [0 ,1 ,2*] 
    , ( 1,         1,        2,           1       ) -- [1 ,2*,3 ] 
    , ( 0,         2,        2,           1       ) -- [0 ,2*,4 ] 
    , ( 1,         2,        2,           0       ) -- [1*,3 ,5 ] 
    , ( 0,         5,       10,           2       ) -- [0 ,5 ,10*] 
    , ( 3,         5,        8,           1       ) -- [3 ,8*,13] 
    , ( 3,         5,        7,           0       ) -- [3*,8 ,13] 
    ]


validIndexTests = map (\(min, max, desired, expected) -> assertEqual
    (printf "validIndex min: %d, max: %d, desired: %d = %d" 
            min max desired expected) expected $ validIndex (min, max) desired)
    -- Min  Max     Desired Expected
    [ (0,   0,      0,      0)
    , (0,   0,      1,      0)
    , (0,   5,      4,      4)
    , (4,   6,      3,      4)
    , (4,   6,      8,      6)
    ]

tests = TestList $ map TestCase $
    negotiateUpTests ++
    getIndexForTimeTests ++
    validIndexTests 

runTests = runTestTT tests


-- Mini Spikes
main = do
    putStrLn getPrintFormat
    where 
        getArraySlice = elems . ixmap (1, 3) id $ source 
            where source = listArray (0, 4) [ 1, 2, 3, 4, 5 ]
        getPrintFormat = printf "Hello %d" (3 :: TimeOffset)

