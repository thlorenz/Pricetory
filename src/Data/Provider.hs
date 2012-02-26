module Data.Provider (provide) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Text.Printf
import Data.Array (elems, ixmap, listArray, (!))
import Data.List (sort)
import Data.Maybe
import Contract.Types
import Contract.Protocol (decodeTick)

import Test.HUnit

provide :: HistoricalTickDataMap -> SymbolCode -> TimeInterval -> TimeOffset -> TimeOffset -> [L.ByteString]
provide allTickData code interval fromTime toTime = 
    elems . ixmap (getIndex fromTime, getIndex toTime) id $ matchingTickData
    where 
        getIndex = getIndexForTime startTime interval . fromIntegral
        startTime = timeOffset . decodeTick . (! 0) $ matchingTickData
        matchingTickData = fromJust . Map.lookup key $ tickDataForCode 
        key = negotiateUp interval (Map.keys tickDataForCode)
        tickDataForCode = tickDataByInterval . getAllHistTickDataForCode $ allTickData
        getAllHistTickDataForCode = fromJust . Map.lookup code . historicalTickDataBySymbol 

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

tests = TestList $ map TestCase $
    negotiateUpTests ++
    getIndexForTimeTests

runTests = runTestTT tests


-- Mini Spikes
main = do
    putStrLn getPrintFormat
    where 
        getArraySlice = elems . ixmap (1, 3) id $ source 
            where source = listArray (0, 4) [ 1, 2, 3, 4, 5 ]
        getPrintFormat = printf "Hello %d" (3 :: TimeOffset)

