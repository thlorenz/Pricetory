module Data.Provider where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Array (elems, ixmap)
import Data.List (sort)
import Data.Maybe
import Contract.Types
import Test.HUnit

provide :: HistoricalTickDataMap -> SymbolCode -> TimeInterval -> TimeOffset -> TimeOffset -> [L.ByteString]
provide allTickData code interval from to = 
    elems . ixmap (fromIntegral from, fromIntegral to) (\i -> i) $ tickArray
    where 
        tickArray = fromJust . Map.lookup key $ tickDataForCode 
        key = negotiateUp interval (Map.keys tickDataForCode)
        tickDataForCode = tickDataByInterval . getHistTickDataForCode $ allTickData
        getHistTickDataForCode = fromJust . Map.lookup code . historicalTickDataBySymbol 

negotiateUp :: (Ord a) => a -> [a] -> a
negotiateUp desired availables
    | min >= desired = min
    | max <= desired = max
    | otherwise      = closest desired $ (sort availables)
    where 
        min = minimum availables
        max = maximum availables
        closest d (x:xs) 
          | d <= x    = x
          | otherwise = closest d xs
    

-----------------------
-- ----  Tests  ---- --
-----------------------

negotiateUpTests =
    [ assertEqual "negotiateUp 2 in [1, 2, 3] returns 2" 2 $ negotiateUp 2 [1, 2, 3]
    , assertEqual "negotiateUp 2 in [1, 3, 4] returns 3" 3 $ negotiateUp 2 [1, 3, 4]
    , assertEqual "negotiateUp 0 in [1, 2, 3] returns 1" 1 $ negotiateUp 0 [1, 3, 4]
    , assertEqual "negotiateUp 5 in [1, 2, 3] returns 3" 3 $ negotiateUp 5 [1, 2, 3]
    ]

tests = TestList $ map TestCase $
    negotiateUpTests

runTests = runTestTT tests


