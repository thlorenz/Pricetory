module Data.Provider where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.List (sort)
import Data.Maybe
import Contract.Types
import Test.HUnit

provide :: HistoricalTickDataMap -> SymbolCode -> TimeOffset -> TimeInterval -> Int -> L.ByteString
provide allTickData code offset interval points = undefined
    where 
        -- tickData = fromJust $ Map.lookup code allTickData     
        -- key = negotiate interval (Map.keys tickData)

negotiateUp :: (Ord a) => a -> [a] -> a
negotiateUp desired availables
    | minimum availables >= desired = minimum availables
    | maximum availables <= desired = maximum availables
    | otherwise                     = closest desired $ (sort availables)
    where closest d (x:xs)
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
