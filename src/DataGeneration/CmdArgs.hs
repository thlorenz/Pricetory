{-# LANGUAGE DeriveDataTypeable #-}

module DataGeneration.CmdArgs (Arguments (..), arguments) where

import Data.Word
import System.Console.CmdArgs

import qualified Contract.Types as T
import Contract.Symbols

data Arguments = Arguments { symbol    :: T.Symbol
                           , time      :: T.TimeOffset
                           , interval  :: T.TimeInterval
                           , firstRate :: T.Rate
                           , points    :: Word32
                           , directory :: FilePath
                           , random    :: Int 
                           } deriving (Show, Data, Typeable)

-- | Defaults to EURUSD starting at 0, 1 tick/sec for a year (31556926s) into data directory
arguments = Arguments 
    { symbol = eurusd    &= typ "Symbol"       &= help "Symbol for which to generate data"
    , time = 0           &= typ "TimeOffset"   &= help "Time of first tick"
    , interval = 1       &= typ "TimeInterval" &= help "Seconds between ticks"
    , firstRate = 13245  &= typ "Rate"         &= help "Rate of first tick"
    , points = defPts    &= typ "Int"          &= help "Number of ticks to generate"
    , directory = defDir &= typ "FilePath"     &= help "Directory into which to output binary file"
    , random = 0         &= typ "Int"          &= help "Seed for random number generation"
    } &= summary "Random Tick Data Generator version 0.0.1"
    where defPts = 100000 
          defDir = "/Users/thlorenz/dev/data/Pricetory"
