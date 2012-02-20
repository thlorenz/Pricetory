{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import Data.Word
import Contract.Types
import Contract.Symbols

data Arguments = Arguments { symbol    :: Symbol
                           , startTime :: TimeOffset
                           , startRate :: Rate
                           , points    :: Word32
                           } deriving (Show, Data, Typeable)

arguments = Arguments 
    { symbol = eurusd     &= typ "Symbol"     &= help "Symbol for which to generate data"
    , startTime = 0       &= typ "TimeOffset" &= help "Time of first tick"
    , startRate = 5555555 &= typ "Rate"       &= help "Rate of first tick"
    , points = 100        &= typ "Word32"     &= help "Number of ticks to generate"
    } &= summary "Random Data Generator version 0.0.1"

main = print =<< cmdArgs arguments

