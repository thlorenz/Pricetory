module Contract.Types where

import Data.Word

type Symbol       =  String
type SymbolCode   =  Word32
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Header = Header { symbol   :: SymbolCode
                     , offset   :: TimeOffset
                     , interval :: TimeInterval
                     , points   :: Word32
                     } deriving (Show, Eq)

data Tick = Tick { timeOffset :: TimeOffset, rate :: Rate } deriving Show

