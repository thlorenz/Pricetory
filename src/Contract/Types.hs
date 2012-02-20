module Contract.Types where

import Data.Word

type Symbol       =  String
type SymbolCode   =  Word32
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Tick = Tick { timeOffset :: TimeOffset, rate :: Rate } deriving Show
