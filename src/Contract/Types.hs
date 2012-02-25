module Contract.Types where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Word
import Data.Array

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

data Tick = Tick { timeOffset :: TimeOffset, rate :: Rate } deriving (Show, Eq)

-- | Interval of Tick Data is given in seconds
-- e.g., Minute:60, Hour:3600, Day:86400
data HistoricalTickData = HistoricalTickData 
    { tickDataSymbol     :: SymbolCode 
    , tickDataByInterval :: Map.Map TimeInterval (Array Int L.ByteString)
    } deriving Show

-- | Holds Historical Tick Data for all known symbols 
data HistoricalTickDataMap = HistoricalTickDataMap 
    { historicalTickDataBySymbol :: Map.Map SymbolCode HistoricalTickData }
    

