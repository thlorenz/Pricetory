module Contract.Types where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Word
import Data.Array

import Contract.RequestAckMessages (decodeRequestAckMessage)

type Symbol       =  String
type SymbolCode   =  Word32
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Header = Header 
    { symbol   :: SymbolCode
    , offset   :: TimeOffset
    , interval :: TimeInterval
    , points   :: Word32
    } deriving (Show, Eq)

data Request = Request 
    { reqSymbol      :: SymbolCode
    , reqStartOffset :: TimeOffset
    , reqEndOffset   :: TimeOffset
    , reqInterval    :: TimeInterval
    } deriving (Show, Eq)

-- | Sent to client to ack valid (ackOK = 1) or invalid (ackOK = 0) request
data RequestAck = RequestAck { ackOK :: Word32 , ackMsgCode :: Word32}
instance Show RequestAck where
    show (RequestAck ackOK ackMsgCode)
        | ackOK == 1 = "RequestAck: OK."
        | otherwise  = "RequestAck: NotOK. (" ++ (decodeRequestAckMessage ackMsgCode) ++ ")"

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
    deriving Show
    
data ProvidedTickData = ProvidedTickData
    { ptdFromIndex   :: Int
    , ptdToIndex     :: Int
    , ptdKey         :: TimeInterval
    , ptdByteStrings :: [L.ByteString]
    } 

-- | sizes in bytes
wordSize       =  4 :: Int
tickSize       =  2 * wordSize :: Int
headerSize     =  4 * wordSize :: Int
requestSize    =  4 * wordSize :: Int
requestAckSize =  2 * wordSize :: Int
fileHeaderSize =  5 * wordSize :: Int

