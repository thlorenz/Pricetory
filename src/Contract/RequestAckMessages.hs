module Contract.RequestAckMessages where

import qualified Data.Map as Map
import Data.Word

requestAckMessages :: Map.Map Word32 String
requestAckMessages = Map.fromList 
    [ (00, "Everything ok.")
    , (01, "Invalid format.")
    , (02, "StartOffset needs to be smaller than EndOffset.")
    , (03, "Interval must be greater 0.")
    , (04, "Unknown symbol code.")
    ]

validMsgCode           =  00 :: Word32 
invalidFormatMsgCode   =  01 :: Word32 
invalidOffsetsMsgCode  =  02 :: Word32 
invalidIntervalMsgCode =  03 :: Word32 
invalidSymbolMsgCode   =  04 :: Word32 

decodeRequestAckMessage code = Map.lookup code requestAckMessages 
