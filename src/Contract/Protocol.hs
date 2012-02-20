module Contract.Protocol ( magicNumber
                         , createMsgHeader
                         , createFileHeader
                         ) where

import qualified Data.ByteString.Lazy as L;
import Data.Binary

import Contract.Types

magicNumber :: Word32
magicNumber = 0x54484f52

msgHeader :: SymbolCode -> TimeOffset -> TimeInterval -> Word32 -> [Word32] 
msgHeader symbol offset interval length =  [symbol, offset, interval, length]

createMsgHeader :: SymbolCode -> TimeOffset -> TimeInterval -> Word32 -> L.ByteString
createMsgHeader symbol offset interval length = 
    (L.concat . map encode) $ msgHeader symbol offset interval length

createFileHeader :: SymbolCode -> TimeOffset -> TimeInterval -> Word32 -> L.ByteString
createFileHeader symbol offset interval length = 
    (L.concat . map encode) $ magicNumber : msgHeader symbol offset interval length
