module Contract.Protocol ( magicNumber
                         , createMsgHeader
                         , createFileHeader
                         ) where

import qualified Data.ByteString.Lazy as L;
import Data.Binary

import Contract.Types

magicNumber :: Word32
magicNumber = 0x54484f52

msgHeader :: Header -> [Word32] 
msgHeader hdr = [symbol hdr, offset hdr, interval hdr, points hdr]

createMsgHeader :: Header -> L.ByteString
createMsgHeader hdr = (L.concat . map encode) $ msgHeader hdr

createFileHeader :: Header -> L.ByteString
createFileHeader hdr = (L.concat . map encode) $ magicNumber : msgHeader hdr


