module Contract.Protocol ( magicNumber
                         , encodeMsgHeader
                         , encodeFileHeader
                         ) where

import qualified Data.ByteString.Lazy as L;
import Data.Binary

import Contract.Types

magicNumber :: Word32
magicNumber = 0x54484f52

msgHeader :: Header -> [Word32] 
msgHeader hdr = [symbol hdr, offset hdr, interval hdr, points hdr]

encodeMsgHeader :: Header -> L.ByteString
encodeMsgHeader hdr = (L.concat . map encode) $ msgHeader hdr

encodeFileHeader :: Header -> L.ByteString
encodeFileHeader hdr = (L.concat . map encode) $ magicNumber : msgHeader hdr

decodeFileHeader :: L.ByteString -> Header
decodeFileHeader bs = undefined

