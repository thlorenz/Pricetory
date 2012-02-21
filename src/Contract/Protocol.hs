module Contract.Protocol ( magicNumber
                         , encodeHeader
                         , encodeFileHeader
                         ) where

import qualified Data.ByteString.Lazy as L;
import Data.Binary

import Contract.Types

magicNumber :: Word32
magicNumber = 0x54484f52

unfoldHeader :: Header -> [Word32] 
unfoldHeader hdr = [symbol hdr, offset hdr, interval hdr, points hdr]

encodeHeader :: Header -> L.ByteString
encodeHeader = L.concat . map encode . unfoldHeader

encodeFileHeader :: Header -> L.ByteString
encodeFileHeader hdr = L.append (encode magicNumber) (encodeHeader hdr)

decodeFileHeader :: L.ByteString -> Header
decodeFileHeader bs = undefined

