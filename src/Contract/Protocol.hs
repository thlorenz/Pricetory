module Contract.Protocol ( magicNumber
                         , bytesInHeader
                         , bytesInRequest
                         , bytesInFileHeader
                         , invalid
                         , valid
                         , encodeHeader
                         , decodeHeader
                         , encodeRequest
                         , decodeRequest
                         , encodeFileHeader
                         , decodeFileHeader
                         , encodeTick
                         , decodeTick   
                         , encodeRequestAck
                         , decodeRequestAck
                         ) where

import qualified Data.ByteString.Lazy as L;
import qualified Data.ByteString as B;
import Data.Binary
import Data.Maybe (fromJust)

import Contract.Types
import Contract.Constants

import Test.QuickCheck

magicNumber :: Word32
magicNumber = 0x54484f52

bytesInHeader ::  Int
bytesInHeader = 4 * wordSize

bytesInRequest ::  Int
bytesInRequest = 4 * wordSize

bytesInFileHeader ::  Int
bytesInFileHeader = 5 * wordSize

invalid :: Word32
invalid = 0

valid :: Word32
valid = 1

encodeHeader :: Header -> L.ByteString
encodeHeader = L.concat . map encode . unfoldHeader
    where unfoldHeader :: Header -> [Word32] 
          unfoldHeader hdr = [symbol hdr, offset hdr, interval hdr, points hdr]

decodeHeader :: L.ByteString -> Maybe Header
decodeHeader = headerFromWord32s . decodeWord32s

encodeRequest :: Request -> L.ByteString
encodeRequest = L.concat . map encode . unfoldHeader
    where unfoldHeader :: Request -> [Word32] 
          unfoldHeader hdr = 
            [reqSymbol hdr, reqStartOffset hdr, reqEndOffset hdr, reqInterval hdr]

decodeRequest :: L.ByteString -> Maybe Request
decodeRequest = requestFromWord32s . decodeWord32s

encodeFileHeader :: Header -> L.ByteString
encodeFileHeader hdr = L.append (encode magicNumber) (encodeHeader hdr)

decodeFileHeader :: L.ByteString -> Maybe Header
decodeFileHeader bs = 
    let (x:xs) = decodeWord32s bs
    in  if x == magicNumber then headerFromWord32s xs else Nothing

encodeTick :: Tick -> L.ByteString 
encodeTick x = L.concat [encode $ timeOffset x, encode $ rate x]

decodeTick :: L.ByteString -> Tick
decodeTick = tickFromWords . decodeWord32s 
    where tickFromWords [timeOffset, rate] = Tick timeOffset rate

encodeRequestAck :: RequestAck -> L.ByteString
encodeRequestAck (RequestAck ackOK) = encode ackOK

decodeRequestAck :: L.ByteString -> RequestAck
decodeRequestAck = RequestAck . decode 

headerFromWord32s :: [Word32] -> Maybe Header
headerFromWord32s [symbol, offset, interval, points] = 
    Just $ Header symbol offset interval points
headerFromWord32s _ = Nothing

requestFromWord32s :: [Word32] -> Maybe Request
requestFromWord32s [reqSymbol, reqStartOffset, reqEndOffset, reqInterval] = 
    Just $ Request reqSymbol reqStartOffset reqEndOffset reqInterval  
requestFromWord32s _ = Nothing

decodeWord32s :: L.ByteString -> [Word32]
decodeWord32s bs = map decode (chunk32s [] bs)
    where chunk32s xs bs = if L.null bs then reverse xs
                           else let (x, y) = L.splitAt 4 bs in chunk32s (x:xs) y 
                                    
-- ------------ TESTS -------------

instance Arbitrary Header where
    arbitrary = do symbol   <- elements [1..5]
                   offset   <- elements [1..5]
                   interval <- elements [1..5]
                   points   <- elements [1..5]
                   return $ Header symbol offset interval points

instance Arbitrary Tick where
    arbitrary = do offset   <- elements [1..5]
                   rate     <- elements [1..5]
                   return $ Tick offset rate

prop_header_encode_decode_roundtrippable :: Header -> Property
prop_header_encode_decode_roundtrippable hdr =
    property $ (fromJust . decodeHeader . encodeHeader) hdr == hdr

prop_ticks_encode_decode_roundtrippable :: Tick -> Property
prop_ticks_encode_decode_roundtrippable tick = 
    property $ (decodeTick . encodeTick) tick == tick

runTests = do 
    quickCheck prop_header_encode_decode_roundtrippable
    quickCheck prop_ticks_encode_decode_roundtrippable

    
main = runTests
