module Contract.Protocol ( magicNumber
                         , encodeHeader
                         , encodeFileHeader
                         , decodeHeader
                         , decodeFileHeader
                         ) where

import qualified Data.ByteString.Lazy as L;
import qualified Data.ByteString as B;
import Data.Binary

import Contract.Types

import Test.QuickCheck

magicNumber :: Word32
magicNumber = 0x54484f52

encodeHeader :: Header -> L.ByteString
encodeHeader = L.concat . map encode . unfoldHeader
    where unfoldHeader :: Header -> [Word32] 
          unfoldHeader hdr = [symbol hdr, offset hdr, interval hdr, points hdr]

decodeHeader :: L.ByteString -> Header
decodeHeader = headerFromWord32s . decodeWord32s

encodeFileHeader :: Header -> L.ByteString
encodeFileHeader hdr = L.append (encode magicNumber) (encodeHeader hdr)

decodeFileHeader :: L.ByteString -> Maybe Header
decodeFileHeader bs = 
    let (x:xs) = decodeWord32s bs
    in  if x == magicNumber then Just $ headerFromWord32s xs else Nothing

encodeTick :: Tick -> L.ByteString 
encodeTick x = L.concat [encode $ timeOffset x, encode $ rate x]

decodeTick :: L.ByteString -> Tick
decodeTick = tickFromWords . decodeWord32s 
    where tickFromWords [timeOffset, rate] = Tick timeOffset rate

decodeWord32s :: L.ByteString -> [Word32]
decodeWord32s bs = map decode (chunk32s [] bs)
    where chunk32s xs bs = if L.null bs then reverse xs
                           else let (x, y) = L.splitAt 4 bs in chunk32s (x:xs) y 

headerFromWord32s :: [Word32] -> Header
headerFromWord32s [symbol, offset, interval, points] = Header symbol offset interval points

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
    property $ (decodeHeader . encodeHeader) hdr == hdr

prop_ticks_encode_decode_roundtrippable :: Tick -> Property
prop_ticks_encode_decode_roundtrippable tick = 
    property $ (decodeTick . encodeTick) tick == tick

runTests = do 
    quickCheck prop_header_encode_decode_roundtrippable
    quickCheck prop_ticks_encode_decode_roundtrippable

    
main = runTests
