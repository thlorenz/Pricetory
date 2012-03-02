module Contract.Protocol ( magicNumber
                         , headerSize
                         , requestSize
                         , requestAckSize
                         , fileHeaderSize
                         , invalid
                         , valid
                         , send
                         , recv
                         , encodeHeader
                         , decodeHeader
                         , encodeRequest
                         , decodeRequest
                         , encodeRequestAck
                         , decodeRequestAck
                         , encodeFileHeader
                         , decodeFileHeader
                         , encodeTick
                         , decodeTick   
                         , decodeTicks
                         ) where

import qualified Data.ByteString.Lazy as L;
import qualified Data.ByteString as B;
import Data.Binary (encode, decode)
import Data.Maybe (fromJust)
import Data.Word (Word32)

import Control.Monad (liftM)

import System.IO (Handle)

import Contract.Types
import Contract.Constants

import Test.QuickCheck

magicNumber = 0x54484f52 :: Word32

invalid =  0 :: Word32
valid   =  1 :: Word32

-- | Sends length of byte string to send followed by that byte string
genericSend :: (Handle -> L.ByteString -> IO ()) -> Handle -> L.ByteString -> IO ()
genericSend put h bs = put h bytesToSend   
    where len = encode ((fromIntegral . L.length) bs :: Word32)
          bytesToSend = L.concat [len, bs] 

send :: Handle -> L.ByteString -> IO ()
send = genericSend L.hPut

-- | Receives length of byte string and then the byte string itself
genericRecv :: (Handle -> Int -> IO L.ByteString) -> Handle -> IO L.ByteString
genericRecv get h = len >>= get h
    where len = liftM (fromIntegral . decodeWord32) . get h $ wordSize
          decodeWord32 = decode :: L.ByteString -> Word32

recv :: Handle -> IO L.ByteString
recv = genericRecv L.hGet

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

encodeRequestAck :: RequestAck -> L.ByteString
encodeRequestAck (RequestAck ackOK ackMsgCode) = 
    L.concat . map encode $ [ackOK, ackMsgCode]

decodeRequestAck :: L.ByteString -> RequestAck
decodeRequestAck = requestAckFromWords . decodeWord32s
    where requestAckFromWords [ackOK, ackMsgCode] = RequestAck ackOK ackMsgCode

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

tickFromWords [timeOffset, rate] = Tick timeOffset rate

decodeTicks :: L.ByteString -> [Tick]
decodeTicks = (ticksFromWords [] . decodeWord32s)
    where ticksFromWords ticks [] = reverse ticks
          ticksFromWords ticks xs = ticksFromWords (newTick:ticks) (snd splits)
              where newTick = tickFromWords $ fst splits
                    splits = splitAt 2 xs

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
                                    
instance Show ProvidedTickData where
    show x = "ProvidedTickData: {" ++
             "Ticks: " ++ (show . length) ticks ++
             " indexes [" ++ (show $ ptdFromIndex x) ++ ", " ++ (show $ ptdToIndex x) ++ "] " ++
             "Key: " ++ (show $ ptdKey x) ++ "}"
        where ticks = map decodeTick $ ptdByteStrings x

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

prop_tick_encode_decode_roundtrippable :: Tick -> Property
prop_tick_encode_decode_roundtrippable tick = 
    property $ (decodeTick . encodeTick) tick == tick

prop_ticks_encode_decode_roundtrippable :: [Tick] -> Property
prop_ticks_encode_decode_roundtrippable ticks = 
    property $ (decodeTicks . L.concat . map encodeTick) ticks == ticks

runTests = do 
    quickCheck prop_header_encode_decode_roundtrippable
    quickCheck prop_tick_encode_decode_roundtrippable
    quickCheck prop_ticks_encode_decode_roundtrippable

    
main = runTests
