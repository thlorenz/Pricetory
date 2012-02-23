import qualified Data.ByteString.Lazy as L

import System.IO
import Data.Word
import Data.Maybe (fromJust)
import Control.Monad (liftM)

import Contract.Types
import Contract.Protocol (decodeFileHeader)

-- | Converts a list of ByteStrings into a list of Ticks, assuming that interval is 1 sec.
getSecondIntervalData :: [L.ByteString] -> Word32 -> [Tick]
getSecondIntervalData bs points = undefined

decodeTicks :: [L.ByteString] -> [Tick]
decodeTicks = undefined

dataDir = "/Users/thlorenz/dev/data/Pricetory"
symbolName = "EURUSD"
fileName = dataDir ++ "/" ++ symbolName ++ ".bin"

word = 4
tickSize = 2 * word

-- | TODO: handle Nothing which signifies an exception
readHeader ::  Handle -> IO Header
readHeader fh = liftM (fromJust . decodeFileHeader) $ L.hGet fh bytesInHeader
    where bytesInHeader = 5 * word

{- | Samples byte strings from a given file handle
     h          - the file handle
     bytes      - the number of bytes after which to stop reading (usually the end of the file)
     skipNum    - the number of ticks to skip in between each read
-}
sampleByteStrings :: Handle -> Int -> Int -> IO [L.ByteString]
sampleByteStrings h bytes skipNum = sample bytes []
    where sample bytes xs = do 
            x <- L.hGet h tickSize
            if bytes > 0 then do 
                 hSeek h RelativeSeek $ fromIntegral (tickSize * skipNum)
                 sample (bytes - tickSize) (x:xs)
            else return $ reverse xs

main = do
    h <- openBinaryFile fileName ReadMode
    print =<< readHeader h

    print =<< sampleByteStrings h 200 60
    hClose h
    
    putStrLn "\n--------------\nDone!"

-- Reader monad via data-ivar, write-once variables
-- http://hackage.haskell.org/packages/archive/data-ivar/0.30/doc/html/Data-IVar.html
