import qualified Data.ByteString.Lazy as L

import System.IO
import Data.Word
import Data.Maybe (fromJust)
import Control.Monad (liftM)

import Contract.Types
import Contract.Protocol (decodeFileHeader, decodeTick)

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
     maxPoint    - the point after which to stop reading (usually the end of the file)
     pointInterval- interval at which to read points
-}
sampleByteStrings :: Handle -> Int -> Int -> Int -> IO [L.ByteString]
sampleByteStrings h maxPoint interval pointSize = sample maxPoint []
    where skipSize = pointSize * (interval - 1)
          sample remainingPoints xs = do 
              x <- L.hGet h pointSize
              if remainingPoints >= interval then do 
                   hSeek h RelativeSeek $ fromIntegral skipSize
                   sample (remainingPoints - interval) (x:xs)
              else return $ reverse xs

main = do
    h <- openBinaryFile fileName ReadMode
    hdr <- readHeader h
    print hdr

    fileSize <- hFileSize h
    print fileSize
    bs <- sampleByteStrings h (fromIntegral $ points hdr) 60 tickSize 
    let ticks = map decodeTick bs
    print ticks
    hClose h
    
    putStrLn "\n--------------\nDone!"

-- Reader monad via data-ivar, write-once variables
-- http://hackage.haskell.org/packages/archive/data-ivar/0.30/doc/html/Data-IVar.html
