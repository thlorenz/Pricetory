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

-- | TODO: handle Nothing which signifies an exception
readHeader ::  Handle -> IO Header
readHeader fh = liftM (fromJust . decodeFileHeader) $ L.hGet fh (5 * 4) 

main = do
    fh <- openFile fileName ReadMode
    print =<< readHeader fh

    -- hSeek hp AbsoluteSeek x
    hClose fh
    
    putStrLn "\n--------------\nDone!"

