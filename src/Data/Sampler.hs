import qualified Data.ByteString.Lazy as L

import System.IO
import Data.Word
import Data.Maybe (fromJust)

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

main = do
    hp <- openFile fileName ReadMode
    bs <- L.hGet hp (5 * 4) 
    print bs
    let hdr = (fromJust . decodeFileHeader) bs
    print hdr
    -- putStrLn "Header" ++ (show hdr) 

    -- hSeek hp AbsoluteSeek x
    hClose hp
    
    putStrLn "\n--------------\nDone!"

