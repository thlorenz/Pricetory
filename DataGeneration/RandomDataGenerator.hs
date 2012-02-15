import qualified Data.ByteString as S;
import qualified Data.ByteString.Lazy as L;
import Data.Char (ord)
import Data.Word
import Data.Binary

type SymbolCode   =  Word32
type Symbol       =  String
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Tick = Tick TimeOffset Rate

magicBytes :: L.ByteString
magicBytes = encode (0x54484f52 :: Word32)

symbolMap :: [(Symbol, SymbolCode)]
symbolMap = [ ("EURUSD", 0x00000001)
            , ("EURGBP", 0x00000002)
            , ("USDCAD", 0x00000003)
            , ("USDCHF", 0x00000004)
            , ("USDCNY", 0x00000005)
            ]

-- TODO add symbolcode mapping function go symbol

createHeader :: SymbolCode -> TimeOffset -> TimeInterval -> L.ByteString
createHeader symbol offset interval = 
    L.concat [magicBytes, symbolBytes, offsetBytes, intervalBytes]
    where
        symbolBytes = encode symbol
        offsetBytes = encode offset
        intervalBytes = encode interval

main = do
    let header = (createHeader 0x00000001 0x20 0x0000001)
    print $ L.unpack header

