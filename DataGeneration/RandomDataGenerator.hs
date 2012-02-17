import qualified Data.ByteString as S;
import qualified Data.ByteString.Lazy as L;
import qualified Data.Map as Map;
import Data.Maybe
import Data.Char (ord)
import Data.Word
import Data.Binary

type SymbolCode   =  Word32
type Symbol       =  String
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Tick = Tick TimeOffset Rate

magicNumber :: Word32
magicNumber = 0x54484f52

symbolMap ::  Map.Map Symbol SymbolCode
symbolMap = Map.fromList $
    [ ("EURUSD", 0x00000001)
    , ("EURGBP", 0x00000002)
    , ("USDCAD", 0x00000003)
    , ("USDCHF", 0x00000004)
    , ("USDCNY", 0x00000005)
    ] 

getSymbolCode :: Symbol -> SymbolCode
getSymbolCode symbol = 
    case Map.lookup symbol symbolMap of
        Just code       -> code 
        Nothing         -> head $ (Map.elems symbolMap)

createHeader :: SymbolCode -> TimeOffset -> TimeInterval -> L.ByteString
createHeader symbol offset interval = 
    (L.concat . map encode) [magicNumber, symbol, offset, interval]

main = do
    let symbol = "EURGBP"
    let header = (createHeader (getSymbolCode symbol) 0x20 0x0000001)
    print $ L.unpack header


