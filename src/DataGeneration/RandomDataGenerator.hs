import qualified Data.ByteString as S;
import qualified Data.ByteString.Lazy as L;
import qualified Data.Map as Map;
import Random (randomR, RandomGen, newStdGen)
import Control.Monad
import Data.Maybe
import Data.Char (ord)
import Data.Word
import Data.Binary

type SymbolCode   =  Word32
type Symbol       =  String
type TimeOffset   =  Word32
type TimeInterval =  Word32
type Rate         =  Word32

data Tick = Tick { timeOffset :: TimeOffset, rate :: Rate } deriving Show

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

writeHeader = do
    let symbol = "EURGBP"
    let header = (createHeader (getSymbolCode symbol) 0x20 0x0000001)       
    print $ L.unpack header

getDeltaTick ::  Tick -> Rate -> Tick
getDeltaTick currentTick rateDelta = Tick nextTimeOffset nextRate 
    where 
        nextTimeOffset = (+1) $ timeOffset currentTick
        nextRate = (+rateDelta) $ rate currentTick

randomRateDelta :: (RandomGen g) => g -> (Rate, g)
randomRateDelta gen = (deltas !! index, nextGen)
    where
        (index, nextGen)  = randomR (0, upperBound) gen 
        upperBound = (length deltas) - 1 

        -- smaller rate changes occur more often
        deltas :: [Rate]
        deltas = 
            (replicate 5 (-1)) ++ (replicate 5 1) ++
            (replicate 3 (-2)) ++ (replicate 3 2) ++
            (replicate 2 (-3)) ++ (replicate 2 3) ++
            [4, (-4), 5, (-5)]

getTicks initialTick rateDeltas = map fst . scanl f (initialTick, rateDeltas)
    where f (Tick timeOffset rate, x:xs) _ = (Tick (timeOffset + 1) (rate + x), xs)

main = do
    let points = 10
    let initialRate = 12345
    let rt = getTicks (Tick 0 initialRate) [1,2,3,4,2,1] [1..4]
    print rt






