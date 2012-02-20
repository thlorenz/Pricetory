import qualified Data.ByteString as S;
import qualified Data.ByteString.Lazy as L;
import qualified Data.Map as Map;
import Random (randomRs, RandomGen, Random, mkStdGen, newStdGen)
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

getRandomRateDeltas :: (RandomGen g) => g -> [Rate]
getRandomRateDeltas g = map (\index -> deltas !! index) indexes
    where
        indexes = randomRs (0, upperBound) g
        upperBound = (length deltas) - 1 

        -- smaller rate changes occur more often
        deltas :: [Rate]
        deltas = 
                                  (replicate 8 0) ++
            (replicate 5 (-1)) ++ (replicate 5 1) ++
            (replicate 3 (-2)) ++ (replicate 3 2) ++
            (replicate 2 (-3)) ++ (replicate 2 3) ++
            [4, (-4), 5, (-5)]

getTicks :: Tick -> [Rate] -> TimeOffset -> [Tick]
getTicks prevTick rateDeltas timeInterval = x : getTicks x (tail rateDeltas) timeInterval
    where 
        x = getDeltaTick prevTick (head rateDeltas)

        getDeltaTick currentTick rateDelta = 
                  let nextTimeOffset = (+timeInterval) $ timeOffset currentTick
                      nextRate = (+rateDelta) $ rate currentTick
                  in  Tick nextTimeOffset nextRate 

main = do
    let initialRate = 1234500
    let rt = take 500 $ 
             getTicks (Tick 0 initialRate) (getRandomRateDeltas $ mkStdGen 100) 1
    print rt






