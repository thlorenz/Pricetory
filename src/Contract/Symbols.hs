module Contract.Symbols where

import qualified Data.Map as Map;

import Contract.Types

symbolMap ::  Map.Map Symbol SymbolCode
symbolMap = Map.fromList $
    [ (eurusd, 0x00000001)
    , (eurgbp, 0x00000002)
    , (usdcad, 0x00000003)
    , (usdchf, 0x00000004)
    , (usdcny, 0x00000005)
    ] 

getSymbolCode :: Symbol -> SymbolCode
getSymbolCode symbol = 
    case Map.lookup symbol symbolMap of
        Just code       -> code 
        Nothing         -> head $ (Map.elems symbolMap)

eurusd = "EURUSD"
eurgbp = "EURGBP"
usdcad = "USDCAD"
usdchf = "USDCHF"
usdcny = "USDCNY"
