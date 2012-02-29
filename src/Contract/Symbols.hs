module Contract.Symbols ( symbolToCode
                        , codeToSymbol
                        , eurusd
                        , eurgbp
                        , usdcad
                        , usdchf
                        , usdcny
                        ) where

import qualified Data.Map as Map;

import Contract.Types

symbolToCode ::  Symbol -> Maybe SymbolCode
symbolToCode symbol = Map.lookup symbol symbolToCodeMap
    where symbolToCodeMap = Map.fromList symbols

codeToSymbol ::  SymbolCode -> Maybe Symbol
codeToSymbol code = Map.lookup code codeToSymbolMap 
    where codeToSymbolMap = Map.fromList . map (\(x, y) -> (y, x)) $ symbols

symbols =
    [ (eurusd, 0x00000001)
    , (eurgbp, 0x00000002)
    , (usdcad, 0x00000003)
    , (usdchf, 0x00000004)
    , (usdcny, 0x00000005)
    ] 

eurusd = "EURUSD"
eurgbp = "EURGBP"
usdcad = "USDCAD"
usdchf = "USDCHF"
usdcny = "USDCNY"
