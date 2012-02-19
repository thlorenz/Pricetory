import System.IO
import Data.Char (ord)
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

eurusdFileP = "EurUsdPacked.bin"
eurusdFileE = "EurUsdEncoded.bin"

main :: IO ()
main = do
    -- Both options yield exact same result
    hp <- openFile eurusdFileP WriteMode
    he <- openFile eurusdFileE WriteMode

    let packed = (L.pack . map (fromIntegral . ord)) "EUR" 
    L.hPut hp packed

    let encoded = map encode "EUR"
    L.hPut he (encoded !! 0)
    L.hPut he (encoded !! 1)
    L.hPut he (encoded !! 2)

    hClose hp
    hClose he

