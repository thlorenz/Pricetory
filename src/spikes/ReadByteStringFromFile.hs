import System.IO
import Data.Char (ord)
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

eurusdFileP = "EurUsdPacked.bin"

main :: IO ()
main = do
    hp <- openFile eurusdFileP ReadMode
    -- get all chars
    L.hGet hp 3 >>= print

    -- get last char ('R') of EUR
    hSeek hp AbsoluteSeek 2
    L.hGet hp 1 >>= print

