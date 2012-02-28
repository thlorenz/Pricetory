{-# LANGUAGE DeriveDataTypeable #-}

module Network.Client where

import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as L;

import System.Console.CmdArgs
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..), Handle, FilePath, hClose)
import System.IO.Error (isEOFError)

import Network (connectTo, accept, PortID(..), withSocketsDo)

import Control.Exception (finally, catch, Exception(..))
import Control.Concurrent (forkIO)
import Control.Monad (liftM)

import Contract.Types
import Contract.Constants
import Contract.Protocol (encodeRequest, decodeRequestAck)

data Arguments = Arguments { host       :: String
                           , port       :: Int
                           } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host name of server to connect to."
    , port = 3000        &= typ "Int"    &= help "Port on which to connect to server."
    } &= summary "Pricetory TCP Client version 0.0.1"


main :: IO ()
main = withSocketsDo $ do
    args <- cmdArgs arguments
    handle <- connectTo (host args) $ (PortNumber . fromIntegral . port) args
    sockHandler handle `catch` handler `finally` hClose handle
        where handler e
                | isEOFError e = return ()
                | otherwise    = print e

sockHandler :: Handle -> IO ()
sockHandler h = do
    putStrLn "Handling socket connection"
    
    -- no buffering for client's socket handle
    hSetBuffering h NoBuffering
    -- messages are in binary format
    hSetBinaryMode h True

    let hdr = encodeRequest  (Request 0 1 2 3)
    L.hPut h hdr
    
    bs <- L.hGet h wordSize
    let ack = decodeRequestAck bs 
    print ack


{- Smarter way to forkIO
spawnThread :: IO () -> IO ThreadId
spawnThread action = do
    mainTID <- myThreadId
    forkIO $ action `catch` throwTo mainTID
-}
