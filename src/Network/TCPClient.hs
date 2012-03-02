{-# LANGUAGE DeriveDataTypeable #-}

module Network.Client where

import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as L;

import System.Console.CmdArgs
import System.IO (Handle, FilePath, hClose)
import System.IO.Error (isEOFError)

import Network (connectTo, accept, PortID(..), withSocketsDo)

import Control.Exception (finally, catch, Exception(..))
import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Data.Word (Word32)

import Contract.Types
import Contract.Constants
import Contract.Protocol (send, recv, valid, encodeRequest, decodeRequestAck, decodeTicks)

import Network.TCPCommon (initHandle)

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
    putStrLn "Got socket connection"
    
    initHandle h

    sendReq h $ Request 1 500 1200 300 
    
    ack <- recvReqAck h
    if (ackOK ack == valid) then recvTicks h else return ()

sendReq h = send h . encodeRequest

recvReqAck h = do 
    ack <- (liftM decodeRequestAck . recv) h
    print ack
    return ack

recvTicks h = recv h >>= print . show . decodeTicks

{- Smarter way to forkIO
spawnThread :: IO () -> IO ThreadId
spawnThread action = do
    mainTID <- myThreadId
    forkIO $ action `catch` throwTo mainTID
-}
