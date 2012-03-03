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
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

import Contract.Types
import Contract.Constants
import Contract.Protocol (send, recv, valid, encodeRequest, decodeRequestAck, decodeTicks)

import Network.TCPCommon
import Import.SystemLog

data Arguments = Arguments { host            :: String
                           , port            :: Int
                           , loggingPriority :: String
                           } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host name of server to connect to."
    , port = 3000        &= typ "Int"    &= help "Port on which to connect to server."
    , loggingPriority = "debug" &= typ "String" &= help "The priority level to log on"
    } &= summary "Pricetory TCP Client version 0.0.1"

loggerName = "Client"
logd = debugM loggerName
logi = infoM  loggerName
loge = errorM loggerName

main :: IO ()
main = withSocketsDo $ do
    args <- cmdArgs arguments
    initLogger loggerName . loggingPriority $ args

    handle <- connectTo (host args) $ (PortNumber . fromIntegral . port) args
    sockHandler handle `catch` handler `finally` hClose handle
        where handler e
                | isEOFError e = return ()
                | otherwise    = loge $ show e

sockHandler :: Handle -> IO ()
sockHandler h = do
    logi "Got socket connection"
    initHandle h
    getCurrentTime >>= fireRequest h 0 

fireRequest :: Handle -> Int -> UTCTime -> IO ()
fireRequest h reqNum startTime= do
    sendReq h $ Request 1 500 1200 300 
    ack <- recvReqAck h
    if (ackOK ack == valid) then recvTicks h else return ()

    if (reqNum == 10000)
        then logStats reqNum startTime >> getCurrentTime >>= fireRequest h 0
        else fireRequest h (reqNum + 1) startTime

logStats :: Int -> UTCTime -> IO ()
logStats reqNum startTime = do
    currentTime <- getCurrentTime
    let secs = diffUTCTime currentTime startTime
    logi $ show reqNum ++ " request where fullfilled in " ++ show secs ++ " seconds."
    

sendReq h = send h . encodeRequest

recvReqAck h = do 
    ack <- (liftM decodeRequestAck . recv) h
    logd $ show ack
    return ack

recvTicks h = recv h >>= logd . show . decodeTicks

{- Smarter way to forkIO
spawnThread :: IO () -> IO ThreadId
spawnThread action = do
    mainTID <- myThreadId
    forkIO $ action `catch` throwTo mainTID
-}
