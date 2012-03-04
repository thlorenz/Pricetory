{-# LANGUAGE DeriveDataTypeable #-}

module Network.Client where

import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as L;

import System.Console.CmdArgs
import System.IO (Handle, FilePath, hClose)
import System.IO.Error (isEOFError)

import Network.Socket (HostName)
import Network (connectTo, accept, PortID(..), withSocketsDo)

import Control.Exception (finally, catch, Exception(..))
import Control.Concurrent (forkIO)
import Control.Monad (liftM, forever)

import Data.Word (Word32)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

import Contract.Types
import Contract.Constants
import Contract.Protocol (send, recv, valid, encodeRequest, decodeRequestAck, decodeTicks)

import Network.TCPCommon
import Network.RandomRequestGenerator (getRandomRequests)
import Import.SystemLog

data Arguments = Arguments { host            :: String
                           , port            :: Int
                           , loggingPriority :: String
                           , clients         :: Int
                           } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host name of server to connect to."
    , port = 3000        &= typ "Int"    &= help "Port on which to connect to server."
    , loggingPriority = "debug" &= typ "String" &= help "The priority level to log on."
    , clients = 1        &= typ "Int"    &= help "Number of concurrent clients to fire up."
    } &= summary "Pricetory TCP Client version 0.0.1"

loggerName = "Client"
logd = debugM loggerName
logi = infoM  loggerName
loge = errorM loggerName

main :: IO ()
main = withSocketsDo $ do
    args <- cmdArgs arguments
    initLogger loggerName . loggingPriority $ args

    mapM (\clientId -> forkIO $ launchClient (host args) (portId args) clientId) 
         [1 .. clients args - 1]

    -- launch last client on main thread to keep it from exiting and killing all child 
    -- processes
    launchClient (host args) (portId args) (clients args)
    where portId = PortNumber . fromIntegral . port

launchClient :: HostName -> PortID -> Int -> IO ()
launchClient hostAddress portId clientId = do
    handle <- connectTo hostAddress portId
    sockHandler handle clientId `catch` handler `finally` hClose handle
    where handler e
            | isEOFError e = return ()
            | otherwise    = loge $ show e

sockHandler :: Handle -> Int -> IO ()
sockHandler h clientId = do
    logi $ "Client: " ++ show clientId ++ " got socket connection."
    initHandle h
    randomRequests <- getRandomRequests 
    getCurrentTime >>= fireRequest h clientId 0 randomRequests

fireRequest :: Handle -> Int -> Int -> [Request] -> UTCTime -> IO ()
fireRequest h clientId reqNum (req:reqs) startTime = do
    logd $ "Sending: " ++ (show req)
    sendReq h req
    ack <- recvReqAck h
    if (ackOK ack == valid) then recvTicks h else return ()

    if (reqNum == 1000)
        then logStats clientId reqNum startTime 
             >>  getCurrentTime 
             >>= fireRequest h clientId 0 reqs
        else fireRequest h clientId (reqNum + 1) reqs startTime

logStats :: Int -> Int -> UTCTime -> IO ()
logStats clientId reqNum startTime = do
    currentTime <- getCurrentTime
    let secs = diffUTCTime currentTime startTime
    logi $ "Client " ++ show clientId ++ ": " ++ show reqNum ++ 
           " request where fullfilled in " ++ show secs ++ " seconds."
    

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
