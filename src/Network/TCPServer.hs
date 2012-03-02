{-# LANGUAGE DeriveDataTypeable #-}

module Network.TCPServer where

import qualified Data.ByteString.Lazy as L

import Network (listenOn, accept, PortID(..), Socket, withSocketsDo)

import System.Console.CmdArgs
import System.IO (Handle)

import Import.SystemLog 

import Control.Concurrent (forkIO)
import Control.Monad (liftM, when)

import Data.Sampler (getWorldOfTickData)

import Contract.Protocol (send, recv, requestSize, decodeRequest, encodeRequestAck, invalid, valid) 
import Contract.Types
import Contract.Symbols
import Contract.RequestAckMessages

import Data.Provider (provide)

import Network.TCPCommon
import Import.SystemLog

data Arguments = Arguments { host       :: String
                           , port       :: Int
                           , dataFolder :: FilePath
                           , loggingPriority :: String
                           } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host server is running on"
    , port = 3000        &= typ "Int"    &= help "Port server is listening on for incoming connections"
    , dataFolder = defFolder &= typ "FilePath" &= help "The folder in which the data resides"
    , loggingPriority = "debug" &= typ "String" &= help "The priority level to log on"
    } &= summary "Pricetory TCP Server version 0.0.1"
    where defFolder = "/Users/thlorenz/dev/data/Pricetory"

loggerName = "Server"
logd = debugM loggerName
logi = infoM  loggerName
loge = errorM loggerName

main :: IO ()
main = withSocketsDo $ do
    args <- cmdArgs arguments
    sock <- (listenOn . PortNumber . fromIntegral . port) args
    initLogger loggerName . loggingPriority $ args

    logi $ "Getting world of tickdata from " ++ (dataFolder args)
    worldOfTickData <- getWorldOfTickData (dataFolder args) [eurusd] 

    logi $ "Listening on [" ++ (host args) ++ ":" ++ (show $ port args) ++ "]."

    sockHandler sock worldOfTickData 

sockHandler :: Socket -> HistoricalTickDataMap -> IO ()
sockHandler sock world = do
    (handle, host, port) <- accept sock
    logi $ "Accepted [" ++ (show host) ++ ":" ++ (show port) ++ "]."
    
    initHandle handle

    forkIO $ commandProcessor handle world

    -- handle more incoming connections
    sockHandler sock world

commandProcessor :: Handle -> HistoricalTickDataMap -> IO () 
commandProcessor h world = do
    bytes <- recv h

    when (bytes /= L.empty) $ do
        let mbReq = decodeRequest bytes
        case mbReq of
            Just req    -> handleWellformedRequest h world req
            Nothing     -> handleMalformedRequest h bytes
    
    -- recurse to execute several commands over same connection
    commandProcessor h world

handleMalformedRequest :: Handle -> L.ByteString -> IO ()
handleMalformedRequest h reqBytes =
    loge ("Invalid request format" ++ (show reqBytes)) >>
    send h (encodeRequestAck $ RequestAck invalid invalidFormatMsgCode)

handleWellformedRequest :: Handle -> HistoricalTickDataMap -> Request -> IO ()
handleWellformedRequest h world req = do 
    logd  $ "Handling " ++ (show req)
    let ack = requestAck req
    send h (encodeRequestAck ack)
    when (ackOK ack == valid) $ processRequest h world req
    where requestAck (Request symCode start end itrvl)
            | start >= end  = RequestAck invalid invalidOffsetsMsgCode 
            | itrvl <= 0    = RequestAck invalid invalidIntervalMsgCode
            | codeToSymbol symCode == Nothing = 
                RequestAck invalid invalidSymbolMsgCode
            | otherwise     = RequestAck valid   validMsgCode

processRequest :: Handle -> HistoricalTickDataMap -> Request -> IO ()
processRequest h world (Request symCode start end itrvl) = do
    let dta = provide world symCode start end itrvl
    logd $ show dta
    
    send h (L.concat $ ptdByteStrings dta)
 

{- Comments
    L.hGet always returns something - empty - when no bytes where sent b/c it uses
    hGetN under the hood which is implemented as follows:

    hGetN :: Int -> Handle -> Int -> IO ByteString
    hGetN k h n | n > 0 = readChunks n
      where
        STRICT1(readChunks)
        readChunks i = do
            c <- S.hGet h (min k i)
            case S.length c of
                0 -> return Empty                   <--- that's why
                m -> do cs <- readChunks (i - m)
                        return (Chunk c cs)
-}
