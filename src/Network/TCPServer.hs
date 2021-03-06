{-# LANGUAGE DeriveDataTypeable #-}

module Network.TCPServer where

import qualified Data.ByteString.Lazy as L

import Network (listenOn, accept, PortID(..), Socket, withSocketsDo)

import System.Console.CmdArgs
import System.IO

import Data.Maybe (fromJust)

import Control.Concurrent (forkIO)
import Control.Monad (when)

import Data.Sampler (getWorldOfTickData)

import Contract.Protocol ( getFullSymbolDataPath  
                         , send
                         , recv
                         , requestSize
                         , decodeRequest
                         , encodeRequestAck
                         , invalid
                         , valid
                         ) 
import Contract.Types
import Contract.Symbols
import Contract.RequestAckMessages

import Data.Provider (provide, provideFromFile)

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
    , loggingPriority = "info" &= typ "String" &= help "The priority level to log on"
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

    sockHandler sock worldOfTickData (dataFolder args)

sockHandler :: Socket -> HistoricalTickDataMap -> FilePath -> IO ()
sockHandler sock world dataFolder = do
    (handle, host, port) <- accept sock
    logi $ "Accepted [" ++ (show host) ++ ":" ++ (show port) ++ "]."
    
    initHandle handle LineBuffering -- (BlockBuffering $ Just 4096)

    forkIO $ commandProcessor handle world dataFolder

    -- handle more incoming connections
    sockHandler sock world dataFolder

commandProcessor :: Handle -> HistoricalTickDataMap -> FilePath -> IO () 
commandProcessor h world dataFolder = do
    bytes <- recv h

    when (bytes /= L.empty) $ do
        let mbReq = decodeRequest bytes
        case mbReq of
            Just req    -> handleWellformedRequest h world dataFolder req
            Nothing     -> handleMalformedRequest h bytes
    
    -- recurse to execute several commands over same connection
    commandProcessor h world dataFolder

handleMalformedRequest :: Handle -> L.ByteString -> IO ()
handleMalformedRequest h reqBytes =
    loge ("Invalid request format" ++ (show reqBytes)) >>
    send h (encodeRequestAck $ RequestAck invalid invalidFormatMsgCode)

handleWellformedRequest :: Handle -> HistoricalTickDataMap -> FilePath -> Request -> IO ()
handleWellformedRequest h world dataFolder req = do 
    logd  $ "Handling " ++ (show req)
    let ack = requestAck req
    send h (encodeRequestAck ack)
    when (ackOK ack == valid) $ processRequest h world dataFolder req
    where requestAck (Request symCode start end itrvl)
            | start >= end  = RequestAck invalid invalidOffsetsMsgCode 
            | itrvl <= 0    = RequestAck invalid invalidIntervalMsgCode
            | codeToSymbol symCode == Nothing = 
                RequestAck invalid invalidSymbolMsgCode
            | otherwise     = RequestAck valid   validMsgCode

processRequest :: Handle -> HistoricalTickDataMap -> FilePath -> Request -> IO ()
processRequest h world dataFolder (Request symCode start end itrvl) =
    if itrvl /= 1
       then do 
            let dta = provide world symCode start end itrvl
            logd $ show dta
            send h (L.concat $ ptdByteStrings dta)
       else do 
            logd $ "Providing from File from " ++ show start ++ " to " ++ show end ++ "."
            openBinaryFile filePath ReadMode >>= provideFromFile start end >>=  send h
       where filePath = 
                getFullSymbolDataPath dataFolder . fromJust . codeToSymbol $ symCode
            
 

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
