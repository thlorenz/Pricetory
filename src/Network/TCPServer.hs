{-# LANGUAGE DeriveDataTypeable #-}

module Network.TCPServer where

import qualified Data.ByteString.Lazy as L

import Network (listenOn, accept, PortID(..), Socket, withSocketsDo)

import System.Console.CmdArgs
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..), Handle, FilePath)

import Control.Concurrent (forkIO)
import Control.Monad (liftM, when)

import Data.Sampler (getWorldOfTickData)

import Contract.Protocol (requestSize, decodeRequest, encodeRequestAck, invalid, valid) 
import Contract.Types
import Contract.Symbols
import Contract.RequestAckMessages

data Arguments = Arguments { host       :: String
                           , port       :: Int
                           , dataFolder :: FilePath
                           } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host server is running on"
    , port = 3000        &= typ "Int"    &= help "Port server is listening on for incoming connections"
    , dataFolder = defFolder &= typ "FilePath" &= help "The folder in which the data resides"
    } &= summary "Pricetory TCP Server version 0.0.1"
    where defFolder = "/Users/thlorenz/dev/data/Pricetory"

main :: IO ()
main = withSocketsDo $ do
    args <- cmdArgs arguments
    sock <- (listenOn . PortNumber . fromIntegral . port) args
    putStrLn $ "Getting world of tickdata from " ++ (dataFolder args)
    -- worldOfTickData <- getWorldOfTickData (dataFolder args) [eurusd] 
    let worldOfTickData = undefined    

    putStrLn $ "Listening on [" ++ (host args) ++ ":" ++ (show $ port args) ++ "]."
    sockHandler sock worldOfTickData 

sockHandler :: Socket -> HistoricalTickDataMap -> IO ()
sockHandler sock world = do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted [" ++ (show host) ++ ":" ++ (show port) ++ "]."
    
    -- no buffering for client's socket handle
    hSetBuffering handle NoBuffering
    -- messages are in binary format
    hSetBinaryMode handle True

    forkIO $ commandProcessor handle world

    -- handle more incoming connections
    sockHandler sock world

commandProcessor :: Handle -> HistoricalTickDataMap -> IO () 
commandProcessor h world = do
    bytes <- L.hGet h requestSize

    when (bytes /= L.empty) $ do
        let mbReq = decodeRequest bytes
        case mbReq of
            Just req    -> handleWellformedRequest h world req
            Nothing     -> handleMalformedRequest h bytes
    
    -- recurse to execute several commands over same connection
    commandProcessor h world

handleMalformedRequest :: Handle -> L.ByteString -> IO ()
handleMalformedRequest h reqBytes =
    print ("Invalid request format" ++ (show reqBytes)) >>
    L.hPut h (encodeRequestAck $ RequestAck invalid invalidFormatMsgCode)

handleWellformedRequest :: Handle -> HistoricalTickDataMap -> Request -> IO ()
handleWellformedRequest h world req = do 
    print  $ "Handling " ++ (show req)
    let ack = requestAck req
    L.hPut h (encodeRequestAck ack)
    when (ackOK ack == valid) $ processRequest h world req
    where requestAck (Request symCode start end itrvl)
            | start >= end  = RequestAck invalid invalidOffsetsMsgCode 
            | itrvl <= 0    = RequestAck invalid invalidIntervalMsgCode
            | codeToSymbol symCode == Nothing = 
                RequestAck invalid invalidSymbolMsgCode
            | otherwise     = RequestAck valid   validMsgCode

processRequest :: Handle -> HistoricalTickDataMap -> Request -> IO ()
processRequest = undefined

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
