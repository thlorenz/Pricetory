{-# LANGUAGE DeriveDataTypeable #-}

module Network.TCPServer where

import System.Console.CmdArgs
import Network (listenOn, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, FilePath)
import Control.Concurrent (forkIO)

import Data.Sampler (getWorldOfTickData)
import Contract.Types
import Contract.Symbols

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
main = do
    args <- cmdArgs arguments
    sock <- (listenOn . PortNumber . fromIntegral . port) args
    putStrLn $ "Getting world of tickdata from " ++ (dataFolder args)
    worldOfTickData <- getWorldOfTickData (dataFolder args) [eurusd] 

    putStrLn $ "Listening on " ++ (host args) ++ ":" ++ (show $ port args) ++ "."
    sockHandler sock worldOfTickData 

sockHandler :: Socket -> HistoricalTickDataMap -> IO ()
sockHandler sock world = do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted " ++ (show host) ++ ":" ++ (show port)
    
    -- no buffering for client's socket handle
    hSetBuffering handle NoBuffering

    forkIO $ commandProcessor handle

    -- handle more incoming connections
    sockHandler sock world

commandProcessor :: Handle -> IO () 
commandProcessor handle = do
    
    -- recurse to execute several commands over same connection
    commandProcessor handle
