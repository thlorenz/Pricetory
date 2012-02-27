{-# LANGUAGE DeriveDataTypeable #-}

module Network.TCPServer where

import System.Console.CmdArgs
import Network (listenOn, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

data Arguments = Arguments { host :: String, port :: Int } deriving (Show, Data, Typeable)

arguments = Arguments
    { host = "localhost" &= typ "String" &= help "Host server is running on"
    , port = 3000        &= typ "Int"    &= help "Port server is listening on for incoming connections"
    } &= summary "Pricetory TCP Server version 0.0.1"

main :: IO ()
main = do
    args <- cmdArgs arguments
    sock <- (listenOn . PortNumber . fromIntegral . port) args
    putStrLn $ "Listening on " ++ (host args) ++ ":" ++ (show $ port args) ++ "."
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted " ++ (show host) ++ ":" ++ (show port)
    
    -- no buffering for client's socket handle
    hSetBuffering handle NoBuffering

    forkIO $ commandProcessor handle

    -- handle more incoming connections
    sockHandler sock

commandProcessor :: Handle -> IO () 
commandProcessor handle = do
    
    -- recurse to execute several commands over same connection
    commandProcessor handle
