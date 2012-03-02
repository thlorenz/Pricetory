module Network.TCPCommon (initHandle) where

import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..), Handle)

initHandle ::  Handle -> IO ()
initHandle h = hSetBuffering h NoBuffering >> hSetBinaryMode h True
