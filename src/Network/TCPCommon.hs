module Network.TCPCommon ( initHandle
                         , initLogger 
                         ) where

import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(..), Handle)
import Import.SystemLog

import Data.Char

growlLoggerPriority = EMERGENCY

initHandle ::  Handle -> BufferMode -> IO ()
initHandle h bufmode = hSetBuffering h bufmode >> hSetBinaryMode h True

initLogger loggerName p = do
    updateGlobalLogger loggerName (setLevel prio)
    updateGlobalLogger rootLoggerName . addHandler . growlNotifyHandler loggerName $ growlLoggerPriority
    where prio = stringToPriority p
          stringToPriority = read . map toUpper :: String -> Priority
