module Utils.GrowlNotifyHandler (growlNotifyHandler) where

import System.Log (Priority)
import System.Log.Handler (LogHandler(..))
import System.Log.Formatter (nullFormatter, LogFormatter(..))
import System.Cmd (rawSystem)

data GrowlNotifyHandler = GrowlNotifyHandler 
    { priority :: Priority
    , formatter :: LogFormatter GrowlNotifyHandler
    , appName :: String
    }

instance LogHandler GrowlNotifyHandler where
    setLevel gnh p = gnh { priority = p }

    getLevel = priority
               
    setFormatter gh f = gh { formatter = f }
    getFormatter = formatter

    emit gnh (prio, msg) _ = do     
        rawSystem "growlnotify" ["-m", (show prio) ++ "\n" ++ msg]
        return ()
    close gnh     = return ()

growlNotifyHandler :: String -> Priority -> GrowlNotifyHandler
growlNotifyHandler service priority = GrowlNotifyHandler priority nullFormatter service 

