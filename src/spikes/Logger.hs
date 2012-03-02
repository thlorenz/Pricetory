import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

main = do
    s <- openlog "SyslogStuff" [PID] USER DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)

    errorM "Spike.Logger" "Goes to stderr and syslog"

    debugM "Spike.Logger" "Goes nowhere"
    warningM "Spike.Logger" "This goes to stdout"

    updateGlobalLogger "Spike.Logger" (setLevel DEBUG)

    debugM "Spike.Logger" "Now Debug also goes to stdout"
    warningM "Spike.Logger" "This now goes to stderr and syslog"

    {- Growl not working (possibly b/c of new growl app)
    hdlr <- growlHandler "Spike.Logger" DEBUG
    tgt <- addTarget "192.168.1.107" hdlr 
    updateGlobalLogger rootLoggerName (addHandler tgt)
    errorM "Spike.Logger" "This also shows in growl"
    -}

    h <- fileHandler "../log/debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Spike.Logger" (addHandler h)

    debugM "Spike.Logger" "This goes into syslog, stderr and debug.log"
