import System.Log.Logger
import Utils.GrowlNotifyHandler

main = do 
    updateGlobalLogger "Main.Logger" (setLevel DEBUG)
    let hdlr = growlNotifyHandler "Main.Logger" DEBUG
    updateGlobalLogger rootLoggerName (addHandler hdlr)

    debugM "Main.Logger" "This shows in a growl message with Terminal Icon" 

