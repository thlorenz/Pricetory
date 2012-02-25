module Contract.Constants where

import Contract.Types

secondsPerMinute = 60 :: TimeInterval
minutesPerHour   = 60 :: TimeInterval
hoursPerDay      = 24 :: TimeInterval  
secondsPerHour   = secondsPerMinute * minutesPerHour
secondsPerDay    = secondsPerHour * hoursPerDay

word = 4 :: Int
tickSize = 2 * word

